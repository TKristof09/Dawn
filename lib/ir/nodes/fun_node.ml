open Core

let create g loc fun_ptr_type =
    let ret_typ =
        match fun_ptr_type with
        | Types.FunPtr (Value { params = _; ret; fun_indices = _ }) -> ret
        | _ -> failwithf "Expected function pointer type got %s" (Types.show fun_ptr_type) ()
    in
    let return_value = Node2.create_data loc ret_typ Phi in
    let return_mem = Node2.create_mem loc Memory Phi in
    let return_region = Node2.create_ctrl loc Control Region in
    Node2.G.add_node g return_region { Node2.ctrl_inputs = [] };
    Node2.G.add_node g return_value { Node2.phi_inputs = [] };
    Node2.G.set_ctrl g return_value return_region;
    Node2.G.add_node g return_mem { Node2.phi_inputs = [] };
    Node2.G.set_ctrl g return_mem return_region;
    let ret_node_typ : Types.t =
        match return_value.typ with
        | Tuple (Value t) -> Tuple (Value (Types.Control :: Memory :: t))
        | v -> Tuple (Value [ Control; Memory; v ])
    in
    let ret_node = Node2.create_ctrl loc ret_node_typ Return in
    Node2.G.add_node g ret_node
      { Node2.data = Some (AnyData return_value); mem = Some (AnyMem return_mem) };
    Node2.G.set_ctrl g ret_node return_region;
    let fun_node =
        Node2.create_ctrl loc Control
          (Function { ret = ret_node; signature = fun_ptr_type; idx = -1 })
    in
    Node2.G.add_node g fun_node { Node2.call_sites = [] };
    (fun_node, ret_node)

let create_param g loc ?parent_fun fun_node param_type i =
    let n = Node2.create_data ?parent_fun loc param_type (Param i) in
    Node2.G.add_node g n { Node2.phi_inputs = [] };
    Node2.G.set_ctrl g n fun_node;
    n

let add_call g loc ?parent_fun ~ctrl ~mem ~fun_ptr args =
    let ret_typ =
        match fun_ptr.Node2.typ with
        | FunPtr (Value { params = _; ret; fun_indices = _ }) -> ret
        | t -> (
            match fun_ptr.kind with
            | ForwardRef _ -> ALL
            | _ -> failwithf "Expected function pointer got: %s" (Types.show t) ())
    in
    let call = Node2.create_ctrl ?parent_fun loc Control FunctionCall in
    Node2.G.add_node g call
      {
        Node2.fun_ptr = Some (AnyData fun_ptr);
        mem = Some (AnyMem mem);
        args = List.map args ~f:(fun a -> Some (Node2.AnyData a));
      };
    Node2.G.set_ctrl g call ctrl;
    let call_end =
        Node2.create_ctrl ?parent_fun loc
          (Tuple (Value [ Control; Memory; ret_typ ]))
          FunctionCallEnd
    in
    Node2.G.add_node g call_end { Node2.ret_nodes = [] };
    Node2.G.set_ctrl g call_end call;
    (call, call_end)

let link_call g ~call_node ~fun_node =
    let (Ctrl (Function { ret = ret_node; signature = _; idx = _ })) = fun_node.Node2.kind in
    let rec find_call_end : Node2.any list -> (Node2.fun_call_end, Node2.ctrl) Node2.t = function
        | [] -> assert false
        | Node2.AnyNode h :: t -> (
            match h.Node2.kind with
            | Ctrl FunctionCallEnd -> h
            | _ -> find_call_end t)
    in
    let call_end = Node2.G.get_dependants g call_node |> find_call_end in
    let rec get_params_nodes : Node2.any list -> (Node2.any_data Node2.phi, Node2.data) Node2.t list
        = function
        | [] -> []
        | Node2.AnyNode h :: t -> (
            match h.Node2.kind with
            | Data (Param _) -> h :: get_params_nodes t
            | _ -> get_params_nodes t)
    in
    let param_nodes = Node2.G.get_dependants g fun_node |> get_params_nodes in
    let { Node2.fun_ptr = _; mem = _; args } = Node2.G.get_dependencies_exn g call_node in
    match List.zip param_nodes args with
    | Unequal_lengths ->
        [%log.debug
            "Call linking failed for %a, incorrect number of arguments. Expected %d got %d" Node2.pp
              call_node (List.length param_nodes) (List.length args)]
    | Ok l ->
        let { Node2.ret_nodes } = Node2.G.get_dependencies_exn g call_end in
        let { Node2.call_sites } = Node2.G.get_dependencies_exn g fun_node in
        Node2.G.set_node_inputs g call_end
          { Node2.ret_nodes = ret_nodes @ [ Some (AnyCtrl ret_node) ] };
        Node2.G.set_node_inputs g fun_node
          { call_sites = call_sites @ [ Some (AnyCtrl call_node) ] };
        List.iter l ~f:(fun (param, arg) ->
            let arg = Option.value_exn arg in
            Phi_node.add_input g param arg)

let add_return ?parent_fun g ret_node ~ctrl ~mem ~val_n =
    let { Node2.mem = phi_mem; data = phi_data } = Node2.G.get_dependencies_exn g ret_node in
    let (AnyMem phi_mem) = Option.value_exn phi_mem in
    let (AnyData phi_data) = Option.value_exn phi_data in
    let (AnyNode region) = Node2.G.get_ctrl g ret_node |> Option.value_exn in

    let region = Node2.unpack_exn region (Ctrl Region) in
    let { Node2.ctrl_inputs } = Node2.G.get_dependencies_exn g region in

    let phi_mem = Node2.unpack_exn phi_mem (Mem Phi) in
    let phi_data = Node2.unpack_exn phi_data (Data Phi) in

    if Option.is_none ret_node.Node2.parent_fun then (
      region.parent_fun <- parent_fun;
      phi_mem.parent_fun <- parent_fun;
      phi_data.parent_fun <- parent_fun;
      ret_node.parent_fun <- parent_fun);

    Phi_node.add_input g phi_data (Node2.AnyData val_n);
    Phi_node.add_input g phi_mem (Node2.AnyMem mem);
    Node2.G.set_node_inputs g region { Node2.ctrl_inputs = ctrl_inputs @ [ Some (AnyCtrl ctrl) ] };
    (* TODO do i care about this? we will set the types to ANY during SCCP anyway *)
    let ~new_type:typ, ~extra_deps:_ = Phi_node.compute_type g phi_mem in
    phi_mem.typ <- typ;
    let ~new_type:typ, ~extra_deps:_ = Phi_node.compute_type g phi_data in
    phi_data.typ <- typ

let get_signature n =
    match n.Node2.kind with
    | Ctrl (Function { ret = _; signature; idx = _ }) -> signature

let get_call_fun_ptr g n =
    let { Node2.fun_ptr; mem = _; args = _ } = Node2.G.get_dependencies_exn g n in
    fun_ptr

let compute_fun_node_type g n =
    let new_type =
        Graph.get_dependencies g n
        |> List.filter_map ~f:(function
          | None -> None
          | Some dep -> (
              match dep.Node.kind with
              | Ctrl Start -> None
              | _ -> Some dep.typ))
        |> List.fold ~init:Types.DeadControl ~f:Types.meet
    in
    (~new_type, ~extra_deps:[])

let compute_call_end_type g (n : Node.t) =
    let call = Graph.get_dependency g n 0 |> Option.value_exn in
    let new_type =
        if not (Poly.equal call.typ Types.Control) then
          (* if call is not yet sure to be reachable we stay as ANY *)
          Types.Tuple (Value [ DeadControl; Memory; ANY ])
        else
          let fun_ptr = Graph.get_dependency g call 1 |> Option.value_exn in
          let fun_typ = fun_ptr.typ in
          match fun_typ with
          | FunPtr (Value { params = _; ret; fun_indices = _ }) ->
              (* use call type so if call is reachable (i.e is Control) callend is
             also reachable but if call is unknown (i.e. ANY) then callend is
             also unknown for now *)
              (* TODO: once we have actual function pointers for indirect function
              calling FunctionCallEnd will be linked to all return nodes of the
              possible functions. In this case we'll only want to do the
              Types.meet for precise return type calculation once all possible
              functions are already linked *)
              let ret_nodes = Graph.get_dependencies g n |> List.tl_exn |> List.filter_opt in
              let ret_type =
                  List.fold ret_nodes ~init:Types.ANY ~f:(fun acc ret_node ->
                      let ret_node_ret_type =
                          match ret_node.typ with
                          | Tuple (Value [ _; _; ret_typ ]) -> ret_typ
                          | _ -> ANY
                      in
                      Types.meet acc ret_node_ret_type)
              in
              Types.Tuple (Value [ call.typ; Memory; Types.meet ret ret_type ])
          | _ -> failwith "Function ptr isn't known"
    in
    (~new_type, ~extra_deps:[])
