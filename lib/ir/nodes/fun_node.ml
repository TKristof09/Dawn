open Core

let create g loc fun_ptr_type =
    let ret_typ =
        match fun_ptr_type with
        | Types.FunPtr (Value { params = _; ret; fun_indices = _ }) -> ret
        | _ -> failwithf "Expected function pointer type got %s" (Types.show fun_ptr_type) ()
    in
    let return_value = Node.create_data loc ret_typ Phi in
    let return_region = Node.create_ctrl loc Control Region in
    Graph.add_dependencies g return_region [ None ];
    Graph.add_dependencies g return_value [ Some return_region ];
    let ret_node_typ : Types.t =
        match return_value.typ with
        | Tuple (Value t) -> Tuple (Value (Control :: t))
        | v -> Tuple (Value [ Control; v ])
    in
    let ret_node = Node.create_ctrl loc ret_node_typ Return in
    Graph.add_dependencies g ret_node [ Some return_region; Some return_value ];
    let fun_node =
        Node.create_ctrl loc Control
          (Function { ret = ret_node; signature = fun_ptr_type; idx = -1 })
    in
    let start = Graph.get_start g in
    Graph.add_dependencies g fun_node [ Some start ];
    (fun_node, ret_node)

let create_param g loc fun_node param_type i =
    let n = Node.create_data loc param_type (Param i) in
    Graph.add_dependencies g n [ Some fun_node ];
    n

let add_call g loc ~ctrl ~fun_ptr args =
    let ret_typ =
        match fun_ptr.Node.typ with
        | FunPtr (Value { params = _; ret; fun_indices = _ }) -> ret
        | t -> failwithf "Expected function pointer got: %s" (Types.show t) ()
    in

    let call = Node.create_ctrl loc Control FunctionCall in
    Graph.add_dependencies g call (Some ctrl :: Some fun_ptr :: List.map args ~f:Option.some);
    let call_end = Node.create_ctrl loc (Tuple (Value [ Control; ret_typ ])) FunctionCallEnd in
    Graph.add_dependencies g call_end [ Some call ];
    (call, call_end)

let link_call g ~(call_node : Node.t) ~(fun_node : Node.t) =
    let ret_node =
        match fun_node.kind with
        | Ctrl (Function { ret; signature = _; idx = _ }) -> ret
        | _ -> assert false
    in
    let call_end =
        Graph.get_dependants g call_node
        |> List.find_exn ~f:(fun n ->
            match n.kind with
            | Ctrl FunctionCallEnd -> true
            | _ -> false)
    in
    Graph.add_dependencies g call_end [ Some ret_node ];
    Graph.add_dependencies g fun_node [ Some call_node ];
    let param_nodes =
        Graph.get_dependants g fun_node
        |> List.filter ~f:(fun n ->
            match n.kind with
            | Data (Param _) -> true
            | _ -> false)
    in
    (* drop ctrl and fun_ptr *)
    let args = List.drop (Graph.get_dependencies g call_node) 2 in
    List.zip_exn param_nodes args
    |> List.iter ~f:(fun (param, arg) -> Graph.add_dependencies g param [ arg ])

let add_return g ret_node ~ctrl ~val_n =
    let region = Graph.get_dependency g ret_node 0 |> Option.value_exn in
    let phi = Graph.get_dependency g ret_node 1 |> Option.value_exn in
    Graph.add_dependencies g phi [ Some val_n ];
    Graph.add_dependencies g region [ Some ctrl ];
    (* TODO do i care about this? we will set the types to ANY during SCCP anyway *)
    let ~new_type:typ, ~extra_deps:_ = Phi_node.compute_type g phi in
    phi.typ <- typ

let get_signature (n : Node.t) =
    match n.kind with
    | Ctrl (Function { ret = _; signature; idx = _ }) -> signature
    | _ -> failwithf "Invalid node kind %s" (Node.show n) ()

let get_call_fun_ptr g n = Graph.get_dependency g n 1 |> Option.value_exn

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
          Types.Tuple (Value [ DeadControl; ANY ])
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
              let ret_node = Graph.get_dependency g n 1 |> Option.value_exn in
              let ret_node_ret_type =
                  match ret_node.typ with
                  | Tuple (Value [ _; ret_typ ]) -> ret_typ
                  | _ -> ANY
              in
              Types.Tuple (Value [ call.typ; Types.meet ret ret_node_ret_type ])
          | _ -> failwith "Function ptr isn't known"
    in
    (~new_type, ~extra_deps:[])
