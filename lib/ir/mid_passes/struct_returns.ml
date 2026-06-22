open Core

(* For every function that returns a struct, patch up call sites to allocate
   stack space and pass a hidden pointer as the first argument, and patch the
   function body to copy the result into that pointer. This is the SystemV ABI
   convention for struct returns. 
   Doing this as a separate pass (instead of during IR construction) allows
   recursive functions to work correctly, since at IR construction time the
   caller's fun_ptr may only be a forward reference with no known type. *)

let has_big_return signature =
    (* A function has big return type when make_fun_ptr added a Ptr ret param as first param
and it matches the declared return type. *)
    match signature with
    | Types.FunPtr (Value { params; ret; fun_indices = _ }) -> (
        match List.hd params with
        | Some (Ptr (Struct _ as s) as t) when Types.equal t ret -> Some s
        | _ -> None)
    | _ -> None

(* Allocate hidden return buffer, prepend ptr as first arg, and add a deref for the returned pointer. *)
let patch_call_site g call_node deref_ret_type =
    [%log.debug "Patching up struct return for %a" Node.pp call_node];
    let parent_fun = call_node.parent_fun in
    let loc = call_node.loc in
    let { Node.fun_ptr; mem = call_mem; args } = Node.G.get_dependencies_exn g call_node in
    let (Node.AnyMem call_mem) = Option.value_exn call_mem in

    (* Allocate stack space for the return struct *)
    let (Node.AnyNode ctrl_any) = Node.G.get_ctrl_exn g call_node in
    let ctrl = Node.as_ctrl_exn ctrl_any in
    let size = Types.get_size deref_ret_type |> Const_node.create_int ?parent_fun g loc in
    let new_node =
        Mem_nodes.create_new ?parent_fun g loc ~ctrl ~mem:call_mem ~size deref_ret_type
    in
    let alloc_mem = Proj_node.create_mem ?parent_fun g loc new_node 0 in
    let ret_struct = Proj_node.create_data ?parent_fun g loc new_node 1 in
    ret_struct.min_typ <- Some deref_ret_type;
    let ret_ptr = Mem_nodes.create_addr_of ?parent_fun g loc ret_struct in

    let new_args = Some (Node.AnyData ret_ptr) :: args in
    Node.G.set_node_inputs g call_node
      { Node.fun_ptr; mem = Some (Node.AnyMem alloc_mem); args = new_args };

    (* Create deref of the return value so that downstream users still see the
       actual struct type not just a pointer. *)
    let (Node.AnyNode call_end) =
        Node.G.get_dependants g call_node
        |> List.find_map ~f:(fun (Node.AnyNode n) ->
            match n.kind with
            | Ctrl FunctionCallEnd -> Some (Node.AnyNode n)
            | _ -> None)
        |> Option.value_exn
    in
    let call_end = Node.unpack_exn call_end (Ctrl FunctionCallEnd) in
    let ~new_type, ~extra_deps = Fun_node.compute_call_end_type (Node.G.readonly g) call_end in
    call_end.typ <- new_type;

    let data_proj =
        Node.G.get_dependants g call_end
        |> List.find_map
             ~f:(fun (Node.AnyNode n) : (Node.any Node.unary, Node.data) Node.t option ->
               match n.kind with
               | Data (Proj _) -> Some n
               | _ -> None)
        |> Option.value_exn
    in
    let ~new_type, ~extra_deps = Proj_node.compute_type (Node.G.readonly g) data_proj in
    data_proj.typ <- new_type;

    let old_users = Node.G.get_dependants g data_proj in

    let mem_proj =
        Node.G.get_dependants g call_end
        |> List.find_map ~f:(fun (Node.AnyNode n) : (Node.any Node.unary, Node.mem) Node.t option ->
            match n.kind with
            | Mem (Proj _) -> Some n
            | _ -> None)
        |> Option.value_exn
    in
    let ~new_type, ~extra_deps = Proj_node.compute_type (Node.G.readonly g) data_proj in
    data_proj.typ <- new_type;
    let deref = Mem_nodes.create_deref ?parent_fun g loc ~mem:mem_proj data_proj in

    List.iter old_users ~f:(fun (Node.AnyNode user) ->
        Node.G.replace_input_unsafe g ~node:user ~from:(Node.AnyNode data_proj)
          ~to_:(Node.AnyNode deref))

(* Create param for the incoming ret ptr then for each return site, insert a
   copy of the return value into the hidden ret pointer and return the pointer
   instead. *)
let patch_function_body g fun_node deref_ret_type =
    [%log.debug "Patching up struct return for %a" Node.pp fun_node];
    let parent_fun = fun_node.Node.parent_fun in
    let (Ctrl (Function { ret = ret_node; signature = _; idx = fun_idx })) = fun_node.kind in
    let loc = fun_node.loc in

    let ret_ptr_param =
        Fun_node.create_param g fun_node.loc ~parent_fun:fun_idx fun_node (Ptr deref_ret_type) 0
    in

    let { Node.mem = phi_mem; data = phi_data } = Node.G.get_dependencies_exn g ret_node in
    let (Node.AnyMem phi_mem) = Option.value_exn phi_mem in
    let (Node.AnyData phi_data) = Option.value_exn phi_data in
    let phi_mem = Node.unpack_exn phi_mem (Node.Mem Phi) in
    let phi_data = Node.unpack_exn phi_data (Node.Data Phi) in

    let { Node.phi_inputs = data_inputs } = Node.G.get_dependencies_exn g phi_data in
    let { Node.phi_inputs = mem_inputs } = Node.G.get_dependencies_exn g phi_mem in

    let (Node.AnyNode region) = Node.G.get_ctrl_exn g ret_node in
    let region = Node.unpack_exn region (Node.Ctrl Region) in
    let { Node.ctrl_inputs } = Node.G.get_dependencies_exn g region in

    (* For each return site, insert copy and update inputs *)
    let new_inputs =
        match
          List.map3 data_inputs mem_inputs ctrl_inputs ~f:(fun di mi ci ->
              let (Node.AnyData val_n) = Option.value_exn di in
              let (Node.AnyMem mem) = Option.value_exn mi in
              let (Node.AnyCtrl ctrl) = Option.value_exn ci in

              (* Create addr_of body value, then copy into $ret *)
              let val_addr = Mem_nodes.create_addr_of ?parent_fun g loc val_n in

              let copy =
                  Mem_nodes.create_copy ?parent_fun g loc ~mem ~src:val_addr ~dst:ret_ptr_param
              in

              (Some (Node.AnyData ret_ptr_param), Some (Node.AnyMem copy), Some (Node.AnyCtrl ctrl)))
        with
        | Ok inputs -> inputs
        | Unequal_lengths ->
            failwith "data_inputs, mem_inputs, and ctrl_inputs should have equal lengths"
    in
    let new_data_inputs = List.map new_inputs ~f:(fun (d, _, _) -> d) in
    let new_mem_inputs = List.map new_inputs ~f:(fun (_, m, _) -> m) in
    let new_ctrl_inputs = List.map new_inputs ~f:(fun (_, _, c) -> c) in

    (* Update phi nodes and region *)
    Node.G.set_node_inputs g phi_data { Node.phi_inputs = new_data_inputs };
    Node.G.set_node_inputs g phi_mem { Node.phi_inputs = new_mem_inputs };
    Node.G.set_node_inputs g region { Node.ctrl_inputs = new_ctrl_inputs }

let run g =
    let nodes =
        Node.G.fold g ~init:[] ~f:(fun acc (Node.AnyNode n) ->
            match n.kind with
            | Ctrl (Function _) -> (
                let signature = Fun_node.get_signature n in
                match has_big_return signature with
                | None -> acc
                | Some deref_ret_type -> (Node.AnyCtrl n, deref_ret_type) :: acc)
            | Ctrl FunctionCall -> (
                let (AnyData fun_ptr) = Fun_node.get_call_fun_ptr g n in
                match has_big_return fun_ptr.typ with
                | None -> acc
                | Some deref_ret_type -> (Node.AnyCtrl n, deref_ret_type) :: acc)
            | _ -> acc)
    in
    (* NOTE: it is fine to iterate in whatever order (even if the call site is
       before the function def) because patching up the call site doesnt
       actually link the ret ptr argument to the param node. This linking is
       done during sccp. *)
    List.iter nodes ~f:(fun (AnyCtrl n, deref_ret_type) ->
        match n.kind with
        | Ctrl (Function x) -> patch_function_body g n deref_ret_type
        | Ctrl FunctionCall -> patch_call_site g n deref_ret_type
        | _ -> assert false)
