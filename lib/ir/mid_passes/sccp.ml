open Core

module NodeSet = struct
  include Set.Make_plain (struct
    include Node
    include Comparator.Make (Node)
  end)
end

let set_type g (n : Node.t) new_type =
    (* We can't assert monotonicity because for integers we track a minimum
       type (= max bit width) which raises monotonically during sccp. The node
       type then gets clamped to this min type. So node type can actually raise
       in these cases. But since this min type moves monotonically it will terminate
       at some point and from then on the node type drops monotonically so the whole
       SCCP terminates *)
    n.typ <- new_type;
    Graph.get_dependants g n

let backwards_prop_min_integer_type min_integer_types g n min_type =
    assert (Types.is_a min_type (Integer All));

    let update min_type n =
        let changed = ref false in
        Hashtbl.update min_integer_types n ~f:(function
          | None ->
              changed := true;
              min_type
          | Some t ->
              let t_new = Types.join t min_type in
              changed := not (Types.equal t t_new);
              t_new);
        !changed
    in
    match n.Node.kind with
    | Data Add
    | Data Sub
    | Data Mul
    | Data Div
    | Data Phi
    | Data BAnd
    | Data BOr
    | Data (Param _) ->
        ignore (update min_type n);
        Graph.get_dependencies g n
        |> List.tl_exn
        |> List.filter_opt
        |> List.filter ~f:(fun n -> Option.is_none n.min_typ)
        |> List.filter ~f:(update min_type)
    | Data Lsh
    | Data Rsh ->
        ignore (update min_type n);
        let lhs = Graph.get_dependency g n 1 |> Option.value_exn in
        let rhs = Graph.get_dependency g n 2 |> Option.value_exn in
        let changed1 = update min_type lhs in
        (* TODO: rhs of shift needs to be smaller (e.g. if lhs is i32 then rhs has to be u5) *)
        let changed2 = update min_type rhs in
        let l = if changed1 then [ lhs ] else [] in
        if changed2 then rhs :: l else l
    | _ -> []

let work linker extra_node_deps min_integer_types (g : (Node.t, Graph.readwrite) Graph.t)
    (n : Node.t) ~type_fn =
    let ~new_type, ~extra_deps = type_fn (Graph.readonly g) n in

    let integer_min_type_changes =
        match Hashtbl.find min_integer_types n with
        | Some min_type -> backwards_prop_min_integer_type min_integer_types g n min_type
        | None -> (
            match n.min_typ with
            | Some (Integer _ as min_type) ->
                backwards_prop_min_integer_type min_integer_types g n min_type
            | _ -> [])
    in

    List.iter extra_deps ~f:(fun d ->
        Hashtbl.update extra_node_deps d ~f:(function
          | None -> NodeSet.singleton n
          | Some s -> Set.add s n));

    let new_type =
        match new_type with
        | Types.Integer (Value new_i) -> (
            match Hashtbl.find min_integer_types n with
            | None -> new_type
            | Some (Integer (Value i) as min_type) ->
                (* If the type fits in the min_type's range then leave it alone
                   otherwise use min_type. This is to constrain integers to
                   some width because otherwise when a phi node at a loop head
                   fully widens then uses of that phi can go outside the range.
                   E.g. in (i is i32) while(i <= 10) { i+= 1} the phi fully
                   widens to i32 range but the add still executes so it's range
                   becomes [i32_min + 1; i32_max + 1] which is bad. *)
                if Z.leq i.min new_i.min && Z.leq new_i.max i.max then
                  Types.make_int ~num_widens:new_i.num_widens ?fixed_width:i.fixed_width new_i.min
                    new_i.max
                else
                  min_type
            | Some (Integer All) ->
                (* this is full i64 range *)
                Types.make_int ~num_widens:new_i.num_widens ~fixed_width:64 new_i.min new_i.max
            | Some _ -> assert false)
        | Integer All -> (
            (* try to constrain Integer All to some fixed width if we know the width we want *)
            match Hashtbl.find min_integer_types n with
            | None -> new_type
            | Some min_type -> min_type)
        | _ -> new_type
    in
    let new_work =
        if Types.equal new_type n.typ then
          []
        else
          let new_work = set_type g n new_type in
          let new_fun_links =
              match new_type with
              | FunPtr _ ->
                  Graph.get_dependants g n
                  |> List.filter ~f:(fun dep ->
                      match dep.kind with
                      | Ctrl FunctionCall -> Node.equal (Fun_node.get_call_fun_ptr g dep) n
                      | _ -> false)
                  |> List.fold ~init:[] ~f:(fun acc dep ->
                      let fun_nodes = Linker.link linker g dep in
                      List.fold fun_nodes ~init:acc ~f:(fun acc fun_node ->
                          let params =
                              Graph.get_dependants g fun_node
                              |> List.filter ~f:(fun n ->
                                  match n.kind with
                                  | Data (Param _) -> true
                                  | _ -> false)
                          in
                          (fun_node :: params) @ acc))
              | _ -> []
          in
          let extras = Hashtbl.find extra_node_deps n |> Option.value ~default:NodeSet.empty in
          new_fun_links @ Set.to_list extras @ new_work
    in
    integer_min_type_changes @ new_work

let do_data_node linker extra_node_deps min_integer_types g n (k : Node.data_kind) =
    match k with
    | Constant -> (* constant stays the same as it was *) []
    | Add
    | Sub
    | Mul
    | Div ->
        work linker extra_node_deps min_integer_types g n ~type_fn:Arithmetic_nodes.compute_type
    | Lsh
    | Rsh
    | BAnd
    | BOr ->
        work linker extra_node_deps min_integer_types g n ~type_fn:Bitop_nodes.compute_type
    | Proj _ -> work linker extra_node_deps min_integer_types g n ~type_fn:Proj_node.compute_type
    | Eq
    | NEq
    | Lt
    | LEq
    | Gt
    | GEq ->
        work linker extra_node_deps min_integer_types g n ~type_fn:Bool_nodes.compute_type
    | Phi -> work linker extra_node_deps min_integer_types g n ~type_fn:Phi_node.compute_type
    | Param _ ->
        (* params are just fancy phi nodes so this should work the same *)
        work linker extra_node_deps min_integer_types g n ~type_fn:Phi_node.compute_type
    | External _ -> (* this is just like a constant *) []
    | Cast -> failwith "TODO"

let do_ctrl_node linker extra_node_deps min_integer_types g n (c : Node.ctrl_kind) =
    match c with
    | Start ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun _ _ ->
            (~new_type:(Types.Tuple (Value [ Control; Memory ])), ~extra_deps:[]))
    | Stop ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun _ _ ->
            (~new_type:Types.Control, ~extra_deps:[]))
    | Proj _ -> work linker extra_node_deps min_integer_types g n ~type_fn:Proj_node.compute_type
    | If -> work linker extra_node_deps min_integer_types g n ~type_fn:If_node.compute_type
    | Region -> work linker extra_node_deps min_integer_types g n ~type_fn:Region_node.compute_type
    | Loop ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun g n ->
            let entry = Loop_node.get_entry_edge g n in
            (~new_type:entry.typ, ~extra_deps:[]))
    | Function _ ->
        work linker extra_node_deps min_integer_types g n ~type_fn:Fun_node.compute_fun_node_type
    | Return ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun g n ->
            let ctrl = Graph.get_dependency g n 0 |> Option.value_exn in
            let mem = Graph.get_dependency g n 1 |> Option.value_exn in
            let data = Graph.get_dependency g n 2 |> Option.value_exn in
            let new_type = Types.Tuple (Value [ ctrl.typ; mem.typ; data.typ ]) in
            (~new_type, ~extra_deps:[]))
    | FunctionCall ->
        let old_type = n.typ in
        let new_type =
            let ctrl = Graph.get_dependency g n 0 |> Option.value_exn in
            ctrl.typ
        in
        let fun_ptr = Graph.get_dependency g n 1 |> Option.value_exn in
        (* link calls to function when it just became reachable *)
        let param_work =
            if
              (not (Types.equal old_type Control))
              && Types.equal new_type Control
              && (not (Types.equal fun_ptr.typ ANY))
              && Types.is_a fun_ptr.typ (FunPtr All)
            then
              let fun_nodes = Linker.link linker g n in
              List.fold fun_nodes ~init:[] ~f:(fun acc fun_node ->
                  let params =
                      Graph.get_dependants g fun_node
                      |> List.filter ~f:(fun n ->
                          match n.kind with
                          | Data (Param _) -> true
                          | _ -> false)
                  in
                  (fun_node :: params) @ acc)
            else
              []
        in
        param_work
        @ work linker extra_node_deps min_integer_types g n ~type_fn:(fun _ _ ->
            (~new_type, ~extra_deps:[]))
    | FunctionCallEnd ->
        work linker extra_node_deps min_integer_types g n ~type_fn:Fun_node.compute_call_end_type

let do_mem_node linker extra_node_deps min_integer_types g n (m : Node.mem_kind) =
    match m with
    | Load field ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun g n ->
            let ptr = Graph.get_dependency g n 2 |> Option.value_exn in
            match ptr.typ with
            (* TODO remove this *)
            (* | Struct (Value { name = _; fields }) *)
            (* | Ptr (Struct (Value { name = _; fields })) -> ( *)
            (*     let field_type = Types.get_field_type ptr.typ field in *)
            (*     match field_type with *)
            (*     | None -> (~new_type:ALL, ~extra_deps:[]) *)
            (*     | Some (ConstArray (Value arr)) -> ( *)
            (*         let offs = Graph.get_dependency g n 3 |> Option.value_exn in *)
            (*         match offs.typ with *)
            (*         | Integer _ when Types.is_constant offs.typ -> *)
            (*             let i = Types.get_integer_const_exn offs.typ in *)
            (*             (* TODO: bounds check on the idx would be nice *) *)
            (*             let idx = (Z.to_int i - 8) / Types.get_size arr.element_type in *)
            (*             (~new_type:(List.nth_exn (arr.values :> Types.t list) idx), ~extra_deps:[]) *)
            (*         | _ -> (~new_type:arr.element_type, ~extra_deps:[])) *)
            (*     | Some field_type -> (~new_type:field_type, ~extra_deps:[])) *)
            | Ptr p -> (~new_type:p, ~extra_deps:[])
            | ANY -> (~new_type:ANY, ~extra_deps:[])
            | ALL -> (~new_type:ALL, ~extra_deps:[])
            | _ -> assert false)
    | Store _ ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun _ _ ->
            (~new_type:Memory, ~extra_deps:[]))
    | New -> []
    | AddrOf ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun g n ->
            let input = Graph.get_dependency g n 1 |> Option.value_exn in
            (~new_type:(Ptr input.typ), ~extra_deps:[]))
    | AddrOfField field ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun g n ->
            let input = Graph.get_dependency g n 1 |> Option.value_exn in
            match input.typ with
            | Struct _ ->
                let t = Types.get_field_type input.typ field |> Option.value_exn in
                (~new_type:(Ptr t), ~extra_deps:[])
            | ANY -> (~new_type:(Ptr ANY), ~extra_deps:[])
            | _ -> (~new_type:(Ptr ALL), ~extra_deps:[]))
    | Deref ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun g n ->
            let input = Graph.get_dependency g n 2 |> Option.value_exn in
            let t =
                match input.typ with
                | Ptr p -> p
                | ANY -> ANY
                | _ -> ALL
            in
            (~new_type:t, ~extra_deps:[]))
    | Copy ->
        work linker extra_node_deps min_integer_types g n ~type_fn:(fun _ _ ->
            (~new_type:Memory, ~extra_deps:[]))

let do_node extra_node_deps min_integer_types (g : (Node.t, Graph.readwrite) Graph.t) linker
    (n : Node.t) =
    match n.kind with
    | Data d -> do_data_node linker extra_node_deps min_integer_types g n d
    | Ctrl c -> do_ctrl_node linker extra_node_deps min_integer_types g n c
    | Scope _ -> []
    | Mem m -> do_mem_node linker extra_node_deps min_integer_types g n m
    | ForwardRef _ ->
        (* ignore these, they will produce an error in type checking *)
        []

let run g linker =
    let worklist = Queue.create ~capacity:(Graph.get_num_nodes g) () in
    Graph.iter g ~f:(fun (n : Node.t) ->
        (* TODO: I really dont like this type of skipping some kinds, looks like a mess waiting to happen *)
        (match n.kind with
        | Data Constant
        | Data (External _) ->
            (* constants keep their original type since these are fixed and dont need any type inference *)
            ()
        | Mem New ->
            (* new needs to keep the type it will produce *)
            ()
        | _ -> n.typ <- ANY);
        Queue.enqueue worklist n);

    let fun_nodes =
        Graph.fold g ~init:[] ~f:(fun acc n ->
            match n.kind with
            | Ctrl (Function _) -> n :: acc
            | _ -> acc)
    in
    (* unlink start from function nodes as we only want to care about functions if someone actually callls them, the start->function node connection was just for convenience *)
    List.iter fun_nodes ~f:(fun n ->
        let start = Graph.get_start g in
        Graph.remove_dependency g ~node:n ~dep:start);

    let extra_node_deps = Hashtbl.create (module Node) in
    let min_integer_types = Hashtbl.create (module Node) in
    let rec loop i =
        match Queue.dequeue worklist with
        | None -> i
        | Some n ->
            let news = do_node extra_node_deps min_integer_types g linker n in
            Queue.enqueue_all worklist news;
            loop (i + 1)
    in
    let num_iters = loop 0 in
    [%log.debug "SCCP took %d iters" num_iters];
    (* relink function nodes to start because we'll unlink them from FunctionCalls during scheduling so if they arent linked to start they'd be unreachable i think *)
    List.iter fun_nodes ~f:(fun n ->
        let start = Graph.get_start g in
        Graph.set_dependency g n (Some start) 0)
