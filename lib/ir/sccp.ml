open Core

module NodeSet = struct
  include Set.Make_plain (struct
    include Node
    include Comparator.Make (Node)
  end)
end

let set_type g (n : Node.t) new_type =
    (* assert monotonicity of transfer function which should always refine the
        type or keep it same types should always drop downwards in the lattice, we
        start from top (Any) and drop types as more constraints on them get discovered
        *)
    assert (Types.is_a n.typ new_type);
    n.typ <- new_type;
    Graph.get_dependants g n

let work extra_node_deps (g : (Node.t, Graph.readwrite) Graph.t) (n : Node.t) ~type_fn =
    let ~new_type, ~extra_deps = type_fn (Graph.readonly g) n in
    List.iter extra_deps ~f:(fun d ->
        Hashtbl.update extra_node_deps d ~f:(function
          | None -> NodeSet.singleton n
          | Some s -> Set.add s n));
    let new_work =
        if Types.equal new_type n.typ then
          []
        else
          let extras = Hashtbl.find extra_node_deps n |> Option.value ~default:NodeSet.empty in
          Set.to_list extras @ set_type g n new_type
    in
    new_work

let do_data_node extra_node_deps g n (k : Node.data_kind) =
    match k with
    | Constant -> (* constant stays the same as it was *) []
    | Add
    | Sub
    | Mul
    | Div ->
        work extra_node_deps g n ~type_fn:Arithmetic_nodes.compute_type
    | Lsh
    | Rsh
    | BAnd
    | BOr ->
        work extra_node_deps g n ~type_fn:Bitop_nodes.compute_type
    | Proj _ -> work extra_node_deps g n ~type_fn:Proj_node.compute_type
    | Eq
    | NEq
    | Lt
    | LEq
    | Gt
    | GEq ->
        work extra_node_deps g n ~type_fn:Bool_nodes.compute_type
    | Phi -> work extra_node_deps g n ~type_fn:Phi_node.compute_type
    | Param _ ->
        (* params are just fancy phi nodes so this should work the same *)
        work extra_node_deps g n ~type_fn:Phi_node.compute_type
    | External _ -> (* this is just like a constant *) []

let do_ctrl_node extra_node_deps g n (c : Node.ctrl_kind) =
    match c with
    | Start ->
        work extra_node_deps g n ~type_fn:(fun _ _ ->
            (~new_type:(Types.Tuple (Value [ Control; Memory ])), ~extra_deps:[]))
    | Stop ->
        work extra_node_deps g n ~type_fn:(fun _ _ -> (~new_type:Types.Control, ~extra_deps:[]))
    | Proj _ -> work extra_node_deps g n ~type_fn:Proj_node.compute_type
    | If -> work extra_node_deps g n ~type_fn:If_node.compute_type
    | Region -> work extra_node_deps g n ~type_fn:Region_node.compute_type
    | Loop ->
        work extra_node_deps g n ~type_fn:(fun g n ->
            let entry = Loop_node.get_entry_edge g n in
            (~new_type:entry.typ, ~extra_deps:[]))
    | Function _ -> work extra_node_deps g n ~type_fn:Fun_node.compute_fun_node_type
    | Return ->
        work extra_node_deps g n ~type_fn:(fun g n ->
            let ctrl = Graph.get_dependency g n 0 |> Option.value_exn in
            let data = Graph.get_dependency g n 1 |> Option.value_exn in
            let new_type = Types.Tuple (Value [ ctrl.typ; data.typ ]) in
            (~new_type, ~extra_deps:[]))
    | FunctionCall ->
        work extra_node_deps g n ~type_fn:(fun g n ->
            let ctrl = Graph.get_dependency g n 0 |> Option.value_exn in
            (~new_type:ctrl.typ, ~extra_deps:[]))
    | FunctionCallEnd -> work extra_node_deps g n ~type_fn:Fun_node.compute_call_end_type

let do_mem_node extra_node_deps g n (m : Node.mem_kind) =
    match m with
    | Load field ->
        work extra_node_deps g n ~type_fn:(fun g n ->
            let ptr = Graph.get_dependency g n 2 |> Option.value_exn in
            match ptr.typ with
            | Ptr (Struct (Value { name = _; fields })) -> (
                let field_type =
                    List.find_map fields ~f:(fun (name, t) ->
                        if String.equal field name then
                          Some t
                        else
                          None)
                in
                match field_type with
                | None -> (~new_type:ALL, ~extra_deps:[])
                | Some field_type -> (~new_type:field_type, ~extra_deps:[]))
            | ANY -> (~new_type:ANY, ~extra_deps:[])
            | ALL -> (~new_type:ALL, ~extra_deps:[])
            | _ -> assert false)
    | Store _ -> work extra_node_deps g n ~type_fn:(fun _ _ -> (~new_type:Memory, ~extra_deps:[]))
    | New -> []

let do_node extra_node_deps (g : (Node.t, Graph.readwrite) Graph.t) (n : Node.t) =
    match n.kind with
    | Data d -> do_data_node extra_node_deps g n d
    | Ctrl c -> do_ctrl_node extra_node_deps g n c
    | Scope _ -> []
    | Mem m -> do_mem_node extra_node_deps g n m

let run g =
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
    let rec loop i =
        match Queue.dequeue worklist with
        | None -> i
        | Some n ->
            let news = do_node extra_node_deps g n in
            Queue.enqueue_all worklist news;
            loop (i + 1)
    in
    let num_iters = loop 0 in
    [%log.debug "SCCP took %d iters" num_iters];
    (* relink function nodes to start because we'll unlink them from FunctionCalls during scheduling so if they arent linked to start they'd be unreachable i think *)
    List.iter fun_nodes ~f:(fun n ->
        let start = Graph.get_start g in
        Graph.set_dependency g n (Some start) 0)
