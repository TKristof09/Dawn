let create g ctrl nodes =
    match nodes with
    | [ _ ] -> assert false
    | _ ->
        let n =
            let typ =
                Core.List.map nodes ~f:(fun (n : Node.t) -> n.typ)
                |> Core.List.reduce_exn ~f:Types.join
            in
            let n = Node.create_data typ Phi in
            Graph.add_dependencies g n (List.map Option.some (ctrl :: nodes));
            n
        in
        Graph.finalize_node g n

let get_ctrl g n = Graph.get_dependencies g n |> Core.List.hd_exn |> Core.Option.value_exn

let create_no_backedge g ctrl (dep : Node.t) =
    let typ = dep.typ in
    let n = Node.create_data typ Phi in
    Graph.add_dependencies g n [ Some ctrl; None; Some dep ];
    n

let add_backedge_input g (n : Node.t) (dep : Node.t) =
    (* keep the phi's original type as the type, it will get recomputed by the
       scope_node.merge_loop function after all phis have both their inputs *)
    Graph.set_dependency g n (Some dep) 1

let compute_type g phi =
    let typ =
        Graph.get_dependencies g phi
        |> Core.List.filter_opt
        |> Core.List.map ~f:(fun (n : Node.t) -> n.typ)
        |> Core.List.reduce_exn ~f:Types.join
    in
    phi.typ <- typ
