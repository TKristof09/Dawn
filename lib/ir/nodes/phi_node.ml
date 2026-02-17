open Core

let create g loc ctrl nodes =
    match nodes with
    | [ _ ] -> assert false
    | _ ->
        let n =
            let typ =
                List.map nodes ~f:(fun (n : Node.t) -> n.typ) |> List.reduce_exn ~f:Types.meet
            in
            let n = Node.create_data loc typ Phi in
            Graph.add_dependencies g n (List.map (ctrl :: nodes) ~f:Option.some);
            n
        in
        Graph.finalize_node g n

let get_ctrl g n = Graph.get_dependencies g n |> Core.List.hd_exn |> Core.Option.value_exn

let create_no_backedge g loc ctrl (dep : Node.t) =
    let typ = dep.typ in
    let n = Node.create_data loc typ Phi in
    Graph.add_dependencies g n [ Some ctrl; None; Some dep ];
    n

let add_backedge_input g (n : Node.t) (dep : Node.t) =
    (* keep the phi's original type as the type, it will get recomputed by the
       scope_node.merge_loop function after all phis have both their inputs *)
    Graph.set_dependency g n (Some dep) 1

let compute_type g n =
    let in_control =
        Graph.get_dependency g n 0 |> Option.value_exn |> Graph.get_dependencies g |> List.tl_exn
    in
    let in_data = Graph.get_dependencies g n |> List.tl_exn in
    let new_type =
        List.zip_exn in_control in_data
        |> List.filter_map ~f:(fun (c, d) ->
            match (c, d) with
            | Some c, Some d -> (
                match c.Node.typ with
                | DeadControl -> None
                | Control -> Some d.typ
                | ANY -> None
                | _ -> None)
            | _ -> None)
        |> List.fold ~init:Types.ANY ~f:(fun t t' -> Types.meet t t')
    in
    (* TODO: join with the initial type so that the type stays in the original "domain" e.g. it stays int bottom instead of global bottom. this would probably help produce better errors *)
    (~new_type, ~extra_deps:(in_control |> List.filter_opt))

