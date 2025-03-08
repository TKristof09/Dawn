let create g ctrl nodes =
    match nodes with
    | [ _ ] -> assert false
    | _ ->
        let n =
            let typ = Types.Tuple (Value (Core.List.map nodes ~f:(fun (n : Node.t) -> n.typ))) in
            let n = Node.create_data typ Phi in
            Graph.add_dependencies g n (List.map Option.some (ctrl :: nodes));
            n
        in
        Graph.finalize_node g n

let get_ctrl g n = Graph.get_dependencies g n |> Core.List.hd_exn |> Core.Option.value_exn

let create_half g ctrl (dep : Node.t) =
    let n =
        let typ = Types.Tuple (Value [ dep.typ ]) in
        let n = Node.create_data typ Phi in
        Graph.add_dependencies g n [ Some ctrl; None; Some dep ];
        n
    in
    n

let add_input g (n : Node.t) (dep : Node.t) =
    match n.typ with
    | Types.Tuple (Value l) ->
        n.typ <- Types.Tuple (Value (dep.typ :: l));
        Graph.set_dependency g n (Some dep) 1
    | _ ->
        Printf.eprintf "Unexpected type for phi node: %s\n" (Types.show_node_type n.typ);
        assert false
