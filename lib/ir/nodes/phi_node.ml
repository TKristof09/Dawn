let create g ctrl nodes =
    let typ = Types.Tuple (Value (Core.List.map nodes ~f:(fun (n : Node.t) -> n.typ))) in
    let n = Node.create_data typ (Phi (ctrl, nodes)) in
    Graph.add_dependencies g n (ctrl :: nodes);
    n

let add_input g (n : Node.t) ins =
    match n.typ with
    | Types.Tuple (Value l) ->
        n.typ <- Types.Tuple (Value (Core.List.map ins ~f:(fun (input : Node.t) -> input.typ) @ l));
        Graph.add_dependencies g n ins
    | _ ->
        Printf.eprintf "Unexpected type for phi node: %s\n" (Types.show_node_type n.typ);
        Graph.add_dependencies g n ins
