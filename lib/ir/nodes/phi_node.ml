let create g ctrl nodes =
    let typ = Types.Tuple (Value (Core.List.map nodes ~f:(fun (n : Node.t) -> n.typ))) in
    let n = Node.create_data typ (Phi (ctrl, nodes)) in
    Graph.add_dependencies g n (ctrl :: nodes);
    n
