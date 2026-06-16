let create g loc ?parent_fun typ name params =
    let n = Node2.create_data ?parent_fun loc typ (External name) in
    Node2.G.add_node g n { params = Core.List.map params ~f:(fun n -> Node2.AnyData n) };
    n
