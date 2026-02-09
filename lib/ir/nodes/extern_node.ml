let create g loc typ name =
    let n = Node.create_data loc typ (External name) in
    Graph.add_dependencies g n [ None ];
    Graph.finalize_node g n
