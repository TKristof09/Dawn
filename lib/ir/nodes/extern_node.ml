let create g typ name =
    let n = Node.create_data typ (External name) in
    Graph.add_dependencies g n [ None ];
    Graph.finalize_node g n
