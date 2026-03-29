let create g loc ?parent_fun typ name =
    let n = Node.create_data ?parent_fun loc typ (External name) in
    Graph.add_dependencies g n [ None ];
    Graph.finalize_node g n
