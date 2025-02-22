let create g n =
    let stop_node = Node.create_ctrl BOTTOM Stop in
    Graph.add_dependencies g stop_node [ n ];
    stop_node
