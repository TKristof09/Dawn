let create g (n, n') =
    let node = Node.create_ctrl Control Region in
    Graph.add_dependencies g node [ n; n' ];
    node

let create_loop g n =
    let node = Node.create_ctrl Control Region in
    Graph.add_dependencies g node [ n ];
    node

let set_loop_back_edge g n back = Graph.add_dependencies g n [ back ]
