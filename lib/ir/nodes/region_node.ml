let create g (n, n') =
    let node = Node.create_ctrl Control Region in
    Graph.add_dependencies g node [ Some n; Some n' ];
    node
