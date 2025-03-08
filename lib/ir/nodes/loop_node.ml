let create g n =
    let node = Node.create_ctrl Control Loop in
    Graph.add_dependencies g node [ None; Some n ];
    node

let set_back_edge g n back = Graph.set_dependency g n (Some back) 0
