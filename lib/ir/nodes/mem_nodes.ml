let create_new g ~ctrl ~mem ~size typ =
    let node_type = Types.Tuple (Value [ Memory; Ptr typ ]) in
    let n = Node.create_mem node_type New in
    Graph.add_dependencies g n [ Some ctrl; Some mem; Some size ];
    Graph.finalize_node g n

let create_store (g : (Node.t, Graph.readwrite) Graph.t) ~mem ~(ptr : Node.t) ~offset ~value =
    let n = Node.create_mem Memory Store in
    Graph.add_dependencies g n [ None; Some mem; Some ptr; Some offset; Some value ];
    Graph.finalize_node g n

let create_load g ~mem ~(ptr : Node.t) ~offset =
    let n = Node.create_mem ptr.typ Load in
    Graph.add_dependencies g n [ None; Some mem; Some ptr; Some offset ];
    Graph.finalize_node g n
