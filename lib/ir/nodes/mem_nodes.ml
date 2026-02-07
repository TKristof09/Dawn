let create_new g ~ctrl ~mem ~count typ =
    let node_type = Types.Tuple (Value [ Memory; typ ]) in
    let n = Node.create_mem node_type New in
    Graph.add_dependencies g n [ Some ctrl; Some mem; Some count ];
    Graph.finalize_node g n

let create_store (g : (Node.t, Graph.readwrite) Graph.t) ~mem ~(ptr : Node.t) ~offset ~value =
    let n = Node.create_mem ptr.typ Store in
    Graph.add_dependencies g n [ None; Some mem; Some ptr; Some offset; Some value ];
    Graph.finalize_node g n

let create_load g ~mem ~(ptr : Node.t) ~offset =
    let n = Node.create_mem ptr.typ Load in
    Graph.add_dependencies g n [ None; Some mem; Some ptr; Some offset ];
    Graph.finalize_node g n
