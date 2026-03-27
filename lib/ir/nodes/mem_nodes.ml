let create_new g loc ~ctrl ~mem ~size typ =
    let node_type = Types.Tuple (Value [ Memory; typ ]) in
    let n = Node.create_mem loc node_type New in
    Graph.add_dependencies g n [ Some ctrl; Some mem; Some size ];
    Graph.finalize_node g n

let create_store (g : (Node.t, Graph.readwrite) Graph.t) loc ~mem ~(ptr : Node.t) ~offset field_name
    ~value =
    let n = Node.create_mem loc Memory (Store field_name) in
    Graph.add_dependencies g n [ None; Some mem; Some ptr; Some offset; Some value ];
    Graph.finalize_node g n

let create_load g loc ~mem ~(ptr : Node.t) field_name ~offset field_typ =
    let n = Node.create_mem loc field_typ (Load field_name) in
    Graph.add_dependencies g n [ None; Some mem; Some ptr; Some offset ];
    Graph.finalize_node g n

let create_addr_of g loc (n : Node.t) =
    let ptr = Node.create_mem loc (Ptr n.typ) AddrOf in
    Graph.add_dependencies g ptr [ None; Some n ];
    Graph.finalize_node g ptr
