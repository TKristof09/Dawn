let create_new g loc ?parent_fun ~ctrl ~mem ~size typ =
    let node_type = Types.Tuple (Value [ Memory; typ ]) in
    let n = Node.create_mem ?parent_fun loc node_type New in
    Graph.add_dependencies g n [ Some ctrl; Some mem; Some size ];
    Graph.finalize_node g n

let create_store g loc ?parent_fun ~mem ~(ptr : Node.t) ~offset field_name ~value =
    let n = Node.create_mem ?parent_fun loc Memory (Store field_name) in
    Graph.add_dependencies g n [ None; Some mem; Some ptr; Some offset; Some value ];
    Graph.finalize_node g n

let create_load g loc ?parent_fun ~mem ~(ptr : Node.t) field_name ~offset field_typ =
    let n = Node.create_mem ?parent_fun loc field_typ (Load field_name) in
    Graph.add_dependencies g n [ None; Some mem; Some ptr; Some offset ];
    Graph.finalize_node g n

let create_addr_of g loc ?parent_fun (n : Node.t) =
    let ptr = Node.create_mem ?parent_fun loc (Ptr n.typ) AddrOf in
    Graph.add_dependencies g ptr [ None; Some n ];
    Graph.finalize_node g ptr
