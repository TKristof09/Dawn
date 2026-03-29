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

let create_addr_of_field g loc ?parent_fun (n : Node.t) field_name =
    let t = Types.get_field_type n.typ field_name |> Option.value ~default:ALL in
    let ptr = Node.create_mem ?parent_fun loc (Ptr t) (AddrOfField field_name) in
    Graph.add_dependencies g ptr [ None; Some n ];
    Graph.finalize_node g ptr

let create_deref g loc ?parent_fun ~mem (n : Node.t) =
    let t =
        match n.typ with
        | Ptr p -> p
        | _ -> assert false
    in
    let ptr = Node.create_mem ?parent_fun loc t Deref in
    Graph.add_dependencies g ptr [ None; Some mem; Some n ];
    Graph.finalize_node g ptr

let create_copy ?parent_fun g loc ~mem ~src ~dst =
    let n = Node.create_mem ?parent_fun loc Memory Copy in
    Graph.add_dependencies g n [ None; Some mem; Some src; Some dst ];
    Graph.finalize_node g n
