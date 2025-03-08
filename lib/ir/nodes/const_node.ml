let create_int g i =
    let n =
        let n = Node.create_data (Types.Integer (Value i)) Constant in
        Graph.add_dependencies g n [ Some (Graph.get_start g) ];
        n
    in
    Graph.finalize_node g n
