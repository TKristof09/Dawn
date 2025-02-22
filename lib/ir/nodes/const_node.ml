let create_int g i =
    let n = Node.create_data (Types.Integer (Value i)) Constant in
    Graph.add_dependencies g n [ Graph.get_start g ];
    n
