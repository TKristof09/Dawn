let create g loc ~(ctrl : Node.t) ~(pred : Node.t) =
    let n =
        let n = Node.create_ctrl loc (Tuple (Value [ Control; Control ])) If in
        Graph.add_dependencies g n [ Some ctrl; Some pred ];
        n
    in
    Graph.finalize_node g n
