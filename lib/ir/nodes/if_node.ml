let create g ~(ctrl : Node.t) ~(pred : Node.t) =
    let n =
        let n = Node.create_ctrl (Tuple (Value [ Control; Control ])) If in
        Graph.add_dependencies g n [ Some ctrl; Some pred ];
        n
    in
    Graph.finalize_node g n
