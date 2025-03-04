let create g ~(ctrl : Node.t) ~(pred : Node.t) =
    let n =
        let n = Node.create_ctrl (Tuple (Value [ Control; Control ])) If in
        Graph.add_dependencies g n [ ctrl; pred ];
        n
    in
    Gvn.finalize g n
