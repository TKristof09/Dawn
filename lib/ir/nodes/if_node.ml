let create g ~(ctrl : Node.t) ~(pred : Node.t) =
    let n = Node.create_ctrl (Tuple (Value [ Control; Control ])) (If (ctrl, pred)) in
    Graph.add_dependencies g n [ ctrl; pred ];
    n
