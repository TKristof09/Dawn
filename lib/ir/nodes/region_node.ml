let create g loc (n, n') =
    let node = Node.create_ctrl loc Control Region in
    (* Need extra input at idx 0 to match up with phi nodes inputs as those have the region node as input 0 *)
    Graph.add_dependencies g node [ None; Some n; Some n' ];
    node
