let create g loc ?parent_fun entry =
    let n = Node2.create_ctrl ?parent_fun loc Control Loop in
    (* Need extra input at idx 0 to match up with phi nodes inputs as those have the region node as input 0 *)
    Node2.G.add_node g n { Node2.entry = Some (AnyCtrl entry); backedge = None };
    n

let set_back_edge g n back =
    let inputs = Node2.G.get_dependencies_exn g n in
    Node2.G.set_node_inputs g n { inputs with Node2.backedge = Some (AnyCtrl back) }
