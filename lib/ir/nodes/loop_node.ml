let create g loc ?parent_fun entry =
    let n = Node.create_ctrl ?parent_fun loc Control Loop in
    (* Need extra input at idx 0 to match up with phi nodes inputs as those have the region node as input 0 *)
    Node.G.add_node g n { Node.entry = Some (AnyCtrl entry); backedge = None };
    n

let set_back_edge g n back =
    let inputs = Node.G.get_dependencies_exn g n in
    Node.G.set_node_inputs g n { inputs with Node.backedge = Some (AnyCtrl back) }
