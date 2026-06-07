let create g loc ?parent_fun typ name =
    let n = Node2.create_data ?parent_fun loc typ (External name) in
    Node2.G.add_node g n ();
    n
