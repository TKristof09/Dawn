let create g loc n =
    let node = Node.create_ctrl loc Control Loop in
    (* Need extra input at idx 0 to match up with phi nodes inputs as those have the region node as input 0 *)
    Graph.add_dependencies g node [ None; None; Some n ];
    node

let set_back_edge g n back = Graph.set_dependency g n (Some back) 1
let get_back_edge g n = Graph.get_dependency g n 1 |> Core.Option.value_exn
let get_entry_edge g n = Graph.get_dependency g n 2 |> Core.Option.value_exn
