let create g (n : Node.t) i =
    match n.typ with
    | Tuple (Value l) -> (
        match n.kind with
        | Ctrl _ ->
            let proj = Node.create_ctrl (List.nth l i) (Proj n) in
            Graph.add_dependencies g proj [ n ];
            proj
        | Data _ ->
            let proj = Node.create_data (List.nth l i) (Proj n) in
            Graph.add_dependencies g proj [ n ];
            proj
        | Scope _ -> failwith "Can't project off of a scope node")
    | Tuple _ -> failwith "idk if this should be possible"
    | _ -> failwith "Proj node needs an input with tuple type"
