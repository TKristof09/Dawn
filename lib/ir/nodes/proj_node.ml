let create g (n : Node.t) i =
    let n =
        match n.typ with
        | Tuple (Value l) -> (
            match n.kind with
            | Ctrl _ ->
                let proj = Node.create_ctrl (List.nth l i) (Proj i) in
                Graph.add_dependencies g proj [ Some n ];
                proj
            | Data _ ->
                let proj = Node.create_data (List.nth l i) (Proj i) in
                Graph.add_dependencies g proj [ Some n ];
                proj
            | Scope _ -> failwith "Can't project off of a scope node")
        | Tuple _ -> failwith "idk if this should be possible"
        | _ -> failwith "Proj node needs an input with tuple type"
    in
    Graph.finalize_node g n
