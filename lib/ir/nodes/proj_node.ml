let create g (n : Node.t) i =
    let n =
        match n.typ with
        | Tuple (Value l) -> (
            match List.nth l i with
            | Control ->
                let proj = Node.create_ctrl Control (Proj i) in
                Graph.add_dependencies g proj [ Some n ];
                proj
            | t ->
                let proj = Node.create_data t (Proj i) in
                Graph.add_dependencies g proj [ Some n ];
                proj)
        | Tuple _ -> failwith "idk if this should be possible"
        | _ -> failwith "Proj node needs an input with tuple type"
    in
    Graph.finalize_node g n
