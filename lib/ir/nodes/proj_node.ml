open Core

let create g loc (n : Node.t) i =
    let n =
        match n.typ with
        | Tuple (Value l) -> (
            match List.nth_exn l i with
            | Types.Control ->
                let proj = Node.create_ctrl loc Control (Proj i) in
                Graph.add_dependencies g proj [ Some n ];
                proj
            | t ->
                let proj = Node.create_data loc t (Proj i) in
                Graph.add_dependencies g proj [ Some n ];
                proj)
        | Tuple _ -> failwith "idk if this should be possible"
        | _ -> failwith "Proj node needs an input with tuple type"
    in
    Graph.finalize_node g n

let compute_type g (n : Node.t) =
    let new_type =
        match n.kind with
        | Ctrl (Proj i) ->
            let in_control = Graph.get_dependency g n 0 |> Option.value_exn in
            let typ =
                match in_control.typ with
                | Tuple (Value l) -> List.nth_exn l i
                | ANY -> ANY
                | _ -> Types.ALL
            in
            typ
        | Data (Proj i) ->
            let in_data = Graph.get_dependency g n 0 |> Option.value_exn in
            let typ =
                match in_data.typ with
                | Tuple (Value l) -> List.nth_exn l i
                | ANY -> ANY
                | _ -> Types.ALL
            in
            typ
        | _ -> assert false
    in
    (~new_type, ~extra_deps:[])
