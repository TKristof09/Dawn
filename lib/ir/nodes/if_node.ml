open Core

let create g loc ~(ctrl : Node.t) ~(pred : Node.t) =
    let n =
        let n = Node.create_ctrl loc (Tuple (Value [ Control; Control ])) If in
        Graph.add_dependencies g n [ Some ctrl; Some pred ];
        n
    in
    Graph.finalize_node g n

let compute_type g (n : Node.t) =
    let in_control = Graph.get_dependency g n 0 |> Option.value_exn in
    let new_type =
        match in_control.typ with
        | Control -> (
            let cond = Graph.get_dependency g n 1 |> Option.value_exn in
            match cond.typ with
            | Integer (Value 0) -> Types.Tuple (Value [ DeadControl; Control ])
            | Integer (Value 1) -> Types.Tuple (Value [ Control; DeadControl ])
            | ANY
            | Integer Any ->
                Tuple (Value [ DeadControl; DeadControl ])
            | _ -> Tuple (Value [ Control; Control ]))
        | DeadControl ->
            (* if is not reachable so both branches are dead too *)
            Tuple (Value [ DeadControl; DeadControl ])
        | ANY -> Tuple (Value [ ANY; ANY ])
        | _ -> n.typ
    in
    (~new_type, ~extra_deps:[])

