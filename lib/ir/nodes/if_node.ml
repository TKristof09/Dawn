open Core

let create ?parent_fun g loc ~ctrl ~pred =
    let n = Node2.create_ctrl ?parent_fun loc (Tuple (Value [ Control; Control ])) If in
    Node2.G.add_node g n
      { Node2.cond = Some (AnyData pred); true_branch = None; false_branch = None };
    Node2.G.set_ctrl g n ctrl;
    n

let compute_type g (n : Node.t) =
    let in_control = Graph.get_dependency g n 0 |> Option.value_exn in
    let new_type =
        match in_control.typ with
        | Control -> (
            let cond = Graph.get_dependency g n 1 |> Option.value_exn in
            match cond.typ with
            | Bool (Value false) -> Types.Tuple (Value [ DeadControl; Control ])
            | Bool (Value true) -> Types.Tuple (Value [ Control; DeadControl ])
            | ANY
            | Bool Any ->
                Tuple (Value [ DeadControl; DeadControl ])
            | _ -> Tuple (Value [ Control; Control ]))
        | DeadControl ->
            (* if is not reachable so both branches are dead too *)
            Tuple (Value [ DeadControl; DeadControl ])
        | ANY -> Tuple (Value [ ANY; ANY ])
        | _ -> n.typ
    in
    (~new_type, ~extra_deps:[])
