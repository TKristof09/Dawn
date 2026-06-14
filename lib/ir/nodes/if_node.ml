open Core

let create ?parent_fun g loc ~ctrl ~pred =
    let n = Node2.create_ctrl ?parent_fun loc (Tuple (Value [ Control; Control ])) If in
    Node2.G.add_node g n { Node2.input = Some (AnyData pred) };
    Node2.G.set_ctrl g n ctrl;
    n

let compute_type g n =
    let (AnyNode in_control) = Node2.G.get_ctrl_exn g n in
    let new_type =
        match in_control.typ with
        | Control -> (
            let { Node2.input } = Node2.G.get_dependencies_exn g n in
            let (Node2.AnyData input) = Option.value_exn input in
            match input.typ with
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
