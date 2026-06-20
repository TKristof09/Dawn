open Core

let create ?parent_fun g loc ~ctrl ~pred =
    let n = Node.create_ctrl ?parent_fun loc (Tuple (Value [ Control; Control ])) If in
    Node.G.add_node g n { Node.input = Some (AnyData pred) };
    Node.G.set_ctrl g n ctrl;
    n

let compute_type g n =
    let (AnyNode in_control) = Node.G.get_ctrl_exn g n in
    let new_type =
        match in_control.typ with
        | Control -> (
            let { Node.input } = Node.G.get_dependencies_exn g n in
            let (Node.AnyData input) = Option.value_exn input in
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
