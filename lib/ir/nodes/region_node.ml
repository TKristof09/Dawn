open Core

let create g loc ?parent_fun nodes =
    let n = Node2.create_ctrl ?parent_fun loc Control Region in
    Node2.G.add_node g n { Node2.ctrl_inputs = List.map nodes ~f:Option.some };
    n

let compute_type g (n : Node.t) =
    let new_type =
        Graph.get_dependencies g n
        |> List.filter_map ~f:(function
          | None -> None
          | Some n -> Some n.typ)
        |> List.reduce_exn ~f:Types.meet
    in
    (~new_type, ~extra_deps:[])
