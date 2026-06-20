open Core

let create g loc ?parent_fun nodes =
    let n = Node.create_ctrl ?parent_fun loc Control Region in
    Node.G.add_node g n { Node.ctrl_inputs = List.map nodes ~f:Option.some };
    n

let compute_type g n =
    let { Node.ctrl_inputs } = Node.G.get_dependencies_exn g n in
    let new_type =
        ctrl_inputs
        |> List.filter_map ~f:(function
          | None -> None
          | Some (AnyCtrl n) -> Some n.typ)
        |> List.reduce_exn ~f:Types.meet
    in
    (~new_type, ~extra_deps:[])
