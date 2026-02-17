open Core

let create g loc (n, n') =
    let node = Node.create_ctrl loc Control Region in
    (* Need extra input at idx 0 to match up with phi nodes inputs as those have the region node as input 0 *)
    Graph.add_dependencies g node [ None; Some n; Some n' ];
    node

let compute_type g (n : Node.t) =
    let new_type =
        Graph.get_dependencies g n
        |> List.filter_map ~f:(function
          | None -> None
          | Some n -> Some n.typ)
        |> List.reduce_exn ~f:Types.meet
    in
    (~new_type, ~extra_deps:[])
