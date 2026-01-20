let create g (lhs : Node.t) (rhs : Node.t) =
    let n =
        match (lhs.typ, rhs.typ) with
        (* | Integer (Value lhs_v), Integer (Value rhs_v) -> *)
        (*     let n = Const_node.create_int g (lhs_v / rhs_v) in *)
        (*     Graph.remove_node g lhs; *)
        (*     Graph.remove_node g rhs; *)
        (*     n *)
        | _ ->
            let n = Node.create_data (Integer Top) Div in
            Graph.add_dependencies g n [ None; Some lhs; Some rhs ];
            n
    in
    Graph.finalize_node g n
