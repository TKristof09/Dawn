let create g (lhs : Node.t) (rhs : Node.t) =
    let create_aux lhs rhs =
        let n = Node.create_data (Integer Top) Add in
        Graph.add_dependencies g n [ None; Some lhs; Some rhs ];
        n
    in
    let n =
        match (lhs.kind, rhs.kind) with
        (* | Data Constant, Data Constant -> ( *)
        (*     match (lhs.typ, rhs.typ) with *)
        (*     | Integer (Value lhs_v), Integer (Value rhs_v) -> *)
        (*         let n = Const_node.create_int g (lhs_v + rhs_v) in *)
        (*         Graph.remove_node g lhs; *)
        (*         Graph.remove_node g rhs; *)
        (*         n *)
        (*     | _ -> create_aux lhs rhs) *)
        | _ -> create_aux lhs rhs
    in
    Graph.finalize_node g n
