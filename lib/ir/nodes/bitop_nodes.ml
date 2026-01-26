let common g (lhs : Node.t) (rhs : Node.t) (kind : Node.data_kind) =
    let _op, _commutes =
        match kind with
        | Lsh -> (Int.shift_left, false)
        | Rsh -> (Int.shift_right, false)
        | BAnd -> (Int.logand, true)
        | BOr -> (Int.logor, true)
        | _ -> assert false
    in
    let n =
        match (lhs.typ, rhs.typ) with
        (*     | Integer (Value lhs_v), Integer (Value rhs_v) -> *)
        (*         let n = Const_node.create_int g (op lhs_v rhs_v) in *)
        (*         Graph.remove_node g lhs; *)
        (*         Graph.remove_node g rhs; *)
        (*         n *)
        | _ ->
            let n = Node.create_data (Integer Top) kind in
            Graph.add_dependencies g n [ None; Some lhs; Some rhs ];
            n
    in
    Graph.finalize_node g n

let create_lsh g (lhs : Node.t) (rhs : Node.t) = common g lhs rhs Lsh
let create_rsh g (lhs : Node.t) (rhs : Node.t) = common g lhs rhs Rsh
let create_band g (lhs : Node.t) (rhs : Node.t) = common g lhs rhs BAnd
let create_bor g (lhs : Node.t) (rhs : Node.t) = common g lhs rhs BOr
