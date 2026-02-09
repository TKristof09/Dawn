let common g loc (lhs : Node.t) (rhs : Node.t) (kind : Node.data_kind) =
    let _op, _commutes =
        match kind with
        | Add -> (( + ), true)
        | Sub -> (( - ), false)
        | Mul -> (Int.mul, true)
        | Div -> (( / ), false)
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
            let n = Node.create_data loc (Integer Any) kind in
            Graph.add_dependencies g n [ None; Some lhs; Some rhs ];
            n
    in
    Graph.finalize_node g n

let create_add g loc (lhs : Node.t) (rhs : Node.t) = common g loc lhs rhs Add
let create_sub g loc (lhs : Node.t) (rhs : Node.t) = common g loc lhs rhs Sub
let create_mul g loc (lhs : Node.t) (rhs : Node.t) = common g loc lhs rhs Mul
let create_div g loc (lhs : Node.t) (rhs : Node.t) = common g loc lhs rhs Div
