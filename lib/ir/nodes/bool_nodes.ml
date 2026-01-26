let create_common g (lhs : Node.t) (rhs : Node.t) ~cmp:_ kind =
    let n =
        match (lhs.typ, rhs.typ) with
        (* | Integer (Value lhs_v), Integer (Value rhs_v) -> *)
        (*     Const_node.create_int g (Bool.to_int (cmp lhs_v rhs_v)) *)
        | _ ->
            let n = Node.create_data (Integer Top) kind in
            Graph.add_dependencies g n [ None; Some lhs; Some rhs ];
            n
    in
    Graph.finalize_node g n

let create_eq g (lhs : Node.t) (rhs : Node.t) =
    (* if two operands are same nodes there value will always be the same during the program *)
    if Node.equal lhs rhs then
      Const_node.create_int g 1 |> Graph.finalize_node g
    else
      create_common g lhs rhs ~cmp:( = ) Eq

let create_neq g (lhs : Node.t) (rhs : Node.t) = create_common g lhs rhs ~cmp:( <> ) NEq
let create_lt g (lhs : Node.t) (rhs : Node.t) = create_common g lhs rhs ~cmp:( < ) Lt
let create_leq g (lhs : Node.t) (rhs : Node.t) = create_common g lhs rhs ~cmp:( <= ) LEq
let create_gt g (lhs : Node.t) (rhs : Node.t) = create_common g lhs rhs ~cmp:( > ) Gt
let create_geq g (lhs : Node.t) (rhs : Node.t) = create_common g lhs rhs ~cmp:( >= ) GEq

