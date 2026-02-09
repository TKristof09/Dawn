let create_common g loc (lhs : Node.t) (rhs : Node.t) ~cmp:_ kind =
    let n =
        match (lhs.typ, rhs.typ) with
        (* | Integer (Value lhs_v), Integer (Value rhs_v) -> *)
        (*     Const_node.create_int g loc (Bool.to_int (cmp lhs_v rhs_v)) *)
        | _ ->
            let n = Node.create_data loc (Integer Any) kind in
            Graph.add_dependencies g n [ None; Some lhs; Some rhs ];
            n
    in
    Graph.finalize_node g n

let create_eq g loc (lhs : Node.t) (rhs : Node.t) =
    (* if two operands are same nodes there value will always be the same during the program *)
    if Node.equal lhs rhs then
      Const_node.create_int g loc 1 |> Graph.finalize_node g
    else
      create_common g loc lhs rhs ~cmp:( = ) Eq

let create_neq g loc (lhs : Node.t) (rhs : Node.t) = create_common g loc lhs rhs ~cmp:( <> ) NEq
let create_lt g loc (lhs : Node.t) (rhs : Node.t) = create_common g loc lhs rhs ~cmp:( < ) Lt
let create_leq g loc (lhs : Node.t) (rhs : Node.t) = create_common g loc lhs rhs ~cmp:( <= ) LEq
let create_gt g loc (lhs : Node.t) (rhs : Node.t) = create_common g loc lhs rhs ~cmp:( > ) Gt
let create_geq g loc (lhs : Node.t) (rhs : Node.t) = create_common g loc lhs rhs ~cmp:( >= ) GEq
