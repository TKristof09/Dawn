let create g (lhs : Node.t) (rhs : Node.t) =
    let n =
        if lhs = rhs then
          Const_node.create_int g 1
        else
          match (lhs.typ, rhs.typ) with
          | Integer (Value lhs_v), Integer (Value rhs_v) ->
              Const_node.create_int g (Bool.to_int (lhs_v = rhs_v))
          | _ ->
              let n = Node.create_data (Integer Top) Eq in
              Graph.add_dependencies g n [ lhs; rhs ];
              n
    in
    Gvn.finalize g n
