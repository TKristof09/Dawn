let rec create g (lhs : Node.t) (rhs : Node.t) =
    let create_aux lhs rhs =
        let n = Node.create_data (Integer Top) (Add (lhs, rhs)) in
        Graph.add_dependencies g n [ lhs; rhs ];
        n
    in
    match (lhs.kind, rhs.kind) with
    | Data Constant, Data Constant -> (
        match (lhs.typ, rhs.typ) with
        | Integer (Value lhs_v), Integer (Value rhs_v) ->
            let n = Const_node.create_int g (lhs_v + rhs_v) in
            Graph.remove_node g lhs;
            Graph.remove_node g rhs;
            n
        | _ -> create_aux lhs rhs)
    (* (_ + y) + z -> _ + (const y+z) *)
    | Data (Add (ll, lr)), _ when Types.is_constant lr.typ -> (
        match (lr.typ, rhs.typ) with
        | Integer (Value lr_v), Integer (Value rhs_v) ->
            let rhs = Const_node.create_int g (lr_v + rhs_v) in
            Graph.remove_node g lr;
            Graph.remove_node g rhs;
            create_aux ll rhs
        | _ ->
            (* (_ + y) + _ -> (_ + _) + y *)
            let new_lhs = create g ll rhs in
            Graph.remove_node g lhs;
            create_aux new_lhs lr)
    (* addition with 0 *)
    | _, Data Constant when rhs.typ = Integer (Value 0) ->
        Graph.remove_node g rhs;
        lhs
    | Data Constant, _ when lhs.typ = Integer (Value 0) ->
        Graph.remove_node g lhs;
        rhs
    (* make the tree left leaning: (x+y) + (z+w) -> ((x+y)+z)+w , this is useful because w might be a const *)
    | Data (Add _), Data (Add (rl, rr)) ->
        let new_lhs = create g lhs rl in
        let n = create_aux new_lhs rr in
        Graph.remove_node g rhs;
        n
    (* move non add nodes to the right to help constant folding *)
    | _, Data (Add _) -> create g rhs lhs
    | _ -> create_aux lhs rhs
