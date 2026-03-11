open Core

let create_common g loc (lhs : Node.t) (rhs : Node.t) kind =
    let typ = Types.Bool Any in
    let n = Node.create_data loc typ kind in
    Graph.add_dependencies g n [ None; Some lhs; Some rhs ];
    Graph.finalize_node g n

let create_eq g loc (lhs : Node.t) (rhs : Node.t) = create_common g loc lhs rhs Eq
let create_neq g loc (lhs : Node.t) (rhs : Node.t) = create_common g loc lhs rhs NEq
let create_lt g loc (lhs : Node.t) (rhs : Node.t) = create_common g loc lhs rhs Lt
let create_leq g loc (lhs : Node.t) (rhs : Node.t) = create_common g loc lhs rhs LEq
let create_gt g loc (lhs : Node.t) (rhs : Node.t) = create_common g loc lhs rhs Gt
let create_geq g loc (lhs : Node.t) (rhs : Node.t) = create_common g loc lhs rhs GEq

let compute_type g (n : Node.t) =
    let op =
        match n.kind with
        | Data Eq -> ( = )
        | Data NEq -> ( <> )
        | Data Lt -> ( < )
        | Data LEq -> ( <= )
        | Data Gt -> ( > )
        | Data GEq -> ( >= )
        | _ -> assert false
    in
    let lhs = Graph.get_dependency g n 1 |> Option.value_exn in
    let rhs = Graph.get_dependency g n 2 |> Option.value_exn in
    let new_type : Types.t =
        match (lhs.typ, rhs.typ) with
        | Integer _, Integer _ when Types.is_constant lhs.typ && Types.is_constant rhs.typ ->
            let lhs_v = Types.get_integer_const_exn lhs.typ in
            let rhs_v = Types.get_integer_const_exn rhs.typ in
            Bool (Value (op lhs_v rhs_v))
        | Integer (Value lhs), Integer _ when Types.is_constant rhs.typ -> (
            let rhs_v = Types.get_integer_const_exn rhs.typ in
            match n.kind with
            | Data Eq ->
                if not (lhs.min <= rhs_v && rhs_v <= lhs.max) then Bool (Value false) else Bool All
            | Data NEq ->
                if not (lhs.min <= rhs_v && rhs_v <= lhs.max) then Bool (Value true) else Bool All
            | Data Lt ->
                if lhs.max < rhs_v then
                  Bool (Value true)
                else if lhs.min >= rhs_v then
                  Bool (Value false)
                else
                  Bool All
            | Data LEq ->
                if lhs.max <= rhs_v then
                  Bool (Value true)
                else if lhs.min > rhs_v then
                  Bool (Value false)
                else
                  Bool All
            | Data Gt ->
                if lhs.min > rhs_v then
                  Bool (Value true)
                else if lhs.max <= rhs_v then
                  Bool (Value false)
                else
                  Bool All
            | Data GEq ->
                if lhs.min >= rhs_v then
                  Bool (Value true)
                else if lhs.max < rhs_v then
                  Bool (Value false)
                else
                  Bool All
            | _ -> assert false)
        | Integer _, Integer (Value rhs) when Types.is_constant lhs.typ -> (
            let lhs_v = Types.get_integer_const_exn lhs.typ in
            match n.kind with
            | Data Eq ->
                if not (rhs.min <= lhs_v && lhs_v <= rhs.max) then Bool (Value false) else Bool All
            | Data NEq ->
                if not (rhs.min <= lhs_v && lhs_v <= rhs.max) then Bool (Value true) else Bool All
            | Data Lt ->
                if lhs_v < rhs.min then
                  Bool (Value true)
                else if lhs_v >= rhs.max then
                  Bool (Value false)
                else
                  Bool All
            | Data LEq ->
                if lhs_v <= rhs.min then
                  Bool (Value true)
                else if lhs_v > rhs.max then
                  Bool (Value false)
                else
                  Bool All
            | Data Gt ->
                if lhs_v > rhs.max then
                  Bool (Value true)
                else if lhs_v <= rhs.min then
                  Bool (Value false)
                else
                  Bool All
            | Data GEq ->
                if lhs_v >= rhs.max then
                  Bool (Value true)
                else if lhs_v < rhs.min then
                  Bool (Value false)
                else
                  Bool All
            | _ -> assert false)
        | Integer (Value lhs), Integer (Value rhs) -> (
            match n.kind with
            | Data Eq ->
                if lhs.max < rhs.min || lhs.min > rhs.max then Bool (Value false) else Bool All
            | Data NEq ->
                if lhs.max < rhs.min || lhs.min > rhs.max then Bool (Value true) else Bool All
            | Data Lt ->
                if lhs.max < rhs.min then
                  Bool (Value true)
                else if lhs.min >= rhs.max then
                  Bool (Value false)
                else
                  Bool All
            | Data LEq ->
                if lhs.max <= rhs.min then
                  Bool (Value true)
                else if lhs.min > rhs.max then
                  Bool (Value false)
                else
                  Bool All
            | Data Gt ->
                if lhs.min > rhs.max then
                  Bool (Value true)
                else if lhs.max <= rhs.min then
                  Bool (Value false)
                else
                  Bool All
            | Data GEq ->
                if lhs.min >= rhs.max then
                  Bool (Value true)
                else if lhs.max < rhs.min then
                  Bool (Value false)
                else
                  Bool All
            | _ -> assert false)
        | Integer Any, Integer Any -> Bool Any
        | Integer Any, Integer _ when Types.is_constant rhs.typ -> Bool Any
        | Integer _, Integer Any when Types.is_constant lhs.typ -> Bool Any
        | Integer _, Integer _ -> Bool All
        | ANY, _
        | _, ANY ->
            Bool Any
        | _, _ -> ALL
    in
    (~new_type, ~extra_deps:[])
