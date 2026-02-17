open Core

let create_common g loc (lhs : Node.t) (rhs : Node.t) kind =
    let typ = Types.Integer Any in
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
        | Integer (Value lhs_v), Integer (Value rhs_v) ->
            Integer (Value (op lhs_v rhs_v |> Bool.to_int))
        | Integer Any, Integer Any -> Integer Any
        | Integer Any, Integer (Value _)
        | Integer (Value _), Integer Any ->
            Integer Any
        | Integer _, Integer _ -> Integer All
        | ANY, _
        | _, ANY ->
            Integer Any
        | _, _ -> ALL
    in
    (~new_type, ~extra_deps:[])
