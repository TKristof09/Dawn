open Core

let common g loc (lhs : Node.t) (rhs : Node.t) (kind : Node.data_kind) =
    let n = Node.create_data loc (Integer Any) kind in
    Graph.add_dependencies g n [ None; Some lhs; Some rhs ];
    Graph.finalize_node g n

let create_lsh g loc (lhs : Node.t) (rhs : Node.t) = common g loc lhs rhs Lsh
let create_rsh g loc (lhs : Node.t) (rhs : Node.t) = common g loc lhs rhs Rsh
let create_band g loc (lhs : Node.t) (rhs : Node.t) = common g loc lhs rhs BAnd
let create_bor g loc (lhs : Node.t) (rhs : Node.t) = common g loc lhs rhs BOr

let compute_type g (n : Node.t) =
    let op =
        match n.kind with
        | Data Lsh -> Int.shift_left
        | Data Rsh -> Int.shift_right
        | Data BAnd -> Int.bit_and
        | Data BOr -> Int.bit_or
        | _ -> assert false
    in
    let lhs = Graph.get_dependency g n 1 |> Option.value_exn in
    let rhs = Graph.get_dependency g n 2 |> Option.value_exn in
    let new_type : Types.t =
        match (lhs.typ, rhs.typ) with
        | Integer (Value lhs_v), Integer (Value rhs_v) -> Integer (Value (op lhs_v rhs_v))
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
