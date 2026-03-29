open Core

let common g loc ?parent_fun (lhs : Node.t) (rhs : Node.t) (kind : Node.data_kind) =
    let n = Node.create_data ?parent_fun loc (Integer Any) kind in
    Graph.add_dependencies g n [ None; Some lhs; Some rhs ];
    Graph.finalize_node g n

let create_lsh g loc ?parent_fun (lhs : Node.t) (rhs : Node.t) =
    common g loc ?parent_fun lhs rhs Lsh

let create_rsh g loc ?parent_fun (lhs : Node.t) (rhs : Node.t) =
    common g loc ?parent_fun lhs rhs Rsh

let create_band g loc ?parent_fun (lhs : Node.t) (rhs : Node.t) =
    common g loc ?parent_fun lhs rhs BAnd

let create_bor g loc ?parent_fun (lhs : Node.t) (rhs : Node.t) =
    common g loc ?parent_fun lhs rhs BOr

let compute_type g (n : Node.t) =
    let op =
        match n.kind with
        | Data Lsh -> fun a b -> Z.shift_left a (Z.to_int b)
        | Data Rsh -> fun a b -> Z.shift_right a (Z.to_int b)
        | Data BAnd -> Z.logand
        | Data BOr -> Z.logor
        | _ -> assert false
    in
    let lhs = Graph.get_dependency g n 1 |> Option.value_exn in
    let rhs = Graph.get_dependency g n 2 |> Option.value_exn in
    let new_type : Types.t =
        match (lhs.typ, rhs.typ) with
        | Integer _, Integer _ when Types.is_constant lhs.typ && Types.is_constant rhs.typ ->
            let lhs_v = Types.get_integer_const_exn lhs.typ in
            let rhs_v = Types.get_integer_const_exn rhs.typ in
            Types.make_int_const (op lhs_v rhs_v)
        | Integer (Value lhs), Integer _ when Types.is_constant rhs.typ -> (
            (* TODO consider making a version for when lhs is constant but rhs isn't. That should also be doable but more complex *)
            let rhs_v = Types.get_integer_const_exn rhs.typ in
            match n.kind with
            | Data Lsh
            | Data Rsh ->
                Types.make_int ~num_widens:lhs.num_widens (op lhs.min rhs_v) (op lhs.max rhs_v)
            | _ -> Integer All)
        | Integer Any, Integer Any -> Integer Any
        | Integer Any, Integer _ when Types.is_constant rhs.typ -> Integer Any
        | Integer _, Integer Any when Types.is_constant lhs.typ -> Integer Any
        | Integer _, Integer _ -> Integer All
        | ANY, _
        | _, ANY ->
            Integer Any
        | _, _ -> ALL
    in
    (~new_type, ~extra_deps:[])
