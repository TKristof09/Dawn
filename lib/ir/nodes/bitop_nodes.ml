open Core

let common g loc ?parent_fun lhs rhs kind =
    let n = Node.create_data ?parent_fun loc (Integer Any) kind in
    Node.G.add_node g n { Node.lhs = Some (AnyData lhs); rhs = Some (AnyData rhs) };
    n

let create_lsh g loc ?parent_fun lhs rhs = common g loc ?parent_fun lhs rhs Lsh
let create_rsh g loc ?parent_fun lhs rhs = common g loc ?parent_fun lhs rhs Rsh
let create_band g loc ?parent_fun lhs rhs = common g loc ?parent_fun lhs rhs BAnd
let create_bor g loc ?parent_fun lhs rhs = common g loc ?parent_fun lhs rhs BOr

let compute_type g n =
    let op =
        match n.Node.kind with
        | Data Lsh -> fun a b -> Z.shift_left a (Z.to_int b)
        | Data Rsh -> fun a b -> Z.shift_right a (Z.to_int b)
        | Data BAnd -> Z.logand
        | Data BOr -> Z.logor
        | _ -> assert false
    in
    let { Node.lhs; rhs } = Node.G.get_dependencies_exn g n in
    let (AnyData lhs) = Option.value_exn lhs in
    let (AnyData rhs) = Option.value_exn rhs in
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
