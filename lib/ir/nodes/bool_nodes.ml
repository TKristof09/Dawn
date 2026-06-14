open Core

let create_common g loc ?parent_fun lhs rhs kind =
    let n = Node2.create_data ?parent_fun loc (Bool Any) kind in
    Node2.G.add_node g n { Node2.lhs = Some (AnyData lhs); rhs = Some (AnyData rhs) };
    n

let create_eq g loc ?parent_fun lhs rhs = create_common g loc ?parent_fun lhs rhs Eq
let create_neq g loc ?parent_fun lhs rhs = create_common g loc ?parent_fun lhs rhs NEq
let create_lt g loc ?parent_fun lhs rhs = create_common g loc ?parent_fun lhs rhs Lt
let create_leq g loc ?parent_fun lhs rhs = create_common g loc ?parent_fun lhs rhs LEq
let create_gt g loc ?parent_fun lhs rhs = create_common g loc ?parent_fun lhs rhs Gt
let create_geq g loc ?parent_fun lhs rhs = create_common g loc ?parent_fun lhs rhs GEq

let compute_type g n =
    let op =
        match n.Node2.kind with
        | Data Eq -> Z.equal
        | Data NEq -> fun a b -> not (Z.equal a b)
        | Data Lt -> Z.lt
        | Data LEq -> Z.leq
        | Data Gt -> Z.gt
        | Data GEq -> Z.geq
        | _ -> assert false
    in
    let { Node2.lhs; rhs } = Node2.G.get_dependencies_exn g n in
    let (AnyData lhs) = Option.value_exn lhs in
    let (AnyData rhs) = Option.value_exn rhs in
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
                if not (Z.leq lhs.min rhs_v && Z.leq rhs_v lhs.max) then
                  Bool (Value false)
                else
                  Bool All
            | Data NEq ->
                if not (Z.leq lhs.min rhs_v && Z.leq rhs_v lhs.max) then
                  Bool (Value true)
                else
                  Bool All
            | Data Lt ->
                if Z.lt lhs.max rhs_v then
                  Bool (Value true)
                else if Z.geq lhs.min rhs_v then
                  Bool (Value false)
                else
                  Bool All
            | Data LEq ->
                if Z.leq lhs.max rhs_v then
                  Bool (Value true)
                else if Z.gt lhs.min rhs_v then
                  Bool (Value false)
                else
                  Bool All
            | Data Gt ->
                if Z.gt lhs.min rhs_v then
                  Bool (Value true)
                else if Z.leq lhs.max rhs_v then
                  Bool (Value false)
                else
                  Bool All
            | Data GEq ->
                if Z.geq lhs.min rhs_v then
                  Bool (Value true)
                else if Z.lt lhs.max rhs_v then
                  Bool (Value false)
                else
                  Bool All
            | _ -> assert false)
        | Integer _, Integer (Value rhs) when Types.is_constant lhs.typ -> (
            let lhs_v = Types.get_integer_const_exn lhs.typ in
            match n.kind with
            | Data Eq ->
                if not (Z.leq rhs.min lhs_v && Z.leq lhs_v rhs.max) then
                  Bool (Value false)
                else
                  Bool All
            | Data NEq ->
                if not (Z.leq rhs.min lhs_v && Z.leq lhs_v rhs.max) then
                  Bool (Value true)
                else
                  Bool All
            | Data Lt ->
                if Z.lt lhs_v rhs.min then
                  Bool (Value true)
                else if Z.geq lhs_v rhs.max then
                  Bool (Value false)
                else
                  Bool All
            | Data LEq ->
                if Z.leq lhs_v rhs.min then
                  Bool (Value true)
                else if Z.gt lhs_v rhs.max then
                  Bool (Value false)
                else
                  Bool All
            | Data Gt ->
                if Z.gt lhs_v rhs.max then
                  Bool (Value true)
                else if Z.leq lhs_v rhs.min then
                  Bool (Value false)
                else
                  Bool All
            | Data GEq ->
                if Z.geq lhs_v rhs.max then
                  Bool (Value true)
                else if Z.lt lhs_v rhs.min then
                  Bool (Value false)
                else
                  Bool All
            | _ -> assert false)
        | Integer (Value lhs), Integer (Value rhs) -> (
            match n.kind with
            | Data Eq ->
                if Z.lt lhs.max rhs.min || Z.gt lhs.min rhs.max then
                  Bool (Value false)
                else
                  Bool All
            | Data NEq ->
                if Z.lt lhs.max rhs.min || Z.gt lhs.min rhs.max then Bool (Value true) else Bool All
            | Data Lt ->
                if Z.lt lhs.max rhs.min then
                  Bool (Value true)
                else if Z.geq lhs.min rhs.max then
                  Bool (Value false)
                else
                  Bool All
            | Data LEq ->
                if Z.leq lhs.max rhs.min then
                  Bool (Value true)
                else if Z.gt lhs.min rhs.max then
                  Bool (Value false)
                else
                  Bool All
            | Data Gt ->
                if Z.gt lhs.min rhs.max then
                  Bool (Value true)
                else if Z.leq lhs.max rhs.min then
                  Bool (Value false)
                else
                  Bool All
            | Data GEq ->
                if Z.geq lhs.min rhs.max then
                  Bool (Value true)
                else if Z.lt lhs.max rhs.min then
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
