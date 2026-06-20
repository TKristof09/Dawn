open Core

let common g loc ?parent_fun (lhs : ('a, Node.data) Node.t) (rhs : ('b, Node.data) Node.t)
    (kind : 'c Node.data_kind) =
    let typ = Types.Integer Any in
    let n = Node.create_data ?parent_fun loc typ kind in
    Node.G.add_node g n { Node.lhs = Some (AnyData lhs); rhs = Some (AnyData rhs) };
    n

let create_add g loc ?parent_fun lhs rhs = common g loc ?parent_fun lhs rhs Add
let create_sub g loc ?parent_fun lhs rhs = common g loc ?parent_fun lhs rhs Sub
let create_mul g loc ?parent_fun lhs rhs = common g loc ?parent_fun lhs rhs Mul
let create_div g loc ?parent_fun lhs rhs = common g loc ?parent_fun lhs rhs Div

let compute_type g n =
    let op =
        match n.Node.kind with
        | Data Add -> Z.add
        | Data Sub -> Z.sub
        | Data Mul -> Z.mul
        | Data Div -> Z.div
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
            if Poly.equal n.kind (Data Div) && Z.equal rhs_v Z.zero then
              Integer All
            else
              Types.make_int_const (op lhs_v rhs_v)
        | Integer (Value lhs), Integer _ when Types.is_constant rhs.typ ->
            let rhs_v = Types.get_integer_const_exn rhs.typ in
            if Poly.equal n.kind (Data Div) && Z.equal rhs_v Z.zero then
              Integer All
            else
              let a = op lhs.min rhs_v in
              let b = op lhs.max rhs_v in
              Types.make_int ~num_widens:lhs.num_widens (Z.min a b) (Z.max a b)
        | Integer _, Integer (Value rhs) when Types.is_constant lhs.typ ->
            let lhs_v = Types.get_integer_const_exn lhs.typ in
            if Poly.equal n.kind (Data Div) && Z.leq rhs.min Z.zero && Z.geq rhs.max Z.zero then
              Integer All
            else
              let a = op lhs_v rhs.min in
              let b = op lhs_v rhs.max in
              Types.make_int ~num_widens:rhs.num_widens (Z.min a b) (Z.max a b)
        | Integer (Value lhs), Integer (Value rhs) -> (
            match n.kind with
            | Data Add ->
                Types.make_int
                  ~num_widens:(max lhs.num_widens rhs.num_widens)
                  Z.(lhs.min + rhs.min)
                  Z.(lhs.max + rhs.max)
            | Data Sub ->
                Types.make_int
                  ~num_widens:(max lhs.num_widens rhs.num_widens)
                  Z.(lhs.min - rhs.max)
                  Z.(lhs.max - rhs.min)
            | Data Mul
            | Data Div ->
                Integer All
            | _ -> assert false)
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
