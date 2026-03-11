open Core

let common g loc (lhs : Node.t) (rhs : Node.t) (kind : Node.data_kind) =
    let typ = Types.Integer Any in
    let n = Node.create_data loc typ kind in
    Graph.add_dependencies g n [ None; Some lhs; Some rhs ];
    Graph.finalize_node g n

let create_add g loc (lhs : Node.t) (rhs : Node.t) = common g loc lhs rhs Add
let create_sub g loc (lhs : Node.t) (rhs : Node.t) = common g loc lhs rhs Sub
let create_mul g loc (lhs : Node.t) (rhs : Node.t) = common g loc lhs rhs Mul
let create_div g loc (lhs : Node.t) (rhs : Node.t) = common g loc lhs rhs Div

let compute_type g (n : Node.t) =
    let op =
        match n.kind with
        | Data Add -> ( + )
        | Data Sub -> ( - )
        | Data Mul -> ( * )
        | Data Div -> ( / )
        | _ -> assert false
    in
    let lhs = Graph.get_dependency g n 1 |> Option.value_exn in
    let rhs = Graph.get_dependency g n 2 |> Option.value_exn in
    let new_type : Types.t =
        match (lhs.typ, rhs.typ) with
        | Integer _, Integer _ when Types.is_constant lhs.typ && Types.is_constant rhs.typ ->
            let lhs_v = Types.get_integer_const_exn lhs.typ in
            let rhs_v = Types.get_integer_const_exn rhs.typ in
            if Poly.equal n.kind (Data Div) && rhs_v = 0 then
              Integer All
            else
              Types.make_int_const (op lhs_v rhs_v)
        | Integer (Value lhs), Integer _ when Types.is_constant rhs.typ ->
            let rhs_v = Types.get_integer_const_exn rhs.typ in
            if Poly.equal n.kind (Data Div) && rhs_v = 0 then
              Integer All
            else
              let a = op lhs.min rhs_v in
              let b = op lhs.max rhs_v in
              Types.make_int ~num_widens:lhs.num_widens (min a b) (max a b)
        | Integer _, Integer (Value rhs) when Types.is_constant lhs.typ ->
            let lhs_v = Types.get_integer_const_exn lhs.typ in
            if Poly.equal n.kind (Data Div) && rhs.min <= 0 && rhs.max >= 0 then
              Integer All
            else
              let a = op lhs_v rhs.min in
              let b = op lhs_v rhs.max in
              Types.make_int ~num_widens:rhs.num_widens (min a b) (max a b)
        | Integer (Value lhs), Integer (Value rhs) -> (
            match n.kind with
            | Data Add ->
                Types.make_int
                  ~num_widens:(max lhs.num_widens rhs.num_widens)
                  (lhs.min + rhs.min) (lhs.max + rhs.max)
            | Data Sub ->
                Types.make_int
                  ~num_widens:(max lhs.num_widens rhs.num_widens)
                  (lhs.min - rhs.max) (lhs.max - rhs.min)
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
