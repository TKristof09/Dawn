open Core

type data_kind =
    | Constant
    | Add
    | Sub
    | Mul
    | Div
    | Lsh
    | Rsh
    | BAnd
    | BOr
    | Proj of int
    | Eq
    | NEq
    | Lt
    | LEq
    | Gt
    | GEq
    | Phi
    | Param of int
    | External of string

and ctrl_kind =
    | Start
    | Stop
    | Proj of int
    | If
    | Region
    | Loop
    | Function of {
        ret : t;
        signature : Types.node_type;
        idx : int;
      }
    | Return
    | FunctionCall
    | FunctionCallEnd

and mem_kind =
    | New
    | Load
    | Store

and kind =
    | Data of data_kind
    | Ctrl of ctrl_kind
    | Mem of mem_kind
    | Scope of t Symbol_table.t

and t = {
    mutable typ : Types.node_type;
    mutable kind : kind;
    id : int;
    loc : Ast.loc;
  }
[@@deriving sexp_of]

let id_counter = ref 0
let reset_id () = id_counter := 0

let next_id () =
    incr id_counter;
    !id_counter

let show_kind kind =
    let show_sexp s =
        match s with
        | Sexplib.Sexp.List (h :: _) -> Sexplib.Sexp.to_string h
        | Sexplib.Sexp.Atom a -> a
        | Sexplib.Sexp.List [] -> assert false
    in
    match kind with
    | Data (Proj i) -> Printf.sprintf "DProj %d" i
    | Ctrl (Proj i) -> Printf.sprintf "CProj %d" i
    | Data d -> show_sexp (sexp_of_data_kind d)
    | Ctrl c -> show_sexp (sexp_of_ctrl_kind c)
    | Mem m -> show_sexp (sexp_of_mem_kind m)
    | Scope _ -> "Scope"

let show node =
    let kind_str = show_kind node.kind in
    let type_string = Types.show_node_type node.typ in
    Printf.sprintf "Node { id : %d ; kind : %s; type :%s}" node.id kind_str type_string

let pp fmt node = Format.fprintf fmt "%s" (show node)
let compare n1 n2 = Int.compare n1.id n2.id
let equal n1 n2 = Int.equal n1.id n2.id

let semantic_equal n1 deps1 n2 deps2 =
    let is_same_kind n1 n2 =
        let constant_same n1 n2 =
            match (n1.typ, n2.typ) with
            | Integer (Value v1), Integer (Value v2) -> v1 = v2
            | _ -> false
        in
        match (n1.kind, n2.kind) with
        | Data Constant, Data Constant -> constant_same n1 n2
        | _, _ -> Poly.equal n1.kind n2.kind
    in
    is_same_kind n1 n2
    && List.equal
         (fun a b ->
           match (a, b) with
           | None, None -> true
           | Some _, None
           | None, Some _ ->
               false
           | Some a, Some b -> equal a b)
         deps1 deps2

let hash n = Int.hash n.id
let create_data loc typ kind = { typ; kind = Data kind; id = next_id (); loc }
let create_ctrl loc typ kind = { typ; kind = Ctrl kind; id = next_id (); loc }
let create_mem loc typ kind = { typ; kind = Mem kind; id = next_id (); loc }

let create_scope () =
    {
      typ = Types.ALL;
      kind = Scope (Symbol_table.create ());
      id = next_id ();
      loc = { filename = ""; line = 0; col = 0 };
    }

let is_ctrl n =
    match n.kind with
    | Ctrl _ -> true
    | _ -> false

let is_data n =
    match n.kind with
    | Data _ -> true
    | _ -> false

let is_scope n =
    match n.kind with
    | Scope _ -> true
    | _ -> false

let is_blockhead n =
    match n.kind with
    | Ctrl Start
    | Ctrl (Proj _)
    | Ctrl Region
    | Ctrl Loop
    | Ctrl Stop ->
        true
    | _ -> false
