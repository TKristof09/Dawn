open Core

type data_kind =
    | Constant
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Div of t * t
    | Proj of t
    | Eq of t * t
    | Phi of t * t list

and ctrl_kind =
    | Start
    | Stop
    | Proj of t
    | If of t * t
    | Region

and kind =
    | Data of data_kind
    | Ctrl of ctrl_kind
    | Scope of t Symbol_table.t

and t = {
    mutable typ : Types.node_type;
    mutable kind : kind;
    id : int;
  }
[@@deriving sexp_of]

let id_counter = ref 0

let next_id () =
    incr id_counter;
    !id_counter

let show node =
    let kind_str =
        let show_sexp s =
            match s with
            | Sexplib.Sexp.List (h :: _) -> Sexplib.Sexp.to_string h
            | Sexplib.Sexp.Atom a -> a
            | Sexplib.Sexp.List [] -> assert false
        in
        match node.kind with
        | Data d -> show_sexp (sexp_of_data_kind d)
        | Ctrl c -> show_sexp (sexp_of_ctrl_kind c)
        | Scope _ -> ""
    in
    let type_string = Types.show_node_type node.typ in
    Printf.sprintf "Node { id : %d ; kind : %s; type :%s}" node.id kind_str type_string

let compare n1 n2 = Int.compare n1.id n2.id
let equal n1 n2 = Int.equal n1.id n2.id
let hash n = Int.hash n.id
let create_data typ kind = { typ; kind = Data kind; id = next_id () }
let create_ctrl typ kind = { typ; kind = Ctrl kind; id = next_id () }
let create_scope () = { typ = Types.BOTTOM; kind = Scope (Symbol_table.create ()); id = next_id () }
