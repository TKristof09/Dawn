type data_kind =
    | Constant
    | Add
    | Sub
    | Mul
    | Div
    | Proj of int
    | Eq
    | Phi

and ctrl_kind =
    | Start
    | Stop
    | Proj of int
    | If
    | Region
    | Loop

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

val is_same : t -> t option list -> t -> t option list -> bool
(** Check for semantical equality. Basically same kind and same inputs *)

val hard_equal : t -> t -> bool
(** Chek for equality of ids *)

val compare : t -> t -> int
val hash : t -> int
val create_data : Types.node_type -> data_kind -> t
val create_ctrl : Types.node_type -> ctrl_kind -> t
val create_scope : unit -> t
val show : t -> string
val is_ctrl : t -> bool
val is_data : t -> bool
