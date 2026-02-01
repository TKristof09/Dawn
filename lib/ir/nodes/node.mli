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

and ctrl_kind =
    | Start
    | Stop
    | Proj of int
    | If
    | Region
    | Loop
    | Function of t
    | Return
    | FunctionCall
    | FunctionCallEnd

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

val semantic_equal : t -> t option list -> t -> t option list -> bool
(** Check for semantical equality. Basically same kind and same inputs *)

val equal : t -> t -> bool
(** Chek for equality of ids *)

val compare : t -> t -> int
val hash : t -> int
val create_data : Types.node_type -> data_kind -> t
val create_ctrl : Types.node_type -> ctrl_kind -> t
val create_scope : unit -> t
val show : t -> string
val show_kind : kind -> string
val pp : Format.formatter -> t -> unit
val is_ctrl : t -> bool
val is_data : t -> bool
val is_scope : t -> bool
val is_blockhead : t -> bool
