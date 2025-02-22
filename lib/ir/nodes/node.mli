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

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int
val create_data : Types.node_type -> data_kind -> t
val create_ctrl : Types.node_type -> ctrl_kind -> t
val create_scope : unit -> t
val show : t -> string
