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
        signature : Types.t;
        idx : int;
      }
    | Return
    | FunctionCall
    | FunctionCallEnd

and mem_kind =
    | New
    | Load of string
    | Store of string

and kind =
    | Data of data_kind
    | Ctrl of ctrl_kind
    | Mem of mem_kind
    | Scope of t Symbol_table.t

and t = {
    mutable typ : Types.t;
    mutable kind : kind;
    id : int;
    loc : Ast.loc;
  }
[@@deriving sexp_of]

val semantic_equal : t -> t option list -> t -> t option list -> bool
(** Check for semantical equality. Basically same kind and same inputs *)

val equal : t -> t -> bool
(** Chek for equality of ids *)

val compare : t -> t -> int
val hash : t -> int
val create_data : Ast.loc -> Types.t -> data_kind -> t
val create_ctrl : Ast.loc -> Types.t -> ctrl_kind -> t
val create_mem : Ast.loc -> Types.t -> mem_kind -> t
val create_scope : unit -> t
val show : t -> string
val show_kind : kind -> string
val pp : Format.formatter -> t -> unit
val is_ctrl : t -> bool
val is_data : t -> bool
val is_scope : t -> bool
val is_blockhead : t -> bool
val reset_id : unit -> unit
