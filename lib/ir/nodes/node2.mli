type data = [ `Data ]
type ctrl = [ `Ctrl ]
type mem = [ `Mem ]
type misc = [ `Misc ]

type any = AnyNode : ('a, 'tag) t -> any
and any_data = AnyData : ('a, data) t -> any_data
and any_ctrl = AnyCtrl : ('a, ctrl) t -> any_ctrl
and any_mem = AnyMem : ('a, mem) t -> any_mem

and _ data_kind =
    | Constant : unit data_kind
    | Add : binop data_kind
    | Sub : binop data_kind
    | Mul : binop data_kind
    | Div : binop data_kind
    | Lsh : binop data_kind
    | Rsh : binop data_kind
    | BAnd : binop data_kind
    | BOr : binop data_kind
    | Eq : binop data_kind
    | NEq : binop data_kind
    | Lt : binop data_kind
    | LEq : binop data_kind
    | Gt : binop data_kind
    | GEq : binop data_kind
    | Phi : any_data phi data_kind
    (* proj nodes take in any because they need to be able to project off of a node that can be of other type (e.g. create pointer off of a New node *)
    | Proj : int -> any unary data_kind
    | Param : int -> any_data phi data_kind
    | External : string -> extern_fun data_kind
    | Cast : any_data unary data_kind
    | Load : string -> load data_kind
    | AddrOf : addr_of data_kind
    | AddrOfField : string -> addr_of data_kind
    | Deref : deref data_kind

and _ ctrl_kind =
    | Start : unit ctrl_kind
    | Stop : stop ctrl_kind
    | Proj : int -> any unary ctrl_kind
    | If : any_data unary ctrl_kind
    | Region : merge_point ctrl_kind
    | Loop : loop ctrl_kind
    | Function : {
        ret : (ret, ctrl) t;
        signature : Types.t;
        idx : int;
      }
        -> fun_def ctrl_kind
    | Return : ret ctrl_kind
    | FunctionCall : fun_call ctrl_kind
    | FunctionCallEnd : fun_call_end ctrl_kind

and _ mem_kind =
    | New : alloc mem_kind
    | Store : string -> store mem_kind
    | Copy : copy mem_kind
    | Phi : any_mem phi mem_kind
    | Param : any_mem phi mem_kind
    | Proj : int -> any unary mem_kind

and ('a, 'tag) kind =
    | Data : 'a data_kind -> ('a, data) kind
    | Ctrl : 'a ctrl_kind -> ('a, ctrl) kind
    | Mem : 'a mem_kind -> ('a, mem) kind
    | Scope : any Symbol_table.t -> (scope_kind, misc) kind
    | ForwardRef : string -> (unit, data) kind

and binop = {
    lhs : any_data option;
    rhs : any_data option;
  }

and 'a unary = { input : 'a option }
and 'a phi = { phi_inputs : 'a option list }
and stop = { mem : any_mem option }
and merge_point = { ctrl_inputs : any_ctrl option list }

and loop = {
    entry : any_ctrl option;
    backedge : any_ctrl option;
  }

and fun_def = { call_sites : any_ctrl option list }
and extern_fun = { params : any_data list }

and ret = {
    mem : any_mem option;
    data : any_data option;
  }

and fun_call = {
    fun_ptr : any_data option;
    mem : any_mem option;
    args : any_data option list;
  }

and fun_call_end = { ret_nodes : any_ctrl option list }

and alloc = {
    mem : any_mem option;
    size : any_data option;
  }

and load = {
    mem : any_mem option;
    ptr : any_data option;
  }

and store = {
    mem : any_mem option;
    ptr : any_data option;
    value : any_data option;
  }

and addr_of = {
    place : any_data option;
    offset : any_data option;
  }

and deref = {
    mem : any_mem option;
    ptr : any_data option;
  }

and copy = {
    mem : any_mem option;
    src : any_data option;
    dst : any_data option;
  }

and scope_kind = { vars : any option list }

and ('a, 'tag) t = {
    mutable typ : Types.t;
    mutable min_typ : Types.t option;
    mutable kind : ('a, 'tag) kind;
    id : int;
    loc : Ast.loc;
    mutable parent_fun : int option;
    list_of_inputs : 'a -> any option list;
    inputs_of_list : any option list -> 'a;
  }
[@@deriving sexp_of]

val id : ('a, 'tag) t -> int
val equal : ('a, 'taga) t -> ('b, 'tagb) t -> bool
val type_eq : ('a, 'taga) t -> ('b, 'tagb) t -> (('a, 'b) Type.eq * ('taga, 'tagb) Type.eq) option

val kind_eq :
  ('a, 'taga) kind -> ('b, 'tagb) kind -> (('a, 'b) Type.eq * ('taga, 'tagb) Type.eq) option

val unpack : ('a, 'c) t -> ('b, 'd) kind -> ('b, 'd) t option
val unpack_exn : ('a, 'c) t -> ('b, 'd) kind -> ('b, 'd) t
val as_ctrl_exn : ('a, 'b) t -> ('a, ctrl) t
val as_data_exn : ('a, 'b) t -> ('a, data) t
val as_mem_exn : ('a, 'b) t -> ('a, mem) t
val create_data : ?parent_fun:int -> Ast.loc -> Types.t -> 'a data_kind -> ('a, data) t
val create_ctrl : ?parent_fun:int -> Ast.loc -> Types.t -> 'a ctrl_kind -> ('a, ctrl) t
val create_mem : ?parent_fun:int -> Ast.loc -> Types.t -> 'a mem_kind -> ('a, mem) t
val create_scope : unit -> (scope_kind, misc) t
val create_forward_ref : string -> (unit, data) t
val pp : Format.formatter -> ('a, 'b) t -> unit
val pp_kind : Format.formatter -> ('a, 'b) kind -> unit
val show : ('a, 'b) t -> string
val show_kind : ('a, 't) kind -> string
val is_ctrl : ('a, 'b) t -> bool
val is_data : ('a, 'b) t -> bool
val is_blockhead : ('a, 'b) t -> bool

module N : Graph.NODE with type ('a, 'tag) t = ('a, 'tag) t and type any = any
module G : Graph.S with module N := N

module Any : sig
  type t = any

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

module AnyData : sig
  type t = any_data

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

module AnyCtrl : sig
  type t = any_ctrl

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

module AnyMem : sig
  type t = any_mem

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val sexp_of_t : t -> Sexplib0.Sexp.t
end
