type cmp =
    | Eq
    | NEq
    | Lt
    | LEq
    | Gt
    | GEq
[@@deriving show { with_path = false }, sexp_of]

type any = AnyNode : 'a t -> any

and _ ideal =
    | Loop : loop ideal
    | CProj : int -> unary ideal
    | Start : unit ideal
    | Stop : stop ideal
    | Region : merge_point ideal
    | Phi : phi ideal
    | External : string -> unit ideal

and _ kind =
    | Int : Z.t -> unit kind
    | Ptr : unit kind
    | AddrOf : unary kind
    | Deref : deref kind
    | ZeroExtend : unary kind
    | SignExtend : unary kind
    | Add : binop kind
    | AddImm : Z.t -> unary kind
    | Sub : binop kind
    | SubImm : Z.t -> unary kind
    | Mul : binop kind
    | MulImm : Z.t -> unary kind
    | Div : binop kind
    | Lsh : binop kind
    | Rsh : binop kind
    | And : binop kind
    | Or : binop kind
    | LshImm : Z.t -> unary kind
    | RshImm : Z.t -> unary kind
    | AndImm : Z.t -> unary kind
    | OrImm : Z.t -> unary kind
    | Cmp : binop kind
    | CmpImm : Z.t -> unary kind
    | Set : cmp -> unary kind
    | JmpAlways : unit kind
    | Jmp : cmp -> unary kind
    | Mov : unary kind
    | DProj : int -> unary kind
    | FunctionProlog : int -> merge_point kind
    | Return : return kind
    | FunctionCall : int option -> fun_call kind
    | FunctionCallEnd : fun_call_end kind
    | Param : int -> phi kind
    | CalleeSave : Registers.reg -> unit kind
    | New : alloc kind
    | Store : store kind
    | Load : load kind
    | Noop : unit kind
    | RepMov : int -> repmov kind
    | Ideal : 'a ideal -> 'a kind

and stop = { mem : any }
and merge_point = { ctrl_inputs : any option list }

and loop = {
    entry : any;
    backedge : any;
  }

and phi = { phi_inputs : any option list }
and unary = { input : any }

and binop = {
    lhs : any;
    rhs : any;
  }

and deref = {
    mem : any;
    ptr : any;
  }

and alloc = {
    mem : any;
    size : any;
  }

and store = {
    mem : any;
    ptr : any;
    value : any;
  }

and load = {
    mem : any;
    ptr : any;
  }

and repmov = {
    mem : any;
    src : any;
    dst : any;
  }

and fun_call = {
    fun_ptr : any option;
    mem : any;
    args : any list;
  }

and fun_call_end = { ret_nodes : any list }

and return = {
    mem : any;
    value : any;
    callee_saves : unit t list;
  }
[@@deriving show { with_path = false }, sexp_of]

and 'a t = {
    id : int;
    mutable kind : 'a kind;
    ir_node : Node2.any;
    list_of_inputs : 'a -> any option list;
    inputs_of_list : any option list -> 'a;
  }
[@@deriving show { with_path = false }, sexp_of]

module N : Graph.NODE with type ('a, 'tag) t = 'a t and type any = any
module G : Graph.S with module N := N

val create_node : 'a kind -> Node2.any -> 'a t
val invert_cond : cmp -> cmp
val is_cheap_to_clone : 'a t -> bool
val is_control_node : 'a t -> bool
val is_blockhead : 'a t -> bool
val is_two_address : 'a t -> bool
val is_multi_output : 'a t -> bool
val get_in_reg_mask : 'q G.t -> 'a t -> int -> Registers.Mask.t option
val get_out_reg_mask : 'q G.t -> 'a t -> int -> Registers.Mask.t option
val get_register_kills : 'a t -> Registers.Mask.t option
val convert_graph : Node2.G.readonly Node2.G.t -> G.readwrite G.t
val next_id : unit -> int
val reset_id : unit -> unit
val id : 'a t -> int
val equal : 'a t -> 'b t -> bool
val equal_kind : 'a kind -> 'b kind -> bool
val type_eq : 'a t -> 'b t -> (('a, 'b) Type.eq * ('taga, 'tagb) Type.eq) option
val unpack : 'a t -> 'b kind -> 'b t option
val unpack_exn : 'a t -> 'b kind -> 'b t
val compare : 'a t -> 'a t -> int
val hash : 'a t -> int
val show : 'a t -> string

module Any : sig
  type t = any

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val sexp_of_t : t -> Sexplib0.Sexp.t
end
