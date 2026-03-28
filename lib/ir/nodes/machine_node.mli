type cmp =
    | Eq
    | NEq
    | Lt
    | LEq
    | Gt
    | GEq
[@@deriving show { with_path = false }, sexp_of]

type ideal =
    | Loop
    | CProj of int
    | Start
    | Stop
    | Region
    | Phi
    | External of string
[@@deriving show { with_path = false }, sexp_of]

type machine_node_kind =
    | Int of Z.t
    | Ptr
    | AddrOf
    | ZeroExtend
    | SignExtend
    | Add
    | AddImm of Z.t
    | Sub
    | SubImm of Z.t
    | Mul
    | MulImm of Z.t
    | Div
    | Lsh
    | Rsh
    | And
    | Or
    | LshImm of Z.t
    | RshImm of Z.t
    | AndImm of Z.t
    | OrImm of Z.t
    | Cmp
    | CmpImm of Z.t
    | Set of cmp
    | JmpAlways
    | Jmp of cmp
    | Mov
    | DProj of int
    | FunctionProlog of int
    | Return
    | FunctionCall of int option
    | FunctionCallEnd
    | Param of int
    | CalleeSave of Registers.reg
    | New
    | Store
    | Load
    | Noop
    | Ideal of ideal
[@@deriving show { with_path = false }, sexp_of]

type t = {
    id : int;
    mutable kind : machine_node_kind;
    ir_node : Node.t;
  }
[@@deriving show { with_path = false }, sexp_of]

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
val invert_cond : cmp -> cmp
val invert_jmp : machine_node_kind -> machine_node_kind
val is_cheap_to_clone : t -> bool
val is_control_node : t -> bool
val is_blockhead : t -> bool
val is_two_address : t -> bool
val is_multi_output : t -> bool
val get_in_reg_mask : (t, 'a) Graph.t -> t -> int -> Registers.Mask.t option
val get_out_reg_mask : (t, 'a) Graph.t -> t -> int -> Registers.Mask.t option
val get_register_kills : t -> Registers.Mask.t option
val convert_graph : (Node.t, 'a) Graph.t -> (t, Graph.readwrite) Graph.t
val next_id : unit -> int
val reset_id : unit -> unit
