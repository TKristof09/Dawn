type 'a sub_lattice =
    | Any
    | Value of 'a
    | All
[@@deriving sexp_of]

val pp_sub_lattice : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a sub_lattice -> unit

type fun_ptr = private {
    params : t list;
    ret : t;
    fun_indices : [ `Exclude of Core.Int.Set.t | `Include of Core.Int.Set.t ];
  }

and struct_type = private {
    name : string;
    fields : (string * t) list;
  }

and const_array = {
    element_type : t;
    values : t list;
  }

and integer = private {
    min : int;
    max : int;
    num_widens : int;
  }

and t =
    | ANY
    | Integer of integer sub_lattice
    | Bool of bool sub_lattice
    | Tuple of t list sub_lattice
    | FunPtr of fun_ptr sub_lattice
    | Ptr of t
    | Struct of struct_type sub_lattice
    | ConstArray of const_array sub_lattice
    | Type of t sub_lattice
    | Void
    | Memory
    | Control
    | DeadControl
    | ALL
[@@deriving show { with_path = false }, sexp_of]

val equal : t -> t -> bool
val make_int : ?num_widens:int -> int -> int -> t
val make_int_const : int -> t
val make_fun_ptr : ?idx:int -> t list -> t -> t
val make_struct : string -> (string * t) list -> t
val make_array : t -> t -> t
val make_string : string -> t
val of_ast_type : Ast.var_type -> t
val meet : t -> t -> t
val join : t -> t -> t
val is_constant : t -> bool
val get_fun_idx : t -> int option
val iter_fun_indices : t -> Core.Int.Set.t -> f:(int -> unit) -> unit
val get_string : t -> string option
val is_const_array : t -> bool
val get_offset : t -> string -> int option
val is_a : t -> t -> bool
val get_field_type : t -> string -> t option
val human_readable : t -> string
val get_size : t -> int
val get_top : t -> t
val get_integer_const_exn : t -> int
val widen_int : t -> t -> t
val i32 : t
val i16 : t
val i8 : t
val u32 : t
val u16 : t
val u8 : t
