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

and const_array = private t list

and t =
    | ANY
    | Integer of int sub_lattice
    | Tuple of t list sub_lattice
    | FunPtr of fun_ptr sub_lattice
    | Ptr of t
    | Struct of struct_type sub_lattice
    | ConstArray of const_array sub_lattice
    | Memory
    | Control
    | DeadControl
    | ALL
[@@deriving show { with_path = false }, sexp_of]

val equal : t -> t -> bool
val make_fun_ptr : ?idx:int -> t list -> t -> t
val make_struct : string -> (string * t) list -> t
val make_array : t -> t -> t
val make_string : string -> t
val of_ast_type : Ast.var_type -> t
val meet : t -> t -> t
val join : t -> t -> t
val is_constant : t -> bool
val get_fun_idx : t -> Core.Int.Set.Elt.t
val get_string : t -> string option
val is_const_array : t -> bool
val get_offset : t -> string -> int
val is_a : t -> t -> bool
val get_fun_param_type : t -> int -> t
val get_field_type : t -> string -> t
val human_readable : t -> string
