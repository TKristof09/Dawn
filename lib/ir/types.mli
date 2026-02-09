type 'a sub_lattice =
    | Any
    | Value of 'a
    | All
[@@deriving sexp_of]

val pp_sub_lattice : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a sub_lattice -> unit

type fun_ptr = private {
    params : node_type list;
    ret : node_type;
    fun_indices : [ `Exclude of Core.Int.Set.t | `Include of Core.Int.Set.t ];
  }

and struct_type = private {
    name : string;
    fields : (string * node_type) list;
  }

and const_array = private node_type list

and node_type =
    | ANY
    | Integer of int sub_lattice
    | Tuple of node_type list sub_lattice
    | FunPtr of fun_ptr sub_lattice
    | Ptr of node_type
    | Struct of struct_type sub_lattice
    | ConstArray of const_array sub_lattice
    | Memory
    | Control
    | ALL
[@@deriving show { with_path = false }, sexp_of]

val make_fun_ptr : ?idx:int -> node_type list -> node_type -> node_type
val make_struct : string -> (string * node_type) list -> node_type
val make_array : node_type -> node_type -> node_type
val make_string : string -> node_type
val of_ast_type : Ast.var_type -> node_type
val meet : node_type -> node_type -> node_type
val join : node_type -> node_type -> node_type
val is_constant : node_type -> bool
val get_fun_idx : node_type -> Core.Int.Set.Elt.t
val get_string : node_type -> string option
val is_const_array : node_type -> bool
val get_offset : node_type -> string -> int
