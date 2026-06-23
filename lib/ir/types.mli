type 'a sub_lattice =
    | Any
    | Value of 'a
    | All
[@@deriving sexp_of]

type integer = private {
    min : Z.t;
    max : Z.t;
    num_widens : int;
    fixed_width : int option; (* in bits *)
  }

type fun_ptr = private {
    params : t list;
    ret : t;
    fun_indices : [ `Exclude of Core.Int.Set.t | `Include of Core.Int.Set.t ];
  }

and struct_type = private {
    name : string;
    fields : (string * t) list;
  }

and arr = {
    element_type : t;
    values : t array;
  }

and t =
    | ANY
    | Self
    | Integer of integer sub_lattice
    | Bool of bool sub_lattice
    | Tuple of t list sub_lattice
    | FunPtr of fun_ptr sub_lattice
    | Ptr of t
    | Struct of struct_type sub_lattice
    | Trait of struct_type sub_lattice
    | Array of arr sub_lattice
    | ConstArray of arr sub_lattice
    | Type of t sub_lattice
    | Void
    | Memory
    | Control
    | DeadControl
    | ALL
[@@deriving show { with_path = false }, sexp_of]

val equal : t -> t -> bool

val make_int : ?num_widens:int -> ?fixed_width:int -> Z.t -> Z.t -> t
(** [make_int ?num_widens ?fixed_width min max] creates an integer type for the range [min, max]
    (inclusive on both ends) with optionally specified bit width and num_widens (which is used to
    limit the number of widenings done by [widen_int]) *)

val make_int_const : ?fixed_width:int -> Z.t -> t
(** [make_int_const ?fixed_width n] creates a type for the constant integer n with optionally
    specified bit width *)

val make_fun_ptr : ?idx:int -> t list -> t -> t
(** [make_fun_ptr ?idx params ret] creates a function pointer type with the specified paramets and
    return types with an optionally specified function idx. If [idx] is not specified the we use the
    empty set of function indices *)

val make_struct : string -> (string * t) list -> t
(** [make_struct name fields] creates struct type. [fields] is a list of (field_name, field_type)
    pairs *)

val make_trait : string -> (string * t) list -> t
(** [make_trait name fields] creates trait type. [fields] is a list of (field_name, field_type)
    pairs. Only FunPtr type fields are allowed. *)

val make_array : t -> t -> t
(** [make_arry elem_type len_type] creates an array for elements of type [elem_type] with length
    [len_type]. The length can only be of Integer type and can both be compile time known constant
    or not. *)

val make_string : string -> t
(** [make_string str] creates a u8 ConstArray type with as contents the characters of [str]. *)

val of_ast_type : Ast.var_type -> t

val meet : t -> t -> t
(** [meet t t'] is the lattice meet of [t] and [t'] *)

val join : t -> t -> t
(** [join t t'] is the lattice join of [t] and [t'] *)

val is_constant : t -> bool
val get_fun_idx : t -> int option
(* [get_fun_idx fun_ptr] returns the function idx of [fun_ptr] if it can be determined to be a single possible index, otherwise returns [None] *)

val iter_fun_indices : t -> Core.Int.Set.t -> f:(int -> unit) -> unit
(** [iter_fun_indices fun_ptr universe ~f] iterates the function indices of [fun_ptr] considired in
    the unvirse of indeces [universe]. [universe] is needed as the set of function indices of a
    function pointer can potentially be open (e.g. if the function pointer comes from a memory load)
*)

val get_string : t -> string option
(** [get_string t] gets the string corresponding to the type [t] if [t] is a u8 ConstArray. Returns
    [None] otherwise *)

val is_const_array : t -> bool

val get_offset : t -> string -> int option
(** [get_offset t field] returns the offset in bytes of the field [field] in the type [t]. [t] can
    be Struct, Trait, or Ptr to either of those. Returns [None] for any other type or if the field
    is not found. *)

val get_field_type : t -> string -> t option
(** [get_field_type t field] returns the type of field [field] in [t]. [t] can be Struct, Trait, or
    Ptr to either of those. Returns [None] for any other type or if the field is not found. *)

val is_a : t -> t -> bool
(** [is_a t t'] checks if [t] is a [t']. In terms of lattice operations this is equal to
    [meet t t' = t']. *)

val get_array_element_type : t -> t
(** [get_array_element_type t] gets the element type of an array type [t]. Raises for other types.
*)

val human_readable : t -> string
(** [human_readable t] returns a human readable representation of the type [t]. *)

val get_size : t -> int
(** [get_size t] returns the size of [t] in bytes. Integer types are rounded up to nearest
    byte/word/dword/qword width. *)

val get_top : t -> t
(** [get_top t] gets the top of the local sublattice of the type [t]. *)

val get_integer_const_exn : t -> Z.t
(** [get_integer_const_exn t] returns the value of an integer constant type [t]. Raises for non
    integer and non constant integer types *)

val widen_int : t -> t -> t
(** [widen_int t min_type] widens the integer type [t]'s bit width. The number of widenings of an
    integer type is limited, after widen_int is called more times than this limit it returns
    [min_type]. *)

val is_high : t -> bool
(** [is_high t] checks if [t] is the top of the local sublattice *)

(* Integer types for the common bit widths. *)
val i64 : t
val i32 : t
val i16 : t
val i8 : t
val u64 : t
val u32 : t
val u16 : t
val u8 : t
