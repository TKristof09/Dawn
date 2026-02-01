type reg =
    | RAX
    | RBX
    | RCX
    | RDX
    | RSI
    | RDI
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    | RSP
    | RBP
    | Flags
[@@deriving show]

type loc =
    | Reg of reg
    | Stack of int
[@@deriving show { with_path = false }, compare, sexp]

module Mask : sig
  type t [@@deriving show, sexp_of]

  val empty : t

  val general_w : t
  (** Not including RSP *)

  val general_r : t
  val all : t
  val all_and_stack : t
  val rax : t
  val cl : t
  val div : t
  val flags : t
  val of_list : loc list -> t
  val common : t -> t -> t
  val choose : t -> loc option
  val mem : t -> loc -> bool
  val add : t -> loc -> t
  val remove : t -> loc -> t
  val are_disjoint : t -> t -> bool
  val are_overlapping : t -> t -> bool
  val equal : t -> t -> bool
  val diff : t -> t -> t
  val is_empty : t -> bool
  val length : t -> int
  val sexp_of_t : t -> Sexplib0.Sexp.t
end
