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

module Mask : sig
  type t [@@deriving show, sexp_of]

  val empty : t

  val general_w : t
  (** Not including RSP *)

  val general_r : t
  val all : t
  val rax : t
  val flags : t
  val of_list : reg list -> t
  val common : t -> t -> t
  val choose : t -> reg option
  val mem : t -> reg -> bool
  val add : t -> reg -> t
  val remove : t -> reg -> t
  val are_disjoint : t -> t -> bool
  val equal : t -> t -> bool
  val diff : t -> t -> t
  val is_empty : t -> bool
  val sexp_of_t : t -> Sexplib0.Sexp.t
end
