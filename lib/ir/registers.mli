type reg =
    | RAX
    | RBX
    | RCX
    | RDX
    | RSI
    | RDI
    | RSP
    | RBP
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    | Flags

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
  val any : t -> reg
  val sexp_of_t : t -> Sexplib0.Sexp.t
end
