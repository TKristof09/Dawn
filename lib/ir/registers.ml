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
[@@deriving show { with_path = false }, compare, sexp]

module Mask : sig
  type t [@@deriving show, sexp_of]

  val empty : t
  val general_w : t
  val general_r : t
  val all : t
  val rax : t
  val flags : t
  val of_list : reg list -> t
  val common : t -> t -> t
  val any : t -> reg
  val sexp_of_t : t -> Sexplib0.Sexp.t
end = struct
  open Core

  module S = Set.Make (struct
    type t = reg [@@deriving compare, sexp]
  end)

  type t = S.t [@@deriving sexp_of]

  let show m = Set.to_list m |> [%derive.show: reg list]
  let pp fmt m = Format.fprintf fmt "%s" (show m)
  let empty = S.empty

  let general_w =
      S.of_list [ RAX; RBX; RCX; RDX; RSI; RDI; RBP; R8; R9; R10; R11; R12; R13; R14; R15 ]

  let general_r =
      S.of_list [ RAX; RBX; RCX; RDX; RSI; RDI; RSP; RBP; R8; R9; R10; R11; R12; R13; R14; R15 ]

  let all =
      S.of_list
        [ RAX; RBX; RCX; RDX; RSI; RDI; RSP; RBP; R8; R9; R10; R11; R12; R13; R14; R15; Flags ]

  let rax = S.of_list [ RAX ]
  let of_list = S.of_list
  let common a b = Set.inter a b
  let any = Set.choose_exn
  let flags = S.singleton Flags

  (* TODO *)
  let sexp_of_t _ = Sexplib0.Sexp.Atom "RegisterMask"
end
