open Core

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
[@@deriving show { with_path = false }, compare, sexp]

type loc =
    | Reg of reg
    | Stack of int [@compare.custom compare_stack_slots]
[@@deriving show { with_path = false }, compare, sexp]

let compare_loc l l' =
    match (l, l') with
    | Reg r, Reg r' -> compare_reg r r'
    | Reg _, _ -> -1
    | _, Reg _ -> 1
    | Stack i, Stack i' ->
        (* Make Stack -1 be greater than any stack slot as -1 is a placeholder for "any slot" so we don't want Mask.choose to ever give this *)
        if i = -1 && i' <> -1 then
          1
        else if i <> -1 && i' = -1 then
          -1
        else
          Int.compare i i'

module Mask : sig
  type t [@@deriving show, sexp_of]

  val empty : t
  val general_w : t
  val general_r : t
  val all : t
  val all_and_stack : t
  val rax : t
  val cl : t
  val div : t
  val flags : t
  val x64_systemv : int -> t
  val caller_save : t
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
end = struct
  open Core

  module S = Set.Make (struct
    type t = loc [@@deriving compare, sexp]
  end)

  type t = S.t [@@deriving sexp_of]

  let show m = Set.to_list m |> [%derive.show: loc list]
  let pp fmt m = Format.fprintf fmt "%s" (show m)
  let empty = S.empty

  let general_w =
      S.of_list
        [
          Reg RAX;
          Reg RBX;
          Reg RCX;
          Reg RDX;
          Reg RSI;
          Reg RDI;
          Reg RBP;
          Reg R8;
          Reg R9;
          Reg R10;
          Reg R11;
          Reg R12;
          Reg R13;
          Reg R14;
          Reg R15;
        ]

  let general_r =
      S.of_list
        [
          Reg RAX;
          Reg RBX;
          Reg RCX;
          Reg RDX;
          Reg RSI;
          Reg RDI;
          Reg RSP;
          Reg RBP;
          Reg R8;
          Reg R9;
          Reg R10;
          Reg R11;
          Reg R12;
          Reg R13;
          Reg R14;
          Reg R15;
        ]

  let all =
      S.of_list
        [
          Reg RAX;
          Reg RBX;
          Reg RCX;
          Reg RDX;
          Reg RSI;
          Reg RDI;
          Reg RSP;
          Reg RBP;
          Reg R8;
          Reg R9;
          Reg R10;
          Reg R11;
          Reg R12;
          Reg R13;
          Reg R14;
          Reg R15;
          Reg Flags;
        ]

  let all_and_stack = Set.add all (Stack (-1))
  let rax = S.of_list [ Reg RAX ]
  let cl = S.of_list [ Reg RCX ]
  let div = Set.diff general_r (S.of_list [ Reg RAX; Reg RDX ])
  let flags = S.singleton (Reg Flags)

  let x64_systemv i =
      match i with
      | 0 -> S.singleton (Reg RDI)
      | 1 -> S.singleton (Reg RSI)
      | 2 -> S.singleton (Reg RDX)
      | 3 -> S.singleton (Reg RCX)
      | 4 -> S.singleton (Reg R8)
      | 5 -> S.singleton (Reg R9)
      | _ -> failwith "TODO: systemv arguments on stack "

  let caller_save =
      S.of_list
        [ Reg RAX; Reg RCX; Reg RDX; Reg RDI; Reg RSI; Reg R8; Reg R9; Reg R10; Reg R11; Reg Flags ]

  let of_list = S.of_list
  let common a b = Set.inter a b
  let choose = Set.min_elt
  let mem = Set.mem
  let add = Set.add
  let remove = Set.remove
  let are_disjoint = Set.are_disjoint
  let are_overlapping m m' = not (are_disjoint m m')
  let equal = Set.equal
  let diff = Set.diff
  let is_empty = Set.is_empty
  let length = Set.length

  (* TODO *)
  let sexp_of_t _ = Sexplib0.Sexp.Atom "RegisterMask"
end
