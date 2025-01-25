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

and operand =
    | Reg of reg
    | Imm of int (* TODO ocaml ints are 63 bits so we need int64 *)
    | Mem of reg * int (* [reg + offset] *)
    | Label of string
    | Weirdo of int * reg * int (* [rbp - x + reg * y] *)

and condition =
    | Eq
    | Z
    | NEq
    | Nz
    | Negative
    | Non_negative
    | Gt
    | GEq
    | Lt
    | LEq
    | Gt_unsigned
    | GEq_unsigned
    | Lt_unsigned
    | LEq_unsigned

and instruction =
    | Mov of operand * operand
    | Add of operand * operand
    | Sub of operand * operand
    | IMul of operand * operand
    | Div of operand
    | Lsh of operand * operand
    | Rsh of operand * operand
    | And of operand * operand
    | Or of operand * operand
    | Push of operand
    | Pop of operand
    | Enter of int
    | Leave
    | Call of string
    | Ret
    | Cmp of operand * operand
    | Test of operand * operand
    | Jmp of string
    | Jmp_cond of condition * string
    | Label of string
    | Set of condition * operand
    | Not of operand
    | StringLiteral of string
[@@deriving show { with_path = false }]
