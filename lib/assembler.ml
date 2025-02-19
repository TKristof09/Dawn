open Core
open Asm

let assemble (functions, instrs) out_filename =
    let string_of_reg reg size =
        match reg with
        | RAX -> (
            match size with
            | 8 -> "al"
            | 16 -> "ax"
            | 32 -> "eax"
            | 64 -> "rax"
            | _ -> failwith "Invalid register size")
        | RBX -> (
            match size with
            | 8 -> "bl"
            | 16 -> "bx"
            | 32 -> "ebx"
            | 64 -> "rbx"
            | _ -> failwith "Invalid register size")
        | RCX -> (
            match size with
            | 8 -> "cl"
            | 16 -> "cx"
            | 32 -> "ecx"
            | 64 -> "rcx"
            | _ -> failwith "Invalid register size")
        | RDX -> (
            match size with
            | 8 -> "dl"
            | 16 -> "dx"
            | 32 -> "edx"
            | 64 -> "rdx"
            | _ -> failwith "Invalid register size")
        | RSI -> (
            match size with
            | 8 -> "sil"
            | 16 -> "si"
            | 32 -> "esi"
            | 64 -> "rsi"
            | _ -> failwith "Invalid register size")
        | RDI -> (
            match size with
            | 8 -> "dil"
            | 16 -> "di"
            | 32 -> "edi"
            | 64 -> "rdi"
            | _ -> failwith "Invalid register size")
        | RSP -> (
            match size with
            | 8 -> "spl"
            | 16 -> "sp"
            | 32 -> "esp"
            | 64 -> "rsp"
            | _ -> failwith "Invalid register size")
        | RBP -> (
            match size with
            | 8 -> "bpl"
            | 16 -> "bp"
            | 32 -> "ebp"
            | 64 -> "rbp"
            | _ -> failwith "Invalid register size")
        | R8 -> (
            match size with
            | 8 -> "r8b"
            | 16 -> "r8w"
            | 32 -> "r8d"
            | 64 -> "r8"
            | _ -> failwith "Invalid register size")
        | R9 -> (
            match size with
            | 8 -> "r9b"
            | 16 -> "r9w"
            | 32 -> "r9d"
            | 64 -> "r9"
            | _ -> failwith "Invalid register size")
        | R10 -> (
            match size with
            | 8 -> "r10b"
            | 16 -> "r10w"
            | 32 -> "r10d"
            | 64 -> "r10"
            | _ -> failwith "Invalid register size")
        | R11 -> (
            match size with
            | 8 -> "r11b"
            | 16 -> "r11w"
            | 32 -> "r11d"
            | 64 -> "r11"
            | _ -> failwith "Invalid register size")
        | R12 -> (
            match size with
            | 8 -> "r12b"
            | 16 -> "r12w"
            | 32 -> "r12d"
            | 64 -> "r12"
            | _ -> failwith "Invalid register size")
        | R13 -> (
            match size with
            | 8 -> "r13b"
            | 16 -> "r13w"
            | 32 -> "r13d"
            | 64 -> "r13"
            | _ -> failwith "Invalid register size")
        | R14 -> (
            match size with
            | 8 -> "r14b"
            | 16 -> "r14w"
            | 32 -> "r14d"
            | 64 -> "r14"
            | _ -> failwith "Invalid register size")
        | R15 -> (
            match size with
            | 8 -> "r15b"
            | 16 -> "r15w"
            | 32 -> "r15d"
            | 64 -> "r15"
            | _ -> failwith "Invalid register size")
    in

    let string_of_operand ?(reg_size = 64) = function
        | Reg r -> string_of_reg r reg_size
        | Imm i -> string_of_int i
        | Mem (r, offset) -> sprintf "[%s + %d]" (string_of_reg r 64) offset
        | Label l -> l
        | ScaledIndexed (x, r, y) -> sprintf "[rbp + %d + %s * %d]" x (string_of_reg r 64) y
    in

    let string_of_condition = function
        | Eq -> "e"
        | Z -> "z"
        | NEq -> "ne"
        | Nz -> "nz"
        | Negative -> "s"
        | Non_negative -> "ns"
        | Gt -> "g"
        | GEq -> "ge"
        | Lt -> "l"
        | LEq -> "le"
        | Gt_unsigned -> "a"
        | GEq_unsigned -> "ae"
        | Lt_unsigned -> "b"
        | LEq_unsigned -> "be"
    in

    let string_of_instruction strings = function
        | Mov (dst, src) -> sprintf "    mov %s, %s" (string_of_operand dst) (string_of_operand src)
        | Add (dst, src) -> sprintf "    add %s, %s" (string_of_operand dst) (string_of_operand src)
        | Sub (dst, src) -> sprintf "    sub %s, %s" (string_of_operand dst) (string_of_operand src)
        | IMul (dst, src) ->
            sprintf "    imul %s, %s" (string_of_operand dst) (string_of_operand src)
        | Div src -> sprintf "    xor rdx, rdx\n    idiv %s" (string_of_operand src)
        | Lsh (dst, src) ->
            sprintf "    shl %s, %s" (string_of_operand dst) (string_of_operand ~reg_size:8 src)
        | Rsh (dst, src) ->
            sprintf "    shr %s, %s" (string_of_operand dst) (string_of_operand ~reg_size:8 src)
        | And (dst, src) -> sprintf "    and %s, %s" (string_of_operand dst) (string_of_operand src)
        | Or (dst, src) -> sprintf "    or %s, %s" (string_of_operand dst) (string_of_operand src)
        | Push op -> sprintf "    push %s" (string_of_operand op)
        | Pop op -> sprintf "    pop %s" (string_of_operand op)
        | Enter x when x > 0 -> sprintf "    push rbp\n    mov rbp, rsp\n    sub rsp, %d" x
        | Enter _ -> sprintf "    push rbp\n    mov rbp, rsp"
        | Leave -> sprintf "    leave"
        | Call name -> sprintf "    call %s" name
        | Ret -> "    ret"
        | Cmp (op1, op2) -> sprintf "    cmp %s, %s" (string_of_operand op1) (string_of_operand op2)
        | Test (op1, op2) ->
            sprintf "    test %s, %s" (string_of_operand op1) (string_of_operand op2)
        | Jmp label -> sprintf "    jmp %s" label
        | Jmp_cond (cond, label) -> sprintf "    j%s %s" (string_of_condition cond) label
        | Label name -> sprintf "%s:" name
        | Set (cond, dst) ->
            sprintf "    set%s %s" (string_of_condition cond) (string_of_operand ~reg_size:8 dst)
        | Not op -> sprintf "    not %s" (string_of_operand op)
        | StringLiteral s ->
            let label_id =
                Hashtbl.update_and_return strings s ~f:(function
                  | None -> Hashtbl.length strings
                  | Some label_id -> label_id)
            in
            sprintf "    lea rax, [STR_%d]" label_id
    in

    let header = "format ELF64 executable 3\nentry start\nsegment readable executable\n" in
    let print_int =
        {|
print_int:
    mov     rcx, rdi
    sub     rsp, 40
    mov     esi, 20
    mov     r8d, 10
    neg     rcx
    cmovs   rcx, rdi
.L2:
    mov     rax, rcx
    xor     edx, edx
    div     r8
    add     edx, 48
    mov     [rsp+11+rsi], dl
    mov     rdx, rcx
    mov     rcx, rax
    mov     rax, rsi
    dec     rsi
    cmp     rdx, 9
    ja      .L2
    test    rdi, rdi
    jns     .L4
    dec     eax
    movsxd  rdx, eax
    mov     BYTE [rsp+11+rdx], 45
.L4:
    cdqe
    mov     edx, 21
    mov     edi, 1
    lea     rsi, [rsp+11+rax]
    sub     rdx, rax
    mov     rax, 1
    syscall
    add     rsp, 40
    ret|}
    in
    let print =
        {|
print:
    mov     rsi, rdi
    add     rsi, 8
    mov     rdx, [rdi]
    mov     rax, 1
    mov     rdi, 1
    syscall
    ret|}
    in
    let stdlib_functions = print ^ print_int in
    let start = "\nstart:\n    mov rbp, rsp\n" in
    let exit_sequence =
        "\n    mov rax, 60    ; exit syscall\n    xor rdi, rdi  ; exit code 0\n    syscall\n"
    in
    let strings = Hashtbl.create (module String) in
    let asm_content =
        header
        ^ stdlib_functions
        ^ "\n"
        ^ String.concat ~sep:"\n" (List.map ~f:(string_of_instruction strings) functions)
        ^ start
        ^ String.concat ~sep:"\n" (List.map ~f:(string_of_instruction strings) instrs)
        ^ "\n"
        ^ exit_sequence
    in
    let string_literals =
        if Hashtbl.is_empty strings then
          ""
        else
          "\nsegment readable\nalign 8\n"
          ^ Hashtbl.fold strings ~init:"" ~f:(fun ~key:str ~data:label_id acc ->
                (* we can't put escaped characters like \n in string literals in fasm, instead we'd have to replace the escaped characters with their ascii code, I'm too lazy to find each escaped character so I just replace every character with their ascii code *)
                let s =
                    String.concat_map ~sep:"," str ~f:(fun c -> c |> Char.to_int |> Int.to_string)
                in
                acc
                ^ sprintf "STR_%d:\n    ; %s\n    dq %d\n    db %s, 0\n" label_id
                    (String.escaped str) (String.length str) s)
    in
    Out_channel.write_all out_filename ~data:(asm_content ^ string_literals)
