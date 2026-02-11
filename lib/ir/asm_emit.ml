open Core

let asm_of_op (kind : Machine_node.machine_node_kind) =
    match kind with
    | Add
    | AddImm _ ->
        "add"
    | Sub
    | SubImm _ ->
        "sub"
    | Cmp
    | CmpImm _ ->
        "cmp"
    | Mul
    | MulImm _ ->
        "imul"
    | Div -> "idiv"
    | Lsh
    | LshImm _ ->
        "sal"
    | Rsh
    | RshImm _ ->
        "sar"
    | And
    | AndImm _ ->
        "and"
    | Or
    | OrImm _ ->
        "or"
    | JmpAlways -> "jmp"
    | Jmp cond -> (
        (* TODO: signed vs unsigned have different names *)
        match cond with
        | Eq -> "je"
        | NEq -> "jne"
        | Lt -> "jl"
        | LEq -> "jle"
        | Gt -> "jg"
        | GEq -> "jge")
    | Int _ -> "mov"
    | Ptr -> "lea"
    | Mov -> "mov"
    | Set cond -> (
        (* TODO: signed vs unsigned have different names *)
        match cond with
        | Eq -> "sete"
        | NEq -> "setne"
        | Lt -> "setl"
        | LEq -> "setle"
        | Gt -> "setg"
        | GEq -> "setge")
    | DProj _ -> ""
    | Ideal _ -> ""
    | FunctionProlog _ -> "enter"
    | Return -> "ret"
    | FunctionCall _ -> "call"
    | FunctionCallEnd -> ""
    | CalleeSave _ -> ""
    | Param _ -> ""
    | New -> assert false
    | Store -> "mov"
    | Load -> "mov"

let asm_of_loc (loc : Registers.loc) =
    match loc with
    | Reg Flags -> ""
    | Reg reg -> Registers.show_reg reg |> String.lowercase
    | Stack offs ->
        (* HACK: for now we consider everything as 64 bit value but this needs to be dealt with better *)
        let offs = (offs * 8) + 256 in
        Printf.sprintf "[rbp %s 0x%x]" (if offs > 0 then "+" else "-") (abs offs)

let get_label (n : Machine_node.t) =
    match n.kind with
    | Ideal Loop -> Printf.sprintf "Loop_%d" n.id
    | Ideal Stop -> "Exit"
    | _ when Machine_node.is_blockhead n -> Printf.sprintf "L_%d" n.id
    | _ -> failwithf "get_label invalid node: %s" (Machine_node.show n) ()

let rec is_jmp_target g (n : Machine_node.t) =
    Poly.equal n.kind (Ideal Region)
    || List.exists (Graph.get_dependencies g n) ~f:(fun (n' : Machine_node.t option) ->
        match n' with
        | Some { kind = Jmp _; _ }
        | Some { kind = JmpAlways; _ } ->
            true
        | Some ({ kind = Ideal (CProj _); _ } as n') ->
            (* TODO: these useless CProj nodes should just get removed, that would be way cleaner *)
            is_jmp_target g n' && List.length (Graph.get_dependants g n') = 1
        | _ -> false)

let get_first_blockhead g n =
    let n' = ref (Graph.get_dependants g n |> List.find_exn ~f:Machine_node.is_control_node) in
    while not (Machine_node.is_blockhead !n') do
      n' := Graph.get_dependants g !n' |> List.find_exn ~f:Machine_node.is_control_node
    done;
    !n'

let asm_of_node g reg_assoc linker (n : Machine_node.t) prev_node next_node =
    let node_asm =
        match n.kind with
        | Int i ->
            let reg = Hashtbl.find_exn reg_assoc n in
            let op_str = asm_of_op n.kind in
            Printf.sprintf "\t%s %s, %d" op_str (asm_of_loc reg) i
        | Ptr ->
            let reg = Hashtbl.find_exn reg_assoc n in
            let op_str = asm_of_op n.kind in
            let ptr_addr =
                match n.ir_node.typ with
                | Ptr p when Types.is_const_array p -> Printf.sprintf "C_%d" n.ir_node.id
                | _ -> failwith "idk"
            in
            Printf.sprintf "\t%s %s, [%s]" op_str (asm_of_loc reg) ptr_addr
        | AddImm i
        | SubImm i
        | MulImm i
        | CmpImm i
        | LshImm i
        | RshImm i
        | AndImm i
        | OrImm i ->
            let deps = Graph.get_dependencies g n |> List.tl_exn in
            let regs = deps |> List.filter_opt |> List.map ~f:(Hashtbl.find_exn reg_assoc) in
            let op_str = asm_of_op n.kind in
            let reg_str = regs |> List.map ~f:asm_of_loc |> String.concat ~sep:", " in
            Printf.sprintf "\t%s %s, %d" op_str reg_str i
        | Div ->
            let deps = Graph.get_dependencies g n |> List.tl_exn in
            let reg_divisor =
                List.nth_exn deps 1 |> Option.value_exn |> Hashtbl.find_exn reg_assoc
            in
            let reg_str = asm_of_loc reg_divisor in
            let op_str = asm_of_op n.kind in
            (* have to sign extend RAX into RDX for signed division *)
            (* TODO: use xor rdx, rdx  for unsigned division once we have that distinction *)
            Printf.sprintf "\tcqo\n\t%s %s \t\t// rax = rax / %s" op_str reg_str reg_str
        | Ideal Stop ->
            let epilogue = "\t;Exit program\n\tmov rax, 60\n\txor rdi, rdi\n\tsyscall" in
            epilogue
        | Ideal _ -> ""
        | Jmp _ ->
            let true_branch =
                Graph.get_dependants g n
                |> List.find_exn ~f:(fun n ->
                    match n.kind with
                    | Ideal (CProj 0) -> true
                    | _ -> false)
            in
            let false_branch =
                Graph.get_dependants g n
                |> List.find_exn ~f:(fun n ->
                    match n.kind with
                    | Ideal (CProj 1) -> true
                    | _ -> false)
            in
            let next_node = Option.value_exn next_node in
            let target_branch =
                if Machine_node.equal next_node true_branch then false_branch else true_branch
            in
            let op =
                if Machine_node.equal next_node true_branch then
                  Machine_node.invert_jmp n.kind
                else
                  n.kind
            in
            let op_str, label_str =
                (* FIXME: this is not correct when we have cproj ->
                    functioncall(ptr/const) because the ptr/const might not be
                    part of the cproj bb. Probably need to check List.filter
                    ~f:is_blockhead |> is_empty or something like that *)
                if List.length (Graph.get_dependants g target_branch) = 1 && false then
                  let next_target_bb = get_first_blockhead g target_branch in
                  (asm_of_op op, get_label next_target_bb)
                else
                  (asm_of_op op, get_label target_branch)
            in
            Printf.sprintf "\t%s %s\n" op_str label_str
        | JmpAlways ->
            let op_str = asm_of_op n.kind in
            let target = Graph.get_dependants g n |> List.hd_exn in
            let label_str = get_label target in
            Printf.sprintf "\t%s %s\n" op_str label_str
        | Lsh
        | Rsh ->
            let deps = Graph.get_dependencies g n |> List.tl_exn in
            let reg = deps |> List.hd_exn |> Option.value_exn |> Hashtbl.find_exn reg_assoc in
            let op_str = asm_of_op n.kind in
            Printf.sprintf "\t%s %s, cl" op_str (asm_of_loc reg)
        | Add
        | Sub
        | Mul
        | And
        | Or
        | Cmp
        | Mov ->
            let deps = Graph.get_dependencies g n |> List.tl_exn in
            (* if not two address node, add the node itself to the start of the list to get the output reg too *)
            let nodes =
                if
                  (not (Machine_node.is_two_address n))
                  &&
                  (* TODO this is ugly just to get cmp node assembly to not be weird *)
                  match Machine_node.get_out_reg_mask g n 0 with
                  | None -> false
                  | Some m when Registers.Mask.equal m Registers.Mask.flags -> false
                  | Some _ -> true
                then
                  Some n :: deps
                else
                  deps
            in
            let regs = nodes |> List.filter_opt |> List.map ~f:(Hashtbl.find_exn reg_assoc) in
            let op_str = asm_of_op n.kind in
            let reg_str = regs |> List.map ~f:asm_of_loc |> String.concat ~sep:", " in
            Printf.sprintf "\t%s %s" op_str reg_str
        | Set _ ->
            let op_str = asm_of_op n.kind in
            let reg = Hashtbl.find_exn reg_assoc n in
            Printf.sprintf "\t%s %s" op_str (asm_of_loc reg)
        | DProj _ -> ""
        | FunctionProlog i ->
            let target = Linker.get_name linker i in
            Printf.sprintf "%s:\n\tpush rbp\n\tmov rbp, rsp" target
        | Return -> Printf.sprintf "\tleave\n\t%s" (asm_of_op n.kind)
        | Param _ -> ""
        | FunctionCall i ->
            let op = asm_of_op n.kind in
            let target = Linker.get_name linker i in
            Printf.sprintf "\t%s %s" op target
        | FunctionCallEnd -> ""
        | CalleeSave _ -> ""
        | New ->
            let ptr =
                Graph.get_dependants g n
                |> List.find_exn ~f:(fun n ->
                    match n.kind with
                    | DProj 1 -> true
                    | _ -> false)
            in
            let ptr_reg = Hashtbl.find_exn reg_assoc ptr in
            let size_reg =
                Hashtbl.find_exn reg_assoc (Graph.get_dependency g n 2 |> Option.value_exn)
            in
            assert (Poly.equal ptr_reg (Reg RAX));
            assert (Poly.equal size_reg (Reg RDI));
            (* HACK: this is only until i get heap memory alloc *)
            Printf.sprintf "\tsub rsp, %s   ; alloc\n\tmov %s, rsp" (asm_of_loc size_reg)
              (asm_of_loc ptr_reg)
        | Store ->
            let reg = Hashtbl.find_exn reg_assoc (Graph.get_dependency g n 4 |> Option.value_exn) in
            let ptr_reg =
                Hashtbl.find_exn reg_assoc (Graph.get_dependency g n 2 |> Option.value_exn)
            in
            let offset_reg =
                Hashtbl.find_exn reg_assoc (Graph.get_dependency g n 3 |> Option.value_exn)
            in
            let op_str = asm_of_op n.kind in
            Printf.sprintf "\t%s [%s + %s], %s" op_str (asm_of_loc ptr_reg) (asm_of_loc offset_reg)
              (asm_of_loc reg)
        | Load ->
            let reg = Hashtbl.find_exn reg_assoc n in
            let ptr_reg =
                Hashtbl.find_exn reg_assoc (Graph.get_dependency g n 2 |> Option.value_exn)
            in
            let offset_reg =
                Hashtbl.find_exn reg_assoc (Graph.get_dependency g n 3 |> Option.value_exn)
            in
            let op_str = asm_of_op n.kind in
            Printf.sprintf "\t%s %s, [%s + %s]" op_str (asm_of_loc reg) (asm_of_loc ptr_reg)
              (asm_of_loc offset_reg)
    in
    (* let node_loc = *)
    (*     Printf.sprintf "%s:%d:%d" n.ir_node.loc.filename n.ir_node.loc.line n.ir_node.loc.col *)
    (* in *)
    (* let node_asm = Printf.sprintf "%s ; %s" node_asm node_loc in *)
    let jmp_target =
        match prev_node with
        | None -> None
        | Some prev_node ->
            let cfg_prev =
                if Machine_node.is_control_node prev_node then
                  prev_node
                else
                  Graph.get_dependency g prev_node 0 |> Option.value_exn
            in
            let precedent =
                match Graph.get_dependency g n 0 with
                | Some n -> n
                | None ->
                    Graph.get_dependencies g n
                    |> List.filter_opt
                    |> List.find_exn ~f:Machine_node.is_control_node
            in
            if Machine_node.is_blockhead n && not (Machine_node.equal precedent cfg_prev) then
              let target =
                  Graph.get_dependants g cfg_prev |> List.find_exn ~f:Machine_node.is_blockhead
              in
              Some target
            else
              None
    in
    match jmp_target with
    | None ->
        if is_jmp_target g n then
          let this_label_str = get_label n in
          Printf.sprintf "%s:\n%s" this_label_str node_asm
        else
          node_asm
    | Some jmp_target ->
        let op_str = asm_of_op JmpAlways in
        let target_label_str = get_label jmp_target in
        let this_label_str = get_label n in
        Printf.sprintf "\t%s %s\n%s:\n%s" op_str target_label_str this_label_str node_asm

let emit_function g reg_assoc program linker =
    let rec aux l prev =
        match l with
        | [] -> []
        | [ n ] -> [ asm_of_node g reg_assoc linker n prev None ]
        | n :: n' :: t -> asm_of_node g reg_assoc linker n prev (Some n') :: aux (n' :: t) (Some n)
    in
    (* let program = add_jumps g program in *)
    (* |> invert_loop_conditions g *)
    aux program None |> List.filter ~f:(Fun.negate String.is_empty) |> String.concat ~sep:"\n"

let stdlib_functions =
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
    print ^ print_int

let gather_const_arrays functions =
    let arr_map = Hashtbl.create (module Int) in
    List.iter functions ~f:(fun (g, _, _) ->
        Graph.iter g ~f:(fun (n : Machine_node.t) ->
            match n.kind with
            | Ptr -> (
                match Types.get_string n.ir_node.typ with
                | None -> ()
                | Some s ->
                    let with_len =
                        Printf.sprintf "\n\tdq %d\n\tdb %s" (String.length s)
                          (String.to_list s
                          |> List.map ~f:(fun c -> c |> Char.to_int |> Int.to_string)
                          |> String.concat ~sep:", ")
                    in
                    Hashtbl.add arr_map ~key:n.ir_node.id ~data:with_len |> ignore)
            | _ -> ()));
    Hashtbl.to_alist arr_map
    |> List.map ~f:(fun (id, arr) -> Printf.sprintf "C_%d: %s" id arr)
    |> String.concat ~sep:"\n"

let emit_program functions linker =
    let header = "format ELF64 executable 3\nentry start\nsegment readable executable\n" in
    let code =
        List.map functions ~f:(fun (g, reg_assignment, prog) ->
            emit_function g reg_assignment prog linker)
        |> String.concat ~sep:"\n\n"
    in
    let constants = gather_const_arrays functions in
    let rodata =
        if String.is_empty constants then
          ""
        else
          Printf.sprintf "segment readable\nalign 8\n%s" constants
    in
    let asm_content =
        header ^ stdlib_functions ^ "\n\n" ^ "start:\n\tmov rbp, rsp\n" ^ code ^ "\n" ^ rodata
    in
    asm_content
