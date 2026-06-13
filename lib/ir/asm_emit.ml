open Core
module Graph = Machine_node.G

let asm_of_op : type a. a Machine_node.kind -> string = function
    | ZeroExtend -> failwith "Handle zero extend outside this function"
    | SignExtend -> failwith "Handle sign extend outside this function"
    | AddrOf -> "lea"
    | Deref -> "mov"
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
    | Load ->
        (* TODO: as an optimisation we could load using movsx instead of
           potentially load with mov and signextend with movsx *)
        "mov"
    | Noop -> ""
    | RepMov _ -> assert false

let asm_of_loc (loc : Registers.loc) size =
    let pick ~size ~r8 ~r4 ~r2 ~r1 =
        match size with
        | 8 -> r8
        | 4 -> r4
        | 2 -> r2
        | 1 -> r1
        | n -> failwithf "unsupported register size: %d" n ()
    in
    match loc with
    | Stack offs ->
        (* HACK: for now we consider everything as 64 bit value but this needs to be dealt with better *)
        let offs = (offs * 8) + 256 in
        let size_str = pick ~size ~r8:"qword" ~r4:"dword" ~r2:"word" ~r1:"byte" in
        Printf.sprintf "%s [rbp %s 0x%x]" size_str (if offs > 0 then "+" else "-") (abs offs)
    | Reg reg -> (
        match reg with
        | RAX -> pick ~size ~r8:"rax" ~r4:"eax" ~r2:"ax" ~r1:"al"
        | RBX -> pick ~size ~r8:"rbx" ~r4:"ebx" ~r2:"bx" ~r1:"bl"
        | RCX -> pick ~size ~r8:"rcx" ~r4:"ecx" ~r2:"cx" ~r1:"cl"
        | RDX -> pick ~size ~r8:"rdx" ~r4:"edx" ~r2:"dx" ~r1:"dl"
        | RSI -> pick ~size ~r8:"rsi" ~r4:"esi" ~r2:"si" ~r1:"sil"
        | RDI -> pick ~size ~r8:"rdi" ~r4:"edi" ~r2:"di" ~r1:"dil"
        | R8 -> pick ~size ~r8:"r8" ~r4:"r8d" ~r2:"r8w" ~r1:"r8b"
        | R9 -> pick ~size ~r8:"r9" ~r4:"r9d" ~r2:"r9w" ~r1:"r9b"
        | R10 -> pick ~size ~r8:"r10" ~r4:"r10d" ~r2:"r10w" ~r1:"r10b"
        | R11 -> pick ~size ~r8:"r11" ~r4:"r11d" ~r2:"r11w" ~r1:"r11b"
        | R12 -> pick ~size ~r8:"r12" ~r4:"r12d" ~r2:"r12w" ~r1:"r12b"
        | R13 -> pick ~size ~r8:"r13" ~r4:"r13d" ~r2:"r13w" ~r1:"r13b"
        | R14 -> pick ~size ~r8:"r14" ~r4:"r14d" ~r2:"r14w" ~r1:"r14b"
        | R15 -> pick ~size ~r8:"r15" ~r4:"r15d" ~r2:"r15w" ~r1:"r15b"
        | RSP -> pick ~size ~r8:"rsp" ~r4:"esp" ~r2:"sp" ~r1:"spl"
        | RBP -> pick ~size ~r8:"rbp" ~r4:"ebp" ~r2:"bp" ~r1:"bpl"
        | Flags -> "")

let get_label (Machine_node.AnyNode n) =
    match n.kind with
    | Ideal Loop -> Printf.sprintf "Loop_%d" n.id
    | Ideal Stop -> "Exit"
    | _ when Machine_node.is_blockhead n -> Printf.sprintf "L_%d" n.id
    | _ -> failwithf "get_label invalid node: %s" (Machine_node.show n) ()

let rec is_jmp_target g (Machine_node.AnyNode n) =
    match n.kind with
    | Ideal Region -> true
    | _ ->
        List.exists (Graph.get_dependencies_list g n) ~f:(fun n' ->
            match n' with
            | Some (AnyNode { kind = Jmp _; _ })
            | Some (AnyNode { kind = JmpAlways; _ }) ->
                true
            | Some (AnyNode ({ kind = Ideal (CProj _); _ } as n')) ->
                (* TODO: these useless CProj nodes should just get removed, that would be way cleaner *)
                is_jmp_target g (AnyNode n')
                && List.length (Graph.get_dependants g n') = 1
                && Graph.get_dependants g n' |> List.hd_exn |> fun (Machine_node.AnyNode n) ->
                   Machine_node.is_blockhead n
            | _ -> false)

let get_first_blockhead g (Machine_node.AnyNode n) =
    let n' =
        ref
          (Graph.get_dependants g n
          |> List.find_exn ~f:(fun (AnyNode n) -> Machine_node.is_control_node n))
    in
    let is_blockhead (Machine_node.AnyNode n) = Machine_node.is_blockhead n in
    while not (is_blockhead !n') do
      let (AnyNode n'_uwrapped) = !n' in
      n' :=
        Graph.get_dependants g n'_uwrapped
        |> List.find_exn ~f:(fun (AnyNode n) -> Machine_node.is_control_node n)
    done;
    !n'

let asm_of_node g reg_assoc linker n prev_node next_node =
    let (Machine_node.AnyNode n_unwrapped) = n in
    let (AnyNode ir_node) = n_unwrapped.ir_node in
    let node_asm =
        match n_unwrapped.kind with
        | Int i ->
            let reg = Hashtbl.find_exn reg_assoc n in
            let op_str = asm_of_op n_unwrapped.kind in
            (* NOTE: Use at least 32bit reg as the smaller ones don't zero extend the upper bits. 

               Also, based on my research: smaller width movs such as mov al, N
               are a partial register write so it can introduce false
               depedencies in the CPU execution since the register's higher
               bits depend on the earlier value. 32bit or 64bit mov sets the
               entire register value so the new value is independent of
               previous values so it can actually be a bit faster. Only
               downside is slightly larger instruction size.
               https://stackoverflow.com/questions/53562739/if-i-have-an-8-bit-value-is-there-any-advantage-to-using-an-8-bit-register-inst
               *)
            let reg_size = max 4 (Types.get_size ir_node.typ) in
            Printf.sprintf "\t%s %s, %s" op_str (asm_of_loc reg reg_size) (Z.to_string i)
        | Ptr ->
            let reg = Hashtbl.find_exn reg_assoc n in
            let op_str = asm_of_op n_unwrapped.kind in
            let ptr_addr =
                match ir_node.typ with
                | Ptr p when Types.is_const_array p -> Printf.sprintf "C_%d" ir_node.id
                | Struct _ when Types.is_const_array ir_node.typ -> Printf.sprintf "C_%d" ir_node.id
                | FunPtr (Value _) ->
                    Linker.get_name linker (Types.get_fun_idx ir_node.typ |> Option.value_exn)
                | _ -> failwithf "idk %s" (Types.show ir_node.typ) ()
            in
            let size =
                match ir_node.typ with
                | Struct _ -> Types.get_size (Ptr ir_node.typ)
                | _ -> Types.get_size ir_node.typ
            in
            Printf.sprintf "\t%s %s, [%s]" op_str (asm_of_loc reg size) ptr_addr
        | AddrOf ->
            let { Machine_node.input } = Graph.get_dependencies_exn g n_unwrapped in
            let in_reg = Hashtbl.find_exn reg_assoc input in
            let out_reg = Hashtbl.find_exn reg_assoc n in
            let out_size =
                match ir_node.typ with
                | Struct _ -> 8
                | _ -> Types.get_size ir_node.typ
            in
            assert (out_size = 4 || out_size = 8);
            Printf.sprintf "\t%s %s, [%s]" (asm_of_op n_unwrapped.kind)
              (asm_of_loc out_reg out_size) (asm_of_loc in_reg out_size)
        | Deref ->
            let { Machine_node.mem; ptr } : Machine_node.deref =
                Graph.get_dependencies_exn g n_unwrapped
            in
            let in_reg = Hashtbl.find_exn reg_assoc ptr in
            let out_reg = Hashtbl.find_exn reg_assoc n in
            let out_size = Types.get_size ir_node.typ in
            Printf.sprintf "\t%s %s, [%s]" (asm_of_op n_unwrapped.kind)
              (asm_of_loc out_reg out_size) (asm_of_loc in_reg out_size)
        | ZeroExtend ->
            let reg = Hashtbl.find_exn reg_assoc n in
            let { Machine_node.input } = Graph.get_dependencies_exn g n_unwrapped in
            let input_reg = Hashtbl.find_exn reg_assoc input in
            let (AnyNode input_ir_node) =
                let (AnyNode tmp) = input in
                tmp.ir_node
            in
            let input_size = Types.get_size input_ir_node.typ in
            let output_size = Types.get_size ir_node.typ in
            if input_size = 4 && output_size = 8 then
              Printf.sprintf "\tmov %s, %s" (asm_of_loc reg 4) (asm_of_loc input_reg input_size)
            else
              Printf.sprintf "\tmovzx %s, %s" (asm_of_loc reg output_size)
                (asm_of_loc input_reg input_size)
        | SignExtend ->
            let reg = Hashtbl.find_exn reg_assoc n in
            let { Machine_node.input } = Graph.get_dependencies_exn g n_unwrapped in
            let input_reg = Hashtbl.find_exn reg_assoc input in
            let (AnyNode input_ir_node) =
                let (AnyNode tmp) = input in
                tmp.ir_node
            in
            let input_size = Types.get_size input_ir_node.typ in
            let output_size = Types.get_size ir_node.typ in
            if input_size = 4 && output_size = 8 then
              Printf.sprintf "\tmovsxd %s, %s" (asm_of_loc reg output_size)
                (asm_of_loc input_reg input_size)
            else
              Printf.sprintf "\tmovsx %s, %s" (asm_of_loc reg output_size)
                (asm_of_loc input_reg input_size)
        | AddImm i
        | SubImm i
        | MulImm i
        | CmpImm i
        | LshImm i
        | RshImm i
        | AndImm i
        | OrImm i ->
            assert (Z.fits_int32 i);
            let as_unary : type a. a Machine_node.t -> Machine_node.unary Machine_node.t =
               fun n ->
                match n.kind with
                | AddImm _ -> n
                | SubImm _ -> n
                | MulImm _ -> n
                | CmpImm _ -> n
                | LshImm _ -> n
                | RshImm _ -> n
                | AndImm _ -> n
                | OrImm _ -> n
                | _ -> assert false
            in
            let n_unwrapped = as_unary n_unwrapped in
            let { Machine_node.input } = Graph.get_dependencies_exn g n_unwrapped in
            let (AnyNode input_ir_node) =
                let (AnyNode tmp) = input in
                tmp.ir_node
            in
            let reg = Hashtbl.find_exn reg_assoc input in
            let op_str = asm_of_op n_unwrapped.kind in
            let reg_str = asm_of_loc reg (Types.get_size input_ir_node.typ) in
            Printf.sprintf "\t%s %s, %s" op_str reg_str (Z.to_string i)
        | Div ->
            let { Machine_node.lhs; rhs } = Graph.get_dependencies_exn g n_unwrapped in
            let divisor = rhs in
            let (AnyNode divisor_ir_node) =
                let (AnyNode tmp) = divisor in
                tmp.ir_node
            in
            let reg_divisor = Hashtbl.find_exn reg_assoc divisor in
            let reg_str = asm_of_loc reg_divisor (Types.get_size divisor_ir_node.typ) in
            let op_str = asm_of_op n_unwrapped.kind in
            (* have to sign extend RAX into RDX for signed division *)
            (* TODO: use xor rdx, rdx  for unsigned division once we have that distinction *)
            Printf.sprintf "\tcqo\n\t%s %s \t\t; rax = rax / %s" op_str reg_str reg_str
        | Ideal Stop ->
            let epilogue = "\t;Exit program\n\tmov rax, 60\n\txor rdi, rdi\n\tsyscall" in
            epilogue
        | Ideal _ -> ""
        | Jmp cond ->
            let (AnyNode true_branch) =
                Graph.get_dependants g n_unwrapped
                |> List.find_exn ~f:(fun (AnyNode n) ->
                    match n.kind with
                    | Ideal (CProj 0) -> true
                    | _ -> false)
            in
            let (AnyNode false_branch) =
                Graph.get_dependants g n_unwrapped
                |> List.find_exn ~f:(fun (AnyNode n) ->
                    match n.kind with
                    | Ideal (CProj 1) -> true
                    | _ -> false)
            in
            let (Machine_node.AnyNode next_node) = Option.value_exn next_node in
            let (AnyNode target_branch) =
                if Machine_node.equal next_node true_branch then
                  Machine_node.AnyNode false_branch
                else
                  Machine_node.AnyNode true_branch
            in
            let op_str =
                if Machine_node.equal next_node true_branch then
                  asm_of_op (Jmp (Machine_node.invert_cond cond))
                else
                  asm_of_op n_unwrapped.kind
            in
            let label_str =
                (* FIXME: this is not correct when we have cproj ->
                    functioncall(ptr/const) because the ptr/const might not be
                    part of the cproj bb. Probably need to check List.filter
                    ~f:is_blockhead |> is_empty or something like that *)
                if List.length (Graph.get_dependants g target_branch) = 1 && false then
                  let next_target_bb = get_first_blockhead g (AnyNode target_branch) in
                  get_label next_target_bb
                else
                  get_label (AnyNode target_branch)
            in
            Printf.sprintf "\t%s %s\n" op_str label_str
        | JmpAlways ->
            let op_str = asm_of_op n_unwrapped.kind in
            let target = Graph.get_dependants g n_unwrapped |> List.hd_exn in
            let label_str = get_label target in
            Printf.sprintf "\t%s %s\n" op_str label_str
        | Lsh
        | Rsh ->
            let as_binop : type a. a Machine_node.t -> Machine_node.binop Machine_node.t =
               fun n ->
                match n.kind with
                | Lsh -> n
                | Rsh -> n
                | _ -> assert false
            in
            let n_unwrapped = as_binop n_unwrapped in
            let { Machine_node.lhs; rhs } = Graph.get_dependencies_exn g n_unwrapped in
            let (AnyNode lhs_ir_node) =
                let (AnyNode tmp) = lhs in
                tmp.ir_node
            in
            let reg = Hashtbl.find_exn reg_assoc lhs in
            let op_str = asm_of_op n_unwrapped.kind in
            assert (0 = Registers.compare_loc (Hashtbl.find_exn reg_assoc rhs) (Reg Registers.RCX));
            Printf.sprintf "\t%s %s, cl" op_str (asm_of_loc reg (Types.get_size lhs_ir_node.typ))
        | Add
        | Sub
        | Mul
        | And
        | Or
        | Cmp ->
            let as_binop : type a. a Machine_node.t -> Machine_node.binop Machine_node.t =
               fun n ->
                match n.kind with
                | Add -> n
                | Sub -> n
                | Mul -> n
                | And -> n
                | Or -> n
                | Cmp -> n
                | _ -> assert false
            in
            let n_unwrapped = as_binop n_unwrapped in
            let { Machine_node.lhs; rhs } = Graph.get_dependencies_exn g n_unwrapped in
            let (AnyNode lhs_ir_node) =
                let (AnyNode tmp) = lhs in
                tmp.ir_node
            in
            let (AnyNode rhs_ir_node) =
                let (AnyNode tmp) = rhs in
                tmp.ir_node
            in
            let lhs_reg = Hashtbl.find_exn reg_assoc lhs in
            let rhs_reg = Hashtbl.find_exn reg_assoc rhs in
            let op_str = asm_of_op n_unwrapped.kind in
            Printf.sprintf "\t%s %s, %s" op_str
              (asm_of_loc lhs_reg (Types.get_size lhs_ir_node.typ))
              (asm_of_loc rhs_reg (Types.get_size lhs_ir_node.typ))
        | Mov ->
            let { Machine_node.input } = Graph.get_dependencies_exn g n_unwrapped in
            let (AnyNode input_unwrapped) = input in
            (* if not two address node, add the node itself to the start of the list to get the output reg too *)
            let in_reg, in_size =
                match input_unwrapped.kind with
                | CalleeSave _ ->
                    (* CalleSaves always need to save the full 64bit register *)
                    (Hashtbl.find_exn reg_assoc input, 8)
                | Mov -> (
                    (* Movs that spill CalleeSaved nodes have their ir_node
                           as the Funciton, these need to be treated specifically as 64bit
                           to save the full 64bit reg *)
                    let (AnyNode input_ir_node) = input_unwrapped.ir_node in
                    match input_ir_node.kind with
                    | Ctrl (Function _) -> (Hashtbl.find_exn reg_assoc input, 8)
                    | _ ->
                        let size =
                            match input_ir_node.typ with
                            | Struct _ -> 8
                            | _ -> Types.get_size input_ir_node.typ
                        in

                        (Hashtbl.find_exn reg_assoc input, size))
                | _ ->
                    let (AnyNode input_ir_node) = input_unwrapped.ir_node in
                    let size =
                        match input_ir_node.typ with
                        | Struct _ -> 8
                        | _ -> Types.get_size input_ir_node.typ
                    in
                    (Hashtbl.find_exn reg_assoc input, size)
            in
            let reg = Hashtbl.find_exn reg_assoc n in
            let size = Types.get_size ir_node.typ in
            let op_str = asm_of_op n_unwrapped.kind in
            Printf.sprintf "\t%s %s, %s" op_str (asm_of_loc reg size) (asm_of_loc in_reg in_size)
        | Set _ ->
            let op_str = asm_of_op n_unwrapped.kind in
            let reg = Hashtbl.find_exn reg_assoc n in
            Printf.sprintf "\t%s %s" op_str (asm_of_loc reg (Types.get_size ir_node.typ))
        | DProj _ -> ""
        | FunctionProlog i ->
            let target = Linker.get_name linker i in
            Printf.sprintf "%s:\n\tpush rbp\n\tmov rbp, rsp" target
        | Return -> Printf.sprintf "\tleave\n\t%s" (asm_of_op n_unwrapped.kind)
        | Param _ -> ""
        | FunctionCall (Some i) ->
            let op = asm_of_op n_unwrapped.kind in
            let target = Linker.get_name linker i in
            Printf.sprintf "\t%s %s" op target
        | FunctionCall None ->
            let op = asm_of_op n_unwrapped.kind in
            let { Machine_node.fun_ptr; mem; args } = Graph.get_dependencies_exn g n_unwrapped in
            let target = Option.value_exn fun_ptr in
            let (AnyNode target_ir_node) =
                let (AnyNode tmp) = target in
                tmp.ir_node
            in
            let target_reg = Hashtbl.find_exn reg_assoc target in
            Printf.sprintf "\t%s %s" op (asm_of_loc target_reg (Types.get_size target_ir_node.typ))
        | FunctionCallEnd -> ""
        | CalleeSave _ -> ""
        | New ->
            let ptr =
                Graph.get_dependants g n_unwrapped
                |> List.find_exn ~f:(fun (AnyNode n) ->
                    match n.kind with
                    | DProj 1 -> true
                    | _ -> false)
            in
            let { Machine_node.mem = _; size } = Graph.get_dependencies_exn g n_unwrapped in
            let (AnyNode size_ir_node) =
                let (AnyNode tmp) = size in
                tmp.ir_node
            in
            let ptr_reg = Hashtbl.find_exn reg_assoc ptr in
            let size_reg = Hashtbl.find_exn reg_assoc size in
            (* HACK: this is only until i get heap memory alloc. *)
            Printf.sprintf "\tsub rsp, %s   ; alloc\n\tmov %s, rsp"
              (asm_of_loc size_reg (Types.get_size size_ir_node.typ))
              (asm_of_loc ptr_reg 8)
        | Store ->
            let { Machine_node.mem; ptr; value } = Graph.get_dependencies_exn g n_unwrapped in
            let reg = Hashtbl.find_exn reg_assoc value in
            let ptr_reg = Hashtbl.find_exn reg_assoc ptr in
            let (AnyNode value_ir_node) =
                let (AnyNode tmp) = value in
                tmp.ir_node
            in
            let op_str = asm_of_op n_unwrapped.kind in
            Printf.sprintf "\t%s [%s], %s" op_str (asm_of_loc ptr_reg 8)
              (asm_of_loc reg (Types.get_size value_ir_node.typ))
        | Load ->
            let { Machine_node.mem; ptr } = Graph.get_dependencies_exn g n_unwrapped in
            let reg = Hashtbl.find_exn reg_assoc n in
            let ptr_reg = Hashtbl.find_exn reg_assoc ptr in
            let output_size = Types.get_size ir_node.typ in
            let s =
                Printf.sprintf "\t%s %s, [%s]" (asm_of_op n_unwrapped.kind)
                  (asm_of_loc reg output_size) (asm_of_loc ptr_reg 8)
            in
            s
        | RepMov num ->
            let { Machine_node.mem; src; dst } = Graph.get_dependencies_exn g n_unwrapped in
            let src_reg = Hashtbl.find_exn reg_assoc src in
            let dst_reg = Hashtbl.find_exn reg_assoc dst in
            assert (Poly.equal src_reg (Reg RSI));
            assert (Poly.equal dst_reg (Reg RDI));
            let kind = 8 in
            let op_str =
                match kind with
                | 8 -> "movsq"
                | _ -> failwithf "todo %s" __LOC__ ()
            in
            assert (num % kind = 0);
            let size = num / kind in
            Printf.sprintf "\tmov rcx, %d\n\trep %s" size op_str
        | Noop -> ""
    in
    let jmp_target =
        match prev_node with
        | None -> None
        | Some (Machine_node.AnyNode prev_node) ->
            let (AnyNode cfg_prev) =
                if Machine_node.is_control_node prev_node then
                  Machine_node.AnyNode prev_node
                else
                  Graph.get_ctrl_exn g prev_node
            in
            let (AnyNode precedent) = Graph.get_ctrl_exn g n_unwrapped in
            if Machine_node.is_blockhead n_unwrapped && not (Machine_node.equal precedent cfg_prev)
            then
              let target =
                  Graph.get_dependants g cfg_prev
                  |> List.find_exn ~f:(fun (AnyNode n) -> Machine_node.is_blockhead n)
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
        | [ Machine_node.AnyNode n ] -> (
            let node_asm = asm_of_node g reg_assoc linker (AnyNode n) prev None in
            (* Check if the last emitted control node has a CFG successor
               that won't be fallen into (since there's nothing after us). *)
            let (AnyNode last_ctrl) =
                if Machine_node.is_control_node n then
                  Machine_node.AnyNode n
                else
                  Graph.get_ctrl_exn g n
            in
            let trailing_jmp =
                Graph.get_dependants g last_ctrl
                |> List.find ~f:(fun (AnyNode n) -> Machine_node.is_blockhead n)
                |> Option.map ~f:(fun target ->
                    Printf.sprintf "\t%s %s" (asm_of_op JmpAlways) (get_label target))
            in
            match trailing_jmp with
            | None -> [ node_asm ]
            | Some jmp -> [ node_asm ^ "\n" ^ jmp ])
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
    mov     rdx, rdi
    mov     rax, 1
    mov     rdi, 1
    syscall
    ret|}
    in
    print ^ print_int

let gather_const_arrays functions =
    (* TODO: currently we put the string literal in rodata
    and also create the fat pointer struct in rodata. Another option is to
    create the struct as an immediate value instead of keeping it in rodata.
    Having the struct simply as an immediate would be better in some cases.
    Keeping the struct in rodata is better if same string used in many places,
    if we need to take address of the struct, ... but worse because more
    indirection to get to contents. We should choose between the two options on
    a case by case basis *)
    let arr_map = Hashtbl.create (module Int) in
    List.iter functions ~f:(fun (g, _, _) ->
        Graph.iter g ~f:(fun (AnyNode n) ->
            match n.kind with
            | Ptr -> (
                let (AnyNode ir_node) = n.ir_node in
                match Types.get_string ir_node.typ with
                | None -> ()
                | Some s ->
                    let with_len =
                        Printf.sprintf "\n\tdq %d\n\tdq STR_%d\nSTR_%d:\n\tdb %s" (String.length s)
                          n.id n.id
                          (String.to_list s
                          |> List.map ~f:(fun c -> c |> Char.to_int |> Int.to_string)
                          |> String.concat ~sep:", ")
                    in
                    Hashtbl.add arr_map ~key:ir_node.id ~data:with_len |> ignore)
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
