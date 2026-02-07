open Dawn

let test str =
    (* HACK: the output seems to be dependant on node ids. So this just resets it to get more stable output until i fix *)
    Machine_node.id_counter := 0;
    Node.reset_id ();

    match Parser.parse_str str with
    | Ok ast ->
        let linker = Linker.create () in
        let son = Son.of_ast ast linker in
        let schedules = Scheduler.schedule son in

        (* only do code gen on non external functions *)
        let functions =
            Core.List.filter schedules ~f:(fun (g, _) ->
                Graph.find g ~f:(fun n ->
                    match n.kind with
                    | Ideal (External _) -> true
                    | _ -> false)
                |> Option.is_none)
            |> Core.List.map ~f:(fun (g, program) ->
                let program = List.concat program in
                let program, reg_assoc = Reg_allocator.allocate g program in
                Ir_printer.to_string_machine_linear_regs g program reg_assoc |> Printf.printf "%s\n";
                (g, reg_assoc, program))
        in
        Asm_emit.emit_program functions linker |> print_endline
    | Error msg -> Printf.eprintf "%s\n" msg

let%expect_test "" =
    let test_str =
        {|
    let k:int = 69;
    let i:int = k;
    let j:int = i / 69;
    let x:int = i / 420;
    i = i + 1;
    if(i == j) {j = j+1;}
    if(j==0){j=j+4;}
    if(x==j){x=j+1;}
    |}
    in
    test test_str;
    [%expect
        {|
      === Machine Graph (Linearized with registers) ===

      Block #1 ((Ideal Start)): -> [#12]

      Block #12 ((Ideal (CProj 0))): -> [T: #10,F: #19]
        #RCX   (%16 ): (Int 1)                                                       (Ideal IR: #11)
        #RCX   (%15 ): (AddImm 69)     [ #RCX (%16) ]                                (Ideal IR: #12)
        #RAX   (%18 ): (Int 69)                                                      (Ideal IR: #6)
        #RAX   (%43 ): (Int 69)                                                      (Ideal IR: #6)
        #RBX   (%44 ): (Int 69)                                                      (Ideal IR: #6)
        #RAX   (%17 ): Div             [ #RAX (%43), #RBX (%44) ]                    (Ideal IR: #8)
        #RBX   (%34 ): Mov             [ #RAX (%17) ]                                (Ideal IR: #8)
        #RAX   (%33 ): Mov             [ #RBX (%34) ]                                (Ideal IR: #8)
        #Flags (%14 ): Cmp             [ #RCX (%15), #RAX (%33) ]                    (Ideal IR: #13)
               (%11 ): (Jmp Eq)        [ #Flags (%14) ]                              (Ideal IR: #14)

      Block #10 ((Ideal (CProj 0))): -> [#9]
        #RBX   (%23 ): (AddImm 1)      [ #RBX (%34) ]                                (Ideal IR: #19)
        #RAX   (%38 ): Mov             [ #RBX (%23) ]                                (Ideal IR: #19)
        #RBX   (%37 ): Mov             [ #RAX (%38) ]                                (Ideal IR: #19)

      Block #19 ((Ideal (CProj 1))): -> [#9]

      Block #9 ((Ideal Region)): -> [T: #7,F: #24]
        #RBX   (%22 ): (Ideal Phi)     [ #RBX (%37), #RBX (%34) ]                    (Ideal IR: #21)
        #RAX   (%35 ): Mov             [ #RBX (%22) ]                                (Ideal IR: #21)
        #Flags (%21 ): (CmpImm 0)      [ #RAX (%35) ]                                (Ideal IR: #23)
               (%8  ): (Jmp Eq)        [ #Flags (%21) ]                              (Ideal IR: #24)

      Block #7 ((Ideal (CProj 0))): -> [#6]
        #RBX   (%30 ): (AddImm 4)      [ #RBX (%22) ]                                (Ideal IR: #29)
        #RAX   (%42 ): Mov             [ #RBX (%30) ]                                (Ideal IR: #29)
        #RBX   (%41 ): Mov             [ #RAX (%42) ]                                (Ideal IR: #29)

      Block #24 ((Ideal (CProj 1))): -> [#6]

      Block #6 ((Ideal Region)): -> [T: #4,F: #31]
        #RBX   (%29 ): (Ideal Phi)     [ #RBX (%41), #RBX (%22) ]                    (Ideal IR: #31)
        #RCX   (%28 ): (Int 420)                                                     (Ideal IR: #9)
        #RAX   (%45 ): (Int 69)                                                      (Ideal IR: #6)
        #RAX   (%27 ): Div             [ #RAX (%45), #RCX (%28) ]                    (Ideal IR: #10)
        #Flags (%26 ): Cmp             [ #RAX (%27), #RBX (%29) ]                    (Ideal IR: #32)
               (%5  ): (Jmp Eq)        [ #Flags (%26) ]                              (Ideal IR: #33)

      Block #4 ((Ideal (CProj 0))): -> [#3]

      Block #31 ((Ideal (CProj 1))): -> [#3]

      Block #3 ((Ideal Region)): -> [#2]

      Block #2 ((Ideal Stop)): -> []


      mov rcx, 1
      add rcx, 69
      mov rax, 69
      mov rax, 69
      mov rbx, 69
      cqo
      idiv rbx 		// rax = rax / rbx
      mov rbx, rax
      mov rax, rbx
      cmp rcx, rax
      jne L_9

      L_10:
      add rbx, 1
      mov rax, rbx
      mov rbx, rax
      L_19:
      L_9:
      mov rax, rbx
      cmp rax, 0
      jne L_6

      L_7:
      add rbx, 4
      mov rax, rbx
      mov rbx, rax
      L_24:
      L_6:
      mov rcx, 420
      mov rax, 69
      cqo
      idiv rcx 		// rax = rax / rcx
      cmp rax, rbx
      jne L_3

      L_4:
      L_31:
      L_3:
      //Exit program
      mov rax, 60
      xor rdi, rdi
      syscall
      |}]

let%expect_test "fibonacci" =
    let test_str =
        {|
    let x:int = 0;
    let y:int = 1;
    while(0 <= x) {
        let tmp:int = x + y;
        x = y;
        y = tmp;
    }
    |}
    in
    test test_str;
    [%expect
        {|
      === Machine Graph (Linearized with registers) ===

      Block #1 ((Ideal Start)): -> [#7]

      Block #7 ((Ideal (CProj 0))): -> [#5]
        #RAX   (%13 ): (Int 1)                                                       (Ideal IR: #7)
        #RAX   (%14 ): (Int 0)                                                       (Ideal IR: #6)
        #RAX   (%18 ): (Int 0)                                                       (Ideal IR: #6)
        #RBX   (%21 ): (Int 1)                                                       (Ideal IR: #7)

      Block #5 ((Ideal Loop)): -> [T: #6,F: #3]
        #RBX   (%11 ): (Ideal Phi)     [ #RBX (%22), #RBX (%21) ]                    (Ideal IR: #17)
        #RAX   (%10 ): (Ideal Phi)     [ #RAX (%19), #RAX (%18) ]                    (Ideal IR: #11)
        #Flags (%9  ): (CmpImm 0)      [ #RAX (%10) ]                                (Ideal IR: #12)
               (%4  ): (Jmp GEq)       [ #Flags (%9) ]                               (Ideal IR: #14)

      Block #6 ((Ideal (CProj 0))): -> [#5]
        #RCX   (%23 ): Mov             [ #RAX (%10) ]                                (Ideal IR: #11)
        #RCX   (%12 ): Add             [ #RCX (%23), #RBX (%11) ]                    (Ideal IR: #18)
        #RAX   (%19 ): Mov             [ #RBX (%11) ]                                (Ideal IR: #17)
        #RBX   (%22 ): Mov             [ #RCX (%12) ]                                (Ideal IR: #18)

      Block #3 ((Ideal (CProj 1))): -> [#2]

      Block #2 ((Ideal Stop)): -> []


      mov rax, 1
      mov rax, 0
      mov rax, 0
      mov rbx, 1
      Loop_5:
      cmp rax, 0
      jl Exit

      L_6:
      mov rcx, rax
      add rcx, rbx
      mov rax, rbx
      mov rbx, rcx
      jmp Loop_5

      L_3:
      Exit:
      //Exit program
      mov rax, 60
      xor rdi, rdi
      syscall
      |}]

let%expect_test "nested loop" =
    let test_str =
        {|
    let i:int = 0;
    let sum:int = 0;
    let c: int = 5;
    while(i == c - 1) {
        i = i + 1;
        let j:int = 0;
        while(i+j == 11){
            sum = j;
            j = j + 2;
        }
    }
    if(sum == 0) {}
    |}
    in
    test test_str;
    [%expect
        {|
      === Machine Graph (Linearized with registers) ===

      Block #1 ((Ideal Start)): -> [#22]

      Block #22 ((Ideal (CProj 0))): -> [#8]
        #RAX   (%19 ): (Int 0)                                                       (Ideal IR: #6)
        #RSI   (%26 ): (Int 5)                                                       (Ideal IR: #8)
        #RSI   (%25 ): (SubImm 1)      [ #RSI (%26) ]                                (Ideal IR: #14)
        #RAX   (%37 ): (Int 0)                                                       (Ideal IR: #6)
        #RBX   (%38 ): (Int 0)                                                       (Ideal IR: #6)
        #RDI   (%46 ): (Int 0)                                                       (Ideal IR: #6)

      Block #8 ((Ideal Loop)): -> [T: #13,F: #6]
        #RDI   (%29 ): (Ideal Phi)     [ #RDI (%30), #RDI (%46) ]                    (Ideal IR: #34)
        #RAX   (%18 ): (Ideal Phi)     [ #RAX (%34), #RAX (%37) ]                    (Ideal IR: #11)
        #RBX   (%45 ): Mov             [ #RDI (%29) ]                                (Ideal IR: #34)
        #Flags (%24 ): Cmp             [ #RAX (%18), #RSI (%25) ]                    (Ideal IR: #15)
               (%7  ): (Jmp Eq)        [ #Flags (%24) ]                              (Ideal IR: #17)

      Block #13 ((Ideal (CProj 0))): -> [#11]
        #RCX   (%35 ): Mov             [ #RAX (%18) ]                                (Ideal IR: #11)
        #RCX   (%17 ): (AddImm 1)      [ #RCX (%35) ]                                (Ideal IR: #21)
        #RAX   (%39 ): (Int 0)                                                       (Ideal IR: #6)
        #RDX   (%43 ): (Int 0)                                                       (Ideal IR: #6)
        #RDI   (%44 ): Mov             [ #RBX (%45) ]                                (Ideal IR: #34)

      Block #11 ((Ideal Loop)): -> [T: #12,F: #9]
        #RDX   (%20 ): (Ideal Phi)     [ #RDX (%21), #RDX (%43) ]                    (Ideal IR: #26)
        #RDI   (%30 ): (Ideal Phi)     [ #RDI (%41), #RDI (%44) ]                    (Ideal IR: #35)
        #RAX   (%42 ): Mov             [ #RDX (%20) ]                                (Ideal IR: #26)
        #RBX   (%33 ): Mov             [ #RCX (%17) ]                                (Ideal IR: #21)
        #RBX   (%16 ): Add             [ #RBX (%33), #RAX (%42) ]                    (Ideal IR: #27)
        #Flags (%15 ): (CmpImm 11)     [ #RBX (%16) ]                                (Ideal IR: #29)
               (%10 ): (Jmp Eq)        [ #Flags (%15) ]                              (Ideal IR: #31)

      Block #12 ((Ideal (CProj 0))): -> [#11]
        #RDX   (%40 ): Mov             [ #RAX (%42) ]                                (Ideal IR: #26)
        #RDX   (%21 ): (AddImm 2)      [ #RDX (%40) ]                                (Ideal IR: #37)
        #RDI   (%41 ): Mov             [ #RAX (%42) ]                                (Ideal IR: #26)

      Block #9 ((Ideal (CProj 1))): -> [#8]
        #RAX   (%34 ): Mov             [ #RCX (%17) ]                                (Ideal IR: #21)

      Block #6 ((Ideal (CProj 1))): -> [T: #4,F: #31]
        #Flags (%28 ): (CmpImm 0)      [ #RBX (%45) ]                                (Ideal IR: #39)
               (%5  ): (Jmp Eq)        [ #Flags (%28) ]                              (Ideal IR: #40)

      Block #4 ((Ideal (CProj 0))): -> [#3]

      Block #31 ((Ideal (CProj 1))): -> [#3]

      Block #3 ((Ideal Region)): -> [#2]

      Block #2 ((Ideal Stop)): -> []


      mov rax, 0
      mov rsi, 5
      sub rsi, 1
      mov rax, 0
      mov rbx, 0
      mov rdi, 0
      Loop_8:
      mov rbx, rdi
      cmp rax, rsi
      jne L_6

      L_13:
      mov rcx, rax
      add rcx, 1
      mov rax, 0
      mov rdx, 0
      mov rdi, rbx
      Loop_11:
      mov rax, rdx
      mov rbx, rcx
      add rbx, rax
      cmp rbx, 11
      jne L_9

      L_12:
      mov rdx, rax
      add rdx, 2
      mov rdi, rax
      jmp Loop_11

      L_9:
      mov rax, rcx
      jmp Loop_8

      L_6:
      cmp rbx, 0
      jne L_3

      L_4:
      L_31:
      L_3:
      //Exit program
      mov rax, 60
      xor rdi, rdi
      syscall
      |}]

let%expect_test "binops" =
    let test_str =
        {|
    let i:int = 0;
    i = i | (1 << 2);
    if(((i>>1) & 1) == 1) {}
    |}
    in
    test test_str;
    [%expect
        {|
      === Machine Graph (Linearized with registers) ===

      Block #1 ((Ideal Start)): -> [#6]

      Block #6 ((Ideal (CProj 0))): -> [T: #4,F: #14]
        #RAX   (%13 ): (Int 1)                                                       (Ideal IR: #7)
        #RAX   (%12 ): (LshImm 2)      [ #RAX (%13) ]                                (Ideal IR: #9)
        #RAX   (%11 ): (OrImm 0)       [ #RAX (%12) ]                                (Ideal IR: #10)
        #RAX   (%10 ): (RshImm 1)      [ #RAX (%11) ]                                (Ideal IR: #12)
        #RAX   (%9  ): (AndImm 1)      [ #RAX (%10) ]                                (Ideal IR: #14)
        #Flags (%8  ): (CmpImm 1)      [ #RAX (%9) ]                                 (Ideal IR: #16)
               (%5  ): (Jmp Eq)        [ #Flags (%8) ]                               (Ideal IR: #17)

      Block #4 ((Ideal (CProj 0))): -> [#3]

      Block #14 ((Ideal (CProj 1))): -> [#3]

      Block #3 ((Ideal Region)): -> [#2]

      Block #2 ((Ideal Stop)): -> []


      mov rax, 1
      sal rax, 2
      or rax, 0
      sar rax, 1
      and rax, 1
      cmp rax, 1
      jne L_3

      L_4:
      L_14:
      L_3:
      //Exit program
      mov rax, 60
      xor rdi, rdi
      syscall
      |}]

let%expect_test "function call" =
    let test_str =
        {|
    fun f(a: int, b: int, c: int, d: int, e: int, f: int) -> int {
        69 - a + b + c + d + e + f
    }

    let i: int = f(1,2,3,4,5,6) + 69;
    if(i==0) {}
    |}
    in
    test test_str;
    [%expect
        {|
      === Machine Graph (Linearized with registers) ===

      Block #1 ((Ideal Start)): -> [#9]

      Block #9 ((Ideal (CProj 0))): -> []
        #RDX   (%12 ): (Int 3)                                                       (Ideal IR: #26)
        #RCX   (%13 ): (Int 4)                                                       (Ideal IR: #27)
        #R9    (%15 ): (Int 6)                                                       (Ideal IR: #29)
        #R8    (%14 ): (Int 5)                                                       (Ideal IR: #28)
        #RSI   (%11 ): (Int 2)                                                       (Ideal IR: #25)
        #RDI   (%10 ): (Int 1)                                                       (Ideal IR: #24)
               (%8  ): (FunctionCall 1) [ #RDI (%10), #RSI (%11), #RDX (%12), #RCX (%13), #R8 (%14), #R9 (%15) ] (Ideal IR: #30)

      Block #7 (FunctionCallEnd): -> [#6]
        #RAX   (%36 ):   |-(DProj 1)                                                 (Ideal IR: #33)

      Block #6 ((Ideal (CProj 0))): -> [T: #4,F: #37]
        #RAX   (%35 ): (AddImm 69)     [ #RAX (%36) ]                                (Ideal IR: #35)
        #Flags (%34 ): (CmpImm 0)      [ #RAX (%35) ]                                (Ideal IR: #37)
               (%5  ): (Jmp Eq)        [ #Flags (%34) ]                              (Ideal IR: #38)

      Block #4 ((Ideal (CProj 0))): -> [#3]

      Block #37 ((Ideal (CProj 1))): -> [#3]

      Block #3 ((Ideal Region)): -> [#2]

      Block #2 ((Ideal Stop)): -> []


      === Machine Graph (Linearized with registers) ===

      Block #1 ((Ideal Start)): -> [#18]

      Block #18 ((FunctionProlog 1)): -> [#17]
        #R9    (%32 ): (Param 5)                                                     (Ideal IR: #16)
        #RDX   (%29 ): (Param 2)                                                     (Ideal IR: #13)
        #RDI   (%27 ): (Param 0)                                                     (Ideal IR: #11)
        #R8    (%31 ): (Param 4)                                                     (Ideal IR: #15)
        #RCX   (%30 ): (Param 3)                                                     (Ideal IR: #14)
        #RSI   (%28 ): (Param 1)                                                     (Ideal IR: #12)
        #RAX   (%39 ): (Int 69)                                                      (Ideal IR: #17)
        #RAX   (%25 ): Sub             [ #RAX (%39), #RDI (%27) ]                    (Ideal IR: #18)
        #RAX   (%24 ): Add             [ #RAX (%25), #RSI (%28) ]                    (Ideal IR: #19)
        #RAX   (%23 ): Add             [ #RAX (%24), #RDX (%29) ]                    (Ideal IR: #20)
        #RAX   (%22 ): Add             [ #RAX (%23), #RCX (%30) ]                    (Ideal IR: #21)
        #RAX   (%21 ): Add             [ #RAX (%22), #R8 (%31) ]                     (Ideal IR: #22)
        #RAX   (%20 ): Add             [ #RAX (%21), #R9 (%32) ]                     (Ideal IR: #23)

      Block #17 ((Ideal Region)): -> []
        #RAX   (%19 ): (Ideal Phi)     [ #RAX (%20) ]                                (Ideal IR: #6)
        #RAX   (%16 ): Return          [ #RAX (%19) ]                                (Ideal IR: #8)

      Block #2 ((Ideal Stop)): -> []


      mov rdx, 3
      mov rcx, 4
      mov r9, 6
      mov r8, 5
      mov rsi, 2
      mov rdi, 1
      call f
      add rax, 69
      cmp rax, 0
      jne L_3

      L_4:
      L_37:
      L_3:
      //Exit program
      mov rax, 60
      xor rdi, rdi
      syscall

      f:
      mov rax, 69
      sub rax, rdi
      add rax, rsi
      add rax, rdx
      add rax, rcx
      add rax, r8
      add rax, r9
      L_17:
      ret
      //Exit program
      mov rax, 60
      xor rdi, rdi
      syscall
      |}]
