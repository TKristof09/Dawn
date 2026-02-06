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
        Core.List.iter schedules ~f:(fun (machine_graph, program) ->
            let program = List.concat program in
            let program, reg_assoc = Reg_allocator.allocate machine_graph program in
            let code = Asm_emit.emit_program machine_graph reg_assoc program linker in
            Ir_printer.to_string_machine_linear_regs machine_graph program reg_assoc
            |> Printf.printf "%s\n";
            Printf.printf "\n%s\n\n" code)
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

      Block #1 ((Ideal Start)): -> [T: #10,F: #18]
        #RCX   (%15 ): (Int 1)                                             (Ideal IR: #9)
        #RCX   (%14 ): (AddImm 69)          [ #RCX (%15) ]                 (Ideal IR: #10)
        #RAX   (%17 ): (Int 69)                                            (Ideal IR: #4)
        #RAX   (%42 ): (Int 69)                                            (Ideal IR: #4)
        #RBX   (%43 ): (Int 69)                                            (Ideal IR: #4)
        #RAX   (%16 ): Div                  [ #RAX (%42), #RBX (%43) ]     (Ideal IR: #6)
        #RBX   (%33 ): Mov                  [ #RAX (%16) ]                 (Ideal IR: #6)
        #RAX   (%32 ): Mov                  [ #RBX (%33) ]                 (Ideal IR: #6)
        #Flags (%13 ): Cmp                  [ #RCX (%14), #RAX (%32) ]     (Ideal IR: #11)
               (%11 ): (Jmp Eq)             [ #Flags (%13) ]               (Ideal IR: #12)

      Block #10 ((Ideal (CProj 0))): -> [#9]
        #RBX   (%22 ): (AddImm 1)           [ #RBX (%33) ]                 (Ideal IR: #17)
        #RAX   (%37 ): Mov                  [ #RBX (%22) ]                 (Ideal IR: #17)
        #RBX   (%36 ): Mov                  [ #RAX (%37) ]                 (Ideal IR: #17)

      Block #18 ((Ideal (CProj 1))): -> [#9]

      Block #9 ((Ideal Region)): -> [T: #7,F: #23]
        #RBX   (%21 ): (Ideal Phi)          [ #RBX (%36), #RBX (%33) ]     (Ideal IR: #19)
        #RAX   (%34 ): Mov                  [ #RBX (%21) ]                 (Ideal IR: #19)
        #Flags (%20 ): (CmpImm 0)           [ #RAX (%34) ]                 (Ideal IR: #21)
               (%8  ): (Jmp Eq)             [ #Flags (%20) ]               (Ideal IR: #22)

      Block #7 ((Ideal (CProj 0))): -> [#6]
        #RBX   (%29 ): (AddImm 4)           [ #RBX (%21) ]                 (Ideal IR: #27)
        #RAX   (%41 ): Mov                  [ #RBX (%29) ]                 (Ideal IR: #27)
        #RBX   (%40 ): Mov                  [ #RAX (%41) ]                 (Ideal IR: #27)

      Block #23 ((Ideal (CProj 1))): -> [#6]

      Block #6 ((Ideal Region)): -> [T: #4,F: #30]
        #RBX   (%28 ): (Ideal Phi)          [ #RBX (%40), #RBX (%21) ]     (Ideal IR: #29)
        #RCX   (%27 ): (Int 420)                                           (Ideal IR: #7)
        #RAX   (%44 ): (Int 69)                                            (Ideal IR: #4)
        #RAX   (%26 ): Div                  [ #RAX (%44), #RCX (%27) ]     (Ideal IR: #8)
        #Flags (%25 ): Cmp                  [ #RAX (%26), #RBX (%28) ]     (Ideal IR: #30)
               (%5  ): (Jmp Eq)             [ #Flags (%25) ]               (Ideal IR: #31)

      Block #4 ((Ideal (CProj 0))): -> [#3]

      Block #30 ((Ideal (CProj 1))): -> [#3]

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
      L_18:
      L_9:
      mov rax, rbx
      cmp rax, 0
      jne L_6

      L_7:
      add rbx, 4
      mov rax, rbx
      mov rbx, rax
      L_23:
      L_6:
      mov rcx, 420
      mov rax, 69
      cqo
      idiv rcx 		// rax = rax / rcx
      cmp rax, rbx
      jne L_3

      L_4:
      L_30:
      L_3:
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

      Block #1 ((Ideal Start)): -> [#5]
        #RAX   (%12 ): (Int 1)                                             (Ideal IR: #5)
        #RAX   (%13 ): (Int 0)                                             (Ideal IR: #4)
        #RAX   (%17 ): (Int 0)                                             (Ideal IR: #4)
        #RBX   (%20 ): (Int 1)                                             (Ideal IR: #5)

      Block #5 ((Ideal Loop)): -> [T: #6,F: #3]
        #RBX   (%10 ): (Ideal Phi)          [ #RBX (%21), #RBX (%20) ]     (Ideal IR: #15)
        #RAX   (%9  ): (Ideal Phi)          [ #RAX (%18), #RAX (%17) ]     (Ideal IR: #9)
        #Flags (%8  ): (CmpImm 0)           [ #RAX (%9) ]                  (Ideal IR: #10)
               (%4  ): (Jmp GEq)            [ #Flags (%8) ]                (Ideal IR: #12)

      Block #6 ((Ideal (CProj 0))): -> []
        #RCX   (%22 ): Mov                  [ #RAX (%9) ]                  (Ideal IR: #9)
        #RCX   (%11 ): Add                  [ #RCX (%22), #RBX (%10) ]     (Ideal IR: #16)
        #RAX   (%18 ): Mov                  [ #RBX (%10) ]                 (Ideal IR: #15)
        #RBX   (%21 ): Mov                  [ #RCX (%11) ]                 (Ideal IR: #16)

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

      Block #1 ((Ideal Start)): -> [#8]
        #RSI   (%25 ): (Int 5)                                             (Ideal IR: #6)
        #RSI   (%24 ): (SubImm 1)           [ #RSI (%25) ]                 (Ideal IR: #12)
        #RAX   (%19 ): (Int 0)                                             (Ideal IR: #4)
        #RCX   (%36 ): (Int 0)                                             (Ideal IR: #4)
        #RAX   (%37 ): (Int 0)                                             (Ideal IR: #4)
        #RBX   (%45 ): (Int 0)                                             (Ideal IR: #4)

      Block #8 ((Ideal Loop)): -> [T: #13,F: #6]
        #RBX   (%28 ): (Ideal Phi)          [ #RBX (%29), #RBX (%45) ]     (Ideal IR: #32)
        #RCX   (%18 ): (Ideal Phi)          [ #RCX (%33), #RCX (%36) ]     (Ideal IR: #9)
        #RAX   (%44 ): Mov                  [ #RBX (%28) ]                 (Ideal IR: #32)
        #RBX   (%35 ): Mov                  [ #RCX (%18) ]                 (Ideal IR: #9)
        #Flags (%23 ): Cmp                  [ #RBX (%35), #RSI (%24) ]     (Ideal IR: #13)
               (%7  ): (Jmp Eq)             [ #Flags (%23) ]               (Ideal IR: #15)

      Block #13 ((Ideal (CProj 0))): -> [#11]
        #RDX   (%34 ): Mov                  [ #RBX (%35) ]                 (Ideal IR: #9)
        #RDX   (%17 ): (AddImm 1)           [ #RDX (%34) ]                 (Ideal IR: #19)
        #RBX   (%38 ): (Int 0)                                             (Ideal IR: #4)
        #RCX   (%42 ): (Int 0)                                             (Ideal IR: #4)
        #RBX   (%43 ): Mov                  [ #RAX (%44) ]                 (Ideal IR: #32)

      Block #11 ((Ideal Loop)): -> [T: #12,F: #9]
        #RCX   (%20 ): (Ideal Phi)          [ #RCX (%21), #RCX (%42) ]     (Ideal IR: #24)
        #RBX   (%29 ): (Ideal Phi)          [ #RBX (%40), #RBX (%43) ]     (Ideal IR: #33)
        #RAX   (%41 ): Mov                  [ #RCX (%20) ]                 (Ideal IR: #24)
        #RCX   (%32 ): Mov                  [ #RDX (%17) ]                 (Ideal IR: #19)
        #RCX   (%16 ): Add                  [ #RCX (%32), #RAX (%41) ]     (Ideal IR: #25)
        #Flags (%15 ): (CmpImm 11)          [ #RCX (%16) ]                 (Ideal IR: #27)
               (%10 ): (Jmp Eq)             [ #Flags (%15) ]               (Ideal IR: #29)

      Block #12 ((Ideal (CProj 0))): -> []
        #RCX   (%39 ): Mov                  [ #RAX (%41) ]                 (Ideal IR: #24)
        #RCX   (%21 ): (AddImm 2)           [ #RCX (%39) ]                 (Ideal IR: #35)
        #RBX   (%40 ): Mov                  [ #RAX (%41) ]                 (Ideal IR: #24)

      Block #9 ((Ideal (CProj 1))): -> []
        #RCX   (%33 ): Mov                  [ #RDX (%17) ]                 (Ideal IR: #19)

      Block #6 ((Ideal (CProj 1))): -> [T: #4,F: #30]
        #Flags (%27 ): (CmpImm 0)           [ #RAX (%44) ]                 (Ideal IR: #37)
               (%5  ): (Jmp Eq)             [ #Flags (%27) ]               (Ideal IR: #38)

      Block #4 ((Ideal (CProj 0))): -> [#3]

      Block #30 ((Ideal (CProj 1))): -> [#3]

      Block #3 ((Ideal Region)): -> [#2]

      Block #2 ((Ideal Stop)): -> []



      mov rsi, 5
      sub rsi, 1
      mov rax, 0
      mov rcx, 0
      mov rax, 0
      mov rbx, 0
      Loop_8:
      mov rax, rbx
      mov rbx, rcx
      cmp rbx, rsi
      jne L_6

      L_13:
      mov rdx, rbx
      add rdx, 1
      mov rbx, 0
      mov rcx, 0
      mov rbx, rax
      Loop_11:
      mov rax, rcx
      mov rcx, rdx
      add rcx, rax
      cmp rcx, 11
      jne L_9

      L_12:
      mov rcx, rax
      add rcx, 2
      mov rbx, rax
      jmp Loop_11

      L_9:
      mov rcx, rdx
      jmp Loop_8

      L_6:
      cmp rax, 0
      jne L_3

      L_4:
      L_30:
      L_3:
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

      Block #1 ((Ideal Start)): -> [T: #4,F: #13]
        #RAX   (%12 ): (Int 1)                                             (Ideal IR: #5)
        #RAX   (%11 ): (LshImm 2)           [ #RAX (%12) ]                 (Ideal IR: #7)
        #RAX   (%10 ): (OrImm 0)            [ #RAX (%11) ]                 (Ideal IR: #8)
        #RAX   (%9  ): (RshImm 1)           [ #RAX (%10) ]                 (Ideal IR: #10)
        #RAX   (%8  ): (AndImm 1)           [ #RAX (%9) ]                  (Ideal IR: #12)
        #Flags (%7  ): (CmpImm 1)           [ #RAX (%8) ]                  (Ideal IR: #14)
               (%5  ): (Jmp Eq)             [ #Flags (%7) ]                (Ideal IR: #15)

      Block #4 ((Ideal (CProj 0))): -> [#3]

      Block #13 ((Ideal (CProj 1))): -> [#3]

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
      L_13:
      L_3:
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

      Block #1 ((Ideal Start)): -> []
        #RCX   (%12 ): (Int 4)                                             (Ideal IR: #25)
        #R8    (%13 ): (Int 5)                                             (Ideal IR: #26)
        #R9    (%14 ): (Int 6)                                             (Ideal IR: #27)
        #RDX   (%11 ): (Int 3)                                             (Ideal IR: #24)
        #RSI   (%10 ): (Int 2)                                             (Ideal IR: #23)
        #RDI   (%9  ): (Int 1)                                             (Ideal IR: #22)
               (%8  ): (FunctionCall 1)     [ #RDI (%9), #RSI (%10), #RDX (%11), #RCX (%12), #R8 (%13), #R9 (%14) ] (Ideal IR: #28)

      Block #6 ((Ideal (CProj 0))): -> [T: #4,F: #36]
        #RAX   (%34 ): (AddImm 69)          [ #RAX (%35) ]                 (Ideal IR: #33)
        #Flags (%33 ): (CmpImm 0)           [ #RAX (%34) ]                 (Ideal IR: #35)
               (%5  ): (Jmp Eq)             [ #Flags (%33) ]               (Ideal IR: #36)

      Block #4 ((Ideal (CProj 0))): -> [#3]

      Block #36 ((Ideal (CProj 1))): -> [#3]

      Block #3 ((Ideal Region)): -> [#2]

      Block #2 ((Ideal Stop)): -> []



      mov rcx, 4
      mov r8, 5
      mov r9, 6
      mov rdx, 3
      mov rsi, 2
      mov rdi, 1
      call f
      add rax, 69
      cmp rax, 0
      jne L_3

      L_4:
      L_36:
      L_3:

      === Machine Graph (Linearized with registers) ===

      Block #1 ((Ideal Start)): -> [#17]

      Block #17 ((FunctionProlog 1)): -> [#16]
        #RCX   (%29 ): (Param 3)                                           (Ideal IR: #12)
        #RSI   (%27 ): (Param 1)                                           (Ideal IR: #10)
        #RDI   (%26 ): (Param 0)                                           (Ideal IR: #9)
        #R9    (%31 ): (Param 5)                                           (Ideal IR: #14)
        #R8    (%30 ): (Param 4)                                           (Ideal IR: #13)
        #RDX   (%28 ): (Param 2)                                           (Ideal IR: #11)
        #RAX   (%38 ): (Int 69)                                            (Ideal IR: #15)
        #RAX   (%24 ): Sub                  [ #RAX (%38), #RDI (%26) ]     (Ideal IR: #16)
        #RAX   (%23 ): Add                  [ #RAX (%24), #RSI (%27) ]     (Ideal IR: #17)
        #RAX   (%22 ): Add                  [ #RAX (%23), #RDX (%28) ]     (Ideal IR: #18)
        #RAX   (%21 ): Add                  [ #RAX (%22), #RCX (%29) ]     (Ideal IR: #19)
        #RAX   (%20 ): Add                  [ #RAX (%21), #R8 (%30) ]      (Ideal IR: #20)
        #RAX   (%19 ): Add                  [ #RAX (%20), #R9 (%31) ]      (Ideal IR: #21)

      Block #16 ((Ideal Region)): -> []
        #RAX   (%18 ): (Ideal Phi)          [ #RAX (%19) ]                 (Ideal IR: #4)
        #RAX   (%15 ): Return               [ #RAX (%18) ]                 (Ideal IR: #6)

      Block #2 ((Ideal Stop)): -> []



      f:
      mov rax, 69
      sub rax, rdi
      add rax, rsi
      add rax, rdx
      add rax, rcx
      add rax, r8
      add rax, r9
      L_16:
      ret
      |}]
