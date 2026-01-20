open Dawn

let test str =
    match Parser.parse_str str with
    | Ok ast ->
        let son = Son.of_ast ast in
        let machine_graph, program = Scheduler.schedule son in
        let program, reg_assoc = Basic_reg_allocator.allocate machine_graph program in
        let code = Asm_emit.emit_program machine_graph reg_assoc program in
        Ir_printer.to_string_machine_linear_regs machine_graph program reg_assoc
        |> Printf.printf "%s\n";
        Printf.printf "\n%s\n" code
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
        #RBX   (%15 ): (Int 1)                                             (Ideal IR: #9)
        #RBX   (%14 ): (AddImm 69)          [ #RBX (%15) ]                 (Ideal IR: #10)
        #RAX   (%17 ): (Int 69)                                            (Ideal IR: #4)
        #RAX   (%16 ): Div                  [ #RAX (%17), #RAX (%17) ]     (Ideal IR: #6)
        #Flags (%13 ): Cmp                  [ #RBX (%14), #RAX (%16) ]     (Ideal IR: #11)
               (%11 ): (Jmp Eq)             [ #Flags (%13) ]               (Ideal IR: #12)

      Block #10 ((Ideal (CProj 0))): -> [#9]
        #RAX   (%22 ): (AddImm 1)           [ #RAX (%16) ]                 (Ideal IR: #17)

      Block #18 ((Ideal (CProj 1))): -> [#9]

      Block #9 ((Ideal Region)): -> [T: #7,F: #23]
        #RAX   (%21 ): (Ideal Phi)          [ #RAX (%22), #RAX (%16) ]     (Ideal IR: #19)
        #Flags (%20 ): (CmpImm 0)           [ #RAX (%21) ]                 (Ideal IR: #21)
               (%8  ): (Jmp Eq)             [ #Flags (%20) ]               (Ideal IR: #22)

      Block #7 ((Ideal (CProj 0))): -> [#6]
        #RAX   (%29 ): (AddImm 4)           [ #RAX (%21) ]                 (Ideal IR: #27)

      Block #23 ((Ideal (CProj 1))): -> [#6]

      Block #6 ((Ideal Region)): -> [T: #4,F: #30]
        #RAX   (%28 ): (Ideal Phi)          [ #RAX (%29), #RAX (%21) ]     (Ideal IR: #29)
        #RBX   (%27 ): (Int 420)                                           (Ideal IR: #7)
        #RCX   (%39 ): Mov                  [ #RAX (%28) ]                 (Ideal IR: #29)
        #RAX   (%38 ): (Int 69)                                            (Ideal IR: #4)
        #RAX   (%26 ): Div                  [ #RAX (%38), #RBX (%27) ]     (Ideal IR: #8)
        #Flags (%25 ): Cmp                  [ #RAX (%26), #RCX (%39) ]     (Ideal IR: #30)
               (%5  ): (Jmp Eq)             [ #Flags (%25) ]               (Ideal IR: #31)

      Block #4 ((Ideal (CProj 0))): -> []
        #RBX   (%35 ): Mov                  [ #RCX (%39) ]                 (Ideal IR: #29)
        #RBX   (%32 ): (AddImm 1)           [ #RBX (%35) ]                 (Ideal IR: #36)

      Block #30 ((Ideal (CProj 1))): -> [#3]
        #RBX   (%34 ): Mov                  [ #RAX (%26) ]                 (Ideal IR: #8)

      Block #3 ((Ideal Region)): -> [#2]
        #RBX   (%31 ): (Ideal Phi)          [ #RBX (%32), #RBX (%34) ]     (Ideal IR: #38)

      Block #2 ((Ideal Stop)): -> []



      mov rbx, 1
      add rbx, 69
      mov rax, 69
      cqo
      idiv rax 		// rax = rax / rax
      cmp rbx, rax
      jne L_9

      L_10:
      add rax, 1
      L_18:
      L_9:
      cmp rax, 0
      jne L_6

      L_7:
      add rax, 4
      L_23:
      L_6:
      mov rbx, 420
      mov rcx, rax
      mov rax, 69
      cqo
      idiv rbx 		// rax = rax / rbx
      cmp rax, rcx
      jne L_30

      L_4:
      mov rbx, rcx
      add rbx, 1
      jmp L_3

      L_30:
      mov rbx, rax
      L_3:
      |}]

let%expect_test "fibonacci" =
    let test_str =
        {|
    let x:int = 0;
    let y:int = 1;
    while(x == 0) {
        let tmp:int = x + y;
        y = x;
        x = tmp;
    }
    |}
    in
    test test_str;
    [%expect
        {|
      === Machine Graph (Linearized with registers) ===

      Block #42 ((Ideal Start)): -> [#46]
        #RAX   (%59 ): (Int 0)                                             (Ideal IR: #42)
        #RBX   (%61 ): (Int 1)                                             (Ideal IR: #43)

      Block #46 ((Ideal Loop)): -> [T: #47,F: #44]
        #RAX   (%50 ): (Ideal Phi)          [ #RAX (%51), #RAX (%59) ]     (Ideal IR: #46)
        #RBX   (%52 ): (Ideal Phi)          [ #RBX (%56), #RBX (%61) ]     (Ideal IR: #53)
        #RCX   (%60 ): Mov                  [ #RBX (%52) ]                 (Ideal IR: #53)
        #RDX   (%58 ): Mov                  [ #RAX (%50) ]                 (Ideal IR: #46)
        #Flags (%49 ): (CmpImm 0)           [ #RDX (%58) ]                 (Ideal IR: #48)
               (%45 ): (Jmp Eq)             [ #Flags (%49) ]               (Ideal IR: #50)

      Block #47 ((Ideal (CProj 0))): -> []
        #RAX   (%57 ): Mov                  [ #RDX (%58) ]                 (Ideal IR: #46)
        #RAX   (%51 ): Add                  [ #RAX (%57), #RCX (%60) ]     (Ideal IR: #54)
        #RBX   (%56 ): Mov                  [ #RDX (%58) ]                 (Ideal IR: #46)

      Block #44 ((Ideal (CProj 1))): -> [#43]

      Block #43 ((Ideal Stop)): -> []



      mov rax, 0
      mov rbx, 1
      Loop_46:
      mov rcx, rbx
      mov rdx, rax
      cmp rdx, 0
      jne Exit

      L_47:
      mov rax, rdx
      add rax, rcx
      mov rbx, rdx
      jmp Loop_46

      L_44:
      Exit:
      |}]
