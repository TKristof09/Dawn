open Dawn

let test str =
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

      Block #45 ((Ideal Start)): -> [#49]
        #RAX   (%56 ): (Int 1)                                             (Ideal IR: #43)
        #RAX   (%57 ): (Int 0)                                             (Ideal IR: #42)
        #RAX   (%61 ): (Int 0)                                             (Ideal IR: #42)
        #RBX   (%64 ): (Int 1)                                             (Ideal IR: #43)

      Block #49 ((Ideal Loop)): -> [T: #50,F: #47]
        #RBX   (%54 ): (Ideal Phi)          [ #RBX (%65), #RBX (%64) ]     (Ideal IR: #53)
        #RAX   (%53 ): (Ideal Phi)          [ #RAX (%62), #RAX (%61) ]     (Ideal IR: #47)
        #Flags (%52 ): (CmpImm 0)           [ #RAX (%53) ]                 (Ideal IR: #48)
               (%48 ): (Jmp GEq)            [ #Flags (%52) ]               (Ideal IR: #50)

      Block #50 ((Ideal (CProj 0))): -> []
        #RCX   (%66 ): Mov                  [ #RAX (%53) ]                 (Ideal IR: #47)
        #RCX   (%55 ): Add                  [ #RCX (%66), #RBX (%54) ]     (Ideal IR: #54)
        #RAX   (%62 ): Mov                  [ #RBX (%54) ]                 (Ideal IR: #53)
        #RBX   (%65 ): Mov                  [ #RCX (%55) ]                 (Ideal IR: #54)

      Block #47 ((Ideal (CProj 1))): -> [#46]

      Block #46 ((Ideal Stop)): -> []



      mov rax, 1
      mov rax, 0
      mov rax, 0
      mov rbx, 1
      Loop_49:
      cmp rax, 0
      jl Exit

      L_50:
      mov rcx, rax
      add rcx, rbx
      mov rax, rbx
      mov rbx, rcx
      jmp Loop_49

      L_47:
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

      Block #68 ((Ideal Start)): -> [#75]
        #RCX   (%92 ): (Int 5)                                             (Ideal IR: #60)
        #RCX   (%91 ): (SubImm 1)           [ #RCX (%92) ]                 (Ideal IR: #66)
        #RAX   (%86 ): (Int 0)                                             (Ideal IR: #58)
        #RAX   (%103): (Int 0)                                             (Ideal IR: #58)
        #RBX   (%104): (Int 0)                                             (Ideal IR: #58)
        #RBX   (%112): (Int 0)                                             (Ideal IR: #58)

      Block #75 ((Ideal Loop)): -> [T: #80,F: #73]
        #RAX   (%85 ): (Ideal Phi)          [ #RAX (%100), #RAX (%103) ]   (Ideal IR: #63)
        #RBX   (%95 ): (Ideal Phi)          [ #RBX (%96), #RBX (%112) ]    (Ideal IR: #86)
        #Flags (%90 ): Cmp                  [ #RAX (%85), #RCX (%91) ]     (Ideal IR: #67)
               (%74 ): (Jmp Eq)             [ #Flags (%90) ]               (Ideal IR: #69)

      Block #80 ((Ideal (CProj 0))): -> [#78]
        #RSI   (%101): Mov                  [ #RAX (%85) ]                 (Ideal IR: #63)
        #RSI   (%84 ): (AddImm 1)           [ #RSI (%101) ]                (Ideal IR: #73)
        #RAX   (%105): (Int 0)                                             (Ideal IR: #58)
        #RDX   (%109): (Int 0)                                             (Ideal IR: #58)

      Block #78 ((Ideal Loop)): -> [T: #79,F: #76]
        #RDX   (%87 ): (Ideal Phi)          [ #RDX (%88), #RDX (%109) ]    (Ideal IR: #78)
        #RBX   (%96 ): (Ideal Phi)          [ #RBX (%107), #RBX (%95) ]    (Ideal IR: #87)
        #RAX   (%108): Mov                  [ #RDX (%87) ]                 (Ideal IR: #78)
        #RDX   (%99 ): Mov                  [ #RSI (%84) ]                 (Ideal IR: #73)
        #RDX   (%83 ): Add                  [ #RDX (%99), #RAX (%108) ]    (Ideal IR: #79)
        #Flags (%82 ): (CmpImm 11)          [ #RDX (%83) ]                 (Ideal IR: #81)
               (%77 ): (Jmp Eq)             [ #Flags (%82) ]               (Ideal IR: #83)

      Block #79 ((Ideal (CProj 0))): -> []
        #RDX   (%106): Mov                  [ #RAX (%108) ]                (Ideal IR: #78)
        #RDX   (%88 ): (AddImm 2)           [ #RDX (%106) ]                (Ideal IR: #89)
        #RBX   (%107): Mov                  [ #RAX (%108) ]                (Ideal IR: #78)

      Block #76 ((Ideal (CProj 1))): -> []
        #RAX   (%100): Mov                  [ #RSI (%84) ]                 (Ideal IR: #73)

      Block #73 ((Ideal (CProj 1))): -> [T: #71,F: #97]
        #Flags (%94 ): (CmpImm 0)           [ #RBX (%95) ]                 (Ideal IR: #91)
               (%72 ): (Jmp Eq)             [ #Flags (%94) ]               (Ideal IR: #92)

      Block #71 ((Ideal (CProj 0))): -> [#70]

      Block #97 ((Ideal (CProj 1))): -> [#70]

      Block #70 ((Ideal Region)): -> [#69]

      Block #69 ((Ideal Stop)): -> []



      mov rcx, 5
      sub rcx, 1
      mov rax, 0
      mov rax, 0
      mov rbx, 0
      mov rbx, 0
      Loop_75:
      cmp rax, rcx
      jne L_73

      L_80:
      mov rsi, rax
      add rsi, 1
      mov rax, 0
      mov rdx, 0
      Loop_78:
      mov rax, rdx
      mov rdx, rsi
      add rdx, rax
      cmp rdx, 11
      jne L_76

      L_79:
      mov rdx, rax
      add rdx, 2
      mov rbx, rax
      jmp Loop_78

      L_76:
      mov rax, rsi
      jmp Loop_75

      L_73:
      cmp rbx, 0
      jne L_70

      L_71:
      L_97:
      L_70:
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

      Block #115 ((Ideal Start)): -> [T: #118,F: #127]
        #RAX   (%126): (Int 1)                                             (Ideal IR: #101)
        #RAX   (%125): (LshImm 2)           [ #RAX (%126) ]                (Ideal IR: #103)
        #RAX   (%124): (OrImm 0)            [ #RAX (%125) ]                (Ideal IR: #104)
        #RAX   (%123): (RshImm 1)           [ #RAX (%124) ]                (Ideal IR: #106)
        #RAX   (%122): (AndImm 1)           [ #RAX (%123) ]                (Ideal IR: #108)
        #Flags (%121): (CmpImm 1)           [ #RAX (%122) ]                (Ideal IR: #110)
               (%119): (Jmp Eq)             [ #Flags (%121) ]              (Ideal IR: #111)

      Block #118 ((Ideal (CProj 0))): -> [#117]

      Block #127 ((Ideal (CProj 1))): -> [#117]

      Block #117 ((Ideal Region)): -> [#116]

      Block #116 ((Ideal Stop)): -> []



      mov rax, 1
      sal rax, 2
      or rax, 0
      sar rax, 1
      and rax, 1
      cmp rax, 1
      jne L_117

      L_118:
      L_127:
      L_117:
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

      Block #129 ((Ideal Start)): -> []
        #RDX   (%139): (Int 3)                                             (Ideal IR: #139)
        #R8    (%141): (Int 5)                                             (Ideal IR: #141)
        #R9    (%142): (Int 6)                                             (Ideal IR: #142)
        #RSI   (%138): (Int 2)                                             (Ideal IR: #138)
        #RCX   (%140): (Int 4)                                             (Ideal IR: #140)
        #RDI   (%137): (Int 1)                                             (Ideal IR: #137)
               (%136): (FunctionCall 1)     [ #RDI (%137), #RSI (%138), #RDX (%139), #RCX (%140), #R8 (%141), #R9 (%142) ] (Ideal IR: #143)

      Block #134 ((Ideal (CProj 0))): -> [T: #132,F: #164]
        #RAX   (%162): (AddImm 69)          [ #RAX (%163) ]                (Ideal IR: #148)
        #Flags (%161): (CmpImm 0)           [ #RAX (%162) ]                (Ideal IR: #150)
               (%133): (Jmp Eq)             [ #Flags (%161) ]              (Ideal IR: #151)

      Block #132 ((Ideal (CProj 0))): -> [#131]

      Block #164 ((Ideal (CProj 1))): -> [#131]

      Block #131 ((Ideal Region)): -> [#130]

      Block #130 ((Ideal Stop)): -> []



      mov rdx, 3
      mov r8, 5
      mov r9, 6
      mov rsi, 2
      mov rcx, 4
      mov rdi, 1
      call f
      add rax, 69
      cmp rax, 0
      jne L_131

      L_132:
      L_164:
      L_131:

      === Machine Graph (Linearized with registers) ===

      Block #129 ((Ideal Start)): -> [#145]

      Block #145 ((FunctionProlog 1)): -> [#144]
        #RDI   (%154): (Param 0)                                           (Ideal IR: #124)
        #RSI   (%155): (Param 1)                                           (Ideal IR: #125)
        #RAX   (%166): (Int 69)                                            (Ideal IR: #130)
        #RAX   (%152): Sub                  [ #RAX (%166), #RDI (%154) ]   (Ideal IR: #131)
        #RAX   (%151): Add                  [ #RAX (%152), #RSI (%155) ]   (Ideal IR: #132)
        #RDX   (%156): (Param 2)                                           (Ideal IR: #126)
        #RCX   (%157): (Param 3)                                           (Ideal IR: #127)
        #R8    (%158): (Param 4)                                           (Ideal IR: #128)
        #R9    (%159): (Param 5)                                           (Ideal IR: #129)
        #RAX   (%150): Add                  [ #RAX (%151), #RDX (%156) ]   (Ideal IR: #133)
        #RAX   (%149): Add                  [ #RAX (%150), #RCX (%157) ]   (Ideal IR: #134)
        #RAX   (%148): Add                  [ #RAX (%149), #R8 (%158) ]    (Ideal IR: #135)
        #RAX   (%147): Add                  [ #RAX (%148), #R9 (%159) ]    (Ideal IR: #136)

      Block #144 ((Ideal Region)): -> []
        #RAX   (%146): (Ideal Phi)          [ #RAX (%147) ]                (Ideal IR: #119)
        #RAX   (%143): Return               [ #RAX (%146) ]                (Ideal IR: #121)

      Block #130 ((Ideal Stop)): -> []



      f:
      mov rax, 69
      sub rax, rdi
      add rax, rsi
      add rax, rdx
      add rax, rcx
      add rax, r8
      add rax, r9
      L_144:
      ret
      |}]
