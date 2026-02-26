open Dawn

let test str =
    (* HACK: the output seems to be dependant on node ids. So this just resets it to get more stable output until i fix *)
    Machine_node.reset_id ();
    Node.reset_id ();

    match Parser.parse_str str with
    | Ok ast ->
        let linker = Linker.create () in
        let son = Son.of_ast ast linker in
        Sccp.run son linker;
        let son = Graph.readonly son in
        let type_errors = Type_check.run son in
        if not (List.is_empty type_errors) then
          List.iter print_endline type_errors
        else
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
                  Ir_printer.to_string_machine_linear_regs g program reg_assoc
                  |> Printf.printf "%s\n";
                  (g, reg_assoc, program))
          in
          Asm_emit.emit_program functions linker |> print_endline
    | Error msg -> Printf.eprintf "%s\n" msg

let%expect_test "" =
    let test_str =
        {|
    let k:i64 = 69;
    let i:i64 = k;
    let j:i64 = i / 69;
    let x:i64 = i / 420;
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

      Block #12 ((Ideal (CProj 0))): -> [T: #10,F: #17]
        #RAX   (%16 ): (Int 69)                                                      (Ideal IR: #6)
        #RAX   (%15 ): Div             [ #RAX (%16), #RAX (%16) ]                    (Ideal IR: #8)
        #Flags (%14 ): (CmpImm 70)     [ #RAX (%15) ]                                (Ideal IR: #13)
               (%11 ): (Jmp Eq)        [ #Flags (%14) ]                              (Ideal IR: #14)

      Block #10 ((Ideal (CProj 0))): -> [#9]
        #RAX   (%29 ): (Int 1)                                                       (Ideal IR: #11)
        #RAX   (%28 ): (AddImm 1)      [ #RAX (%29) ]                                (Ideal IR: #19)

      Block #17 ((Ideal (CProj 1))): -> [#9]

      Block #9 ((Ideal Region)): -> [T: #7,F: #21]
        #RAX   (%27 ): (Ideal Phi)     [ #RAX (%28), #RAX (%15) ]                    (Ideal IR: #21)
        #RBX   (%20 ): (Int 0)                                                       (Ideal IR: #22)
        #Flags (%19 ): (CmpImm 1)      [ #RBX (%20) ]                                (Ideal IR: #23)
               (%8  ): (Jmp Eq)        [ #Flags (%19) ]                              (Ideal IR: #24)

      Block #7 ((Ideal (CProj 0))): -> [#6]
        #RAX   (%26 ): (Int 4)                                                       (Ideal IR: #28)
        #RAX   (%25 ): (AddImm 1)      [ #RAX (%26) ]                                (Ideal IR: #29)

      Block #21 ((Ideal (CProj 1))): -> [#6]

      Block #6 ((Ideal Region)): -> [T: #4,F: #30]
        #RAX   (%24 ): (Ideal Phi)     [ #RAX (%25), #RAX (%27) ]                    (Ideal IR: #31)
        #Flags (%23 ): (CmpImm 0)      [ #RAX (%24) ]                                (Ideal IR: #32)
               (%5  ): (Jmp Eq)        [ #Flags (%23) ]                              (Ideal IR: #33)

      Block #4 ((Ideal (CProj 0))): -> [#3]

      Block #30 ((Ideal (CProj 1))): -> [#3]

      Block #3 ((Ideal Region)): -> [#2]

      Block #2 ((Ideal Stop)): -> []


      format ELF64 executable 3
      entry start
      segment readable executable

      print:
          mov     rsi, rdi
          add     rsi, 8
          mov     rdx, [rdi]
          mov     rax, 1
          mov     rdi, 1
          syscall
          ret
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
          ret

      start:
      mov rbp, rsp
      mov rax, 69
      cqo
      idiv rax 		; rax = rax / rax
      cmp rax, 70
      jne L_17

      L_10:

      mov rax, 1
      add rax, 1
      jmp L_9
      L_17:

      jmp L_9
      L_9:

      mov rbx, 0
      cmp rbx, 1
      jne L_21

      L_7:

      mov rax, 4
      add rax, 1
      jmp L_6
      L_21:

      jmp L_6
      L_6:

      cmp rax, 0
      jne L_30

      L_4:

      jmp L_3
      L_30:

      jmp L_3
      L_3:

      ;Exit program
      mov rax, 60
      xor rdi, rdi
      syscall
      |}]

let%expect_test "fibonacci" =
    let test_str =
        {|
    let x:i64 = 0;
    let y:i64 = 1;
    while(0 <= x) {
        let tmp:i64 = x + y;
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
        #RBX   (%18 ): (Int 0)                                                       (Ideal IR: #6)
        #RAX   (%21 ): (Int 1)                                                       (Ideal IR: #7)

      Block #5 ((Ideal Loop)): -> [T: #6,F: #3]
        #RAX   (%11 ): (Ideal Phi)     [ #RAX (%12), #RAX (%21) ]                    (Ideal IR: #17)
        #RBX   (%10 ): (Ideal Phi)     [ #RBX (%19), #RBX (%18) ]                    (Ideal IR: #11)
        #RCX   (%20 ): Mov             [ #RAX (%11) ]                                (Ideal IR: #17)
        #RAX   (%17 ): Mov             [ #RBX (%10) ]                                (Ideal IR: #11)
        #Flags (%9  ): (CmpImm 0)      [ #RAX (%17) ]                                (Ideal IR: #12)
               (%4  ): (Jmp GEq)       [ #Flags (%9) ]                               (Ideal IR: #14)

      Block #6 ((Ideal (CProj 0))): -> [#5]
        #RAX   (%12 ): Add             [ #RAX (%17), #RCX (%20) ]                    (Ideal IR: #18)
        #RBX   (%19 ): Mov             [ #RCX (%20) ]                                (Ideal IR: #17)

      Block #3 ((Ideal (CProj 1))): -> [#2]

      Block #2 ((Ideal Stop)): -> []


      format ELF64 executable 3
      entry start
      segment readable executable

      print:
          mov     rsi, rdi
          add     rsi, 8
          mov     rdx, [rdi]
          mov     rax, 1
          mov     rdi, 1
          syscall
          ret
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
          ret

      start:
      mov rbp, rsp
      mov rbx, 0
      mov rax, 1
      jmp Loop_5
      Loop_5:

      mov rcx, rax
      mov rax, rbx
      cmp rax, 0
      jl L_3

      L_6:

      add rax, rcx
      mov rbx, rcx
      jmp Loop_5
      L_3:

      Exit:
      ;Exit program
      mov rax, 60
      xor rdi, rdi
      syscall
      |}]

let%expect_test "nested loop" =
    let test_str =
        {|
    let i:i64 = 0;
    let sum:i64 = 0;
    let c:i64 = 5;
    while(i == c - 1) {
        i = i + 1;
        let j:i64 = 0;
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

      Block #1 ((Ideal Start)): -> [#20]

      Block #20 ((Ideal (CProj 0))): -> [#8]
        #RDX   (%24 ): (Int 5)                                                       (Ideal IR: #8)
        #RDX   (%23 ): (SubImm 1)      [ #RDX (%24) ]                                (Ideal IR: #14)
        #RBX   (%19 ): (Int 0)                                                       (Ideal IR: #6)

      Block #8 ((Ideal Loop)): -> [T: #13,F: #6]
        #Flags (%34 ): (CmpImm 0)      [ #RDX (%23) ]                                (Ideal IR: #15)
               (%7  ): (Jmp Eq)        [ #Flags (%34) ]                              (Ideal IR: #17)

      Block #13 ((Ideal (CProj 0))): -> [#11]
        #RAX   (%32 ): (Int 0)                                                       (Ideal IR: #6)

      Block #11 ((Ideal Loop)): -> [T: #12,F: #9]
        #RAX   (%17 ): (Ideal Phi)     [ #RAX (%18), #RAX (%32) ]                    (Ideal IR: #26)
        #RCX   (%29 ): Mov             [ #RAX (%17) ]                                (Ideal IR: #26)
        #RCX   (%16 ): (AddImm 1)      [ #RCX (%29) ]                                (Ideal IR: #27)
        #Flags (%15 ): (CmpImm 11)     [ #RCX (%16) ]                                (Ideal IR: #29)
               (%10 ): (Jmp Eq)        [ #Flags (%15) ]                              (Ideal IR: #31)

      Block #12 ((Ideal (CProj 0))): -> [#11]
        #RAX   (%18 ): (AddImm 2)      [ #RAX (%17) ]                                (Ideal IR: #37)

      Block #9 ((Ideal (CProj 1))): -> [#8]

      Block #6 ((Ideal (CProj 1))): -> [T: #4,F: #27]
        #Flags (%26 ): (CmpImm 0)      [ #RBX (%19) ]                                (Ideal IR: #39)
               (%5  ): (Jmp Eq)        [ #Flags (%26) ]                              (Ideal IR: #40)

      Block #4 ((Ideal (CProj 0))): -> [#3]

      Block #27 ((Ideal (CProj 1))): -> [#3]

      Block #3 ((Ideal Region)): -> [#2]

      Block #2 ((Ideal Stop)): -> []


      format ELF64 executable 3
      entry start
      segment readable executable

      print:
          mov     rsi, rdi
          add     rsi, 8
          mov     rdx, [rdi]
          mov     rax, 1
          mov     rdi, 1
          syscall
          ret
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
          ret

      start:
      mov rbp, rsp
      mov rdx, 5
      sub rdx, 1
      mov rbx, 0
      jmp Loop_8
      Loop_8:

      cmp rdx, 0
      jne L_6

      L_13:

      mov rax, 0
      jmp Loop_11
      Loop_11:

      mov rcx, rax
      add rcx, 1
      cmp rcx, 11
      jne L_9

      L_12:

      add rax, 2
      jmp Loop_11
      L_9:

      jmp Loop_8
      L_6:

      cmp rbx, 0
      jne L_27

      L_4:

      jmp L_3
      L_27:

      jmp L_3
      L_3:

      ;Exit program
      mov rax, 60
      xor rdi, rdi
      syscall
      |}]

let%expect_test "binops" =
    let test_str =
        {|
    let i:i64 = 0;
    i = i | (1 << 2);
    if(((i>>1) & 1) == 1) {}
    |}
    in
    test test_str;
    [%expect
        {|
      === Machine Graph (Linearized with registers) ===

      Block #1 ((Ideal Start)): -> [#6]

      Block #6 ((Ideal (CProj 0))): -> [T: #4,F: #10]
        #RAX   (%9  ): (Int 1)                                                       (Ideal IR: #7)
        #Flags (%8  ): (CmpImm 0)      [ #RAX (%9) ]                                 (Ideal IR: #16)
               (%5  ): (Jmp Eq)        [ #Flags (%8) ]                               (Ideal IR: #17)

      Block #4 ((Ideal (CProj 0))): -> [#3]

      Block #10 ((Ideal (CProj 1))): -> [#3]

      Block #3 ((Ideal Region)): -> [#2]

      Block #2 ((Ideal Stop)): -> []


      format ELF64 executable 3
      entry start
      segment readable executable

      print:
          mov     rsi, rdi
          add     rsi, 8
          mov     rdx, [rdi]
          mov     rax, 1
          mov     rdi, 1
          syscall
          ret
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
          ret

      start:
      mov rbp, rsp
      mov rax, 1
      cmp rax, 0
      jne L_10

      L_4:

      jmp L_3
      L_10:

      jmp L_3
      L_3:

      ;Exit program
      mov rax, 60
      xor rdi, rdi
      syscall
      |}]

let%expect_test "function call" =
    let test_str =
        {|
    fun f(a: i64, b: i64, c: i64, d: i64, e: i64, f: i64) -> i64 {
        69 - a + b + c + d + e + f
    }

    let i: i64 = f(1,2,3,4,5,6) + 69;
    if(i==0) {}
    |}
    in
    test test_str;
    [%expect
        {|
      === Machine Graph (Linearized with registers) ===

      Block #1 ((Ideal Start)): -> [#9]

      Block #9 ((Ideal (CProj 0))): -> [#7]
        #RDX   (%12 ): (Int 3)                                                       (Ideal IR: #26)
        #RCX   (%13 ): (Int 4)                                                       (Ideal IR: #27)
        #R9    (%15 ): (Int 6)                                                       (Ideal IR: #29)
        #R8    (%14 ): (Int 5)                                                       (Ideal IR: #28)
        #RSI   (%11 ): (Int 2)                                                       (Ideal IR: #25)
        #RDI   (%10 ): (Int 1)                                                       (Ideal IR: #24)
               (%8  ): (FunctionCall 1) [ #RDI (%10), #RSI (%11), #RDX (%12), #RCX (%13), #R8 (%14), #R9 (%15) ] (Ideal IR: #30)

      Block #7 (FunctionCallEnd): -> [#6]
        #RAX   (%25 ):   |-(DProj 1)                                                 (Ideal IR: #33)

      Block #6 ((Ideal (CProj 0))): -> [T: #4,F: #26]
        #RAX   (%24 ): (AddImm 69)     [ #RAX (%25) ]                                (Ideal IR: #35)
        #Flags (%23 ): (CmpImm 0)      [ #RAX (%24) ]                                (Ideal IR: #37)
               (%5  ): (Jmp Eq)        [ #Flags (%23) ]                              (Ideal IR: #38)

      Block #4 ((Ideal (CProj 0))): -> [#3]

      Block #26 ((Ideal (CProj 1))): -> [#3]

      Block #3 ((Ideal Region)): -> [#2]

      Block #2 ((Ideal Stop)): -> []


      === Machine Graph (Linearized with registers) ===

      Block #18 ((FunctionProlog 1)): -> [#17]
        #R9    (%21 ): (Param 5)                                                     (Ideal IR: #16)
        #RBX   (%28 ): (CalleeSave RBX)                                               (Ideal IR: #9)
        #R12   (%29 ): (CalleeSave R12)                                               (Ideal IR: #9)
        #R13   (%30 ): (CalleeSave R13)                                               (Ideal IR: #9)
        #R14   (%31 ): (CalleeSave R14)                                               (Ideal IR: #9)
        #R15   (%32 ): (CalleeSave R15)                                               (Ideal IR: #9)
        #RSP   (%33 ): (CalleeSave RSP)                                               (Ideal IR: #9)
        #RBP   (%34 ): (CalleeSave RBP)                                               (Ideal IR: #9)
        #RAX   (%35 ): Mov             [ #R9 (%21) ]                                 (Ideal IR: #16)
        #RAX   (%20 ): (AddImm 82)     [ #RAX (%35) ]                                (Ideal IR: #23)

      Block #17 ((Ideal Region)): -> []
        #RAX   (%19 ): (Ideal Phi)     [ #RAX (%20) ]                                (Ideal IR: #6)
        #RAX   (%16 ): Return          [ #RAX (%19), #RBX (%28), #R12 (%29), #R13 (%30), #R14 (%31), #R15 (%32), #RSP (%33), #RBP (%34) ] (Ideal IR: #8)


      format ELF64 executable 3
      entry start
      segment readable executable

      print:
          mov     rsi, rdi
          add     rsi, 8
          mov     rdx, [rdi]
          mov     rax, 1
          mov     rdi, 1
          syscall
          ret
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
          ret

      start:
      mov rbp, rsp
      mov rdx, 3
      mov rcx, 4
      mov r9, 6
      mov r8, 5
      mov rsi, 2
      mov rdi, 1
      call f
      add rax, 69
      cmp rax, 0
      jne L_26

      L_4:

      jmp L_3
      L_26:

      jmp L_3
      L_3:

      ;Exit program
      mov rax, 60
      xor rdi, rdi
      syscall

      f:
      push rbp
      mov rbp, rsp
      mov rax, r9
      add rax, 82
      L_17:

      leave
      ret
      |}]
