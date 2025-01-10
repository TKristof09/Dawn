format ELF64 executable 3
segment readable executable
entry start
print:
        mov     r8, 7378697629483820647
        sub     rsp, 40
        mov     BYTE [rsp+31], 10
        lea     rcx, [rsp+30]
.printL2:
        mov     rax, rdi
        mov     rsi, rcx
        sub     rcx, 1
        imul    r8
        mov     rax, rdi
        sar     rax, 63
        sar     rdx, 2
        sub     rdx, rax
        lea     rax, [rdx+rdx*4]
        add     rax, rax
        sub     rdi, rax
        add     edi, 48
        mov     BYTE [rcx+1], dil
        mov     rdi, rdx
        test    rdx, rdx
        jne     .printL2
        lea     rdx, [rsp+32]
        mov     edi, 1
        sub     rdx, rsi
        mov     rax, 1
        syscall
        add     rsp, 40
        ret
prints:
        mov     rsi, rdi
        add     rsi, 8
        mov     rdx, [rdi]
        mov     rax, 1
        mov     rdi, 1
        syscall
        ret
start:
        mov rbp, rsp
        ; variable declaration: board
        sub rsp, 488
        mov QWORD [rbp - 488], 60
        ; variable declaration: board_size
        sub rsp, 16
        mov QWORD [rbp - 504], 1
        ;  Number Literal
        mov rax, 30
        mov [rbp - 504], rax
        ; variable declaration: k
        sub rsp, 16
        mov QWORD [rbp - 520], 1
        ;  Number Literal
        mov rax, 0
        mov [rbp - 520], rax
        ; while
.L0:
        ;  Binary expression LT
        ;  Variable Access: k
        mov rax, [rbp - 520]
        push rax
        ;  Variable Access: board_size
        mov rax, [rbp - 504]
        mov rbx, rax
        pop rax
        cmp rax, rbx
        setb al
        test al, al
        jz .L1
        ;  Variable Assignment: board
        ;  Variable Access: k
        mov rax, [rbp - 520]
        push rax
        ;  Number Literal
        mov rax, 0
        pop rbx
        mov [rbp - 480 + rbx * 8], rax
        ;  Variable Assignment: board
        ;  Binary expression PLUS
        ;  Variable Access: k
        mov rax, [rbp - 520]
        push rax
        ;  Variable Access: board_size
        mov rax, [rbp - 504]
        mov rbx, rax
        pop rax
        add rax, rbx
        push rax
        ;  Number Literal
        mov rax, 0
        pop rbx
        mov [rbp - 480 + rbx * 8], rax
        ;  Variable Assignment: k
        ;  Binary expression PLUS
        ;  Variable Access: k
        mov rax, [rbp - 520]
        push rax
        ;  Number Literal
        mov rax, 1
        mov rbx, rax
        pop rax
        add rax, rbx
        mov [rbp - 520], rax
        jmp .L0
.L1:
        ; variable declaration: i
        sub rsp, 16
        mov QWORD [rbp - 536], 1
        ;  Number Literal
        mov rax, 0
        mov [rbp - 536], rax
        ; variable declaration: j
        sub rsp, 16
        mov QWORD [rbp - 552], 1
        ;  Number Literal
        mov rax, 1
        mov [rbp - 552], rax
        ; variable declaration: curr_board
        sub rsp, 16
        mov QWORD [rbp - 568], 1
        ;  Number Literal
        mov rax, 0
        mov [rbp - 568], rax
        ;  Variable Assignment: board
        ;  Number Literal
        mov rax, 0
        push rax
        ;  Number Literal
        mov rax, 1
        pop rbx
        mov [rbp - 480 + rbx * 8], rax
        ;  Variable Assignment: board
        ;  Number Literal
        mov rax, 29
        push rax
        ;  Number Literal
        mov rax, 1
        pop rbx
        mov [rbp - 480 + rbx * 8], rax
        ;  Variable Assignment: board
        ;  Number Literal
        mov rax, 30
        push rax
        ;  Number Literal
        mov rax, 1
        pop rbx
        mov [rbp - 480 + rbx * 8], rax
        ;  Variable Assignment: board
        ;  Number Literal
        mov rax, 59
        push rax
        ;  Number Literal
        mov rax, 1
        pop rbx
        mov [rbp - 480 + rbx * 8], rax
        ; variable declaration: a
        sub rsp, 16
        mov QWORD [rbp - 584], 1
        ; variable declaration: b
        sub rsp, 16
        mov QWORD [rbp - 600], 1
        ; variable declaration: c
        sub rsp, 16
        mov QWORD [rbp - 616], 1
        ; variable declaration: patterns
        sub rsp, 72
        mov QWORD [rbp - 688], 8
        ;  Variable Assignment: patterns
        ;  Number Literal
        mov rax, 0
        push rax
        ;  Number Literal
        mov rax, 0
        pop rbx
        mov [rbp - 680 + rbx * 8], rax
        ;  Variable Assignment: patterns
        ;  Number Literal
        mov rax, 1
        push rax
        ;  Number Literal
        mov rax, 1
        pop rbx
        mov [rbp - 680 + rbx * 8], rax
        ;  Variable Assignment: patterns
        ;  Number Literal
        mov rax, 2
        push rax
        ;  Number Literal
        mov rax, 1
        pop rbx
        mov [rbp - 680 + rbx * 8], rax
        ;  Variable Assignment: patterns
        ;  Number Literal
        mov rax, 3
        push rax
        ;  Number Literal
        mov rax, 1
        pop rbx
        mov [rbp - 680 + rbx * 8], rax
        ;  Variable Assignment: patterns
        ;  Number Literal
        mov rax, 4
        push rax
        ;  Number Literal
        mov rax, 0
        pop rbx
        mov [rbp - 680 + rbx * 8], rax
        ;  Variable Assignment: patterns
        ;  Number Literal
        mov rax, 5
        push rax
        ;  Number Literal
        mov rax, 1
        pop rbx
        mov [rbp - 680 + rbx * 8], rax
        ;  Variable Assignment: patterns
        ;  Number Literal
        mov rax, 6
        push rax
        ;  Number Literal
        mov rax, 1
        pop rbx
        mov [rbp - 680 + rbx * 8], rax
        ;  Variable Assignment: patterns
        ;  Number Literal
        mov rax, 7
        push rax
        ;  Number Literal
        mov rax, 0
        pop rbx
        mov [rbp - 680 + rbx * 8], rax
        ; variable declaration: lookup
        sub rsp, 16
        mov QWORD [rbp - 704], 1
        ; variable declaration: next_board
        sub rsp, 16
        mov QWORD [rbp - 720], 1
        ; while
.L2:
        ;  Binary expression LT
        ;  Variable Access: i
        mov rax, [rbp - 536]
        push rax
        ;  Variable Access: board_size
        mov rax, [rbp - 504]
        mov rbx, rax
        pop rax
        cmp rax, rbx
        setb al
        test al, al
        jz .L3
        ;  Variable Assignment: j
        ;  Number Literal
        mov rax, 0
        mov [rbp - 552], rax
        ; while
.L4:
        ;  Binary expression LT
        ;  Variable Access: j
        mov rax, [rbp - 552]
        push rax
        ;  Number Literal
        mov rax, 30
        mov rbx, rax
        pop rax
        cmp rax, rbx
        setb al
        test al, al
        jz .L5
        ; fn call
        ;  Variable Access: board
        ;  Binary expression PLUS
        ;  Binary expression MUL
        ;  Variable Access: curr_board
        mov rax, [rbp - 568]
        push rax
        ;  Variable Access: board_size
        mov rax, [rbp - 504]
        mov rbx, rax
        pop rax
        imul rax, rbx
        push rax
        ;  Variable Access: j
        mov rax, [rbp - 552]
        mov rbx, rax
        pop rax
        add rax, rbx
        mov rax, [rbp - 480 + rax * 8]
        mov [rbp - 728], rax
        mov rdi, [rbp - 728]
        call print
        ;  Variable Assignment: j
        ;  Binary expression PLUS
        ;  Variable Access: j
        mov rax, [rbp - 552]
        push rax
        ;  Number Literal
        mov rax, 1
        mov rbx, rax
        pop rax
        add rax, rbx
        mov [rbp - 552], rax
        jmp .L4
.L5:
        ;  Variable Assignment: j
        ;  Number Literal
        mov rax, 1
        mov [rbp - 552], rax
        ;  Variable Assignment: next_board
        ;  Binary expression MINUS
        ;  Number Literal
        mov rax, 1
        push rax
        ;  Variable Access: curr_board
        mov rax, [rbp - 568]
        mov rbx, rax
        pop rax
        sub rax, rbx
        mov [rbp - 720], rax
        ; while
.L6:
        ;  Binary expression LT
        ;  Variable Access: j
        mov rax, [rbp - 552]
        push rax
        ;  Binary expression MINUS
        ;  Variable Access: board_size
        mov rax, [rbp - 504]
        push rax
        ;  Number Literal
        mov rax, 1
        mov rbx, rax
        pop rax
        sub rax, rbx
        mov rbx, rax
        pop rax
        cmp rax, rbx
        setb al
        test al, al
        jz .L7
        ;  Variable Assignment: a
        ;  Variable Access: board
        ;  Binary expression MINUS
        ;  Binary expression PLUS
        ;  Binary expression MUL
        ;  Variable Access: curr_board
        mov rax, [rbp - 568]
        push rax
        ;  Variable Access: board_size
        mov rax, [rbp - 504]
        mov rbx, rax
        pop rax
        imul rax, rbx
        push rax
        ;  Variable Access: j
        mov rax, [rbp - 552]
        mov rbx, rax
        pop rax
        add rax, rbx
        push rax
        ;  Number Literal
        mov rax, 1
        mov rbx, rax
        pop rax
        sub rax, rbx
        mov rax, [rbp - 480 + rax * 8]
        mov [rbp - 584], rax
        ;  Variable Assignment: b
        ;  Variable Access: board
        ;  Binary expression PLUS
        ;  Binary expression MUL
        ;  Variable Access: curr_board
        mov rax, [rbp - 568]
        push rax
        ;  Variable Access: board_size
        mov rax, [rbp - 504]
        mov rbx, rax
        pop rax
        imul rax, rbx
        push rax
        ;  Variable Access: j
        mov rax, [rbp - 552]
        mov rbx, rax
        pop rax
        add rax, rbx
        mov rax, [rbp - 480 + rax * 8]
        mov [rbp - 600], rax
        ;  Variable Assignment: c
        ;  Variable Access: board
        ;  Binary expression PLUS
        ;  Binary expression PLUS
        ;  Binary expression MUL
        ;  Variable Access: curr_board
        mov rax, [rbp - 568]
        push rax
        ;  Variable Access: board_size
        mov rax, [rbp - 504]
        mov rbx, rax
        pop rax
        imul rax, rbx
        push rax
        ;  Variable Access: j
        mov rax, [rbp - 552]
        mov rbx, rax
        pop rax
        add rax, rbx
        push rax
        ;  Number Literal
        mov rax, 1
        mov rbx, rax
        pop rax
        add rax, rbx
        mov rax, [rbp - 480 + rax * 8]
        mov [rbp - 616], rax
        ;  Variable Assignment: lookup
        ;  Binary expression BOR
        ;  Binary expression BOR
        ;  Binary expression LSH
        ;  Variable Access: a
        mov rax, [rbp - 584]
        push rax
        ;  Number Literal
        mov rax, 2
        mov rbx, rax
        pop rax
        mov rcx, rbx
        shl rax, cl
        push rax
        ;  Binary expression LSH
        ;  Variable Access: b
        mov rax, [rbp - 600]
        push rax
        ;  Number Literal
        mov rax, 1
        mov rbx, rax
        pop rax
        mov rcx, rbx
        shl rax, cl
        mov rbx, rax
        pop rax
        or rax, rbx
        push rax
        ;  Variable Access: c
        mov rax, [rbp - 616]
        mov rbx, rax
        pop rax
        or rax, rbx
        mov [rbp - 704], rax
        ;  Variable Assignment: lookup
        ;  Binary expression BAND
        ;  Variable Access: lookup
        mov rax, [rbp - 704]
        push rax
        ;  Number Literal
        mov rax, 7
        mov rbx, rax
        pop rax
        and rax, rbx
        mov [rbp - 704], rax
        ;  Variable Assignment: board
        ;  Binary expression PLUS
        ;  Binary expression MUL
        ;  Variable Access: next_board
        mov rax, [rbp - 720]
        push rax
        ;  Variable Access: board_size
        mov rax, [rbp - 504]
        mov rbx, rax
        pop rax
        imul rax, rbx
        push rax
        ;  Variable Access: j
        mov rax, [rbp - 552]
        mov rbx, rax
        pop rax
        add rax, rbx
        push rax
        ;  Variable Access: patterns
        ;  Variable Access: lookup
        mov rax, [rbp - 704]
        mov rax, [rbp - 680 + rax * 8]
        pop rbx
        mov [rbp - 480 + rbx * 8], rax
        ;  Variable Assignment: j
        ;  Binary expression PLUS
        ;  Variable Access: j
        mov rax, [rbp - 552]
        push rax
        ;  Number Literal
        mov rax, 1
        mov rbx, rax
        pop rax
        add rax, rbx
        mov [rbp - 552], rax
        jmp .L6
.L7:
        ;  Variable Assignment: curr_board
        ;  Variable Access: next_board
        mov rax, [rbp - 720]
        mov [rbp - 568], rax
        ;  Variable Assignment: i
        ;  Binary expression PLUS
        ;  Variable Access: i
        mov rax, [rbp - 536]
        push rax
        ;  Number Literal
        mov rax, 1
        mov rbx, rax
        pop rax
        add rax, rbx
        mov [rbp - 536], rax
        jmp .L2
.L3:
        mov rdi, 0
        mov rax, 60
        syscall
segment readable
