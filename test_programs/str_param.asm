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
        ; fn declaration: dprint
dprint:
        enter 0, 0
        sub rsp, 8
        mov [rbp - 8], rdi
        sub rsp, 8
        mov [rbp - 16], rsi
        ; fn call
        ;  Variable Access: s
        mov rax, [rbp - 8]
        mov [rbp - 24], rax
        mov rdi, [rbp - 24]
        call prints
        ; fn call
        ;  Variable Access: s2
        mov rax, [rbp - 16]
        mov [rbp - 24], rax
        mov rdi, [rbp - 24]
        call prints
        leave
        ret
start:
        mov rbp, rsp
        ; variable declaration: s
        sub rsp, 16
        mov QWORD [rbp - 16], 1
        ;  String Literal
        lea rax, [STRLEN0]
        mov [rbp - 16], rax
        ; variable declaration: s2
        sub rsp, 16
        mov QWORD [rbp - 32], 1
        ;  String Literal
        lea rax, [STRLEN1]
        mov [rbp - 32], rax
        ; fn call
        ;  Variable Access: s
        mov rax, [rbp - 16]
        mov [rbp - 40], rax
        ;  Variable Access: s2
        mov rax, [rbp - 32]
        mov [rbp - 48], rax
        mov rdi, [rbp - 40]
        mov rsi, [rbp - 48]
        call dprint
        mov rdi, 0
        mov rax, 60
        syscall
segment readable
        STRLEN1 dq 12
        STR1 db "Hello, Eos!", 0xa
        STRLEN0 dq 14
        STR0 db "Hello, World!", 0xa
