#pragma once
#include "AST.h"

class CodeGenerator
{
public:
    CodeGenerator(const AST& ast) : m_ast(ast)
    {
        Stack stack;

        stack.PushVariable({"print", 0, 0});

        std::format_to(std::back_inserter(m_code), "format ELF64 executable 3\n");
        std::format_to(std::back_inserter(m_code), "segment readable executable\n");
        std::format_to(std::back_inserter(m_code), "entry start\n");

        std::format_to(std::back_inserter(m_code), "print:\n");
        std::format_to(std::back_inserter(m_code), "        mov     r8, 7378697629483820647\n");
        std::format_to(std::back_inserter(m_code), "        sub     rsp, 40\n");
        std::format_to(std::back_inserter(m_code), "        mov     BYTE [rsp+31], 10\n");
        std::format_to(std::back_inserter(m_code), "        lea     rcx, [rsp+30]\n");
        std::format_to(std::back_inserter(m_code), ".L2:\n");
        std::format_to(std::back_inserter(m_code), "        mov     rax, rdi\n");
        std::format_to(std::back_inserter(m_code), "        mov     rsi, rcx\n");
        std::format_to(std::back_inserter(m_code), "        sub     rcx, 1\n");
        std::format_to(std::back_inserter(m_code), "        imul    r8\n");
        std::format_to(std::back_inserter(m_code), "        mov     rax, rdi\n");
        std::format_to(std::back_inserter(m_code), "        sar     rax, 63\n");
        std::format_to(std::back_inserter(m_code), "        sar     rdx, 2\n");
        std::format_to(std::back_inserter(m_code), "        sub     rdx, rax\n");
        std::format_to(std::back_inserter(m_code), "        lea     rax, [rdx+rdx*4]\n");
        std::format_to(std::back_inserter(m_code), "        add     rax, rax\n");
        std::format_to(std::back_inserter(m_code), "        sub     rdi, rax\n");
        std::format_to(std::back_inserter(m_code), "        add     edi, 48\n");
        std::format_to(std::back_inserter(m_code), "        mov     BYTE [rcx+1], dil\n");
        std::format_to(std::back_inserter(m_code), "        mov     rdi, rdx\n");
        std::format_to(std::back_inserter(m_code), "        test    rdx, rdx\n");
        std::format_to(std::back_inserter(m_code), "        jne     .L2\n");
        std::format_to(std::back_inserter(m_code), "        lea     rdx, [rsp+32]\n");
        std::format_to(std::back_inserter(m_code), "        mov     edi, 1\n");
        std::format_to(std::back_inserter(m_code), "        sub     rdx, rsi\n");
        std::format_to(std::back_inserter(m_code), "        mov     rax, 1\n");
        std::format_to(std::back_inserter(m_code), "        syscall\n");
        std::format_to(std::back_inserter(m_code), "        add     rsp, 40\n");
        std::format_to(std::back_inserter(m_code), "        ret\n");

        m_ast.GenerateFunctions(stack, m_code, 1);

        std::format_to(std::back_inserter(m_code), "start:\n");
        std::format_to(std::back_inserter(m_code), "mov rbp, rsp\n");

        m_ast.GenerateCode(stack, m_code, 1);

        std::format_to(std::back_inserter(m_code), "mov rdi, 0\n");
        std::format_to(std::back_inserter(m_code), "mov rax, 60\n");
        std::format_to(std::back_inserter(m_code), "syscall\n");

        std::format_to(std::back_inserter(m_code), "segment readable writable\n");
    }

    std::string_view GetCode() const { return m_code; }

private:
    AST m_ast;
    std::string m_code;
};
template<typename... Args>
static constexpr void PrintASMIndented(std::string& buffer, int indent, std::format_string<Args...> format_str, Args&&... args)
{
    auto inserter = std::back_inserter(buffer);
    std::format_to(inserter, "{:{}}", "", indent * 4);
    std::format_to(inserter, format_str, std::forward<Args>(args)...);
    std::format_to(inserter, "\n");
}
