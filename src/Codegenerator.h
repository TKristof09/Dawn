#pragma once
#include "AST.h"

class CodeGenerator
{
public:
    CodeGenerator(const AST& ast) : m_ast(ast)
    {
        std::format_to(std::back_inserter(m_code), "format ELF64 executable 3\n");
        std::format_to(std::back_inserter(m_code), "segment readable executable\n");
        std::format_to(std::back_inserter(m_code), "entry start\n");
        std::format_to(std::back_inserter(m_code), "start:\n");
        std::format_to(std::back_inserter(m_code), "mov rbp, rsp\n");

        m_ast.GenerateCode(m_code, 1);

        std::format_to(std::back_inserter(m_code), "mov rdi, rax\n");
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
