#pragma once
#include "AST.h"

class CodeGenerator : public VisitorBase
{
public:
    CodeGenerator(const AST& ast) : m_ast(ast)
    {
        m_stack.PushVariable({
            "print", 0, Types::Function{{Types::Int()}, {Types::NoneType()}}
        });
        m_stack.PushVariable({
            "prints", 0, Types::Function{{Types::String()}, {Types::NoneType()}}
        });

        PrintASMNoIndent("format ELF64 executable 3");
        PrintASMNoIndent("segment readable executable");
        PrintASMNoIndent("entry start");

        PrintASMLabel("print:");
        PrintASM("mov     r8, 7378697629483820647");
        PrintASM("sub     rsp, 40");
        PrintASM("mov     BYTE [rsp+31], 10");
        PrintASM("lea     rcx, [rsp+30]");
        PrintASMLabel(".printL2:");
        PrintASM("mov     rax, rdi");
        PrintASM("mov     rsi, rcx");
        PrintASM("sub     rcx, 1");
        PrintASM("imul    r8");
        PrintASM("mov     rax, rdi");
        PrintASM("sar     rax, 63");
        PrintASM("sar     rdx, 2");
        PrintASM("sub     rdx, rax");
        PrintASM("lea     rax, [rdx+rdx*4]");
        PrintASM("add     rax, rax");
        PrintASM("sub     rdi, rax");
        PrintASM("add     edi, 48");
        PrintASM("mov     BYTE [rcx+1], dil");
        PrintASM("mov     rdi, rdx");
        PrintASM("test    rdx, rdx");
        PrintASM("jne     .printL2");
        PrintASM("lea     rdx, [rsp+32]");
        PrintASM("mov     edi, 1");
        PrintASM("sub     rdx, rsi");
        PrintASM("mov     rax, 1");
        PrintASM("syscall");
        PrintASM("add     rsp, 40");
        PrintASM("ret");

        PrintASMLabel("prints:");
        PrintASM("mov     rsi, rdi");
        PrintASM("add     rsi, 8");
        PrintASM("mov     rdx, [rdi]");
        PrintASM("mov     rax, 1");
        PrintASM("mov     rdi, 1");
        PrintASM("syscall");
        PrintASM("ret");


        // m_ast.GenerateFunctions(stack, m_code, 1);

        m_generateFunctions = true;
        m_ast.Visit(this);
        m_generateFunctions = false;

        PrintASMLabel("start:");
        PrintASM("mov rbp, rsp");

        m_ast.Visit(this);

        PrintASM("mov rdi, 0");
        PrintASM("mov rax, 60");
        PrintASM("syscall");

        PrintASMNoIndent("segment readable");
        for(const auto& [str, num] : m_stringLiterals)
        {
            PrintASM("STRLEN{} dq {}", num, str.length() + 1);  // store the length of the string as a 64-bit integer
            PrintASM("STR{} db \"{}\", 0xa", num, str);
        }
    }

    std::string_view GetCode() const
    {
        return m_code;
    }

    void Visit(AST& node) override;
    void Visit(UnaryExpression& node) override;
    void Visit(BinaryExpression& node) override;
    void Visit(NumberLiteral& node) override;
    void Visit(StringLiteral& node) override;
    void Visit(BoolLiteral& node) override;
    void Visit(VariableAccess& node) override;
    void Visit(VariableAssignment& node) override;
    void Visit(Block& node) override;
    void Visit(IfElse& node) override;
    void Visit(WhileLoop& node) override;
    void Visit(FnCall& node) override;
    void Visit(ExpressionStatement& node) override;
    void Visit(VariableDeclaration& node) override;
    void Visit(FnDeclaration& node) override;

private:
    AST m_ast;
    std::string m_code;
    std::back_insert_iterator<std::string> m_inserter = std::back_inserter(m_code);
    std::unordered_map<std::string_view, uint32_t> m_stringLiterals;
    int m_indent        = 0;
    uint32_t m_labelNr  = 0;
    uint32_t m_stringNr = 0;
    Stack m_stack;
    bool m_generateFunctions = true;

    template<typename... Args>
    constexpr void PrintASM(std::format_string<Args...> format_str, Args&&... args)
    {
        std::format_to(m_inserter, "        ");
        std::format_to(m_inserter, format_str, std::forward<Args>(args)...);
        std::format_to(m_inserter, "\n");
    }

    template<typename... Args>
    constexpr void PrintASMNoIndent(std::format_string<Args...> format_str, Args&&... args)
    {
        std::format_to(m_inserter, format_str, std::forward<Args>(args)...);
        std::format_to(m_inserter, "\n");
    }
    template<typename... Args>
    constexpr void PrintASMLabel(std::format_string<Args...> format_str, Args&&... args)
    {
        std::format_to(m_inserter, format_str, std::forward<Args>(args)...);
        std::format_to(m_inserter, "\n");
    }
    void PrintASMLabel(int labelNum)
    {
        std::format_to(m_inserter, ".L{}:\n", labelNum);
    }

    uint32_t GetLabelNum()
    {
        return m_labelNr++;
    }

    uint32_t GetStringNum()
    {
        return m_stringNr++;
    }
};
