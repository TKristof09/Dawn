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
        for(auto& expr : m_ast.expressions)
            std::visit(CodegenVisitor{&m_code}, *expr);
        std::format_to(std::back_inserter(m_code), "mov rdi, rax\n");
        std::format_to(std::back_inserter(m_code), "mov rax, 60\n");
        std::format_to(std::back_inserter(m_code), "syscall\n");

        std::format_to(std::back_inserter(m_code), "segment readable writable\n");
    }

    std::string_view GetCode() const { return m_code; }

private:
    struct CodegenVisitor
    {
        CodegenVisitor(std::string* code) : m_code(code) {}
        void operator()(const BinaryExpression& expr)
        {
            std::format_to(std::back_inserter(*m_code), ";  Binary expression {}\n", expr.op);
            std::format_to(std::back_inserter(*m_code), "push rsp\n", expr.op);
            std::visit(CodegenVisitor{m_code}, *expr.left);
            std::format_to(std::back_inserter(*m_code), "push rax\n");
            std::visit(CodegenVisitor{m_code}, *expr.right);
            std::format_to(std::back_inserter(*m_code), "mov rbx, rax\n");
            std::format_to(std::back_inserter(*m_code), "pop rax\n");
            switch(expr.op)
            {
            case Op::PLUS:
                std::format_to(std::back_inserter(*m_code), "add rax, rbx\n");
                break;
            case Op::MINUS:
                std::format_to(std::back_inserter(*m_code), "sub rax, rbx\n");
                break;
            case Op::MUL:
                std::format_to(std::back_inserter(*m_code), "imul rax, rbx\n");
                break;
            }

            std::format_to(std::back_inserter(*m_code), "pop rbp\n", expr.op);
        }

        void operator()(const UnaryExpression& expr)
        {
            std::visit(CodegenVisitor{m_code}, *expr.expr);
        }

        void operator()(const NumberLiteral& expr)
        {
            std::format_to(std::back_inserter(*m_code), ";  Number Literal\n");
            std::format_to(std::back_inserter(*m_code), "mov rax, {}\n", expr.value);
        }

    private:
        std::string* m_code;
    };


    AST m_ast;
    std::string m_code;
};
