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
            std::visit(CodegenVisitor{&m_code, 1}, *expr);
        std::format_to(std::back_inserter(m_code), "mov rdi, rax\n");
        std::format_to(std::back_inserter(m_code), "mov rax, 60\n");
        std::format_to(std::back_inserter(m_code), "syscall\n");

        std::format_to(std::back_inserter(m_code), "segment readable writable\n");
    }

    std::string_view GetCode() const { return m_code; }

private:
    static uint32_t GetLabelNum()
    {
        static uint32_t labelNr = 0;
        return labelNr++;
    }

    struct CodegenVisitor
    {
        CodegenVisitor(std::string* code, uint8_t indent) : m_code(code), m_indent(indent) {}
        void operator()(const BinaryExpression& expr)
        {
            PrintASMIndented(m_code, m_indent, ";  Binary expression {}", expr.op);
            PrintASMIndented(m_code, m_indent, "push rsp", expr.op);
            visit(expr.left);
            PrintASMIndented(m_code, m_indent, "push rax");
            visit(expr.right);
            PrintASMIndented(m_code, m_indent, "mov rbx, rax");
            PrintASMIndented(m_code, m_indent, "pop rax");
            switch(expr.op)
            {
            case Op::PLUS:
                PrintASMIndented(m_code, m_indent, "add rax, rbx");
                break;
            case Op::MINUS:
                PrintASMIndented(m_code, m_indent, "sub rax, rbx");
                break;
            case Op::MUL:
                PrintASMIndented(m_code, m_indent, "imul rax, rbx");
                break;

            // TODO: the register needs to be cleaned if we want to use the whole register (eg. casting the result into an int instead of bool) since `set__` only sets the first 8bits of it
            case Op::EQUAL:
                PrintASMIndented(m_code, m_indent, "cmp rax, rbx");
                PrintASMIndented(m_code, m_indent, "sete al");
                break;
            case Op::NOT_EQUAL:
                PrintASMIndented(m_code, m_indent, "cmp rax, rbx");
                PrintASMIndented(m_code, m_indent, "setne al");
                break;
            case Op::GT:
                PrintASMIndented(m_code, m_indent, "cmp rax, rbx");
                PrintASMIndented(m_code, m_indent, "seta al");
                break;
            case Op::LT:
                PrintASMIndented(m_code, m_indent, "cmp rax, rbx");
                PrintASMIndented(m_code, m_indent, "setb al");
                break;
            case Op::GEQ:
                PrintASMIndented(m_code, m_indent, "cmp rax, rbx");
                PrintASMIndented(m_code, m_indent, "setae al");
                break;
            case Op::LEQ:
                PrintASMIndented(m_code, m_indent, "cmp rax, rbx");
                PrintASMIndented(m_code, m_indent, "setbe al");
                break;
            }

            PrintASMIndented(m_code, m_indent, "pop rbp", expr.op);
        }

        void operator()(const UnaryExpression& expr)
        {
            visit(expr.expr);
            switch(expr.op)
            {
            case Op::U_MINUS:
                PrintASMIndented(m_code, m_indent, "neg rax");
                break;
            // TODO: this is a bitwise not, so not(0) = 255, in the future we probably want to make it into a logical not
            //      it also doesn't set the EFLAGS for conditional insructions
            // cland with -O0 does
            // xor rax, -1
            // and rax, 1
            case Op::NOT:
                PrintASMIndented(m_code, m_indent, "not rax");
                break;
            }
        }

        void operator()(const NumberLiteral& expr)
        {
            PrintASMIndented(m_code, m_indent, ";  Number Literal");
            PrintASMIndented(m_code, m_indent, "mov rax, {}", expr.value);
        }

        void operator()(const If& expr)
        {
            PrintASMIndented(m_code, m_indent, "; if ");
            visit(expr.condition);
            PrintASMIndented(m_code, m_indent, "test rax, rax");

            uint32_t ifEndLabel   = CodeGenerator::GetLabelNum();
            uint32_t elseEndLabel = 0;

            PrintASMIndented(m_code, m_indent, "jz .L{}", ifEndLabel);
            visit(expr.body);

            if(expr.elseBlock)
            {
                elseEndLabel = CodeGenerator::GetLabelNum();
                PrintASMIndented(m_code, m_indent, "jmp .L{}", elseEndLabel);
            }

            PrintASMIndented(m_code, m_indent, ".L{}:", ifEndLabel);
            if(expr.elseBlock)
            {
                visit(expr.elseBlock);
                PrintASMIndented(m_code, m_indent, ".L{}:", elseEndLabel);
            }
        }
        void operator()(const Block& expr)
        {
            visit(expr.body);
        }

    private:
        template<typename... Args>
        static constexpr void PrintASMIndented(std::string* buffer, int indent, std::format_string<Args...> format_str, Args&&... args)
        {
            auto inserter = std::back_inserter(*buffer);
            std::format_to(inserter, "{:{}}", "", indent * 4);
            std::format_to(inserter, format_str, std::forward<Args>(args)...);
            std::format_to(inserter, "\n");
        }

        void visit(ASTNode* expr)
        {
            std::visit(CodegenVisitor{m_code, static_cast<uint8_t>(m_indent + 1)}, *expr);
        }
        uint8_t m_indent;
        std::string* m_code;
    };


    AST m_ast;
    std::string m_code;
};
