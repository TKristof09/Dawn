#include "Codegenerator.h"
#include "AST.h"
#include "Stack.h"
#include <cassert>

static void PrintASMLabel(std::string& buffer, int labelNum)
{
    std::format_to(std::back_inserter(buffer), ".L{}:\n", labelNum);
}

static uint32_t GetLabelNum()
{
    static uint32_t labelNr = 0;
    return labelNr++;
}


void BinaryExpression::GenerateCode(Stack& stack, std::string& buffer, int indent)
{
    PrintASMIndented(buffer, indent, ";  Binary expression {}", op);
    left->GenerateCode(stack, buffer, indent + 1);
    PrintASMIndented(buffer, indent, "push rax");
    right->GenerateCode(stack, buffer, indent + 1);
    PrintASMIndented(buffer, indent, "mov rbx, rax");
    PrintASMIndented(buffer, indent, "pop rax");
    switch(op)
    {
    case Op::PLUS:
        PrintASMIndented(buffer, indent, "add rax, rbx");
        break;
    case Op::MINUS:
        PrintASMIndented(buffer, indent, "sub rax, rbx");
        break;
    case Op::MUL:
        PrintASMIndented(buffer, indent, "imul rax, rbx");
        break;
    case Op::DIV:
        PrintASMIndented(buffer, indent, "xor rdx, rdx");
        PrintASMIndented(buffer, indent, "div rbx");  // TODO: this also gives remainder, so look into that in the future
        break;

    // TODO: the register needs to be cleaned if we want to use the whole register (eg. casting the result into an int instead of bool) since `set__` only sets the first 8bits of it
    case Op::EQUAL:
        PrintASMIndented(buffer, indent, "cmp rax, rbx");
        PrintASMIndented(buffer, indent, "sete al");
        break;
    case Op::NOT_EQUAL:
        PrintASMIndented(buffer, indent, "cmp rax, rbx");
        PrintASMIndented(buffer, indent, "setne al");
        break;
    case Op::GT:
        PrintASMIndented(buffer, indent, "cmp rax, rbx");
        PrintASMIndented(buffer, indent, "seta al");
        break;
    case Op::LT:
        PrintASMIndented(buffer, indent, "cmp rax, rbx");
        PrintASMIndented(buffer, indent, "setb al");
        break;
    case Op::GEQ:
        PrintASMIndented(buffer, indent, "cmp rax, rbx");
        PrintASMIndented(buffer, indent, "setae al");
        break;
    case Op::LEQ:
        PrintASMIndented(buffer, indent, "cmp rax, rbx");
        PrintASMIndented(buffer, indent, "setbe al");
        break;
    }
}
void UnaryExpression::GenerateCode(Stack& stack, std::string& buffer, int indent)
{
    expr->GenerateCode(stack, buffer, indent + 1);
    switch(op)
    {
    case Op::U_MINUS:
        PrintASMIndented(buffer, indent, "neg rax");
        break;
    // TODO: this is a bitwise not, so not(0) = 255, in the future we probably want to make it into a logical not
    //      it also doesn't set the EFLAGS for conditional insructions
    // cland with -O0 does
    // xor rax, -1
    // and rax, 1
    case Op::NOT:
        PrintASMIndented(buffer, indent, "not rax");
        break;
    }
}

void NumberLiteral::GenerateCode(Stack& stack, std::string& buffer, int indent)
{
    PrintASMIndented(buffer, indent, ";  Number Literal");
    PrintASMIndented(buffer, indent, "mov rax, {}", value);
}

void VariableAccess::GenerateCode(Stack& stack, std::string& buffer, int indent)
{
    PrintASMIndented(buffer, indent, ";  Variable Access: {}", name);
    Variable var = stack.Find(name, loc);
    if(index)
    {
        index->GenerateCode(stack, buffer, indent + 1);
        PrintASMIndented(buffer, indent, "push rax");
    }
    if(index)
    {
        PrintASMIndented(buffer, indent, "pop rbx");
        PrintASMIndented(buffer, indent, "mov rax, [rbp - {} + rbx * {}]", var.baseOffset, var.baseSize);
    }
    else
    {
        PrintASMIndented(buffer, indent, "mov rax, [rbp - {}]", var.baseOffset);
    }
}

void VariableAssignment::GenerateCode(Stack& stack, std::string& buffer, int indent)
{
    PrintASMIndented(buffer, indent, ";  Variable Assignment: {}", name);
    if(index)
    {
        index->GenerateCode(stack, buffer, indent + 1);
        PrintASMIndented(buffer, indent, "push rax");
    }
    value->GenerateCode(stack, buffer, indent + 1);
    Variable var = stack.Find(name, loc);


    if(index)
    {
        PrintASMIndented(buffer, indent, "pop rbx");
        PrintASMIndented(buffer, indent, "mov [rbp - {} + rbx * {}], rax", var.baseOffset, var.baseSize);
    }
    else
    {
        PrintASMIndented(buffer, indent, "mov [rbp - {}], rax", var.baseOffset);
    }
}

void WhileLoop::GenerateCode(Stack& stack, std::string& buffer, int indent)
{
    PrintASMIndented(buffer, indent, "; while");
    uint32_t whileStartLabel = GetLabelNum();
    uint32_t whileEndLabel   = GetLabelNum();
    PrintASMLabel(buffer, whileStartLabel);
    condition->GenerateCode(stack, buffer, indent + 1);
    PrintASMIndented(buffer, indent, "test rax, rax");
    PrintASMIndented(buffer, indent, "jz .L{}", whileEndLabel);
    body->GenerateCode(stack, buffer, indent);
    PrintASMIndented(buffer, indent, "jmp .L{}", whileStartLabel);
    PrintASMLabel(buffer, whileEndLabel);
}
void IfElse::GenerateCode(Stack& stack, std::string& buffer, int indent)
{
    PrintASMIndented(buffer, indent, "; if ");
    condition->GenerateCode(stack, buffer, indent + 1);
    PrintASMIndented(buffer, indent, "test rax, rax");

    uint32_t ifEndLabel   = GetLabelNum();
    uint32_t elseEndLabel = 0;

    PrintASMIndented(buffer, indent, "jz .L{}", ifEndLabel);
    body->GenerateCode(stack, buffer, indent);

    if(elseBlock)
    {
        elseEndLabel = GetLabelNum();
        PrintASMIndented(buffer, indent, "jmp .L{}", elseEndLabel);
    }

    PrintASMLabel(buffer, ifEndLabel);
    if(elseBlock)
    {
        elseBlock->GenerateCode(stack, buffer, indent);
        PrintASMLabel(buffer, elseEndLabel);
    }
}
void Block::GenerateCode(Stack& stack, std::string& buffer, int indent)
{
    stack.PushFrame();

    for(auto* statement : statements)
        statement->GenerateCode(stack, buffer, indent);
    if(expr)
        expr->GenerateCode(stack, buffer, indent + 1);

    stack.PopFrame();
}

void FnCall::GenerateCode(Stack& stack, std::string& buffer, int indent)
{
    PrintASMIndented(buffer, indent, "; fn call");

    // TODO: handle register allocations correctly, for now this will just keep overwriting rax
    for(auto* argument : arguments)
        argument->GenerateCode(stack, buffer, indent + 1);

    PrintASMIndented(buffer, indent, "mov rdi, rax");
    PrintASMIndented(buffer, indent, "call {}", name);
}

void ExpressionStatement::GenerateCode(Stack& stack, std::string& buffer, int indent)
{
    expr->GenerateCode(stack, buffer, indent);
}

void VariableDeclaration::GenerateCode(Stack& stack, std::string& buffer, int indent)
{
    PrintASMIndented(buffer, indent, "; variable declaration: {}", name);
    // TODO: stack needs to be aligned ta 16 bytes
    size_t size  = baseSize * arraySize;
    Variable var = stack.PushVariable({name, size, baseSize});
    PrintASMIndented(buffer, indent, "sub rsp, {}", size);
    if(value)
    {
        value->GenerateCode(stack, buffer, indent + 1);
        PrintASMIndented(buffer, indent, "mov [rbp + {}], rax", var.baseOffset);
    }
}
