#include "Codegenerator.h"
#include "AST.h"
#include "Stack.h"
#include <cassert>

std::string_view GetCallRegister(size_t i)
{
    switch(i)
    {
    case 0:
        return "rdi";
    case 1:
        return "rsi";
    case 2:
        return "rdx";
    case 3:
        return "rcx";
    case 4:
        return "r8";
    case 5:
        return "r9";
    default:
        assert(false && "Only 6 arguments go in registers, rest should go on the stack");
    }
}

void CodeGenerator::Visit(AST& node)
{
    for(auto* statement : node.statements)
        statement->Accept(this);
}

void CodeGenerator::Visit(BinaryExpression& node)
{
    PrintASM(";  Binary expression {}", node.op);
    node.left->Accept(this);
    PrintASM("push rax");
    node.right->Accept(this);
    PrintASM("mov rbx, rax");
    PrintASM("pop rax");
    switch(node.op)
    {
    case Op::PLUS:
        PrintASM("add rax, rbx");
        break;
    case Op::MINUS:
        PrintASM("sub rax, rbx");
        break;
    case Op::MUL:
        PrintASM("imul rax, rbx");
        break;
    case Op::DIV:
        PrintASM("xor rdx, rdx");
        PrintASM("div rbx");  // TODO: this also gives remainder, so look into that in the future
        break;
    case Op::LSH:
        PrintASM("mov rcx, rbx");
        PrintASM("shl rax, cl");
        break;
    case Op::RSH:
        PrintASM("mov cl, bl");
        PrintASM("sar rax, cl");
        break;
    case Op::BAND:
        PrintASM("and rax, rbx");
        break;
    case Op::BOR:
        PrintASM("or rax, rbx");
        break;

    // TODO: the register needs to be cleaned if we want to use the whole register (eg. casting the result into an int instead of bool) since `set__` only sets the first 8bits of it
    case Op::EQUAL:
        PrintASM("cmp rax, rbx");
        PrintASM("sete al");
        break;
    case Op::NOT_EQUAL:
        PrintASM("cmp rax, rbx");
        PrintASM("setne al");
        break;
    case Op::GT:
        PrintASM("cmp rax, rbx");
        PrintASM("seta al");
        break;
    case Op::LT:
        PrintASM("cmp rax, rbx");
        PrintASM("setb al");
        break;
    case Op::GEQ:
        PrintASM("cmp rax, rbx");
        PrintASM("setae al");
        break;
    case Op::LEQ:
        PrintASM("cmp rax, rbx");
        PrintASM("setbe al");
        break;
    }
}
void CodeGenerator::Visit(UnaryExpression& node)
{
    node.expr->Accept(this);
    switch(node.op)
    {
    case Op::U_MINUS:
        PrintASM("neg rax");
        break;
    // TODO: this is a bitwise not, so not(0) = 255, in the future we probably want to make it into a logical not
    //      it also doesn't set the EFLAGS for conditional insructions
    // cland with -O0 does
    // xor rax, -1
    // and rax, 1
    case Op::NOT:
        PrintASM("not rax");
        break;
    }
}

void CodeGenerator::Visit(NumberLiteral& node)
{
    PrintASM(";  Number Literal");
    PrintASM("mov rax, {}", node.value);
}

void CodeGenerator::Visit(StringLiteral& node)
{
    PrintASM(";  String Literal");
    if(auto it = m_stringLiterals.find(node.value); it != m_stringLiterals.end())
    {
        PrintASM("mov rax, STRLEN{}", it->second);
    }
    else
    {
        uint32_t label = GetStringNum();
        PrintASM("mov rax, STRLEN{}", label);
        m_stringLiterals[node.value] = label;
    }
}

void CodeGenerator::Visit(VariableAccess& node)
{
    PrintASM(";  Variable Access: {}", node.name);
    Variable var = m_stack.Find(node.name, node.loc);
    if(node.index)
    {
        node.index->Accept(this);

        // TODO: this kind of adressing  only works with 1,2,4,8 sizes
        PrintASM("mov rax, [rbp - {} + rax * {}]", var.baseOffset, var.baseSize);
    }
    else
    {
        PrintASM("mov rax, [rbp - {}]", var.baseOffset);
    }
}

void CodeGenerator::Visit(VariableAssignment& node)
{
    PrintASM(";  Variable Assignment: {}", node.name);
    if(node.index)
    {
        node.index->Accept(this);
        PrintASM("push rax");
    }
    node.value->Accept(this);
    Variable var = m_stack.Find(node.name, node.loc);


    if(node.index)
    {
        PrintASM("pop rbx");

        // TODO: this kind of adressing  only works with 1,2,4,8 sizes
        PrintASM("mov [rbp - {} + rbx * {}], rax", var.baseOffset, var.baseSize);
    }
    else
    {
        PrintASM("mov [rbp - {}], rax", var.baseOffset);
    }
}

void CodeGenerator::Visit(WhileLoop& node)
{
    PrintASM("; while");
    uint32_t whileStartLabel = GetLabelNum();
    uint32_t whileEndLabel   = GetLabelNum();
    PrintASMLabel(whileStartLabel);
    node.condition->Accept(this);
    PrintASM("test rax, rax");
    PrintASM("jz .L{}", whileEndLabel);
    node.body->Accept(this);
    PrintASM("jmp .L{}", whileStartLabel);
    PrintASMLabel(whileEndLabel);
}
void CodeGenerator::Visit(IfElse& node)
{
    PrintASM("; if ");
    node.condition->Accept(this);
    PrintASM("test rax, rax");

    uint32_t ifEndLabel   = GetLabelNum();
    uint32_t elseEndLabel = 0;

    PrintASM("jz .L{}", ifEndLabel);
    node.body->Accept(this);

    if(node.elseBlock)
    {
        elseEndLabel = GetLabelNum();
        PrintASM("jmp .L{}", elseEndLabel);
    }

    PrintASMLabel(ifEndLabel);
    if(node.elseBlock)
    {
        node.elseBlock->Accept(this);
        PrintASMLabel(elseEndLabel);
    }
}
void CodeGenerator::Visit(Block& node)
{
    m_stack.PushFrame();

    for(auto* statement : node.statements)
        statement->Accept(this);
    if(node.expr)
        node.expr->Accept(this);

    m_stack.PopFrame();
}

void CodeGenerator::Visit(FnCall& node)
{
    PrintASM("; fn call");


    m_stack.PushFrame();
    for(int i = 0; i < node.arguments.size(); i++)
    {
        node.arguments[i]->Accept(this);

        Variable var = m_stack.PushVariable({"arg" + std::to_string(i), 8, 8});
        PrintASM("mov [rbp - {}], rax", var.baseOffset);
    }

    for(int i = 0; i < node.arguments.size(); i++)
    {
        PrintASM("mov {}, [rbp - {}]", GetCallRegister(i), m_stack.Find("arg" + std::to_string(i), node.loc).baseOffset);
    }
    m_stack.PopFrame();

    Variable var = m_stack.Find(node.name, node.loc);
    PrintASM("call {}", var.name);
}

void CodeGenerator::Visit(ExpressionStatement& node)
{
    if(m_generateFunctions)
        return;

    node.expr->Accept(this);
}

void CodeGenerator::Visit(VariableDeclaration& node)
{
    if(m_generateFunctions)
        return;

    PrintASM("; variable declaration: {}", node.name);
    // TODO: stack needs to be aligned ta 16 bytes
    size_t size  = node.baseSize * node.arraySize;
    Variable var = m_stack.PushVariable({node.name, size, node.baseSize});
    PrintASM("sub rsp, {}", size);
    if(node.value)
    {
        node.value->Accept(this);
        PrintASM("mov [rbp - {}], rax", var.baseOffset);
    }
}


void CodeGenerator::Visit(FnDeclaration& node)
{
    if(!m_generateFunctions)
        return;

    m_generateFunctions = false;

    m_stack.PushVariable({node.name, 0, 0});
    PrintASM("; fn declaration: {}", node.name);
    PrintASMLabel("{}:", node.name);
    PrintASM("enter 0, 0");  // enter = push rbp; mov rbp, rsp, it can also allocate space for local variables but we don't do that yet so use 0, 0
    m_stack.PushFrame(true);
    for(int i = 0; i < node.parameters.size(); i++)
    {
        size_t size  = node.parameters[i].second;
        Variable var = m_stack.PushVariable({node.parameters[i].first, size, size});  // no array parameters for now

        PrintASM("sub rsp, {}", size);
        PrintASM("mov [rbp - {}], {}", var.baseOffset, GetCallRegister(i));
    }
    node.body->Accept(this);
    m_stack.PopFrame();
    PrintASM("leave");  // leave = mov rsp, rbp; pop rbp
    PrintASM("ret");

    m_generateFunctions = true;
}
