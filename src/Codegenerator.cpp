#include "Codegenerator.h"
#include "AST.h"
#include "Stack.h"
#include <cassert>

std::string_view GetSizedRegister(std::string_view reg, size_t size)
{
    if(size == 1)
    {
        if(reg == "rax")
            return "al";
        if(reg == "rbx")
            return "bl";
        if(reg == "rcx")
            return "cl";
        if(reg == "rdx")
            return "dl";
        if(reg == "rsi")
            return "sil";
        if(reg == "rdi")
            return "dil";
        if(reg == "rbp")
            return "bpl";
        if(reg == "rsp")
            return "spl";
        if(reg == "r8")
            return "r8b";
        if(reg == "r9")
            return "r9b";
        if(reg == "r10")
            return "r10b";
        if(reg == "r11")
            return "r11b";
        if(reg == "r12")
            return "r12b";
        if(reg == "r13")
            return "r13b";
        if(reg == "r14")
            return "r14b";
        if(reg == "r15")
            return "r15b";
    }
    else if(size == 2)
    {
        if(reg == "rax")
            return "ax";
        if(reg == "rbx")
            return "bx";
        if(reg == "rcx")
            return "cx";
        if(reg == "rdx")
            return "dx";
        if(reg == "rsi")
            return "si";
        if(reg == "rdi")
            return "di";
        if(reg == "rbp")
            return "bp";
        if(reg == "rsp")
            return "sp";
        if(reg == "r8")
            return "r8w";
        if(reg == "r9")
            return "r9w";
        if(reg == "r10")
            return "r10w";
        if(reg == "r11")
            return "r11w";
        if(reg == "r12")
            return "r12w";
        if(reg == "r13")
            return "r13w";
        if(reg == "r14")
            return "r14w";
        if(reg == "r15")
            return "r15w";
    }
    else if(size == 4)
    {
        if(reg == "rax")
            return "eax";
        if(reg == "rbx")
            return "ebx";
        if(reg == "rcx")
            return "ecx";
        if(reg == "rdx")
            return "edx";
        if(reg == "rsi")
            return "esi";
        if(reg == "rdi")
            return "edi";
        if(reg == "rbp")
            return "ebp";
        if(reg == "rsp")
            return "esp";
        if(reg == "r8")
            return "r8d";
        if(reg == "r9")
            return "r9d";
        if(reg == "r10")
            return "r10d";
        if(reg == "r11")
            return "r11d";
        if(reg == "r12")
            return "r12d";
        if(reg == "r13")
            return "r13d";
        if(reg == "r14")
            return "r14d";
        if(reg == "r15")
            return "r15d";
    }
    else if(size == 8)
    {
        return reg;
    }

    assert(false && "Unknown size for register");
}

std::string_view GetCallRegister(size_t i, size_t size)
{
    switch(i)
    {
    case 0:
        return GetSizedRegister("rdi", size);
    case 1:
        return GetSizedRegister("rsi", size);
    case 2:
        return GetSizedRegister("rdx", size);
    case 3:
        return GetSizedRegister("rcx", size);
    case 4:
        return GetSizedRegister("r8", size);
    case 5:
        return GetSizedRegister("r9", size);
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
        PrintASM("lea rax, [STRLEN{}]", it->second);
    }
    else
    {
        uint32_t label = GetStringNum();
        PrintASM("lea rax, [STRLEN{}]", label);
        m_stringLiterals[node.value] = label;
    }
}

void CodeGenerator::Visit(BoolLiteral& node)
{
    PrintASM(";  Bool Literal");
    PrintASM("mov rax, {}", node.value ? 1 : 0);  // keep rax here to make sure there is nothing in the upper bits
}

void CodeGenerator::Visit(VariableAccess& node)
{
    PrintASM(";  Variable Access: {}", node.name);
    Variable var = m_stack.Find(node.name, node.loc);
    if(node.index)
    {
        node.index->Accept(this);

        // TODO: this kind of adressing  only works with 1,2,4,8 sizes
        // baseoffset - 8 because for the size, this will always be positive since the size variable is included in the base offset
        if(std::holds_alternative<Types::String>(var.type))
        {
            PrintASM("mov rbx, [rbp - {}]", var.baseOffset);
            PrintASM("add rbx, 8");
            PrintASM("mov al, BYTE [rbx + rax]");  // TODO: clear the upper bits of rax probably
        }
        else
        {
            PrintASM("mov {}, [rbp - {} + rax * {}]", GetSizedRegister("rax", GetSize(var.type)), var.baseOffset - 8, GetSize(var.type));
        }
    }
    else
    {
        PrintASM("mov {}, [rbp - {}]", GetSizedRegister("rax", GetSize(var.type)), var.baseOffset);
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
        // baseoffset - 8 because for the size, this wilalways be positive since the size variable is included in the base offset
        if(std::holds_alternative<Types::String>(var.type))
        {
            std::println(stderr, "{}: Cannot modify string literal", node.loc);
        }
        else
        {
            PrintASM("mov [rbp - {} + rbx * {}], {}", var.baseOffset - 8, GetSize(var.type), GetSizedRegister("rax", GetSize(var.type)));
        }
    }
    else
    {
        PrintASM("mov [rbp - {}], {}", var.baseOffset, GetSizedRegister("rax", GetSize(var.type)));
    }
}

void CodeGenerator::Visit(WhileLoop& node)
{
    PrintASM("; while");
    uint32_t whileStartLabel = GetLabelNum();
    uint32_t whileEndLabel   = GetLabelNum();
    PrintASMLabel(whileStartLabel);
    node.condition->Accept(this);
    PrintASM("test al, al");
    PrintASM("jz .L{}", whileEndLabel);
    node.body->Accept(this);
    PrintASM("jmp .L{}", whileStartLabel);
    PrintASMLabel(whileEndLabel);
}
void CodeGenerator::Visit(IfElse& node)
{
    PrintASM("; if ");
    node.condition->Accept(this);
    PrintASM("test al, al");

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
    Variable fn            = m_stack.Find(node.name, node.loc);
    Types::Function fnType = std::get<Types::Function>(fn.type);
    for(int i = 0; i < node.arguments.size(); i++)
    {
        node.arguments[i]->Accept(this);

        Variable var = m_stack.PushVariable({"arg" + std::to_string(i), 8, fnType.parameters[i]});
        PrintASM("mov [rbp - {}], {}", var.baseOffset, GetSizedRegister("rax", GetSize(var.type)));
    }

    for(int i = 0; i < node.arguments.size(); i++)
    {
        Variable var = m_stack.Find("arg" + std::to_string(i), node.loc);
        PrintASM("mov {}, [rbp - {}]", GetCallRegister(i, GetSize(var.type)), var.baseOffset);
    }
    m_stack.PopFrame();

    PrintASM("call {}", fn.name);
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
    size_t size = GetSize(node.type) * node.arraySize;
    if(node.arraySize > 0)
    {
        size += 8;  // for the size of the array
    }
    Variable var = m_stack.PushVariable({node.name, size, node.type});
    PrintASM("sub rsp, {}", size);
    if(node.arraySize > 0)
    {
        PrintASM("mov QWORD [rbp - {}], {}", var.baseOffset, node.arraySize);
    }
    if(node.value)
    {
        node.value->Accept(this);
        PrintASM("mov [rbp - {}], {}", var.baseOffset, GetSizedRegister("rax", GetSize(node.type)));
    }
}


void CodeGenerator::Visit(FnDeclaration& node)
{
    if(!m_generateFunctions)
        return;

    m_generateFunctions = false;

    m_stack.PushVariable({node.name, 0, node.type});
    PrintASM("; fn declaration: {}", node.name);
    PrintASMLabel("{}:", node.name);
    PrintASM("enter 0, 0");  // enter = push rbp; mov rbp, rsp, it can also allocate space for local variables but we don't do that yet so use 0, 0
    m_stack.PushFrame(true);
    for(int i = 0; i < node.parameters.size(); i++)
    {
        Variable var = m_stack.PushVariable({node.parameters[i], GetSize(node.type.parameters[i]), node.type.parameters[i]});  // no array parameters for now

        PrintASM("sub rsp, {}", GetSize(var.type));
        PrintASM("mov [rbp - {}], {}", var.baseOffset, GetCallRegister(i, GetSize(var.type)));
    }
    node.body->Accept(this);
    m_stack.PopFrame();
    PrintASM("leave");  // leave = mov rsp, rbp; pop rbp
    PrintASM("ret");

    m_generateFunctions = true;
}
