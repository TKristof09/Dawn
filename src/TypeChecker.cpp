#include "TypeChecker.h"

using namespace Types;

void TypeChecker::Visit(AST& node)
{
    // collect all top level functions, to not need forward declarations like in C
    m_findFunctions = true;
    for(auto* statement : node.statements)
        statement->Accept(this);


    m_findFunctions = false;
    for(auto* statement : node.statements)
        statement->Accept(this);
}

void TypeChecker::Visit(UnaryExpression& node)
{
    node.expr->Accept(this);
    if(!std::holds_alternative<Int>(m_currentType))
    {
        std::println(stderr, "{}: Unary expression must be of type int, got {}", node.expr->loc, m_currentType);
        m_error = true;
    }
    if(node.op == Op::NOT)
    {
        m_currentType = Bool();
    }
    else if(node.op == Op::U_MINUS)
    {
        m_currentType = Int();
    }
}

void TypeChecker::Visit(BinaryExpression& node)
{
    node.left->Accept(this);
    if(!std::holds_alternative<Int>(m_currentType))
    {
        std::println(stderr, "{}: Binary expression operands must be of type int, got {}", node.left->loc, m_currentType);
        m_error = true;
    }
    node.right->Accept(this);
    if(!std::holds_alternative<Int>(m_currentType))
    {
        std::println(stderr, "{}: Unary expression operands must be of type int, got {}", node.right->loc, m_currentType);
        m_error = true;
    }

    switch(node.op)
    {
    case Op::PLUS:
    case Op::MINUS:
    case Op::MUL:
    case Op::DIV:
    case Op::LSH:
    case Op::RSH:
    case Op::BAND:
    case Op::BOR:
        m_currentType = Int();
        break;

    case Op::LT:
    case Op::GT:
    case Op::LEQ:
    case Op::GEQ:
    case Op::EQUAL:
    case Op::NOT_EQUAL:
        m_currentType = Bool();
        break;
    }
}

void TypeChecker::Visit(NumberLiteral& node)
{
    m_currentType = Int();
}

void TypeChecker::Visit(StringLiteral& node)
{
    m_currentType = String();
}
void TypeChecker::Visit(BoolLiteral& node)
{
    m_currentType = Bool();
}

void TypeChecker::Visit(VariableAccess& node)
{
    if(node.index)
    {
        if(std::holds_alternative<Array>(m_stack.Find(node.name, node.loc)))
        {
            m_currentType = std::get<Array>(m_stack.Find(node.name, node.loc)).type[0];
        }
        else if(std::holds_alternative<String>(m_stack.Find(node.name, node.loc)))
        {
            m_currentType = Int();
        }
        else
        {
            std::println(stderr, "{}: Expected array or string type for variable access but got {}", node.loc, m_stack.Find(node.name, node.loc));
            m_error = true;
        }
    }
    else
    {
        m_currentType = m_stack.Find(node.name, node.loc);
    }
}

void TypeChecker::Visit(VariableAssignment& node)
{
    node.value->Accept(this);

    Type t;
    if(node.index)
    {
        t = std::get<Array>(m_stack.Find(node.name, node.loc)).type[0];
    }
    else
    {
        t = m_stack.Find(node.name, node.loc);
    }
    if(m_currentType.index() != t.index())
    {
        std::println(stderr, "{}: Expected {} type for variable assignment but got {}", node.value->loc, t, m_currentType);
        m_error = true;
    }
}

void TypeChecker::Visit(Block& node)
{
    for(auto* statement : node.statements)
        statement->Accept(this);
    if(node.expr)
        node.expr->Accept(this);
}

void TypeChecker::Visit(IfElse& node)
{
    node.condition->Accept(this);
    if(std::holds_alternative<Bool>(m_currentType) == false)
    {
        std::println(stderr, "{}: Expected bool type for if condition but got {}", node.condition->loc, m_currentType);
        m_error = true;
    }
    node.body->Accept(this);
    if(node.elseBlock)
    {
        Type bodyType = m_currentType;
        node.elseBlock->Accept(this);

        if(bodyType.index() != m_currentType.index())
        {
            std::println(stderr, "{}: The body of both branches of an if must have the same type. Type of if branch: {}, type of else branch: {}", node.body->loc, bodyType, m_currentType);
            m_error = true;
        }
    }
}

void TypeChecker::Visit(WhileLoop& node)
{
    node.condition->Accept(this);
    if(std::holds_alternative<Bool>(m_currentType) == false)
    {
        std::println(stderr, "{}: Expected bool type for while condition but got {}", node.condition->loc, m_currentType);
        m_error = true;
    }
    node.body->Accept(this);
    if(std::holds_alternative<NoneType>(m_currentType) == false)
    {
        std::println(stderr, "{}: While loop's body must have NoneType, but got {}", node.body->loc, m_currentType);
        m_error = true;
    }
}

void TypeChecker::Visit(FnCall& node)
{
    Function funType;
    try
    {
        funType = std::get<Function>(m_stack.Find(node.name, node.loc));
    }
    catch(std::bad_variant_access&)
    {
        std::println(stderr, "{}: Expected function type for function call but got {}", node.loc, m_currentType);
        m_error = true;
    }

    if(funType.parameters.size() != node.arguments.size())
    {
        std::println(stderr, "{}: Expected {} arguments for function call but got {}", node.loc, funType.parameters.size(), node.arguments.size());
        m_error = true;
    }

    for(int i = 0; i < node.arguments.size() && i < funType.parameters.size(); i++)
    {
        auto* arg = node.arguments[i];
        arg->Accept(this);
        if(m_currentType.index() != funType.parameters[i].index())
        {
            std::println(stderr, "{}: Expected {} type for function argument but got {}", arg->loc, funType.parameters[i], m_currentType);
            m_error = true;
        }
    }
    m_currentType = m_stack.Find(node.name, node.loc);
}

void TypeChecker::Visit(ExpressionStatement& node)
{
    if(m_findFunctions)
    {
        return;
    }
    node.expr->Accept(this);
    m_currentType = NoneType();  // expression statements just ignore the type of the expression
}

void TypeChecker::Visit(VariableDeclaration& node)
{
    if(m_findFunctions)
    {
        return;
    }

    if(node.value)
    {
        node.value->Accept(this);

        if(m_currentType.index() != node.type.index())
        {
            std::println(stderr, "{}: Expected {} type for variable initialisation but got {}", node.value->loc, node.type, m_currentType);
            m_error = true;
        }
    }
    m_stack.Push(node.name, node.type);

    m_currentType = NoneType();
}

void TypeChecker::Visit(FnDeclaration& node)
{
    m_stack.Push(node.name, node.type);
    if(m_findFunctions)
    {
        return;
    }

    for(int i = 0; i < node.type.parameters.size(); i++)
    {
        m_stack.Push(node.parameters[i], node.type.parameters[i]);
    }
    node.body->Accept(this);

    if(m_currentType.index() != node.type.returnType[0].index())
    {
        Location loc = node.loc;
        if(node.body->expr)
        {
            loc = node.body->expr->loc;
        }
        else if(!node.body->statements.empty())
        {
            loc = node.body->statements.back()->loc;
        }
        std::println(stderr, "{}: Expected {} for function return value type but got {}", loc, node.type.returnType[0], m_currentType);
        m_error = true;
    }


    m_currentType = NoneType();
}
