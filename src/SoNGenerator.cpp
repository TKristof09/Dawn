#include "SoNGenerator.h"


void SoNGenerator::Visit(AST& node)
{
    for(auto& statement : node.statements)
    {
        statement->Accept(this);
    }
    m_stopNode.AddInput(m_currentNode);

    std::println("digraph G {{");
    m_startNode.Print();
    m_scopeNode.Print();
    std::println("}}");
}
void SoNGenerator::Visit(UnaryExpression& node)
{
}
void SoNGenerator::Visit(BinaryExpression& node)
{
    node.left->Accept(this);
    Node* left = m_currentNode;
    node.right->Accept(this);
    Node* right = m_currentNode;
    switch(node.op)
    {
    case Op::Plus:
        m_currentNode = new AddNode(left, right);
        break;
    case Op::Minus:
        m_currentNode = new SubNode(left, right);
        break;
    case Op::Mul:
        m_currentNode = new MulNode(left, right);
        break;
    case Op::Div:
        m_currentNode = new DivNode(left, right);
        break;
    case Op::Lsh:
        m_currentNode = new LshNode(left, right);
        break;
    case Op::Rsh:
        m_currentNode = new RshNode(left, right);
        break;
    case Op::BAnd:
        m_currentNode = new AndNode(left, right);
        break;
    case Op::BOr:
        m_currentNode = new OrNode(left, right);
        break;
    case Op::Eq:
        m_currentNode = new EqNode(left, right);
        break;
    case Op::NotEq:
        m_currentNode = new NotEqNode(left, right);
        break;
    case Op::LT:
        m_currentNode = new LtNode(left, right);
        break;
    case Op::GT:
        m_currentNode = new GtNode(left, right);
        break;
    case Op::LEq:
        m_currentNode = new LeqNode(left, right);
        break;
    case Op::GEq:
        m_currentNode = new GeqNode(left, right);
        break;
    }
}
void SoNGenerator::Visit(NumberLiteral& node)
{
    m_currentNode = new ConstantNode(&m_startNode, IntType(uint64_t(node.value)));
}
void SoNGenerator::Visit(StringLiteral& node)
{
}
void SoNGenerator::Visit(BoolLiteral& node)
{
}
void SoNGenerator::Visit(VariableDeclaration& node)
{
    if(node.value)
    {
        node.value->Accept(this);
        m_scopeNode.PushNode(node.name, m_currentNode);
    }
    else
    {
        m_scopeNode.PushNode(node.name, nullptr);
    }
}
void SoNGenerator::Visit(VariableAccess& node)
{
    m_currentNode = m_scopeNode.Find(node.name);
    std::println("VariableAccess: {}, id={}", node.name, m_currentNode->GetId());
}
void SoNGenerator::Visit(VariableAssignment& node)
{
    node.value->Accept(this);
    m_scopeNode.Update(node.name, m_currentNode);
}
void SoNGenerator::Visit(Block& node)
{
    m_scopeNode.PushFrame();
    for(auto& statement : node.statements)
    {
        statement->Accept(this);
    }
    if(node.expr)
    {
        node.expr->Accept(this);
    }

    m_scopeNode.PopFrame();
}
void SoNGenerator::Visit(IfElse& node)
{
}
void SoNGenerator::Visit(WhileLoop& node)
{
}
void SoNGenerator::Visit(FnCall& node)
{
}
void SoNGenerator::Visit(ExpressionStatement& node)
{
    node.expr->Accept(this);
}
void SoNGenerator::Visit(FnDeclaration& node)
{
}
