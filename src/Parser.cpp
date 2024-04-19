#include "Parser.h"
#include <string>
#include <print>

void Parser::Parse()
{
    while(!IsEnd())
    {
        ASTNode* expr = ParseStatement();
        if(!expr)
        {
            std::println(stderr, "Skipping unexpected token {} at {}:{}", Peek().type, Peek().line, Peek().col);
            Advance();
            continue;
        }
        m_ast.statements.push_back(expr);
    }


    m_ast.Print(0);
}

ASTNode* Parser::ParseExpression()
{
    Expression* expr = static_cast<Expression*>(ParseEquality());
    if(expr)
        return expr;

    expr = static_cast<Expression*>(ParseWhile());
    if(expr)
        return expr;

    expr = static_cast<Expression*>(ParseIf());
    if(expr)
        return expr;

    expr = static_cast<Expression*>(ParseBlock());
    if(expr)
        return expr;

    return nullptr;
}

ASTNode* Parser::ParseStatement(bool reportSemicolonError)
{
    while(Peek().type == TokenType::SEMICOLON)
        Advance();

    Statement* statement = static_cast<Statement*>(ParseVariableDeclaration());
    if(statement)
        return statement;

    statement = static_cast<Statement*>(ParseExpressionStatement(reportSemicolonError));
    if(statement)
        return statement;


    return nullptr;
}

ASTNode* Parser::ParseWhile()
{
    if(Match(TokenType::WHILE))
    {
        Expression* condition = static_cast<Expression*>(ParseExpression());

        if(!condition)
        {
            std::println(stderr, "{}:{}: Expected expression for while condition", Peek().line, Peek().col);
            exit(1);
        }

        Block* body = static_cast<Block*>(ParseBlock());

        if(!body)
        {
            std::println(stderr, "{}:{}: Expected block expression for while body", Peek().line, Peek().col);
            exit(1);
        }

        return new WhileLoop{condition, body};
    }
    return nullptr;
}

ASTNode* Parser::ParseIf()
{
    if(Match(TokenType::IF))
    {
        Expression* condition = static_cast<Expression*>(ParseExpression());
        if(!condition)
        {
            std::println(stderr, "{}:{}: Expected expression for if condition", Peek().line, Peek().col);
            exit(1);
        }

        Block* body = static_cast<Block*>(ParseBlock());
        if(!body)
        {
            std::println(stderr, "{}:{}: Expected block expression for if body", Peek().line, Peek().col);
            exit(1);
        }
        Block* elseBlock = nullptr;
        if(Match(TokenType::ELSE))
        {
            elseBlock = static_cast<Block*>(ParseBlock());

            if(!elseBlock)
            {
                std::println(stderr, "{}:{}: Expected block expression for else body", Peek().line, Peek().col);
                exit(1);
            }
        }
        return new IfElse{condition, body, elseBlock};
    }
    return nullptr;
}

ASTNode* Parser::ParseBlock()
{
    if(Match(TokenType::LBRACE))
    {
        std::vector<Statement*> statements;
        Statement* statement = static_cast<Statement*>(ParseStatement(false));
        while(statement)
        {
            statements.push_back(statement);
            statement = static_cast<Statement*>(ParseStatement(false));
        }

        Expression* expr = static_cast<Expression*>(ParseExpression());

        if(!Match(TokenType::RBRACE))
        {
            std::println(stderr, "{}:{}: Expected closing brace for block expression", Peek().line, Peek().col);
            exit(1);
        }
        if(statements.empty())
            return new Block(expr);
        else
            return new Block(expr, std::move(statements));
    }
    return nullptr;
}

ASTNode* Parser::ParseEquality()
{
    Expression* expr = static_cast<Expression*>(ParseComparison());

    while(Match(TokenType::EQUAL) || Match(TokenType::NOT_EQUAL))
    {
        Op op             = Previous().type == TokenType::EQUAL ? Op::EQUAL : Op::NOT_EQUAL;
        Expression* right = static_cast<Expression*>(ParseComparison());
        expr              = new BinaryExpression{expr, op, right};
    }

    return expr;
}

ASTNode* Parser::ParseComparison()
{
    Expression* expr = static_cast<Expression*>(ParseTerm());

    while(Match(TokenType::LT) || Match(TokenType::GT) || Match(TokenType::LEQ) || Match(TokenType::GEQ))
    {
        Op op = Op::LT;
        switch(Previous().type)
        {
        case TokenType::LT:
            op = Op::LT;
            break;
        case TokenType::GT:
            op = Op::GT;
            break;
        case TokenType::LEQ:
            op = Op::LEQ;
            break;
        case TokenType::GEQ:
            op = Op::GEQ;
            break;
        }

        Expression* right = static_cast<Expression*>(ParseTerm());
        expr              = new BinaryExpression{expr, op, right};
    }

    return expr;
}

ASTNode* Parser::ParseTerm()
{
    Expression* expr = static_cast<Expression*>(ParseFactor());

    while(Match(TokenType::MINUS) || Match(TokenType::PLUS))
    {
        Op op             = Previous().type == TokenType::MINUS ? Op::MINUS : Op::PLUS;
        Expression* right = static_cast<Expression*>(ParseFactor());
        expr              = new BinaryExpression{expr, op, right};
    }

    return expr;
}

ASTNode* Parser::ParseFactor()
{
    Expression* expr = static_cast<Expression*>(ParseUnary());

    while(Match(TokenType::MUL) || Match(TokenType::DIV))
    {
        Op op             = Previous().type == TokenType::MUL ? Op::MUL : Op::DIV;
        Expression* right = static_cast<Expression*>(ParseUnary());
        expr              = new BinaryExpression{expr, op, right};
    }

    return expr;
}

ASTNode* Parser::ParseUnary()
{
    if(Match(TokenType::MINUS))
    {
        Expression* expr = static_cast<Expression*>(ParseUnary());
        return new UnaryExpression{Op::U_MINUS, expr};
    }
    if(Match(TokenType::NOT))
    {
        Expression* expr = static_cast<Expression*>(ParseUnary());
        return new UnaryExpression{Op::NOT, expr};
    }

    return ParseFnCall();
}


ASTNode* Parser::ParseFnCall()
{
    // TODO: instead of requiring IDENTIFIER we should ParsePrimary to be able to do stuff like f()() where f returns a function, etc...
    if(Match(TokenType::IDENTIFIER))
    {
        Token token           = Previous();
        std::string_view name = m_src.substr(token.start, token.len);
        if(Match(TokenType::LPAREN))
        {
            std::vector<Expression*> arguments;
            arguments.push_back(static_cast<Expression*>(ParseExpression()));
            while(Match(TokenType::COMMA))
            {
                arguments.push_back(static_cast<Expression*>(ParseExpression()));
            }
            if(Match(TokenType::RPAREN))
            {
                return new FnCall({name, std::move(arguments)});
            }
            else
            {
                std::println(stderr, "{}:{} Expected closing paranthesis for funtion arguments", Previous().line, Previous().col);
                exit(1);
            }
        }
    }
    return ParsePrimary();
}

ASTNode* Parser::ParsePrimary()
{
    if(Match(TokenType::NUMBER))
    {
        Token token = Previous();
        return new NumberLiteral{std::stoi(std::string(m_src.substr(token.start, token.len)))};
    }

    if(Match(TokenType::LPAREN))
    {
        Expression* expr = static_cast<Expression*>(ParseExpression());
        if(!Match(TokenType::RPAREN))
        {
            std::println(stderr, "{}:{} Expected ')'", Previous().line, Previous().col);
            exit(1);
        }
        return expr;
    }

    return nullptr;
}


ASTNode* Parser::ParseExpressionStatement(bool reportSemicolonError)
{
    size_t start     = m_current;
    Expression* expr = static_cast<Expression*>(ParseExpression());
    if(Match(TokenType::SEMICOLON))
    {
        return new ExpressionStatement{expr};
    }


    // We don't want to report this error if we are parsing a list of statements like in a block where the last one can be a normal expression (without the semicolon)
    if(reportSemicolonError)
    {
        std::println(stderr, "{}:{} Expected ';'", Previous().line, Previous().col);
        exit(1);
    }
    m_current = start;  // revert back to the start of the expression so that it can be parsed as an expression in another function
    return nullptr;
}

ASTNode* Parser::ParseVariableDeclaration()
{
    if(Match(TokenType::LET))
    {
        if(!Match(TokenType::IDENTIFIER))
        {
            std::println(stderr, "{}:{} Expected identifier after let", Previous().line, Previous().col);
            exit(1);
        }
        Token token           = Previous();
        std::string_view name = m_src.substr(token.start, token.len);
        if(!Match(TokenType::ASSIGN))
        {
            std::println(stderr, "{}:{} Expected '=' after variable declaration", Previous().line, Previous().col);
            exit(1);
        }
        Expression* expr = static_cast<Expression*>(ParseExpression());
        if(!Match(TokenType::SEMICOLON))
        {
            std::println(stderr, "{}:{} Expected ';'", Previous().line, Previous().col);
            exit(1);
        }
        return new VariableDeclaration{name, expr};
    }
    return nullptr;
}


bool Parser::IsEnd()
{
    return Peek().type == TokenType::END_FILE;
}

Token Parser::Peek()
{
    return m_tokens[m_current];
}
Token Parser::Advance()
{
    if(!IsEnd())
        m_current++;
    return Previous();
}

Token Parser::Previous()
{
    return m_tokens[m_current - 1];
}

bool Parser::Match(TokenType type)
{
    if(Peek().type == type)
    {
        Advance();
        return true;
    }
    return false;
}
