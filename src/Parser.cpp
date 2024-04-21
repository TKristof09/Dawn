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
            std::println(stderr, "{}: Skipping unexpected token {}", Peek().loc, Peek().type);
            Advance();
            continue;
        }
        m_ast.statements.push_back(expr);
    }


    m_ast.Print(0);
}

ASTNode* Parser::ParseExpression()
{
    Expression* expr = static_cast<Expression*>(ParseAssignment());
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
        Location loc = Previous().loc;

        Expression* condition = static_cast<Expression*>(ParseExpression());

        if(!condition)
        {
            std::println(stderr, "{}:: Expected expression for while condition", Peek().loc);
            exit(1);
        }

        Block* body = static_cast<Block*>(ParseBlock());

        if(!body)
        {
            std::println(stderr, "{}:: Expected block expression for while body", Peek().loc);
            exit(1);
        }

        return MakeNode<WhileLoop>(loc, condition, body);
    }
    return nullptr;
}

ASTNode* Parser::ParseAssignment()
{
    size_t start = m_current;
    if(Match(TokenType::IDENTIFIER))
    {
        Token token           = Previous();
        Location loc          = token.loc;
        std::string_view name = m_src.substr(token.start, token.len);

        Expression* index = ParseIndex();

        if(Match(TokenType::ASSIGN))
        {
            Expression* expr = static_cast<Expression*>(ParseExpression());
            return MakeNode<VariableAssignment>(loc, name, index, expr);
        }
    }

    m_current = start;  // revert the IDENTIFIER match, in case it was not an assignment
    return ParseBitwiseAnd();
}

ASTNode* Parser::ParseIf()
{
    if(Match(TokenType::IF))
    {
        Location loc          = Previous().loc;
        Expression* condition = static_cast<Expression*>(ParseExpression());
        if(!condition)
        {
            std::println(stderr, "{}:: Expected expression for if condition", Peek().loc);
            exit(1);
        }

        Block* body = static_cast<Block*>(ParseBlock());
        if(!body)
        {
            std::println(stderr, "{}:: Expected block expression for if body", Peek().loc);
            exit(1);
        }
        Block* elseBlock = nullptr;
        if(Match(TokenType::ELSE))
        {
            elseBlock = static_cast<Block*>(ParseBlock());

            if(!elseBlock)
            {
                std::println(stderr, "{}:: Expected block expression for else body", Peek().loc);
                exit(1);
            }
        }
        return MakeNode<IfElse>(loc, condition, body, elseBlock);
    }
    return nullptr;
}

ASTNode* Parser::ParseBlock()
{
    if(Match(TokenType::LBRACE))
    {
        Location loc = Previous().loc;
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
            std::println(stderr, "{}:: Expected closing brace for block expression", Peek().loc);
            exit(1);
        }
        if(statements.empty())
            return MakeNode<Block>(loc, expr);
        else
            return MakeNode<Block>(loc, expr, std::move(statements));
    }
    return nullptr;
}

ASTNode* Parser::ParseBitwiseAnd()
{
    Expression* expr = static_cast<Expression*>(ParseBitwiseOr());

    while(Match(TokenType::BAND))
    {
        Location loc      = Previous().loc;
        Expression* right = static_cast<Expression*>(ParseBitwiseOr());
        expr              = MakeNode<BinaryExpression>(loc, expr, Op::BAND, right);
    }

    return expr;
}

ASTNode* Parser::ParseBitwiseOr()
{
    Expression* expr = static_cast<Expression*>(ParseEquality());

    while(Match(TokenType::BOR))
    {
        Location loc      = Previous().loc;
        Expression* right = static_cast<Expression*>(ParseEquality());
        expr              = MakeNode<BinaryExpression>(loc, expr, Op::BOR, right);
    }

    return expr;
}

ASTNode* Parser::ParseEquality()
{
    Expression* expr = static_cast<Expression*>(ParseComparison());

    while(Match(TokenType::EQUAL) || Match(TokenType::NOT_EQUAL))
    {
        Location loc      = Previous().loc;
        Op op             = Previous().type == TokenType::EQUAL ? Op::EQUAL : Op::NOT_EQUAL;
        Expression* right = static_cast<Expression*>(ParseComparison());
        expr              = MakeNode<BinaryExpression>(loc, expr, op, right);
    }

    return expr;
}

ASTNode* Parser::ParseComparison()
{
    Expression* expr = static_cast<Expression*>(ParseShift());

    while(Match(TokenType::LT) || Match(TokenType::GT) || Match(TokenType::LEQ) || Match(TokenType::GEQ))
    {
        Location loc = Previous().loc;

        Op op;
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

        Expression* right = static_cast<Expression*>(ParseShift());
        expr              = MakeNode<BinaryExpression>(loc, expr, op, right);
    }

    return expr;
}

ASTNode* Parser::ParseShift()
{
    Expression* expr = static_cast<Expression*>(ParseTerm());

    while(Match(TokenType::LSH) || Match(TokenType::RSH))
    {
        Location loc      = Previous().loc;
        Op op             = Previous().type == TokenType::LSH ? Op::LSH : Op::RSH;
        Expression* right = static_cast<Expression*>(ParseTerm());
        expr              = MakeNode<BinaryExpression>(loc, expr, op, right);
    }

    return expr;
}

ASTNode* Parser::ParseTerm()
{
    Expression* expr = static_cast<Expression*>(ParseFactor());

    while(Match(TokenType::MINUS) || Match(TokenType::PLUS))
    {
        Location loc      = Previous().loc;
        Op op             = Previous().type == TokenType::MINUS ? Op::MINUS : Op::PLUS;
        Expression* right = static_cast<Expression*>(ParseFactor());
        expr              = MakeNode<BinaryExpression>(loc, expr, op, right);
    }

    return expr;
}

ASTNode* Parser::ParseFactor()
{
    Expression* expr = static_cast<Expression*>(ParseUnary());

    while(Match(TokenType::MUL) || Match(TokenType::DIV))
    {
        Location loc      = Previous().loc;
        Op op             = Previous().type == TokenType::MUL ? Op::MUL : Op::DIV;
        Expression* right = static_cast<Expression*>(ParseUnary());
        expr              = MakeNode<BinaryExpression>(loc, expr, op, right);
    }

    return expr;
}

ASTNode* Parser::ParseUnary()
{
    if(Match(TokenType::MINUS))
    {
        Location loc     = Previous().loc;
        Expression* expr = static_cast<Expression*>(ParseUnary());
        return MakeNode<UnaryExpression>(loc, Op::U_MINUS, expr);
    }
    if(Match(TokenType::NOT))
    {
        Location loc     = Previous().loc;
        Expression* expr = static_cast<Expression*>(ParseUnary());
        return MakeNode<UnaryExpression>(loc, Op::NOT, expr);
    }

    return ParseFnCall();
}


ASTNode* Parser::ParseFnCall()
{
    // TODO: instead of requiring IDENTIFIER we should ParsePrimary to be able to do stuff like f()() where f returns a function, etc...
    if(Peek().type == TokenType::IDENTIFIER && Peek2().type == TokenType::LPAREN)
    {
        Token token           = Peek();
        Location loc          = token.loc;
        std::string_view name = m_src.substr(token.start, token.len);

        Advance();  // consume the identifier
        Advance();  // consume the '('

        std::vector<Expression*> arguments;
        arguments.push_back(static_cast<Expression*>(ParseExpression()));
        while(Match(TokenType::COMMA))
        {
            arguments.push_back(static_cast<Expression*>(ParseExpression()));
        }
        if(Match(TokenType::RPAREN))
        {
            return MakeNode<FnCall>(loc, name, std::move(arguments));
        }
        else
        {
            std::println(stderr, "{}: Expected closing paranthesis for funtion arguments", Previous().loc);
            exit(1);
        }
    }
    return ParsePrimary();
}

ASTNode* Parser::ParsePrimary()
{
    if(Match(TokenType::NUMBER))
    {
        Token token = Previous();
        return MakeNode<NumberLiteral>(token.loc, std::stoi(std::string(m_src.substr(token.start, token.len))));
    }

    if(Match(TokenType::IDENTIFIER))
    {
        Token token           = Previous();
        Location loc          = token.loc;
        std::string_view name = m_src.substr(token.start, token.len);
        Expression* index     = ParseIndex();
        return MakeNode<VariableAccess>(loc, name, index);
    }

    if(Match(TokenType::LPAREN))
    {
        Expression* expr = static_cast<Expression*>(ParseExpression());
        if(!Match(TokenType::RPAREN))
        {
            std::println(stderr, "{}: Expected ')'", Previous().loc);
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
        return MakeNode<ExpressionStatement>(m_tokens[start].loc, expr);
    }


    // We don't want to report this error if we are parsing a list of statements like in a block where the last one can be a normal expression (without the semicolon)
    if(reportSemicolonError)
    {
        std::println(stderr, "{}: Expected ';'", Previous().loc);
        exit(1);
    }
    m_current = start;  // revert back to the start of the expression so that it can be parsed as an expression in another function
    return nullptr;
}

ASTNode* Parser::ParseVariableDeclaration()
{
    if(Match(TokenType::LET))
    {
        Location loc = Previous().loc;

        if(!Match(TokenType::IDENTIFIER))
        {
            std::println(stderr, "{}: Expected identifier after let", Previous().loc);
            exit(1);
        }
        Token token           = Previous();
        std::string_view name = m_src.substr(token.start, token.len);
        if(!Match(TokenType::COLON))
        {
            std::println(stderr, "{}: Expected ':' after variable name", Previous().loc);
            exit(1);
        }

        size_t size      = ParseType();
        size_t arraySize = ParseArraySize();

        Expression* expr = nullptr;
        if(Match(TokenType::ASSIGN))
        {
            expr = static_cast<Expression*>(ParseExpression());
        }
        if(!Match(TokenType::SEMICOLON))
        {
            std::println(stderr, "{}: Expected ';'", Previous().loc);
            exit(1);
        }
        if(arraySize)
            return MakeNode<VariableDeclaration>(loc, name, size, arraySize, expr);
        else
            return MakeNode<VariableDeclaration>(loc, name, size, expr);
    }
    return nullptr;
}

size_t Parser::ParseType()
{
    if(!Match(TokenType::IDENTIFIER))
    {
        std::println(stderr, "{}: Expected type after ':'", Previous().loc);
        exit(1);
    }
    Token type                = Previous();
    std::string_view typeName = m_src.substr(type.start, type.len);

    size_t baseSize = 0;

    if(typeName.starts_with("u") || typeName.starts_with("i"))
    {
        if(typeName.ends_with("8"))
            baseSize = 1;
        else if(typeName.ends_with("16"))
            baseSize = 2;
        else if(typeName.ends_with("32"))
            baseSize = 4;
        else if(typeName.ends_with("64"))
            baseSize = 8;
    }
    else
    {
        std::println(stderr, "{}: Unknown type {}", type.loc, typeName);
        exit(1);
    }

    return baseSize;
}

Expression* Parser::ParseIndex()
{
    if(Match(TokenType::LBRACKET))
    {
        Expression* expr = static_cast<Expression*>(ParseExpression());
        if(!Match(TokenType::RBRACKET))
        {
            std::println(stderr, "{}: Expected ']' after number", Previous().loc);
            exit(1);
        }
        return expr;
    }
    return nullptr;
}

size_t Parser::ParseArraySize()
{
    size_t index = 0;
    if(Match(TokenType::LBRACKET))
    {
        if(!Match(TokenType::NUMBER))
        {
            std::println(stderr, "{}: Expected number after '['", Previous().loc);
            exit(1);
        }
        Token sizeToken = Previous();
        if(!Match(TokenType::RBRACKET))
        {
            std::println(stderr, "{}: Expected ']' after number", Previous().loc);
            exit(1);
        }
        index = std::stoi(std::string(m_src.substr(sizeToken.start, sizeToken.len)));
    }
    return index;
}

bool Parser::IsEnd()
{
    return Peek().type == TokenType::END_FILE;
}

Token Parser::Peek()
{
    return m_tokens[m_current];
}
Token Parser::Peek2()
{
    return m_tokens[m_current + 1];
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
