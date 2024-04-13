#include <iostream>

#include <fstream>
#include <print>

#include "Lexer.h"
#include "Parser.h"


std::string ReadFile(const std::string& path)
{
    std::ifstream stream(path, std::ios::in);
    if(!stream.is_open())
    {
        std::print(stderr, "Error reading file: {}", path);
        exit(1);
    }
    return {std::istreambuf_iterator<char>(stream), std::istreambuf_iterator<char>()};
}

int main()
{
    std::string src = ReadFile("examples/test.eos");
    Lexer lexer(src);
    lexer.Lex();
    Parser parser(src, lexer.Tokens());
    parser.Parse();
    for(const auto& token : lexer.Tokens())
    {
        std::cout << token << std::endl;
    }
    return 0;
}
