#include <iostream>

#include <fstream>
#include <print>

#include "Lexer.h"
#include "Parser.h"
#include "Codegenerator.h"


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

void WriteFile(const std::string& path, std::string_view content)
{
    std::ofstream stream{
        path,
        std::ios::out | std::ios::trunc};
    if(!stream.is_open())
    {
        std::print(stderr, "Error writing file: {}", path);
        exit(1);
    }
    stream << content;
}
int main()
{
    std::string file = "examples/test";
    std::string src  = ReadFile(file + ".eos");
    Lexer lexer(file, src);
    lexer.Lex();
    Parser parser(src, lexer.Tokens());
    parser.Parse();
    CodeGenerator codegen(parser.GetAST());
    WriteFile(file + ".asm", codegen.GetCode());

    return 0;
}
