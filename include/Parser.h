#pragma once

#include <vector>
#include "Token.hpp"
#include "AST.h"

class Parser {
    std::vector<Token> tokenList;
    int current = 0;

    bool matchToken(const std::vector<TokenType>& tokenTypeList);
    bool isAtEnd();

    std::unique_ptr<ASTNode> logError(std::string error);

    std::unique_ptr<Token> getToken();
    std::unique_ptr<Token> currentToken();
    std::unique_ptr<Token> nextToken();
    std::unique_ptr<Token> previousToken();
    std::unique_ptr<Token> consumeToken(TokenType type, std::string errorMessage);

    std::unique_ptr<ASTNode> statement();
    std::unique_ptr<ASTNode> variableDeclaration();
    std::unique_ptr<ASTNode> print();
    std::unique_ptr<ASTNode> block();
    std::unique_ptr<ASTNode> condition();
    std::unique_ptr<ASTNode> forLoop();
    std::unique_ptr<ASTNode> whileLoop();
    std::unique_ptr<ASTNode> function();

    std::unique_ptr<ASTNode> expression();
    std::unique_ptr<ASTNode> assignment();
    std::unique_ptr<ASTNode> logicalOR();
    std::unique_ptr<ASTNode> logicalAND();
    std::unique_ptr<ASTNode> bitwiseOR();
    std::unique_ptr<ASTNode> bitwiseXOR();
    std::unique_ptr<ASTNode> bitwiseAND();
    std::unique_ptr<ASTNode> equality();
    std::unique_ptr<ASTNode> comparison();
    std::unique_ptr<ASTNode> term();
    std::unique_ptr<ASTNode> factor();
    std::unique_ptr<ASTNode> unary();
    std::unique_ptr<ASTNode> primary();
    std::unique_ptr<ASTNode> identifier();

public:
    Parser(const std::vector<Token>& tokenList) : tokenList(std::move(tokenList)){};
    void parse();
};