#pragma once

#include <vector>
#include "Token.hpp"
#include "AST.h"

class Parser {
    std::vector<Token> tokenList;
    std::vector<std::unique_ptr<ASTNode>> ASTNodeList;
    int current = 0;

    bool matchToken(const std::vector<TokenType>& tokenTypeList);
    bool isAtEnd();

    std::unique_ptr<ASTNode> logError(std::string error);

    std::unique_ptr<Token> getToken();
    std::unique_ptr<Token> currentToken();
    std::unique_ptr<Token> nextToken();
    std::unique_ptr<Token> previousToken();
    std::unique_ptr<Token> consumeToken(TokenType type, std::string errorMessage);

    std::unique_ptr<Statement> statement();
    std::unique_ptr<Statement> variableDeclaration();
    std::unique_ptr<Statement> arrayDeclaration();
    std::unique_ptr<Statement> print();
    std::unique_ptr<Statement> block();
    std::unique_ptr<Statement> condition();
    std::unique_ptr<Statement> forLoop();
    std::unique_ptr<Statement> whileLoop();
    std::unique_ptr<Statement> prototypeFunction();
    std::unique_ptr<Statement> function();
    std::unique_ptr<Statement> returnStmt();
    std::unique_ptr<Statement> expressionStatement();

    std::unique_ptr<Expression> expression();
    std::unique_ptr<Expression> assignment();
    std::unique_ptr<Expression> logicalOR();
    std::unique_ptr<Expression> logicalAND();
    std::unique_ptr<Expression> bitwiseOR();
    std::unique_ptr<Expression> bitwiseXOR();
    std::unique_ptr<Expression> bitwiseAND();
    std::unique_ptr<Expression> equality();
    std::unique_ptr<Expression> comparison();
    std::unique_ptr<Expression> term();
    std::unique_ptr<Expression> factor();
    std::unique_ptr<Expression> unary();
    std::unique_ptr<Expression> primary();
    std::unique_ptr<Expression> identifier();

public:
    Parser(std::vector<Token> tokenList) : tokenList(std::move(tokenList)){};
    std::vector<std::unique_ptr<ASTNode>> getASTNodeList();
    void parse();
};