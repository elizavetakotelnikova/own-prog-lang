#pragma once

#include "Token.hpp"
#include <vector>
#include <memory>

class ASTNode {
public:
    virtual ~ASTNode() = default;
    virtual Value *codeGeneration() = 0;
};

class Expression : public ASTNode {
};

class Statement : public ASTNode {
};

class IntegerLiteralASTNode : public ASTNode {
    int value;
public:
    IntegerLiteralASTNode(int value): value(value){};
    int getValue(){ return value; }
};

class FloatLiteralASTNode : public ASTNode {
    float value;
public:
    FloatLiteralASTNode(float value): value(value){};
    float getValue(){ return value; }
};

class StringLiteralASTNode : public ASTNode {
    std::string value;
public:
    StringLiteralASTNode(std::string value): value(value){};
    std::string getValue(){ return value; }
};

class BoolLiteralASTNode : public ASTNode {
    bool value;
public:
    BoolLiteralASTNode(bool value): value(value){}; 
    bool getValue(){ return value; }
};

class CharLiteralASTNode : public ASTNode {
    char value;
public:
    CharLiteralASTNode(char value): value(value){}; 
    char getValue(){ return value; }
};

class IdentifierASTNode : public ASTNode {
    std::string value;
public:
    IdentifierASTNode(const std::string& value) : value(value){};
    std::string getValue(){ return value; }
};

class UnaryASTNode : public ASTNode {
    Token _operator;
    std::unique_ptr<ASTNode> operand;
public:
    UnaryASTNode(Token _operator, std::unique_ptr<ASTNode> operand) :
        _operator(_operator), operand(std::move(operand)){};
};

class BinaryASTNode : public ASTNode {
    Token _operator;
    std::unique_ptr<ASTNode> leftOperand, righOperand;
public:
    BinaryASTNode(Token _operator, std::unique_ptr<ASTNode> leftOperand, std::unique_ptr<ASTNode> righOperand) :
        _operator(_operator), leftOperand(std::move(leftOperand)), righOperand(move(righOperand)){};
};

class VariableDeclarationASTNode : public ASTNode {
    TokenType type; // INT, FLOAT, CHAR, STR, BOOL
    std::string varName;
    std::unique_ptr<ASTNode> initValue;
public:
    VariableDeclarationASTNode(const std::string& varName, std::unique_ptr<ASTNode> initValue) :
        varName(varName), initValue(std::move(initValue)){};
};

class AssignmentASTNode : public ASTNode {
    std::string varName;
    std::unique_ptr<ASTNode> value;
public:
    AssignmentASTNode(const std::string& varName, std::unique_ptr<ASTNode> value) :
        varName(varName), value(std::move(value)){};
};

class PrintASTNode : public ASTNode {
    std::unique_ptr<ASTNode> expr;
public:
    PrintASTNode(std::unique_ptr<ASTNode> expr) : expr(std::move(expr)){};
};

class BlockASTNode : public ASTNode {
    std::vector<std::unique_ptr<ASTNode>> statementList;
public:
    BlockASTNode(const std::vector<std::unique_ptr<ASTNode>> statementList) :
        statementList(std::move(statementList)){};
};

class ConditionASTNode : public ASTNode {
    std::unique_ptr<ASTNode> ifBlock, elseBlock;
public:
    ConditionASTNode(std::unique_ptr<ASTNode> ifBlock, std::unique_ptr<ASTNode> elseBlock) :
        ifBlock(std::move(ifBlock)), elseBlock(std::move(elseBlock)){};
};

class ForLoop : public ASTNode {
    
};

class WhileLoop : public ASTNode {
    std::unique_ptr<ASTNode> a;
};

class FunctionASTNode : public ASTNode {
    std::string functionName;
    std::vector<std::pair<TokenType, std::string>> functionArgs;
    std::unique_ptr<BlockASTNode> body;
};

class CallFunctionASTNode : public ASTNode {
    std::string functionName;
    std::vector<std::unique_ptr<ASTNode>> functionArgs;
public:
    CallFunctionASTNode(const std::string& functionName, const std::vector<std::unique_ptr<ASTNode>>& functionArgs) :
        functionName(functionName), functionArgs(move(functionArgs)){};
};