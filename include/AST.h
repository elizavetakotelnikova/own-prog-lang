#pragma once

#include "Token.hpp"
#include "GCManager.h"
#include <vector>
#include <sstream>
#include <memory>
#include <llvm/IR/Value.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>


class Visitor;
class CodeGenContext;

class ASTNode : public GCObject {
public:
    virtual ~ASTNode() = default;
    virtual void accept(Visitor& visitor) = 0;
    virtual std::string toString() { return "ASTNode"; } 
    bool isChecked = false;

    virtual llvm::Value* codeGeneration(CodeGenContext& context) = 0;
    void traceReferences(std::function<void(GCObject*)> visitor) override {}
};

class Expression : public ASTNode {
public:
    virtual ~Expression() = default;
    std::string toString() override { return "Expression"; }
    void accept(Visitor& visitor) override;
};

class Statement : public ASTNode {
public:
    virtual ~Statement() = default;
    std::string toString() override { return "Statement"; }
    void accept(Visitor& visitor) override;
};

// --------------------------Expression--------------------------

class IntegerLiteral : public Expression {
public:
    int value;

    IntegerLiteral(int value): value(value){}
    void accept(Visitor& visitor) override;
    int getValue(){ return value; }
    std::string toString() override {
        std::stringstream s;
        s << "Integer: " << value;
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;
};

class FloatLiteral : public Expression {
public:
    float value;

    FloatLiteral(float value): value(value){}
    void accept(Visitor& visitor) override;
    float getValue(){ return value; }
    std::string toString() override {
        std::stringstream s;
        s << "Float: " << value;
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;
};

class StringLiteral : public Expression {
public:
    std::string value;

    StringLiteral(const std::string& value): value(value){}
    void accept(Visitor& visitor) override;
    std::string getValue(){ return value; }
    std::string toString() override {
        std::stringstream s;
        s << "String: " << value;
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;
};

class BoolLiteral : public Expression {
public:
    bool value;

    BoolLiteral(bool value): value(value){}
    void accept(Visitor& visitor) override;
    bool getValue(){ return value; }
    std::string toString() override {
        std::stringstream s;
        s << "Bool: " << value;
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;
};

class CharLiteral : public Expression {
public:
    char value;

    CharLiteral(char value): value(value){}; 
    void accept(Visitor& visitor) override;
    char getValue(){ return value; }
    std::string toString() override {
        std::stringstream s;
        s << "Char: " << value;
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;
};

class Identifier : public Expression {
public:
    std::string value;

    Identifier(const std::string& value) : value(value){};
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Identifier: " << value;
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;
};

class Unary : public Expression {
public:
    Token _operator;
    std::unique_ptr<Expression> operand;

    Unary(Token _operator, std::unique_ptr<Expression> operand) :
        _operator(_operator), operand(std::move(operand)){};
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Unary operator: " << _operator.value;
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;

    void traceReferences(std::function<void(GCObject*)> visitor) override {
        if (operand) visitor(operand.get());
    }
};

class Binary : public Expression {
public:
    Token _operator;
    std::unique_ptr<Expression> leftOperand, rightOperand;

    Binary(Token _operator, std::unique_ptr<Expression> leftOperand, std::unique_ptr<Expression> rightOperand) :
        _operator(_operator), leftOperand(std::move(leftOperand)), rightOperand(std::move(rightOperand)){};
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Binary Operator: " << _operator.value;
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;

    void traceReferences(std::function<void(GCObject*)> visitor) override {
        if (leftOperand) visitor(leftOperand.get());
        if (rightOperand) visitor(rightOperand.get());
    }
};

class Comparison : public Expression {
public:
    Token _operator;
    std::unique_ptr<Expression> leftOperand, rightOperand;

    Comparison(Token _operator, std::unique_ptr<Expression> leftOperand, std::unique_ptr<Expression> rightOperand) :
        _operator(_operator), leftOperand(std::move(leftOperand)), rightOperand(std::move(rightOperand)){};
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Comparison Operator: " << _operator.value;
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;
};

class CallFunction : public Expression {
public:
    std::string functionName;
    std::vector<std::unique_ptr<Expression>> functionArgs;

    CallFunction(const std::string& functionName, std::vector<std::unique_ptr<Expression>> functionArgs) :
        functionName(functionName), functionArgs(std::move(functionArgs)){};
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Call FunctionNode with name: " << functionName;
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;

    void traceReferences(std::function<void(GCObject*)> visitor) override {
        for (auto& arg : functionArgs) {
            if (arg) visitor(arg.get());
        }
    }
};

class ArrayAccess : public Expression {
public:
    std::unique_ptr<Identifier> identifier;
    std::unique_ptr<Expression> index;

    ArrayAccess(std::unique_ptr<Identifier> identifier, std::unique_ptr<Expression> index):
        identifier(std::move(identifier)), index(std::move(index)){};

    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Array Access";
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;

    void traceReferences(std::function<void(GCObject*)> visitor) override {
        if (identifier) visitor(identifier.get());
    }
};

class Assignment : public Expression {
public:
    std::unique_ptr<Expression> identifier;
    std::unique_ptr<Expression> value;

    Assignment(std::unique_ptr<Expression> identifier, std::unique_ptr<Expression> value) :
        identifier(std::move(identifier)), value(std::move(value)){}
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Assignment";
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;

    void traceReferences(std::function<void(GCObject*)> visitor) override {
        if (identifier) visitor(identifier.get());
        if (value) visitor(value.get());
    }
};

// --------------------------Statement-------------------------

class ExpressionStatement : public Statement {
public:
    std::unique_ptr<Expression> expression;

    ExpressionStatement(std::unique_ptr<Expression> expression) :
        expression(std::move(expression)){};
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Expression Statement";
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;

    void traceReferences(std::function<void(GCObject*)> visitor) override {
        if (expression) visitor(expression.get());
    }
};

class ArrayDeclaration : public Statement {
public:
    TokenType type;
    std::unique_ptr<Identifier> identifier;
    int size;
    std::vector<std::unique_ptr<Expression>> initValues;

    ArrayDeclaration(TokenType type, 
                    std::unique_ptr<Identifier> identifier,
                    int size,
                    std::vector<std::unique_ptr<Expression>> initValues) :
                    type(type),
                    identifier(std::move(identifier)),
                    size(size),
                    initValues(std::move(initValues)){};
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Array Declaration with type " << type;
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;

    void traceReferences(std::function<void(GCObject*)> visitor) override {
        if (identifier) visitor(identifier.get());
        for (auto& initValue : initValues) {
            if (initValue) visitor(initValue.get());
        }
    }
};

class VariableDeclaration : public Statement {
public:
    TokenType type; // INT, FLOAT, CHAR, STR, BOOL
    std::unique_ptr<Identifier> identifier;
    std::unique_ptr<Expression> initValue;

    VariableDeclaration(TokenType type, std::unique_ptr<Identifier> identifier, std::unique_ptr<Expression> initValue) :
        type(type), identifier(std::move(identifier)), initValue(std::move(initValue)){};
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Variable Declaration with type " << type;
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;

    void traceReferences(std::function<void(GCObject*)> visitor) override {
        if (identifier) visitor(identifier.get());
        if (initValue) visitor(initValue.get());
    }
};

class Print : public Statement {
public:
    std::unique_ptr<Expression> expr;

    Print(std::unique_ptr<Expression> expr) : expr(std::move(expr)){};
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Print";
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;

    void traceReferences(std::function<void(GCObject*)> visitor) override {
        if (expr) visitor(expr.get());
    }
};

class Block : public Statement {
public:
    std::vector<std::unique_ptr<Statement>> statementList;

    Block(std::vector<std::unique_ptr<Statement>> statementList) :
        statementList(std::move(statementList)){}
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Block";
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;

    void traceReferences(std::function<void(GCObject*)> visitor) override {
        for (auto& stmt : statementList) {
            if (stmt) visitor(stmt.get());
        }
    }
};

class Condition : public Statement {
public:
    std::unique_ptr<Expression> conditionExpr;
    std::unique_ptr<Statement> ifBlock, elseBlock;

    Condition(std::unique_ptr<Expression> conditionExpr, std::unique_ptr<Statement> ifBlock, std::unique_ptr<Statement> elseBlock) :
        conditionExpr(std::move(conditionExpr)), ifBlock(std::move(ifBlock)), elseBlock(std::move(elseBlock)){}
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Condition";
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;

    void traceReferences(std::function<void(GCObject*)> visitor) override {
        if (conditionExpr) visitor(conditionExpr.get());
        if (ifBlock) visitor(ifBlock.get());
        if (elseBlock) visitor(elseBlock.get());
    }
};

class ForLoop : public Statement {
public:
    std::unique_ptr<Expression> condition, update;
    std::unique_ptr<Statement> initializer, body;

    ForLoop(std::unique_ptr<Statement> initializer, 
        std::unique_ptr<Expression> condition, 
        std::unique_ptr<Expression> update,
        std::unique_ptr<Statement> body) :
        initializer(std::move(initializer)),
        condition(std::move(condition)),
        update(std::move(update)),
        body(std::move(body)){};
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "For Loop";
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;

    void traceReferences(std::function<void(GCObject*)> visitor) override {
        if (initializer) visitor(initializer.get());
        if (condition) visitor(condition.get());
        if (update) visitor(update.get());
        if (body) visitor(body.get());
    }
};

class WhileLoop : public Statement {
public:
    std::unique_ptr<Expression> condition;
    std::unique_ptr<Statement> body;

    WhileLoop(std::unique_ptr<Expression> condition, std::unique_ptr<Statement> body) :
        condition(std::move(condition)), body(std::move(body)){};
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "While Loop";
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;

    void traceReferences(std::function<void(GCObject*)> visitor) override {
        if (condition) visitor(condition.get());
        if (body) visitor(body.get());
    }
};

class PrototypeFunction : public Statement {
public:
    std::string name;
    std::vector<std::pair<TokenType, std::string>> args;
    TokenType returnType;

    PrototypeFunction(std::string name, std::vector<std::pair<TokenType, std::string>> args, TokenType returnType) :
        name(name), args(args), returnType(returnType){}

    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Prototype FunctionNode with type " << returnType;
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;
};

class FunctionNode : public Statement {
public:
    std::unique_ptr<PrototypeFunction> prototype;
    std::unique_ptr<Block> bodyBlock;

    FunctionNode(std::unique_ptr<PrototypeFunction> proto, std::unique_ptr<Block> body) :
        prototype(std::move(proto)), bodyBlock(std::move(body)){}

    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "FunctionNode";
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;

    void traceReferences(std::function<void(GCObject*)> visitor) override {
        if (prototype) visitor(prototype.get());
        if (bodyBlock) visitor(bodyBlock.get());
    }
};

class Return : public Statement {
public:
    std::unique_ptr<Expression> expr;

    Return(std::unique_ptr<Expression> expr) : expr(std::move(expr)){};
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Return";
        return s.str();
    }
    llvm::Value* codeGeneration(CodeGenContext& context) override;

    void traceReferences(std::function<void(GCObject*)> visitor) override {
        if (expr) visitor(expr.get());
    }
};