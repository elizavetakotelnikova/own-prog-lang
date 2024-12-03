#pragma once

#include "Token.hpp"
#include <vector>
#include <memory>
#include <sstream>

class Visitor;

class ASTNode {
public:
    virtual ~ASTNode() = default;
    virtual void accept(Visitor& visitor) = 0;
    virtual Value *codeGeneration() = 0;
    virtual std::string toString() { return "ASTNode"; } 
    bool isChecked = false;
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
};

class Binary : public Expression {
public:
    Token _operator;
    std::unique_ptr<Expression> leftOperand, rightOperand;

    Binary(Token _operator, std::unique_ptr<Expression> leftOperand, std::unique_ptr<Expression> rightOperand) :
        _operator(_operator), leftOperand(std::move(leftOperand)), rightOperand(move(rightOperand)){};
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Binary Operator: " << _operator.value;
        return s.str();
    }
};

class CallFunction : public Expression {
public:
    std::string functionName;
    std::vector<std::unique_ptr<Expression>> functionArgs;

    CallFunction(const std::string& functionName, std::vector<std::unique_ptr<Expression>> functionArgs) :
        functionName(functionName), functionArgs(move(functionArgs)){};
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Call Function with name: " << functionName;
        return s.str();
    }
};

class ArrayAccess : public Expression {
public:
    std::unique_ptr<Identifier> identifier;
    int index;

    ArrayAccess(std::unique_ptr<Identifier> identifier, int index):
        identifier(std::move(identifier)), index(index){};

    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Array Access";
        return s.str();
    }
};

class Assignment : public Expression {
public:
    std::unique_ptr<Expression> identifier;
    std::unique_ptr<Expression> initValue;

    Assignment(std::unique_ptr<Expression> identifier, std::unique_ptr<Expression> initValue) :
        identifier(std::move(identifier)), initValue(std::move(initValue)){}
    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Assignment";
        return s.str();
    }
};

// Do we really need this ?
// class VariableReferencingASTNode : public ASTNode {
//   std::string Name;
// public:
//   VariableExprAST(const std::string &Name) : Name(Name) {}
// };


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
};

class PrototypeFunction : public Statement {
public:
    std::string name;
    std::vector<std::pair<TokenType, std::string>> args;
    TokenType returnType;

    PrototypeFunction(std::string name, std::vector<std::pair<TokenType, std::string>> args, TokenType retrunType) :
        name(name), args(args), returnType(returnType){}

    void accept(Visitor& visitor) override;
    std::string toString() override {
        std::stringstream s;
        s << "Prototype Function with type " << returnType;
        return s.str();
    }
};


class Function : public Statement {
public:
    std::unique_ptr<PrototypeFunction> proto;
    std::unique_ptr<Block> body;

    Function(std::unique_ptr<PrototypeFunction> proto, std::unique_ptr<Block> body) :
        proto(std::move(proto)), body(std::move(body)){}

    void accept(Visitor& visitor) override;
    std::string getName() {
        return proto->name;
    }

    std::string toString() override {
        std::stringstream s;
        s << "Function";
        return s.str();
    }

    Function *codeGeneration();
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
};