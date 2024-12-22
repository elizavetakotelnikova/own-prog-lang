#include "include/AST.h"
#include "include/Visitor.h"

void Expression::accept(Visitor &visitor)
{
    visitor.visitExpression(*this);
}

void Statement::accept(Visitor &visitor)
{
    visitor.visitStatement(*this);
}

void IntegerLiteral::accept(Visitor &visitor)
{
    visitor.visitIntegerLiteral(*this);
}

void FloatLiteral::accept(Visitor &visitor)
{
    visitor.visitFloatLiteral(*this);
}

void StringLiteral::accept(Visitor &visitor)
{
    visitor.visitStringLiteral(*this);
}

void BoolLiteral::accept(Visitor &visitor)
{
    visitor.visitBoolLiteral(*this);
}

void CharLiteral::accept(Visitor &visitor)
{
    visitor.visitCharLiteral(*this);
}

void Identifier::accept(Visitor &visitor)
{
    visitor.visitIdentifier(*this);
}

void Unary::accept(Visitor &visitor)
{
    visitor.visitUnary(*this);
}

void Binary::accept(Visitor &visitor)
{
    visitor.visitBinary(*this);
}

void Comparison::accept(Visitor &visitor)
{
    visitor.visitComparison(*this);
}

void CallFunction::accept(Visitor &visitor)
{
    visitor.visitCallFunction(*this);
}

void ArrayAccess::accept(Visitor &visitor)
{
    visitor.visitArrayAccess(*this);
}

void Assignment::accept(Visitor &visitor)
{
    visitor.visitAssignment(*this);
}

void ExpressionStatement::accept(Visitor &visitor)
{
    visitor.visitExpressionStatement(*this);
}

void ArrayDeclaration::accept(Visitor &visitor)
{
    visitor.visitArrayDeclaration(*this);
}

void VariableDeclaration::accept(Visitor &visitor)
{
    visitor.visitVariableDeclaration(*this);
}

void Print::accept(Visitor &visitor)
{
    visitor.visitPrint(*this);
}

void Block::accept(Visitor &visitor)
{
    visitor.visitBlock(*this);
}

void Condition::accept(Visitor &visitor)
{
    visitor.visitCondition(*this);
}

void ForLoop::accept(Visitor &visitor)
{
    visitor.visitForLoop(*this);
}

void WhileLoop::accept(Visitor &visitor)
{
    visitor.visitWhileLoop(*this);
}

void PrototypeFunction::accept(Visitor &visitor)
{
    visitor.visitPrototypeFunction(*this);
}

void FunctionNode::accept(Visitor &visitor)
{
    visitor.visitFunction(*this);
}

void Return::accept(Visitor &visitor)
{
    visitor.visitReturn(*this);
}
