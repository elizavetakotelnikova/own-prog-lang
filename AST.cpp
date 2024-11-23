// Implement each node here with codeGenerationeration function pls

#pragma once

#include "Token.hpp"
#include <vector>
#include <memory>

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, Value *> NamedValues;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
static ExitOnError ExitOnErr;

Value *logError(const char *Str) {
  logError(Str);
  return nullptr;
}

Function *getFunction(std::string Name) {
  if (auto *F = TheModule->getFunction(Name))
    return F;

  auto FI = FunctionProtos.find(Name);
  if (FI != FunctionProtos.end())
    return FI->second->codeGeneration();

  return nullptr;
}

Value *IntegerLiteralASTNode::codeGeneration() {
        return ConstantFP::get(*TheContext, APFloat(value));
}

Value *FloatLiteralASTNode::codeGeneration() {
        return ConstantFP::get(*TheContext, APFloat(value));
}

Value *DoubleLiteralASTNode::codeGeneration() {
        return ConstantFP::get(*TheContext, APFloat(value));
}

Value *VariableReferencingASTNode::codeGeneration() {
  // Look this variable up in the function.
  Value *V = NamedValues[varName];
  if (!V)
    logError("Unknown variable name");
  return V;
}

Value *BinaryExpressionASTNode::codeGeneration() {
  Value *left = leftOperand->codeGeneration();
  Value *right = rightOperand->codeGeneration();
  if (!left || !right)
    return nullptr;

  switch (_operator.value) {
  case '+':
    return Builder->CreateFAdd(left, right, "addtmp");
  case '-':
    return Builder->CreateFSusb(left, right, "subtmp");
  case '*':
    return Builder->CreateFMul(left, right, "multmp");
  case '<':
    left = Builder->CreateFCmpULT(left, right, "cmptmp");
    return Builder->CreateUIToFP(left, Type::getDoubleTy(*TheContext),
                                 "booltmp");
  default:
    return logError("invalid binary operator");
  }
}

class Statement : public ASTNode {
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

Value *UnaryExprAST::codeGeneration() {
  Value *operandCode = operand->codeGeneration();
  if (!operandCode)
    return nullptr;

  Function *currFunc = getFunction(std::string("unary") + _operator);
  if (!currFunc)
    return logError("Unknown unary operator");

  return Builder->CreateCall(currFunc, operandCode, "unop");
}

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

Value *ConditionASTNode::codeGeneration() {
  Value *condition = ifBlock->codeGeneration();
  if (!condition)
    return nullptr;

  condV = Builder->CreateFCmpONE(
      condV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");

  Function *currFunc = Builder->GetInsertBlock()->getParent();

  BasicBlock *thenBl = BasicBlock::Create(*TheContext, "then", currFunc);
  BasicBlock *elseBl = BasicBlock::Create(*TheContext, "else");
  BasicBlock *mergeBl = BasicBlock::Create(*TheContext, "ifcont");

  Builder->CreateCondBr(condition, thenBl, elseBl);

  Builder->SetInsertPoint(thenBl);

  Value *thenV = thenBlock->codeGeneration();
  if (!thenV)
    return nullptr;

  Builder->CreateBr(mergeBl);
  thenBl = Builder->GetInsertBlock();

  currFunc->insert(currFunc->end(), elseBl);
  Builder->SetInsertPoint(elseBl);

  Value *elseV = elseBlock->codeGeneration();
  if (!elseV)
    return nullptr;

  Builder->CreateBr(mergeBl);
  elseBl = Builder->GetInsertBlock();

  currFunc->insert(currFunc->end(), mergeBl);
  Builder->SetInsertPoint(mergeBl);
  PHINode *PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");

  PN->addIncoming(thenV, thenBl;
  PN->addIncoming(elseV, elseBl);
  return PN;
}

Value *ForLoopAST::codeGeneration() {
  Function *currFunc = Builder->GetInsertBlock()->getParent();

  AllocaInst *allocated = CreateEntryBlockAlloca(currFunc, variableName);

  Value *startValue = start->codeGeneration();
  if (!startValue)
    return nullptr;

  Builder->CreateStore(startValue, allocated);

  BasicBlock *loopBlock = BasicBlock::Create(*TheContext, "loop", currFunction);

  Builder->CreateBr(loopBlock);

  Builder->SetInsertPoint(loopBlock);

  AllocaInst *oldValue = NamedValues[variableName];
  NamedValues[variableName] = allocated;

  if (!body->codeGeneration())
    return nullptr;

  Value *stepValue = nullptr;
  if (step) {
    stepValue = step->codeGeneration();
    if (!stepVal)
      return nullptr;
  } else {
    StepVal = ConstantFP::get(*TheContext, APFloat(1.0));
  }

  Value *endCondition = end->codeGeneration();
  if (!endCondition)
    return nullptr;

  Value *currVar = Builder->CreateLoad(Type::getDoubleTy(*TheContext), allocated,
                                      variableName.c_str());
  Value *nextVar = Builder->CreateFAdd(currVar, stepValue, "nextvar");
  Builder->CreateStore(nextVar, allocated);

  endCondition = Builder->CreateFCmpONE(
      endCondition, ConstantFP::get(*TheContext, APFloat(0.0)), "loopcond");

  BasicBlock *afterBlock =
      BasicBlock::Create(*TheContext, "afterloop", currFunction);

  Builder->CreateCondBr(endCondition, loopBlock, afterBlock);

  Builder->SetInsertPoint(afterBlock);

  if (oldValue)
    NamedValues[variableName] = oldValue;
  else
    NamedValues.erase(variableName);

  return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}

void *WhileLoopASTNode::codeGeneration() {
    static int labelCount = 0;
    std::string startLabel = "while_start_" + std::to_string(labelCount);
    std::string endLabel = "while_end_" + std::to_string(labelCount);
    labelCount++;
    std::cout << startLabel << ":" << std::endl;
    condition->codeGeneration(ir);
    std::cout << "if_false" << " " << "goto" << std::endl;
    body->codeGeneration(ir);
    std::cout << "goto" << " " << startLabel << std::endl;
    std::cout << endLabel << ":" << startLabel << std::endl;
};

Function *PrototypeAST::codeGeneration() {
  std::vector<Type *> Doubles(args.size(), Type::getDoubleTy(*TheContext));
  FunctionType *funcType =
      FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);

  Function *func =
      Function::Create(funcType, Function::ExternalLinkage, name, TheModule.get());

  unsigned index = 0;
  for (auto &Arg : func->args())
    Arg.setName(args[index++]);

  return func;
}

Function *FunctionAST::codeGeneration() {
  auto &prototype = *proto;
  FunctionProtos[proto->getName()] = std::move(proto);
  Function *currFunction = getFunction(prototype.getName());
  if (!currFunction)
    return nullptr;

  if (prototype.isBinaryOp())
    binopPrecedence[prototype.getOperatorName()] = prototype.getBinaryPrecedence();

  BasicBlock *block = BasicBlock::Create(*TheContext, "entry", currFunction);
  Builder->SetInsertPoint(block);

  NamedValues.clear();
  for (auto &Arg : currFunction->args()) {
    allocaInstance *allocated = CreateEntryBlockAlloca(currFunction, Arg.getName());

    Builder->CreateStore(&Arg, allocated);

    NamedValues[std::string(Arg.getName())] = allocated;
  }

  if (Value *returnValue = body->codeGeneration()) {
    Builder->CreateRet(returnValue);
    verifyFunction(*currFunction);

    return currFunction;
  }

  currFunction->eraseFromParent();
  if (prototype.isBinaryOp())
    BinopPrecedence.erase(prototype.getOperatorName());
  return nullptr;
}

Value *CallFunctionASTNode::codegen() {
    Function *CalleeF = TheModule->getFunction(Callee);
    if (!CalleeF)
        return logError("Unknown function referenced");

    if (CalleeF->arg_size() != Args.size())
        return logError("Incorrect # arguments passed");

    std::vector<Value *> ArgsV;
    for (unsigned i = 0, e = Args.size(); i != e; ++i) {
        ArgsV.push_back(Args[i]->codegen());
        if (!ArgsV.back())
            return nullptr;
    }

    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}