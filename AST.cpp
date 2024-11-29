#pragma once

#include "Token.hpp"
#include "include/AST.h"
#include "include/Token.hpp"
#include <vector>
#include <memory>

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, Value*> NamedValues;
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
        return ConstantInt::get(*TheContext, APInt(value));
}

Value *FloatLiteralASTNode::codeGeneration() {
        return ConstantFP::get(*TheContext, APFloat(value));
}

Value *DoubleLiteralASTNode::codeGeneration() {
        return ConstantFP::get(*TheContext, APFloat(value));
}

Value *VariableReferencingASTNode::codeGeneration() {
  Value *V = NamedValues[varName];
  if (!V)
    logError("Unknown variable name");
  return V;
}

Value *BinaryASTNode::codeGeneration() {
  Value *left = leftOperand->codeGeneration();
  Value *right = rightOperand->codeGeneration();
  if (!left || !right)
    return nullptr;

  Type *leftType = left->getType();
  Type *rightType = right->getType();

  if (leftType != rightType) {
      logError("operands must be same type");
  };

  if (_operator.value == "==") {
      if (leftType->isFloatingPointTy()) {
          Value* res = Builder->CreateFCmpUEQ(left, right, "cmpeqtmp");
          return Builder->CreateUIToFP(res, Type::getDoubleTy(*TheContext), "booltmp");
      }

      if (leftType->isIntegerTy()) {
          Value* res = Builder->CreateICmpEQ(left, right, "cmpeqtmp");
          return Builder->CreateUIToFP(res, Type::getDoubleTy(*TheContext), "booltmp");
      }
  }

  if (_operator.value == "!=") {
     if (leftType->isFloatingPointTy()) {
        Value *res = Builder->CreateFCmpUNE(left, right, "cmpnetmp");
        return Builder->CreateUIToFP(res, Type::getDoubleTy(*TheContext), "booltmp");
     }

     if (leftType->isIntegerTy()) {
         Value *res = Builder->CreateICmpNE(left, right, "cmpnetmp");
         return Builder->CreateUIToFP(res, Type::getDoubleTy(*TheContext), "booltmp");
     }

     return logError("Invalid operand type for '!='");
  }

  if (_operator.value == ">=") {
      if (leftType->isFloatingPointTy()) {
          Value* res = Builder->CreateFCmpUGE(left, right, "cmpgetmp");
          return Builder->CreateUIToFP(res, Type::getDoubleTy(*TheContext), "booltmp");
      }

      if (leftType->isIntegerTy()) {
          Value* res = Builder->CreateICmpSGE(left, right, "cmpgetmp");
          return Builder->CreateUIToFP(res, Type::getDoubleTy(*TheContext), "booltmp");
      }

      return logError("Invalid operand type for '>='");
  }

  if (_operator.value == "<=") {
      if (leftType->isFloatingPointTy()) {
          Value* res = Builder->CreateFCmpULE(left, right, "cmpletmp");
          return Builder->CreateUIToFP(res, Type::getDoubleTy(*TheContext), "booltmp");
      }

      if (leftType->isIntegerTy()) {
          Value* res = Builder->CreateICmpSLE(left, right, "cmpletmp");
          return Builder->CreateUIToFP(res, Type::getDoubleTy(*TheContext), "booltmp");
      }

      return logError("Invalid operand type for '<='");
  }

  switch (_operator.value[0]) {
      case '+':
          return Builder->CreateFAdd(left, right, "addtmp");
      case '-':
          return Builder->CreateFSub(left, right, "subtmp");
      case '*':
          return Builder->CreateFMul(left, right, "divtmp");
      case '/':
           return Builder->CreateFDiv(left, right, "multmp");
      case '<':
           if (leftType->isFloatingPointTy()) {
               Value* res = Builder->CreateFCmpUGT(left, right, "cmpgttmp");
               return Builder->CreateUIToFP(res, Type::getDoubleTy(*TheContext),
                                            "booltmp");
           }

           if (leftType->isIntegerTy()) {
               Value* res = Builder->CreateICmpULT(left, right, "cmpgttmp");
               return Builder->CreateUIToFP(res, Type::getDoubleTy(*TheContext),
                                            "booltmp");
           }

          logError("invalid operand type");
      case '>':
           if (leftType->isFloatingPointTy()) {
               Value* res = Builder->CreateFCmpUGT(left, right, "cmplttmp");
               return Builder->CreateUIToFP(res, Type::getDoubleTy(*TheContext),
                                            "booltmp");
           }

           // or CreateUITo ??
          if (leftType->isIntegerTy()) {
              Value* res = Builder->CreateICmpUGT(left, right, "cmpgttmp");
              return Builder->CreateUIToFP(res, Type::getDoubleTy(*TheContext),
                                           "booltmp");
          }

          logError("invalid operand type");
      default:
          return logError("invalid binary operator");
    }
}

class Statement : public ASTNode {
};

Value* StringLiteralAstNode::codeGeneration() {
    LLVMContext& Context = Module->getContext();

    Constant* stringConstant = ConstantDataArray::getString(Context, value, true);

    GlobalVariable* globalString = new GlobalVariable(
            &Module,
            stringConstant->getType(),
            true,
            GlobalValue::PrivateLinkage,
            stringConstant,
            ".str"
    );

    Value* stringPtr = Builder.CreateInBoundsGEP(
            globalString->getValueType(),
            globalString,
            {Builder->getInt32(0), Builder->getInt32(0)},
            "str_ptr"
    );

    return stringPtr;
};

Value* BoolLiteralASTNode::codeGeneration() {
    return ConstantInt::get(Builder->getInt1Ty(), value);
}

Value* CharLiteralASTNode::codeGeneration() {
    return ConstantInt::get(Builder->getInt8Ty(), static_cast<uint8_t>(value));
}

class IdentifierASTNode : public ASTNode {
    std::string value;
public:
    IdentifierASTNode(const std::string& value) : value(value){};
    std::string getValue(){ return value; }
};

Value* IdentifierASTNode::codeGeneration() {
    Value* result = codeGen.getSymbolValue(value);

    if (!result) {
        throw std::runtime_error("Undefined identifier: " + value);
    }

    return result;
}

Value *UnaryExprAST::codeGeneration() {
  Value *operandCode = operand->codeGeneration();
  if (!operandCode)
    return nullptr;

  Function *currFunc = getFunction(std::string("unary") + _operator);
  if (!currFunc)
    return logError("Unknown unary operator");

  return Builder->CreateCall(currFunc, operandCode, "unop");
}

Value* VariableDeclarationASTNode::codeGeneration() {
    LLVMContext& Context = Module->getContext();

    Type* llvmType = nullptr;
    switch (type) {
        case TokenType::INT:   llvmType = Builder->getInt32Ty(); break;
        case TokenType::FLOAT: llvmType = Builder->getFloatTy(Context); break;
        case TokenType::CHAR:  llvmType = Builder->getInt8Ty(); break;
        case TokenType::STR:   llvmType = Builder->getInt8PtrTy(); break;
        case TokenType::BOOL:  llvmType = Builder->getInt1Ty(); break;
    }

    if (!llvmType) {
        logError("Unsupported variable type");
        return nullptr;
    }

    AllocaInst* allocaInst = Builder->CreateAlloca(llvmType, nullptr, varName);

    if (initValue) {
        Value* initVal = initValue->codeGeneration();

        if (initVal->getType() != llvmType) {
            if (llvmType->isIntegerTy() && initVal->getType()->isIntegerTy()) {
                initVal = Builder->CreateIntCast(initVal, llvmType, true, "cast");
            } else if (llvmType->isFloatingPointTy() && initVal->getType()->isIntegerTy()) {
                initVal = Builder->CreateSIToFP(initVal, llvmType, "int_to_float");
            } else if (llvmType->isIntegerTy() && initVal->getType()->isFloatingPointTy()) {
                initVal = Builder->CreateFPToSI(initVal, llvmType, "float_to_int");
            } else {
                logError("Type mismatch in variable initialization");
                return nullptr;
            }
        }

        Builder->CreateStore(initVal, allocaInst);
    }

    NamedValues[varName] = allocaInst;
    return allocaInst;
}

Value* AssigmentASTNode::codeGeneration() {
    Value* varPtr = NamedValues[varName];
    if (!varPtr) {
        throw std::runtime_error("Undefined variable: " + varName);
    }

    Value* exprValue = value->codeGeneration();

    llvm::Type* varType = varPtr->getType()->getPointerElementType();
    if (exprValue->getType() != varType) {
        if (varType->isIntegerTy() && exprValue->getType()->isIntegerTy()) {
            exprValue = Builder.CreateIntCast(exprValue, varType, true, "cast");
        } else if (varType->isFloatingPointTy() && exprValue->getType()->isIntegerTy()) {
            exprValue = Builder.CreateSIToFP(exprValue, varType, "int_to_float");
        } else if (varType->isIntegerTy() && exprValue->getType()->isFloatingPointTy()) {
            exprValue = Builder.CreateFPToSI(exprValue, varType, "float_to_int");
        } else {
            throw std::runtime_error("Type mismatch in assignment to " + varName);
        }
    }

    Builder.CreateStore(exprValue, varPtr);
    return exprValue;
}

void *PrintASTNode::codeGeneration() {
    /*Function *CalleeF = TheModule->getOrInsertFunction("printf",
                                                       FunctionType::get(IntegerType::getInt32Ty(Context), PointerType::get(Type::getInt8Ty(Context), 0), true));
    Builder.CreateCall(CalleeF, expr, "printfCall");*/

    LLVMContext& Context = Module->getContext();
    Value* value = expr->codeGeneration();
    Type* valueType = value->getType();
    Function* printfFunc = Module->getFunction("printf");
    if (!printfFunc) {
        FunctionType* printfType = FunctionType::get(
                Builder.getInt32Ty(),
                PointerType::get(Builder.getInt8Ty(), 0),
                true
        );
        printfFunc = Function::Create(
                printfType, Function::ExternalLinkage, "printf", &Module);
    }

    Value* formatStr = nullptr;
    if (valueType->isIntegerTy(32)) {
        formatStr = Builder.CreateGlobalStringPtr("%d\n", "formatStr");
    } else if (valueType->isFloatingPointTy()) {
        formatStr = Builder.CreateGlobalStringPtr("%f\n", "formatStr");
    } else if (valueType->isIntegerTy(1)) {
        formatStr = Builder.CreateGlobalStringPtr("%d\n", "formatStr");
    } else if (valueType->isIntegerTy(8)) {
        formatStr = Builder.CreateGlobalStringPtr("%c\n", "formatStr");
    } else if (valueType->isPointerTy() && valueType->getPointerElementType()->isIntegerTy(8)) {
        formatStr = Builder.CreateGlobalStringPtr("%s\n", "formatStr");
    } else {
        throw std::runtime_error("Unsupported type for print");
    }

    return Builder.CreateCall(printfFunc, {formatStr, value}, "printCall");
};

Value *BlockASTNode::codeGeneration() {
    Value* lastValue = nullptr;
    for (const auto& statement : statementList) {
        if (statement) {
            lastValue = statement->codeGeneration();
        }
    }

    return lastValue;
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

Value *ForLoopASTNode::codeGeneration() {
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

Value *WhileLoopASTNode::codeGeneration() {
    LLVMContext& Context = Module->getContext();
    Function* parentFunction = Builder->GetInsertBlock()->getParent();

    BasicBlock* condBlock = BasicBlock::Create(Context, "while.cond", parentFunction);
    BasicBlock* bodyBlock = BasicBlock::Create(Context, "while.body", parentFunction);
    BasicBlock* endBlock = BasicBlock::Create(Context, "while.end", parentFunction);

    Builder.CreateBr(condBlock);
    Builder.SetInsertPoint(condBlock);
    Value* condValue = condition->codeGeneration();
    if (!condValue) {
        throw std::runtime_error("Failed to generate condition for while loop");
    }

    if (condValue->getType()->isIntegerTy() && condValue->getType()->getIntegerBitWidth() != 1) {
        condValue = Builder.CreateICmpNE(condValue, Builder.getInt32(0), "while.cond.bool");
    } else if (!condValue->getType()->isIntegerTy(1)) {
        throw std::runtime_error("Condition of while loop must be boolean");
    }

    Builder.CreateCondBr(condValue, bodyBlock, endBlock);
    Builder.SetInsertPoint(bodyBlock);
    Value* bodyValue = body->codeGeneration();
    Builder.CreateBr(condBlock);
    Builder.SetInsertPoint(endBlock);
    return nullptr;
};

/*Function *PrototypeAST::codeGeneration() {
  std::vector<Type*> Doubles(args.size(), Type::getDoubleTy(*TheContext));
  FunctionType *funcType =
      FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);

  Function *func =
      Function::Create(funcType, Function::ExternalLinkage, name, TheModule.get());

  unsigned index = 0;
  for (auto &Arg : func->args())
    Arg.setName(args[index++]);

  return func;
}*/

Function *PrototypeAST::codeGeneration() {
    std::vector<Type*> argsNumbers(args.size());
    for (int i = 0; i < args.size(); i++) {
        if (args.first.type == TokenType.INT) {
            argsNumbers[i] = Type::getINT32Ty(*TheContext)
        }

        if (args.first.type == TokenType.DOUBLE) {
            argsNumbers[i] = Type::getDoubleTy(*TheContext)
        }

        if (args.first.type == TokenType.FLOAT) {
            argsNumbers[i] = Type::getFloatTy(*TheContext)
        }

        if (args.first.type == TokenType.FLOAT) {
            argsNumbers[i] = Type::getINT8Ty(*TheContext)
        }

        if (args.first.type == TokenType.BOOL) {
            argsNumbers[i] = Type::getINT1Ty(*TheContext)
        }
    }

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

Value *CallFunctionASTNode::codeGeneration() {
    Function *CalleeF = TheModule->getFunction(Callee);
    if (!CalleeF)
        return logError("Unknown function referenced");

    if (CalleeF->arg_size() != Args.size())
        return logError("Incorrect # arguments passed");

    std::vector<Value *> ArgsV;
    for (unsigned i = 0, e = Args.size(); i != e; ++i) {
        ArgsV.push_back(Args[i]->codeGeneration());
        if (!ArgsV.back())
            return nullptr;
    }

    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}