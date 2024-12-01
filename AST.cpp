#pragma once

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

Value *IntegerLiteral::codeGeneration() {
        return ConstantInt::get(*TheContext, APInt(value));
}

Value *FloatLiteral::codeGeneration() {
        return ConstantFP::get(*TheContext, APFloat(value));
}

Value *DoubleLiteral::codeGeneration() {
        return ConstantFP::get(*TheContext, APFloat(value));
}

Value* ArrayAccess::codeGeneration() {
    Value *arrayPtr = identifier->codeGeneration();
    if (!arrayPtr) {
        return nullptr;
    }

    if (NamedValues[identifier->value]) {
        logError("No such array");
        return nullptr;
    }

    // Я без понятия как это писать
}

Value* ArrayDeclaration::codeGeneration() {
    Type* llvmType = nullptr;
    switch (type) {
        case TokenType::INT:   llvmType = Type::getInt32Ty(*TheContext); break;
        case TokenType::FLOAT: llvmType = Type::getFloatTy(*TheContext); break;
        case TokenType::CHAR:  llvmType = Type::getInt8Ty(*TheContext); break;
        case TokenType::STR:   llvmType = Type::getInt8PtrTy(*TheContext); break;
        case TokenType::BOOL:  llvmType = Type::getInt1Ty(*TheContext); break;
    }

    if (!llvmType) {
        logError("Unsupported type");
        return nullptr;
    }

    ArrayType *arrayType = llvm::ArrayType::get(llvmType, size);
    AllocaInst *arrayAlloc = Builder->CreateAlloca(arrayType, nullptr, identifier->value);

    for (int i = 0; i < size; ++i) {
        if (i < initValues.size()) {
            llvm::Value *initValue = initValues[i]->codeGeneration();
            if (!initValue) {
                return nullptr;
            }

            Value *index = ConstantInt::get(Type::getInt32Ty(*TheContext), i);
            Value *elementPtr = Builder->CreateGEP(arrayType, arrayAlloc, index, "array_element_ptr");
            Builder->CreateStore(initValue, elementPtr);
        } else {
            Value *defaultValue = ConstantInt::get(llvmType, 0);
            Value *index = ConstantInt::get(Type::getInt32Ty(*TheContext), i);
            Value *elementPtr = Builder->CreateGEP(arrayType, arrayAlloc, index, "array_element_ptr");
            Builder->CreateStore(defaultValue, elementPtr);
        }
    }

    return arrayAlloc;
}

Value *VariableReferencing::codeGeneration() {
  Value *V = NamedValues[varName];
  if (!V)
    logError("Unknown variable name");
  return V;
}

Value *Binary::codeGeneration() {
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


Value* StringLiteral::codeGeneration() {
    Constant* stringConstant = ConstantDataArray::getString(*TheContext, value, true);

    GlobalVariable* globalString = new GlobalVariable(
            &TheModule,
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

Value* BoolLiteral::codeGeneration() {
    return ConstantInt::get(Builder->getInt1Ty(), value);
}

Value* CharLiteral::codeGeneration() {
    return ConstantInt::get(Builder->getInt8Ty(), static_cast<uint8_t>(value));
}

Value* Identifier::codeGeneration() {
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

Value* VariableDeclaration::codeGeneration() {
    Type* llvmType = nullptr;
    switch (type) {
        case TokenType::INT:   llvmType = Builder->getInt32Ty(*TheContext); break;
        case TokenType::FLOAT: llvmType = Builder->getFloatTy(*TheContext); break;
        case TokenType::CHAR:  llvmType = Builder->getInt8Ty(*TheContext); break;
        case TokenType::STR:   llvmType = Builder->getInt8PtrTy(*TheContext); break;
        case TokenType::BOOL:  llvmType = Builder->getInt1Ty(*TheContext); break;
    }

    if (!llvmType) {
        logError("Unsupported variable type");
        return nullptr;
    }

    AllocaInst* allocaInst = Builder->CreateAlloca(llvmType, nullptr, identifier->value);

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

    NamedValues[identifier->value] = allocaInst;
    return allocaInst;
}

/*Value* Assignment::codeGeneration() {
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
}*/

void *Print::codeGeneration() {
    Value* value = expr->codeGeneration();
    Type* valueType = value->getType();
    Function* printfFunc = TheModule->getFunction("printf");
    if (!printfFunc) {
        FunctionType* printfType = FunctionType::get(
                Builder.getInt32Ty(),
                PointerType::get(Builder.getInt8Ty(), 0),
                true
        );
        printfFunc = Function::Create(
                printfType, Function::ExternalLinkage, "printf", &TheModule);
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

Value *Block::codeGeneration() {
    Value* lastValue = nullptr;
    for (const auto& statement : statementList) {
        if (statement) {
            lastValue = statement->codeGeneration();
        }
    }

    return lastValue;
};

Value *Condition::codeGeneration() {
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

Value* ForLoop::codeGeneration() {
    Function *function = Builder->GetInsertBlock()->getParent();
    BasicBlock *loopHeader = BasicBlock::Create(*TheContext, "loopHeader", function);
    BasicBlock *loopBody = BasicBlock::Create(*TheContext, "loopBody", function);
    BasicBlock *loopEnd = BasicBlock::Create(*TheContext, "loopEnd", function);

    if (initializer) {
        initializer->codeGeneration();
    }

    Builder->CreateBr(loopHeader);
    Builder->SetInsertPoint(loopHeader);

    Value *condValue = condition->codeGeneration();
    if (!condValue) {
        logError("Unsupported condition");
        return nullptr;
    }

    Builder->CreateCondBr(condValue, loopBody, loopEnd);

    Builder->SetInsertPoint(loopBody);
    if (body) {
        body->codeGeneration();
    }

    if (update) {
        update->codeGeneration();
    }

    Builder->CreateBr(loopHeader);
    Builder->SetInsertPoint(loopEnd);

    return nullptr;
}

Value *WhileLoop::codeGeneration() {
    Function* parentFunction = Builder->GetInsertBlock()->getParent();

    BasicBlock* condBlock = BasicBlock::Create(*TheContext, "while.cond", parentFunction);
    BasicBlock* bodyBlock = BasicBlock::Create(*TheContext, "while.body", parentFunction);
    BasicBlock* endBlock = BasicBlock::Create(*TheContext, "while.end", parentFunction);

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

Value *CallFunction::codeGeneration() {
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

// в туториале у чела BasicBlock хранится, у нас такого нет, я хз че делать тоже
Value* Return::codeGeneration() {
    Value *returnValue = expr->codeGeneration();
    if (!returnValue) {
        return nullptr;
    }

    Function *currentFunction = Builder->GetInsertBlock()->getParent();
    if (!currentFunction) {
        return nullptr;
    }

    Type *returnType = currentFunction->getReturnType();
    if (returnType != returnValue->getType()) {
        returnValue = Builder->CreateBitCast(returnValue, returnType, "casted_return_value");
    }

    Builder->CreateRet(returnValue);

    return nullptr;
}