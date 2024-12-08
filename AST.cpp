#pragma once

#include "include/AST.h"
#include "include/Token.hpp"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"

#include <vector>
#include <memory>

static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::Module> TheModule;
static std::unique_ptr<llvm::IRBuilder<>> Builder;
static std::map<std::string, llvm::Value*> NamedValues;
static std::map<std::string, std::unique_ptr<PrototypeFunction>> FunctionProtos;
static ExitOnError ExitOnErr;

llvm::Value *logError(const char *Str) {
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

llvm::Value *IntegerLiteral::codeGeneration() {
        return llvm::ConstantInt::get(*TheContext, llvm::APInt(value));
}

llvm::Value *FloatLiteral::codeGeneration() {
        return llvm::ConstantFP::get(*TheContext, llvm::APFloat(value));
}

llvm::Value *DoubleLiteral::codeGeneration() {
        return llvm::ConstantFP::get(*TheContext, llvm::APFloat(value));
}

llvm::Value* ArrayAccess::codeGeneration() {
    llvm::Value *arrayPtr = identifier->codeGeneration();
    if (!arrayPtr) {
        return nullptr;
    }

    if (NamedValues[identifier->value]) {
        logError("No such array");
        return nullptr;
    }

    // Я без понятия как это писать
}

llvm::Value* ArrayDeclaration::codeGeneration() {
    llvm::Type* llvmType = nullptr;
    switch (type) {
        case TokenType::INT:   llvmType = llvm::Type::getInt32Ty(*TheContext); break;
        case TokenType::FLOAT: llvmType = llvm::Type::getFloatTy(*TheContext); break;
        case TokenType::CHAR:  llvmType = llvm::Type::getInt8Ty(*TheContext); break;
        case TokenType::STR:   llvmType = llvm::Type::getInt8PtrTy(*TheContext); break;
        case TokenType::BOOL:  llvmType = llvm::Type::getInt1Ty(*TheContext); break;
    }

    if (!llvmType) {
        logError("Unsupported type");
        return nullptr;
    }

    llvm::ArrayType *arrayType = llvm::ArrayType::get(llvmType, size);
    llvm::AllocaInst *arrayAlloc = Builder->CreateAlloca(arrayType, nullptr, identifier->value);

    for (int i = 0; i < size; ++i) {
        if (i < initValues.size()) {
            llvm::Value *initValue = initValues[i]->codeGeneration();
            if (!initValue) {
                return nullptr;
            }

            llvm::Value *index = llvm::ConstantInt::get(Type::getInt32Ty(*TheContext), i);
            llvm::Value *elementPtr = Builder->CreateGEP(arrayType, arrayAlloc, index, "array_element_ptr");
            Builder->CreateStore(initValue, elementPtr);
        } else {
            llvm::Value *defaultValue = llvm::ConstantInt::get(llvmType, 0);
            llvm::Value *index = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), i);
            llvm::Value *elementPtr = Builder->CreateGEP(arrayType, arrayAlloc, index, "array_element_ptr");
            Builder->CreateStore(defaultValue, elementPtr);
        }
    }

    return arrayAlloc;
}

llvm::Value *VariableReferencing::codeGeneration() {
    llvm::Value *V = NamedValues[varName];
    if (!V)
       logError("Unknown variable name");
    return V;
}

llvm::Value *Binary::codeGeneration() {
    llvm::Value *left = leftOperand->codeGeneration();
    llvm::Value *right = rightOperand->codeGeneration();
    if (!left || !right)
        return nullptr;

    llvm::Type *leftType = left->getType();
    llvm::Type *rightType = right->getType();

  if (leftType != rightType) {
      logError("operands must be same type");
  };

  if (_operator.value == "==") {
      if (leftType->isFloatingPointTy()) {
          llvm::Value* res = Builder->CreateFCmpUEQ(left, right, "cmpeqtmp");
          return Builder->CreateUIToFP(res, llvm::Type::getDoubleTy(*TheContext), "booltmp");
      }

      if (leftType->isIntegerTy()) {
          llvm::Value* res = Builder->CreateICmpEQ(left, right, "cmpeqtmp");
          return Builder->CreateUIToFP(res, llvm::Type::getDoubleTy(*TheContext), "booltmp");
      }
  }

  if (_operator.value == "!=") {
     if (leftType->isFloatingPointTy()) {
         llvm::Value *res = Builder->CreateFCmpUNE(left, right, "cmpnetmp");
        return Builder->CreateUIToFP(res, llvm::Type::getDoubleTy(*TheContext), "booltmp");
     }

     if (leftType->isIntegerTy()) {
         llvm::Value *res = Builder->CreateICmpNE(left, right, "cmpnetmp");
         return Builder->CreateUIToFP(res, llvm::Type::getDoubleTy(*TheContext), "booltmp");
     }

     return logError("Invalid operand type for '!='");
  }

  if (_operator.value == ">=") {
      if (leftType->isFloatingPointTy()) {
          llvm::Value* res = Builder->CreateFCmpUGE(left, right, "cmpgetmp");
          return Builder->CreateUIToFP(res, llvm::Type::getDoubleTy(*TheContext), "booltmp");
      }

      if (leftType->isIntegerTy()) {
          llvm::Value* res = Builder->CreateICmpSGE(left, right, "cmpgetmp");
          return Builder->CreateUIToFP(res, llvm::Type::getDoubleTy(*TheContext), "booltmp");
      }

      return logError("Invalid operand type for '>='");
  }

  if (_operator.value == "<=") {
      if (leftType->isFloatingPointTy()) {
          llvm::Value* res = Builder->CreateFCmpULE(left, right, "cmpletmp");
          return Builder->CreateUIToFP(res, llvm::Type::getDoubleTy(*TheContext), "booltmp");
      }

      if (leftType->isIntegerTy()) {
          llvm::Value* res = Builder->CreateICmpSLE(left, right, "cmpletmp");
          return Builder->CreateUIToFP(res, llvm::Type::getDoubleTy(*TheContext), "booltmp");
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
               llvm::Value* res = Builder->CreateFCmpUGT(left, right, "cmpgttmp");
               return Builder->CreateUIToFP(res, llvm::Type::getDoubleTy(*TheContext),
                                            "booltmp");
           }

           if (leftType->isIntegerTy()) {
               llvm::Value* res = Builder->CreateICmpULT(left, right, "cmpgttmp");
               return Builder->CreateUIToFP(res, llvm::Type::getDoubleTy(*TheContext),
                                            "booltmp");
           }

          logError("invalid operand type");
      case '>':
           if (leftType->isFloatingPointTy()) {
               llvm::Value* res = Builder->CreateFCmpUGT(left, right, "cmplttmp");
               return Builder->CreateUIToFP(res, llvm::Type::getDoubleTy(*TheContext),
                                            "booltmp");
           }

           // or CreateUITo ??
          if (leftType->isIntegerTy()) {
              llvm::Value* res = Builder->CreateICmpUGT(left, right, "cmpgttmp");
              return Builder->CreateUIToFP(res, llvm::Type::getDoubleTy(*TheContext),
                                           "booltmp");
          }

          logError("invalid operand type");
      default:
          return logError("invalid binary operator");
    }
}


llvm::Value* StringLiteral::codeGeneration() {
    llvm::Constant* stringConstant = llvm::ConstantDataArray::getString(*TheContext, value, true);

    llvm::GlobalVariable* globalString = new GlobalVariable(
            &TheModule,
            stringConstant->getType(),
            true,
            GlobalValue::PrivateLinkage,
            stringConstant,
            ".str"
    );

    llvm::Value* stringPtr = Builder.CreateInBoundsGEP(
            globalString->getValueType(),
            globalString,
            {Builder->getInt32(0), Builder->getInt32(0)},
            "str_ptr"
    );

    return stringPtr;
};

llvm::Value* BoolLiteral::codeGeneration() {
    return llvm::ConstantInt::get(Builder->getInt1Ty(), value);
}

llvm::Value* CharLiteral::codeGeneration() {
    return llvm::ConstantInt::get(Builder->getInt8Ty(), static_cast<uint8_t>(value));
}

llvm::Value* Identifier::codeGeneration() {
    llvm::Value* result = NamedValues[result];

    if (!result) {
        throw std::runtime_error("Undefined identifier: " + value);
    }

    return result;
}

llvm::Value *UnaryExprAST::codeGeneration() {
  Value *operandCode = operand->codeGeneration();
  if (!operandCode)
    return nullptr;

  Function *currFunc = getFunction(std::string("unary") + _operator);
  if (!currFunc)
    return logError("Unknown unary operator");

  return Builder->CreateCall(currFunc, operandCode, "unop");
}

llvm::Value* VariableDeclaration::codeGeneration() {
    llvm::Type* llvmType = nullptr;
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

    llvm::AllocaInst* allocaInst = Builder->CreateAlloca(llvmType, nullptr, identifier->value);

    if (initValue) {
        llvm::Value* initVal = initValue->codeGeneration();

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
    llvm::Value* value = expr->codeGeneration();
    llvm::Type* valueType = value->getType();
    Function* printfFunc = TheModule->getFunction("printf");
    if (!printfFunc) {
        llvm::FunctionType* printfType = llvm::FunctionType::get(
                Builder.getInt32Ty(),
                llvm::PointerType::get(Builder.getInt8Ty(), 0),
                true
        );
        printfFunc = Function::Create(
                printfType, Function::ExternalLinkage, "printf", &TheModule);
    }

    llvm::Value* formatStr = nullptr;
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

llvm::Value *Block::codeGeneration() {
    llvm::Value* lastValue = nullptr;
    for (const auto& statement : statementList) {
        if (statement) {
            lastValue = statement->codeGeneration();
        }
    }

    return lastValue;
};

llvm::Value *Condition::codeGeneration() {
    llvm::Value *condition = ifBlock->codeGeneration();
    if (!condition)
        return nullptr;

    condV = Builder->CreateFCmpONE(
        condV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");

    Function *currFunc = Builder->GetInsertBlock()->getParent();

    llvm::BasicBlock *thenBl = llvm::BasicBlock::Create(*TheContext, "then", currFunc);
    llvm::BasicBlock *elseBl = llvm::BasicBlock::Create(*TheContext, "else");
    llvm::BasicBlock *mergeBl = llvm::BasicBlock::Create(*TheContext, "ifcont");

    Builder->CreateCondBr(condition, thenBl, elseBl);

    Builder->SetInsertPoint(thenBl);

    llvm::Value *thenV = thenBlock->codeGeneration();
    if (!thenV)
        return nullptr;

    Builder->CreateBr(mergeBl);
    thenBl = Builder->GetInsertBlock();

    currFunc->insert(currFunc->end(), elseBl);
    Builder->SetInsertPoint(elseBl);

    llvm::Value *elseV = elseBlock->codeGeneration();
    if (!elseV)
        return nullptr;

    Builder->CreateBr(mergeBl);
    elseBl = Builder->GetInsertBlock();

    currFunc->insert(currFunc->end(), mergeBl);
    Builder->SetInsertPoint(mergeBl);
    llvm::PHINode *PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");

    PN->addIncoming(thenV, thenBl;
    PN->addIncoming(elseV, elseBl);
    return PN;
}

llvm::Value* ForLoop::codeGeneration() {
    Function *function = Builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *loopHeader = llvm::BasicBlock::Create(*TheContext, "loopHeader", function);
    llvm::BasicBlock *loopBody = llvm::BasicBlock::Create(*TheContext, "loopBody", function);
    llvm::BasicBlock *loopEnd = llvm::BasicBlock::Create(*TheContext, "loopEnd", function);

    if (initializer) {
        initializer->codeGeneration();
    }

    Builder->CreateBr(loopHeader);
    Builder->SetInsertPoint(loopHeader);

    llvm::Value *condValue = condition->codeGeneration();
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

llvm::Value *WhileLoop::codeGeneration() {
    Function* parentFunction = Builder->GetInsertBlock()->getParent();

    llvm::BasicBlock* condBlock = llvm::BasicBlock::Create(*TheContext, "while.cond", parentFunction);
    llvm::BasicBlock* bodyBlock = llvm::BasicBlock::Create(*TheContext, "while.body", parentFunction);
    llvm::BasicBlock* endBlock = llvm::BasicBlock::Create(*TheContext, "while.end", parentFunction);

    Builder.CreateBr(condBlock);
    Builder.SetInsertPoint(condBlock);
    llvm::Value* condValue = condition->codeGeneration();
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
    llvm::Value* bodyValue = body->codeGeneration();
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

llvm::Function *PrototypeFunction::codeGeneration() {
    std::vector<llvm::Type*> argsNumbers(args.size());
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

    llvm::FunctionType *funcType =
            llvm::FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);

    llvm::Function *func =
            llvm::Function::Create(funcType, Function::ExternalLinkage, name, TheModule.get());

    unsigned index = 0;
    for (auto &Arg : func->args())
        Arg.setName(args[index++]);

    return func;
}

llvm::Function *Function::codeGeneration() {
    auto &prototype = *proto;
    FunctionProtos[proto->name] = std::move(proto);
    llvm::Function *currFunction = getFunction(prototype.name);
    if (!currFunction)
        currFunction = proto->codeGeneration();

    if (!currFunction)
        return nullptr;

    llvm::BasicBlock *block = llvm::BasicBlock::Create(*TheContext, "entry", currFunction);
    Builder->SetInsertPoint(block);

    NamedValues.clear();
    for (auto &Arg : currFunction->args()) {
        llvm::AllocaInstance *allocated = CreateEntryBlockAlloca(currFunction, Arg.getName());
        Builder->CreateStore(&Arg, allocated);

        NamedValues[std::string(Arg.getName())] = &allocated;
    }

    if (llvm::Value *returnValue = body->codeGeneration()) {
        Builder->CreateRet(returnValue);
        llvm::verifyFunction(*currFunction);
        return currFunction;
    }

    currFunction->eraseFromParent();
    return nullptr;
}

llvm::Value *CallFunction::codeGeneration() {
    llvm::Function *CalleeF = TheModule->getFunction(functionName);
    if (!CalleeF)
        return logError("Unknown function referenced");

    if (CalleeF->arg_size() != functionArgs.size())
        return logError("Incorrect # arguments passed");

    std::vector<llvm::Value *> ArgsV;
    for (unsigned i = 0, e = functionArgs.size(); i != e; ++i) {
        ArgsV.push_back(functionArgs[i]->codeGeneration());
        if (!ArgsV.back())
            return nullptr;
    }

    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

// в туториале у чела BasicBlock хранится, у нас такого нет, я хз че делать тоже
llvm::Value* Return::codeGeneration() {
    llvm::Value *returnValue = expr->codeGeneration();
    if (!returnValue) {
        return nullptr;
    }

    llvm::Function *currentFunction = Builder->GetInsertBlock()->getParent();
    if (!currentFunction) {
        return nullptr;
    }

    llvm::Type *returnType = currentFunction->getReturnType();
    if (returnType != returnValue->getType()) {
        returnValue = Builder->CreateBitCast(returnValue, returnType, "casted_return_value");
    }

    Builder->CreateRet(returnValue);

    return nullptr;
}