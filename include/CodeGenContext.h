#pragma once

#include "OwnProgLangJIT.h"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include <memory>
#include <map>
#include <stack>
#include "AST.h"

class CodeGenBlock {
public:
    llvm::BasicBlock* block;
    std::map<std::string, std::pair<llvm::Value*, llvm::Type*>> locals;

    CodeGenBlock(llvm::BasicBlock* block) :
        block(block){}
};

class CodeGenContext {
private:
    GCManager gcManager;
    std::unique_ptr<llvm::orc::OwnProgLangJIT> JIT;

public:
    std::list<CodeGenBlock*> blocks;
    llvm::LLVMContext llvmContext;
    std::unique_ptr<llvm::Module> module;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Function> mainFunction;

    CodeGenContext() : builder(llvmContext), JIT(std::move(*llvm::orc::OwnProgLangJIT::Create())) {
        module = std::make_unique<llvm::Module>("main", llvmContext);
    }

    std::map<std::string, std::pair<llvm::Value*, llvm::Type*>>& locals(){
        return blocks.back()->locals;
    }
    llvm::BasicBlock* currentBlock(){
        return blocks.back()->block;
    }

    void pushBlock(llvm::BasicBlock* block){
        blocks.push_back(new CodeGenBlock(block));
    }

    void popBlock(){
        CodeGenBlock *top = blocks.back();
        blocks.pop_back();
        delete top;
    }

    void generateCode(std::vector<std::unique_ptr<ASTNode>> nodeList);
    void runCode();

    GCManager& getGCManager() {
        return gcManager;
    }
};
