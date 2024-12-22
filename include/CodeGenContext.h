#pragma once

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetSelect.h> 
#include <llvm/ExecutionEngine/GenericValue.h>
#include <memory>
#include <map>
#include <stack>
#include "AST.h"

class CodeGenBlock {
public:
    std::unique_ptr<llvm::BasicBlock> block;
    std::map<std::string, std::pair<llvm::Value*, llvm::Type*>> locals;

    CodeGenBlock(std::unique_ptr<llvm::BasicBlock> block) : 
        block(std::move(block)){}
};

class CodeGenContext {
private:
    GCManager gcManager;

public:
    std::list<std::unique_ptr<CodeGenBlock>> blocks;
    llvm::LLVMContext llvmContext; 
    std::unique_ptr<llvm::Module> module; 
    llvm::IRBuilder<> builder; 
    std::unique_ptr<llvm::Function> mainFunction;

    CodeGenContext() : builder(llvmContext) 
    {
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmParser();
        llvm::InitializeNativeTargetAsmPrinter();
        module = std::make_unique<llvm::Module>("main", llvmContext);
    }

    std::map<std::string, std::pair<llvm::Value*, llvm::Type*>>& locals(){
        return blocks.back()->locals;
    }
    std::unique_ptr<llvm::BasicBlock> currentBlock(){
        return std::move(blocks.back()->block);
    }

    void pushBlock(std::unique_ptr<llvm::BasicBlock> block){
        blocks.push_back(std::make_unique<CodeGenBlock>(std::move(block)));
    }

    void popBlock(){
        blocks.pop_back();
    }

    void generateCode(std::vector<std::unique_ptr<ASTNode>> nodeList);
    llvm::GenericValue runCode();

    GCManager& getGCManager() {
        return gcManager;
    }
};
