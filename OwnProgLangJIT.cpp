#include "OwnProgLangJIT.h"
#include "llvm/Support/Error.h"

llvm::LLVMContext TheContext;
std::unique_ptr<llvm::Module> TheModule;
std::unique_ptr<llvm::IRBuilder<>> Builder;


void OwnProgLangJIT::addModule(std::unique_ptr<llvm::Module> M) {
    cantFail(CompileLayer.add(MainJD, llvm::orc::ThreadSafeModule(std::move(M), std::make_shared<llvm::LLVMContext>())));
}

llvm::Expected<llvm::JITEvaluatedSymbol> OwnProgLangJIT::lookup(llvm::StringRef Name) {
    return ES.lookup({&MainJD}, Mangle(Name.str()));
}

void InitializeModule() {
    TheContext = llvm::LLVMContext();
    TheModule = std::make_unique<llvm::Module>("OwnProgLang", TheContext);
    Builder = std::make_unique<llvm::IRBuilder<>>(TheContext);
}


//maybe move it to main or somehow pass TheJit?

void AddIR(std:unique_ptr<ASTNode> &node) {
	if (node->codeGeneration()) {
		auto ThreadSafeMod = llvm::orc::ThreadSafeModule(std::move(Module), std::make_unique<llvm::LLVMContext>());
	    TheJIT->addIRModule(std::move(ThreadSafeMod));
	        
	}

}