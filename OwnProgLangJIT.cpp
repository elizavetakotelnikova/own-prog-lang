#include "OwnProgLangJIT.h"
#include "llvm/Support/Error.h"

llvm::LLVMContext TheContext;
std::unique_ptr<llvm::Module> TheModule;
std::unique_ptr<llvm::IRBuilder<>> Builder;

Expected<std::unique_ptr<OwnProgLangJIT>> OwnProgLangJIT::Create() {
    auto EPC = SelfExecutorProcessControl::Create();
    if (!EPC)
        return EPC.takeError();

    auto ES = std::make_unique<ExecutionSession>(std::move(*EPC));

    JITTargetMachineBuilder JTMB(
        ES->getExecutorProcessControl().getTargetTriple());

    auto DL = JTMB.getDefaultDataLayoutForTarget();
    if (!DL)
        return DL.takeError();

    return std::make_unique<OwnProgLangJIT>(std::move(ES), std::move(JTMB),
                                           std::move(*DL));
}

Error OwnProgLangJIT::addModule(ThreadSafeModule TSM, ResourceTrackerSP RT = nullptr) {
    if (!RT)
        RT = MainJD.getDefaultResourceTracker();

    return TransformLayer.add(RT, std::move(TSM));
}

Expected<llvm::JITEvaluatedSymbol> OwnProgLangJIT::lookup(llvm::StringRef Name) {
    return ES->lookup({&MainJD}, Mangle(Name.str()));
}

Expected<ThreadSafeModule> OwnProgLangJIT::optimizeModule(ThreadSafeModule TSM, const MaterializationResponsibility &R) {
    TSM.withModuleDo([](Module &M) {
        auto FPM = std::make_unique<legacy::FunctionPassManager>(&M);
        FPM->add(createInstructionCombiningPass());
        FPM->add(createReassociatePass());
        FPM->add(createGVNPass());
        FPM->add(createCFGSimplificationPass());
        FPM->add(llvm::createLoopUnrollPass());
        FPM->add(llvm::createLoopSimplifyPass());
        FPM->add(llvm::createLoopDeletionPass());
        FPM->add(llvm::createFunctionInliningPass());
        FPM->add(llvm::createTailCallEliminationPass());
        FPM->doInitialization();

        for (auto &F : M)
            FPM->run(F);
    });
    return std::move(M);
}

void InitializeModule() {
    TheContext = llvm::LLVMContext();
    TheModule = std::make_unique<llvm::Module>("OwnProgLang", TheContext);
    Builder = std::make_unique<llvm::IRBuilder<>>(TheContext);
}

//maybe move it to main or somehow pass TheJit?

void AddIR(std:unique_ptr<ASTNode> &node) {
    if (node->codeGeneration()) {
        auto ThreadSafeMod = llvm::orc::ThreadSafeModule(std::move(TheModule), std::make_shared<llvm::LLVMContext>(TheContext));
        TheJIT->addIRModule(std::move(ThreadSafeMod));

    }

}
