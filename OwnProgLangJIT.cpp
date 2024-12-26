#include "include/OwnProgLangJIT.h"
#include "llvm/Support/Error.h"

llvm::LLVMContext TheContext;
std::unique_ptr<llvm::Module> TheModule;
std::unique_ptr<llvm::IRBuilder<>> Builder;

llvm::Expected<std::unique_ptr<llvm::orc::OwnProgLangJIT>> llvm::orc::OwnProgLangJIT::Create() {
    auto EPC = SelfExecutorProcessControl::Create();
    if (!EPC)
        return EPC.takeError();

    auto ES = std::make_unique<ExecutionSession>(std::move(*EPC));

    JITTargetMachineBuilder JTMB(
        ES->getExecutorProcessControl().getTargetTriple());

    auto DL = JTMB.getDefaultDataLayoutForTarget();
    if (!DL)
        return DL.takeError();

    return std::make_unique<OwnProgLangJIT>(std::move(ES), std::move(JTMB), std::move(*DL));
}

llvm::Error llvm::orc::OwnProgLangJIT::addModule(ThreadSafeModule TSM, ResourceTrackerSP RT) {
    if (!RT)
        RT = MainJD.getDefaultResourceTracker();

    return TransformLayer.add(RT, std::move(TSM));
}

llvm::Expected<llvm::orc::ExecutorSymbolDef> llvm::orc::OwnProgLangJIT::lookup(StringRef Name) {
    return ES->lookup({&MainJD}, Mangle(Name.str()));
}

llvm::Expected<llvm::orc::ThreadSafeModule> llvm::orc::OwnProgLangJIT::optimizeModule(ThreadSafeModule TSM, const MaterializationResponsibility &R) {
//    TSM.withModuleDo([](Module &M) {
//        auto FPM = std::make_unique<legacy::FunctionPassManager>(&M);
//        FPM->add(createInstructionCombiningPass());
//        FPM->add(createReassociatePass());
//        FPM->add(createGVNPass());
//        FPM->add(createCFGSimplificationPass());
//        FPM->add(createLoopUnrollPass());
//        FPM->add(createTailCallEliminationPass());
//        FPM->doInitialization();
//
//        for (auto &F : M)
//            FPM->run(F);
//    });
    return std::move(TSM);
}