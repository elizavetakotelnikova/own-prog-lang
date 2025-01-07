#include "include/OwnProgLangJIT.h"
#include "llvm/Support/Error.h"
#include "llvm/Transforms/Scalar.h"

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

    llvm::errs() << "Adding module to JIT...\n";

    if (auto Err = TransformLayer.add(RT, std::move(TSM))) {
        llvm::errs() << "Failed to add module: " << llvm::toString(std::move(Err)) << "\n";
        return Err;
    }

    llvm::errs() << "Module successfully added.\n";
    return llvm::Error::success();
}

llvm::Expected<llvm::orc::ExecutorSymbolDef> llvm::orc::OwnProgLangJIT::lookup(StringRef Name) {
    auto Sym = ES->lookup({&MainJD}, Mangle(Name.str()));
    if (!Sym) {
        llvm::errs() << "Error: Symbol '" << Name << "' not found.\n";
        llvm::errs() << llvm::toString(Sym.takeError()) << "\n";
        return Sym.takeError();
    }
    
    return Sym;
}

llvm::Expected<llvm::orc::ThreadSafeModule> llvm::orc::OwnProgLangJIT::optimizeModule(ThreadSafeModule TSM, const MaterializationResponsibility &R) {
    TSM.withModuleDo([](Module &M) {
        auto FPM = std::make_unique<legacy::FunctionPassManager>(&M);
        FPM->add(createInstructionCombiningPass());
        FPM->add(createGVNPass());
        FPM->add(createCFGSimplificationPass());
        FPM->add(createTailCallEliminationPass());
        FPM->doInitialization();
        for (auto &F : M)
            FPM->run(F);

        llvm::errs() << "Optimized IR:\n";
        M.print(llvm::errs(), nullptr);
    });
    return std::move(TSM);
}