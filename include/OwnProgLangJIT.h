#ifndef OWN_PROG_LANG_JIT_H
#define OWN_PROG_LANG_JIT_H

#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/Orc/CompileOnDemandLayer.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/EPCIndirectionUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/IRTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/Orc/Shared/ExecutorSymbolDef.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "AST.h"
#include <memory>

namespace llvm
{
	namespace orc
	{

		class OwnProgLangASTMaterializationUnit : public MaterializationUnit
		{
		public:
			OwnProgLangASTMaterializationUnit::OwnProgLangASTMaterializationUnit(OwnProgLangASTLayer &L, std::unique_ptr<Function> F) : MaterializationUnit(L.getInterface(*F)), L(L), F(std::move(F)) {}

			StringRef getName() const override
			{
				return "OwnProgLangASTMaterializationUnit";
			}

			void materialize(std::unique_ptr<MaterializationResponsibility> R) override;

		private:
			OwnProgLangASTLayer &L;
			std::unique_ptr<Function> F;

			void discard(const JITDylib &JD, const SymbolStringPtr &Sym) override
			{
				llvm_unreachable("OwnProgLang functions are not overridable");
			}
		};

		class OwnProgLangASTLayer
		{
		public:
			OwnProgLangASTLayer(IRLayer &BaseLayer, const DataLayout &DL) : BaseLayer(BaseLayer), DL(DL) {}

			Error add(ResourceTrackerSP RT, std::unique_ptr<Function> F);

			void emit(std::unique_ptr<MaterializationResponsibility> MR, std::unique_ptr<Function> F);

			MaterializationUnit::Interface getInterface(Function &F);

		private:
			IRLayer &BaseLayer;
			const DataLayout &DL;
		};

		class OwnProgLangJIT
		{
		private:
			std::unique_ptr<ExecutionSession> ES;
			std::unique_ptr<EPCIndirectionUtils> EPCIU;
			RTDyldObjectLinkingLayer ObjectLayer;
			IRCompileLayer CompileLayer;
			IRTransformLayer TransformLayer;
			OwnProgLangASTLayer ASTLayer;

			DataLayout DL;
			MangleAndInterner Mangle;
			ThreadSafeContext Ctx;

			JITDylib &MainJD;

			static void handleLazyCallThroughError()
			{
				errs() << "LazyCallThrough error: Could not find function body";
				exit(1);
			}

		public:
			OwnProgLangJIT(std::unique_ptr<ExecutionSession> ES, std::unique_ptr<EPCIndirectionUtils> EPCIU, JITTargetMachineBuilder JTMB, DataLayout DL)
				: ES(std::move(ES)), EPCIU(std::move(EPCIU)), DL(std::move(DL)), Mangle(*this->ES, this->DL),
				  ObjectLayer(*this->ES, []()
							  { return std::make_unique<SectionMemoryManager>(); }),
				  CompileLayer(*this->ES, ObjectLayer, std::make_unique<ConcurrentIRCompiler>(std::move(JTMB))),
				  TransformLayer(*this->ES, CompileLayer, optimizeModule),
				  ASTLayer(TransformLayer, this->DL),
				  MainJD(this->ES->createBareJITDylib("<main>"))
			{
				MainJD.addGenerator(cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(DL.getGlobalPrefix())));
			}
			static Expected<std::unique_ptr<OwnProgLangJIT>> Create();
			Error addModule(ThreadSafeModule TSM, ResourceTrackerSP RT = nullptr);
			Error addAST(std::unique_ptr<Function> F, ResourceTrackerSP RT = nullptr);
			llvm::Expected<llvm::JITEvaluatedSymbol> lookup(llvm::StringRef Name);
			const llvm::DataLayout &getDataLayout() const { return DL; }
			JITDylib &getMainJITDylib() { return MainJD; }

		private:
			static Expected<ThreadSafeModule> optimizeModule(ThreadSafeModule TSM, const MaterializationResponsibility &R);
		};
	}
}
#endif
