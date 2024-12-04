#include <iostream>
#include <fstream>
#include <sstream>
#include "include/VisitorPrintNode.h"
#include "include/Lexer.h"
#include "include/Parser.h"
#include "OwnProgLangJIT.h"

int main(int argc, char *argv[]){
    std::ifstream inputFile(argv[1]);
	if (!inputFile.is_open()){
		std::cout << "Error opening file" << std::endl;
		return 1;
	}
    std::stringstream ss;
	ss << inputFile.rdbuf();
	std::string sourceCode = ss.str();

	auto JIT = llvm::orc::OwnProgLangJIT::Create();
	if (!JIT) {
		llvm::errs() << "Failed to create JIT: " << llvm::toString(JIT.takeError()) << "\n";
		return 1;
	}

	Lexer lexer(sourceCode);
	lexer.scanSourceCode();
	int i = 0;
	for (Token t : lexer.getTokenList()){
		std::cout << "Token No." << i << " : " << t.type << " " << t.value << "\n";
		i++;
	}
	Parser parser(lexer.getTokenList());
	parser.parse();
	VisitorPrintNode visitor(std::cout);
	auto nodeList = parser.getASTNodeList();
	std::cout << "Node list size: " << nodeList.size() << std::endl;
	i = 0;
	for (auto &node : nodeList){
		std::cout << "Node No." << i << " : " << std::endl;
		if (!node->isChecked){
			node->accept(visitor);

			auto *FuncNode = dynamic_cast<Function *>(node.get());
			if (FuncNode) {
				if (auto Err = JIT->addAST(std::make_unique<FunctionAST>(FuncNode))) {
					llvm::errs() << "Error adding AST to JIT: " << llvm::toString(std::move(Err)) << "\n";
					return 1;
				}
			}
		}
		i++;
	}
	return 0;
}