#include <iostream>
#include <fstream>
#include <sstream>
#include <gc/gc.h>
#include "include/VisitorPrintNode.h"
#include "include/Lexer.h"
#include "include/Parser.h"
#include "include/CodeGenContext.h"
#include "include/OwnProgLangJIT.h"

int main(int argc, char *argv[]){

	GC_INIT();

	llvm::InitializeNativeTarget();
	llvm::InitializeNativeTargetAsmPrinter();
	llvm::InitializeNativeTargetAsmParser();
    std::ifstream inputFile(argv[1]);
	if (!inputFile.is_open()){
		std::cout << "Error opening file" << "\n";
		return 1;
	}
    std::stringstream ss;
	ss << inputFile.rdbuf();
	std::string sourceCode = ss.str();
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
	std::cout << "Node list size: " << nodeList.size() << "\n";
	i = 0;
	for (auto &node : nodeList){
		std::cout << "Node No." << i << " : " << "\n";
		if (!node->isChecked){
			node->accept(visitor);

		}
		i++;
	}
	CodeGenContext context;
	context.generateCode(std::move(nodeList));

	context.runCode();

	return 0;
}