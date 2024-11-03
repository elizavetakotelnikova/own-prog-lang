#include <iostream>
#include <fstream>
#include <sstream>
#include "include/Lexer.h"

int main(int argc, char *argv[]){
    std::ifstream inputFile(argv[1]);
	if (!inputFile.is_open()){
		std::cout << "Error opening file" << std::endl;
		return 1;
	}
    std::stringstream ss;
	ss << inputFile.rdbuf();
	std::string sourceCode = ss.str();

	Lexer lexer(sourceCode);
	lexer.scanSourceCode();
	for (Token t : lexer.getTokenList()){
		std::cout << t.type << " " << t.value << "\n";
	}

	return 0;
}