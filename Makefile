CC=g++
CFLAGS=-Wall -Wextra -Werror -O3

clean:
	rm main.exe

main:
	$(CC) main.cpp Parser.cpp Lexer.cpp ASTVisitor.cpp VisitorPrintNode.cpp AST.cpp -o main
