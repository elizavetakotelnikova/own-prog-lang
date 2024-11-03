CC=g++
CFLAGS=-Wall -Wextra -Werror -O3

clean:
	rm Dynamite.exe

main:
	$(CC) main.cpp Lexer.cpp -o main
