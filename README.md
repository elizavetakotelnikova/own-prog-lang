# Precedence rules

expression      → assignment;
assignment      → IDENTIFIER "=" assignment 
                | logical_or
logical_or      → logical_and ( ( "||" ) logical_and )*;
logical_and     → bitwise_or ( ( "&&" ) bitwise_or )*;
bitwise_or      → bitwise_xor ( ( "|" ) bitwise_xor )*;
bitwise_xor     → bitwise_and ( ( "^" ) bitwise_and )*;
bitwise_and     → equality( ( "&" ) equality )*;
equality        → comparison ( ( "!=" | "==" ) comparison )* ;
comparison      → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term            → factor ( ( "-" | "+" ) factor )* ;
factor          → unary ( ( "/" | "*" | "%" ) unary )* ;
unary           → ( "!" | "-" ) unary
                | primary ;
primary         → NUMBER | STRING | "true" | "false" | IDENTIFIER
                | "(" expression ")" ;


Statements

program         → statement* EOF ;

statement       → expression
                | varDecl
                | printStmt
                | block
                | condition
                | forLoop
                | whileLoop ;

varDecl         → TYPE IDENTIFIER ( "=" expression )? ";" ;
ArrayDecl       → 'array' TYPE IDENTIFIER '[' Size ']' ('=' Initializer)? ';'
block           → "{" declaration* "}" ;
printStmt       → "print" "(" expression ")" ";"; 
condition       → if "(" expression ")" statement 
                 ( "else" statement )? ;
whileStmt       → "while" "(" expression ")" statement ;
forStmt         → "for" "(" ( varDecl | exprStmt | ";" )
                 expression? ";"
                 expression? ")" statement ;