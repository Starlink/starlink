%token IDENTIFIER CONSTANT STRING_LITERAL SIZEOF

%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
%token STRUCT UNION ENUM ELIPSIS RANGE

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token F77_FUNCTION_NAME F77_SUBROUTINE_NAME F77_EXTERNAL_NAME FUNC_NAME
%token ILLEGALCHAR

%start file

%{
#define YYSTYPE char *
%}

%%

file
	: external_definition
	| file external_definition
	;

external_definition
	: f77_macro_name '(' identifier ')' '(' nonexecutable_code ')' function_body
	| f77_macro_name '(' identifier ')' '(' nonexecutable_code ')' ';'
	| FUNC_NAME '(' nonexecutable_code ')' function_body
	| FUNC_NAME '(' nonexecutable_code ')' ';'
	| FUNC_NAME '(' ')' function_body
	| FUNC_NAME '(' ')' ';'
	| declaration_word
	| ';'
	;

function_body
	: '{' executable_code '}'
	| declaration_statement function_body
	;

declaration_statement
	: reserved declaration_wordlist ';'
	| identifier declaration_wordlist ';'
	;

declaration_wordlist
	: declaration_word
	| declaration_wordlist declaration_word
	;

declaration_word
	: reserved
	| identifier
	| CONSTANT
	| STRING_LITERAL
	| '{' nonexecutable_code '}'
	| '(' nonexecutable_code ')'
	| otherchar
	;

nonexecutable_code
	: nonexecutable_item
	| nonexecutable_code nonexecutable_item
	;

nonexecutable_item
	: declaration_word
	| FUNC_NAME '(' nonexecutable_code ')'
	| ';'
	;

executable_code
	: executable_item
	| executable_code executable_item
	;

executable_item
	: f77_macro_name '(' identifier ')' 
	| FUNC_NAME '(' executable_code ')'
	| FUNC_NAME '(' ')'
	| reserved
	| identifier
	| CONSTANT
	| STRING_LITERAL
	| '{' executable_code '}'
	| '(' executable_code ')'
	| ';'
	| otherchar
	;

f77_macro_name
	: F77_FUNCTION_NAME
	| F77_SUBROUTINE_NAME
	| F77_EXTERNAL_NAME
	;

otherchar
	: other_legal_char
	| ILLEGALCHAR
	;

other_legal_char  /*  This excludes parentheses, braces and semicolon  */
	: ',' | ':' | '=' | '[' | ']' | '.' | '&' | '!' | '~'
	| '-' | '+' | '*' | '/' | '%' | '^' | '|' | '?' | '<' | '>'
	;

reserved
	: AUTO | BREAK | CASE | CHAR | CONST | CONTINUE | DEFAULT
	| DO | DOUBLE | ELSE | ENUM | EXTERN | FLOAT | FOR | GOTO
	| IF | INT | LONG | REGISTER | RETURN | SHORT | SIGNED 
	| SIZEOF | STATIC | STRUCT | SWITCH | TYPEDEF | UNION 
	| UNSIGNED | VOID | VOLATILE | WHILE
	;

identifier
	: IDENTIFIER
	;

%%
#include <stdio.h>

extern char yytext[];
extern int column;

yyerror(s)
char *s;
{
        fflush(stdout);
        printf("\n%*s\n%*s\n", column, "^", column, s);
}





