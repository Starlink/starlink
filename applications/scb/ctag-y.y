%token IDENTIFIER CONSTANT STRING_LITERAL SIZEOF

%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
%token STRUCT UNION ENUM ELIPSIS RANGE

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token F77_FUNCTION_OPEN F77_SUBROUTINE_OPEN F77_EXTERNAL_OPEN ILLEGALCHAR
%token FUNC_OPEN

%token SIZEOF_OPEN IF_OPEN WHILE_OPEN FOR_OPEN SWITCH_OPEN


%start file
%%

file
	: external_definition
	| file external_definition
	;

external_definition
	: f77_macro_open identifier ')' '(' nonexecutable_code ')' function_body
	| f77_macro_open identifier ')' '(' nonexecutable_code ')' ';'
	| FUNC_OPEN nonexecutable_code ')' function_body
	| FUNC_OPEN nonexecutable_code ')' ';'
	| FUNC_OPEN ')' function_body
	| FUNC_OPEN ')' ';'
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
	: reserved
	| identifier
	| CONSTANT
	| STRING_LITERAL
	| '{' nonexecutable_code '}'
	| '(' nonexecutable_code ')'
	| FUNC_OPEN nonexecutable_code ')'
	| SIZEOF_OPEN nonexecutable_code ')'
	| ';'
	| otherchar
	;

executable_code
	: executable_item
	| executable_code executable_item
	;

executable_item
	: f77_macro_open identifier ')' 
	| FUNC_OPEN executable_code ')'
	| FUNC_OPEN ')'
	| reserved
	| identifier
	| CONSTANT
	| STRING_LITERAL
	| '{' executable_code '}'
	| '(' executable_code ')'
	| reserved_open executable_code ')'
	| ';'
	| otherchar
	;

f77_macro_open
	: F77_FUNCTION_OPEN
	| F77_SUBROUTINE_OPEN
	| F77_EXTERNAL_OPEN
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

reserved_open
	: SIZEOF_OPEN | IF_OPEN | WHILE_OPEN | FOR_OPEN | SWITCH_OPEN
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





