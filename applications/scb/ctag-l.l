D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*
S			[ \t\v\n\f]*

%{
#include <stdio.h>
#include "y.tab.h"

void count();
%}

%%
^#include[ \t]+		{ include(); }
^#			{ cpp(); }

"/*"			{ comment(); }

"auto"			{ count(); return(AUTO); }
"break"			{ count(); return(BREAK); }
"case"			{ count(); return(CASE); }
"char"			{ count(); return(CHAR); }
"const"			{ count(); return(CONST); }
"continue"		{ count(); return(CONTINUE); }
"default"		{ count(); return(DEFAULT); }
"do"			{ count(); return(DO); }
"double"		{ count(); return(DOUBLE); }
"else"			{ count(); return(ELSE); }
"enum"			{ count(); return(ENUM); }
"extern"		{ count(); return(EXTERN); }
"float"			{ count(); return(FLOAT); }
"for"			{ count(); return(FOR); }
"goto"			{ count(); return(GOTO); }
"if"			{ count(); return(IF); }
"int"			{ count(); return(INT); }
"long"			{ count(); return(LONG); }
"register"		{ count(); return(REGISTER); }
"return"		{ count(); return(RETURN); }
"short"			{ count(); return(SHORT); }
"signed"		{ count(); return(SIGNED); }
"sizeof"		{ count(); return(SIZEOF); }
"static"		{ count(); return(STATIC); }
"struct"		{ count(); return(STRUCT); }
"switch"		{ count(); return(SWITCH); }
"typedef"		{ count(); return(TYPEDEF); }
"union"			{ count(); return(UNION); }
"unsigned"		{ count(); return(UNSIGNED); }
"void"			{ count(); return(VOID); }
"volatile"		{ count(); return(VOLATILE); }
"while"			{ count(); return(WHILE); }

F77_SUBROUTINE{S}\(	{ count(); return(F77_SUBROUTINE_OPEN); }
F77_[A-Z]+_FUNCTION{S}\(	{ count(); return(F77_FUNCTION_OPEN); }
F77_EXTERNAL_NAME{S}\(	{ count(); return(F77_EXTERNAL_OPEN); }

{L}({L}|{D})*{S}\(	{ count(); return(FUNC_OPEN); }
{L}({L}|{D})*		{ count(); return(identifier()); }

0[xX]{H}+{IS}?		{ count(); return(CONSTANT); }
0{D}+{IS}?		{ count(); return(CONSTANT); }
{D}+{IS}?		{ count(); return(CONSTANT); }
'(\\.|[^\\'])+'		{ count(); return(CONSTANT); }

{D}+{E}{FS}?		{ count(); return(CONSTANT); }
{D}*"."{D}+({E})?{FS}?	{ count(); return(CONSTANT); }
{D}+"."{D}*({E})?{FS}?	{ count(); return(CONSTANT); }

\"(\\.|[^\\"])*\"	{ count(); return(STRING_LITERAL); }

";"                     { count(); return(';'); }
"{"                     { count(); return('{'); }
"}"                     { count(); return('}'); }
","                     { count(); return(','); }
":"                     { count(); return(':'); }
"="                     { count(); return('='); }
"("                     { count(); return('('); }
")"                     { count(); return(')'); }
"["                     { count(); return('['); }
"]"                     { count(); return(']'); }
"."                     { count(); return('.'); }
"&"                     { count(); return('&'); }
"!"                     { count(); return('!'); }
"~"                     { count(); return('~'); }
"-"                     { count(); return('-'); }
"+"                     { count(); return('+'); }
"*"                     { count(); return('*'); }
"/"                     { count(); return('/'); }
"%"                     { count(); return('%'); }
"<"                     { count(); return('<'); }
">"                     { count(); return('>'); }
"^"                     { count(); return('^'); }
"|"                     { count(); return('|'); }
"?"                     { count(); return('?'); }

[ \t\v\n\f]		{ count(); }
.                       { count(); return(ILLEGALCHAR); }

%%

yywrap()
{
	return(1);
}

#define LENG 100

include()
{
    char delim, c;
    char filename[LENG];
    int i;

    ECHO;
    delim = nextch();
    putchar(delim);

    i = 0;
    while ((c = nextch()) != delim) {
        if (c == '\n' || i >= LENG) break;
        filename[i++] = c;
    }
    filename[i] = '\0';
    if (delim == '"')
        printf("<a name='INCLUDE-%s'>%s</a>", filename, filename);
    else
        printf("%s", filename);
    putchar(c);
    while ((c = nextch()) != '\n') 
        putchar(c);
    putchar(c);
}

cpp()
{
    char c;

    ECHO;
    while ((c = nextch()) != '\n')
        putchar(c);
    putchar(c);
}

nextch()
{
    char c,c1;
    c = input();
    if (c == '\\') {
        c1 = input();
        if (c1 == '\n') {
            putchar(c);
            putchar(c1);
            return(nextch());
        } else {
            unput(c1);
            return(c);
        }
    }
}

comment()
{
	char c, c1;

	ECHO;
loop:
	while ((c = input()) != '*' && c != 0)
		putchar(c);
	putchar(c);

	if ((c1 = input()) != '/' && c != 0)
	{
		unput(c1);
		goto loop;
	}

	if (c != 0)
		putchar(c1);
}

int column = 0;

void count()
{
	int i;

	for (i = 0; yytext[i] != '\0'; i++)
		if (yytext[i] == '\n')
			column = 0;
		else if (yytext[i] == '\t')
			column += 8 - (column % 8);
		else
			column++;

	ECHO; 
}


#include <string.h>

char id_name[LENG];
int identifier() 
{
    strncpy(id_name, yytext, LENG);
    return(IDENTIFIER);
}
