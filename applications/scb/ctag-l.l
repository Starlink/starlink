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
^#include{S}		{ include(); }
^#define{S}		{ cpp(); }
^#			{ cpp(); }

"/*"			{ comment(); }

"auto"{S}		{ count(); return(AUTO); }
"break"{S}		{ count(); return(BREAK); }
"case"{S}		{ count(); return(CASE); }
"char"{S}		{ count(); return(CHAR); }
"const"{S}		{ count(); return(CONST); }
"continue"{S}		{ count(); return(CONTINUE); }
"default"{S}		{ count(); return(DEFAULT); }
"do"{S}			{ count(); return(DO); }
"double"{S}		{ count(); return(DOUBLE); }
"else"{S}		{ count(); return(ELSE); }
"enum"{S}		{ count(); return(ENUM); }
"extern"{S}		{ count(); return(EXTERN); }
"float"{S}		{ count(); return(FLOAT); }
"for"{S}		{ count(); return(FOR); }
"goto"{S}		{ count(); return(GOTO); }
"if"{S}			{ count(); return(IF); }
"int"{S}		{ count(); return(INT); }
"long"{S}		{ count(); return(LONG); }
"register"{S}		{ count(); return(REGISTER); }
"return"{S}		{ count(); return(RETURN); }
"short"{S}		{ count(); return(SHORT); }
"signed"{S}		{ count(); return(SIGNED); }
"sizeof"{S}		{ count(); return(SIZEOF); }
"static"{S}		{ count(); return(STATIC); }
"struct"{S}		{ count(); return(STRUCT); }
"switch"{S}		{ count(); return(SWITCH); }
"typedef"{S}		{ count(); return(TYPEDEF); }
"union"{S}		{ count(); return(UNION); }
"unsigned"{S}		{ count(); return(UNSIGNED); }
"void"{S}		{ count(); return(VOID); }
"volatile"{S}		{ count(); return(VOLATILE); }
"while"{S}		{ count(); return(WHILE); }

F77_SUBROUTINE/{S}\({S}		{ count(); return(F77_SUBROUTINE_NAME); }
F77_[A-Z]+_FUNCTION/{S}\({S}	{ count(); return(F77_FUNCTION_NAME); }
F77_EXTERNAL_NAME/{S}\({S}	{ count(); return(F77_EXTERNAL_NAME); }

{L}({L}|{D})*/{S}\({S}	{ count(); return(FUNC_NAME); }
{L}({L}|{D})*{S}	{ count(); return(identifier()); }

0[xX]{H}+{IS}?{S}	{ count(); return(CONSTANT); }
0{D}+{IS}?{S}		{ count(); return(CONSTANT); }
{D}+{IS}?{S}		{ count(); return(CONSTANT); }
'(\\.|[^\\'])+'{S}		{ count(); return(CONSTANT); }

{D}+{E}{FS}?{S}			{ count(); return(CONSTANT); }
{D}*"."{D}+({E})?{FS}?{S}	{ count(); return(CONSTANT); }
{D}+"."{D}*({E})?{FS}?{S}	{ count(); return(CONSTANT); }

\"(\\.|[^\\"])*\"{S}	{ count(); return(STRING_LITERAL); }

";"{S}			{ count(); return(';'); }
"{"{S}			{ count(); return('{'); }
"}"{S}			{ count(); return('}'); }
","{S}			{ count(); return(','); }
":"{S}			{ count(); return(':'); }
"="{S}			{ count(); return('='); }
"("{S}			{ count(); return('('); }
")"{S}			{ count(); return(')'); }
"["{S}			{ count(); return('['); }
"]"{S}			{ count(); return(']'); }
"."{S}			{ count(); return('.'); }
"&"{S}			{ count(); return('&'); }
"!"{S}			{ count(); return('!'); }
"~"{S}			{ count(); return('~'); }
"-"{S}			{ count(); return('-'); }
"+"{S}			{ count(); return('+'); }
"*"{S}			{ count(); return('*'); }
"/"{S}			{ count(); return('/'); }
"%"{S}			{ count(); return('%'); }
"<"{S}			{ count(); return('<'); }
">"{S}			{ count(); return('>'); }
"^"{S}			{ count(); return('^'); }
"|"{S}			{ count(); return('|'); }
"?"{S}			{ count(); return('?'); }

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
