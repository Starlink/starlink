
CONT			(\n[^cCdD!*\n]....[^ 0\n])
BLANK			([ \t]|!.*|{CONT}|\n[ \t]*!.*)*
LETTER			[A-Za-z$_%]
DIGIT			[0-9]
A			[Aa]
B			[Bb]
C			[Cc]
D			[Dd]
E			[Ee]
F			[Ff]
G			[Gg]
H			[Hh]
I			[Ii]
J			[Jj]
K			[Kk]
L			[Ll]
M			[Mm]
N			[Nn]
O			[Oo]
P			[Pp]
Q			[Qq]
R			[Rr]
S			[Ss]
T			[Tt]
U			[Uu]
V			[Vv]
W			[Ww]
X			[Xx]
Y			[Yy]
Z			[Zz]


%START	FIRST
%{

void count();

%}

%%

^[0-9 ]{5,5}[ 0]{BLANK}		{ count(); BEGIN FIRST; return( LINE_START ); }
^[0-9 ]{5,5}[ 0]{BLANK}\n	{ count(); return( BLANK_LINE ); }
^{BLANK}\n			{ count(); return( BLANK_LINE ); }
^[CcDd!*].*\n			{ count(); return( COMMENT_LINE ); }
\n				{ count(); return( LINE_END ); }


<FIRST>{S}{U}{B}{R}{O}{U}{T}{I}{N}{E}{BLANK}	{ count(); return( SUBROUTINE ); }
<FIRST>{E}{N}{T}{R}{Y}{BLANK}			{ count(); return( ENTRY ); }
<FIRST>{B}{L}{O}{C}{K}{BLANK}{D}{A}{T}{A}{BLANK}	{ count(); return( BLOCKDATA ); }
<FIRST>{P}{R}{O}{G}{R}{A}{M}{BLANK}		{ count(); return( PROGRAM ); }
<FIRST>{I}{N}{C}{L}{U}{D}{E}{BLANK}		{ count(); return( INCLUDE ); }
<FIRST>{E}{L}{S}{E}{BLANK}{I}{F}{BLANK}		{ count(); return( ELSEIF ); }
<FIRST>{T}{H}{E}{N}{BLANK}			{ count(); return( THEN ); }
<FIRST>{I}{N}{T}{E}{G}{E}{R}{BLANK}		{ count(); return( INTEGER ); }
<FIRST>{R}{E}{A}{L}{BLANK}			{ count(); return( REAL ); }
<FIRST>{D}{O}{U}{B}{L}{E}{BLANK}{P}{R}{E}{C}{I}{S}{I}{O}{N}{BLANK}	{ 
					  count(); return( DOUBLEPRECISION ); }
<FIRST>{C}{O}{M}{P}{L}{E}{X}{BLANK}		{ count(); return( COMPLEX ); }
<FIRST>{L}{O}{G}{I}{C}{A}{L}{BLANK}		{ count(); return( LOGICAL ); }
<FIRST>{C}{H}{A}{R}{A}{C}{T}{E}{R}{BLANK}	{ count(); return( CHARACTER ); }
<FIRST>{B}{Y}{T}{E}{BLANK}			{ count(); return( BYTE ); }
<FIRST>{U}{B}{Y}{T}{E}{BLANK}			{ count(); return( UBYTE ); }
<FIRST>{W}{O}{R}{D}{BLANK}			{ count(); return( WORD ); }
<FIRST>{U}{W}{O}{R}{D}{BLANK}			{ count(); return( UWORD ); }
<FIRST>{D}{I}{M}{E}{N}{S}{I}{O}{N}{BLANK}	{ count(); return( DIMENSION ); }
<FIRST>"<"{T}{Y}{P}{E}">"{BLANK}		{ count(); return( GENERIC_TYPE ); }

{F}{U}{N}{C}{T}{I}{O}{N}{BLANK}			{ count(); return( FUNCTION ); }
{I}{F}{BLANK}/\(				{ count(); return( IF ); }
{C}{A}{L}{L}{BLANK}				{ count(); return( CALL ); }


{DIGIT}+{BLANK}				{ count(); return( INTEGER_CONSTANT ); }
\'(''|[^'\n]|{CONT})*\'{BLANK}		{ count(); return( STRING_CONSTANT ); }

{LETTER}({LETTER}|{DIGIT})*{BLANK}/\(	{ count(); return( TOKEN_BRAC ); }
{LETTER}({LETTER}|{DIGIT})*{BLANK}	{ count(); return( TOKEN ); }

":"{BLANK}				{ count(); return( ':' ); }
"("{BLANK}				{ count(); return( '(' ); }
")"{BLANK}				{ count(); return( ')' ); }
"+"{BLANK}				{ count(); return( '+' ); }
"-"{BLANK}				{ count(); return( '-' ); }
"*"{BLANK}				{ count(); return( '*' ); }
"/"{BLANK}				{ count(); return( '/' ); }
"."{BLANK}				{ count(); return( '.' ); }
","{BLANK}				{ count(); return( ',' ); }
"="{BLANK}				{ count(); return( '=' ); }

.([ \t]|{CONT}){BLANK}			{ count(); return( ILLEGAL_CHAR ); }
[^'\n]{BLANK}				{ count(); return( ILLEGAL_CHAR ); }


%%

#include <stdlib.h>
#include <string.h>

   int yywrap() {
      return(1);
   }


   void count() {
      yylval = malloc( yyleng + 1 );
      strcpy( yylval, yytext );
      BEGIN 0;
   }


