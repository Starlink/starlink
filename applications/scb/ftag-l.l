
CONT			(\n.....[^ 0\n])
BLANK			([ \t]|!.*|{CONT})*
LETTER			[A-Za-z$_%]{BLANK}
DIGIT			[0-9]{BLANK}
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


%Start	FIRST
%{

void count();

%}

%%

^[0-9 ]{5,5}[ 0]{BLANK}		{ count(); BEGIN FIRST; return( LINE_START ); }
^[0-9 ]{5,5}[ 0]{BLANK}\n	{ count(); return( BLANK_LINE ); }
^{BLANK}\n			{ count(); return( BLANK_LINE ); }
^[Cc!].*\n			{ count(); return( COMMENT_LINE ); }
\n				{ count(); return( LINE_END ); }


<FIRST>{S}{U}{B}{R}{O}{U}{T}{I}{N}{E}	{ count(); return( SUBROUTINE ); }
<FIRST>{E}{N}{T}{R}{Y}			{ count(); return( ENTRY ); }
<FIRST>{B}{L}{O}{C}{K}{BLANK}{D}{A}{T}{A}	{ count(); return( BLOCKDATA ); }
<FIRST>{P}{R}{O}{G}{R}{A}{M}		{ count(); return( PROGRAM ); }
<FIRST>{I}{N}{C}{L}{U}{D}{E}		{ count(); return( INCLUDE ); }
<FIRST>{E}{L}{S}{E}{BLANK}{I}{F}	{ count(); return( ELSEIF ); }
<FIRST>{T}{H}{E}{N}			{ count(); return( THEN ); }
<FIRST>{I}{N}{T}{E}{G}{E}{R}		{ count(); return( INTEGER ); }
<FIRST>{R}{E}{A}{L}			{ count(); return( REAL ); }
<FIRST>{D}{O}{U}{B}{L}{E}{BLANK}{P}{R}{E}{C}{I}{S}{I}{O}{N}	{ 
					  count(); return( DOUBLEPRECISION ); }
<FIRST>{C}{O}{M}{P}{L}{E}{X}		{ count(); return( COMPLEX ); }
<FIRST>{L}{O}{G}{I}{C}{A}{L}		{ count(); return( LOGICAL ); }
<FIRST>{C}{H}{A}{R}{A}{C}{T}{E}{R}	{ count(); return( CHARACTER ); }
<FIRST>{B}{Y}{T}{E}			{ count(); return( BYTE ); }
<FIRST>{U}{B}{Y}{T}{E}			{ count(); return( UBYTE ); }
<FIRST>{W}{O}{R}{D}			{ count(); return( WORD ); }
<FIRST>{U}{W}{O}{R}{D}			{ count(); return( UWORD ); }
<FIRST>{D}{I}{M}{E}{N}{S}{I}{O}{N}	{ count(); return( DIMENSION ); }
<FIRST>"<"[Tt][Yy][Pp][Ee]">"{BLANK}	{ count(); return( GENERIC_TYPE ); }

{F}{U}{N}{C}{T}{I}{O}{N}		{ count(); return( FUNCTION ); }
{I}{F}/\(				{ count(); return( IF ); }
{C}{A}{L}[Ll]([ \t]|!.*|{CONT})+	{ count(); return( CALL ); }


{DIGIT}+				{ count(); return( INTEGER_CONSTANT ); }
"'"(''|[^'\n]|{CONT})"'"		{ count(); return( STRING_CONSTANT ); }

{LETTER}({LETTER}|{DIGIT})*		{ count(); return( TOKEN ); }

":"{BLANK}				{ count(); return( ':' ); }
"("{BLANK}				{ count(); return( '(' ); }
")"{BLANK}				{ count(); return( ')' ); }
"+"{BLANK}				{ count(); return( '+' ); }
"-"{BLANK}				{ count(); return( '0' ); }
"*"{BLANK}				{ count(); return( '*' ); }
"/"{BLANK}				{ count(); return( '/' ); }
"."{BLANK}				{ count(); return( '.' ); }
","{BLANK}				{ count(); return( ',' ); }
"'"{BLANK}				{ count(); return( '\'' ); }

.{BLANK}				{ count(); return( ILLEGAL_CHAR ); }


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


