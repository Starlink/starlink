%pointer

%{
/*+
* Name:
*    ftag-l.l
*
*  Type of module:
*     lex source
*
*  Purpose:
*     Tokenise Fortran 77 source code for feeding to parser.
*
*  Description:
*     This file contains the lex source code to build a lexical analyser
*     for tokenising Fortran 77 source code well enough to feed it to a 
*     corresponding grammar parser (written as yacc).
*
*     The type of yylval returned value is in all cases pointer to char.
*     The text thus pointed to contains all the text of the original fortran
*     source file in such a way that if the text for each token which is
*     passed to yacc is output as it is encountered, then the original
*     fortran source file will be reconstructed with the following 
*     HTML-like addition:
*        - The characters '<', '>' and '&' are replaced by their HTML
*          entity reference equivalents.
*
*     The space pointed to by yylvals has been malloc'd.  It should be
*     freed by the parser or memory will leak.
*
*     Additionally, the first line_leng characters of the static buffer 
*     line_text contains all the text so far encountered since the last
*     LINE_START token.  This is required by the parser for proper
*     error recovery.
*
*  Timing:
*     Probably the C output by lex would be considerably smaller, and 
*     perhaps more efficient, if the BLANKs were not scooped up as part
*     of each matched pattern.  lex doesn't seem to do this in a very
*     efficient way, it would likely be better to match the tokens 
*     without blanks and match the blanks separately adding these into
*     the yylval without the pattern-matching machine's knowledge. 
*     This would (I think) require matching blanks before, not after,
*     each pattern, and hence some restructuring.  See the C lexer
*     (ctag-l.l) for an example of this approach.
*
*  Bugs:
*     Blanks within, for instance, reserved words are not ignored as
*     they really should be within Fortran 77 (except in cases where 
*     one expects them to maybe crop up).
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     26-NOV-1999 (MBT):
*        Initial version.
*-
*/

void count();
void ecount();
void munch();

%}

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

CONT			(\n[^cCdD!*\n]....[^ 0\n])
BLANK			([ \t]|(!.*)?{CONT}|(!.*)?\n[ \t]*!.*)*
LETTER			[A-Za-z<>$_%]
DIGIT			[0-9]
SPACE			[ \t]*

%start	FIRST

%p 5000
%a 5000
%o 5000

%%


^[0-9 ]{5,5}[ 0]{SPACE}*	{ count(); BEGIN FIRST; return( LINE_START ); }
^[0-9 ]{5,5}[ 0]{SPACE}*(!.*)?\n	{ ecount(); return( BLANK_LINE ); }
^[ \t]*(!.*)?\n			{ ecount(); return( BLANK_LINE ); }
^[CcDd!*].*\n			{ ecount(); return( COMMENT_LINE ); }
(!.*)?\n			{ ecount(); return( LINE_END ); }


<FIRST>{S}{U}{B}{R}{O}{U}{T}{I}{N}{E}	{ count(); return( SUBROUTINE ); }
<FIRST>{E}{N}{T}{R}{Y}			{ count(); return( ENTRY ); }
<FIRST>{B}{L}{O}{C}{K}{SPACE}{D}{A}{T}{A}	{ count(); return( BLOCKDATA ); }
<FIRST>{P}{R}{O}{G}{R}{A}{M}		{ count(); return( PROGRAM ); }
<FIRST>{I}{N}{C}{L}{U}{D}{E}		{ count(); return( INCLUDE ); }
<FIRST>{E}{L}{S}{E}{SPACE}{I}{F}/{SPACE}\(	{ count(); return( ELSEIF ); }
<FIRST>{T}{H}{E}{N}			{ count(); return( THEN ); }
<FIRST>{I}{N}{T}{E}{G}{E}{R}		{ count(); return( INTEGER ); }
<FIRST>{R}{E}{A}{L}			{ count(); return( REAL ); }
<FIRST>{D}{O}{U}{B}{L}{E}{SPACE}{P}{R}{E}{C}{I}{S}{I}{O}{N}	{ 
					  count(); return( DOUBLEPRECISION ); }
<FIRST>{C}{O}{M}{P}{L}{E}{X}		{ count(); return( COMPLEX ); }
<FIRST>{L}{O}{G}{I}{C}{A}{L}		{ count(); return( LOGICAL ); }
<FIRST>{C}{H}{A}{R}{A}{C}{T}{E}{R}	{ count(); return( CHARACTER ); }
<FIRST>{B}{Y}{T}{E}			{ count(); return( BYTE ); }
<FIRST>{U}{B}{Y}{T}{E}			{ count(); return( UBYTE ); }
<FIRST>{W}{O}{R}{D}			{ count(); return( WORD ); }
<FIRST>{U}{W}{O}{R}{D}			{ count(); return( UWORD ); }
<FIRST>{D}{I}{M}{E}{N}{S}{I}{O}{N}	{ count(); return( DIMENSION ); }
<FIRST>"<"{T}{Y}{P}{E}">"		{ ecount(); return( GENERIC_TYPE ); }

{F}{U}{N}{C}{T}{I}{O}{N}		{ count(); return( FUNCTION ); }
{I}{F}/{SPACE}\(			{ count(); return( IF ); }
{C}{A}{L}{L}				{ count(); return( CALL ); }

{DIGIT}+				{ count(); return( INTEGER_CONSTANT ); }
\'(''|[^'\n]|{CONT})*\'			{ ecount(); return( STRING_CONSTANT ); }

{LETTER}({LETTER}|{DIGIT})*/{BLANK}\(	{ ecount(); return( TOKEN_BRAC ); }
{LETTER}({LETTER}|{DIGIT})*		{ ecount(); return( TOKEN ); }

":"					{ count(); return( ':' ); }
"("					{ count(); return( '(' ); }
")"					{ count(); return( ')' ); }
"+"					{ count(); return( '+' ); }
"-"					{ count(); return( '-' ); }
"*"					{ count(); return( '*' ); }
"/" 					{ count(); return( '/' ); }
"."					{ count(); return( '.' ); }
","					{ count(); return( ',' ); }
"="					{ count(); return( '=' ); }
"<"					{ ecount(); return( ILLEGAL_CHAR ); }
">"					{ ecount(); return( ILLEGAL_CHAR ); }
"&"					{ ecount(); return( ILLEGAL_CHAR ); }

{BLANK}					{ munch(); }

.					{ count(); return( ILLEGAL_CHAR ); }


%%

#include <stdlib.h>
#include <string.h>


   int yywrap() {
/*
*+
*  Name:
*     yywrap
*
*  Purpose:
*     Do necessary tidying up at end of lex input file.
*
*  Description:
*     This routine has to return 1 to the caller (yylex) to indicate that
*     there is no further text to come.  Additionally here, we output
*     directly any text in preval, since if it hasn't yet been returned
*     to the parser, it won't be returned at all.
*-
*/
      if ( preleng > 0 ) {
         uadd( preval );
         preleng = 0;
         prealloc = 0;
      }
      return(1);
   }


   void count() {
/*
*+
*  Name:
*     count
*
*  Purpose:
*     Ensure that yylval has all the proper text.
*
*  Description:
*     This routine sets yylval (the value which is passed up to the parser)
*     to the string of all the characters which have been encountered
*     since the last token was passed back.  Note that it mallocs storage
*     for this purpose, so this must be free'd at some point by the parser.
*
*     SGML-like entity references are substituted in where necessary.
*-
*/

/* Append preval and the matched text. */
      yyleng = preleng + strlen( yytext );
      yylval = (char *) memok( malloc( yyleng + 1 ) );
      if ( preleng > 0 ) {
         strcpy( yylval, preval );
         strcat( yylval, yytext );
      }
      else {
         strcpy( yylval, yytext );
      }

/* Reclaim space used by preval if necessary. */
      if ( prealloc > 0 ) {
         free( preval );
         preleng = 0;
         prealloc = 0;
      }

/* Store the position of this encountered string in the global list. */
      uadd( yylval );

/* Set to the normal (i.e. not first token in the line) lex start state. */
      BEGIN 0;
   }


   void ecount() {
/*
*+
*  Name:
*     ecount
*
*  Purpose:
*     Set yylval substituting entity references.
*-
*/
      int i;
      for ( i = 0; i < strlen( yytext ); i++ )
         cappend( yytext[ i ] );
      *yytext = '\0';
      yyleng = 0;
      count();
   }


   void munch() {
      sappend( yytext );
   }



/* $Id$ */
