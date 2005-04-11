%pointer


%{
/*
*+
*  Name:
*    ftok.l
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
*     corresponding grammar parser.
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
*     The tokeniser built by this file is insufficiently precise to 
*     parse all the fortran source.  It is also more complex than it
*     needs to be for the purposes of inscnf.
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
*     24-JAN-2000 (MBT):
*        Adapted for use with EXTREME.
*-
*/

#include <stdlib.h>

void count();
void munch();

#include "ygen.h"
#include "ftokens.h"

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
LETTER			[A-Za-z<>$_]
DIGIT			[0-9]
SPACE			[ \t]*

%start	FIRST

%p 5000
%a 5000
%o 6000

%%


^[0-9 ]{5,5}[ 0]{SPACE}*	{ count(); BEGIN FIRST; return( LINE_START ); }
^\t{SPACE}*			{ count(); BEGIN FIRST; return( LINE_START ); }
^[0-9 ]{5,5}[ 0]{SPACE}*(!.*)?\n	{ count(); return( BLANK_LINE ); }
^[ \t]*(!.*)?\n			{ count(); return( BLANK_LINE ); }
^[CcDd!*].*\n			{ count(); return( COMMENT_LINE ); }
(!.*)?\n			{ count(); return( LINE_END ); }


<FIRST>{S}{U}{B}{R}{O}{U}{T}{I}{N}{E}	{ count(); return( SUBROUTINE ); }
<FIRST>{E}{N}{T}{R}{Y}			{ count(); return( ENTRY ); }
<FIRST>{B}{L}{O}{C}{K}{SPACE}{D}{A}{T}{A}	{ count(); return( BLOCKDATA ); }
<FIRST>{P}{R}{O}{G}{R}{A}{M}		{ count(); return( PROGRAM ); }
<FIRST>{I}{N}{C}{L}{U}{D}{E}		{ count(); return( INCLUDE ); }
<FIRST>{E}{L}{S}{E}{SPACE}{I}{F}	{ count(); return( ELSEIF ); }
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
<FIRST>{E}{Q}{U}{I}{V}{A}{L}{E}{N}{C}{E}	{
					  count(); return( EQUIVALENCE ); }
<FIRST>"<"{T}{Y}{P}{E}">"		{ count(); return( GENERIC_TYPE ); }
<FIRST>{E}{N}{D}			{ count(); return( END ); }
<FIRST>{I}{M}{P}{L}{I}{C}{I}{T}{SPACE}{N}{O}{N}{E}	{ 
					  count(); return( IMPLICITNONE ); }
<FIRST>{I}{M}{P}{L}{I}{C}{I}{T}		{ count(); return( IMPLICIT ); }

{F}{U}{N}{C}{T}{I}{O}{N}		{ count(); return( FUNCTION ); }
{I}{F}					{ count(); return( IF ); }
{C}{A}{L}{L}				{ count(); return( CALL ); }

{V}{A}{L}				{ count(); return( VAL ); }
{C}{N}{F}"_"{P}{V}{A}{L}		{ count(); return( CNF_PVAL ); }
{I}{O}{S}{T}{A}{T}			{ count(); return( IOSTAT ); }

{I}{A}{B}{S}				{ count(); return( IABS ); }
{I}{S}{I}{G}{N}				{ count(); return( ISIGN ); }
{I}{D}{I}{M}				{ count(); return( IDIM ); }
{M}{A}{X}0				{ count(); return( MAX0 ); }
{A}{M}{A}{X}0				{ count(); return( AMAX0 ); }
{M}{I}{N}0				{ count(); return( MIN0 ); }
{A}{M}{I}{N}0				{ count(); return( AMIN0 ); }

{DIGIT}+				{ count(); return( INTEGER_CONSTANT ); }
\'(''|[^'\n]|{CONT})*\'			{ count(); return( STRING_CONSTANT ); }

{LETTER}({LETTER}|{DIGIT})*		{ count(); return( IDENTIFIER ); }

"%"					{ count(); return( '%' ); }
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
"&"					{ count(); return( ILLEGAL_CHAR ); }

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
*     there is no further text to come.  Housekeeping may also be done.
*-
*/

/* If there is any text which has been encountered but not yet associated
   with a token, add it to the encountered text string, since if it hasn't
   been associated with a token yet it will never be. */
      if ( preleng > 0 ) {
         yyleng = preleng;
         yylval = (char *) memok( malloc( yyleng + 1 ) );
         strcpy( yylval, preval );
         preleng = 0;
         prealloc = 0;
      }
      else {
         yyleng = 0;
         yylval = NULL;
      }
/* Return 1 to the caller. */
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
*     It also registers that the text has been encountered with the list
*     of unoutput strings.
*-
*/

/* Append preval and the matched text. */
      yyleng = preleng + strlen( (char *) yytext );
      yylval = (char *) memok( malloc( yyleng + 1 ) );
      ymatst = yylval + preleng;
      if ( preleng > 0 ) {
         strcpy( yylval, preval );
         strcat( yylval, (char *) yytext );
      }
      else {
         strcpy( yylval, (char *) yytext );
      }

/* Reclaim space used by preval if necessary. */
      if ( prealloc > 0 ) {
         free( preval );
         preleng = 0;
         prealloc = 0;
      }

/* Set to the normal (i.e. not first token in the line) lex start state. */
      BEGIN 0;
   }



   void munch() {
      sappend( (char *) yytext );
   }



/* $Id$ */
