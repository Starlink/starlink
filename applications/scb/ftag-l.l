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

CONT			(\n[^cCdD!*\n]....[^ 0\n])
BLANK			([ \t]|!.*|{CONT}|\n[ \t]*!.*)*
LETTER			[A-Za-z<>$_%]
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
void line_start();

%}

%%

^[0-9 ]{5,5}[ 0]{BLANK}		{ line_start(); BEGIN FIRST; 
				  count(); return( LINE_START ); }
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

#define MAX_LINELENG 2048
   char line_text[ MAX_LINELENG + 1 ];
   int line_length;


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
*     there is no further text to come.
*-
*/
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

/* Local variables. */
      char *entity, c;
      int leng, wleng, i, j;

/* Allocate the amount of space we will probably need (this may be an 
   underestimate, if so it will get extended later). */
      leng = yyleng;
      yylval = malloc( leng + 1 );

/* Go through token text a character at a time transferring to storage at
   yyllval. */
      for ( i = j = 0; i < yyleng; i++ ) {
         c = yytext[ i ];

/* Arrange for entity reference substitution if necessary. */
         switch( c ) {
            case '<':
               entity = "&lt;";
               wleng = 4;
               break;
            case '>':
               entity = "&gt;";
               wleng = 4;
               break;
            case '&':
               entity = "&amp;";
               wleng = 5;
               break;
            default:
               wleng = 1;
         }

/* Either copy a single character */
         if ( wleng == 1 ) {
            yylval[ j++ ] = c;
         }

/* Or copy a string (the entity reference) in which case we need to extend
   the memory allocated for the purpose. */
         else {
            wleng = strlen( entity );
            leng += wleng;
            yylval = realloc( yylval, leng + 1 );
            strcpy( yylval + j, entity );
            j += wleng;
         }
      }

/* Finish off the string. */
      yylval[ j ] = '\0';

/* Append the new token text to the line buffer.  It's unlikely to be longer
   than the buffer length; if it is, it just gets truncated. */
      strncpy( line_text + line_length, yylval, MAX_LINELENG - line_length );
      line_length += j;
      if ( line_length > MAX_LINELENG ) line_length = MAX_LINELENG;

/* Set the 'first token in the line' lex start state. */
      BEGIN 0;
   }


   void line_start() {
/*
*+
*  Name:
*     line_start
*
*  Purpose:
*     Assert that a line is starting.
*
*  Description:
*     This routine resets the line_text buffer.  The line_text buffer 
*     contains everything since the last LINE_START token was encountered.
*     For most purposes it is not used, but if the parser encounters a
*     syntax error, it's the only place that it can find the text which
*     has been read (since yacc discards states in this case).
*-
*/
      *line_text = '\0';
      line_length = 0;
   }



/* $Id$ */
