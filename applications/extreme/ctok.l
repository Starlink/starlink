%pointer

%{
/*+
* Name:
*    ctok.l
*
*  Type of module:
*     lex source
*
*  Purpose: 
*     Tokenise C source code for feeding to parser.
*
*  Description:
*     This file contains the lex source code to build a lexical analyser
*     for tokenising C source code.
*
*     The type of yylval returned value is in all cases pointer to char.
*     The text thus pointed to contains all the text of the original C
*     source file in such a way that if the text for each token which is
*     returned from yylex is output as it is encountered, then the original
*     C source file will be reconstructed exactly.
*
*     Note C preprocessor lines are tokenised in parts and returned 
*     like anything else.  This is suitable for the current purpose, 
*     but would be hopeless if one wanted to feed the tokeniser output
*     to a C grammar parser.  That case could be catered for by 
*     generating a token for 'end of preprocessor line' or something
*     (start conditions would probably be useful).
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     23-NOV-1999 (MBT):
*        Initial version.
*     25-JAN-2000 (MBT):
*        Adapted for use with EXTREME.
*-
*/

#include <stdlib.h>

void count();
void ccomm();
void cpluspluscomm();
void munch();

#include "ygen.h"
#include "ctokens.h"
 
%}


D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			[fFlL]
IS			[uUlL]*
S			[ \t\v\n\f]*
IDENT			{L}({L}|{D})*
CPP			^[ \t]*#[ \t]*


%k 1000
%p 5000
%a 3000


%%


{CPP}"include"			{ count(); return(CPP_INCLUDE); }
{CPP}"define"			{ count(); return(CPP_DEFINE); }
{CPP}"undef"			{ count(); return(CPP_UNDEF); }
{CPP}"if"			{ count(); return(CPP_IF); }
{CPP}"ifdef"			{ count(); return(CPP_IFDEF); }
{CPP}"ifndef"			{ count(); return(CPP_IFNDEF); }
{CPP}"elif"			{ count(); return(CPP_ELIF); }
{CPP}"else"			{ count(); return(CPP_ELSE); }
{CPP}"endif"			{ count(); return(CPP_ENDIF); }
{CPP}"line"			{ count(); return(CPP_LINE); }
{CPP}"error"			{ count(); return(CPP_ERROR); }
{CPP}"pragma"			{ count(); return(CPP_PRAGMA); }
{CPP}				{ count(); return(CPP_START); }

"/*"				{ ccomm(); }
"//"				{ cpluspluscomm(); }

"auto"				{ count(); return(AUTO); }
"break"				{ count(); return(BREAK); }
"case"				{ count(); return(CASE); }
"char"				{ count(); return(CHAR); }
"const"				{ count(); return(CONST); }
"continue"			{ count(); return(CONTINUE); }
"default"			{ count(); return(DEFAULT); }
"do"				{ count(); return(DO); }
"double"			{ count(); return(DOUBLE); }
"else"				{ count(); return(ELSE); }
"enum"				{ count(); return(ENUM); }
"extern"			{ count(); return(EXTERN); }
"float"				{ count(); return(FLOAT); }
"for"				{ count(); return(FOR); }
"goto"				{ count(); return(GOTO); }
"if"				{ count(); return(IF); }
"int"				{ count(); return(INT); }
"long"				{ count(); return(LONG); }
"register"			{ count(); return(REGISTER); }
"return"			{ count(); return(RETURN); }
"short"				{ count(); return(SHORT); }
"signed"			{ count(); return(SIGNED); }
"sizeof"			{ count(); return(SIZEOF); }
"static"			{ count(); return(STATIC); }
"struct"			{ count(); return(STRUCT); }
"switch"			{ count(); return(SWITCH); }
"typedef"			{ count(); return(TYPEDEF); }
"union"				{ count(); return(UNION); }
"unsigned"			{ count(); return(UNSIGNED); }
"void"				{ count(); return(VOID); }
"volatile"			{ count(); return(VOLATILE); }
"while"				{ count(); return(WHILE); }

{IDENT}				{ count(); return(IDENTIFIER); }

"..."				{ count(); return(DOTDOTDOT); }

0[xX]{H}+{IS}?			{ count(); return(CONSTANT); }
0{D}+{IS}?			{ count(); return(CONSTANT); }
{D}+{IS}?			{ count(); return(CONSTANT); }
'(\\.|[^\\'])+'			{ count(); return(CONSTANT); }

{D}+{E}{FS}?			{ count(); return(CONSTANT); }
{D}*"."{D}+({E})?{FS}?		{ count(); return(CONSTANT); }
{D}+"."{D}*({E})?{FS}?		{ count(); return(CONSTANT); }

\"(\\.|\\\n|[^\\"])*\"		{ count(); return(STRING_LITERAL); }

"#"				{ count(); return('#'); }
";"				{ count(); return(';'); }
"{"				{ count(); return('{'); }
"}"				{ count(); return('}'); }
","				{ count(); return(','); }
":"				{ count(); return(':'); }
"="				{ count(); return('='); }
"("				{ count(); return('('); }
")"				{ count(); return(')'); }
"["				{ count(); return('['); }
"]"				{ count(); return(']'); }
"."				{ count(); return('.'); }
"!"				{ count(); return('!'); }
"~"				{ count(); return('~'); }
"-"				{ count(); return('-'); }
"+"				{ count(); return('+'); }
"*"				{ count(); return('*'); }
"/"				{ count(); return('/'); }
"%"				{ count(); return('%'); }
"^"				{ count(); return('^'); }
"|"				{ count(); return('|'); }
"?"				{ count(); return('?'); }
"&"				{ count(); return('&'); }
"<"				{ count(); return('<'); }
">"				{ count(); return('>'); }

{S}				{ munch(); }
.				{ munch(); }

%%


#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include <stdio.h>
#ifdef FLEX_SCANNER
#define LEXEOF EOF
#else
#define LEXEOF 0
#endif


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
   }



   void ccomm() {
/*
*+
*  Name:
*     ccom
*
*  Purpose:
*     Store C comment text in preval.
*-
*/
      char c, c1;
      sappend( (char *) yytext );
      c = c1 = ' ';
      while ( c != LEXEOF && ( c1 != '*' || c != '/' ) ) {
         c1 = c;
         c = input();
         cappend( c );
      }
   }


   void cpluspluscomm() {
/*
*+
*  Name:
*     cpluspluscomm
*
*  Purpose:
*     Store C++ style comment in preval.
*-
*/
      char c;
      sappend( (char *) yytext );
      while( ( c = input() ) != LEXEOF && c != '\n' )
         cappend( c );
      cappend( c );
   }
         

   void munch() {
      sappend( (char *) yytext );
   }
         


/* $Id$ */
