%pointer

%{
/*+
* Name:
*    ctag-l.l
*
*  Type of module:
*     lex source
*
*  Purpose: 
*     Tokenise C source code for feeding to parser.
*
*  Description:
*     This file contains the lex source code to build a lexical analyser
*     for tokenising C source code well enough to feed it to a corresponding
*     grammar parser (written as yacc).
*
*     The type of yylval returned value is in all cases pointer to char.
*     The text thus pointed to contains all the text of the original C
*     source file in such a way that if the text for each token which is
*     passed to yacc is output as it is encountered, then the original
*     C source file will be reconstructed with in some cases some HTML-like
*     additions.  These additions are:
*        - HTML-like tags are added into some of the C preprocessor lines,
*          with the intention that no further manipulation of the C 
*          preprocessor lines needs to be done.
*        - The characters '<', '>' and '&' are replaced by their HTML
*          entity reference equivalents.
*
*  Bugs:
*     Currently the right hand side of #define lines is not scanned and
*     tagged looking for things that look like function references.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     23-NOV-1999 (MBT):
*        Initial version.
*-
*/

void count();
void comment();
void cpluspluscomm();
void cpp();
void ecount();
void define();
void include();
void munch();
 
%}


D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			[fFlL]
IS			[uUlL]*
S			[ \t\v\n\f]*
IDENT			{L}({L}|{D})*


%k 1000
%p 5000
%a 3000


%%

^#[ \t]*include[ \t]*\"			{ include(); }
^#[ \t]*define[ \t]*/{IDENT}{S}\(	{ define(); }
^#					{ cpp(); }

"/*"				{ comment(); }
"//"				{ cpluspluscomm(); }

"auto"/{S}\(?			{ count(); return(AUTO); }
"break"/{S}\(?			{ count(); return(BREAK); }
"case"/{S}\(?			{ count(); return(CASE); }
"char"/{S}\(?			{ count(); return(CHAR); }
"const"/{S}\(?			{ count(); return(CONST); }
"continue"/{S}\(?		{ count(); return(CONTINUE); }
"default"/{S}\(?		{ count(); return(DEFAULT); }
"do"/{S}\(?			{ count(); return(DO); }
"double"/{S}\(?			{ count(); return(DOUBLE); }
"else"/{S}\(?			{ count(); return(ELSE); }
"enum"/{S}\(?			{ count(); return(ENUM); }
"extern"/{S}\(?			{ count(); return(EXTERN); }
"float"/{S}\(?			{ count(); return(FLOAT); }
"for"/{S}\(?			{ count(); return(FOR); }
"goto"/{S}\(?			{ count(); return(GOTO); }
"if"/{S}\(?			{ count(); return(IF); }
"int"/{S}\(?			{ count(); return(INT); }
"long"/{S}\(?			{ count(); return(LONG); }
"register"/{S}\(?		{ count(); return(REGISTER); }
"return"/{S}\(?			{ count(); return(RETURN); }
"short"/{S}\(?			{ count(); return(SHORT); }
"signed"/{S}\(?			{ count(); return(SIGNED); }
"sizeof"/{S}\(?			{ count(); return(SIZEOF); }
"static"/{S}\(?			{ count(); return(STATIC); }
"struct"/{S}\(?			{ count(); return(STRUCT); }
"switch"/{S}\(?			{ count(); return(SWITCH); }
"typedef"/{S}\(?		{ count(); return(TYPEDEF); }
"union"/{S}\(?			{ count(); return(UNION); }
"unsigned"/{S}\(?		{ count(); return(UNSIGNED); }
"void"/{S}\(?			{ count(); return(VOID); }
"volatile"/{S}\(?		{ count(); return(VOLATILE); }
"while"/{S}\(?			{ count(); return(WHILE); }

F77_SUBROUTINE/{S}\(		{ count(); return(F77_SUBROUTINE); }
F77_[A-Z]+_FUNCTION{S}/\(	{ count(); return(F77_FUNCTION); }
F77_EXTERNAL_NAME/{S}\(		{ count(); return(F77_EXTERNAL); }
F77_CALL/{S}\(			{ count(); return(F77_CALL); }

{IDENT}/{S}\(			{ count(); return(FUNC_NAME); }
{IDENT}				{ count(); return(IDENTIFIER); }

0[xX]{H}+{IS}?			{ count(); return(CONSTANT); }
0{D}+{IS}?			{ count(); return(CONSTANT); }
{D}+{IS}?			{ count(); return(CONSTANT); }
'(\\.|[^\\'])+'			{ ecount(); return(CONSTANT); }

{D}+{E}{FS}?			{ count(); return(CONSTANT); }
{D}*"."{D}+({E})?{FS}?		{ count(); return(CONSTANT); }
{D}+"."{D}*({E})?{FS}?		{ count(); return(CONSTANT); }

\"(\\.|\\\n|[^\\"])*\"		{ ecount(); return(STRING_LITERAL); }

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
"&"				{ ecount(); return('&'); }
"<"				{ ecount(); return('<'); }
">"				{ ecount(); return('>'); }

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

extern char *yylval;
extern size_t yyleng;


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
*-
*/

/* Append preval and the matched text. */
      yyleng = preleng + strlen( (char *) yytext );
      yylval = (char *) memok( malloc( yyleng + 1 ) );
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

/* Store the position of this encountered string in the global list. */
      uadd( yylval );
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
      for ( i = 0; i < strlen( (char *) yytext ); i++ )
         cappend( yytext[ i ] );
      *yytext = '\0';
      yyleng = 0;
      count();
   }
      

   void comment() {
/*
*+
*  Name:
*     comment
*
*  Purpose:
*     Store C comment text in preval.
*-
*/
      char c, cl;
      sappend( (char *) yytext );
      c = cl = ' ';
      while ( c != LEXEOF && ( cl != '*' || c != '/' ) ) {
         cl = c;
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
         

   void cpprest() {
      char c, cl;
      c = cl = ' ';
      while ( c != LEXEOF && ( c != '\n' || cl == '\\' ) ) {
         cl = c;
         c = input();
         cappend( c );
      }
   }



   void cpp() {
/*
*+
*  Name:
*     cpp
*
*  Purpose:
*     Store miscellaneous C preprocessor text in preval.
*-
*/
      sappend( (char *) yytext );
      cpprest();
   }


   void include() {
/*
*+
*  Name:
*     include
*
*  Purpose:
*     Store C preprocessor include text in preval.
*
*  Description:
*     This routine processes the text of a #include line, which has an
*     argument enclosed in double quotes.  It tags the quoted name 
*     with an HTML-like tag.
*-
*/

/* Local variables. */
      char c;
      int i, p1, p2;

/* Add matched text. */
      sappend( (char *) yytext );

/* Add start of tag. */
      sappend( "<a href='INCLUDE-" );

/* Add the name of the included file to attribute value. */
      p1 = preleng;
      while ( ( c = input() ) != LEXEOF && c != '"' && c != '\n' )
         cappend( c );
      p2 = preleng;

/* Close the start tag. */
      sappend( "'>" );

/* Copy the name of the included file from the attribute value to the
   contents of the tag. */
      for ( i = p1; i < p2; i++ )
         cappend( preval[ i ] );

/* Add the end tag. */
      sappend( "</a>" );

/* Add the final quote. */
      cappend( c );

/* Copy anything else on the line. */
      cpprest();
   }


   void define() {
/*
*+
*  Name:
*     define
*
*  Purpose:
*     Store C preprocessor define text in preval.
*
*  Description:
*     This routine processes the text of a #define line, in which the
*     left hand side is a function (i.e. has parentheses around it).
*     It tags the definiendum with an HTML-like tag.
*-
*/

/* Local variables. */
      char c;
      int i, p1, p2;

/* Add matched text. */
      sappend( (char *) yytext );

/* Add start of tag */
      sappend( "<a name='" );

/* Add definiendum, remembering where it starts and finishes. */
      p1 = preleng;
      while ( ( c = input() ) != LEXEOF && ( isalnum( c ) || c == '_' ) )
         cappend( c );
      p2 = preleng;

/* Add end of tag. */
      sappend( "'>" );

/* Copy definiendum to after the start tag too. */
      for ( i = p1; i < p2; i++ )
         cappend( preval[ i ] );

/* Finish off. */
      sappend( "</a>" );
      cappend( c );
      cpprest();
   }


   void munch() {
      sappend( (char *) yytext );
   }
         



/* $Id$ */
