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
 


D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*
S			[ \t\v\n\f]*
IDENT			{L}({L}|{D})*

%{

void count();
void comment();
void cpluspluscomm();
void cpp();
void ecount();
void define();
void include();
void munch();

%}

%%
{S}\n#include{S}\"		{ include(); }
{S}\n#define{S}/{IDENT}{S}\(	{ define(); }
{S}\n#				{ cpp(); }
^#include{S}\"			{ include(); }
^#define{S}/{IDENT}{S}\(	{ define(); }
^#				{ cpp(); }

{S}"/*"				{ comment(); }
{S}"//"				{ cpluspluscomm(); }

{S}"auto"			{ count(); return(AUTO); }
{S}"break"			{ count(); return(BREAK); }
{S}"case"			{ count(); return(CASE); }
{S}"char"			{ count(); return(CHAR); }
{S}"const"			{ count(); return(CONST); }
{S}"continue"			{ count(); return(CONTINUE); }
{S}"default"			{ count(); return(DEFAULT); }
{S}"do"				{ count(); return(DO); }
{S}"double"			{ count(); return(DOUBLE); }
{S}"else"			{ count(); return(ELSE); }
{S}"enum"			{ count(); return(ENUM); }
{S}"extern"			{ count(); return(EXTERN); }
{S}"float"			{ count(); return(FLOAT); }
{S}"for"			{ count(); return(FOR); }
{S}"goto"			{ count(); return(GOTO); }
{S}"if"				{ count(); return(IF); }
{S}"int"			{ count(); return(INT); }
{S}"long"			{ count(); return(LONG); }
{S}"register"			{ count(); return(REGISTER); }
{S}"return"			{ count(); return(RETURN); }
{S}"short"			{ count(); return(SHORT); }
{S}"signed"			{ count(); return(SIGNED); }
{S}"sizeof"			{ count(); return(SIZEOF); }
{S}"static"			{ count(); return(STATIC); }
{S}"struct"			{ count(); return(STRUCT); }
{S}"switch"			{ count(); return(SWITCH); }
{S}"typedef"			{ count(); return(TYPEDEF); }
{S}"union"			{ count(); return(UNION); }
{S}"unsigned"			{ count(); return(UNSIGNED); }
{S}"void"			{ count(); return(VOID); }
{S}"volatile"			{ count(); return(VOLATILE); }
{S}"while"			{ count(); return(WHILE); }

{S}"if"/{S}\(			{ count(); return(DO); }
{S}"for"/{S}\(			{ count(); return(FOR); }
{S}"switch"/{S}\(		{ count(); return(SWITCH); }
{S}"while"/{S}\(		{ count(); return(WHILE); }

{S}F77_SUBROUTINE/{S}\(		{ count(); return(F77_SUBROUTINE); }
{S}F77_[A-Z]+_FUNCTION/{S}\(	{ count(); return(F77_FUNCTION); }
{S}F77_EXTERNAL_NAME/{S}\(	{ count(); return(F77_EXTERNAL); }
{S}F77_CALL/{S}\(		{ count(); return(F77_CALL); }

{S}{IDENT}/{S}\(		{ count(); return(FUNC_NAME); }
{S}{IDENT}			{ count(); return(IDENTIFIER); }

{S}0[xX]{H}+{IS}?		{ count(); return(CONSTANT); }
{S}0{D}+{IS}?			{ count(); return(CONSTANT); }
{S}{D}+{IS}?			{ count(); return(CONSTANT); }
{S}'(\\.|[^\\'])+'		{ count(); return(CONSTANT); }

{S}{D}+{E}{FS}?			{ count(); return(CONSTANT); }
{S}{D}*"."{D}+({E})?{FS}?	{ count(); return(CONSTANT); }
{S}{D}+"."{D}*({E})?{FS}?	{ count(); return(CONSTANT); }

{S}\"(\\.|[^\\"])*\"		{ count(); return(STRING_LITERAL); }

{S}";"				{ count(); return(';'); }
{S}"{"				{ count(); return('{'); }
{S}"}"				{ count(); return('}'); }
{S}","				{ count(); return(','); }
{S}":"				{ count(); return(':'); }
{S}"="				{ count(); return('='); }
{S}"("				{ count(); return('('); }
{S}")"				{ count(); return(')'); }
{S}"["				{ count(); return('['); }
{S}"]"				{ count(); return(']'); }
{S}"."				{ count(); return('.'); }
{S}"!"				{ count(); return('!'); }
{S}"~"				{ count(); return('~'); }
{S}"-"				{ count(); return('-'); }
{S}"+"				{ count(); return('+'); }
{S}"*"				{ count(); return('*'); }
{S}"/"				{ count(); return('/'); }
{S}"%"				{ count(); return('%'); }
{S}"^"				{ count(); return('^'); }
{S}"|"				{ count(); return('|'); }
{S}"?"				{ count(); return('?'); }
{S}"&"				{ ecount(); return('&'); }
{S}"<"				{ ecount(); return('<'); }
{S}">"				{ ecount(); return('>'); }

[ \t\v\n\f]			{ munch(); }
.				{ munch(); }

%%


#include <string.h>
#include <ctype.h>

extern char *yylval;
extern int yyleng;

#define BUFINC 128       /* Amount by which to increase buffer each time */

/*
*  These definitions set up variables to be used for passing text to
*  the parser.  When each token is returned, as well as the matched 
*  characters an extra string is prepended to them as follows:
*
*     preval   
*        The text to precede the next yylval.
*     length   
*        The length of preval, excluding the terminating NULL.
*     prealloc 
*        The space allocated for preval, excluding the terminating NULL.
*
*  The routines which modify these keep the amount of space allocated
*  and the length values up to date.
*/
   char *preval;
   int preleng = 0;
   int prealloc = 0;


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
   int yywrap() {        /* At end of file, output unreturned text directly. */
      if ( preleng > 0 )
         printf( "%s", preval ); 
      return(1); 
   }

   void sappend( char *s ) {
/*
*+
*  Name:
*     sappend
*
*  Purpose:
*     Append a string to the preval string.
*
*  Description:
*     This routine appends a string to the preval string.  If the preval
*     string is not long enough to hold the new one, then more space 
*     is allocated for it.
*-
*/

/* Local variables. */
      int leng;

/* See how many extra characters are to be added to preval. */
      leng = strlen( s );

/* Extend the allocated space if necessary. */
      while ( preleng + leng > prealloc ) {
         if ( prealloc == 0 ) {
            preval = malloc( BUFINC + 1 );
            *preval = '\0';
         }
         else {
            preval = realloc( preval, prealloc + BUFINC + 1 );
         }
         prealloc += BUFINC;
      }

/* Append the string to preval. */
      strcat( preval, s );
      preleng += leng;
   }

   void cappend( char c ) {
/*
*+
*  Name:
*     cappend
*
*  Purpose:
*     Append a character to the preval string.
*
*  Description:
*     This routine appends a single character to the preval string. 
*     If the character is '<', '>' or '&', then it is replaced in the
*     preval string by the appropriate HTML entity reference.
*-
*/

/* Switch on the value of the character. */
      switch( c ) {

/* If it needs to be replaced by an entity reference, do so via sappend. */
         case '<':
            sappend( "&lt;" );
            break;
         case '>':
            sappend( "&gt;" );
            break;
         case '&':
            sappend( "&amp;" );
            break;

/* Otherwise it's just a single character: extend allocation if necessary
   and add the new character. */
         default:
            if ( preleng + 1 > prealloc ) {
               if ( prealloc == 0 ) 
                  preval = malloc( BUFINC + 1 );
               else
                  preval = realloc( preval, prealloc + BUFINC + 1 );
               prealloc++;
            }
            preval[ preleng ] = c;
            preval[ ++preleng ] = '\0';
      }
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
*     since the last token was passed back.
*-
*/

/*  Append preval and the matched text. */
      yyleng = preleng + strlen( yytext );
      yylval = malloc( yyleng + 1 );
      if ( preleng > 0 ) {
         strcpy( yylval, preval );
         strcat( yylval, yytext );
      }
      else {
         strcpy( yylval, yytext );
      }

/*  Reclaim space used by preval if necessary. */
      if ( prealloc > 0 ) {
         free( preval );
         preleng = 0;
         prealloc = 0;
      }
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
      yytext = "";
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
      sappend( yytext );
      c = ' ';
      while ( c && ( cl != '*' || c != '/' ) ) {
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
      sappend( yytext );
      while( ( c = input() ) && c != '\n' )
         cappend( c );
      cappend( c );
   }
         

   void cpprest() {
      char c, cl;
      c = ' ';
      while ( c && ( c != '\n' || cl == '\\' ) ) {
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
      sappend( yytext );
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
      char c;
      int i, p1, p2;

      sappend( yytext );
      sappend( "<a name='INCLUDE-" );
      p1 = preleng;
      while ( ( c = input() ) && c != '"' && c != '\n' )
         cappend( c );
      p2 = preleng;
      sappend( "'>" );
      for ( i = p1; i < p2; i++ )
         cappend( preval[ i ] );
      sappend( "</a>" );
      cappend( c );
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
      sappend( yytext );

/* Add start of tag */
      sappend( "<a name='" );

/* Add definiendum, remembering where it starts and finishes. */
      p1 = preleng;
      while ( ( c = input() ) && ( isalnum( c ) || c == '_' ) )
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
      sappend( yytext );
   }
         



/* $Id$ */
