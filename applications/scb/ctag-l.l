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
void comment();
void cpp();
void chomp();
void ecount();

%}

%%
{S}\n#include{S}\"	{ cpp(); }
{S}\n#define{S}		{ cpp(); }
{S}\n#			{ cpp(); }
^#include{S}\"		{ cpp(); }
^#define{S}		{ cpp(); }
^#			{ cpp(); }


{S}"/*"			{ comment(); }

{S}"auto"		{ count(); return(AUTO); }
{S}"break"		{ count(); return(BREAK); }
{S}"case"		{ count(); return(CASE); }
{S}"char"		{ count(); return(CHAR); }
{S}"const"		{ count(); return(CONST); }
{S}"continue"		{ count(); return(CONTINUE); }
{S}"default"		{ count(); return(DEFAULT); }
{S}"do"			{ count(); return(DO); }
{S}"double"		{ count(); return(DOUBLE); }
{S}"else"		{ count(); return(ELSE); }
{S}"enum"		{ count(); return(ENUM); }
{S}"extern"		{ count(); return(EXTERN); }
{S}"float"		{ count(); return(FLOAT); }
{S}"for"		{ count(); return(FOR); }
{S}"goto"		{ count(); return(GOTO); }
{S}"if"			{ count(); return(IF); }
{S}"int"		{ count(); return(INT); }
{S}"long"		{ count(); return(LONG); }
{S}"register"		{ count(); return(REGISTER); }
{S}"return"		{ count(); return(RETURN); }
{S}"short"		{ count(); return(SHORT); }
{S}"signed"		{ count(); return(SIGNED); }
{S}"sizeof"		{ count(); return(SIZEOF); }
{S}"static"		{ count(); return(STATIC); }
{S}"struct"		{ count(); return(STRUCT); }
{S}"switch"		{ count(); return(SWITCH); }
{S}"typedef"		{ count(); return(TYPEDEF); }
{S}"union"		{ count(); return(UNION); }
{S}"unsigned"		{ count(); return(UNSIGNED); }
{S}"void"		{ count(); return(VOID); }
{S}"volatile"		{ count(); return(VOLATILE); }
{S}"while"		{ count(); return(WHILE); }

{S}"if"/{S}\(		{ count(); return(DO); }
{S}"for"/{S}\(		{ count(); return(FOR); }
{S}"switch"/{S}\(	{ count(); return(SWITCH); }
{S}"while"/{S}\(	{ count(); return(WHILE); }

{S}F77_SUBROUTINE/{S}\(		{ count(); return(F77_SUBROUTINE_NAME); }
{S}F77_[A-Z]+_FUNCTION/{S}\(	{ count(); return(F77_FUNCTION_NAME); }
{S}F77_EXTERNAL_NAME/{S}\(	{ count(); return(F77_EXTERNAL_NAME); }

{S}{L}({L}|{D})*/{S}\(	{ count(); return(FUNC_NAME); }
{S}{L}({L}|{D})*	{ count(); return(IDENTIFIER); }

{S}0[xX]{H}+{IS}?	{ count(); return(CONSTANT); }
{S}0{D}+{IS}?		{ count(); return(CONSTANT); }
{S}{D}+{IS}?		{ count(); return(CONSTANT); }
{S}'(\\.|[^\\'])+'	{ count(); return(CONSTANT); }

{S}{D}+{E}{FS}?			{ count(); return(CONSTANT); }
{S}{D}*"."{D}+({E})?{FS}?	{ count(); return(CONSTANT); }
{S}{D}+"."{D}*({E})?{FS}?	{ count(); return(CONSTANT); }

{S}\"(\\.|[^\\"])*\"	{ count(); return(STRING_LITERAL); }

{S}";"			{ count(); return(';'); }
{S}"{"			{ count(); return('{'); }
{S}"}"			{ count(); return('}'); }
{S}","			{ count(); return(','); }
{S}":"			{ count(); return(':'); }
{S}"="			{ count(); return('='); }
{S}"("			{ count(); return('('); }
{S}")"			{ count(); return(')'); }
{S}"["			{ count(); return('['); }
{S}"]"			{ count(); return(']'); }
{S}"."			{ count(); return('.'); }
{S}"!"			{ count(); return('!'); }
{S}"~"			{ count(); return('~'); }
{S}"-"			{ count(); return('-'); }
{S}"+"			{ count(); return('+'); }
{S}"*"			{ count(); return('*'); }
{S}"/"			{ count(); return('/'); }
{S}"%"			{ count(); return('%'); }
{S}"^"			{ count(); return('^'); }
{S}"|"			{ count(); return('|'); }
{S}"?"			{ count(); return('?'); }
{S}"&"			{ ecount(); return('&'); }
{S}"<"			{ ecount(); return('<'); }
{S}">"			{ ecount(); return('>'); }

[ \t\v\n\f]		{ chomp(); }
.			{ chomp(); }

%%


#include <string.h>
#include <ctype.h>

extern char *yylval;
extern int yyleng;

#define BUFINC 128

   char *preval;        /* Text to precede the next yylval */
   int preleng = 0;     /* Length (excluding terminator) of preval. */
   int prealloc = 0;    /* Space (excluding terminator) allocated for preval. */

   int yywrap() { 
      if ( preleng > 0 )
         printf( "%s", preval ); 
      return(1); 
   }

   void sappend( char *s ) {
      int leng;
      leng = strlen( s );
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
      strcat( preval, s );
      preleng += leng;
   }

   void cappend( char c ) {
      switch( c ) {
         case '<':
            sappend( "&lt;" );
            break;
         case '>':
            sappend( "&gt;" );
            break;
         case '&':
            sappend( "&amp;" );
            break;
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
      yyleng = preleng + strlen( yytext );
      yylval = malloc( yyleng + 1 );
      if ( preleng > 0 ) {
         strcpy( yylval, preval );
         strcat( yylval, yytext );
      }
      else {
         strcpy( yylval, yytext );
      }
      if ( prealloc > 0 ) {
         free( preval );
         preleng = 0;
         prealloc = 0;
      }
   }


   void ecount() {
      int i;
      for ( i = 0; i < strlen( yytext ); i++ )
         cappend( yytext[ i ] );
      yytext = "";
      yyleng = 0;
      count();
   }
      


   void comment() {
      char c, cl;
      sappend( yytext );
      c = '\0';
      while ( cl != '*' || c != '/' ) {
         cl = c;
         c = input();
         cappend( c );
      }
   }
         

   void cpp() {
      char c, cl;
      sappend( yytext );
      c = '\0';
      while ( c != '\n' || cl == '\\' ) {
         cl = c;
         c = input();
         cappend( c );
      }
   }


   void chomp() {
      sappend( yytext );
   }
         
      



/* $Id$ */

