
%token LINE_START LINE_END COMMENT_LINE BLANK_LINE
%token SUBROUTINE ENTRY BLOCKDATA PROGRAM FUNCTION

%token INTEGER REAL DOUBLEPRECISION COMPLEX LOGICAL CHARACTER
%token BYTE UBYTE WORD UWORD GENERIC_TYPE DIMENSION

%token INCLUDE IF ELSEIF THEN CALL

%token INTEGER_CONSTANT STRING_CONSTANT
%token ILLEGAL_CHAR TOKEN

%start file

%{
#define YYSTYPE char *

char *snew( char * );
char *scat( int, ... );

%}

%%


file
	: unit
	| file unit
	;

unit
	: line
			{ printf( "%s", $1 ); }
	;

line
	: LINE_START module_start_line LINE_END
			{ $$ = scat( 3, $1, $2, $3 ); }
	| LINE_START declaration_line LINE_END
			{ $$ = scat( 3, $1, $2, $3 ); }
	| LINE_START executable_line LINE_END
			{ $$ = scat( 3, $1, $2, $3 ); }
	| LINE_START include_line LINE_END
			{ $$ = scat( 3, $1, $2, $3 ); }
	| COMMENT_LINE
			{ $$ = $1; }
	| BLANK_LINE
			{ $$ = $1; }
	;

module_start_line
	: SUBROUTINE token opt_expression
			{ $$ = scat( 3, $1, fanchor( "name", $2 ), $3 ); }
	| FUNCTION token opt_expression
			{ $$ = scat( 3, $1, fanchor( "name", $2 ), $3 ); }
	| type_spec FUNCTION token opt_expression
			{ $$ = scat( 4, $1, $2, fanchor( "name", $3 ), $4 ); }
	| ENTRY token opt_expression
			{ $$ = scat( 3, $1, fanchor( "name", $2 ), $3 ); }
	| BLOCKDATA opt_expression
			{ $$ = scat( 2, $1, $2 ); }
	| PROGRAM expression
			{ $$ = scat( 2, $1, $2 ); }
	;

executable_line
	: executable_text
			{ $$ = $1; }
	;

include_line
	: INCLUDE STRING_CONSTANT
			{ $$ = scat( 2, $1, fanchor_inc( "href", $2 ) ); }
	;

declaration_line
	: type_spec var_dec_list
			{ $$ = scat( 2, $1, $2 ); }
	;

type_spec
	: type
			{ $$ = $1; }
	| type '*' INTEGER_CONSTANT
			{ $$ = scat( 3, $1, $2, $3 ); }
	| type '*' '(' expression ')'
			{ $$ = scat( 4, $1, $2, $3, $4 ); }
	| DIMENSION
			{ $$ = $1; }
	;

type
	: INTEGER
			{ $$ = $1; }
	| REAL
			{ $$ = $1; }
	| DOUBLEPRECISION
			{ $$ = $1; }
	| COMPLEX
			{ $$ = $1; }
	| LOGICAL
			{ $$ = $1; }
	| CHARACTER
			{ $$ = $1; }
	| BYTE
			{ $$ = $1; }
	| UBYTE
			{ $$ = $1; }
	| WORD
			{ $$ = $1; }
	| UWORD
			{ $$ = $1; }
	| GENERIC_TYPE
			{ $$ = $1; }
	;

var_dec_list
	: var_dec_item
			{ $$ = $1; }
	| var_dec_list ',' var_dec_item
			{ $$ = scat( 3, $1, $2, $3 ); }
	;

var_dec_item
	: token
			{ $$ = $1; }
	| token '(' expression ')'
			{ array_declare( $1 ); 
			  $$ = scat( 4, $1, $2, $3, $4 ); }
	| token '(' expression ':' expression ')'
			{ $$ = scat( 6, $1, $2, $3, $4, $5, $6 ); }
	;

executable_text
	: if '(' expression ')' THEN
			{ $$ = scat( 5, $1, $2, $3, $4, $5 ); }
	| if '(' expression ')' executable_text
			{ $$ = scat( 5, $1, $2, $3, $4, $5 ); }
	| token '=' expression
			{ $$ = scat( 3, $1, $2, $3 ); }
	| token '(' expression ')' '=' expression
			{ $$ = scat( 6, $1, $2, $3, $4, $5, $6 ); }
	| token '(' opt_expression ':' opt_expression ')' '=' expression
			{ $$ = scat( 8, $1, $2, $3, $4, $5, $6, $7, $8 ); }
	| CALL token '(' expression ')'
			{ $$ = scat( 5, $1, fanchor( "href", $2 ), 
			             $3, $4, $5 ); }
	| CALL token
			{ $$ = scat( 2, $1, fanchor( "href", $2 ) ); }
	| token opt_expression
			{ $$ = scat( 2, $1, $2 ); }
	;

if
	: IF
			{ $$ = $1; }
	| ELSEIF
			{ $$ = $1; }
	;

opt_expression
	: /* empty */
			{ $$ = scat( 0 ); }
	| expression
			{ $$ = $1; }
	;

expression
	: expression_term
			{ $$ = $1; }
	| expression expression_term
			{ $$ = scat( 2, $1, $2 ); }
	;

expression_term
	: token
			{ $$ = $1; }
	| INTEGER_CONSTANT
			{ $$ = $1; }
	| STRING_CONSTANT
			{ $$ = $1; }
	| otherchar
			{ $$ = $1; }
	| '(' expression ')'
			{ $$ = scat( 3, $1, $2, $3 ); }
	| '(' opt_expression ':' opt_expression ')'
			{ $$ = scat( 5, $1, $2, $3, $4, $5 ); }
	| token '='
			{ $$ = scat( 2, $1, $2 ); }
	;

otherchar  /*  Everything except alphanumerics, colon and parentheses  */
	: '+'		{ $$ = $1; }
	| '-'		{ $$ = $1; }
	| '*'		{ $$ = $1; }
	| '/'		{ $$ = $1; }
	| '.'		{ $$ = $1; }
	| ','		{ $$ = $1; }
	| '$'		{ $$ = $1; }
	| '\''		{ $$ = $1; }
	| '%'		{ $$ = $1; }
	| '_'		{ $$ = $1; }
	| ILLEGAL_CHAR	{ $$ = $1; }
	;

token
	: TOKEN		{ $$ = $1; }
	;


%%

#include "ftag-l.c"

#include <stdio.h>

   extern char *yytext;

   yyerror(char *s) {
      fflush(stdout);
      printf("\n%s\n", s);
   }



#define BLENG 1024

   char buffer[ BLENG ];

   char *refname( char *name ) {
      int i, j, leng;

      leng = strlen( name );
      for ( i = j = 0; i < leng && j < BLENG - 1; i++ ) {
         switch( name[ i ] ) {
            case ' ':
            case '\t':
            case '\'':
               break;
            case '!':
               while( name[ i + 1 ] != '\n' && i < leng - 1 ) i++;
               break;
            case '\n':
               i += 6;
               break;
            default:
               buffer[ j++ ] = name[ i ];
         }
      }
      buffer[ j++ ] = '\0';
      return( buffer );
   }

   char *fanchor( char *attrib, char *name ) {
      char *string, *rname;
      int leng;

      rname = refname( name );
      leng = strlen( attrib ) + strlen( name ) + strlen( rname ) + 13;
      string = malloc( leng );
      sprintf( string, "<a %s='%s_'>%s</a>", attrib, rname, name );
      return( string );
   }

   char *fanchor_inc( char *attrib, char *name ) {
      char *string, *rname;
      int leng;

      rname = refname( name );
      leng = strlen( attrib ) + strlen( name ) + strlen( rname ) + 20;
      string = malloc( leng );
      sprintf( string, "<a %s='INCLUDE-%s'>%s</a>", attrib, rname, name );
      return( string );
   }

   void array_declare( char *name ) {
   }


