
%token LINE_START LINE_END COMMENT_LINE BLANK_LINE
%token SUBROUTINE ENTRY BLOCKDATA PROGRAM FUNCTION

%token INTEGER REAL DOUBLEPRECISION COMPLEX LOGICAL CHARACTER
%token BYTE UBYTE WORD UWORD GENERIC_TYPE DIMENSION

%token INCLUDE IF ELSEIF THEN CALL

%token INTEGER_CONSTANT STRING_CONSTANT
%token ILLEGAL_CHAR TOKEN TOKEN_BRAC

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
	| LINE_START statement LINE_END
			{ $$ = scat( 3, $1, $2, $3 ); }
	| LINE_START include_line LINE_END
			{ $$ = scat( 3, $1, $2, $3 ); }
	| COMMENT_LINE
			{ $$ = $1; }
	| BLANK_LINE
			{ $$ = $1; }
	;

module_start_line
	: SUBROUTINE token_brac '(' opt_othertext ')'
			{ $$ = scat( 5, $1, fanchor( "name", $2 ), 
                                     $3, $4, $5 ); }
	| SUBROUTINE token
			{ $$ = scat( 2, $1, fanchor( "name", $2 ) ); }
	| FUNCTION token_brac '(' opt_othertext ')'
			{ $$ = scat( 5, $1, fanchor( "name", $2 ), 
                                     $3, $4, $5 ); }
	| FUNCTION token
			{ $$ = scat( 2, $1, fanchor( "name", $2 ) ); }
	| type_spec FUNCTION token_brac '(' opt_othertext ')'
			{ $$ = scat( 6, $1, $2, fanchor( "name", $3 ), 
                                     $4, $5, $6 ); }
	| type_spec FUNCTION token
			{ $$ = scat( 3, $1, $2, fanchor( "name", $3 ) ); }
	| ENTRY token_brac '(' opt_othertext ')'
			{ $$ = scat( 5, $1, fanchor( "name", $2 ), 
                                     $3, $4, $5 ); }
	| ENTRY token
			{ $$ = scat( 2, $1, fanchor( "name", $2 ) ); }
	| BLOCKDATA opt_othertext
			{ $$ = scat( 2, $1, $2 ); }
	| PROGRAM othertext
			{ $$ = scat( 2, $1, $2 ); }
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
	: type opt_size_decl
			{ $$ = scat( 2, $1, $2 ); }
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
	: token_brac '(' othertext ')' opt_size_decl
			{ array_declare( $1 ); 
			  $$ = scat( 5, $1, $2, $3, $4, $5 ); }
	| token_brac '(' othertext ':' othertext ')' opt_size_decl
			{ array_declare( $1 );
			  $$ = scat( 7, $1, $2, $3, $4, $5, $6, $7 ); }
	| token opt_size_decl
			{ $$ = scat( 2, $1, $2 ); }
	;

opt_size_decl
	: /* empty */
			{ $$ = scat( 0 ); }
	| '*' INTEGER_CONSTANT
			{ $$ = scat( 2, $1, $2 ); }
	| '*' '(' othertext ')'
			{ $$ = scat( 4, $1, $2, $3, $4 ); }
	| '*' token
			{ $$ = scat( 2, $1, $2 ); }
	; 

statement
	: if '(' expression ')' THEN
			{ $$ = scat( 5, $1, $2, $3, $4, $5 ); }
	| if '(' expression ')' statement
			{ $$ = scat( 5, $1, $2, $3, $4, $5 ); }
	| if '(' expression ')' INTEGER_CONSTANT expression
			{ $$ = scat( 6, $1, $2, $3, $4, $5, $6 ); }
	| token '=' expression
			{ $$ = scat( 3, $1, $2, $3 ); }
	| token_brac '(' expression ')' '=' expression
			{ $$ = scat( 6, $1, $2, $3, $4, $5, $6 ); }
	| token_brac '(' string_subscript ')' '=' expression
			{ $$ = scat( 6, $1, $2, $3, $4, $5, $6 ); }
	| token_brac '(' expression ')' '(' string_subscript ')' '=' expression
			{ $$ = scat( 9, $1, $2, $3, $4, $5, $6, $7, $8, $9 ); }
	| CALL token_brac '(' expression ')'
			{ $$ = scat( 5, $1, fanchor( "href", $2 ), 
			             $3, $4, $5 ); }
	| CALL token
			{ $$ = scat( 2, $1, fanchor( "href", $2 ) ); }
	| token_brac '(' expression ')' opt_expression
			{ $$ = scat( 4, $1, $2, $3, $4 ); }
	| token opt_expression
			{ $$ = scat( 2, $1, $2 ); }
	;

if
	: IF
			{ $$ = $1; }
	| ELSEIF
			{ $$ = $1; }
	;

expression_term
	: token_brac '(' opt_expression ')'
			{ $$ = scat( 4, $1, $2, $3, $4 ); }
	| token_brac '(' string_subscript ')'
			{ $$ = scat( 4, $1, $2, $3, $4 ); }
	| '(' string_subscript ')'
			{ $$ = scat( 3, $1, $2, $3 ); }
	| '(' expression ')'
			{ $$ = scat( 3, $1, $2, $3 ); }
	| text_term
			{ $$ = $1; }
	;

string_subscript
	: expression ':' expression
			{ $$ = scat( 3, $1, $2, $3 ); }
	| expression ':'
			{ $$ = scat( 2, $1, $2 ); }
	| ':' expression
			{ $$ = scat( 2, $1, $2 ); }
	;

othertext_term
	: token_brac '(' othertext ')'
			{ $$ = scat( 4, $1, $2, $3, $4 ); }
	| token_brac '(' othertext ':' othertext ')'
			{ $$ = scat( 6, $1, $2, $3, $4, $5, $6 ); }
	| '(' othertext ')'
			{ $$ = scat( 3, $1, $2, $3 ); }
	| text_term
			{ $$ = $1; }
	;

expression
	: expression_term
			{ $$ = $1; }
	| expression expression_term
			{ $$ = scat( 2, $1, $2 ); }
	;

othertext
	: othertext_term
			{ $$ = $1; }
	| othertext othertext_term
			{ $$ = scat( 2, $1, $2 ); }
	;

text_term
	: token
			{ $$ = $1; }
	| INTEGER_CONSTANT
			{ $$ = $1; }
	| STRING_CONSTANT
			{ $$ = $1; }
	| otherchar
			{ $$ = $1; }
	| token '='
			{ $$ = scat( 2, $1, $2 ); }
	;

opt_expression
	: /* empty */
			{ $$ = scat( 0 ); }
	| expression
			{ $$ = $1; }
	;

opt_othertext
	: /* empty */
			{ $$ = scat( 0 ); }
	| expression
			{ $$ = $1; }
	;

otherchar  /*  Everything except alphanumerics, colon, equals, parentheses  */
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

token_brac
	: TOKEN_BRAC	{ $$ = $1; }
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
      int leng, i;

      rname = refname( name );
      leng = strlen( attrib ) + strlen( rname ) + strlen( name ) + 13;
      string = malloc( leng );
      sprintf( string, "<a %s=\"%s_\">%s", attrib, rname, name );
      for ( i = strlen( name ); i >= 0 && isspace( name[ i - 1 ] ); i-- );
      sprintf( string + leng - strlen( name ) - 5 + i, "</a>%s", name + i );
      return( string );
   }

   char *fanchor_inc( char *attrib, char *name ) {
      char *string, *rname;
      int leng, nleng, i;

      rname = refname( name );
      leng = strlen( attrib ) + strlen( name ) + strlen( rname ) + 20;
      string = malloc( leng );
      i = strcspn( name, "'" ) + 1;
      strncpy( string, name, i );
      string[ i ] = '\0';
      sprintf( string + i, "<a %s=\"INCLUDE-%s\">%s", attrib, rname, name + i );
      i += strcspn( name + i, "'" );
      sprintf( string + leng - strlen( name ) - 5 + i, "</a>%s", name + i );
      return( string );
   }

   void array_declare( char *name ) {
   }


