/*+
*  Name:
*     ctag-y.y
* 
*  Type of module:
*     yacc grammar
* 
*  Purpose:
*     Specify minimal grammar of C source.
* 
*  Description:
*     This grammar specification provides enough understanding of C source
*     code, as tokenized by the corresponding lexical analyser, to be
*     able to identify and tag function definitions and invocations.
* 
*     It is impossible to do a proper parse of the C, since we don't
*     walk the include files so that we can't know about what identifiers
*     have been typedef'd and what preprocessor substitutions should be
*     made.  We therefore deliberately use a very basic model of how 
*     the C fits together.  In most (the large majority of) cases this 
*     should be adequate to spot the function definitions and 
*     invocations.
* 
*     Note that comments and preprocessor directives are dealt with 
*     by the lexical analyser, so that we can pretend here as far as
*     the token stream is concerned that they do not exist.
* 
*     The yylval values returned by the lexical analyser are all pointers
*     to char, which must be output in order.  These contain, as well
*     as the text of the tokens in question, all intervening whitespace,
*     comments, preprocessor directives etc.
* 
*  Authors:
*     MBT: Mark Taylor (STARLINK)
* 
*  History:
*     23-NOV-1999 (MBT):
*        Initial version.
*-
*/





%token IDENTIFIER CONSTANT STRING_LITERAL SIZEOF

%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
%token STRUCT UNION ENUM ELIPSIS RANGE

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token F77_FUNCTION F77_SUBROUTINE F77_EXTERNAL F77_CALL FUNC_NAME
%token TRAILER

%start file

%{
#define YYSTYPE char *
%}

%%

file
	: external_definition
	| file external_definition
	;

external_definition
	: unit
		{ printf( "%s", $1 ); }
	;

unit
	: f77_define_macro '(' identifier ')' '(' nonexecutable_code ')' 
	  function_body
		{ $$ = scat( 8, $1, $2, anchor( "name", $3, 1 ), 
		             $4, $5, $6, $7, $8 ); }
	| f77_define_macro '(' identifier ')' '(' nonexecutable_code ')' ';'
		{ $$ = scat( 8, $1, $2, $3, $4, $5, $6, $7, $8 ); }
	| FUNC_NAME '(' nonexecutable_code ')' function_body
		{ $$ = scat( 5, anchor( "name", $1, 0 ), $2, $3, $4, $5 ); }
	| FUNC_NAME '(' nonexecutable_code ')' ';'
		{ $$ = scat( 5, $1, $2, $3, $4, $5 ); }
	| FUNC_NAME '(' ')' function_body
		{ $$ = scat( 4, anchor( "name", $1, 0 ), $2, $3, $4 ); }
	| FUNC_NAME '(' ')' ';'
		{ $$ = scat( 4, $1, $2, $3, $4 ); }
	| declaration_word
		{ $$ = $1; }
	| ';'
		{ $$ = $1; }
	| TRAILER
		{ $$ = $1; }
	;

function_body
	: '{' executable_code '}'
		{ $$ = scat( 3, $1, $2, $3 ); }
	| declaration_statement function_body
		{ $$ = scat( 2, $1, $2 ); }
	;

declaration_statement
	: reserved declaration_wordlist ';'
		{ $$ = scat( 3, $1, $2, $3 ); }
	| identifier declaration_wordlist ';'
		{ $$ = scat( 3, $1, $2, $3 ); }
	;

declaration_wordlist
	: declaration_word
		{ $$ = $1; }
	| declaration_wordlist declaration_word
		{ $$ = scat( 2, $1, $2 ); }
	;

declaration_word
	: reserved
		{ $$ = $1; }
	| identifier
		{ $$ = $1; }
	| CONSTANT
		{ $$ = $1; }
	| STRING_LITERAL
		{ $$ = $1; }
	| '{' nonexecutable_code '}'
		{ $$ = scat( 3, $1, $2, $3 ); }
	| '(' nonexecutable_code ')'
		{ $$ = scat( 3, $1, $2, $3 ); }
	| otherchar
		{ $$ = $1; }
	;

nonexecutable_code
	: nonexecutable_item
		{ $$ = $1; }
	| nonexecutable_code nonexecutable_item
		{ $$ = scat( 2, $1, $2 ); }
	;

nonexecutable_item
	: declaration_word
		{ $$ = $1; }
	| FUNC_NAME '(' nonexecutable_code ')'
		{ $$ = scat( 4, anchor( "href", $1, 0 ), $2, $3, $4 ); }
	| ';'
	;

executable_code
	: executable_item
		{ $$ = $1; }
	| executable_code executable_item
		{ $$ = scat( 2, $1, $2 ); }
	;

executable_item
	: F77_CALL '(' identifier ')' 
		{ $$ = scat( 4, $1, $2, anchor( "href", $3, 1 ), $4 ); }
	| FUNC_NAME '(' executable_code ')'
		{ $$ = scat( 4, anchor( "href", $1, 0 ), $2, $3, $4 ); }
	| FUNC_NAME '(' ')'
		{ $$ = scat( 3, anchor( "href", $1, 0 ), $2, $3 ); }
	| reserved
		{ $$ = $1; }
	| identifier
		{ $$ = $1; }
	| CONSTANT
		{ $$ = $1; }
	| STRING_LITERAL
		{ $$ = $1; }
	| '{' executable_code '}'
		{ $$ = scat( 3, $1, $2, $3 ); }
	| '(' executable_code ')'
		{ $$ = scat( 3, $1, $2, $3 ); }
	| ';'
		{ $$ = $1; }
	| otherchar
		{ $$ = $1; }
	;

f77_define_macro
	: F77_FUNCTION		{ $$ = $1; }
	| F77_SUBROUTINE	{ $$ = $1; }
	| F77_EXTERNAL		{ $$ = $1; }
	;

otherchar
	: other_legal_char	{ $$ = $1; }
	;

other_legal_char  /*  This excludes parentheses, braces and semicolon  */
	: ','		{ $$ = $1; }
	| ':'		{ $$ = $1; }
	| '='		{ $$ = $1; }
	| '['		{ $$ = $1; }
	| ']'		{ $$ = $1; }
	| '.'		{ $$ = $1; }
	| '&'		{ $$ = $1; }
	| '!'		{ $$ = $1; }
	| '~'		{ $$ = $1; }
	| '-'		{ $$ = $1; }
	| '+'		{ $$ = $1; }
	| '*'		{ $$ = $1; }
	| '/'		{ $$ = $1; }
	| '%'		{ $$ = $1; }
	| '^'		{ $$ = $1; }
	| '|'		{ $$ = $1; }
	| '?'		{ $$ = $1; }
	| '<'		{ $$ = $1; }
	| '>'		{ $$ = $1; }
	;

reserved
	: AUTO 		{ $$ = $1; }
	| BREAK 	{ $$ = $1; }
	| CASE 		{ $$ = $1; }
	| CHAR 		{ $$ = $1; }
	| CONST 	{ $$ = $1; }
	| CONTINUE 	{ $$ = $1; }
	| DEFAULT	{ $$ = $1; }
	| DO 		{ $$ = $1; }
	| DOUBLE 	{ $$ = $1; }
	| ELSE 		{ $$ = $1; }
	| ENUM 		{ $$ = $1; }
	| EXTERN 	{ $$ = $1; }
	| FLOAT 	{ $$ = $1; }
	| FOR 		{ $$ = $1; }
	| GOTO		{ $$ = $1; }
	| IF 		{ $$ = $1; }
	| INT 		{ $$ = $1; }
	| LONG 		{ $$ = $1; }
	| REGISTER 	{ $$ = $1; }
	| RETURN 	{ $$ = $1; }
	| SHORT 	{ $$ = $1; }
	| SIGNED 	{ $$ = $1; }
	| SIZEOF 	{ $$ = $1; }
	| STATIC 	{ $$ = $1; }
	| STRUCT 	{ $$ = $1; }
	| SWITCH 	{ $$ = $1; }
	| TYPEDEF 	{ $$ = $1; }
	| UNION 	{ $$ = $1; }
	| UNSIGNED 	{ $$ = $1; }
	| VOID 		{ $$ = $1; }
	| VOLATILE 	{ $$ = $1; }
	| WHILE		{ $$ = $1; }
	;

identifier
	: IDENTIFIER	{ $$ = $1; }
	;

%%
#include <stdio.h>

extern char yytext[];
extern int column;

   yyerror(char *s) { 
      fflush(stdout);
      printf("\n%s\n", s);
   }


#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

   char *snew( char *str ) {
/*+
*  Name:
*     snew
* 
*  Invocation:
*     string = snew( str );
* 
*  Purpose:
*     Copy a string into malloc'd space.
* 
*  Arguments:
*     str = char *
*        The string to be copied (must end with '\0').
* 
*  Return Value:
*     string = char *
*        A string with the same contents as str, but in a newly malloc'd
*        location.
* 
*  Description:
*     This routine just mallocs some space and copies the given string
*     into it.  The purpose of this is so that the resulting pointer 
*     can be passed to routines which assume their arguments have been
*     malloc'd and may be free'd.
*-
*/
      char *string;

      string = malloc( strlen( str ) + 1 );
      strcpy( string, str );
      return( string );
   }
 
 
   char *scat( int n, ... ) {
/*+
*  Name:
*     scat
* 
*  Invocation:
*     string = scat( n, ... )
* 
*  Purpose:
*     Concatenate a list of strings.
* 
*  Arguments:
*     n = int
*        The number of strings to be concatenated.
*     sp1, sp2, ... = char *
*        The other arguments are all strings, and there are n of them.
*        free() is called on each of them, so they must have been malloc'd
*        at some time in the past, and must not be used subsequent to
*        passing to this function.
* 
*  Return value:
*     string = char *
*        The return value is a string containing the concatenation of
*        all the strings supplied.  It is obtained using malloc, so
*        should be free'd at some time in the future.
* 
*  Description:
*     This routine returns a newly malloc'd string which is the concatenation
*     of all the strings supplied to it as arguments.  Each of those arguments
*     gets free'd by this routine, so they must have been malloc'd (probably
*     by this routine) in the past, and must not be referred to again after
*     calling this routine.
*-
*/

/* Local variables. */
      va_list ap;
      int len, i;
      char *string, *sp;

/* Work out the length of the final string. */
      len = 1;
      va_start( ap, n );
      for ( i = 0; i < n; i++ ) {
         sp = va_arg( ap, char * );
         len += strlen( sp );
      }
      va_end( ap );

/* Allocate the memory we will need, and initialise it. */
      string = malloc( len + 1 );
      *string = '\0';

/* Copy the arguments into the allocated space, calling free() on each 
 * one as we go along. */
      va_start( ap, n );
      for ( i = 0; i < n; i++ ) {
         sp = va_arg( ap, char * );
         strcat( string, sp );
         free( sp );
      }
      va_end( ap );

/* Return. */
      return( string );
   }
   

   char *anchor( char *attrib, char *fname, int f77flag ) {
/*+
*  Name:
*     anchor
* 
*  Invocation:
*     string = anchor( attrib, fname, f77flag )
* 
*  Purpose:
*     Generates an HTML-like anchor tag around a string.
* 
*  Arguments:
*     attrib = const char *
*        Gives the name of the attribute to be set to fname.  The sensible
*        values would be "href" and "name".
*     fname = char *
*        Gives the string to be used both as the contents of the tag,
*        and of the value of the attribute of the A tag.  This will be
*        free'd by the routine, so must previously have been malloc'd.
*        The argument may consist of any amount of text, but only the
*        identifier right at the end will be used as the attribute value
*        and tagged text.  Any leading text will be output preceding
*        the tag.
*     f77flag = int
*        If nonzero, then a '_' is appended to the attribute value.  This
*        is appropriate if it represents a fortran routine.
* 
*  Return Value:
*     Text tagged with an SGML A tag, such that if fname points to text
*     "leading text identifier" then the returned value will be 
*     "leading text<a attrib='identifier'>identifier</a>".
*     This is malloc'd by this routine so should subsequently be free'd.
* 
*  Description:
*     This routine generates an SGML tag surrounding the text given
*     by the fname argument.  The value of the attribute named by the
*     attrib argument is also fname.  The valid part of fname is supposed
*     to start at the first character and go on for as long as it 
*     constitutes a valid C identifier.  Any trailing characters are
*     appended after the tag, but don't form part of the tagged text or
*     attribute value.  The memory used by fname is free'd by this routine.
*-
*/

/* Local variables. */
      char *string, *vname, *fend;
      
/* Find the start of the identifier itself, which is assumed to be right
   at the end of the string. */
      fend = fname + strlen( fname );
      for ( vname = fend - 1; vname >= fname; vname-- ) {
         if ( ! isalnum( *vname ) && *vname != '_' )
            break;
      }
      if ( vname < fend )
         vname++;

/* Work out how much space the output string requires and allocate it. */
      string = malloc( strlen( attrib ) + strlen( fname ) + strlen( vname ) 
                       + ( f77flag ? 13 : 12 ) );

/* Write the whole input text into the output string. */
      strcpy( string, fname );

/* Overwrite the bare vname at the end with the tag. */
      sprintf( string + (int) (vname - fname), 
               ( f77flag ? "<a %s='%s_'>%s</a>"
                         : "<a %s='%s'>%s</a>" ), 
               attrib, vname, vname );

/* Free up the fname string. */
      free( fname );

/* Return. */
      return( string );
   }
      


   
/* $Id$ */
