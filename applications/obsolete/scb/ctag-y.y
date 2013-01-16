/*+
*  Name:
*     ctag-y.y
* 
*  Type of module:
*     yacc grammar
* 
*  Purpose:
*     Specify minimal grammar of C source for tagging.
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
*  Algorithm:
*     The basic idea is that the return value of each element is a string
*     which contains the text of the token itself as well as any 
*     surrounding syntactically insignificant text (e.g. whitespace) in
*     such a way that concatenating the return values of all the elements
*     passed to the parser by the lexer is identical to the original 
*     source file.  Each of these strings has been malloc'd.
*
*     Each rule generates a new malloc'd string which is the concatenation
*     of its component parts, possibly adding tags where appropriate,
*     and free's the memory associated with the component parts.
*     So by the time we reach a rule near the top level, all that 
*     remains to be done is to output the return value and free it.
*
*     Matters are complicated somewhat by the need to do error recovery:
*     because yacc throws away a lot of things when it encounters an
*     error we need to keep the text in another place too; this is
*     handled by the uadd function and friends.
*  
*  Bugs:
*     Since tokens are popped off the stack by yacc without offering us
*     the chance of intervention, and processing the tokens is normally
*     how we free up memory which has been allocated by inferior levels
*     of token processing, there is a memory leak which occurs every
*     time there is an error.
*
*     As observed above, the parsing is a best guess, and can't be perfect.
*
*     For similar reasons, it can get very confused by conditional
*     compilation directives since it ignores them, so for instance
*     a sequence like
*        #ifdef X
*           signed char func() {
*        #else
*           unsigned char func() {
*        #endif
*
*     will look like two nested open braces to the parser, which will 
*     quite likely never recover and fail to tag any of the rest of
*     the file.  It would be possible, but difficult, to patch it up
*     to work round this sort of thing.
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

%start file

%{

typedef char * STRING;
#define YYSTYPE STRING
#include "tag.h"
#include <stdio.h>
#include <stdlib.h>
char *canchor( char *attrib, char *fname, int f77flag );
void handle_error();

%}

%%

file
	: unit
	| file unit
	;

unit
	: external_definition
		{ printf( "%s", $1 ); free( $1 ); uclear(); }
	;

external_definition
	: f77_define_macro '(' identifier ')' bracket_item function_body
		{ $$ = scat( 6, $1, $2, canchor( "name", $3, 1 ), 
                             $4, $5, $6 ); }
	| f77_define_macro '(' identifier ')' bracket_item ';'
		{ $$ = scat( 6, $1, $2, $3, $4, $5, $6 ); }
	| FUNC_NAME bracket_sequence function_body
		{ $$ = scat( 3, canchor( "name", $1, 0 ), $2, $3 ); }
	| FUNC_NAME bracket_sequence ';'
		{ $$ = scat( 3, $1, $2, $3 ); }
	| declaration_word
		{ $$ = $1; }
	| ';'
		{ $$ = $1; }
	| error
		{ handle_error(); yyerrok; $$ = scat( 0 ); }
	;

bracket_sequence
	: bracket_item
		{ $$ = $1; }
	| bracket_sequence bracket_item
		{ $$ = scat( 2, $1, $2 ); }
	;

bracket_item
	: '(' ')'
		{ $$ = scat( 2, $1, $2 ); }
	| '(' nonexecutable_code ')'
		{ $$ = scat( 3, $1, $2, $3 ); }
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
		{ $$ = scat( 4, $1, $2, $3, $4 ); }
	| ';'
	;

executable_code
	: executable_item
		{ $$ = $1; }
	| executable_code executable_item
		{ $$ = scat( 2, $1, $2 ); }
	;

executable_item
	: F77_CALL '(' identifier ')' '(' executable_code ')'
		{ $$ = scat( 7, $1, $2, canchor( "href", $3, 1 ), 
                             $4, $5, $6, $7 ); }
	| F77_CALL '(' identifier ')' '(' ')'
		{ $$ = scat( 6, $1, $2, canchor( "href", $3, 1 ),
                             $4, $5, $6 ); }
	| FUNC_NAME '(' executable_code ')'
		{ $$ = scat( 4, canchor( "href", $1, 0 ), $2, $3, $4 ); }
	| FUNC_NAME '(' ')'
		{ $$ = scat( 3, canchor( "href", $1, 0 ), $2, $3 ); }
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

/* Include the lexical analyser source code directly into this file. */
#include "ctag-l.c"

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

   yyerror(char *s) { /* No action */ }

   void tagwrap() { 
/*
*+
*  Name: 
*     tagwrap
*
*  Purpose:
*     Tidy up at the end of parsing.
*
*  Description:
*     This routine will be called after yyparse() has returned.  It can
*     be used for any required housekeeping tasks.
*-
*/

/* No action. */ 
   }


   char *canchor( char *attrib, char *fname, int f77flag ) {
/*+
*  Name:
*     canchor
* 
*  Invocation:
*     string = canchor( attrib, fname, f77flag )
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
      string = (char *) memok( malloc( strlen( attrib ) + strlen( fname ) +
                                       strlen( vname ) + 
                                       ( f77flag ? 13 : 12 ) ) );

/* Write the whole input text into the output string. */
      strcpy( string, fname );

/* Overwrite the bare vname at the end with the tag. */
      sprintf( string + (int) (vname - fname), 
               ( f77flag ? "<a %s='%s_'>%s</a>" : "<a %s='%s'>%s</a>" ), 
               attrib, vname, vname );

/* Reclaim space from arguments, which will not be used after this call. */
      free( fname );

/* Return. */
      return( string );
   }


   void handle_error() {
/*
*+
*  Name:
*     handle_error
*
*  Purpose:
*     Deal with a unit which the grammar cannot parse.
*
*  Description:
*     This routine takes over when the yacc 'error' state is detected.
*
*     The desired behaviour is either to print the line unaltered
*     (if the external variable 'strict' is false), or to terminate
*     processing with an error message (if strict is true).  Either
*     way we need the text which could not be parsed.
*
*     Since yacc pops everything off the stack before we are able to
*     intercept this and discards all the associated state, we cannot
*     use yylval as usual.  Instead, we use the ucontent() routine which
*     gets a string consisting of all the text the lexer has encountered
*     since the last uclear() was called.  Note that because of this
*     interrelation between uclear() calls and handle_error() calls,
*     it is not OK to put an error token just anywhere in the grammar.
*
*     This routine also takes it upon itself to advance the lexer's
*     input stream to somewhere suitable to start parsing again.
*     We take the questionable course of skipping until the next blank
*     line.  In practice (because of human habits not C syntax) this is
*     likely to be a good choice.  We can't get yacc to do the skipping
*     forward itself because the lexer does not recognise and return
*     blanks of any kind.
*
*  Bugs:
*     Since tokens are popped off the stack by yacc without offering us
*     the chance of intervention, and processing the tokens is normally
*     how we free up memory which has been allocated by inferior levels
*     of token processing, there is a memory leak which occurs every
*     time there is an error.
*-
*/

/* Local variables. */
      char *text;

/* Get the unprocessed text. */
      text = ucontent();

/* Do something appropriate with it. */
      if ( strict ) {
         fflush( stdout );
         fprintf( stderr,
                  "\n\nError in the following:\n%s\n\nTerminated with error\n",
                  text );
         exit( 1 );
      }
      else {
         printf( "%s", text );
         yyclearin;

         /* yyerrok call here is sensible, but fails because of a bug in 
          * certain yacc versions (yyerrok symbol not declared at the 
          * right place in generated c) - hence yyerrok invocation is made 
          * where this routine is called instead. */
         /*yyerrok;*/
      }

/* Release memory allocated by ucontent. */
      free( text );
   }


/* $Id$ */
