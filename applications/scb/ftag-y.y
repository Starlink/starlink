/*+
*  Name:
*     ftag-y.y
*
*  Type of module:
*     yacc grammar
*
*  Purpose:
*     Specify minimal grammar of Fortran 77 source for tagging.
*
*  Description:
*     This grammar specification provides enough understanding of Fortran
*     77 source code, as tokenized by the corresponding lexical analyser,
*     to be able to identify and tag subroutine/function definitions
*     and invocations, and INCLUDE'd files.
*
*     We do not attempt to do a full parse of the source, but enough to
*     be able to spot:
*        - Subroutine/function declaration
*        - Subroutine call
*        - Function call
*        - INCLUDE file
*        - Array declaration
*
*     It's necessary to spot array declarations, because the only way of
*     deciding whether something which looks like a function invocation
*     is in fact one, or is an array element reference, is to see whether
*     the part that would be the array name has been dimensioned.
*     Dimensions which have been done in include files are missed of 
*     course, so some things will look like function invocations which
*     are in fact array references.
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
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     29-NOV-1999 (MBT):
*        Initial version.
*-
*/



%token LINE_START LINE_END BLANK_LINE COMMENT_LINE
%token SUBROUTINE ENTRY BLOCKDATA PROGRAM FUNCTION

%token INTEGER REAL DOUBLEPRECISION COMPLEX LOGICAL CHARACTER
%token BYTE UBYTE WORD UWORD GENERIC_TYPE DIMENSION

%token INCLUDE IF ELSEIF THEN CALL

%token INTEGER_CONSTANT STRING_CONSTANT
%token ILLEGAL_CHAR TOKEN TOKEN_BRAC

%start file

%{

typedef char * STRING;
#define YYSTYPE STRING
#include "tag.h"
#include <stdio.h>
#include <stdlib.h>
char *refname( const char *name );
char *fanchor( const char *attrib, char *name );
char *fanchor_inc( const char *attrib, char *name );
int isreserved( const char *name );
int inlist( char *name, ELEMENT *list );
void array_declare( const char *name );
char *function_call( char *name );
void module_start();
void tagwrap();
void handle_error();

%}

%%

file
	: unit
	| file unit
	;

unit
	: line
			{ printf( "%s", $1 ); free( $1 ); uclear(); }
	;

line
	: LINE_START module_start_line LINE_END
			{ module_start();
			  $$ = scat( 3, $1, $2, $3 ); }
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
	| error LINE_END
			{ handle_error(); yyerrok; $$ = scat( 0 ); }
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
	: token_brac '(' array_bound_list ')' opt_size_decl
			{ array_declare( $1 ); 
			  $$ = scat( 5, $1, $2, $3, $4, $5 ); }
	| token opt_size_decl
			{ $$ = scat( 2, $1, $2 ); }
	;

array_bound_list
	: array_bound_item
			{ $$ = $1; }
	| array_bound_list ',' array_bound_item
			{ $$ = scat( 3, $1, $2, $3 ); }
	;

array_bound_item
	: declaration_constant
			{ $$ = $1; }
	| '*'
			{ $$ = $1; }
	| declaration_constant ':' declaration_constant
			{ $$ = scat( 3, $1, $2, $3 ); }
	;


opt_size_decl
	: /* empty */
			{ $$ = scat( 0 ); }
	| '*' declaration_constant
			{ $$ = scat( 2, $1, $2 ); }
	;

declaration_constant
	: declaration_term
			{ $$ = $1; }
	| declaration_constant otherchar declaration_term
			{ $$ = scat( 3, $1, $2, $3 ); }
	| declaration_constant '*' '*' declaration_term
			{ $$ = scat( 4, $1, $2, $3, $4 ); }
	;

declaration_term
	: INTEGER_CONSTANT
			{ $$ = $1; }
	| token
			{ $$ = $1; }
	| '(' othertext ')'
			{ $$ = scat( 3, $1, $2, $3 ); }
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
			{ $$ = scat( 5, $1, $2, $3, $4, $5 ); }
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
			{ $$ = scat( 4, function_call( $1 ), $2, $3, $4 ); }
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
	| ','
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

otherchar  /*  All except alphanumerics, colon, equals, parentheses, comma.  */
	: '+'		{ $$ = $1; }
	| '-'		{ $$ = $1; }
	| '*'		{ $$ = $1; }
	| '/'		{ $$ = $1; }
	| '.'		{ $$ = $1; }
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

/* Include the lexical analyser source code directly into this file. */
#include "ftag-l.c"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

   yyerror(char *s) { /* No action. */ }

   char *refname( const char *name ) {
/*
*+
*  Name:
*     refname
*
*  Purpose:
*     Normalise routine name.
*
*  Description:
*     This routine removes extraneous whitespace from a string and folds
*     it to lower case so that the resulting string is suitable for use
*     as the name of a fortran routine to be referenced in an anchor
*     tag attribute value.  No underscore is appended however.
*
*  Arguments:
*     name = const char *
*        The unstripped text to get the name from.
*
*  Return value:
*     buffer = char *
*        The stripped text to use as an attribute value.  Note that this
*        is a pointer to a single static location, so that subsequent
*        calls to this routine will overwrite this value.
*-
*/

/* Local variables. */
#define BLENG 1024
      int i, j, leng;
      static char buffer[ BLENG ];

/* Get length of input string. */
      leng = strlen( name );

/* Go through string a character at a time.  Whitespace is skipped in 
   appropriate ways, other characters are folded to lower case. */
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
               buffer[ j++ ] = tolower( name[ i ] );
         }
      }

/* Write terminating null. */
      buffer[ j++ ] = '\0';

/* Return. */
      return( buffer );
   }


   char *fanchor( const char *attrib, char *name ) {
/*
*+
*  Name:
*     fanchor
*
*  Purpose:
*     Generates an HTML-like anchor tag around a string.
*
*  Arguments:
*     attrib = const char *
*        Gives the name of the attribute to be set to name.  The sensible
*        values would be "href" and "name".
*     name = char *
*        Gives the string to be used both as the contents of the tag,
*        and of the value of the attribute of the A tag.  This will be
*        free'd by the routine, so must previously have been malloc'd.
*        The argument may consist of any amount of text, but only the
*        identifier right at the start will be used as the attribute value
*        and tagged text.  Any trailing text will be output following
*        the tag.
*
*  Return Value:
*     string = char *
*        Text tagged with an SGML A tag, such that if name points to text
*        "text trailing-spaces" then the returned value will be
*        "<a attrib='refname( text )'>text</a> trailing-spaces".
*        This is malloc'd by this routine so should subsequently be free'd.
*
*  Description:
*     This routine generates an SGML tag surrounding the text given
*     by the name argument.  The value of the attribute named by the
*     attrib argument is also name.  The valid part of name is got by
*     calling the refname() function.  Any trailing characters are
*     appended after the tag, but don't form part of the tagged text or
*     attribute value.  The memory used by name is free'd by this routine.
*
*     If the routine looks like a generic one (has '&lt;T&gt;' in it)
*     and attrib is "name", then tags for each of the possible values
*     for "<T>" will be output.
*-
*/

/* Local variables. */
      char *string, *rname, *genpos, *vname, *fend;
      const char *gencode[] = { "i", "r", "d", "l", "c", "b", "ub", "w", "uw", "" };
      int i, l, leng;

/* Find the start of the identifier itself, which is assumed to be right
   at the end of the string. */
      fend = name + strlen( name );
      for ( vname = fend - 1; vname >= name; vname-- ) {
         if ( ! isalnum( *vname ) && strchr( "<>$_%&;", *vname ) == NULL )
            break;
      }
      if ( vname < fend )
         vname++;

/* Get the significant part of the name of the module to be tagged. */ 
      rname = refname( vname );

/* Find out if we need to tag it as a generic function.  We only need to
   worry about this if attrib is "name" (an href does not need multiple
   tags). */
      genpos = strcmp( attrib, "name" ) ? NULL : strstr( rname, "&lt;t&gt;" );

/* Work out how much space is needed for the final string and allocate it. */
      leng = strlen( attrib ) + strlen( rname ) + strlen( name ) + 13
           + ( ( genpos != NULL ) ? ( ( strlen( rname ) + 8 ) * 9 + 2 ) : 0 );
      string = (char *) memok( malloc( leng ) );

/* Write the whole input text into the output string. */
      strcpy( string, name );

/* Overwrite the bare vname at the end with the tag. */
      sprintf( string + (int) (vname - name), "<a %s='%s_'>%s</a>",
               attrib, rname, vname );

/* If it's a generic function, add all the tags for the specific ones. */
      if ( genpos != NULL ) {
         l = strlen( string );
         for ( i = 0; *( gencode[ i ] ) != '\0'; i++ ) {
            sprintf( string + l, "<a name='%s", rname );
            l += ( genpos - rname ) + 9;
            sprintf( string + l, "%s%s_'></a>", gencode[ i ], genpos + 9 );
            l += strlen( genpos ) - 9 + strlen( gencode[ i ] ) + 7;
         }
      }

/* Reclaim space from arguments, which may not be used after this call. */
      free( name );

/* Return. */
      return( string );
   }


   char *fanchor_inc( const char *attrib, char *name ) {
/*
*+
*  Name:
*     fanchor_inc
*
*  Purpose:
*     Generate an HTML-like anchor tag for an include file.
*
*  Arguments:
*     attrib = const char *
*        Name of the attribute, presumably "href".
*     name = const char *
*        Text to be tagged.  Must include two "'" characters.  The text
*        between these will be the contents of the tag.  This variable
*        will be free'd by this function, so must previously have been
*        malloc'd.
*
*  Return value:
*     string = char *
*        Text tagged with an SGML A tag.  This is malloc'd by this 
*        routine so should subsequently be free'd.
*-
*/

/* Local variables. */
      char *string, *rname, *rns;
      int leng, i;

/* Get the stripped value of the name. */
      rname = refname( name );

/* Find out how many characters we need and allocate it. */
      leng = strlen( attrib ) + strlen( name ) + strlen( rname ) + 20;
      string = (char *) memok( malloc( leng ) );

/* Write any text up to and including the first quote. */
      i = strcspn( name, "'" ) + 1;
      strncpy( string, name, i );
      string[ i ] = '\0';

/* Write start tag, contents and the remainder. */
      sprintf( string + i, "<a %s='INCLUDE-%s'>%s", attrib, rname, name + i );
      rns = string + i + strlen( attrib ) + 5;
      i += strcspn( name + i, "'" );
      sprintf( string + leng - strlen( name ) - 5 + i, "</a>%s", name + i );

/* Uppercase the attribute value. */
      for ( ; *rns != '>'; rns++ )
         *rns = toupper( *rns );

/* Reclaim space from arguments, which may not be used after this call. */
      free( name );

/* Return. */
      return( string );
   }


   int isreserved( const char *name ) {
/*
*+
*  Name:
*     isreserved
*
*  Purpose:
*     Find out whether the word is a fortran reserved word.
*
*  Description:
*     This routine just goes through a list of reserved words to see
*     if the given parameter is in it.  This is not the most efficient
*     way to go about it, but it probably doesn't dominate run time.
*
*  Return value:
*     found = int
*        True (1) if the name is a reserved word, false (0) if it's not.
*-
*/

/* Local variables. */
      int i, found;

/* Reserved words.  These are supposed to be all the words which might
   turn up in fortran source followed by an opening parenthesis, which
   ought not to be interpreted as potential function calls. */
      static const char *reserved[] = {
         "%val", "%loc",
         "read", "write", "backspace", "close", "open", "endfile", "format", 
         "inquire", "while", "print", "return",
         "int", "ifix", "idint", "real", "float", "sngl", 
         "dble", "cmplx", "ichar", "char", "aint", "dint", "anint", 
         "dnint", "nint", "idnint", "abs", "iabs", "dabs", "cabs", "mod", 
         "amod", "dmod", "sign", "isign", "dsign", "dim", "idim", "ddim",
         "dprod", "max", "max0", "amax1", "dmax1", "amax0", "max1", "min",
         "min0", "amin1", "dmin1", "amin0", "min1", "len", "index", "aimag",
         "congj", "sqrt", "dsqrt", "csqrt", "exp", "dexp", "cexp", "log",
         "alog", "dlog", "clog", "log10", "alog10", "dlog10", "sin", "dsin",
         "csin", "cos", "dcos", "ccos", "tan", "dtag", "asin", "dasin",
         "acos", "dacos", "atan", "datan", "atan2", "datan2", "sinh",
         "dsinh", "cosh", "dcosh", "tanh", "dtanh", 
         ""
      };

/* Go through the list of words, seeing if name matches any of them. */
      found = 0;
      for ( i = 0; ( ! found ) && ( *( reserved[ i ] ) != '\0' ); i++ )
         found |= ( strcmp( name, reserved[ i ] ) == 0 );

/* Return. */
      return( found );
   }



/* Set up base elements and pointers for list of declared arrays. */
   ELEMENT listbase = { "", (ELEMENT *) NULL }; /* Zero'th element of list. */
   ELEMENT *listfirst = &listbase;              /* Pointer to list start.   */
   ELEMENT *listlast = &listbase;               /* Pointer to list end.     */


   int inlist( char *name, ELEMENT *list ) {
/*
*+
*  Name:
*     inlist
*
*  Purpose:
*     Check whether a name is in a linked list.
*
*  Description:
*     This routine just works through a linked list to see if the given
*     name appears as the text element of any of the items.
*     This is not the most efficient way to go about it, but it probably 
*     doesn't dominate run time.
*
*  Parameters:
*     name = char *
*        The name to be matched.
*     list = ELEMENT *
*        A pointer to the first element of the linked list to be checked.
*
*  Return value:
*     found = int
*        True (1) if name is in the list, or false (0) if it is not.
*-
*/

/* Local variables. */
      int found;

/* Walk through the list an item at a time. */
      found = 0;
      while ( ! found && ( list = list->next ) != NULL )
         found |= ( strcmp( name, list->text ) == 0 );

/* Return. */
      return( found );
   }


   void array_declare( const char *name ) {
/*
*+
*  Name:
*     array_declare
*
*  Purpose:
*     Assert that a word is the name of an array.
*
*  Description:
*     This routine should be called when an array declaration is
*     encountered.  The name of the array will be added to the list
*     of arrays that we know about, so that if it is encountered
*     followed by an opening parenthesis it will not be mistaken
*     for a function invocation.
*-
*/

/* Local variables. */
      char *rname;

/* Get the normalised array name. */
      rname = refname( name );

/* Add the normalised name to the end of the linked list of array names. */
      listlast->next = (ELEMENT *) memok( malloc( sizeof( ELEMENT ) ) );
      listlast = listlast->next;
      listlast->next = NULL;
      listlast->text = (char *) memok( malloc( strlen( rname ) + 1 ) );
      strcpy( listlast->text, rname );
   }


   char *function_call( char *name ) {
/*
*+
*  Name:
*     function_call
*
*  Purpose:
*     Tag a word as a function call if it needs it.
*
*  Description:
*     If the passed value appears to be a function call, then a tagged
*     version of it is returned.  If it appears to be a fortran reserved 
*     word or a predeclared array then it is returned unchanged.
*-
*/

/* Local variables. */
      char *rname, *retval;

/* Get normalised function name. */
      rname = refname( name );

/* If the name is a previously declared array name, or a fortran reserved
   word, then the returned value is the same as the input value. */
      if ( inlist( rname, listfirst ) || isreserved( rname ) ) {
         retval = name;
      }

/* Otherwise, tag the name as a function call. */
      else {
         retval = fanchor( "href", name );
      }

/* Return. */
      return( retval );
   }


   void module_start() {
/*
*+
*  Name:
*     module_start
*
*  Purpose:
*     Assert that a new program part is being started.
*
*  Description:
*     This function should be called when the start of a subroutine or
*     function is encountered.  The effect is to reset the list of
*     declared arrays.
*-
*/

/* Local variables. */
      ELEMENT *i, *j;

/* Go through the linked list of array elements freeing the memory used
   for each one. */
      i = listfirst->next;
      while ( i != NULL ) {
         j = i->next;
         if ( i->text != NULL )
            free( i->text );
         free( i );
         i = j;
      }

/* Reset the pointers to each end of the linked list. */
      listfirst = listlast = &listbase;
      listfirst->next = (ELEMENT *) NULL;
   }


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

/* Reclaim memory from the list of declared arrays. */
      module_start(); 
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
*-
*/

/* Local variables. */
      char *text;

/* Get the unprocessed text. */
      text = ucontent();

/* Do something appropriate with it. */
      if ( strict ) {
         fflush( stdout );
         fprintf( stderr, "\nError in this line:\n\n   %s\n", text );
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
