/*
*+
*  Name:
*     crepint
*
*  Purpose:
*     Replace int by INT_BIG in C.
*
*  Usage:
*     crepint [ in [ out ] ]
*
*  Description:
*     This program is a filter which takes C source code and replaces
*     occurrences of the type specifier `int' by the identifier
*     `INT_BIG'.  This identifier can then be assigned a preprocessor
*     value of a suitable integral type (int or long) either using an
*     include file or with a -DINT_BIG=(type) flag on the C compiler
*     command line.
*
*     It's not quite as simple as replacing every semantically significant
*     occurrence of the `int' identifier; `short int' and `long int' type
*     specifiers will be left alone.
*
*     If a use of int appears to be declaring a symbol called `main' or
*     `argc', then this will be left alone too, and a warning written
*     to standard error to the effect that it is not being changed.
*
*     Additionally, references to the <limits.h> macros INT_MAX, INT_MIN
*     and UINT_MAX are replaced by INT_BIG_MAX, INT_BIG_MIN and
*     UINT_BIG_MAX respectively.  If any of these substitutions are
*     made, then a line `#include "extreme.h"' is added after the
*     `#include <limits.h>' line which is presumably in the file.
*     If <limits.h> is not included in the input file, a warning is
*     written to standard error.
*
*     Explicit declarations which are implicitly of type int will have
*     an INT_BIG token inserted - for instance `static x, y;' will be
*     changed to `static INT_BIG x, y;'.
*
*     The program will write a warning on standard error for certain
*     constructions in the code which are likely to cause trouble after
*     the mass redeclaration of int as INT_BIG has occurred, since in
*     some places the type int, and not INT_BIG, is still required.
*     These constructions are:
*        - Inclusion of system header files other than those of the C
*          standard library, since these may indicate use of functions
*          other than those warned about above with arguments of type
*          pointer to int.
*        - Use of functions from the C standard library which may require
*          changes.
*
*     The functions from the C standard library which may require changes
*     are the following:
*        - Format strings in formatted I/O which may need changes
*          because they use variable argument lists or require
*          arguments of type pointer to int.
*        - The frexp() math function whose second argument must be
*          a pointer to int
*        - The signal() function whose second argument is a function
*          which must take an int argument
*        - The bsearch() and qsort() functions which take a comparison
*          function as argument, and this function must be of type int
*
*     In the case of potentially dangerous format strings, for
*     convenience a comment is inserted in the output code on the line
*     before the function call is made.  The comment will contain the
*     character string `crepint: '.  The warning to standard error
*     notes that the comment line has been inserted.
*
*     The following constructions are also likely to cause trouble, but
*     will not be warned about by the program:
*        - Use of functions without prototypes.  If header files are
*          omitted or old style function declarations are used then the
*          ANSI C machinery for doing type conversion at function call
*          time will not work.  Gcc's `-Wstrict-prototypes' and
*          `-Wimplicit-function-declaration' flags are useful for this.
*        - Implicit declarations, which are implicitly of type int.
*          If a name is declared simply by mentioning it without any type
*          or type qualifiers, it is implicitly of type int, and so
*          should become delcared as INT_BIG.  This program does not
*          find these.  Such implicit declarations (only?) occur in
*          function declarations.  The Tru64 Unix C compiler's `-protois'
*          flag or gcc's `-Wimplicit-int' flag are useful for identifying
*          these.
*
*     The program tries to adjust padding whitespace outside comments
*     so that the spacing of the output looks OK.
*
*     No changes are made to comment lines so that, for instance, the
*     Synopsis stanza of function prologues will not have formal argument
*     types changed from `int' to `INT_BIG'.
*
*     Source code which makes sufficiently inventive use of the C
*     will stand a good chance of confusing this program.
*
*  Notes:
*     Although this program behaves as a filter, it is written on
*     the assumption that it will be run on a file of a finite length:
*     it may buffer large amounts of input before writing output, and
*     it may not free up memory.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     25-JAN-2000 (MBT):
*        Initial version.
*-
*/


#include "ygen.h"
#include "ctokens.h"

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

/* Macro for working out which column a tab character advances to. */
#define tabstop(c) ( ( (c) / 8 ) * 8 + 8 )

/* Macro for general purpose smallish buffer length. */
#define LINELENG 120

/* Local function prototypes. */
   void crepint();

   int main( int argc, char **argv ) {
/*
*+
*  Name:
*     main
*
*  Purpose:
*     Main routine of C program.
*
*  Description:
*     Invoke the appropriate filter function.
*-
*/

/* Declare variables. */
      extern char *name;

/* Set up standard input and output correctly for a filter. */
      name = filter( argc, argv );

/* Filter input to output. */
      crepint();

/* Finish. */
      return 0;
   }



/* The structure to hold all the information gained from the lexer about
   the input read from the input file, and any further information used
   when doing output. */
   struct tokitem {
      char *string;      /* The whole input string                         */
      char *strmat;      /* Start of the substring which matches the token */
      char *interp;      /* Text to be interpolated after the token        */
      int tokval;        /* Value identifying the token                    */
   };


   int tokmatch( struct tokitem *ptok, ... ) {
/*
*+
*  Name:
*     tokmatch
*
*  Purpose:
*     Check a token buffer for a list of token IDs.
*
*  Description:
*     This function checks the token ID values of a set of token structures
*     in memory against a list given in the arguments.  The arguments given
*     are of variable number; the last one should be a zero (this causes
*     matching to stop).  If by the time the final (zero) argument is
*     encountered no mismatches have occurred, a true result is returned,
*     but if any mismatches occur matching attempts stop and a false
*     (zero) result is returned.
*
*  Arguments:
*     ptok = struct tokitem *
*        Start of an array of tokitem structures.
*     ... = int
*        A variable number of int arguments which are to be matched against
*        the tokval components of successive tokitem structures starting
*        at ptok.
*
*  Return value:
*     int
*        Unity if the lists match, zero if they don't.
*-
*/
      va_list ap;
      int mtok;

      va_start( ap, ptok );
      while ( mtok = va_arg( ap, int ) ) {
         if ( mtok != (ptok++)->tokval ) {
            va_end( ap );
            return 0;
         }
      }
      va_end( ap );
      return 1;
   }


   int tokoneof( struct tokitem *ptok, ... ) {
/*
*+
*  Name:
*     tokoneof
*
*  Purpose:
*     Check if a token matches one of a list.
*
*  Description:
*     This function checks the token ID value of a token structure
*     in memory against a list given in the arguments.  The arguments given
*     are of variable number; the last one should be a zero.
*     If the given token has the same tokval value as any of the arguments
*     a true result is returned, otherwise a false result is returned.
*
*  Arguments:
*     ptok = struct tokitem *
*        Pointer to the tokitem structure whose tokval is to be tested.
*     ... = int
*        A variable number of int arguments which are to be matched against
*        the tokval component of ptok.
*        at ptok.
*
*  Return value:
*     int
*        Unity if ptok->tokval matches one of the arguments, zero otherwise.
*-
*/
      va_list ap;
      int mtok;

      va_start( ap, ptok );
      while ( mtok = va_arg( ap, int ) ) {
         if ( mtok == ptok->tokval ) {
            va_end( ap );
            return 1;
         }
      }
      va_end( ap );
      return 0;
   }


   char *stlitcat( struct tokitem *ptok, int *nlittok ) {
/*
*+
*  Name:
*     stlitcat
*
*  Purpose:
*     Concatenate a number of literal strings.
*
*  Description:
*     This function returns a string which contains the concatenation
*     (without the surrounding '"' characters) of as many consecutive
*     string literals as are represented by the tokitem list starting
*     at ptok.  The number of tokens concatenated is also returned in
*     the arguments.
*
*  Arguments:
*     ptok = struct tokitem *
*        Pointer to the start of a list of tokitem structures.
*     nlittok = int *
*        On exit, *nlittok contains the number of consecutive tokens
*        starting at ptok which are string literals.
*
*  Return value:
*     char *
*        If the token at ptok is a string literal, this gives the address
*        of a string (malloc'd for the purpose) containing the concatenation
*        of the string literals.  If the token at ptok is not a string
*        literal, the return value is NULL.
*-
*/
      int buflen = 0;
      int slen;
      char *buf = NULL;

      *nlittok = 0;
      while ( ptok->tokval == STRING_LITERAL ) {
         (*nlittok)++;
         slen = strlen( ptok->strmat ) - 2;  /* Omit quote characters */
         buf = memok( realloc( buf, buflen + slen + 1 ) );
         strncpy( buf + buflen, ptok->strmat + 1, slen );
         buflen += slen;
         buf[ buflen ] = '\0';
         ptok++;
      }
      return buf;
   }

   struct tokitem *nextarg( struct tokitem *ptok ) {
/*
*+
*  Name:
*     nextarg
*
*  Purpose:
*     Skip to the start of the next argument of a function call.
*
*  Description:
*     This function, given the position in the token list of the start
*     of an argument in a function call, returns the position in the
*     token list of the token after the end of this argument.  This
*     should be either the start of the next argument, or the terminating
*     parenthesis of the function call.  It does this basically by
*     skipping forward to the next unbracketed (by significant '()',
*     '[]' or '{}' characters) comma or closing parenthesis.
*
*  Arguments:
*     ptok = struct tokitem *
*        The position in the token list of the start of a parameter in
*        a function call.
*
*  Return value:
*     struct tokitem *
*        The position in the token list of the start of the next parameter
*        of the function call, or of the closing parenthesis if this is
*        the last one.
*-
*/
      int blev = 0;
      int plev = 0;
      int slev = 0;
      int t;
      int trouble;
      ptok--;
      while( t = (++ptok)->tokval ) {
         trouble = 0;
         switch ( t ) {
            case '(':
               ++plev;
               break;
            case ')':
               trouble = ( --plev < 0 );
               break;
            case '{':
               ++blev;
               break;
            case '}':
               trouble = ( --blev < 0 );
               break;
            case '[':
               ++slev;
               break;
            case ']':
               trouble = ( --slev < 0 );
               break;
            case ',':
               if ( plev == 0 && blev == 0 && slev == 0 ) return ++ptok;
               break;
         }
         if ( trouble ) {
            while ( (++ptok)->tokval );
            return( ptok );
         }
      }
      return ptok;
   }


   struct tokitem *funcofarg( struct tokitem *ptok ) {
/*
*+
*  Name:
*     funcofarg
*
*  Purpose:
*     Find the name of the function of which the given token is an argument.
*
*  Description:
*     Given a pointer to a token which represents an argument, or an
*     unbracketed element of an argument, of a function call or
*     declaration, this function works back through the token stream
*     to find the token containing the identifier of the function.
*     This is the first token at a lower level of bracketing in the
*     backwards direction from the starting position.
*
*  Arguments:
*     ptok = struct tokitem *
*        Pointer to a token which is an unbracketed token in an actual
*        or formal argument list of a function call/declaration.
*
*  Return value:
*     funcofarg = struct tokitem *
*        Pointer to the token giving the function name identifier.
*-
*/
      int lev = 1;
      while ( (--ptok)->tokval && lev ) {
         switch( ptok->tokval ) {
            case '(':
               lev--;
               break;
            case ')':
               lev++;
               break;
         }
      }
      return ptok;
   }


   int idmatch( struct tokitem *ptok, char *ident ) {
/*
*+
*  Name:
*     idmatch
*
*  Purpose:
*     Check if a token is an identifier with a given value.
*
*  Description:
*     This function returns true if the token specified is of type
*     IDENTIFIER, and if the significant part of its string is equal
*     to a given string.
*
*  Arguments:
*     ptok = struct tokitem *
*        Pointer to a token.
*     ident = char *
*        A string which the token's representation must match.
*
*  Return value:
*     int
*        Unity if the token is an identifier which matches the given
*        string, zero otherwise.
*-
*/
      if ( ptok->tokval != IDENTIFIER )
         return 0;
      else if ( strcmp( ptok->strmat, ident ) )
         return 0;
      else
         return 1;
   }


   void subst( struct tokitem *ptok, char *replace ) {
/*
*+
*  Name:
*     subst
*
*  Purpose:
*     Substitute text into a token.
*
*  Description:
*     This function replaces the significant portion of the text of a
*     token with the supplied string.
*
*  Arguments:
*     ptok = struct tokitem *
*        Pointer to token to change.
*     replace = char *
*        The string which should replace the matched string of the token
*        at ptok.
*-
*/
      char *put;
      put = ptok->strmat;
      while ( *put && *replace ) *(put++) = *(replace++);
      if ( *put ) {
         while ( *put ) *(put++) = ' ';
      }
      else if ( *replace ) {
         ptok->interp = memok( malloc( strlen( replace ) + 1 ) );
         strcpy( ptok->interp, replace );
      }
   }

   void comment( struct tokitem *ptok, char *message ) {
/*
*+
*  Name:
*     comment
*
*  Purpose:
*     To slip a comment into the output stream.
*
*  Description:
*     This function writes a comment into the output stream, which is
*     typically done as a warning to the user of the program that
*     something needs human attention.  The comment appears on a line
*     of its own, in a distinctive format (it contains the name of this
*     program).  It will be placed at the previous line break to the
*     token specified by the argument ptok.
*
*     The contents of the message are also written to standard error.
*
*  Arguments:
*     ptok = struct tokitem *
*        This gives the position into which to interpolate the comment.
*        The comment should go on the previous line to the matched
*        string represented by this token.
*     message = char *
*        A short string to form the content text of the comment.
*        It should not exceed (about?) sixty characters.
*-
*/

/* Declare local variables. */
      char *cbuf;
      char *nl = NULL;
      char *pc;
      char text[ LINELENG ];
      int done = 0;
      int incomm;
      int tleng;

/* Write message to standard error. */
      fprintf( stderr, "%s: %s (comment inserted)\n", name, message );

/* Build the text of the comment itself. */
      sprintf( text, "/* %s: %-60s */\n", name, message );
      tleng = strlen( text );

/* Find a suitable newline character after which to place the comment.
   If we can't find one in this token, work back through previous ones. */
      for ( nl = NULL; ( nl == NULL ) && ptok->tokval ; ptok-- ) {
         incomm = 0;
         for ( pc = ptok->string; pc < ptok->strmat; pc++ ) {
            if ( pc + 1 < ptok->strmat ) {
               if ( *pc == '/' && *(pc + 1) == '*' ) incomm = 1;
               if ( *pc == '*' && *(pc + 1) == '/' ) incomm = 0;
            }
            if ( !incomm && *pc == '\n' ) nl = pc;
         }
      }

/* If the token in which the newline was found is before the start of
   the list, or if the newline was at the end of a token (unlikely),
   then interpolate the text before the start of this token. */
      if ( ptok->tokval == 0
         || nl == ptok->string + strlen( ptok->string ) - 1 ) {
         ptok++;
         cbuf = memok( malloc( strlen( ptok->string ) + tleng + 1 ) );
         strcpy( cbuf, text );
         strcpy( cbuf + strlen( text ), ptok->string );
         ptok->strmat = cbuf + ( ptok->strmat - ptok->string ) + tleng;
         ptok->string = cbuf;
      }

/* Otherwise, interpolate the text after the last newline character in
   the previous token. */
      else {
         ptok++;
         cbuf = memok( malloc( strlen( ptok->string ) + tleng + 1 ) );
         strcpy( cbuf, ptok->string );
         strcpy( cbuf + ( nl + 1 - ptok->string ), text );
         strcpy( cbuf + ( nl + 1 - ptok->string ) + tleng, nl + 1 );
         ptok->strmat = cbuf + ( ptok->strmat - ptok->string ) + tleng;
         ptok->string = cbuf;
      }
   }


   void crepint() {
/*
*+
*  Name:
*     crepint
*
*  Purpose:
*     Perform data processing for crepint program.
*
*  Invocation:
*     crepint();
*
*  Description:
*     This routine reads characters from standard input and writes them to
*     standard output.  The output is as similar as possible to the input
*     except that (most) occurrences of the type specifier token `int'
*     are replaced by a new string.
*
*     Tokenising the input stream is done using code generated by lex.
*-
*/

/* Declare local variables. */
      int arg;
      int col0;
      int col1;
      int done;
      int hasint;
      int hasintp;
      int i;
      int incomm;
      int j;
      int lastspc;
      int leng = 0;
      int limline = 0;
      int lost;
      int nlittok;
      int padding;
      int skipspc;
      int t;
      int t1;
      int tn;
      int tbufsiz = 0;
      int tok;
      int usedlimits = 0;
      char c;
      char c1;
      char *cbuf;
      char *interp;
      char line[ LINELENG ];
      char *stlit;
      char *string;
      char *strmat;
      char *warn;
      struct tokitem *tbuf;

/* Dummy zeroth record to act as stop indicator for functions that search
   backward through the buffer. */
      leng = 0;
      tbufsiz = BUFINC;
      tbuf = memok( malloc( tbufsiz * sizeof( struct tokitem ) ) );
      tbuf[ leng ].tokval = 0;
      tbuf[ leng ].string = "";
      tbuf[ leng ].strmat = tbuf->string;
      tbuf[ leng ].interp = NULL;
      leng++;

/* Fill buffer of tokens with all the information we get from the lexer. */
      while ( ( tok = yylex() ) || ( yylval != NULL ) ) {
         if ( leng + 1 >= tbufsiz ) {
            tbufsiz += BUFINC;
            tbuf = memok( realloc( tbuf, tbufsiz * sizeof( struct tokitem ) ) );
         }
         tbuf[ leng ].tokval = tok;
         tbuf[ leng ].string = yylval;
         tbuf[ leng ].strmat = ymatst;
         tbuf[ leng ].interp = NULL;
         leng++;
      }

/* Go through the token buffer looking for sequences which we need to do
   something about. */
      for ( i = 1; i < leng; i++ ) {
         t = tbuf[ i ].tokval;
         t1 = tbuf[ i + 1 ].tokval;

/* Check for, and mostly change, occurences of 'int'.
   NB. this isn't perfect - it will, incorrectly, make a substitution if
   it finds something like 'short static int'.  Such usages must be pretty
   rare, and in any case, such erroneous substitutions will lead to
   syntax errors when INT_BIG=long, so will not go unnoticed. */
         if ( t == INT && tbuf[ i - 1 ].tokval != SHORT
                       && tbuf[ i + 1 ].tokval != SHORT
                       && tbuf[ i - 1 ].tokval != LONG
                       && tbuf[ i + 1 ].tokval != LONG )
            if ( idmatch( tbuf + i + 1, "argc" ) ||
                 idmatch( tbuf + i + 1, "main" ) ) {
               fprintf( stderr, "%s: Type of %s not changed from int\n",
                                name, tbuf[ i + 1 ].strmat );
            }
            else {
               subst( tbuf + i, "INT_BIG" );
            }

/* Check for and change occurrences of symbolic constants. */
         if ( idmatch( tbuf + i, "INT_MAX" ) ) {
            subst( tbuf + i, "INT_BIG_MAX" );
            usedlimits = 1;
         }
         if ( idmatch( tbuf + i, "INT_MIN" ) ) {
            subst( tbuf + i, "INT_BIG_MIN" );
            usedlimits = 1;
         }
         if ( idmatch( tbuf + i, "UINT_MAX" ) ) {
            subst( tbuf + i, "UINT_BIG_MAX" );
            usedlimits = 1;
         }

/* Check for, and record position of, inclusion of the limits.h header file.
   This is a good place to include extreme.h if it is needed. */
         if ( tokmatch( tbuf + i,
                        CPP_INCLUDE, '<', IDENTIFIER, '.', IDENTIFIER, '>', 0 )
              && idmatch( tbuf + i + 2, "limits" )
              && idmatch( tbuf + i + 4, "h" ) ) {
            limline = i + 6;
         }

/* System header file.  Depending on what it is, this may need some action. */
         if ( tokmatch( tbuf + i, CPP_INCLUDE, '<', IDENTIFIER, 0 ) ) {

/* Assemble name of file (may span several tokens since it may contain '/'). */
            char fname[ LINELENG + 1 ];
            char *qc = fname;
            char *pc;
            for ( j = i + 2; tbuf[ j ].tokval != '>' && j < leng; j++ ) {
               for ( pc = tbuf[ j ].string; *pc && qc - fname < LINELENG; )
                  *(qc++) = *(pc++);
            }
            *qc = '\0';

/* Record position of limits.h header file.  This will be a good place to
   include extreme.h if it is needed. */
            if ( ! strcmp( fname, "limits.h" ) ) {
               limline = i + 6;
            }

/* Check if it is a standard library header file.  If so, no action needs
   to be taken. */
            else if ( ! strcmp( fname, "assert.h" ) ||
                      ! strcmp( fname, "ctype.h" )  ||
                      ! strcmp( fname, "errno.h" )  ||
                      ! strcmp( fname, "float.h" )  ||
                      ! strcmp( fname, "locale.h" ) ||
                      ! strcmp( fname, "math.h" )   ||
                      ! strcmp( fname, "setjmp.h" ) ||
                      ! strcmp( fname, "signal.h" ) ||
                      ! strcmp( fname, "stdarg.h" ) ||
                      ! strcmp( fname, "stddef.h" ) ||
                      ! strcmp( fname, "stdio.h" )  ||
                      ! strcmp( fname, "stdlib.h" ) ||
                      ! strcmp( fname, "string.h" ) ||
                      ! strcmp( fname, "time.h" ) ) {
            }

/* Otherwise, it is an unrecognised system header file.  This may indicate
   that unknown functions are being used, which may have dangerous
   declarations (arguments of type pointer to int).  Warn the user that
   further checking is in order. */
            else {
               fprintf( stderr, "%s: Non-stdlib system header file <%s>\n",
                                name, fname );
            }
         }

/* Set categories of token which will be useful. */
#define TYPES      VOID, CHAR, SHORT, INT, LONG, FLOAT, DOUBLE, \
                   STRUCT, UNION, ENUM
#define TYPEQUALS  AUTO, REGISTER, STATIC, EXTERN, CONST, VOLATILE, SIGNED, \
                   UNSIGNED, TYPEDEF
#define AFTERDEC   (int) ';', (int) ',', (int) '[', (int) '(', (int) ')'

/* Check for occurrences of int which are implicit in lists of type
   qualifiers, e.g. 'static unsigned', which really means
   'static unsigned int'. */
         if ( tokoneof( tbuf + i, TYPEQUALS, 0 ) &&
            ! tokoneof( tbuf + i + 1, TYPEQUALS, TYPES, 0 ) ) {
            if ( t1 != IDENTIFIER ||
                 strncmp( tbuf[ i + 1 ].strmat, "F77", 3 ) != 0 &&
                 tokoneof( tbuf + i + 2, AFTERDEC, 0 ) ) {

/* The token at tbuf[ i ] is the last of a list of known type specifier
   tokens.  Now work backwards, skipping over type qualifier tokens. */
                 for ( j = i - 1; tokoneof( tbuf + j, TYPEQUALS, 0 ); j-- );

/* If the previous token is one of the following:
      known type:   type declaration is explicit
      identifier:   might be a typedef'd type
      close brace:  must(?) be an enum or a struct
      asterisk:     part of a complicated pointer declaration
   we don't have implicit int, so we don't have to do anything.
   Otherwise, we should append an INT_BIG specifier to the end of the list. */
                 if ( ! tokoneof( tbuf + j, TYPES, IDENTIFIER,
                                            (int) '}', (int) '*', 0 ) ) {
                    tbuf[ i ].interp = " INT_BIG";
                 }
            }

         }

/* Check for, and warn about, certain functions from the standard library
   which will need attention. */

/* Check for, and warn about, frexp (declared in math.h). */
         if ( t1 == '(' && idmatch( tbuf + i, "frexp" ) ) {
            fprintf( stderr, "%s: Second arg of frexp() must be int *\n",
                             name );
         }

/* Check for, and warn about, signal (declared in signal.h). */
         else if ( t1 == '(' && idmatch( tbuf + i, "signal" ) ) {
            fprintf( stderr, "%s: Check func passed to signal() is (*)(int)\n",
                             name );
         }

/* Check for, and warn about, bsearch and qsort (declared in stdlib.h). */
         else if ( t1 == '(' && ( idmatch( tbuf + i, "bsearch" )
                               || idmatch( tbuf + i, "qsort" ) ) ) {
            fprintf( stderr, "%s: Check func passed to %s() is int (*)()\n",
                             name, tbuf[ i ].strmat );
         }

/* Check for, and warn about if necessary, format strings used by printf
   and related functions (declared in stdio.h).
   This code was written with reference to K&R (2nd Ed.) */
         else if ( t1 == '(' && ( idmatch( tbuf + i, "printf" )
                               || idmatch( tbuf + i, "fprintf" )
                               || idmatch( tbuf + i, "sprintf" ) ) ) {
            arg = i + 2;
            if ( ! idmatch( tbuf + i, "printf" ) )
               arg = nextarg( tbuf + arg ) - tbuf;
            stlit = stlitcat( tbuf + arg, &nlittok );
            tn = tbuf[ arg + nlittok ].tokval;
            warn = "";
            if ( tn == ',' || tn == ')' ) {
               hasint = hasintp = lost = 0;
               for ( j = 0; stlit[ j ]; j++ ) {
                  if ( stlit[ j ] == '%' ) {
                     done = 0;
                     while ( ( ! done ) && stlit[ ++j ] ) {
                        switch ( stlit[ j ] ) {
                           case '-': case '+': case ' ': case '0': case '#':
                           case '1': case '2': case '3': case '4': case '5':
                           case '6': case '7': case '8': case '9': case '.':
                              break;
                           case 'c': case 'd': case 'i': case 'o': case 'u':
                           case 'x': case 'X': case '*':
                              hasint = 1;
                              done = 1;
                              break;
                           case 'l': case 'L': case 'h':
                           case 's': case 'f': case 'e': case 'E': case 'g':
                           case 'G': case 'p': case '%':
                              done = 1;
                              break;
                           case 'n':
                              hasintp = 1;
                              done = 1;
                              break;
                           default:
                              lost = 1;
                              done = 1;
                        }
                     }
                  }
               }
               if ( lost )
                  warn = "Failed to parse format string";
               else if ( hasintp )
                  warn = "Format string has %n";
               else if ( hasint )
                  warn = "Format string has %[cdiouxX*]";
            }
            else {
               warn = "Format string non-literal";
            }
            if ( *warn ) comment( tbuf + i, warn );
         }

/* Check for, and warn about if necessary, format strings used by scanf
   and related functions (declared in stdio.h).
   This code was written with reference to K&R (2nd Ed.) */
         else if ( t1 == '(' && ( idmatch( tbuf + i, "scanf" )
                               || idmatch( tbuf + i, "fscanf" )
                               || idmatch( tbuf + i, "sscanf" ) ) ) {
            arg = i + 2;
            if ( ! idmatch( tbuf + i, "scanf" ) )
               arg = nextarg( tbuf + arg ) - tbuf;
            stlit = stlitcat( tbuf + arg, &nlittok );
            tn = tbuf[ arg + nlittok ].tokval;
            warn = "";
            if ( tn == ',' || tn == ')' ) {
               hasintp = lost = 0;
               for ( j = 0; stlit[ j ]; j++ ) {
                  if ( stlit[ j ] == '%' ) {
                     done = 0;
                     while ( stlit[ ++j ] && ! done ) {
                        switch( stlit[ j ] ) {
                           case '1': case '2': case '3': case '4': case '5':
                           case '6': case '7': case '8': case '9': case '0':
                              break;
                           case '*': case 'h': case 'l': case 'L':
                              done = 1;
                              break;
                           case 'd': case 'i': case 'o': case 'u': case 'x':
                           case 'X': case 'n':
                              hasintp = 1;
                              done = 1;
                              break;
                           case 'c': case 's': case 'e': case 'f': case 'g':
                           case 'p': case '[': case '%':
                              done = 1;
                              break;
                           default:
                              lost = 1;
                              done = 1;
                        }
                     }
                  }
               }
               if ( lost )
                  warn = "Failed to parse format string";
               else if ( hasintp )
                  warn = "Format string implies int *";
            }
            else {
               warn = "Format string non-literal";
            }
            if ( *warn ) comment( tbuf + i, warn );
         }
      }

/* If we have used any of the macros defined in extreme.h, we should
   arrange for inclusion of that at a place already identified. */
      if ( usedlimits ) {
         if ( limline ) {
            char *incline = "#include \"extreme.h\"\n";
            char *pc;
            char *qc;

            cbuf = memok( malloc( strlen( tbuf[ limline ].string ) +
                                  strlen( incline ) + 1 ) );
            pc = tbuf[ limline ].string;
            tbuf[ limline ].string = cbuf;
            qc = cbuf;
            while ( *(qc++) = *(pc++) ) {
               if ( pc - 1 == tbuf[ limline ].strmat )
                  tbuf[ limline ].strmat = qc - 1;
               if ( pc[ -1 ] == '\n' && *incline ) {
                  while ( *(qc++) = *(incline++) );
                  qc--;
               }
            }
         }
         else {
            fprintf( stderr, "%s: Nowhere to include \"extreme.h\"\n", name );
         }
      }

/* Go through token buffer outputting the characters associated with each
   token. */
      col0 = 0;    /* Column the input text is at */
      col1 = 0;    /* Column the output text is at */
      for ( i = 1; i < leng; i++ ) {
         string = tbuf[ i ].string;
         strmat = tbuf[ i ].strmat;
         interp = tbuf[ i ].interp;
         c1 = 0;
         incomm = 0;

/* Output the original content of one token, skipping spaces if necessary. */
         while ( c = *(string++) ) {

/* Was the last character a space? */
            lastspc = ( c1 == ' ' || c1 == '\t' );

/* Are we in the semantically significant part of the token text? */
            padding = ( string < strmat );

/* We encounter a space, when we need to skip space. */
            if ( col1 > col0 && ! incomm && padding && c == ' ' && lastspc ) {
               col0++;
            }

/* We encounter a tab, when we need to skip space. */
            else if ( c == '\t' && tabstop( col1 ) > tabstop( col0 )
                                              && padding && ! incomm ) {
               col0 = tabstop( col0 );
               if ( ! lastspc ) {
                  putchar( ' ' );
                  col1++;
               }
            }

/* We encounter any other character, or we do not need to skip space. */
            else {
               putchar( c );
               switch ( c ) {
                  case '\n':
                  case '\r':
                  case '\f':
                     col0 = 0;
                     col1 = 0;
                     break;
                  case '\t':
                     col0 = tabstop( col0 );
                     col1 = tabstop( col1 );
                     break;
                  default:
                     col0++;
                     col1++;
               }
               if ( padding ) {
                  if ( c1 == '/' && c == '*' ) incomm = 1;
                  if ( c1 == '*' && c == '/' ) incomm = 0;
               }
            }
            c1 = c;
         }

/* Output interpolated content associated with the token, recording how
   much space this means we would like to skip in subsequent tokens. */
         if ( interp ) {
            while ( c = *(interp++) ) {
               putchar( c );
               switch( c ) {
                  case '\n':
                  case '\r':
                  case '\f':
                     col1 = 0;
                     break;
                  case '\t':
                     col1 = tabstop( col1 );
                     break;
                  default:
                     col1++;
               }
            }
         }
      }
   }






/* $Id$ */
