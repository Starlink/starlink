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
*     any occurrences of the type specifier `int' by the identifier
*     `INT_BIG'.  This identifier should then be assigned a preprocessor 
*     value of a suitable integral type (int or long) either using an
*     include file or with a -DINT_BIG=xxx flag on the C compiler.
*
*     It's not quite as simple as replacing every semantically significant
*     occurrence of the `int' identifier; `short int' and `long int' type 
*     specifiers will be left alone.
*
*     If a use of int appears to be declaring a symbol called `main' or
*     `argc', then this will be left alone too, and a message written
*     to standard error to the effect that it is not being changed.
*
*     Additionally, references to the limit.h macros INT_MAX, INT_MIN 
*     and UINT_MAX are replaced by INT_BIG_MAX, INT_BIG_MIN and 
*     UINT_BIG_MAX respectively.  These may be defined on the compiler
*     command line, or using a suitable header file (extreme.h), which
*     would sensibly be included immediately after limit.h.
*
*     The program will write a warning on standard error for certain 
*     constructinons in the code which are likely to cause trouble after
*     the mass redeclaration of int as INT_BIG has occurred.
*     These constructions are:
*        - Declarations of functions with variable argument lists
*        - Use of format strings in formatted I/O which may need changes
*
*     In the case of potentially dangerous format strings, for
*     convenience a comment is inserted in the output code on the line
*     before the format string is used.  The comment will contain the
*     character string `crepint: '.
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
      char text[ 120 ];
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
      int done;
      int hasint;
      int hasintp;
      int i;
      int incomm;
      int j;
      int leng = 0;
      int lost;
      int nlittok;
      int skipspc;
      int t;
      int t1;
      int tn;
      int tbufsiz = 0;
      int tok;
      char c;
      char c1;
      char *interp;
      char line[ 100 ];
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

/* Check for, and mostly change, occurences of 'int'. */
         if ( t == INT && tbuf[ i - 1 ].tokval != SHORT 
                       && tbuf[ i - 1 ].tokval != LONG )
            if ( idmatch( tbuf + i + 1, "argc" ) || 
                 idmatch( tbuf + i + 1, "main" ) ) {
               fprintf( stderr, "%s: Type of %s not changed from int\n", 
                                name, tbuf[ i + 1 ].strmat );
            }
            else {
               subst( tbuf + i, "INT_BIG" );
            }

/* Check for and change occurrences of symbolic constants. */
         if ( idmatch( tbuf + i, "INT_MAX" ) )
            subst( tbuf + i, "INT_BIG_MAX" );
         if ( idmatch( tbuf + i, "INT_MIN" ) )
            subst( tbuf + i, "INT_BIG_MIN" );
         if ( idmatch( tbuf + i, "UINT_MAX" ) )
            subst( tbuf + i, "UINT_BIG_MAX" );

/* Check for and warn about variable argument lists. */
         if ( t == DOTDOTDOT && t1 == ')' ) {
            fprintf( stderr, "%s: Variable argument list for %s()\n",
                             name, funcofarg( tbuf + i )->strmat );
         }

/* Check for, and warn about if necessary, printf (etc) format strings.
   This code was written with reference to K&R (2nd Ed.) */
         if ( t1 == '(' && ( idmatch( tbuf + i, "printf" ) 
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
                     while ( stlit[ ++j ] && ! done ) {
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

/* Check for, and warn about if necessary, scanf (etc) format strings. 
   This code was written with reference to K&R (2nd Ed.) */
         if ( t1 == '(' && ( idmatch( tbuf + i, "scanf" )
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

/* Go through token buffer outputting the characters associated with each 
   token. */
      skipspc = 0;
      for ( i = 1; i < leng; i++ ) {
         string = tbuf[ i ].string;
         strmat = tbuf[ i ].strmat;
         interp = tbuf[ i ].interp;
         c1 = 0;
         incomm = 0;
         while ( c = *(string++) ) {
            if ( c == ' ' && c1 == ' ' && skipspc && ! incomm ) {
               skipspc--;
            }
            else {
               putchar( c );
               if ( c == '\n' || c == '\t' ) skipspc = 0;
               if ( string < strmat ) {
                  if ( c1 == '/' && c == '*' ) incomm = 1;
                  if ( c1 == '*' && c == '/' ) incomm = 0;
               }
            }
            c1 = c;
         }
         if ( interp ) {
            while ( c = *(interp++) ) {
               putchar( c );
               skipspc = ( c == '\n' ) ? 0 : skipspc + 1;
            }
         }
      }
   }





/* $Id$ */
