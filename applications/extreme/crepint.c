/*
*+
*  Name:
*     crepint
*
*  Purpose:
*     Replace int declarations by INT_BIG declarations in C source code.
*
*  Usage:
*     crepint [ in [ out ] ]
*
*  Description:
*     This program is a filter which takes C source code and replaces 
*     any occurrences of the type specifier `int' by the value
*     `INT_BIG'.  This should then be assigned a preprocessor value 
*     at compile time using a -DINT_BIG=int or -DINT_BIG=long flag on
*     the C compiler.
*
*     It's not quite as simple as replacing every occurrence; `short int'
*     and `long int' type specifiers will be left alone.
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



   struct tokitem {
      char *string;      /* The whole input string                         */
      char *strmat;      /* Start of the substring which matches the token */
      char *interp;      /* Text to be interpolated after the token        */
      int tokval;        /* Value identifying the token                    */
   };


   int tokmatch( struct tokitem *tok, ... ) {
      va_list ap;
      int mtok;

      va_start( ap, tok );
      while ( mtok = va_arg( ap, int ) ) {
         if ( mtok != (tok++)->tokval ) {
            va_end( ap );
            return 0;
         }
      }
      va_end( ap );
      return 1;
   }

   struct tokitem *nextarg( struct tokitem *ptok ) {
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

   int idmatch( struct tokitem *ptok, char *ident ) {
      if ( ptok->tokval != IDENTIFIER )
         return 0;
      else if ( strcmp( ptok->strmat, ident ) )
         return 0;
      else
         return 1;
   }

   void subst( struct tokitem *ptok, char *replace ) {
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
      char *cbuf;
      char *nl = NULL;
      char *pc;
      char text[ 120 ];
      int done = 0;
      int incomm;
      int tleng;
      
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
      int skipspc;
      int t;
      int t1;
      int tbufsiz = 0;
      int tok;
      char c;
      char c1;
      char *interp;
      char line[ 100 ];
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
         if ( t == INT && tbuf[ i - 1 ].tokval != SHORT 
                       && tbuf[ i - 1 ].tokval != LONG )
            if ( idmatch( tbuf + i + 1, "argc" ) || 
                 idmatch( tbuf + i + 1, "main" ) ) {
               sprintf( line, "Type of %s not changed from int", 
                        tbuf[ i + 1 ].strmat );
               comment( tbuf + i + 1, line );
            }
            else {
               subst( tbuf + i, "INT_BIG" );
            }
         if ( idmatch( tbuf + i, "INT_MAX" ) )
            subst( tbuf + i, "INT_BIG_MAX" );
         if ( idmatch( tbuf + i, "INT_MIN" ) )
            subst( tbuf + i, "INT_BIG_MIN" );
         if ( t1 == '(' && ( idmatch( tbuf + i, "printf" ) 
                          || idmatch( tbuf + i, "fprintf" )
                          || idmatch( tbuf + i, "sprintf" ) ) ) {
            arg = i + 2;
            if ( ! idmatch( tbuf + i, "printf" ) )
               arg = nextarg( tbuf + arg ) - tbuf;
            warn = "";
            if ( tokmatch( tbuf + arg, STRING_LITERAL, ',', 0 ) ||
                 tokmatch( tbuf + arg, STRING_LITERAL, ')', 0 ) ) {
               strmat = tbuf[ arg ].strmat;
               hasint = hasintp = lost = 0;
               /* newstr = memok( malloc( 2 * strlen( strmat ) + 1 ) ); */
               /* k = 0; */
               for ( j = 0; strmat[ j ]; j++ ) {
                  /* newstr[ k++ ] = strmat[ j ]; */
                  if ( strmat[ j ] == '%' ) {
                     done = 0;
                     while ( strmat[ ++j ] && ! done ) {
                        switch ( strmat[ j ] ) {
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
                        /* newstr[ k++ ] = strmat[ j ]; */
                     }
                  }
                  /* newstr[ k++ ] = strmat[ j ]; */
               }
               /* newstr[ k++ ] = '\0'; */
               if ( lost )
                  warn = "Failed to parse format string";
               else if ( hasintp )
                  warn = "Format string contains %n";
               else if ( hasint )
                  warn = "Format string contains %[cdiouxX*]";

               /* if ( hasint && ! lost && ! hasintp )  *
                *    subst( tbuf + arg, newstr );       */
            }
            else {
               warn = "Non-literal format string";
            }
            if ( *warn ) comment( tbuf + i, warn );
         }
         if ( t1 == '(' && ( idmatch( tbuf + i, "scanf" )
                          || idmatch( tbuf + i, "fscanf" )
                          || idmatch( tbuf + i, "sscanf" ) ) ) {
            arg = i + 2;
            if ( ! idmatch( tbuf + i, "scanf" ) ) 
               arg = nextarg( tbuf + arg ) - tbuf;
            warn = "";
            if ( tokmatch( tbuf + arg, STRING_LITERAL, ',', 0 ) ||
                 tokmatch( tbuf + arg, STRING_LITERAL, ')', 0 ) ) {
               strmat = tbuf[ arg ].strmat;
               hasintp = lost = 0;
               for ( j = 0; strmat[ j ]; j++ ) {
                  if ( strmat[ j ] == '%' ) {
                     done = 0;
                     while ( strmat[ ++j ] && ! done ) {
                        switch( strmat[ j ] ) {
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
                  warn = "Format string contains pointers to int";
            }
            else {
               warn = "Non-literal format string";
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
