/*
*+
*  Name:
*     frepint
*
*  Purpose:
*     Replace INTEGER by INTEGER*8 in Fortran.
*
*  Usage:
*     frepint [ in [ out ] ] 
*
*  Description:
*     This program is a filter which takes FORTRAN 77 source code
*     and modifies it so that INTEGER declarations are rewritten as
*     'INTEGER * 8'.  It also attempts to warn if there are usages which
*     might cause trouble given this change. 
*
*     Additionally, if there appear to be actual arguments to subroutines
*     or functions which are literal integers, the program will attempt
*     to replace them with symbolic constants, as defined in the 
*     EXT_PAR include file.  It will also attempt to add a line INCLUDEing
*     this file.  This aspect of the processing is not foolproof.
*     It can misidentify an array reference as a function call - if
*     the 'function' name contains an underscore it assumes that it is
*     a function name and not an array name.  Only single literal integers
*     as actual arguments are thus replaced, so that, e.g. the code
*
*        L = PKG1_FUNC( X, Y, Z, 0, 3 * 5, STATUS )
*
*     will be replaced by 
*
*        L = PKG1_FUNC( X, Y, Z, EXT__0, 3 * 5, STATUS )
*
*     Attention is paid to fortran 77 source format, so that lines
*     more than 72 characters long are avoided (unless they were there
*     in the first place).
*
*     Some attention is paid to the aesthetic qualities of the output:
*     line breaks are made, where possible, following the usage in, e.g.,
*     KAPPA.  An attempt is made to copy the style of case usage from the 
*     input.
*
*     No changes are made to comment lines so that, for instance, the 
*     Arguments stanza of subroutine prologues will not have argument
*     types modified from 'INTEGER' to 'INTEGER * 8'.
*
*     The program will write a warning on standard error for certain 
*     constructions in the code which are likely to cause trouble after
*     the mass redeclaration of INTEGER as INTEGER*8 has occurred.
*     These constructions are:
*        - INTEGER * n declarations which already exist in the code 
*          (these are not modified)
*        - EQUIVALENCE statements
*        - Use of INTEGER Specific names for standard intrinsic functions
*          (IABS, ISIGN, IDIM, MAX0, AMAX0, MIN0, IMIN0)
*        - Any module (SUBROUTINE, FUNCTION or BLOCK DATA) which does not
*          include an IMPLICIT NONE statement.
*
*  Bugs:
*     In some cases, the line breaks are not made in very beautiful places.
*     They should, however, always be correct.
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
*     01-FEB-2000 (MBT):
*        Initial version.
*-
*/


#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include <ctype.h>

#include "ygen.h"
#include "ftokens.h"


/* Local function prototypes. */
   void frepint();
   void outchar( char );
   void outmark( char * );
   void outwrite();


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
*     Invokes the appropriate filter function.
*-
*/

/* Declare variables. */
      extern char *name;

/* Set up standard input and output correctly for a filter. */
      name = filter( argc, argv );

/* Filter input to output. */
      frepint();

/* Finish. */
      return 0;
   }


/* Structure to hold all the useful information about the tokens got from
   the lexer. */
   struct tokitem {
      char *string;       /* The whole input string.                         */
      char *strmat;       /* Start of the substring which matches the token. */
      int tokval;         /* Value identifying the token.                    */
   };


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


/* Get the next column from the current column and the character being 
   output. */
#define colchar(col,c) ( (c) == '\n' ? 1 : (col) + ( (c) == '\t' ? 8 : 1 ) )


   void frepint() {
/*
*+
*  Name:
*     frepint
* 
*  Purpose:
*     Perform data processing for frepint program.
*
*  Invocation:
*     frepint()
*
*  Description:
*     This routine reads characters from standard input and writes them
*     to standard output.  The output is substantially similar to the
*     input except that INTEGER declarations are changed to INTEGER*8.
*
*     Tokenising the input stream is done using code generated by lex.
*-
*/

/* Local variable declarations. */
      int col;
      int done;
      int fixedint = 0;
      int i;
      int implicitnone = 0;
      int itok;
      int j;
      int k;
      int leng;
      int nc;
      int nspc;
      int pre;
      int post;
      int skipspc;
      int t;
      int t1;
      int tbufsiz = 0;
      int tok;
      char c;
      char incbuf[ 160 ] = "";
      char modname[ 160 ] = "";
      char *pc;
      char *qc;
      struct tokitem *pctok;
      struct tokitem *tbuf = NULL;

/* Get characters from the lex tokeniser.  As well as the token id which
   is the return value of yylex, the global yylval points to the 
   characters which constituted this token, and the global ymatst points
   to the part of yylval where the syntactically significant part of
   that token begins. */
      leng = 0;
      while ( ( tok = yylex() ) || ( yylval != NULL ) ) {
         if ( leng + 1 >= tbufsiz ) {
            tbufsiz += BUFINC;
            tbuf = memok( realloc( tbuf, tbufsiz * sizeof( struct tokitem ) ) );
         }
         tbuf[ leng ].tokval = tok;
         tbuf[ leng ].string = yylval;
         tbuf[ leng ].strmat = ymatst;
         leng++;
      }

/* Go through the tokens and output them.  INTEGER can only appear at the
   start of a fortran line.  */
      col = 1;
      for ( i = 0; i < leng; i++ ) {

/* Output the string in any case, keeping track of source line column. */
         for ( pc = tbuf[ i ].string; c = *pc; pc++ ) {
            outchar( c );
            col = colchar( col, c );
         }

/* Get values of tokens for convenience. */
         t = tbuf[ i ].tokval;
         t1 = tbuf[ i + 1 ].tokval;

/* INTEGER declaration to be changed.  Handle this token and any others 
   up till the next newline character. */
         if ( t == INTEGER && t1 != '*' ) {
            for ( pc = " * 8"; c = *pc; pc++ ) 
               outchar( c );
            skipspc = 4;
            if ( ! isspace( *(tbuf[ i + 1 ].string) ) ) {
               outchar( ' ' );
               skipspc++;
            }
            col += skipspc;
            done = 0;
            nspc = 0;
            nc = 0;
            for ( j = i + 1; ! done && j < leng; j++ ) {
               for ( pc = tbuf[ j ].string; c = *pc; pc++ ) {

/* We have found a line end (or start of inline comment).  Now see whether
   the expanded line can fit into 72 characters. */
                  if ( ( ! done ) && ( c == '\n' || c == '!' ) ) {
                     done = 1;

/* Expanded line will fit into 72 characters.  Output the intervening
   tokens, squashing up spaces to retain as much of the formatting as 
   possible if we have the chance. */
                     if ( col + nc + ( pc - tbuf[ j ].string ) 
                              - MIN( nspc, skipspc ) <= 73 ) {
                        for ( k = i + 1; k <= j; k++ ) {
                           for ( qc = tbuf[ k ].string; c = *qc; qc++ ) {
                              if ( c == ' ' && skipspc && 
                                   qc > tbuf[ k ].string && qc[ -1 ] == ' ' ) {
                                 skipspc--;
                              }
                              else {
                                 outchar( c );
                                 switch( c ) {
                                    case '\n':
                                       col = 1;
                                       skipspc = 0;
                                       break;
                                    case '\t':
                                       col += 8;
                                       break;
                                    case '!':
                                       col++;
                                       break;
                                    default:
                                       col++;
                                 }
                              }
                           }
                        }
                     }

/* Expanded line will not fit in 72 characters.  Output a linebreak right
   after the INTEGER declaration and the rest of the line unchanged after 
   it. */
                     else {
                        outchar( '\n' );
                        for ( k = 1; k < col - skipspc ; k++ )
                           outchar( k == 6 ? ':' : ' ' );
                        for ( k = i + 1; k <= j; k++ ) {
                           for ( qc = tbuf[ k ].string; c = *qc; qc++ ) {
                              outchar( c );
                              col = colchar( col, c );
                           }
                        }

                     }
                  }
                  nc++;
                  if ( c == ' ' && pc > tbuf[ j ].string && pc[ -1 ] == ' ' ) {
                     nspc++;
                  }
               }
            }
            if ( ! done ) {
               fprintf( stderr, "%s: Fortran parse error\n", name );
               exit( 1 );
            }
            i = j - 1;
         }

/* Literal integer constant.  If this occurs as an actual argument of a
   subroutine or function call, we should replace it by an expression of
   the right type. */
         else if ( t1 == INTEGER_CONSTANT ) {
            int sign = 1;
            itok = i + 1;

/* Get the token preceding the constant, and any unary sign; and the token
   following it. */
            post = tbuf[ itok + 1 ].tokval;
            pre = tbuf[ itok - 1 ].tokval;
            if ( pre == '+' || pre == '-' ) pre = tbuf[ itok - 2 ].tokval;

/* Only proceed if the constant looks like the whole of an actual argument. */
            if ( ( pre == ',' || pre == '(' ) && 
                 ( post == ',' || post == ')' ) ) {
               
/* Only proceed if the thing it's an argument of is a subroutine, or looks
   like it's probably a function call (the alternative is that it might be
   an array reference). */
               pctok = funcofarg( tbuf + itok );
               if ( pctok[ -1 ].tokval == CALL || 
                    ( pctok->tokval == IDENTIFIER && 
                      strchr( pctok->strmat, '_' ) != NULL ) ) {
                  int col1;
                  int col2;
                  int indent;
                  int lastcol;
                  long cval;
                  char *fmt;
                  char *cstr; 

/* Prepare the replacement text. */
                  sscanf( tbuf[ itok ].strmat, "%ld", &cval );
                  fmt = ( cval <= 10 ) ? "EXT__%ld" : "%ld + EXT__0";

/* Work out whether there is enough space on this line for the modified 
   text. */
                  col1 = col;
                  lastcol = 0;
                  for ( j = itok; ! lastcol && j < leng; j++ ) {
                     for ( pc = tbuf[ j ].string; c = *pc; pc++ ) {
                        if ( pc == tbuf[ itok ].strmat ) col2 = col1;
                        if ( c == '\n' && j > itok ) {
                           lastcol = col1;
                           break;
                        }
                        col1 = colchar( col1, c );
                     }
                     if ( ! lastcol && tbuf[ j ].tokval == LINE_END ) 
                        lastcol = col1;
                  }
                  indent = 0;

/* If we need a line break, work out how far it should be indented.  Find
   out the column of the bracket enclosing this argument list, and try 
   that.  If that's too far, use an indent of 9, which must be OK. */
                  if ( lastcol + strlen( fmt ) - 3 > 72 || ! lastcol ) {
                     for ( j = pctok - tbuf + 1;  
                           tbuf[ j ].tokval != LINE_START && j >= 0; j-- );
                     col1 = 1;
                     for ( ; j <= pctok - tbuf + 1; j++ ) {
                        for ( pc = tbuf[ j ].string; c = *pc; pc++ )
                           col1 = colchar( col1, c );
                     }
                     if ( col1 + strlen( fmt ) - 3 + lastcol - col2 > 72 )
                        indent = 9;
                     else
                        indent = col1;
                  }

/* Reconstruct the integer token with the new text. */
                  cstr = memok( malloc( strlen( tbuf[ itok ].string ) +
                                        strlen( fmt ) + indent ) );
                  qc = cstr;
                  for ( pc = tbuf[ itok ].string; pc < tbuf[ itok ].strmat; ) 
                     *(qc++) = *(pc++);
                  if ( indent ) {
                     *(qc++) = '\n';
                     for ( j = 1; j <= indent; j++ )
                        *(qc++) = ( j == 6 ) ? ':' : ' ';
                  }
                  sprintf( qc, fmt, cval );
                  tbuf[ itok ].strmat = qc;
                  tbuf[ itok ].string = cstr;

/* Record that we have done this, so we can remember to write the correct
   INCLUDE line. */
                  fixedint = 1;
               }
            }
         }

/* INTEGER which already has an explicit length declared - generate a 
   warning. */
         else if ( t == INTEGER && t1 == '*' ) {
            fprintf( stderr, "%s: INTEGER*%s declaration not changed\n", 
                             name, tbuf[ i + 2 ].strmat );
         }

/* EQUIVALENCE statement - generate a warning. */
         else if ( t == EQUIVALENCE ) {
            fprintf( stderr, "%s: EQUIVALENCE statement found\n", name );
         }

/* Integer specific intrinsic function call - generate a warning. */
         else if ( t == IABS || t == ISIGN || t == IDIM || t == MAX0 ||
                   t == AMAX0 || t == MIN0 || t == AMIN0 ) {
            fprintf( stderr, "%s: INTEGER-specific intrinsic name %s\n",
                            name, tbuf[ i ].strmat );
         }

/* Start of module - record the name. */
         else if ( ( t == FUNCTION || t == SUBROUTINE || t == BLOCKDATA ) 
                 && ( t1 == IDENTIFIER ) ) {
            strcpy( modname, tbuf[ i + 1 ].strmat );
         }

/* IMPLICIT NONE statement - record that it has occurred. */
         else if ( t == IMPLICITNONE ) {
            implicitnone = 1;
         }

/* Include line - mark it for interpolating our own includes afterwards. 
   If we've already done this for the current module, don't do it again -
   this way the new include line is interpolated after the first existing
   one, which is (slightly) more likely to be where it should be. */
         else if ( ( itok = i - 2 ) > 0 
                  && tbuf[ itok - 1 ].tokval == LINE_START
                  && tbuf[ itok + 0 ].tokval == INCLUDE
                  && tbuf[ itok + 1 ].tokval == STRING_CONSTANT
                  && tbuf[ itok + 2 ].tokval == LINE_END 
                  && ! *incbuf ) {
            int spc;
            qc = incbuf;
            for ( pc = tbuf[ itok - 1 ].string; *pc; pc++ ) *(qc++) = *pc;
            for ( pc = tbuf[ itok ].string; *pc; pc++ ) *(qc++) = *pc;
            for ( pc = tbuf[ itok + 1 ].string; 
                  pc < tbuf[ itok + 1 ].strmat; pc++ ) *(qc++) = *pc;
            for ( pc = "'EXT_PAR'"; *pc; pc++ ) *(qc++) = *pc;
            if ( strchr( tbuf[ itok + 2 ].string, '!' ) ) {
               spc = strlen( tbuf[ itok + 1 ].strmat ) - strlen( "'EXT_PAR'" )
                   + strchr( tbuf[ itok + 2 ].string, '!' ) 
                   - tbuf[ itok + 2 ].string;
               while ( spc-- ) *(qc++) = ' ';
               for ( pc = "! For large integer type constants"; *pc; pc++ )
                   *(qc++) = *pc;
            }
            *(qc++) = '\n';
            *(qc++) = '\0';
            outmark( incbuf );
         }

/* End of module - tidy up. */
         if ( ( t == END && t1 == LINE_END ) || i == leng - 1 ) {

/* Try to make sure "INCLUDE 'EXT_PAR'" will be inserted, or not inserted,
   as appropriate, and warn if this is not possible. */
            if ( fixedint ) {
               if ( ! *incbuf )
                  fprintf( stderr, "%s: Nowhere to INCLUDE 'EXT_PAR'\n", name );
            }
            else {
               *incbuf = '\0';
            }
            
/* Warn if we have not encountered an "IMPLICIT NONE" but should have done. */
            if ( ! implicitnone && *modname ) {
               fprintf( stderr, "%s: No IMPLICIT NONE in module %s\n", 
                                name, modname );
            }
            *modname = '\0';
            implicitnone = 0;

/* Flush output buffer. */
            outwrite();
            *incbuf = '\0';
            fixedint = 0;
         }
         
      }

   }


   int cleng = 0;
   int cbufsiz = 0;
   int iinterp = -1;
   char *cbuf = NULL;
   char *cinterp = NULL;

   void outmark( char *buf ) {
      cinterp = buf;
      iinterp = cleng - 1;
   }

   void outchar( char c ) {
      if ( cleng + 1 >= cbufsiz  ) {
         cbufsiz += BUFINC;
         cbuf = memok( realloc( cbuf, cbufsiz ) );
      }
      cbuf[ cleng++ ] = c;
   }

   void outwrite() {
      char *pc;
      char c;
      int i;
      for ( i = 0; i < cleng; i++ ) {
         putchar( cbuf[ i ] );
         if ( i == iinterp ) 
            for ( pc = cinterp; c = *pc; pc++ )
               putchar( c );
      }
      cleng = 0;
      cbuf = NULL;
      cbufsiz = 0;
      cinterp = NULL;
      iinterp = -1;
   }


/* $Id$ */
