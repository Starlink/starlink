/*
*+
*  Name:
*     inscnf
*
*  Purpose:
*     Interpolate CNF_PVAL calls round %VAL arguments where necessary.
*
*  Usage:
*     inscnf [ in [ out ] ] 
*
*  Description:
*     This short program is a filter which takes FORTRAN 77 source code
*     and modifies it so that text which is the argument of a %VAL 
*     directive is wrapped in a call to CNF_PVAL; i.e. input text
*
*        %VAL( IPTR )
*
*     is changed to
*
*        %VAL( CNF_PVAL( IPTR ) )
*
*     If the call to CNF_PVAL is already present no change is made.
*     Lines with no references to the %VAL directive are left alone,
*     except that trailing whitespace may be stripped.
*
*     The program must also insert a line including the CNF_PAR 
*     include file.
*
*     Attention is paid to fortran 77 source format, so that lines are
*     more than 72 characters long are avoided (unless they were there
*     in the first place.
*
*     Characters '\r' (carriage return) and '\t' (tab) might possibly
*     cause erroneous line breaking - if any are encountered a warning
*     is given (these shouldn't be in the source really).
*     Code using columns 73-80 of the source cards for comments is 
*     likely to be mangled (nobody does this any more do they?).
*
*     Under certain improbable circumstances it is possible for the 
*     program to get stuck trying to break a line; in this case it will
*     exit with error status and an error message.
*
*     Some effort is made to make the output aesthetically pleasing:
*     line breaks are done, where possible, following the usage in, e.g.,
*     KAPPA.  An attempt is made copy the style of case usage and bracket 
*     spacing from the input.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     06-JAN-2000 (MBT):
*        Initial version.
*-
*/


#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>

#define MAXCONT 100                     /* Maximum continuation lines        */
#define MAXNEST 100                     /* Deepest level of bracket nesting  */
#define LBUFSIZ 4000                    /* Longest source line               */


/* Local function prototypes. */
   void addcpv( char *buffer, char *interp[], int leng );
   void inscnf();
   void writeout();
   void outchar( char c );
   void interpnow();

/* Global variables. */
   char *name;                          /* Name of the program               */
   int tabwarn = 0;                     /* Dangerous tab characters found    */


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
*     This routine sets up standard input and standard output as usual
*     for a Unix filter type program.  It then calls the data processing
*     routines.
*-
*/

/* Declare local variables. */
      char *pc;                         /* Pointer to character              */
      char *usagef;                     /* Usage format string               */

/* Get name of program etc. */
      name = *(argv++);
      argc--;
      if ( ( pc = strrchr( name, '/' ) ) != NULL ) name = pc + 1;
      usagef = "Usage: %s [-s] [ in [ out ] ]\n";
   
/* Open standard input and output appropriately according to command line
   arguments, in the normal filter-type way. */
      switch( argc ) {
         case 2:
            if ( freopen( argv[ 1 ], "w", stdout ) == NULL ) {
               perror( argv[ 1 ] );
               exit( 1 );
            }
         case 1:
            if ( freopen( argv[ 0 ], "r", stdin ) == NULL ) {
               perror( argv[ 0 ] );
               exit( 1 );
            }
         case 0:
            break;
         default:
            printf( usagef, name );
            exit( 1 );
      }

/* Filter input to output. */
      inscnf();

/* Finish. */
      return 0;
   }



   int tokmatch( int *toks, ... ) {   /* Last arg should be zero. */
       va_list ap;
       int mtok;

       va_start( ap, toks );
       while ( mtok = va_arg( ap, int ) ) {
          if ( mtok != *(toks++) ) {
             va_end( ap );
             return 0;
          }
       }
       va_end( ap );
       return 1;
   }



#include "ygen.h"


   struct outchr {
      char *interp;
      short flag;
      unsigned char indent;
      char chr;
   };


   void outbuf( struct outchr *buf, int leng ) {
      char c;
      char chr;
      char ebuf[ LBUFSIZ ];
      int eindent[ LBUFSIZ ];
      char *interp;
      int col;
      int done;
      int epos;
      int flag;
      int hadint;
      int i;
      int ind;
      int indent;
      int j;
      int k;
      int level;
      int ninterp = 0;
      int nonblank;
      int oc;
      int started;
      int we;

      epos = -1;
      hadint = 0;
      for ( oc = 0; oc < leng; oc++ ) {

         chr = buf[ oc ].chr;
         interp = buf[ oc ].interp;
         flag = buf[ oc ].flag;
         indent = buf[ oc ].indent;

         if ( flag == LINE_START ) {
            epos = 0;
            level = 0;
            hadint = 0;
         }

/* Outside a line, interpolations are output without special care. */
         if ( epos < 0 ) {
            if ( interp != NULL ) {
               while ( c = *( interp++ ) ) 
                  putchar( c );
               hadint = 1;
            }
            putchar( chr );
         }

/* Within a line it is ensured that interpolations do not cause characters
   to overflow beyond column 72. */
         else {

            if ( interp != NULL ) {
               while ( c = *( interp++ ) ) {
                  ebuf[ epos ] = c;
                  eindent[ epos ] = indent;
                  epos++;
               }
               hadint = 1;
            }

/* It's not a line end; just add it to the buffer. */
            if ( chr != '\n' ) {
               ebuf[ epos ] = chr;
               eindent[ epos ] = indent;
               epos++;
            }

/* It's a line end.  Process the line now. */
            else {

/* Strip trailing whitespace if there have been interpolations, as it
   simplifies matters later on. */
               if ( hadint )
                  while ( epos >= 0 && ebuf[ epos - 1 ] == ' ' ) epos--;

/* There are no interpolations, or any interpolations do not cause the
   line to overflow 72 columns.  Output it without further ado. */
               if ( ! hadint || epos < 72 ) {
                  for ( i = 0; i < epos; i++ ) putchar( ebuf[ i ] );
               }

/* Interpolations cause the line to overflow.  We will have to break the 
   line. */
               else {

/* The procedure is to identify 'word boundaries'; if all text up to the
   next boundary can be output on the current line do so, otherwise
   introduce a line break and continue with the rest of the input.
   A word starts at a '%' and ends at the first character at the same
   level of bracketing which is none of ')', ',' or ' '.  */
                  i = 0;
                  col = 1;
                  while ( i < epos ) {

/* Find the next place a line break could occur. */
                     done = 0;
                     level = 0;
                     started = 0;
                     for ( j = i; !done && j < epos; j++ ) {
                        if ( ebuf[ i ] == '%' ) {
                           switch ( ebuf[ j + 1 ] ) {
                              case '(':
                                 level++;
                                 started = 1;
                                 break;
                              case ')':
                                 level--;
                                 break;
                              case ' ':
                              case ',':
                                 break;
                              case '\n':
                                 done = 1;
                                 break;
                              default:
                                 done = ( started && level <= 0 );
                           }
                        }
                        else {
                           done = ( ebuf[ j + 1 ] == '%' ) || col + j == 6;
                        }
                     }
                     j--;
                     for ( we = j; isspace( ebuf[ we ] ) && we > i; we-- );
      
/* If this word won't fit on the current line, output a line break now. */
                     if ( we - i + col > 72 ) {
      
/* Set best indent value. */
                        ind = eindent[ i ];
                        if ( ind < 6 ) ind = 6;
                        if ( we - i + ind + 1 > 72 ) ind = 9;
                        if ( we - i + ind + 1 > 72 ) ind = 6;
                        if ( we - i + ind + 1 > 72 )
                           fprintf( stderr, 
                                    "%s: Failed to break line\n", name );
      
/* Output the line break, unless we've just done one. */
                        if ( col != 7 ) {
                           putchar( '\n' );
                           for ( k = 1; k <= ind; k++ )
                              putchar( k == 6 ? ':' : ' ' );
                           col = ind;
                        }
                     }
      
/* Now output the word. */
                     while ( i <= j ) {
                        if ( 1 ) {
                           putchar( ebuf[ i ] );
                           col++;
                        }
                        i++;
                     }
                  }

               }

               putchar( '\n' );
               epos = 0;
               hadint = 0;
            }
         }

         if ( flag == LINE_END ) {
               for ( i = 0; i < epos; i++ ) putchar( ebuf[ i ] );
               epos = -1;
         }

      }
   }


   void inscnf() {
/*
*+
*  Name:
*     inscnf
* 
*  Purpose:
*     Perform data processing for inscnf program.
*
*  Invocation:
*     inscnf()
*
*  Description:
*     This routine reads characters from standard input and writes them
*     to standard output.  The output is substantially similar to the
*     input except that arguments of %VAL are wrapped in the CNF_PVAL
*     function.
*-
*/
#define MAXTOK 1000                          /* Maximum tokens in a line     */
#define LINELENG 160
#define MAXLEVEL 100

      int tokid[ MAXTOK ];
      int tokpos[ MAXTOK ];
      char c;
      char *tokstr[ MAXTOK ];
      int col;
      int incpos = 0;
      int i;
      int j;
      int leng;
      int level;
      int ntok;
      int nval = 0;
      int stcol[ MAXLEVEL ];
      int tok;
      int yleng;
      int bufsiz = 0;
      struct outchr *buf;
      char *cpo[ 4 ];
      char *cpc[ 4 ];
      char *pc;
      int scase;
      int sspace;

/* Set up strings for output. */
#define CASE_UPPER 0
#define CASE_LOWER 2
#define SPACE_YES  0
#define SPACE_NO   1
      cpo[ CASE_UPPER | SPACE_YES ] = " CNF_PVAL(";
      cpo[ CASE_UPPER | SPACE_NO  ] = "CNF_PVAL(";
      cpo[ CASE_LOWER | SPACE_YES ] = " cnf_pval(";
      cpo[ CASE_LOWER | SPACE_NO  ] = "cnf_pval(";
      cpc[ CASE_UPPER | SPACE_YES ] = " )";
      cpc[ CASE_UPPER | SPACE_NO  ] = ")";
      cpc[ CASE_LOWER | SPACE_YES ] = " )";
      cpc[ CASE_LOWER | SPACE_NO  ] = ")";
      scase = CASE_UPPER;
      sspace = SPACE_YES;

/* Initialise. */
      ntok = 0;
      leng = 0;

      while ( tok = yylex() ) {

/* Assemble a line in terms of tokens. */
         tokid[ ntok ] = tok;
         tokpos[ ntok ] = leng;
         tokstr[ ntok ] = yylval;
         tokid[ ++ntok ] = 0;
         if ( ntok >= MAXTOK ) {
            fprintf( stderr, "%s: Too many tokens in line\n", name );
            exit( 1 );
         }

/* Add text to output buffer. */
         yleng = strlen( yylval );
         if ( leng + yleng >= bufsiz ) {
            bufsiz += BUFINC;
            buf = memok( realloc( buf, bufsiz * sizeof( struct outchr ) ) );
            for ( i = 0; i < BUFINC; i++ ) {
               buf[ bufsiz - i - 1 ].interp = NULL;
               buf[ bufsiz - i - 1 ].chr = '\0';
               buf[ bufsiz - i - 1 ].flag = 0;
               buf[ bufsiz - i - 1 ].indent = 0;
            }
         }
         for ( i = 0; i < yleng; i++ ) {
            buf[ leng + i ].chr = yylval[ i ];
         }
         switch( tok ) {
            case LINE_START:
            case LINE_END:
            case '(':
            case ')':
               buf[ leng ].flag = tok;
            default:
         }
         leng += yleng;

/* If we have reached the end of a line, process the line. */
         if ( tok == LINE_END || tok == COMMENT_LINE || tok == BLANK_LINE ) {

/* INCLUDE line - mark as spot for interpolating "INCLUDE 'CNF_PVAL'". */
            if ( tokmatch( tokid, LINE_START, INCLUDE, STRING_CONSTANT, 
                                  LINE_END, 0 ) ) { 
               if ( incpos ) buf[ incpos ].interp = NULL;
               incpos = tokpos[ 3 ] + yleng;
               buf[ incpos ].interp = pc = calloc( LINELENG, 1 );
               strcpy( pc, "      " );
               pc += 6;
               for ( i = tokpos[ 1 ]; ( *(pc++) = buf[ i ].chr ) != '\''; i++ );
               strcpy( pc, "CNF_PAR'" );
               for ( j = 0; ( c = buf[ i + j ].chr ) != '\n' && c != '!' ; j++);
               if ( c == '!' ) {
                  while ( --j > 8 ) strcat( pc, " " );
                  strcat( pc, "! For CNF_PVAL function" );
               }
               strcat( pc, "\n" );
            }

/* END line - reset some counters. */
            else if ( tokmatch( tokid, LINE_START, END, LINE_END, 0 ) ) {
               buf[ tokpos[ 1 ] ].flag = END;
               if ( nval == 0 ) {
                  if ( incpos ) 
                     buf[ incpos ].interp = NULL;
               }
               else {
                  if ( ! incpos ) 
                     fprintf( stderr, "%s: Failed to include CNF_PAR\n", name );
               }
               nval = 0;
               incpos = 0;
            }

/* Otherwise, it is potentially an executable line. */
            else {

/* Check if it contains a %VAL invocation. */
               for ( i = 0; i < ntok; i++ ) {
                  if ( tokmatch( tokid + i, (int) '%', VAL, (int) '(', 0 )
                     && tokid[ i + 3 ] != CNF_PVAL ) {
                     scase = islower( buf[ tokpos[ i + 1 ] ].chr )
                                ? CASE_LOWER : CASE_UPPER;
                     sspace = buf[ tokpos[ i + 2 ] + 1 ].chr == ' '
                                ? SPACE_YES : SPACE_NO;
                     level = 1;
                     for ( j = i + 3; j < ntok; j++ ) {
                        if ( tokid[ j ] == '(' ) {
                           level++;
                        }
                        else if ( tokid[ j ] == ')' ) {
                           level--;
                           if ( level == 0 ) {
                              buf[ tokpos[ i + 3 ] ].interp = 
                                 cpo[ scase | sspace ];
                              buf[ tokpos[ j ] ].interp = cpc[ scase | sspace ];
                              nval++;
                              break;
                           }
                        }
                     }
                  }
               }

/* Set natural indent levels. */
               level = 0;
               col = 0;
               for ( i = tokpos[ 0 ]; i <= tokpos[ ntok - 1 ]; i++ ) {
                  if ( ! isspace( buf[ i ].chr ) && col > 6 ) {
                     if ( stcol[ level ] > col ) stcol[ level ] = col;
                  }
                  if ( buf[ i ].flag == '(' ) {
                     level++;
                     stcol[ level ] = 99999;
                  }
                  else if ( buf[ i ].flag == ')' ) {
                     level--;
                  }
                  col++;
                  if ( buf[ i ].chr == '\n' ) {
                     col = 0;
                  }
                  buf[ i ].indent = stcol[ level ] < 72 ? stcol[ level ] : 0;
               }

            }
            ntok = 0;
         }
      }

/* End of text; ensure that the INCLUDE lines are in the right place. */
      if ( nval == 0 ) {
         if ( incpos ) 
            buf[ incpos ].interp = NULL;
      }
      else {
         if ( ! incpos ) 
            fprintf( stderr, "%s: Failed to include CNF_PAR.\n", name );
      }

/* Pass control to the output routine. */
      outbuf( buf, leng );
   }


/* $Id$ */
