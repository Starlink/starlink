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

#define MAXNEST 100                     /* Deepest level of bracket nesting  */
#define LBUFSIZ 4000                    /* Longest source line               */
#define MAXCONT 100                     /* Maximum continuation lines        */


/* Local function prototypes. */
   void outbuf( char *buffer, char *interp[], int leng );
   void inscnf();

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

/* Declare local variables. */
      char *cpc[ 4 ];                   /* CNF_PVAL closer strings           */
      char *cpo[ 4 ];                   /* CNF_PVAL opener strings           */
      char *interp[ LBUFSIZ ];          /* Pointers to interpolated strings  */
      char buffer[ LBUFSIZ ];           /* Buffer for raw input text         */
      char c;                           /* Character read                    */
      char lastc;                       /* Previous character read           */
      int column;                       /* Column of input text              */
      int comment;                      /* Is it a comment line?             */
      int flush;                        /* Is it time to process buffer?     */
      int leng;                         /* Length of buffer                  */
      int level;                        /* Level of bracket nesting          */
      int scase;                        /* Apparent case of source code      */
      int sspace;                       /* Apparent spacing convention       */
      int spotcpv;                      /* Have we found CNF_PVAL string?    */
      int spotval;                      /* Have we found %VAL string?        */
      int valat[ MAXNEST ];             /* Nesting level of %VAL start       */

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

/* Initialise values for source code processing. */
      leng = 0;
      level = 0;
      column = 0;
      comment = 0;
      spotval = 0;
      spotcpv = 0;

/* Cycle through characters of source file. */
      while ( ( c = getchar() ) != EOF ) {

/* Increment source column and buffer position. */
         leng++;
         column++;

/* Copy character to output buffer. */
         buffer[ leng - 1 ] = c;
         interp[ leng - 1 ] = NULL;

/* Work out whether this is a suitable time to process the current contents
   of the buffer.  Suitable is when it contains a whole source line, give
   or take some ignorable characters near the start or finish.  This will
   be the case when a comment line has just started or finished, or when
   we see the start of a source line which is not a continuation line. */
         flush = 0;
         if ( column == 1 && strchr( "*cCdD!", c ) ) { 
            comment = 1;
            flush = 1;
         }
         if ( comment && ( c == '\n' || c == '\r' ) ) {
            comment = 0;
            flush = 1;
         }
         if ( column == 6 && strchr( " 0", c ) ) {
            flush = 1;
         }

/* If the buffer is ready to be processed, then process it and reset 
   some values appropriately. */
         if ( flush ) {
            outbuf( buffer, interp, leng );
            leng = 0;
            level = 0;
            spotval = 0;
            spotcpv = 0;
         }

/* Significant part of source card. */
         if ( column > 6 && column < 73 && ! comment ) {

/* Act appropriately for each significant character. */
            switch( c ) {

               case '(':
                  level++;
                  valat[ level ] = 0;
                  if ( spotval == 4 ) valat[ level ] = leng;
                  if ( spotcpv == 8 ) valat[ level - 1 ] = 0;
                  spotval = spotcpv = 0;
                  break;
               case ')':
                  sspace = ( lastc == ' ' ) ? SPACE_YES : SPACE_NO;
                  if ( valat[ level ] ) {
                     interp[ valat[ level ] - 1 ] = cpo[ scase | sspace ];
                     interp[ leng - 1 ] = cpc[ scase | sspace ];
                  }
                  if ( level > 0 ) level--;
                  spotval = spotcpv = 0;
                  break;
               case '%':
                  spotval = 1;
                  spotcpv = 0;
                  break;
               case 'v':
               case 'V':
                  spotval = ( spotval == 1 ) ? 2 : 0;
                  spotcpv = ( spotcpv == 5 ) ? 6 : 0;
                  break;
               case 'a':
               case 'A':
                  spotval = ( spotval == 2 ) ? 3 : 0;
                  spotcpv = ( spotcpv == 6 ) ? 7 : 0;
                  break;
               case 'l':
               case 'L':
                  spotval = ( spotval == 3 ) ? 4 : 0;
                  spotcpv = ( spotcpv == 7 ) ? 8 : 0;
                  scase = ( c == 'l' ) ? CASE_LOWER : CASE_UPPER;
                  break;

               case 'c':
               case 'C':
                  spotcpv = ( spotcpv == 0 ) ? 1 : 0;
                  spotval = 0;
                  break;
               case 'n':
               case 'N':
                  spotcpv = ( spotcpv == 1 ) ? 2 : 0;
                  spotval = 0;
                  break;
               case 'f':
               case 'F':
                  spotcpv = ( spotcpv == 2 ) ? 3 : 0;
                  spotval = 0;
                  break;
               case '_':
                  spotcpv = ( spotcpv == 3 ) ? 4 : 0;
                  spotval = 0;
                  break;
               case 'p':
               case 'P':
                  spotcpv = ( spotcpv == 4 ) ? 5 : 0;
                  spotval = 0;
                  break;
            
               case ' ':
               case '\n':
               case '\t':
               case '\r':
                  break;

               default:
                  spotval = spotcpv = 0;
            }
         }
         lastc = c;
         if ( c == '\n' || c == '\r' ) column = 0;
      }

/* Flush any remaining text in buffer. */
      outbuf( buffer, interp, leng );

/* Warn about dangerous characters. */
      if ( tabwarn )
         fprintf( stderr, 
                  "%s: Warning - %d '%s's in modified lines.\n", 
                  name, tabwarn, "\\t" );
   }


   void outbuf( char *buffer, char *interp[], int leng ) {
/*
*+
*  Name:
*     outbuf
*
*  Purpose:
*     Output text with interpolations.
*
*  Usage:
*     outbuf( char *buffer, char *interp[], int leng )
*
*  Description:
*     This routine outputs a small chunk of text with interpolations
*     as given by its arguments.  Where necessary it will make suitable
*     line breaks.
*
*     If any interpolated text is present, this routine should be called 
*     with a single line of source code (i.e. a set of continuation lines
*     with matched brackets and so on).  Otherwise line breaks may get
*     put in ugly (though not incorrect) places.  If there is
*     no interpolation to be done, then output is identical to input,
*     so it doesn't matter.
*
*  Arguments:
*     buffer = char *
*        A pointer to the start of the raw input text.
*     interp[] = char *
*        Interp is an array containing one pointer to char for each 
*        character in buffer.  If any of its elements is non-NULL,
*        then it is interpreted as a pointer to a null-terminated string
*        to interpolate after the corresponding character of buffer in
*        the output text.  (Note the term `interpolate' is used here in
*        its textual sense, and has nothing to do with, e.g. linear
*        interpolation - cf. Perl documentation etc).
*     leng = int
*        The number of characters in buffer, and also the number of 
*        valid pointers in interp.
*-
*/

/* Declare local variables. */
      char c;                           /* Character read                    */
      char *ls[ MAXCONT ];              /* Start of line                     */
      char *pc;                         /* Pointer to character              */
      char *qc;                         /* Pointer to character              */
      char *we;                         /* Pointer to end of word            */
      char ebuf[ LBUFSIZ ];             /* Buffer holding expanded text      */
      static int col = 0;               /* Current output column             */
      int done;                         /* Has a line break been found?      */
      int eindent[ LBUFSIZ ];           /* Preferred indentation at each char*/
      int eleng;                        /* Length of ebuf                    */
      int i;                            /* Loop counter                      */
      int isinterp[ MAXCONT ];          /* Does line contain interpolations? */
      int istab[ MAXCONT ];             /* Does line contain tab characters? */
      int indent;                       /* Characters of indent              */
      int j;                            /* Loop counter                      */
      int lcol;                         /* Temporary column counter          */
      int line;                         /* Index of current line             */
      int lines;                        /* Number of lines in buffer         */
      int ll[ MAXCONT ];                /* Length of lines                   */
      int level;                        /* Parenthesis nesting level         */
      int started;                      /* Has word count got started?       */
      int stcol[ MAXNEST ];             /* Start column of nesting level     */

/* Copy the input buffer, with interpolations, into an expanded buffer.
   Identify line starts, line lengths, and whether each line contains any 
   interpolations. */
      line = 0;
      qc = ebuf;
      isinterp[ line ] = 0;
      istab[ line ] = 0;
      ls[ line ] = ebuf;
      ll[ line ] = 0;
      for ( i = 0; i < leng; i++ ) {
         *(qc++) = buffer[ i ];
         if ( interp[ i ] != NULL ) {
            isinterp[ line ] = 1;
            for ( pc = interp[ i ]; *pc; pc++ )
               *(qc++) = *pc;
         }
         if ( buffer[ i ] == '\t' ) {
            istab[ line ] = 1;
         }
         if ( buffer[ i ] == '\n' || i == leng - 1 ) {

/* If there are any interpolations on this line, strip trailing whitespace
   to simplify matters later on. */
            if ( isinterp[ line ] && buffer[ i ] == '\n' ) {
               while ( qc[ -1 ] == ' ' && qc > ls[ line ] ) {
                  qc[ -1 ] = '\n';
                  qc--;
               }
            }
            if ( isinterp[ line ] && istab[ line ] ) {
               tabwarn++;
            }
            ll[ line ] = qc - ls[ line ];
            line++;
            if ( line >= MAXCONT - 1 ) {
               fprintf( stderr, "%s: ERROR - too many continuation lines.\n",
                        name );
               exit( 1 );
            }
            isinterp[ line ] = 0;
            istab[ line ] = 0;
            ls[ line ] = qc;
            ll[ line ] = 0;
         }
         if ( ( qc - ebuf ) >= LBUFSIZ ) {
            fprintf( stderr, "%s: ERROR - buffer too small.\n", name );
            exit( 1 );
         }
      }
      lines = line + 1;
      eleng = qc - ebuf;

/* Set the natural indent level at each position of the buffer.  This is
   to do with brackets. */
      level = 0;
      lcol = col;
      stcol[ level ] = 0;
      for ( pc = ebuf; pc - ebuf < eleng; pc++ ) {
         if ( lcol > 6 ) {
            switch ( *pc ) {
               case '(':
                  level++;
                  stcol[ level ] = 0;
                  break;
               case ')':
                  level--;
                  break;
               case ' ':
               case '\t':
               case '\n':
                  break;
               default:
                  if ( stcol[ level ] == 0 ) stcol[ level ] = lcol - 1;
            }
         }
         eindent[ pc - ebuf ] = stcol[ level ];
         if ( *pc == '\n' ) lcol = 0;
         lcol++;
      }

/* Process expanded buffer a line at a time. */
      for ( line = 0; line < lines; line++ ) {

/* If there are no interpolations in this line, or the line including 
   interpolations can fit on the source card, then output it unchanged. */
         if ( ! isinterp[ line ] || col + ll[ line ] < 73 ) {
            for ( i = 0; i < ll[ line ]; i++ ) {
               putchar( ls[ line ][ i ] );
               if ( ls[ line ][ i ] == '\n' ) col = 0;
               col++;
            }
         }

/* The expanded line will not fit in 72 columns.  We need to add some 
   line breaks. */
         else {

/* The procedure is to identify 'word boundaries'; if all text up to the
   next boundary can be output on the current line do so, otherwise 
   introduce a line break and continue with the rest of the input.
   A word starts at a '%' and ends at the first character at the same
   level of bracketing which is none of ')', ',' or ' '.  */
            pc = ls[ line ];
            while ( pc < ls[ line ] + ll[ line ] ) {

/* Find the next place a line break could occur. */
               done = 0;
               level = 0;
               started = 0;
               for ( qc = pc; qc - ls[ line ] < ll[ line ] - 1; qc++ ) {
                  if ( *pc == '%' ) {
                     switch ( qc[ 1 ] ) {
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
                     done = ( qc[ 1 ] == '%' );
                  }
                  if ( done ) break;
               }
               for ( we = qc; isspace( *we ) && we > pc; we-- );

/* If this word won't fit on the current line, output a line break now. */
               if ( we - pc + col > 73 ) {
                  indent = eindent[ pc - ebuf ];
                  if ( indent < 6 ) indent = 6;
                  if ( we - pc + indent > 73 ) indent = 9;
                  if ( we - pc + indent > 73 ) indent = 6;
                  if ( we - pc + indent > 73 ) 
                     fprintf( stderr, "%s: Failed to break line\n", name );
                  putchar( '\n' );
                  indent = eindent[ pc - ebuf ];
                  for ( i = 1; i <= indent; i++ )
                     putchar( i == 6 ? ':' : ' ' );
                  col = indent;
               }

/* Now output the word. */
               while ( pc <= qc ) {
                  putchar( *pc );
                  if ( *pc == '\n' ) col = 0;
                  pc++;
                  col++;
               }
            }
         }
      }
   }

/* $Id$ */
