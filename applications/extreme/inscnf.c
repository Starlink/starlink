
#include <stdio.h>
#include <string.h>
#include <stddef.h>

#define MAXNEST 100
#define LBUFSIZ 2000
#define MAXCONT 20


/* Local function prototypes. */
   void outbuf( char *buffer, char *interp[], int leng );
   void inscnf();


/* Main function of program. */
   int main( int argc, char **argv ) {

/* Declare local variables. */
      char *name;
      char *usagef;

/* Get name of program etc. */
      name = *(argv++);
      argc--;
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


/* Main data processing routine. */
   void inscnf() {

/* Declare local variables. */
      char *cpc;
      char *cpo;
      char *interp[ LBUFSIZ ];
      char buffer[ LBUFSIZ ];
      char c;

      int column;
      int comment;
      int flush;
      int leng;
      int level;
      int spotcpv;
      int spotval;
      int valat[ MAXNEST ];

/* Set up strings for output. */
      cpo = " CNF_PVAL(";
      cpc = " )";

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
                  if ( valat[ level ] ) {
                     interp[ valat[ level ] - 1 ] = cpo;
                     interp[ leng - 1 ] = cpc;
                  }
                  level--;
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
         if ( c == '\n' || c == '\r' ) column = 0;
      }

/* Flush any remaining text in buffer. */
      outbuf( buffer, interp, leng );
   }


   void outbuf( char *buffer, char *interp[], int leng ) {
/*
*+
*     If any interpolated text is present, this routine should be called 
*     with a single line of source code (i.e. a set of continuation lines
*     with matched brackets and so on).  Otherwise line breaks may get
*     put in ugly (though not incorrect) places.  If there is
*     no interpolation to be done, then output is identical to input,
*     so it doesn't matter.
*-
*/
      char c;
      char *ls[ MAXCONT ];
      char *pc;
      char *qc;
      char *we;
      char ebuf[ LBUFSIZ ];
      static int col = 0;
      int done;
      int i;
      int isinterp[ MAXCONT ];
      int indent;
      int j;
      int line;
      int lines;
      int ll[ MAXCONT ];
      int level;
      int started;

/* Start of source line, so it shouldn't be within the argument of a %VAL. */

/* Copy the input buffer, with interpolations, into an expanded buffer.
   Identify line starts, line lengths, and whether each line contains any 
   interpolations. */
      line = 0;
      qc = ebuf;
      isinterp[ line ] = 0;
      ls[ line ] = ebuf;
      ll[ line ] = 0;
      for ( i = 0; i < leng; i++ ) {
         *(qc++) = buffer[ i ];
         if ( interp[ i ] != NULL ) {
            isinterp[ line ] = 1;
            for ( pc = interp[ i ]; *pc; pc++ )
               *(qc++) = *pc;
         }
         if ( buffer[ i ] == '\n' || i == leng - 1 ) {
            ll[ line ] = qc - ls[ line ];
            line++;
            isinterp[ line ] = 0;
            ls[ line ] = qc;
            ll[ line ] = 0;
         }
      }
      lines = line + 1;

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
                        case '\n':
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
               for ( we = qc; *we == ' ' && we >= pc; we-- );

/* If this word won't fit on the current line, output a line break now. */
               if ( we - pc + col > 73 ) {
                  putchar( '\n' );
                  indent = 6;
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


