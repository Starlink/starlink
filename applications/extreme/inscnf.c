
#include <stdio.h>
#include <string.h>
#include <stddef.h>

#define MAXNEST 100
#define LBUFSIZ 2000


/* Local function prototypes. */
   void outbuf( char *buffer, char *interp[], int leng );


/* Main function of program. */
   int main( int argc, char **argv ) {

/* Declare local variables. */
      char *cpc;
      char *cpo;
      char *interp[ LBUFSIZ ];
      char *name;
      char *usagef;
      char buffer[ LBUFSIZ ];
      char c;

      int column;
      int hadval;
      int leng;
      int level;
      int spotcpv;
      int spotval;
      int valat[ MAXNEST ];

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

/* Set up strings for output. */
      cpo = " CNF_PVAL(";
      cpc = " )";

/* Initialise values for source code processing. */
      leng = 0;
      level = 0;
      column = 0;
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

/* If a new line is beginning, or it's a comment, then the previous line
   must have finished. */
         if ( ( column == 6 && strchr( " 0", c ) ) 
           || ( column == 1 && strchr( "*cCdD!", c ) ) ) {

/* Flush the buffer. */
            outbuf( buffer, interp, leng );

/* Reset values. */
            leng = 0;
            level = 0;
            spotval = 0;
            spotcpv = 0;
         }

/* Significant part of source card. */
         else if ( column > 6 && column < 73 ) {

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

/* Finish. */
      return 0;
   }


   void outbuf( char *buffer, char *interp[], int leng ) {
      char c;
      static int col = 0;
      int done;
      int fint;
      int i;
      int lint;
      int pos;

      fint = 0;
      lint = 0;
      done = -1;
      for ( pos = 0; pos < leng; pos++ ) {
         if ( interp[ pos ] != NULL ) {
            fint = 1;
            lint += strlen( interp[ pos ] );
         }
      
/* End of input card, or end of buffer.  Output text up to this point. */
         if ( buffer[ pos ] == '\n' || pos == leng - 1 ) {

/* If there are no interpolations to make, output the line undisturbed. */
            if ( ! fint ) {
               for ( i = done + 1; i <= pos; i++ ) 
                  putchar( buffer[ i ] );
            }

/* If the interpolations can be made without overflowing the source card,
   output the line with interpolations. */
            else if ( col + lint + pos - done < 73 ) {
               for ( i = done + 1; i <= pos; i++ ) {
                  putchar( buffer[ i ] );
                  if ( interp[ i ] != NULL ) printf( interp[ i ] );
               }
            }

/* Interpolations would overflow the source card.  We have to break the
   line somewhere sensible. */
            else {
               fprintf( stderr, "** Overflow **\n" );
               exit( 1 );
            }
            done = pos;
            fint = 0;
            lint = 0;
         }
      }
   }


