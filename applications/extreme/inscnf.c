
#include <stdio.h>
#include <string.h>

#define MAXNEST 100


/* Local function prototypes. */
   void outchar( char c, int careful );
   void outstr( char *string, int careful );
   void outinit();


/* Main function of program. */
   int main( int argc, char **argv ) {

/* Declare local variables. */
      char *cpc;
      char *cpo;
      char *name;
      char *usagef;
      char c;
      char cchar;

      int column;
      int hadval;
      int level;
      int scol[ MAXNEST ];
      int spotval;
      int valat[ MAXNEST ];

/* Get name of program etc. */
      name = *(argv++);
      argc--;
      usagef = "Usage: %s [-d[l][y]] [-s] [ in [ out ] ]\n";
   
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
      cchar = ':';

/* Initialise values for source code processing. */
      level = 0;
      hadval = 0;
      scol[ 0 ] = 0;
      column = 0;

/* Do any necessary setup for output. */
      outinit();

/* Cycle through characters of source file. */
      while( ( c = getchar() ) != EOF ) {
         column++;
         if ( column == 6 && ( c == ' ' || c == '0' ) ) {
            level = 0;
            hadval = 0;
         }
         if ( scol[ level ] <= 6 && column > 6 && ! isspace( c ) )
            scol[ level ] = column;

         outchar( c, hadval );

/* Act appropriately for each character. */
         switch( c ) {
            case '(':
               scol[ ++level ] = 0;
               valat[ level ] = 0;
               if ( spotval == 4 ) {
                  hadval = 1;
                  outstr( cpo, hadval );
                  valat[ level ] = 1;
               }
               break;
            case ')':
               if ( valat[ level ] )
                  outstr( cpc, hadval );
               --level;
               break;
            case '%':
               spotval = 1;
               break;
            case 'v':
            case 'V':
               if ( spotval == 1 ) spotval++;
               break;
            case 'a':
            case 'A':
               if ( spotval == 2 ) spotval++;
               break;
            case 'l':
            case 'L':
               if ( spotval == 3 ) spotval++;
               break;
            case ' ':
            case '\n':
            case '\t':
               break;
            default:
               spotval = 0;
         }
      }

   }


   int outcol;
   int softn;

   void outinit() {
      outcol = 0;
      softn = 0;
   }

   void outstr( char *string, int careful ) {
      for ( ; *string; string++ )
         outchar( *string, careful );
   }
      
   void outchar( char c, int careful ) {

/* Declare local variables. */
      static char linebuf[ 100 ];

/* If no reformatting is to be done, simply output the character as read. */
      if ( ! ( careful || softn ) ) {
         putchar( c );
         if ( c == '\n' )
            outinit();
         else
            outcol++;
      }

/* Reformatting may be required.  In this case, keep a check that we are 
   not over the fortran column limit, and insert appropriate continuation
   formatting if we are.  We also try to break lines at a suitable place
   by buffering text until a break is found. */ 
      else {
         putchar( c );
         if ( c == '\n' )
            outinit();
         else
            outcol++;
      }
   }
