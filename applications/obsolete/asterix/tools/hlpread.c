/*+
 *  Name:
 *     hlpread.c

 *  Purpose:
 *     Call the top level ASTERIX help reader

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     hlpread <library_name> [-f [file]] [-p] [-q] [args]

 *  Description:
 *     This function combines the arguments (if any) supplied on the command
 *     line and forms a Fortran character string. The string and its length
 *     are then passed separately to the top level Fortran subroutine
 *     HLPREAD_INT.

 *  Arguments:
 *     argc = const int (Given)
 *        The number of command arguments, including program name
 *     argv = const *char[] (Given)
 *        List of pointers to command arguments

 *  Authors:
 *     David J. Allan (BHVAD::DJA)

 *  History:
 *
 *     9-JUL-1992 (DJA):
 *        Original version.
 *    30-JUL-1993 (DJA):
 *        Added PRINT argument to HLPREAD_TOP. Handle -p and -f qualifiers for
 *        better UNIX syntax.
 *    14-NOV-1994 (DJA):
 *        Library now argument #1, index is found by HLPREAD_TOP given help
 *        root name. Renamed HLPREAD
 *- */

/* Include Statements: */
#include <string.h>                    /* String handling library functions */
#include "f77.h"                       /* c <-> FORTRAN interfacing */
#include "cnf.h"                       /* c <-> FORTRAN strings */

/* External declarations */
void F77_EXTERNAL_NAME(hlpread_top)(         /* The top level help routine */
     CHARACTER(lib),
     CHARACTER(carg),
     CHARACTER(file),
     LOGICAL(spool),
     LOGICAL(screen)
     TRAIL(lib)
     TRAIL(carg)
     TRAIL(file) );

#define BUFLEN   200                    /* Argument buffer length */
#define TRUE     1
#define FALSE    0
#define BOOLEAN  int


void hlperr( char *msg )
  {
  printf( "hlpread : %s\nusage:  hlpread library [-i index] [-f file] [-p] [-q]\n", msg );  
  }


main( int argc, char *argv[] )
  {
  int argl;				/* Length of command argument */
  DECLARE_INTEGER(alen);		/* Length of argument string */
  DECLARE_CHARACTER(aarg,BUFLEN);	/* Argument string */
  DECLARE_CHARACTER(library,80);	/* Help library */
  DECLARE_CHARACTER(file,128);	 	/* Output file */
  DECLARE_LOGICAL(print);               /* Print to file? */
  DECLARE_LOGICAL(screen);              /* Send output to console */

  int i;                                /* Loop over string */
  BOOLEAN got_library = FALSE; 
  int     icopy = 1;                    /* # copied strings */
  
  alen = 0;				/* Various initialisations */
  print = F77_FALSE;
  screen = F77_TRUE;
  file[0] = ' ';

  if ( argc < 2 )
    {
    hlperr( "help library name required" );  
    return;
    }

  else if ( argc > 1 )                  /* Were command args supplied */
    {
    cnf_exprt( argv[1], library, 	/* Export library name to Fortran */
               library_length );
    got_library = TRUE;
    
    for ( i = 2; i<argc; i++ )       	/* Loop over optional args */
      {
      argl = strlen( argv[i] );         /* Get argument length */

      if ( !strncmp("/file=",argv[i],6) )
        {
        cnf_exprt( argv[i]+6, file, 
                     file_length );
        }

      else if ( !strcmp("-f",argv[i]) )	/* Output to file? */
        {
        i++;				/* Move to next argument */

        if ( i < argc )			/* There IS another argument? */
          cnf_exprt( argv[i], file, 
                     file_length );
        else
          cnf_exprt( "asthelp.lis", file, 
                           file_length );
        }

      else if ( !strcmp("/print",argv[i]) /* Spool to printer? */
                || !strcmp("/PRINT",argv[i])
                || !strcmp("-p",argv[i]) )
        print = F77_TRUE;
 
      else if ( !strcmp("/noscreen",argv[i]) /* Suppress console output */
                || !strcmp("/NOSCREEN",argv[i])
                || !strcmp("-q",argv[i]) )
        screen = F77_FALSE;
 
      else
        {
        strncpy( aarg+alen,             /* Concatenate string data */
                       argv[i], argl);  
        alen += argl;
        aarg[alen++] = ' ';             /* Put space between args */
        icopy++;
        }
      }
    }

  for( i=alen; i<BUFLEN; i++ )		/* Pad remainder of string */
    aarg[i] = ' ';

  F77_CALL(hlpread_top)(		/* Invoke the help system */
       CHARACTER_ARG(library),
       CHARACTER_ARG(aarg),
       CHARACTER_ARG(file),
       LOGICAL_ARG(&print),
       LOGICAL_ARG(&screen)
       TRAIL_ARG(library)
       TRAIL_ARG(aarg)
       TRAIL_ARG(file) );
  }
