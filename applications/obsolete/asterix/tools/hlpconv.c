/*+
 *  Name:
 *     hlpconv.c

 *  Purpose:
 *     Convert raw help text to one of 3 output forms

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     hlpconv <html> <mode> <output>

 *  Description:
 *     This function combines the arguments (if any) supplied on the command
 *     line and forms a Fortran character string. The string and its length
 *     are then passed separately to the top level Fortran subroutine
 *     HLPCONV_INT.

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
void F77_EXTERNAL_NAME(hlpconv_top)(         /* The top level help routine */
     CHARACTER(inp),
     CHARACTER(mode),
     CHARACTER(out)
     TRAIL(inp)
     TRAIL(mode)
     TRAIL(out) );


void hlperr( char *msg )
  {
  printf( "hlpconv : %s\nusage:  hlpconv input hlp|tex|html output\n", msg );
  }


main( int argc, char *argv[] )
  {
  DECLARE_CHARACTER(raw,128);	 	/* Input file */
  DECLARE_CHARACTER(mode,4);	 	/* Mode */
  DECLARE_CHARACTER(out,128);	 	/* Output file */

  if ( argc < 3 ) {
    hlperr( "help library name required" );  
    return;
    }

/* Export command args */
  else {				
    cnf_exprt( argv[1], raw, raw_length );
    cnf_exprt( argv[2], mode, mode_length );
    cnf_exprt( argv[3], out,  out_length );
    }

  F77_CALL(hlpconv_top)(		/* Invoke the convertor */
       CHARACTER_ARG(raw),
       CHARACTER_ARG(mode),
       CHARACTER_ARG(out)
       TRAIL_ARG(raw)
       TRAIL_ARG(mode)
       TRAIL_ARG(out) );
  }
