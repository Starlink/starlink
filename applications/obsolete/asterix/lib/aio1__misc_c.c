/*+
 *  Name:
 *     aio1_rewrite_c.c
 *
 *  Purpose:
 *     Write a string with various kinds of carriage control
 *
 *  Language:
 *
 *     Starlink ANSI C
 *
 *  Description:
 *
 *
 *  Invokation:
 *
 *     CALL AIO1_REWRITE( MODE, STRING, STATUS )
 *
 *  Authors:
 *
 *     David J. Allan (JET-X, University of Birmingham)
 *
 *  History:
 *
 *     26-Apr-1994 (DJA):
 *        Original version.
 *- */


#include <stdlib.h>			/* malloc etc */
#include <string.h>			/* String manipulation */

#include "sae_par.h"			/* Starlink constants */
#include "f77.h"			/* C & Fortran interfacing */


F77_SUBROUTINE(aio1_rewrite)( CHARACTER(mode), CHARACTER(str),
                              INTEGER(status) TRAIL(mode) TRAIL(str) )
  {
  GENPTR_CHARACTER(mode)
  GENPTR_CHARACTER(str)
  GENPTR_INTEGER(status)

  int		i;

  if ( *status != SAI__OK )             /* Check status on entry */
    return;

  for( i=0; i<str_length; i++ )		/* Print each character of string */
    putchar( str[i] );

  if ( *mode == 'T' )			/* Normal line termination */
    printf( "\n" );

  else if ( *mode == 'B' ) 		/* Backspace to original point */
    for( i=0; i<str_length; i++ )
      putchar( 8 );
  }
