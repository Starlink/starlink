/*+
 * Name:
 *    spaek

 * Purpose:
 *    Display a Fortran string.

 * Language:
 *    Starlink ANSI C

 * Type of Module:
 *    Fortran-callable C function.

 * Invocation from Fortran:
 *    CALL SPAEK( STRING, STRING_L, STATUS )

 * Description:
 *    This routine simply prints the given string on stdout with a
 *    newline character.

 * Arguments:
 *    string = CHARACTER() (Given)
 *       The Fortran string to be printed.
 *    string_l = INTEGER() (Given)
 *       The declared length of string as a Fortran string.
 *    status = INTEGER() (Given and Returned)
 *       The global status.

 * Returned value:
 *    None.

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    01 Jul 1993 (hme):
 *       Original version.
 *-
 */

#include <stdio.h>
#include "cnf.h"
#include "f77.h"

#define SAI__OK    0

/*:
 */

F77_SUBROUTINE(spaek)( CHARACTER(string), INTEGER(string_l),
   INTEGER(status) )
{
   GENPTR_CHARACTER(string)
   GENPTR_INTEGER(string_l)
   GENPTR_INTEGER(status)

   char      *cstring;           /* C version of Fortran strings */

/*.
 */

/* Check status on entry.
 */
   if ( *status != SAI__OK ) return;

/* Convert given prompt to a C string and print it on standard output.
 * Then print a newline. Do this with a format string, we need one
 * anyway, since string might contain % and other nasty things.
 */
   cstring = cnf_creim( string, *string_l );
   (void) printf( "%s\n", cstring );
   (void) cnf_free( cstring );

/* Return.
 */
   return;
}
