/*+
 * Name:
 *    spaej

 * Purpose:
 *    At end of page prompt for confirmation.

 * Language:
 *    Starlink ANSI C

 * Type of Module:
 *    Fortran-callable C function.

 * Invocation:
 *    CALL SPAEJ( STATUS )

 * Description:
 *    This routine displays a blank line followed by a line with the
 *    string "Press Return to continue ...". It then waits for user
 *    input. Any response except newline or end-of-file is ignored. As
 *    soon as one of these characters has been supplied this routine
 *    prints another newline and returns.

 * Arguments:
 *    status = INTEGER() (Given and Returned)
 *       The global status. If this is bad on entry this routine returns
 *       without action. It is set bad if and only if the user responded
 *       with an end-of-file rather than a newline.

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
#include "f77.h"

#define SAI__ERROR 148013867
#define SAI__OK    0

/*:
 */

F77_SUBROUTINE(spaej)( INTEGER(status) )
{
   GENPTR_INTEGER(status)

   char c;

/*.
 */

/* Check status on entry.
 */
   if ( *status != SAI__OK ) return;

/* Put a blank line and the prompt string. Then get input until a
 * newline or EOF is given. Then print another newline.
 */
   (void) printf( "\nPress Return to continue ..." );
   for( c = '\0'; ( c != '\n' ) && ( c != EOF ); c = fgetc( stdin ) );
   (void) printf( "\n" );

/* If the response was an EOF, set status.
 */
   if ( c == EOF ) *status = SAI__ERROR;

/* Return.
 */
   return;
}
