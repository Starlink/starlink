/*+
 * Name:
 *    spaef

 * Purpose:
 *    Find out terminal length and width.

 * Language:
 *    Starlink ANSI C

 * Type of Module:
 *    Fortran-callable C function.

 * Invocation from Fortran:
 *    CALL SPAEF( PAGE, WIDTH, STATUS )

 * Description:
 *    This routine uses ioctl to find out the size of the terminal in
 *    use. It is desiged to be called from Fortran.

 * Arguments:
 *    page = INTEGER() (Returned)
 *       The number of lines on the terminal.
 *    width = INTEGER() (Returned)
 *       The number of columns on the terminal.
 *    status = INTEGER() (Given and Returned)
 *       The global status.

 * Returned value:
 *    None.

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    30 Jun 1993 (hme):
 *       Original version. Adapted from kpg1_trmsz.
 *    04 Aug 1993 (hme):
 *       Copied from Specdre's spaef.
 *    13 Aug 1993 (hme):
 *       Review with look at new subpar_trmsz. No curses.
 *    16 Aug 1993 (hme):
 *       Back from Figaro to Specdre.
 *-
 */

#include <sys/termio.h>
#include "f77.h"

#define SAI__ERROR 148013867
#define SAI__OK    0

/*:
 */

F77_SUBROUTINE(spaef)( INTEGER(page), INTEGER(width), INTEGER(status) )
{
   GENPTR_INTEGER(page)
   GENPTR_INTEGER(width)
   GENPTR_INTEGER(status)
#ifdef TIOCGWINSZ
   struct winsize s;
#endif

/*.
 */

/* Check status on entry.
 */
   if ( *status != SAI__OK ) return;

/* Do the job.
 */
   if ( ioctl( 1, TIOCGWINSZ, &s ) )
   {  *status = SAI__ERROR;
   }
   else
   {  *width = s.ws_col; *page = s.ws_row;
   }

/* Return.
 */
   return;
}
