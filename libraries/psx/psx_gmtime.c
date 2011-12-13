/* Subroutine:  psx_gmtime( nticks, secs, mins, hours, day, month,
                               year, wday, yday, tstrct, status )
*+
*  Name:
*     PSX_GMTIME

*  Purpose:
*     Convert the value returned by PSX_TIME to individual GMT values

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_GMTIME( NTICKS, SECS, MINS, HOURS, DAY, MONTH, YEAR,
*    :   WDAY, YDAY, TSTRCT, STATUS )

*  Description:
*     Convert the value returned by PSX_TIME into a set of usable numbers
*     expressed in GMT, and a pointer to the corresponding C structure.
*     If GMT is not available, STATUS will be set to PSX__NOGMT and an error
*     is reported.

*  Arguments:
*     NTICKS = INTEGER (Given)
*        The number of ticks since the start of the calendar.
*     SECS = INTEGER (Returned)
*        The number of seconds in the given time.
*     MINS = INTEGER (Returned)
*        The number of minutes in the given time.
*     HOURS = INTEGER (Returned)
*        The number of hours in the given time.
*     DAY = INTEGER (Returned)
*        The number of the day of the month.
*     MONTH = INTEGER (Returned)
*        The number of the month in the year (counting from zero).
*     YEAR = INTEGER (Returned)
*        The number of the year (last two digits).
*     WDAY = INTEGER (Returned)
*        The number of the day in the week.
*     YDAY = INTEGER (Returned)
*        The number of the day in the year (counting from zero).
*     TSTRCT = POINTER (Returned)
*        A pointer to the C time structure.
*     STATUS = INTEGER (Given)
*        The global status.

*  Notes:
*     -  The value of MONTH is 0 for January, 1 for February, etc. This
*        is to maintain compatibility with the C run time library.
*     -  The value of YDAY is 0 for the first of January, 1 for the
*        second of January, etc. This is to maintain compatibility with
*        the C run time library.
*     -  The value of WDAY is 0 for Sunday, 1 for Monday, etc.
*     -  The pointer TSTRCT points to the C structure that contains the
*        information about the time. This pointer is needed as it may be
*        passed on to PSX_ASCTIME.
*        The structure will be overwritten by any future call to
*        PSX_GMTIME or PSX_LOCALTIME.
*     -  TSTRCT is declared to be of type POINTER. This is usually
*        represented in FORTRAN as an INTEGER, although any type that
*        uses the same amount of storage would be just as good.

*  External Routines Used:
*        cnfFptr, cnfMalloc

*  References:
*     -  POSIX standard (1988), section 8.1
*     -  ANSI C standard (1989), section 4.12.3.3

*  Copyright:
*     Copyright (C) 2000 Council for the Central Laboratory of the Research
*     Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     AJC: Alan Chipperfield (Starlink, RAL)
*     AA: Alasdair Allan (Starlink, University of Exeter)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     21-JUN-2000 (AJC):
*        Original version.
*     30-JUN-2004 (AA):
*        Defined *psxtmstr as NULL, otherwise we'll get an undefined
*        symbol when linking using the new autoconf system.
*     22-SEP-2004 (TIMJ):
*        Remove the cast from nticks to time_t and replace with an internal
*        variable since the cast does not work reliably if sizeof(Fortran int)
*        != sizeof(time_t).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
------------------------------------------------------------------------------
*/

#include <config.h>

/* Global Constants:		.					    */

#if STDC_HEADERS
#  include <time.h>		 /* C time library			    */
#endif
#include "f77.h"		 /* C - Fortran interface		    */
#include "sae_par.h"		 /* ADAM constants			    */
#include "psx_err.h"             /* PSX errors                              */
#include "psx1.h"                /* declares psx1_rep_c */

struct tm *psxtmstr = NULL;     /* Pointer to storage for tm struct        */

F77_SUBROUTINE(psx_gmtime)( INTEGER(nticks),
                            INTEGER(secs), INTEGER(mins), INTEGER(hours),
                            INTEGER(day), INTEGER(month), INTEGER(year),
                            INTEGER(wday), INTEGER(yday), POINTER(tstrct),
                            INTEGER(status) )
{

/* Pointers to Arguments:						    */

   GENPTR_INTEGER(nticks)
   GENPTR_INTEGER(secs)
   GENPTR_INTEGER(mins)
   GENPTR_INTEGER(hours)
   GENPTR_INTEGER(day)
   GENPTR_INTEGER(month)
   GENPTR_INTEGER(year)
   GENPTR_INTEGER(wday)
   GENPTR_INTEGER(yday)
   GENPTR_POINTER(tstrct)
   GENPTR_INTEGER(status)

/* Local Variables:							    */

   time_t timep;                 /* Local version of nticks */
   struct tm *timeptr;		 /* Pointer to a tm structure		    */

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Convert the value in NTICKS to a structure contining real times.	    */
/* It is the address of NTICKS that is passed, not its value.		    */

   timep = (time_t) *nticks;
   timeptr = gmtime( &timep );

   if ( timeptr != NULL ) {
/* Extract the values from the structure.				    */

      *secs  = timeptr->tm_sec;
      *mins  = timeptr->tm_min;
      *hours = timeptr->tm_hour;
      *day   = timeptr->tm_mday;
      *month = timeptr->tm_mon;
      *year  = timeptr->tm_year;
      *wday  = timeptr->tm_wday;
      *yday  = timeptr->tm_yday;

/* We allocate space to copy the tm structure so that it can be safely      */
/* registered for C and Fortran use (see CNF SUN/209). A pointer to a       */
/* statically allocated area may (just possibly) clash with another pointer */
/* The same space will be used by PSX_LOCALTIME so the pointer to it is     */
/* external and serves as an 'allocated' flag.                              */

/* Get memory if required                                                   */

      if ( psxtmstr == NULL ) {

         psxtmstr = (struct tm *)cnfMalloc( sizeof(struct tm) );

      }

/* If OK copy the structure to the PSX space and export the pointer         */

      if ( psxtmstr != NULL ) {
         *psxtmstr = *timeptr;
         *tstrct = cnfFptr( psxtmstr );

/* else report no memory allocated                                          */

      } else {
         *status = PSX__NOALL;
         psx1_rep_c( "PSX_TIMSTR_NOALL",
           "Failed to allocate space for time structure", status );
      }

/* else report no GMT available                                             */

   } else {

      *status = PSX__NOGMT;
      psx1_rep_c( "PSX_GMT_NOTAV",
        "GMT is not available on this machine", status );

   }

}
