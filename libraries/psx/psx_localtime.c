
/* Subroutine:  psx_localtime( nticks, secs, mins, hours, day, month,
                               year, wday, yday, isdst, tstrct, status )
*+
*  Name:
*     PSX_LOCALTIME

*  Purpose:
*     Convert a value returned by PSX_TIME to individual local time values

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_LOCALTIME( NTICKS, SECS, MINS, HOURS, DAY, MONTH, YEAR,
*    :   WDAY, YDAY, ISDST, TSTRCT, STATUS )

*  Description:
*     Convert a value returned by PSX_TIME into a set of usable numbers
*     expressed in local time, and a pointer to the corresponding C structure.

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
*     ISDST = INTEGER (Returned)
*        Daylight savings time flag.
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
*     -  The value of ISDST is 1 when daylight saving time is in effect,
*        0 when it is not and -1 when the information is not available.
*     -  The pointer TSTRCT points to the C structure that contains the
*        information about the time. This pointer is needed as it may be
*        passed on to PSX_ASCTIME.
*        The structure will be overwritten by any future call to
*        PSX_LOCALTIME or PSX_GMTIME.
*     -  TSTRCT is declared to be of type POINTER. This is usually
*        represented in FORTRAN as an INTEGER, although any type that
*        uses the same amount of storage would be just as good.

*  External Routines Used:
*        cnf: cnFpptr, cnfMalloc

*  References:
*     -  POSIX standard (1988), section 8.1
*     -  ANSI C standard (1989), section 4.12.3.4

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

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
*     PMA: Peter Allan (Starlink, RAL)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-APR-1991 (PMA):
*        Original version.
*     10-JUN-1991 (PMA):
*        Removed the addition of 1 to MONTH and YDAY.
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*     14-APR-1993 (PMA):
*        Cast the argument of localtime to the correct type.
*        Cast the temporary pointer to an F77 pointer.
*     21-JUN-2000 (AJC)::
*        Properly register and export TSTRCT (see CNF, SUN/209)
*     22-SEP-2004 (TIMJ):
*        Remove the cast from nticks to time_t and replace with an internal
*        variable since the cast
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

F77_SUBROUTINE(psx_localtime)( INTEGER(nticks),
                               INTEGER(secs), INTEGER(mins), INTEGER(hours),
                               INTEGER(day), INTEGER(month), INTEGER(year),
                               INTEGER(wday), INTEGER(yday), INTEGER(isdst),
                               POINTER(tstrct), INTEGER(status) )
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
   GENPTR_INTEGER(isdst)
   GENPTR_POINTER(tstrct)
   GENPTR_INTEGER(status)

/* External Variables:                                                      */

   extern struct tm *psxtmstr;               /* Pointer to tm struct        */
                                             /* defined in psx_gmtime       */
/* Local Variables:							    */

   time_t timep;                 /* Local version of nticks */
   struct tm *timeptr;		 /* Pointer to a tm structure		    */

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Convert the value in NTICKS to a structure contining real times.	    */
/* It is the address of NTICKS that is passed, not its value.		    */
   timep = (time_t) *nticks;
   timeptr = localtime( &timep );

/* Extract the values from the structure.				    */

   *secs  = timeptr->tm_sec;
   *mins  = timeptr->tm_min;
   *hours = timeptr->tm_hour;
   *day   = timeptr->tm_mday;
   *month = timeptr->tm_mon;
   *year  = timeptr->tm_year;
   *wday  = timeptr->tm_wday;
   *yday  = timeptr->tm_yday;
   *isdst = timeptr->tm_isdst;

/* We allocate space to copy the tm structure so that it can be safely      */
/* registered for C and Fortran use (see CNF SUN/209). A pointer to a       */
/* statically allocated area may (just possibly) clash with another pointer */
/* The same space will be used by PSX_GMTIME so the pointer to it is        */
/* external and also serves as an 'allocated' flag.                         */

/* Get memory if required                                                   */

   if ( psxtmstr == NULL ) {

      psxtmstr = (struct tm *)cnfMalloc( sizeof(struct tm) );

   }

/* If OK copy the structure to the PSX space and export the pointer         */

   if ( psxtmstr != NULL ) {
      *psxtmstr = *timeptr;
      *tstrct = cnfFptr( psxtmstr );

/* else report no memeory allocated                                         */

   } else {
      *status = PSX__NOALL;
      psx1_rep_c( "PSX_TIMSTR_NOALL",
        "Failed to allocate space for time structure", status );
   }

}
