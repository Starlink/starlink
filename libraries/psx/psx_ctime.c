/* Subroutine:  psx_ctime( nticks, string, status )
*+
*  Name:
*     PSX_CTIME

*  Purpose:
*     Convert the calendar time to a character string

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_CTIME( NTICKS, STRING, STATUS )

*  Description:
*     Convert the number of ticks since the beginning of the calendar
*     (the value returned by PSX_TIME) to a character string.

*  Arguments:
*     NTICKS = INTEGER (Given)
*        The number of ticks since the start of the calendar.
*     STRING = CHARACTER * ( * ) (Returned)
*        The character string representation of the time.
*     STATUS = INTEGER (Given)
*        The global status.

*  Examples:
*        CALL PSX_TIME( NTICKS, STATUS )
*        CALL PSX_CTIME( NTICKS, STRING, STATUS )
*        PRINT *,'The time is ',STRING
*
*        Prints the current time as something like:
*        "Wed Apr 17 09:01:04 1991" (without the quotes).

*  Notes:
*     -  The C string returned by the POSIX function ctime contains a
*        new line character. This is removed before being passed back
*        to the FORTRAN routine.
*     -  The actual argument corresponding to STRING should be at least
*        24 characters long.

*  External Routines Used:
*     cnf: cnfExprt

*  References:
*     -  POSIX standard (1988), section 8.1
*     -  ANSI C standard (1989), section 4.12.3.2

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
*     PWD: Peter W. Draper (Starlink, Durham University)
*     {enter_new_authors_here}

*  History:
*     17-APR-1991 (PMA):
*        Original version.
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*     23-JUN-2000 (AJC):
*        Tidy refs to CNF routines
*     22-SEP-2004 (TIMJ):
*        Use ctime_r if available.
*        Remove the cast from nticks to time_t and replace with an internal
*        variable since the cast does not work reliably if sizeof(Fortran int)
*        != sizeof(time_t).
*     09-MAR-2005 (PWD):
*        Include unistd.h to get POSIX environment.
*        Test for HAVE_CTIME_R_THREE_ARGS to work around Solaris ctime_r
*        having three arguments by default.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*-----------------------------------------------------------------------------
*/

#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* Global Constants:		.					    */
#if HAVE_UNISTD_H
#  include <unistd.h>            /* Make sure we get POSIX versions         */
#endif

#if STDC_HEADERS
#  include <string.h>
#  include <time.h>		 /* C time library			    */
/* if we start to use sys/time.h, see autoconf AC_HEADER_TIME */
#endif


#include "f77.h"		 /* C - Fortran interface		    */
#include "sae_par.h"		 /* ADAM constants			    */

/* Number of characters mandated by ctime for buffer space */
#define SZ_CTIME 26

F77_SUBROUTINE(psx_ctime)( INTEGER(nticks), CHARACTER(string),
                           INTEGER(status) TRAIL(string) )
{

/* Pointers to Arguments:						    */

   GENPTR_INTEGER(nticks)
   GENPTR_CHARACTER(string)
   GENPTR_INTEGER(status)

/* Local Variables:							    */

   time_t timep;                 /* Local version of nticks */
   int i;			 /* Loop counter			    */
   char time_s[SZ_CTIME+1];	 /* The string returned by asctime	    */
#if HAVE_CTIME && !HAVE_CTIME_R
   char * temps;                 /* Pointer to static string given by ctime */
#endif

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Convert NTICKS and copy it to time_s.				    */
/* ctime requires the address of nticks, not its value.			    */

   timep = (time_t) *nticks;
#if HAVE_CTIME_R
# if HAVE_CTIME_R_THREE_ARGS
   ctime_r( &timep, time_s, SZ_CTIME+1 );
# else
   ctime_r( &timep, time_s );
# endif
#else
#  if HAVE_CTIME
   temps = ctime( &timep );
   if (temps) {
     strncpy( time_s, temps, (size_t)SZ_CTIME );
   } else {
     strcpy( time_s, "<undefined>" );
   }
#  else
#    error do not know how to convert time_t to string
#  endif
#endif

/* Remove the newline character at the end of the string.		    */

   for( i = strlen( time_s ) - 1 ; i >= 0 ; i-- )
   {
      if( time_s[i] == '\n' )
      {
         time_s[i] = '\0';
         break;
      }
   }

/* Export the character string.						    */

   cnfExprt( time_s, string, string_length );

}
