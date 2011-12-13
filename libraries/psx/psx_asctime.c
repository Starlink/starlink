/* Subroutine:  psx_asctime( tstrct, string, status )
*+
*  Name:
*     PSX_ASCTIME

*  Purpose:
*     Convert a time structure to a character string

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_ASCTIME( TSTRCT, STRING, STATUS )

*  Description:
*     Convert the information in the structure pointed to by TSTRCT to a
*     character string. TSRCT should have been set by a call to PSX_LOCALTIME
*     or PSX_GMTIME.

*  Arguments:
*     TSTRCT = POINTER (Given)
*        The pointer to the time structure.
*     STRING = CHARACTER * ( * ) (Returned)
*        The character string representation of the time.
*     STATUS = INTEGER (Given)
*        The global status.

*  Examples:
*        CALL PSX_TIME( NTICKS, STATUS )
*        CALL PSX_LOCALTIME( NTICKS, SEC, MINS, HOUR, DAY, MONTH, YEAR,
*       :   WDAY, YDAY, ISDST, TSTRCT, STATUS )
*        CALL PSX_ASCTIME( TSTRCT, STRING, STATUS )
*        PRINT *,'The time is ',STRING
*
*        Prints the current local time as something like:
*        "Wed Apr 17 09:01:04 1991" (without the quotes).

*  Notes:
*     -  TSTRCT is declared to be of type POINTER. This is usually
*        represented in FORTRAN as an INTEGER, although any type that
*        uses the same amount of storage would be just as good.
*     -  The C string returned by the POSIX function localtime contains
*        a new line character. This is removed before being passed back
*        to the calling FORTRAN routine.
*     -  The actual argument corresponding to STRING should be at least
*        24 characters long.

*  External Routines Used:
*     cnf: cnfCptr, cnfExprt

*  References:
*     -  POSIX standard (1988), section 8.1
*     -  ANSI C standard (1989), section 4.12.3.1

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
*        Properly import TSTRCT
*        Tidy refs to CNF routines
*     08-JAN-2002 (AJC):
*        #include <string.h>
*     22-SEP-2004 (TIMJ):
*        Use asctime_r if present
*     09-MAR-2005 (PWD):
*        Include unistd.h to get POSIX environment.
*        Test for HAVE_ASCTIME_R_THREE_ARGS to work around Solaris asctime_r
*        having three arguments by default.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*-----------------------------------------------------------------------------
*/

#if HAVE_CONFIG_H
# include <config.h>
#endif

/* Global Constants:		.					    */
#if HAVE_UNISTD_H
#  include <unistd.h>            /* Make sure we get POSIX versions         */
#endif

#include <string.h>              /* C string library                        */
#include <time.h>		 /* C time library			    */
#include "f77.h"		 /* C - Fortran interface		    */
#include "sae_par.h"		 /* ADAM constants			    */

/* Number of characters mandated by asctime for buffer space */
#define SZ_ASCTIME 26


F77_SUBROUTINE(psx_asctime)( POINTER(tstrct), CHARACTER(string),
                             INTEGER(status) TRAIL(string) )
{

/* Pointers to Arguments:						    */

   GENPTR_POINTER(tstrct)
   GENPTR_CHARACTER(string)
   GENPTR_INTEGER(status)

/* Local Variables:							    */

   int i;			 /* Loop counter			    */
   char time_s[SZ_ASCTIME+1];	 /* The string returned by asctime	    */
   struct tm *tstrctc;           /* C version of pointer                    */
#if HAVE_ASCTIME && !HAVE_ASCTIME_R
   char * temps;                 /* Pointer to string returned by asctime */
#endif

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Import the time structure                                                */

   tstrctc = cnfCptr( *tstrct );

/* Convert the time structure and copy it to time_s.                        */

#if HAVE_ASCTIME_R
# if HAVE_ASCTIME_R_THREE_ARGS
   asctime_r( tstrctc, time_s, SZ_ASCTIME+1 );
# else
   asctime_r( tstrctc, time_s );
# endif
#else
# if HAVE_ASCTIME
   temps = asctime( tstrctc );
   if (temps) {
     strncpy( time_s, temps, (size_t)SZ_ASCTIME );
   } else {
     strcpy( time_s, "<undefined>" );
   }
# else
#  error Unable to locate asctime function
# endif
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
