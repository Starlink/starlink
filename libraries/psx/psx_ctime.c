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

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     17-APR-1991 (PMA):
*        Original version.
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*     23-JUN-2000 (AJC):
*        Tidy refs to CNF routines
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*-----------------------------------------------------------------------------
*/

/* Global Constants:		.					    */

#include <time.h>		 /* C time library			    */
#include "f77.h"		 /* C - Fortran interface		    */
#include "sae_par.h"		 /* ADAM constants			    */


F77_SUBROUTINE(psx_ctime)( INTEGER(nticks), CHARACTER(string),
                           INTEGER(status) TRAIL(string) )
{

/* Pointers to Arguments:						    */

   GENPTR_INTEGER(nticks)
   GENPTR_CHARACTER(string)
   GENPTR_INTEGER(status)

/* Local Variables:							    */

   int i;			 /* Loop counter			    */
   char time_s[26];		 /* The string returned by asctime	    */

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Convert NTICKS and copy it to time_s.				    */
/* ctime requires the address of nticks, not its value.			    */

   strcpy( time_s, ctime( (const time_t *)nticks ) );

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
