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
*        Properly import TSTRCT
*        Tidy refs to CNF routines
*      8-JAN-2002 (AJC):
*        #include <string.h>
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*-----------------------------------------------------------------------------
*/

/* Global Constants:		.					    */

#include <string.h>              /* C string library                        */
#include <time.h>		 /* C time library			    */
#include "f77.h"		 /* C - Fortran interface		    */
#include "sae_par.h"		 /* ADAM constants			    */


F77_SUBROUTINE(psx_asctime)( POINTER(tstrct), CHARACTER(string),
                             INTEGER(status) TRAIL(string) )
{

/* Pointers to Arguments:						    */

   GENPTR_POINTER(tstrct)
   GENPTR_CHARACTER(string)
   GENPTR_INTEGER(status)

/* Local Variables:							    */

   int i;			 /* Loop counter			    */
   char time_s[26];		 /* The string returned by asctime	    */
   struct tm *tstrctc;           /* C version of pointer                    */

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Import the time structure                                                */

   tstrctc = cnfCptr( *tstrct );

/* Convert the time structure and copy it to time_s.                        */

   strcpy( time_s, asctime( tstrctc ) );

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
