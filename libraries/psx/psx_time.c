/* Subroutine: psx_time( nticks, status )
*+
*  Name:
*     PSX_TIME

*  Purpose:
*     Get the current calendar time

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_TIME( NTICKS, STATUS )

*  Description:
*     Determine the current calendar time. The encoding of the value is
*     unspecified, but is the number of ticks since some date in the
*     past. If it is not possible to get the value of NTICKS, STATUS is
*     set to PSX__NOTIM and an error is reported.

*  Arguments:
*     NTICKS = INTEGER (Returned)
*        The current time.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine is not directly useful in itself, but the value
*        returned in NTICKS can be passed to other routines that process
*        it further.

*  References:
*     -  POSIX standard (1988), section 4.5.1
*     -  ANSI C standard (1989), section 4.12.2.4

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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     15-APR-1991 (PMA):
*        Original version.
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*     22-SEP-2004 (TIMJ):
*        Add check for 32bit integer overflow in 2038
*     10-MAR-2005 (TIMJ):
*        Return immediately with nticks=-1 if status is bad on entry
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
------------------------------------------------------------------------------
*/

/* Global Constants:							    */

#include <time.h>		 /* Time constants and structures	    */
#include <limits.h>              /* int limit */
#include "cnf.h"		 /* C - FORTRAN interface		    */
#include "psx_err.h"		 /* PSX error codes			    */
#include "psx1.h"		 /* Internal PSX routines		    */
#include "sae_par.h"		 /* ADAM constants			    */

#if !defined(NULL)		 /* Do this rather than including stddef.h  */
#define NULL  (void *) 0	 /* since it causes problems with the gcc   */
#endif				 /* compiler.				    */


F77_SUBROUTINE(psx_time)( INTEGER(nticks), INTEGER(status) )
{

/* Pointers to Arguments:						    */

   GENPTR_INTEGER(nticks)
   GENPTR_INTEGER(status)

/* Local Variables:							    */
   time_t t;         /* Local version of the time */

/* Initialise nticks to -1 for bad status                                   */
   *nticks = -1;
   if (*status != SAI__OK) return;

   t = time( NULL );

/* Check the value returned.						    */

   if( t == -1 )
   {
      *status = PSX__NOTIM;
      psx1_rep_c( "PSX_TIME_NOTIM", "Could not get the time", status );
   }

   /* Copy to the fortran integer. Make sure it has not exceeded
      the space since in some cases time_t can be 64bit */
   if ( t > INT_MAX ) {
     *status = PSX__NOTIM;
     psx1_rep_c( "PSX_TIME_INTEXCEEDED",
		 "Time exceeds largest value that can be stored in a Fortran integer",
		 status );
   }

   if (*status == SAI__OK ) {
     *nticks = (F77_INTEGER_TYPE)t;
   }

}
