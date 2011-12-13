/* Subroutine:  psx_free( pntr, status )
*+
*  Name:
*     PSX_FREE

*  Purpose:
*     Free virtual memory

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_FREE( PNTR, STATUS )

*  Description:
*     The routine frees the virtual memory pointed to by PNTR that was
*     previously allocated by a call to PSX_CALLOC or PSX_MALLOC.
*     It will try to free the memory regardless of the given value of
*     STATUS.

*  Arguments:
*     PNTR = POINTER (Given and Returned)
*        A pointer to the allocated storage.
*     STATUS = INTEGER (Given)
*        The global status.

*  Notes:
*     -  PNTR is declared to be of type POINTER. This is usually
*        represented in FORTRAN as an integer, although any type that
*        uses the same amount of storage would be just as good.

*  External Routines Used:
*     cnf: cnfCptr, cnfFree

*  References:
*     -  POSIX standard (1988), section 8.1
*     -  ANSI C standard (1989), section 4.10.3.2

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
*     RFWS: R.F. Warren-Smith (Starlink, RAL)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-APR-1991 (PMA):
*        Original version.
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*     30-JAN-1992 (PMA):
*        Fixed bug - free(pntr) should have been free(*pntr).
*     14-APR-199 (RFWS):
*        Use CNF for memory de-allocation. Execute even if STATUS is set.
*     23-JUN-2000 (AJC):
*        Tidy refs to CNF routines
*     12-MAY-2010 (TIMJ):
*        Force pointer to 0 after freeing it. This makes it easier to spot
*        pointers that have been freed.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
-----------------------------------------------------------------------------
*/


/* Global Constants:		.					    */

#include <stdlib.h>		 /* Standard C library			    */
#include "f77.h"		 /* C - Fortran interface		    */
#include "sae_par.h"		 /* ADAM constants			    */


F77_SUBROUTINE(psx_free)( POINTER(pntr), INTEGER(status) )
{

/* Pointers to Arguments:						    */

   GENPTR_POINTER(pntr)
   GENPTR_INTEGER(status)

/* Removed by RFWS - try to free the memory regardless of STATUS value.     */
#if 0
/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;
#endif

/* Free the space (use CNF, since the pointer will have been registered for */
/* use from both C and Fortran).					    */

   cnfFree( cnfCptr( *pntr ) );
   *pntr = (F77_POINTER_TYPE)0;
}
