/* Subroutine:  psx_malloc( size, pntr, status )
*+
*  Name:
*     PSX_MALLOC

*  Purpose:
*     Allocate virtual memory

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_MALLOC( SIZE, PNTR, STATUS )

*  Description:
*     The routine allocates an amount of virtual memory specified by
*     SIZE. The memory returned is not initialized. The unit of SIZE is
*     the amount of storage required to store a single character. A
*     pointer to the allocated storage is returned in PNTR. This
*     pointer can be passed on to other subroutines using the %VAL
*     construct. If the storage cannot be allocated, then PNTR is set
*     to zero, STATUS is set to PSX__NOALL and an error is reported.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The amount of virtual memory to be allocated. If the number required
*        exceeds the maximum that can be stored in an INTEGER (about
*        2.1E9), them routine PSX_MALLOC8 should be used in place of
*        PSX_MALLOC. PSX_MALLOC8 has an identical interface except that
*        the SIZE argument is an INTEGER*8.
*     PNTR = POINTER (Returned)
*        A pointer to the allocated storage.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*        CALL PSX_MALLOC( 40, PNTR, STATUS )
*        CALL SUB1( %VAL(PNTR), 10, STATUS )
*           ...
*        SUBROUTINE SUB1( ARRAY, N, STATUS )
*        INTEGER N
*        INTEGER ARRAY( N )
*           ...
*
*        Allocates 40 bytes and uses this as a 10 element integer array.
*
*        The call to PSX_MALLOC allocates forty bytes of storage. The
*        pointer to this storage is then passed to subroutine SUB1,
*        where it is accessed as an array of integers.
*        We assume SUB1 returns without action if STATUS is bad.
*
*        Note that here the program needs to know that an INTEGER variable
*        is stored in four bytes. THIS IS NOT PORTABLE. In such a case
*        it is better to use PSX_CALLOC or to use the symbolic
*        constants NUM_NB<T> defined in the file PRM_PAR to determine
*        the number of bytes per unit of storage. (See SUN/39 for a
*        description of these constants).

*  Notes:
*     -  Storage allocated by PSX_MALLOC should be returned by a call to
*        PSX_FREE when it is no longer needed.
*     -  PNTR is declared to be of type POINTER. This is usually
*        represented in FORTRAN as an INTEGER, although any type that
*        uses the same amount of storage would be just as good.
*        The pointer will have been registered for C and FORTRAN use,
*        according to the scheme described in SUN/209, allowing its use
*        where pointers are longer than INTEGERs. For portability, the
*        construct %VAL(CNF_PVAL(PNTR)), rather than simply %VAL(PNTR),
*        should be used to pass the pointer to the subroutine. Function
*        CNF_PVAL is described in SUN/209 Section `Pointers'.
*     -  If several calls to PSX_MALLOC are made, the space returned by
*        each call is completely separate from that made by any other
*        call. In particular, the program should not assume that the
*        space returned by successive calls is contiguous.

*  External Routines Used:
*     cnf: cnfMalloc, cnfFptr

*  References:
*     -  POSIX standard (1988), section 8.1
*     -  ANSI C standard (1989), section 4.10.3.3

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     AJC: A.J. Chipperfield (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     5-MAR-1991 (PMA):
*        Original version.
*     8-APR-1991 (PMA):
*        Added call to ems_rep_c to report errors.
*     15-APR-1991 (PMA):
*        Changed calls to ems to calls to psx1.
*     3-MAY-1991 (PMA):
*        Ensure that a value is returned in PNTR, even on an error.
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*     14-APR-1993 (PMA):
*        Cast the temporary pointer to an F77 pointer.
*     14-APR-199 (RFWS):
*        Use CNF for memory allocation.
*     23-JUN-2000 (AJC):
*        Improve documentation re pointers
*        Tidy refs to CNF routines
*     6-DEC-2004 (TIMJ):
*        Report allocation failure as requesting unsigned int bytes
*     25-MAY-2011 (TIMJ):
*        Simplify error reporting.
*     26-SEP-2019 (DSB):
*        Add 8-byte interface (PSX_MALLOC8).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
-----------------------------------------------------------------------------
*/


/* Global Constants:		.					    */

#include <stdio.h>               /* for sprintf */
#include <stdlib.h>		 /* Standard C library			    */
#include <stdint.h>              /* int64_t definition                      */
#include "f77.h"		 /* C - Fortran interface		    */
#include "psx_err.h"		 /* PSX error values			    */
#include "psx1.h"		 /* Internal PSX routines		    */
#include "sae_par.h"		 /* ADAM constants			    */

/* Prototypes */
F77_SUBROUTINE(psx_malloc8)( INTEGER8(size), POINTER(pntr), INTEGER(status) );


/* 4-byte interface - just calls the 8-byte interface */
F77_SUBROUTINE(psx_malloc)( INTEGER(size), POINTER(pntr), INTEGER(status) ) {
   GENPTR_INTEGER(size)
   GENPTR_POINTER(pntr)
   GENPTR_INTEGER(status)
   DECLARE_INTEGER8(size8);
   size8 = *size;
   F77_CALL(psx_malloc8)( INTEGER8_ARG(&size8), POINTER_ARG(pntr), INTEGER_ARG(status) );
}

/* 8-byte interface */
F77_SUBROUTINE(psx_malloc8)( INTEGER8(size), POINTER(pntr), INTEGER(status) )
{

/* Pointers to Arguments:						    */

   GENPTR_INTEGER8(size)
   GENPTR_POINTER(pntr)
   GENPTR_INTEGER(status)

/* Local Variables:							    */

   void *temp;			 /* Temporary return value from malloc.	    */

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Allocate the space (use CNF so that the resulting pointer can be used    */
/* from both C and Fortran).						    */

   temp = cnfMalloc( (size_t)( *size ) );

/* Check that the space was allocated.					    */

   if( temp != 0 )

/* Copy the pointer to the allocated storage space to the subroutine	    */
/* argument, converting to a Fortran pointer.				    */

   {
      *pntr = cnfFptr( temp );
   }
   else

/* Set STATUS to an error code and report the error.			    */

   {
      *pntr = (F77_POINTER_TYPE)0;
      *status = PSX__NOALL;
      psx1_rep_c( "PSX_MALLOC_NOALL",
                  "Failed to allocate space with malloc. %lu bytes requested",
                  status, (unsigned long)(*size) );
   }

}
