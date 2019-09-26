/* Subroutine:  psx_realloc( size, pntr, status )
*+
*  Name:
*     PSX_REALLOC

*  Purpose:
*     Change the size of an allocated region of virtual memory

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_REALLOC( SIZE, PNTR, STATUS )

*  Description:
*     The routine changes the size of the region of virtual memory
*     pointed to by PNTR. The new size may be larger or smaller than
*     the old size. The contents of the object pointed to by PNTR
*     shall be unchanged up to the lesser of the old and new sizes.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The new amount of virtual memory required. If the number required
*        exceeds the maximum that can be stored in an INTEGER (about
*        2.1E9), them routine PSX_REALLOC8 should be used in place of
*        PSX_REALLOC. PSX_REALLOC8 has an identical interface except that
*        the SIZE argument is an INTEGER*8.
*     PNTR = POINTER (Given and Returned)
*        A pointer to the allocated storage
*     STATUS = INTEGER (Given)
*        The global status

*  Examples:
*        CALL PSX_MALLOC( 20, PNTR, STATUS )
*           ...
*        CALL PSX_REALLOC( 40, PNTR, STATUS )
*        CALL SUB1( %VAL(PNTR), 10, STATUS )
*           ...
*        SUBROUTINE SUB1( ARRAY, N, STATUS )
*        INTEGER N
*        INTEGER ARRAY( N )
*          ...
*
*        Allocates 20 bytes of storage, then extends it to 40 bytes.
*
*        The call to PSX_MALLOC allocates twenty bytes of storage. The
*        subsequent call to PSX_REALLOC extends this area to forty
*        bytes.  The pointer to this storage is then passed to
*        subroutine SUB1, where it is accessed as an array of integers.
*        We assume SUB1 returns without action if STATUS is bad.
*
*        Note that in this case the program needs to know that an
*        INTEGER variable is stored in four bytes. THIS IS NOT
*        PORTABLE. In such a case it is better to use the symbolic
*        constants NUM_NB<T> defined in the file PRM_PAR to determine
*        the number of bytes per unit of storage. (See SUN/39 for a
*        description of these constants).

*  Notes:
*     -  Storage allocated by PSX_REALLOC should be returned by a call
*        to PSX_FREE when it is no longer needed.
*     -  PNTR is declared to be of type POINTER. This is usually
*        represented in FORTRAN as an INTEGER, although any type that
*        uses the same amount of storage would be just as good.
*        The pointer will have been registered for C and FORTRAN use,
*        according to the scheme described in SUN/209, allowing its use
*        where pointers are longer than INTEGERs. For portability, the
*        construct %VAL(CNF_PVAL(PNTR)), rather than simply %VAL(PNTR),
*        should be used to pass the pointer to the subroutine. Function
*        CNF_PVAL is described in SUN/209 Section `Pointers'.
*     -  If SIZE is zero, then the space pointed to by PNTR is freed.
*     -  If the space that PNTR pointed to has been deallocated by a
*        call to PSX_FREE (or to PSX_REALLOC with SIZE = 0), then it is
*        undefined whether the pointer can subsequently be used by
*        PSX_REALLOC.  Consequently this should not be attempted, even
*        though it will work on some machines.
*     -  The value of PNTR may be changed by this routine as it may be
*        necessary to allocate a new region of memory and copy the
*        contents of the old region into the new one.

*  External Routines Used:
*     cnf: cnfCptr, cnfFptr, cnfMalloc, cnfRegp, cnfUregp

*  References:
*     -  POSIX standard (1988), section 8.1
*     -  ANSI C standard (1989), section 4.10.3.4

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
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*     30-JAN-1992 (PMA):
*        Fix bug on incorrect use of pntr and add error reporting.
*     14-APR-1993 (PMA):
*        Cast the temporary pointer to an F77 pointer.
*     14-APR-199 (RFWS):
*        Use CNF for memory allocation.
*     23-JUN-2000 (AJC):
*        Improve documentation re pointers
*        Tidy refs to CNF routines
*     6-DEC-2004 (TIMJ):
*        Report allocation failure as unsigned int
*     23-FEB-2006 (TIMJ):
*        Fix sprintf warning by casting size_t to unsigned long.
*        Use cnfRealloc (which is most of the old psx_realloc)
*     25-MAY-2011 (TIMJ):
*        Simplify error reporting.
*     26-SEP-2019 (DSB):
*        Add 8-byte interface (PSX_REALLOC8).
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
-----------------------------------------------------------------------------
*/

#include <config.h>

/* Global Constants:		.					    */

#include <stdio.h>
#include <stdlib.h>		 /* Standard C library			    */
#if STDC_HEADERS
#  include <string.h>
#  include <stdint.h>            /* int64_t definition                      */
#endif
#include "f77.h"		 /* C - Fortran interface		    */
#include "psx_err.h"             /* PSX error values                        */
#include "sae_par.h"		 /* ADAM constants			    */
#include "psx1.h"                /* declares psx1_rep_c */


/* Prototypes */
F77_SUBROUTINE(psx_realloc8)( INTEGER8(size), POINTER(pntr), INTEGER(status) );


/* 4-byte interface - just calls the 8-byte interface */
F77_SUBROUTINE(psx_realloc)( INTEGER(size), POINTER(pntr), INTEGER(status) ) {
   GENPTR_INTEGER(size)
   GENPTR_POINTER(pntr)
   GENPTR_INTEGER(status)
   DECLARE_INTEGER8(size8);
   size8 = *size;
   F77_CALL(psx_realloc8)( INTEGER8_ARG(&size8), POINTER_ARG(pntr), INTEGER_ARG(status) );
}




/* 8-byte interface */
F77_SUBROUTINE(psx_realloc8)( INTEGER8(size), POINTER(pntr), INTEGER(status) )
{

/* Pointers to Arguments:						    */

   GENPTR_INTEGER8(size)
   GENPTR_POINTER(pntr)
   GENPTR_INTEGER(status)

/* Local Variables:							    */

   size_t csize;                 /* Required memory size                    */
   void *cpntr;                  /* C version of input pointer              */
   void *temp;			 /* Temporary return value from malloc 	    */

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Convert the incoming Fortran pointer to a C pointer and obtain the new   */
/* size.                                                                    */

   cpntr = cnfCptr( *pntr );
   csize = (size_t) *size;

/* Re-allocate the space.		                                    */
   temp = cnfRealloc( cpntr, csize );

/* Check that the space was allocated.					    */

   if( temp != NULL )

/* Copy the pointer to the allocated storage space to the subroutine	    */
/* argument, converting to a Fortran pointer.				    */

   {
      *pntr = cnfFptr( temp );
   }
   else

/* Set STATUS to an error code and report the error.			    */

   {
      /* Free the memory that we had already got */
      cnfFree( pntr );
      *pntr = (F77_POINTER_TYPE)0;
      *status = PSX__NOALL;
      psx1_rep_c( "PSX_REALLOC_NOALL",
                  "Failed to allocate space with realloc. %zu bytes requested",
                  status, csize );
   }

}
