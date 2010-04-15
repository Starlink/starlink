/*
*+
*  Name:
*     smf_realloc

*  Purpose:
*     Low-level SMURF realloc

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     npntr = smf_realloc( void * pntr, size_t nelem, size_t bperel,
*                         int * status );

*  Arguments:
*     pntr = void* (Given)
*        Pointer to use for re-allocation. The return value may or may not
*        be this value. This memory must have been allocated with smf_malloc.
*        On error conditions this memory will be left unchanged.
*     nelem = size_t (Given)
*        Number of elements to re-allocate
*     bperel = size_t (Given)
*        Size of each element in bytes.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_realloc = void*
*        Pointer. NULL on error. If NULL is returned, it may be desirable
*        to free the input pointer "pntr" if status is SMF__NOMEM.

*  Description:
*     This is the SMURF realloc routine. It should be used in preference
*     to system realloc since it understands status and will set status
*     if there is a malloc error.

*  Notes:
*     It is bad practice to assign the return value of this function
*     directly to the first argument. This is because if the reallocation
*     fails it will no longer be possible to free the memory.

*  Authors:
*     Tim Jenness (TIMJ)
*     Andy Gbb (AGG)
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-01-25 (TIMJ):
*        Initial version
*     2006-05-23 (AGG):
*        Add status check on return from astMalloc
*     2008-05-07 (EC):
*        Ast status was not being checked properly
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* System includes */
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"

/* SMURF routines */
#include "smf.h"
#include "smf_err.h"

#define FUNC_NAME "smf_malloc"

void *
smf_realloc( void * pntr, size_t nelem, size_t bperel, int * status ) {
  void * retval;   /* Local temp pointer */
  size_t nbytes;

  if (*status != SAI__OK) return NULL;

  nbytes = nelem * bperel;

  if (nbytes == 0) {
    *status = SMF__NOMEM;
    msgSeti( "NEL", nelem );
    msgSeti( "BP" , bperel );
    if ( nelem == 0 ) {
      errRep( FUNC_NAME, "Attempt to allocate zero elements of size ^BP bytes "
	      "(possible programming error)", status);
    } else {
      errRep( FUNC_NAME, "Attempt to allocate ^NEL elements of zero bytes "
	      "(possible programming error)", status);
    }

    return NULL;
  }

  retval = astRealloc( pntr, nelem * bperel );
  /*retval = realloc( pntr, nelem * bperel );*/

  /* Check for error */
  if ( !astOK || !retval ) {
    *status = SMF__NOMEM;
    msgSeti( "NEL", nelem );
    msgSeti( "BP" , bperel );
    msgSeti( "T", nbytes );
    errRep( FUNC_NAME, "Error re-allocating memory for ^NEL items of size ^BP bytes (^T bytes total)",
	    status );
  }

  return retval;

}
