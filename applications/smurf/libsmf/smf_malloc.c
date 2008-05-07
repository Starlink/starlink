/*
*+
*  Name:
*     smf_malloc

*  Purpose:
*     Low-level SMURF malloc

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_malloc( size_t nelem, size_t bperel, int zero, int * status );

*  Arguments:
*     nelem = size_t (Given)
*        Number of elements to allocate
*     bperel = size_t (Given)
*        Size of each element in bytes.
*     zero = int (Given)
*        If true, the memory will be initialised to zero.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_malloc = void*
*        Pointer. NULL on error.

*  Description:
*     This is the SMURF malloc routine. It should be used in preference
*     to system malloc since it understands status and will set status
*     if there is a malloc error. It can also be used to replace calloc()
*     if the "zero" parameter is true. Additionally, it may at some point
*     use varying allocation routines.

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
smf_malloc( size_t nelem, size_t bperel, int zero, int * status ) {
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

  retval = astMalloc( nelem * bperel );
  /*retval = malloc( nelem * bperel );*/

  /* Check for error */
  if ( !astOK || !retval ) {
    *status = SMF__NOMEM;
    msgSeti( "NEL", nelem );
    msgSeti( "BP" , bperel );
    msgSeti( "T", nbytes );
    errRep( FUNC_NAME, "Error allocating memory for ^NEL items of size ^BP bytes (^T bytes total)",
	    status );
  }

  if (zero && *status == SAI__OK) memset( retval, 0, nbytes );

  return retval;

}
