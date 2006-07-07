/*
*+
*  Name:
*     smf_create_smfArray

*  Purpose:
*     Allocate a smfArray structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_create_smfArray( const size_t size, int * status );

*  Arguments:
*     size = size_t (Given)
*        Number of smfDatas to allocate pointers for
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_create_smfArray = smfArray*
*        Pointer to newly created smfArray. NULL on error.

*  Description:
*     This function allocates memory for a smfArray structure, sets
*     the pointers to smfDatas to NULL and sets the number of smfDatas
*     to be stored.

*  Notes:
*     This routine makes the assumption that there cannot be more than
*     2*SMF__MXSMF smfDatas in a smfArray, essentially allowing the
*     grouping of all four SCUBA-2 subarrays at both
*     wavelengths. Something a little more flexible is desireable.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-06-02 (AGG):
*        Initial version, copy from smf_create_smfData
*     2006-07-07 (AGG):
*        Allocate space for smfDatas, increase maximum size to
*        2*SMF__MXSMF
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_create_smfArray"

smfArray *smf_create_smfArray( const size_t size, int * status ) {

  /* Need to make sure that any memory we malloc will be freed on error 
     so make sure we NULL all pointers first. */
  smfArray *ary = NULL;    /* Main struct */
  int i;

  if (*status != SAI__OK) return NULL;

  ary = smf_malloc( 1, sizeof(smfArray), 0, status );

  if (*status != SAI__OK) {
    /* Add our own message to the stack */
    errRep(FUNC_NAME, "Unable to allocate memory for smfData structure",
	   status);
    goto CLEANUP;
  }

  if ( size < 1 ) {
    if ( *status == SAI__OK ) {
      msgSeti("S",size);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Invalid number, ^S, of smfDatas requested", 
	     status);
      goto CLEANUP;
    }
  }
  /* Check that we're not asking to create more smfDatas than we're
     allowed to */
  if ( size > 2*SMF__MXSMF ) {
    if ( *status == SAI__OK ) {
      msgSeti("S",size);
      msgSeti("M",SMF__MXSMF);
      *status = SAI__ERROR;
      errRep(FUNC_NAME, 
	     "Too many smfDatas requested: size, ^S, exceeds maximum of ^M", 
	     status);
      goto CLEANUP;
    }
  }

  /* Allocate space for smfDatas */
  ary->sdata = smf_malloc( size, sizeof(smfData), 0, status );;

  /* Set each pointer to NULL */
  for ( i=0; i<size; i++) {
    (ary->sdata)[i] = NULL;
  }

  /* Set number of requested smfDatas */
  ary->ndat = size;

  return ary;

 CLEANUP:
  smf_free( ary, status );
  
  return NULL;
}
