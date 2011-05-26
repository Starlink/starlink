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
*     pntr = smf_create_smfArray(  int * status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_create_smfArray = smfArray*
*        Pointer to newly created smfArray. NULL on error. Must be
*        freed using smf_close_related.

*  Description:
*     This function allocates memory for a smfArray structure, sets
*     the pointers to smfDatas to NULL.

*  Notes:
*     - if smf_addto_smfArray is used to populate the smfArray,
*       it will be dynamically resized if more than SMF__MXSMF items
*       are stored in it.
*     - By default, the smfArray is created assuming that any smfData
*       can be freed by it.

*  Authors:
*     Andy Gibb (UBC)
*     Ed Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-06-02 (AGG):
*        Initial version, copy from smf_create_smfData
*     2006-07-07 (AGG):
*        Allocate space for smfDatas, increase maximum size to
*        2*SMF__MXSMF
*     2007-07-10 (EC):
*        smfArray.sdata is now static array with SMF__MXSMF entries, and
*        smfArray.ndat is initialized to 0 (incremented with each
*        smf_addto_smfArray call)
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-07-14 (TIMJ):
*        Initialise to take into account new dynamic behaviour.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006, 2007 University of British Columbia.
*     All Rights Reserved.

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
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_create_smfArray"

smfArray *smf_create_smfArray( int * status ) {

  /* Need to make sure that any memory we malloc will be freed on error
     so make sure we NULL all pointers first. */
  smfArray *ary = NULL;    /* Main struct */
  dim_t i;

  if (*status != SAI__OK) return NULL;

  ary = astMalloc( 1*sizeof(smfArray) );

  if (*status != SAI__OK) {
    /* Add our own message to the stack */
    errRep(FUNC_NAME, "Unable to allocate memory for smfData structure",
	   status);
    goto CLEANUP;
  }

  /* Point the default content to static buffer */
  ary->sdata = ary->stdata;
  ary->dyndata = NULL;

  /* Set each smfData pointer to NULL */
  for ( i=0; i<SMF__MXSMF; i++) {
    (ary->sdata)[i] = NULL;
  }

  /* Initialize number of smfDatas */
  ary->ndat = 0;
  ary->dynsize = 0;

  /* we own any smfData */
  ary->owndata = 1;

  return ary;

 CLEANUP:
  if ( ary )
    ary = astFree( ary );

  return NULL;
}
