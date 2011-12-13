/*
*+
*  Name:
*     smf_deepcopy_smfFile

*  Purpose:
*     Copy all elements of a smfFile structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     new = smf_deepcopy_smfFile( const smfFile *old, int * status );

*  Arguments:
*     old = const smfFile* (Given)
*        Pointer to smfFile to be copied
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_deepcopy_smfFile = smfFile*
*        Pointer to newly created smfFile. NULL on error.

*  Description:
*     This function copies all information from an existing smfFile
*     structure and all the internal structures to a new smfFile
*     structure. The NDF identifier (if present) is cloned.

*  Notes:
*     - Free this memory using smf_close_file, via a smfData structure.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-03-29 (AGG):
*        Initial version.
*     2008-07-16 (TIMJ):
*        Clone the NDF identifier so it can be closed separately.
*        Use one_strlcpy.
*     2010-07-09 (TIMJ):
*        If the input smfFile is NULL we also return NULL.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2010 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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
#include "star/one.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_deepcopy_smfFile"

smfFile *
smf_deepcopy_smfFile( const smfFile *old, int * status ) {


  int isSc2store;            /* True if file opened by sc2store library */
  int isTstream;             /* True if file contains time series data */
  char name[SMF_PATH_MAX+1]; /* Name of file */
  int ndfid;                 /* NDF ID of file if opened by SMURF */

  smfFile *new = NULL;       /* Pointer to new smfFile struct */

  if (*status != SAI__OK) return NULL;
  if (!old) return NULL;

  /* Copy elements */
  isTstream = old->isTstream;

  ndfid = NDF__NOID;
  if (old->ndfid != NDF__NOID) ndfClone( old->ndfid, &ndfid, status );

  /* Set isSc2store to zero because we're now dealing with a copy */
  isSc2store = 0;

  if (old->name != NULL) {
    one_strlcpy( name, old->name, sizeof(name), status );
  } else {
    name[0] = '\0';
  }

  /* Construct the new smfFile */
  new = smf_construct_smfFile( new, ndfid, isSc2store, isTstream, name, status);

  return new;
}
