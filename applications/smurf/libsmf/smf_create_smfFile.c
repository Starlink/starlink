/*
*+
*  Name:
*     smf_create_smfFile

*  Purpose:
*     Allocate a smfFile structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_create_smfFile( int * status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_create_smfFile = smfFile*
*        Pointer to newly created smfFile. NULL on error.

*  Description:
*     This function allocates memory for a smfFile structure and
*     all the internal structures. The structure is initialised.

*  Notes:
*     - Free this memory using smf_close_file, via a smfData structure.
*     - Can be freed with a smf_free if file resources are freed first.

*  Authors:
*     Tim Jenness (TIMJ)
*     {enter_new_authors_here}

*  History:
*     2006-01-26 (TIMJ):
*        Initial version.
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

#define FUNC_NAME "smf_create_smfFile"

smfFile *
smf_create_smfFile( int * status ) {

  smfFile * file = NULL;   /* File components */

  if (*status != SAI__OK) return NULL;

  file = smf_malloc( 1, sizeof(smfFile), 0, status );

  if (*status != SAI__OK) {
    errRep(FUNC_NAME,"Unable to allocate memory for smfFile structure",
	   status );
    return NULL;
  }

  /* Initialise smfFile */
  file->ndfid = NDF__NOID;
  file->isSc2store = 0;
  file->isTstream = 0;
  (file->name)[0] = '\0';
  file->xloc = NULL;
  printf("Inside smfFile: %p\n", file);
  return file;
}
