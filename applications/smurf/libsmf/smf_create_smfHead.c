/*
*+
*  Name:
*     smf_create_smfHead

*  Purpose:
*     Allocate a smfHead structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_create_smfHead( int * status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_create_smfHead = smfHead*
*        Pointer to newly created smfHead. NULL on error.

*  Description:
*     This function allocates memory for a smfHead structure and
*     all the internal structures. The structure is initialised.

*  Notes:
*     - Free this memory using smf_close_file, via a smfData structure.
*     - Can be freed with a smf_free if header resources are freed first.

*  Authors:
*     Tim Jenness (TIMJ)
*     {enter_new_authors_here}

*  History:
*     2006-01-26 (TIMJ):
*        Initial version.
*     2006-01-27 (TIMJ):
*        sc2head now pointer. Add allsc2heads as pointer.
*        initialise isCloned
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

#define FUNC_NAME "smf_create_smfHead"

smfHead *
smf_create_smfHead( int * status ) {

  smfHead * hdr = NULL;   /* Header components */

  if (*status != SAI__OK) return NULL;

  hdr = smf_malloc( 1, sizeof(smfHead), 0, status );

  if (*status != SAI__OK) {
    errRep(FUNC_NAME,"Unable to allocate memory for smfHead structure",
	   status );
    return NULL;
  }

  /* Initialise smfHead */
  hdr->wcs = NULL;
  hdr->fitshdr = NULL;
  hdr->curslice = 0;
  hdr->sc2head = NULL;
  hdr->allsc2heads = NULL;
  hdr->isCloned = 0;
  return hdr;
}
