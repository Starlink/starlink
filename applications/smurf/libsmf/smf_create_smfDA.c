/*
*+
*  Name:
*     smf_create_smfDA

*  Purpose:
*     Allocate a smfDA structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_create_smfDA( int * status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_create_smfDA = smfDa*
*        Pointer to newly created smfDA. NULL on error.

*  Description:
*     This function allocates memory for a smfDA structure and
*     all the internal structures. The structure is initialised.

*  Notes:
*     - Free this memory using smf_close_file, via a smfData structure.
*     - Memory for the flatfield structures is not allocated by this
*       routine.
*     - Can be freed with a smf_free if sc2store resources are freed first.

*  Authors:
*     Tim Jenness (TIMJ)
*     {enter_new_authors_here}

*  History:
*     2006-01-26 (TIMJ):
*        Initial version.
*     2006-01-27 (TIMJ):
*        Remove dksquid entry for now
*     2008-07-11 (TIMJ):
*        Add dksquid.
*     2010-03-09 (TIMJ):
*        flatname is now flatmeth. Add heatval/nheat.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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

#define FUNC_NAME "smf_create_smfDA"

smfDA *
smf_create_smfDA( int * status ) {

  smfDA * da = NULL;   /* File components */

  if (*status != SAI__OK) return NULL;

  da = astMalloc( 1*sizeof(*da) );

  if (*status != SAI__OK) {
    errRep(FUNC_NAME,"Unable to allocate memory for smfDA structure",
	   status );
    return NULL;
  }

  /* Initialise smfDA */
  da->flatcal = NULL;
  da->flatpar = NULL;
  da->dksquid = NULL;
  da->flatmeth = SMF__FLATMETH_NULL;
  da->nflat = 0;
  da->refres = VAL__BADD;
  da->heatval = NULL;
  da->nheat = 0;

  return da;
}
