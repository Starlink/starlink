/*
*+
*  Name:
*     smf_deepcopy_smfDA

*  Purpose:
*     Copy all elements of a smfDA structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     newda = smf_deepcopy_smfDA( const smfData *old, int * status );

*  Arguments:
*     old = const smfData* (Given)
*        Pointer to smfData containing smfDA to be copied
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_deepcopy_smfDA = smfDA*
*        Pointer to newly created smfDA. NULL on error.

*  Description:
*     This function copies all information from an existing smfDA
*     structure and all the internal structures to a new smfDA
*     structure.

*  Notes:
*     - Free this memory using smf_close_file, via a smfData structure.

*  Authors:
*     Andy Gibb (UBC)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-03-29 (AGG):
*        Initial version.
*     2006-04-05 (AGG):
*        Change API to accept a smfData instead of smfDA so that the
*        size of the allocated buffers can be determined.
*     2008-07-11 (TIMJ):
*        Propagate dark squid. Use one_strlcpy.
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
#include "star/one.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_deepcopy_smfDA"

smfDA *
smf_deepcopy_smfDA( const smfData *old, int * status ) {

  int *dksquid = NULL;    /* pointer to dark squid */
  double *flatcal;        /* pointer to flatfield calibration */
  double *flatpar;        /* pointer to flatfield parameters */
  char flatname[SC2STORE_FLATLEN];/* name of flatfield algorithm */
  size_t nflat;           /* number of flat coeffs per bol */
  size_t nbol;            /* Number of bolometers */
  size_t ncol;            /* Number of columns */
  size_t ntslice;         /* NUmber of time slices */
  smfDA *newda = NULL;             /* Pointer to new smfDA struct */
  smfDA *oldda = NULL;             /* Pointer to new smfDA struct */

  if (*status != SAI__OK) return NULL;

  /* Retrieve smfDA to copy */
  oldda = old->da;

  /* Copy elements */
  nflat = oldda->nflat;

  /* Need the number of bolometers, columns and time slices */
  nbol = (old->dims)[0] * (old->dims)[1];
  ncol = (old->dims)[SMF__COL_INDEX];
  ntslice = (old->dims)[2];

  /* Allocate space for and copy contents of pointers */
  flatcal = smf_malloc( nbol * nflat, sizeof(*flatcal), 0, status );
  if ( flatcal != NULL ) {
    memcpy( flatcal, oldda->flatcal, sizeof(*flatcal)*nbol*nflat );
  }
  flatpar = smf_malloc( nflat, sizeof(*flatpar), 0, status );
  if ( flatpar != NULL ) {
    memcpy( flatpar, oldda->flatpar, sizeof(*flatpar)*nflat);
  }

  if (oldda->dksquid) {
    dksquid = smf_malloc( ncol*ntslice, sizeof(*dksquid), 0, status );
    if (dksquid) {
      memcpy( dksquid, oldda->dksquid, sizeof(*dksquid)*ncol*ntslice );
    }
  }

  if (oldda->flatname != NULL) {
    one_strlcpy(flatname, oldda->flatname, sizeof(flatname), status);
  } else {
    flatname[0] = '\0';
  }

  /* Construct the new smfData */
  newda = smf_construct_smfDA( newda, dksquid, flatcal, flatpar, flatname,
                               nflat, status);

  return newda;
}
