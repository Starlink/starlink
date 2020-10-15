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
*     newda = smf_deepcopy_smfDA( ThrWorkForce *wf, const smfData *old, int cpdks, int * status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     old = const smfData* (Given)
*        Pointer to smfData containing smfDA to be copied
*     cpdks = int (Given)
*        If true the dark squid smfData will be copied. If false
*        not dark squid information will be copied and the dksquid
*        will be NULL.
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
*     Tim Jenness (JAC, Hawaii)
*     David Berry (JAC, Hawaii)
*     COBA: Coskun Oba (UoL)
*     {enter_new_authors_here}

*  History:
*     2006-03-29 (AGG):
*        Initial version.
*     2006-04-05 (AGG):
*        Change API to accept a smfData instead of smfDA so that the
*        size of the allocated buffers can be determined.
*     2008-07-11 (TIMJ):
*        Propagate dark squid. Use one_strlcpy.
*     2010-03-04 (TIMJ):
*        Add protection against NULL pointers and add heater entry.
*     2010-03-09 (TIMJ):
*        Method is now an enum
*     2010-03-11 (DSB):
*        Initialise to NULL the pointers that are passed to
*        smf_construct_smfDA in case the supplied smfDA does not contain
*        the corresponding arrays.
*     2010-07-01 (TIMJ):
*        Allow for the dark squid copy to be disabled.
*     2010-07-06 (DSB):
*        Get dimensions using smf_get_dims so that bolo-ordered data
*        can be handled.
*     2010-09-21 (COBA):
*        Add SMF__NOCREATE_FTS
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008, 2010 Science and Technology Facilities Council.
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
#include "star/one.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_deepcopy_smfDA"

smfDA *
smf_deepcopy_smfDA( ThrWorkForce *wf, const smfData *old, int cpdks, int * status ) {

  smfData *dksquid = NULL;/* pointer to dark squid */
  double *flatcal = NULL; /* pointer to flatfield calibration */
  double *flatpar = NULL; /* pointer to flatfield parameters */
  double *heatval = NULL; /* pointer to heater values */
  double refres = VAL__BADD; /* Reference heater resistance */
  smf_flatmeth flatmeth;  /* Flatfield method */
  dim_t nheat;            /* number of entries in heatval */
  int nflat;              /* number of flat coeffs per bol */
  dim_t nbol;             /* Number of bolometers */
  dim_t ncol;             /* Number of columns */
  dim_t ntslice;          /* NUmber of time slices */
  smfDA *newda = NULL;    /* Pointer to new smfDA struct */
  smfDA *oldda = NULL;    /* Pointer to new smfDA struct */

  if (*status != SAI__OK) return NULL;

  /* Retrieve smfDA to copy */
  oldda = old->da;

  /* Return if smfDA is NULL */
  if( !oldda ) return NULL;

  /* Copy elements */
  nflat = oldda->nflat;
  nheat = oldda->nheat;
  flatmeth = oldda->flatmeth;
  refres = oldda->refres;

  /* Need the number of bolometers, columns and time slices */
  smf_get_dims( old, NULL, &ncol, &nbol, &ntslice, NULL, NULL, NULL, status );

  /* Allocate space for and copy contents of pointers */
  if (oldda->flatcal && nflat > 0 ) {
    flatcal = astMalloc( (nbol * nflat)*sizeof(*flatcal) );
    if ( flatcal != NULL ) {
      memcpy( flatcal, oldda->flatcal, sizeof(*flatcal)*nbol*nflat );
    }
  }
  if (oldda->flatpar && nflat > 0) {
    flatpar = astMalloc( nflat*sizeof(*flatpar) );
    if ( flatpar != NULL ) {
      memcpy( flatpar, oldda->flatpar, sizeof(*flatpar)*nflat);
    }
  }

  if (oldda->heatval && nheat > 0) {
    heatval = astMalloc( nflat*sizeof(*heatval) );
    if ( heatval != NULL ) {
      memcpy( heatval, oldda->heatval, sizeof(*heatval)*nflat);
    }
  }

  if (cpdks && oldda->dksquid) {
    dksquid = smf_deepcopy_smfData( wf, oldda->dksquid, 0, SMF__NOCREATE_HEAD |
                                    SMF__NOCREATE_FILE | SMF__NOCREATE_DA |
                                    SMF__NOCREATE_FTS, 0, 0, status );
  }

  /* Construct the new smfData */
  newda = smf_construct_smfDA( newda, dksquid, flatcal, flatpar, flatmeth,
                               nflat, refres, heatval, nheat, status);

  return newda;
}
