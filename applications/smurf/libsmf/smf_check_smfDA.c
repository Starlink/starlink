/*
*+
*  Name:
*     smf_check_smfDA

*  Purpose:
*     Check (and set) all elements of a smfDA structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_check_smfDA( const smfData *idata, smfData *odata, int * status );

*  Arguments:
*     idata = const smfData* (Given)
*        Pointer to input smfData
*     odata = smfData * (Given)
*        Pointer to output smfData
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function checks all elements of a smfDA structure and
*     copies values from the input structure if necessary

*  Authors:
*     Andy Gibb (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-04-03 (AGG):
*        Initial version.
*     2010-03-09 (TIMJ):
*        Change type of flatname. Use smf_flatmeth.
*        Add support for heatcal copying.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
*     Copyright (C) 2006 University of British Columbia. All Rights
*     Reserved.

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
#include <stdio.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_check_smfDA"

void smf_check_smfDA( const smfData *idata, smfData *odata, int * status ) {

  smfDA *ida = NULL;       /* Input smfDA */
  smfDA *oda = NULL;       /* Output smfDA */
  double *flatcal;         /* pointer to flatfield calibration */
  double *flatpar;         /* pointer to flatfield parameters */
  dim_t nbol;              /* Number of bolometers/pixels */
  dim_t nflat;             /* Number of flat coeffs per bol */

  if (*status != SAI__OK) return;

  /* All the checks are of the type: does it exist? If no, copy from
     input. If yes check it's either self-consistent or the same as
     the input. Set status to bad and report and error if there are any
     errors */

  ida = idata->da;
  oda = odata->da;

  if ( oda == NULL) {
    /* If output smfDA is null, then copy from input */
    if ( ida != NULL ) {
      oda = smf_deepcopy_smfDA( NULL, idata, 1, status );
    } else {
      /* Report error if input smfDA is null */
      if ( *status == SAI__OK) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "Input smfDA struct is null. Possible programming error.",
	       status);
      }
    }
    /* Check we got a valid pointer back from the deepcopy call */
    if ( oda == NULL ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to allocate memory for new smfDA", status);
    } else {
      odata->da = oda;
    }
  } else {
    if ( !(oda->nflat) ) {
      oda->nflat = ida->nflat;
    }
    nflat = oda->nflat;
    /* OK output smfDA is not NULL so check individual components */
    if ( oda->flatcal == NULL && ida->flatcal) {
      nbol = (odata->dims)[0] * (odata->dims)[1];
      flatcal = astMalloc( (nbol*nflat)*sizeof(*flatcal) );
      if ( flatcal != NULL ) {
	memcpy( flatcal, ida->flatcal, nbol*nflat*sizeof(*flatcal) );
      }
      oda->flatcal = flatcal;
    }

    if ( oda->flatpar == NULL && ida->flatcal ) {
      flatpar = astMalloc( nflat*sizeof(*flatpar) );
      if ( flatpar != NULL ) {
	memcpy( flatpar, ida->flatpar, nflat*sizeof(*flatpar));
      }
      oda->flatpar = flatpar;
    }

    if (oda->flatmeth == SMF__FLATMETH_NULL ) oda->flatmeth = ida->flatmeth;
    if (oda->refres == VAL__BADD) oda->refres = ida->refres;

    if (!(oda->nheat)) {
      oda->nheat = ida->nheat;
    }
    if ( oda->heatval == NULL && ida->heatval ) {
      double * heatval;
      nbol = (odata->dims)[0] * (odata->dims)[1];
      heatval = astMalloc( (nbol*oda->nheat)*sizeof(*heatval) );
      if ( heatval ) {
	memcpy( heatval, ida->heatval, nbol*nflat*sizeof(*heatval) );
      }
      oda->heatval = heatval;
    }

  }

}
