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
*     {enter_new_authors_here}

*  History:
*     2006-04-03 (AGG):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 University of British Columbia. All Rights
*     Reserved.

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
  int nbol;                /* Number of bolometers/pixels */
  int nflat;               /* Number of flat coeffs per bol */

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
      oda = smf_deepcopy_smfDA( idata, status );
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
    if ( oda->flatcal == NULL ) {
      nbol = (odata->dims)[0] * (odata->dims)[1];
      flatcal = smf_malloc( nbol*nflat, sizeof(double), 0, status );
      if ( flatcal != NULL ) {
	memcpy( flatcal, ida->flatcal, nbol*nflat*sizeof(double) );
      }
      oda->flatcal = flatcal;
    }

    if ( oda->flatpar == NULL ) {
      flatpar = smf_malloc( nflat, sizeof(double), 0, status );
      if ( flatpar != NULL ) {
	memcpy( flatpar, ida->flatpar, nflat*sizeof(double));
      }
      oda->flatpar = flatpar;
    }

    if (oda->flatname == NULL) {
      if ( ida->flatname != NULL ) {
	strncpy(oda->flatname, ida->flatname, SC2STORE_FLATLEN);
	(oda->flatname)[SC2STORE_FLATLEN-1] = '\0';
      } else {
	if ( *status == SAI__OK) {
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "Name of flatfield algorithm is not set. Possible programmign error.", status);
	}
      }
    }

  }

}
