/*
*+
*  Name:
*     smf_subtract_poly

*  Purpose:
*     Low-level polynomial subtraction routine

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_subtract_poly( smfData **data, int *status ) 

*  Arguments:
*     data = smfData** (Given and Returned)
*        Pointer to input data struct
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine implementing the quick-look polynomial
*     sky subtraction. 

*  Notes:
*     Is is assumed that the polynomial is a function of the timeslice
*     index.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-02-17 (AGG):
*        Initial test version
*     2006-04-21 (AGG):
*        Add history check, and update history if routine successful
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

/* Standard includes */
#include <stdio.h>

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_subtract_poly"

void smf_subtract_poly(smfData *data, int *status) {

  /* Local variables */
  int i;                      /* Bolometer index loop counter */
  int j;                      /* Timeslice index loop counter */
  int k;                      /* Coefficient index loop counter */
  int nbol;                   /* Number of bolometers */
  int nframes;                /* Number of time slices */

  double *outdata = NULL;            /* Pointer to output data array */
  double baseline = 0.0;      /* Baseline level to be subtracted */

  double *poly;
  int ncoeff;

  /* Check status */
  if (*status != SAI__OK) return;

  if ( smf_history_check( data, FUNC_NAME, status) ) return;

  /* Retrieve polynomial data */
  ncoeff = data->ncoeff;
  poly = data->poly;

  /* Check they're non NULL */
  if ( (ncoeff == 0)  || (poly == NULL) ) {
    msgOutif(MSG__VERB, FUNC_NAME, "No polynomial coefficients present", status);
    return;
  }

  /* Data array */
  outdata = (data->pntr)[0];

  /* Calculate the number of bolometers and retrieve number of coefficients */
  nbol = (data->dims)[0] * (data->dims)[1];
  nframes = (data->dims)[2];
  
  /* Loop over the number of bolometers */
  for (i=0; i<nbol; i++) {

    /* Loop over the timeslices for this bolometer */
    for (j=0; j<nframes; j++) {

      /* Construct the polynomial for this bolometer */
      baseline = 0.0;
      for (k=0; k<ncoeff; k++) {
	baseline += poly[i + nbol*k] * (double)pow(j, k);
      }
      outdata[i + nbol*j] -= baseline;
    }

  }
  /* Store polynomial-subtracted data */
  (data->pntr)[0] = outdata;

  /* Write history entry */
  if ( *status == SAI__OK ) {
    smf_history_add( data, FUNC_NAME, 
		       "Polynomial sky subtraction successful", status);
  } else {
    errRep(FUNC_NAME, "Error: status set bad. Possible programming error.", 
	   status);
  }

}
