/*
*+
*  Name:
*     smf_calcmodel_com

*  Purpose:
*     Calculate the COMmon-mode model signal component

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_com( smfData *cum, smfData *res, AstKeyMap *keymap, 
*                        double *map, double *mapvar, smfData *model, 
*                        int flags, int *status);

*  Arguments:
*     cum = smfData * (Given and Returned)
*        The cummulative signal from previously calculated model components
*     res = smfData * (Given and Returned)
*        The residual signal from previously calculated model components
*     keymap = AstKeyMap * (Given)
*        Parameters that control the iterative map-maker
*     map = double * (Given)
*        Buffer containing current estimate of the map (must match the LUT
*        in the mapcoord extension of the res data structure)
*     mapvar = double * (Given)
*        Buffer containing current variance estimate corresponding to map
*     model = smfData * (Returned)
*        The data structure that will store the calculated model parameters
*     flags = int (Given )
*        Control flags: 
*        SMF__DIMM_FIRSTCOMP - initializes CUM if first model component
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Calculate the common-mode signal measured at every time-slice.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-07-10 (EC):
*        Initial Version
*     2006-11-02 (EC):
*        Updated to correctly modify cumulative and residual models
*     2007-02-08 (EC):
*        Fixed bug in replacing previous model before calculating new one
*     2007-03-05 (EC)
*        Modified bit flags
*        Modified data array indexing to avoid unnecessary multiplies
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
*     All Rights Reserved.

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

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_calcmodel_com"

void smf_calcmodel_com( smfData *cum, smfData *res, AstKeyMap *keymap, 
			double *map, double *mapvar, smfData *model, 
			int flags, int *status) {

  /* Local Variables */
  dim_t base;                   /* Store base index for data array offsets */
  double *cum_data=NULL;        /* Pointer to DATA component of cum */
  dim_t i;                      /* Loop counter */
  dim_t j;                      /* Loop counter */
  double mean;                  /* Array mean */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Total number of data points */
  dim_t ntslice;                /* Number of time slices */
  double lastmean;              /* Mean from previous iteration */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  double sigma;                 /* Array standard deviation */ 
                                   
  /* Main routine */
  if (*status != SAI__OK) return;

  /* Get pointers to DATA components */
  cum_data = (double *)(cum->pntr)[0];
  res_data = (double *)(res->pntr)[0];
  model_data = (double *)(model->pntr)[0];

  if( (cum_data == NULL) || (res_data == NULL) || (model_data == NULL) ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Null data in inputs", status);      
  } else {
    
    /* Get the raw data dimensions */
    nbolo = (cum->dims)[0] * (cum->dims)[1];
    ntslice = (cum->dims)[2];
    ndata = nbolo*ntslice;

    /* If SMF__DIMM_FIRSTCOMP set, initialize this iteration by clearing the
       cumulative model buffer */
    if( flags & SMF__DIMM_FIRSTCOMP ) {
      memset( cum_data, 0, ndata*sizeof(cum_data) );
    }

    for( i=0; i<ntslice; i++ ) {

      base = i*nbolo;  /* Index to first data point in i'th time slice */

      /* Add previous iteration of the model back into the residual */
      lastmean = model_data[i];

      for( j=0; j<nbolo; j++ ) {
	res_data[base + j] += lastmean;
      }

      /* Now calculate the new model */
      smf_calc_stats( res, "t", i, 0, 0, &mean, &sigma, status );
      model_data[i] = mean;
      
      /* Loop over bolometer */
      for( j=0; j<nbolo; j++ ) {

	/* update the cumulative/residual model */
	cum_data[base + j] += mean;
	res_data[base + j] -= mean;
      }
    }    
  }
}



