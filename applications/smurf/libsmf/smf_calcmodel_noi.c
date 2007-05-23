/*
*+
*  Name:
*     smf_calcmodel_noi

*  Purpose:
*     Calculate the NOIse model for the bolometers

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_noi( smfData *res, AstKeyMap *keymap, 
*                        double *map, double *mapvar, smfData *model, 
*                        int flags, int *status);

*  Arguments:
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
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Calculate the noise distribution for each detector. Currently this
*     will just assume stationary, independent noise in each detector. In
*     addition to storing the standard deviation in the model component, the
*     VARIANCE component of the residuals will store the same information
*     so that it may be used in the estimation of other model components.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-03-02 (EC):
*        Initial Version
*     2007-03-05 (EC)
*        Modified bit flags

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

#define FUNC_NAME "smf_calcmodel_noi"

void smf_calcmodel_noi( smfData *res, AstKeyMap *keymap, 
			double *map, double *mapvar, smfData *model, 
			int flags, int *status) {

  /* Local Variables */
  dim_t i;                      /* Loop counter */
  dim_t j;                      /* Loop counter */
  double mean;                  /* Array mean */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Total number of data points */
  dim_t ntslice;                /* Number of time slices */
  double lastmean;              /* Mean from previous iteration */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  double *res_var=NULL;         /* Pointer to VARIANCE component of res */
  double sigma;                 /* Array standard deviation */ 
  double var;                   /* Sample variance */
                                   
  /* Main routine */
  if (*status != SAI__OK) return;

  /* Get pointers to DATA components */
  res_data = (double *)(res->pntr)[1];
  res_var = (double *)(res->pntr)[1];
  model_data = (double *)(model->pntr)[0];

  if( (res_data == NULL) || (model_data == NULL) ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Null data in inputs", status);      
  } else {
    
    /* Get the raw data dimensions */
    nbolo = (res->dims)[0] * (res->dims)[1];
    ntslice = (res->dims)[2];
    ndata = nbolo*ntslice;

    for( i=0; i<nbolo; i++ ) {
      /* Don't need to adjust res since this model component is just
         a weight calculation. Measure the sample standard deviation for
	 each bolometer assuming it is stationary in time... */
      smf_calc_stats( res, "b", i, 0, 0, &mean, &sigma, status );
      var = sigma*sigma;
      
      /* Loop over time and store the model component / variance for RES */
      for( j=0; j<ntslice; j++ ) {
	model_data[j*nbolo + i] = sigma;
	res_var[j*nbolo + i] = var;
      }
    }    
  }
}



