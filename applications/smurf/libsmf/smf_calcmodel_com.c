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
*     smf_calcmodel_com( smfArray *res, AstKeyMap *keymap, 
*                        double *map, double *mapvar, smfArray *model, 
*                        int flags, int *status);

*  Arguments:
*     res = smfArray * (Given and Returned)
*        The residual signal from previously calculated model components
*     keymap = AstKeyMap * (Given)
*        Parameters that control the iterative map-maker
*     map = double * (Given)
*        Buffer containing current estimate of the map (must match the LUT
*        in the mapcoord extension of the res data structure)
*     mapvar = double * (Given)
*        Buffer containing current variance estimate corresponding to map
*     model = smfArray * (Returned)
*        The data structure that will store the calculated model parameters
*     flags = int (Given )
*        Control flags: 
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
*     2007-05-23 (EC)
*        - Removed CUM calculation
*        - Added COM_BOXCAR parameter to CONFIG file
*     2007-07-10 (EC)
*        Use smfArray instead of smfData
*     2007-07-13 (EC):
*        Calculate only 1 model component for each smfArray
*     2007-07-16 (EC):
*        Modified range checking for range of smfData's in smfArray
*     2007-08-09 (EC):
*        Fixed bug in replacement of model in residual before calculating
*        new model for current iteration.
*     {enter_further_changes_here}


*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
*     All Rights Reserved.

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

void smf_calcmodel_com( smfArray *res, AstKeyMap *keymap, 
			double *map, double *mapvar, smfArray *model, 
			int flags, int *status ) {

  /* Local Variables */
  dim_t base;                   /* Store base index for data array offsets */
  int boxcar=0;                 /* width in samples of boxcar filter */ 
  int do_boxcar=0;              /* flag to do boxcar smooth */
  dim_t i;                      /* Loop counter */
  int idx=0;                    /* Index within subgroup */
  dim_t j;                      /* Loop counter */
  double mean=0;                /* Array mean */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  double *model_data_copy=NULL; /* Copy of model_data */
  dim_t nbolo=0;                /* Number of bolometers */
  dim_t ndata=0;                /* Total number of data points */
  dim_t ntslice=0;              /* Number of time slices */
  double lastmean;              /* Mean from previous iteration */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  double sigma=0;               /* Array standard deviation */ 
  dim_t thisnbolo=0;            /* Check each file same dims as first */
  dim_t thisndata=0;            /* "                                  */
  dim_t thisntslice=0;          /* "                                  */
  double *weight;               /* Weight at each point in model */
                                   
  /* Main routine */
  if (*status != SAI__OK) return;

  /* Check for smoothing parameters in the CONFIG file */
  if( astMapGet0I( keymap, "COM_BOXCAR", &boxcar) ) {
    do_boxcar = 1;
  }

  /* The common mode signal is stored as a 1d array for the entire subgroup.
     The corresponding smfData is at position 0 in the model sdata. */
  
  if( model->sdata[0] ) {
    /* Pointer to model data array */
    model_data = (double *)(model->sdata[0]->pntr)[0];

    /* Copy of model data array */
    model_data_copy = smf_malloc( (model->sdata[0]->dims)[0], 
			  sizeof(*model_data_copy), 1, status );

    if( *status == SAI__OK ) {
      memcpy( model_data_copy, model_data, (model->sdata[0]->dims)[0] *
	      sizeof(*model_data_copy) );

      /* Set model_data to 0 now since it will now be re-calculated */
      memset( model_data, 0, (model->sdata[0]->dims)[0] * 
	      sizeof(*model_data_copy) );
    }


    /* Temporary buffer to store weights */
    weight = smf_malloc( (model->sdata[0]->dims)[0], sizeof(*weight), 1,
			 status );
  } else {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Model smfData was not loaded!", status);      
  }


  /* Loop over index in subgrp (subarray) */
  for( idx=0; idx<res->ndat; idx++ ) if (*status == SAI__OK ) {

    /* Obtain dimensions of the data */
    thisnbolo = (res->sdata[idx]->dims)[0] * (res->sdata[idx]->dims)[1];
    thisntslice = (res->sdata[idx]->dims)[2];
    thisndata = thisnbolo*thisntslice;

    if( idx == 0 ) {
      /* Store dimensions of the first file */
      nbolo = thisnbolo;
      ntslice = thisntslice;
      ndata = thisndata;
    } else {
      /* Check that dimensions haven't changed */
      if( (thisnbolo != nbolo) || (thisntslice != ntslice) || 
	  (thisndata != ndata) ) {
	*status = SAI__ERROR;

	errRep(FUNC_NAME, "smfData's in smfArray have different dimensions!", 
	       status);      
      }

    }

    /* Get pointer to DATA component of residual */
    res_data = (double *)(res->sdata[idx]->pntr)[0];

    if( (res_data == NULL) || (model_data == NULL) ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Null data in inputs", status);      
    } else {
    
      for( i=0; i<ntslice; i++ ) {
	
	base = i*nbolo;  /* Index to first data point in i'th time slice */


	/* Add the previous iteration of the model back into the residual, 
	   and then set the model to 0. */

	lastmean = model_data_copy[i];

	for( j=0; j<nbolo; j++ ) {
	  res_data[base + j] += lastmean;
	}

	/* Now calculate the new contribution to the model from the current
	   sub-array average at this time slice */
	smf_calc_stats( res->sdata[idx], "t", i, 0, 0, &mean, &sigma, status );
	model_data[i] += mean;
	weight[i] ++;
      }
    }
  }
    
  /* Re-normalize the model */
  for( i=0; i<ntslice; i++ ) {
    if( weight[i] ) {
      model_data[i] /= weight[i];
    }
  }

  /* boxcar smooth if desired */
  if( do_boxcar ) {
    smf_boxcar1( model_data, ntslice, boxcar, status );
  }

  /* remove common mode from residual */
  for( idx=0; idx<res->ndat; idx++ ) if (*status == SAI__OK ) {
    /* Get pointer to DATA component of residual */
    res_data = (double *)(res->sdata[idx]->pntr)[0];

    /* Remove common mode from the residual */
    for( i=0; i<ntslice; i++ ) {
      base = i*nbolo;  /* Index to first data point in i'th time slice */
      
      /* Loop over bolometer */
      for( j=0; j<nbolo; j++ ) {
	/* update the residual model */
	res_data[base + j] -= model_data[i];
      }
    }    
  }

  /* Clean up */
  if( weight) smf_free( weight, status );
  if( model_data_copy ) smf_free( model_data_copy, status );
}



