/*
*+
*  Name:
*     smf_calcmodel_ext

*  Purpose:
*     Apply EXTinction correction

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_ext( smfDIMMData *dat, int chunk, AstKeyMap *keymap, 
*			 smfArray **allmodel, int flags, int *status)

*  Arguments:
*     dat = smfDIMMData * (Given)
*        Struct of pointers to information required by model calculation
*     chunk = int (Given)
*        Index of time chunk in allmodel to be calculated
*     keymap = AstKeyMap * (Given)
*        Parameters that control the iterative map-maker
*     allmodel = smfArray ** (Returned)
*        Array of smfArrays (each time chunk) to hold result of model calc
*     flags = int (Given )
*        Control flags: if SMF__DIMM_INVERT undo the extinction correction
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Apply the extinction correction to the residual

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-03-05 (EC):
*        Initial Version
*        Modified data array indexing to avoid unnecessary multiplies
*     2007-07-10 (EC):
*        Use smfArray instead of smfData
*     2008-03-04 (EC)
*        Modified interface to use smfDIMMData
*     2008-03-30 (EC)
*        -Stripped out calculation. Model is now filled by smf_model_create.
*        -added SMF__DIMM_INVERT flag to undo extinction correction
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

#define FUNC_NAME "smf_calcmodel_ext"

void smf_calcmodel_ext( smfDIMMData *dat, int chunk, AstKeyMap *keymap, 
			smfArray **allmodel, int flags, int *status) {

  /* Local Variables */
  dim_t i;                      /* Loop counter */
  dim_t idx=0;                  /* Index within subgroup */
  unsigned char mask;           /* Bitmask for quality */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  dim_t ndata;                  /* Number of data points */
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  unsigned char *qua_data=NULL; /* Pointer to quality data */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DATA component of res */


  /* Main routine */
  if (*status != SAI__OK) return;

  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  qua = dat->qua[chunk];
  model = allmodel[chunk];

  /* Loop over index in subgrp (subarray) */
  for( idx=0; idx<res->ndat; idx++ ) {

    /* Ensure everything is in bolo-order */
    smf_dataOrder( res->sdata[idx], 0, status );
    smf_dataOrder( qua->sdata[idx], 0, status );
    smf_dataOrder( model->sdata[idx], 0, status );

    /* Get pointers to DATA components */
    res_data = (res->sdata[idx]->pntr)[0];
    qua_data = (qua->sdata[idx]->pntr)[0];
    model_data = (model->sdata[idx]->pntr)[0];


    if( (res_data == NULL) || (model_data == NULL) || (qua_data == NULL) ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Null data in inputs", status);      
    } else {
	
      /* Get the raw data dimensions */
      ndata = (res->sdata[idx]->dims)[0] * (res->sdata[idx]->dims)[1] * 
	(res->sdata[idx]->dims)[2];
	
      /* Which QUALITY bits should be considered for ignoring data */
      mask = ~(SMF__Q_JUMP | SMF__Q_SPIKE);

      /* Loop over data points */ 

      if( !(flags&SMF__DIMM_INVERT) ) {
	/* Apply the extinction correction */
	for( i=0; i<ndata; i++ ) {
	  if( !(qua_data[i]&mask) ) {
	    res_data[i] *= model_data[i];
	  }
	}
      } else {
	/* Undo the extinction correction */
	for( i=0; i<ndata; i++ ) {
	  if( !(qua_data[i]&mask) && (model_data[i] > 0) ) {
	    res_data[i] /= model_data[i];
	  }
	}
      }
    }
  }
}




