/*
*+
*  Name:
*     smf_calcmodel_ast

*  Purpose:
*     Calculate the ASTronomical model signal component

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_ast( smfWorkForce *wf, smfDIMMData *dat, int
*			 chunk, AstKeyMap *keymap, smfArray
*			 **allmodel, int flags, int *status)

*  Arguments:
*     wf = smfWorkForce * (Given)
*        Pointer to a pool of worker threads
*     dat = smfDIMMData * (Given)
*        Struct of pointers to information required by model calculation
*     chunk = int (Given)
*        Index of time chunk in allmodel to be calculated
*     keymap = AstKeyMap * (Given)
*        Parameters that control the iterative map-maker
*     allmodel = smfArray ** (Returned)
*        Array of smfArrays (each time chunk) to hold result of model calc
*     flags = int (Given )
*        Control flags: not used 
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     A special model component that assumes that the map is currently the 
*     best rebinned estimate of the sky and projects that signal into the
*     time domain using the LUT.

*  Notes:
*     -The model pointer is ignored and should be set to NULL.

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-07-10 (EC):
*        Initial Version
*     2006-11-02 (EC):
*        Updated to correctly modify cumulative and residual models
*     2007-03-05 (EC)
*        Modified bit flags
*     2007-05-23 (EC)
*        Removed CUM calculation
*     2007-06-13 (EC)
*        pointing lut supplied as extra parameter to accomodate 
*        new DIMM file format
*     2007-07-10 (EC)
*        Use smfArray instead of smfData
*     2007-11-28 (EC)
*        Added assertions to ensure different data orders will work.
*     2008-03-04 (EC)
*        Modified interface to use smfDIMMData
*     2008-04-02 (EC)
*        Use QUALITY
*     2008-04-29 (EC)
*        Check for VAL__BADD in map to avoid propagating to residual
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

#define FUNC_NAME "smf_calcmodel_ast"

void smf_calcmodel_ast( smfWorkForce *wf, smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap __attribute__((unused)), 
                        smfArray **allmodel, int flags __attribute__((unused)),
                        int *status) {

  /* Local Variables */
  dim_t i;                      /* Loop counter */
  dim_t idx=0;                  /* Index within subgroup */
  smfArray *lut=NULL;           /* Pointer to LUT at chunk */
  int *lut_data=NULL;           /* Pointer to DATA component of lut */
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
  lut = dat->lut[chunk];
  qua = dat->qua[chunk];
  model = allmodel[chunk];

  /* Loop over index in subgrp (subarray) */
  for( idx=0; idx<res->ndat; idx++ ) {

    /* Ensure everything is in the same data order */
    smf_dataOrder( res->sdata[idx], lut->sdata[idx]->isTordered, status );
    smf_dataOrder( qua->sdata[idx], lut->sdata[idx]->isTordered, status );

    /* Get pointers to DATA components */
    res_data = (res->sdata[idx]->pntr)[0];
    lut_data = (lut->sdata[idx]->pntr)[0];
    model_data = (model->sdata[idx]->pntr)[0];
    qua_data = (qua->sdata[idx]->pntr)[0];

    if( (res_data == NULL) || (lut_data == NULL) || (model_data == NULL) || 
	(qua_data == NULL) ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Null data in inputs", status);      
    } else {
	
      /* Get the raw data dimensions */
      ndata = (res->sdata[idx]->dims)[0] * (res->sdata[idx]->dims)[1] * 
	(res->sdata[idx]->dims)[2];
	
      /* Which QUALITY bits should be considered for ignoring data */
      mask = ~(SMF__Q_JUMP|SMF__Q_SPIKE|SMF__Q_STAT);

      /* Loop over data points */ 
      for( i=0; i<ndata; i++ ) {
	if( lut_data[i] != VAL__BADI ) {

	  /* Add previous iteration of the model back into the residual */
	  if( !(qua_data[i]&mask) )

	    if( model_data[i] != VAL__BADD ) {
	      res_data[i] += model_data[i];
	    }
	    
	  /* calculate new model using the map/LUT */
	  model_data[i] = dat->map[lut_data[i]];
	    
	  /* update the residual model */
	  if( !(qua_data[i]&mask) && (model_data[i] != VAL__BADD) )
	    res_data[i] -= model_data[i];
	}
      } 
    }
  }
}
