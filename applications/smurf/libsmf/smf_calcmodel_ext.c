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
*        Control flags: only execute if SMF__DIMM_FIRSTITER set
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Apply extinction correction to the data. Generally this will be done
*     deterministically from eithe CSO Tau or WVM measurements stored in
*     the header. Currently this routine is simply a DIMM wrapper for
*     smf_correct_extinction

*  Notes:
*     CSO Tau can't be used because it is not set to anything / obtained from
*     the FITS header.

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
  dim_t base;                   /* Store base index for data array offsets */
  const char *extmethod=NULL;   /* Pointer to static strings created by ast */
  dim_t i;                      /* Loop counter */
  int idx=0;                    /* Index within subgroup */
  dim_t j;                      /* Loop counter */
  double mean;                  /* Array mean */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Total number of data points */
  dim_t ntslice;                /* Number of time slices */
  double lastmean;              /* Mean from previous iteration */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  double sigma;                 /* Array standard deviation */ 
                                   
  /* Main routine */
  if (*status != SAI__OK) return;

  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  model = allmodel[chunk];

  /* Obtain the extinction correction method from the CONFIG file */
  if( !astMapGet0C( keymap, "EXT_METHOD", &extmethod ) ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Error: EXT_METHOD not specified", status);
  } else {

    /* Loop over index in subgrp (subarray) */
    for( idx=0; idx<res->ndat; idx++ ) {

      /* Get pointers to DATA components */
      res_data = (double *)(res->sdata[idx]->pntr)[0];
      model_data = (double *)(model->sdata[idx]->pntr)[0];

      if( (res_data == NULL) || (model_data == NULL) ) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, "Null data in inputs", status);      
      } else {
    
	/* Get the raw data dimensions */
	nbolo = (res->sdata[idx]->dims)[0] * (res->sdata[idx]->dims)[1];
	ntslice = (res->sdata[idx]->dims)[2];
	ndata = nbolo*ntslice;

	/* If SMF__DIMM_FIRSTITER set, call the extinction correction
	   routine for the residual model component, and store the corrections 
	   for each data point to use in subsequent iterations */
	if( flags & SMF__DIMM_FIRSTITER ) {
	  smf_correct_extinction( res->sdata[idx], extmethod, 0, 0, model_data,
				  status );
	  
	} 
      }
    }
  }
}



