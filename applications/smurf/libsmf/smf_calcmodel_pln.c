/*
*+
*  Name:
*     smf_calcmodel_pln

*  Purpose:
*     Model to calculate a fitted plane for each time slice

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_pln( ThrWorkForce *wf, smfDIMMData *dat, int
*			 chunk, AstKeyMap *keymap, smfArray
*			 **allmodel, int flags, int *status)

*  Arguments:
*     wf = ThrWorkForce * (Given)
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
*     Fit a plane to each time slice. The model currently consists of the fit
*     itself rather than the coefficients of the fit. This is easier to visualise.

*  Notes:

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-05-13 (TIMJ):
*        Initial version
*     2010-06-10 (TIMJ):
*        Fix precedence bug in checking !SMF__Q_MOD
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_calcmodel_pln"

void smf_calcmodel_pln( ThrWorkForce *wf, smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap, smfArray **allmodel,
                        int flags __attribute__((unused)),
                        int *status) {

  /* Local Variables */
  dim_t bstride;               /* bolo stride */
  dim_t i;                     /* Loop counter */
  dim_t idx=0;                  /* Index within subgroup */
  AstKeyMap *kmap=NULL;         /* Pointer to PLN-specific keys */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  double *model_data_copy=NULL; /* Copy of model_data for one bolo */
  int *lut_data = NULL;         /* Lut data for one subarray */
  dim_t nbolo=0;                /* Number of bolometers */
  dim_t ndata=0;                /* Total number of data points */
  int notfirst=0;               /* flag for delaying until after 1st iter */
  dim_t ntslice=0;              /* Number of time slices */
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  smf_qual_t *qua_data=NULL; /* Pointer to quality data */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  smfArray *lut=NULL;           /* Pointer to LUT at chunk */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  dim_t tstride;               /* Time slice stride in data array */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Obtain pointer to sub-keymap containing PLN parameters. Something will
     always be available.*/
  astMapGet0A( keymap, "PLN", &kmap );

  /* Are we skipping the first iteration? */
  astMapGet0I(kmap, "NOTFIRST", &notfirst);

  if( notfirst && (flags & SMF__DIMM_FIRSTITER) ) {
    msgOutif( MSG__VERB, "", FUNC_NAME
              ": skipping PLN this iteration", status );
    return;
  }

  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  qua = dat->qua[chunk];
  lut = dat->lut[chunk];

  /* Assert ICD-ordered data */
  smf_model_dataOrder( wf, dat, allmodel, chunk, SMF__RES|SMF__QUA|SMF__LUT,
                       1, status );

  smf_get_dims( res->sdata[0],  NULL, NULL, NULL, NULL,
                &ndata, NULL, NULL, status);

  model = allmodel[chunk];

  /* Loop over index in subgrp (subarray) and put the previous iteration
     of the filtered component back into the residual before calculating
     and removing the new filtered component */
  for( idx=0; (*status==SAI__OK)&&(idx<res->ndat); idx++ ) {
    /* Obtain dimensions of the data */

    smf_get_dims( res->sdata[idx],  NULL, NULL, &nbolo, &ntslice,
                  &ndata, &bstride, &tstride, status);

    /* Get pointers to data/quality/model */
    res_data = (res->sdata[idx]->pntr)[0];
    qua_data = (qua->sdata[idx]->pntr)[0];
    model_data = (model->sdata[idx]->pntr)[0];
    lut_data = (lut->sdata[idx]->pntr)[0];

    if( (res_data == NULL) || (model_data == NULL) || (qua_data == NULL) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": Null data in inputs", status);
    } else {

      if( *status == SAI__OK ) {
        /* Place last iteration of plane signal back into residual */
        for (i=0; i< nbolo*ntslice; i++) {
          if ( !(qua_data[i]&SMF__Q_MOD)  && res_data[i] != VAL__BADD && model_data[i] != VAL__BADD ) {
            res_data[i] += model_data[i];
          }
        }

        /* Copy the residual+old model into model_data where it will be
           fitted again in this iteration. */
        memcpy( model_data, res_data,
                ndata*smf_dtype_size(res->sdata[idx],status) );
      }

      /* Calculate the fit and subtract it*/
      smf_subtract_plane3( wf, res->sdata[idx], dat->mdims, lut_data, status );

      /* Store the difference between the plane-subtracted signal and the residual
         in the model container */
      if( *status == SAI__OK ) {
        for (i=0; i< nbolo*ntslice; i++) {
          if ( !(qua_data[i]&SMF__Q_MOD)  && res_data[i] != VAL__BADD && model_data[i] != VAL__BADD ) {
            model_data[i] -= res_data[i];
          } else {
            model_data[i] = VAL__BADD;
          }
        }
      }

    }
  }

  if( kmap ) kmap = astAnnul( kmap );
  model_data_copy = astFree( model_data_copy );
}
