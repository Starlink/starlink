/*
*+
*  Name:
*     smf_calcmodel_gai

*  Purpose:
*     Calculate the GAIn (and offset) model

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_gai( smfWorkForce *wf, smfDIMMData *dat, int
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
*     Presently this is mostly a dummy routine since the actual calculation of
*     this model occurs within smf_calcmodel_com. With the SMF__DIMM_INVERT
*     flag, however, the gain correction applied in smf_calcmodel_com is
*     undone to prepare for the next iteration.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-12-11 (EC):
*        Initial Version
*     2008-12-12 (EC):
*        Add inverse capability
*     {enter_further_changes_here}


*  Copyright:
*     Copyright (C) 2008 University of British Columbia.
*
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

#define FUNC_NAME "smf_calcmodel_gai"

void smf_calcmodel_gai( smfWorkForce *wf, smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap __attribute__((unused)),
                        smfArray **allmodel, int flags, int *status) {

  /* Local Variables */
  size_t bstride;               /* bolometer stride */
  size_t gbstride;              /* GAIn bolo stride */
  size_t gcstride;              /* GAIn coeff stride */
  dim_t i;                      /* Loop counter */
  dim_t idx=0;                  /* Index within subgroup */
  dim_t j;                      /* Loop counter */
  unsigned char mask;           /* Bitmask for quality */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Number of data points */
  dim_t ntslice;                /* Number of time slices */
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  unsigned char *qua_data=NULL; /* Pointer to quality data */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DAT */
  double scale;                 /* Scale factor */
  size_t tstride;               /* time slice stride */

  /* Main routine */
  if( *status != SAI__OK ) return;
  if( !(flags&SMF__DIMM_INVERT) ) return;

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
      errRep("", FUNC_NAME ": Null data in inputs", status);
    } else {

      /* Get the raw data dimensions */
      smf_get_dims( res->sdata[idx],  NULL, NULL, &nbolo, &ntslice, &ndata,
                    &bstride, &tstride, status);

      smf_get_dims( model->sdata[idx],  NULL, NULL, NULL, NULL, NULL,
                    &gbstride, &gcstride, status);

      /* Which QUALITY bits should be considered for correcting data (must
       match mask_cor in smf_calcmodel_com!) */
      mask = ~(SMF__Q_JUMP|SMF__Q_SPIKE|SMF__Q_APOD|SMF__Q_STAT);

      /* Undo the gain correction stored in GAI */
      for( i=0; i<nbolo; i++ ) {
        if( !(qua_data[i*bstride]&SMF__Q_BADB) && (model_data[i*gbstride]>0) ) {
          scale = 1./model_data[i*gbstride];
          for( j=0; j<ntslice; j++ ) {
            if( !(qua_data[i*bstride + j*tstride]&mask) ) {
              res_data[i*bstride + j*tstride] *= scale;
            }
          }
        }
      }
    }
  }
}
