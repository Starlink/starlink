/*
*+
*  Name:
*     smf_calcmodel_dks

*  Purpose:
*     Calculate the DarK Squid signal component

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_dks( smfDIMMData *dat, int chunk, AstKeyMap *keymap, 
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
*        Control flags: not used 
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Fit and remove dark squid signal for all detectors in the same column.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-07-10 (EC):
*        Initial Version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2008 University of British Columbia.
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

#define FUNC_NAME "smf_calcmodel_dks"

void smf_calcmodel_dks( smfDIMMData *dat, int chunk, AstKeyMap *keymap, 
			smfArray **allmodel, int flags, int *status) {

  /* Local Variables */
  int boxcar_i=0;               /* width in samples of boxcar filter */ 
  size_t boxcar=0;              /* Size of boxcar smooth window */  
  double *corrbuf=NULL;         /* Array of corr coeffs all bolos in this col */
  double *dksquid=NULL;         /* Pointer to current dark squid */
  double *gainbuf=NULL;         /* Array of gains for all bolos in this col */
  dim_t i;                      /* Loop counter */
  dim_t idx=0;                  /* Index within subgroup */
  dim_t index;                  /* index into data buffer */
  dim_t j;                      /* Loop counter */
  dim_t k;                      /* Loop counter */
  unsigned char mask;           /* quality mask */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  dim_t nbolo=0;                /* Number of bolometers */
  dim_t ncol;                   /* Number of columns */
  dim_t ndata=0;                /* Total number of data points */
  dim_t nrow;                   /* Number of rows */
  dim_t ntslice=0;              /* Number of time slices */
  double *offsetbuf=NULL;       /* Array of offsets for all bolos in this col */
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

  /* Check for dark squid smoothing parameter in the CONFIG file */
  if( astMapGet0I( keymap, "DKS_BOXCAR", &boxcar_i) ) {
    if( boxcar_i >= 0 ) boxcar = (size_t) boxcar_i;
    else {
      *status = SAI__ERROR;
      msgSeti("BOX",boxcar_i);
      errRep("", FUNC_NAME ": DKS_BOXCAR in config file (^BOX) must be >= 0.",
             status); 
    }
  } 

  /* Assert bolo-ordered data */
  for( idx=0; (*status==SAI__OK)&&(idx<res->ndat); idx++ ) {
    smf_dataOrder( res->sdata[idx], 0, status );
    smf_dataOrder( qua->sdata[idx], 0, status );
  }

  /* Loop over index in subgrp (subarray) */
  for( idx=0; (*status==SAI__OK)&&(idx<res->ndat); idx++ ) {

    /* Obtain dimensions of the data */
    smf_get_dims( res->sdata[idx], &nrow, &ncol, &nbolo, &ntslice, &ndata,
                  NULL, NULL, status);

    /* Get pointer to DATA component of residual */
    res_data = (res->sdata[idx]->pntr)[0];

    /* Get pointer to DATA components of the model */
    model_data = (model->sdata[idx]->pntr)[0];

    /* Geta pointer to the QUAlity array */
    qua_data = (qua->sdata[idx]->pntr)[0];

    /* Which QUALITY bits should be checked */
    mask = ~SMF__Q_JUMP; 

    if( (res_data == NULL) || (model_data == NULL) || (qua_data == NULL) ) {
      *status = SAI__ERROR;
      errRep("", FUNC_NAME ": Null data in inputs", status);      
    } else {

      if( flags&SMF__DIMM_FIRSTITER ) {
        msgOutif( MSG__VERB, "","   smoothing dark squids", status );
      } else {
        msgOutif( MSG__VERB, "","   replacing signal from last iteration", 
                  status );
      }

      /* Loop over column */
      for( i=0; (*status==SAI__OK)&&(i<ncol); i++ ) {

        /* get pointer to the dark squid and gain/offset buffers this column */
        dksquid = model_data;
        dksquid += i*(ntslice+nrow*3);
        gainbuf = dksquid + ntslice;
        offsetbuf = gainbuf + nrow;
        corrbuf = offsetbuf + nrow;

        /* Loop over rows */
        for( j=0; (*status==SAI__OK)&&(j<nrow); j++ ) {

          /* Index in data array to start of the bolometer */
          if( SMF__COL_INDEX ) {
            index = i*nrow + j;
          } else {
            index = i + j*ncol;
          }
          index *= ntslice;

          /* Continue if the bolo is OK */
          if( !(qua_data[index]&SMF__Q_BADB) ) {
            
            if( flags&SMF__DIMM_FIRSTITER ) {
              /* For the first iteration we need to smooth the dark squids */
              smf_boxcar1D( dksquid, ntslice, boxcar, NULL, 0, status );
              
            } else if( (gainbuf[j]!=VAL__BADD) && (offsetbuf[j]!=VAL__BADD) ) {
              for( k=0; k<ntslice; k++ ) {
                /* If this isn't first iteration, put the previous iteration
                   back into the signal */                
                if( (res_data[index+k]!=VAL__BADD) && (dksquid[k]!=VAL__BADD)) {
                  res_data[index+k] += dksquid[k]*gainbuf[j] + offsetbuf[j]; 
                }

              }
            }
          }
        }
      }
      /* Then re-fit and remove the dark squid signal */
      msgOutif( MSG__VERB, "", "   cleaning detectors", status );
      smf_clean_dksquid( res->sdata[idx], qua_data, mask, 0, model->sdata[idx],
                         0, 0, status );
    }
  }
}




