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
*     smf_calcmodel_dks( ThrWorkForce *wf, smfDIMMData *dat, int
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
*     Fit and remove dark squid signal for all detectors in the same column.

*  Notes:
*     - Currently buggy... doesn't converge, odd behaviour

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-09-30 (EC):
*        Initial Version
*     2009-04-17 (EC)
*        - switch to subkeymap notation in config file
*     2009-10-01 (EC)
*        Measure normalized change in model between iterations (dchisq)
*     2010-05-14 (EC)
*        Modified initialization of dark squids, remove means.
*     2010-06-14 (EC)
*        -Switch to smf_tophat1 from smf_boxcar1
*        -don't need to remove mean since we now have cleandk.order
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2009 University of British Columbia.
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

#define FUNC_NAME "smf_calcmodel_dks"

void smf_calcmodel_dks( ThrWorkForce *wf __attribute__((unused)),
                        smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap, smfArray **allmodel, int flags,
                        int *status) {

  /* Local Variables */
  dim_t bolcounter;            /* bolometer counter */
  dim_t boxcar=0;               /* Size of boxcar smooth window */
  dim_t bstride;               /* bolo stride */
  double *cdksquid=NULL;        /* Pointer to current dark squid (copy) */
  double *cgainbuf=NULL;        /* Array gains for bolos in this col (copy) */
  double *coffsetbuf=NULL;      /* Array offsets for bolos in this col (copy) */
  double dchisq=0;              /* this - last model residual chi^2 */
  double *dksquid=NULL;         /* Pointer to current dark squid */
  double *gainbuf=NULL;         /* Array of gains for all bolos in this col */
  dim_t i;                      /* Loop counter */
  dim_t idx=0;                  /* Index within subgroup */
  dim_t index;                  /* index into data buffer */
  dim_t j;                      /* Loop counter */
  dim_t jt1;
  dim_t jt2;
  dim_t jf1;                   /* Starting tslice that should be fit */
  dim_t jf2;                   /* Final tslice that should be fit */
  dim_t k;                      /* Loop counter */
  AstKeyMap *kmap=NULL;         /* Local keymap */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  double *model_data_copy=NULL; /* Copy of model_data for one subarray */
  dim_t nbolo=0;                /* Number of bolometers */
  dim_t ncol;                   /* Number of columns */
  dim_t ndata=0;                /* Total number of data points */
  dim_t ndchisq=0;             /* number of elements contributing to dchisq */
  dim_t nmodel=0;               /* Total number of elements in model buffer */
  smfArray *noi=NULL;           /* Pointer to NOI at chunk */
  double *noi_data=NULL;        /* Pointer to DATA component of model */
  dim_t noibstride;            /* bolo stride for noise */
  dim_t nointslice;             /* number of time slices for noise */
  dim_t noitstride;            /* Time stride for noise */
  dim_t nrow;                   /* Number of rows */
  dim_t ntot;                  /* total good excluding padding */
  dim_t ntslice=0;              /* Number of time slices */
  double *offsetbuf=NULL;       /* Array of offsets for all bolos in this col */
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  smf_qual_t *qua_data=NULL; /* Pointer to quality data */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  dim_t tstride;               /* time stride */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Obtain pointer to sub-keymap containing COM parameters */
  if( !astMapGet0A( keymap, "DKS", &kmap ) ) {
    kmap = NULL;
  }

  /* Assert bolo-ordered data */
  smf_model_dataOrder( wf, dat, NULL, chunk, SMF__RES|SMF__QUA|SMF__NOI, 0, status );

  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  qua = dat->qua[chunk];
  model = allmodel[chunk];

  if(dat->noi) {
    noi = dat->noi[chunk];

    nmodel = (model->sdata[0])->dims[0] * (model->sdata[0])->dims[1];

    model_data_copy = astCalloc( nmodel, sizeof(*model_data_copy) );
  }

  /* Check for dark squid smoothing parameter in the CONFIG file */
  if( kmap ) smf_get_nsamp( kmap, "BOXCAR", res->sdata[0], &boxcar, status );

  /* Loop over index in subgrp (subarray) */
  for( idx=0; (*status==SAI__OK)&&(idx<res->ndat); idx++ ) {

    /* Obtain dimensions of the data */
    smf_get_dims( res->sdata[idx], &nrow, &ncol, &nbolo, &ntslice, &ndata,
                  &bstride, &tstride, status);

    /* Get pointer to DATA component of residual */
    res_data = (res->sdata[idx]->pntr)[0];

    /* Get pointer to DATA components of the model */
    model_data = (model->sdata[idx]->pntr)[0];

    /* Geta pointer to the QUAlity array */
    qua_data = (qua->sdata[idx]->pntr)[0];

    /* Identify the range of data that should be fit using SMF__Q_BOUND */
    if( qua ) {
      smf_get_goodrange( qua_data, ntslice, tstride, SMF__Q_BOUND, &jf1, &jf2,
                         status );
    } else {
      jf1 = 0;
      jf2 = ntslice-1;
    }

    /* Total total range only using SMF__Q_PAD */
    if( qua ) {
      smf_get_goodrange( qua_data, ntslice, tstride, SMF__Q_PAD, &jt1, &jt2,
                         status );
    } else {
      jt1 = 0;
      jt2 = ntslice-1;
    }
    ntot = jt2-jt1+1;

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

        /* For the first iteration we need to do some pre-processing */
        if( (flags&SMF__DIMM_FIRSTITER) ) {
          /* boxcar smooth */
          smf_tophat1D( &dksquid[jt1], ntot, boxcar, NULL, 0, 0.0,
                        status );
        }

        /* Loop over rows */
        for( j=0; (*status==SAI__OK)&&(j<nrow); j++ ) {

          /* Index in data array to start of the bolometer */
          if( SC2STORE__COL_INDEX ) {
            index = i*nrow + j;
          } else {
            index = i + j*ncol;
          }
          index *= ntslice;

          /* Continue if the bolo is OK */
          if( !(qua_data[index]&SMF__Q_BADB) ) {

            /* If this isn't first iteration, put the previous iteration
               back into the signal */

            if( !(flags&SMF__DIMM_FIRSTITER) && (gainbuf[j]!=VAL__BADD) &&
                (offsetbuf[j]!=VAL__BADD) ) {
              for( k=jf1; k<=jf2; k++ ) {
                if( (res_data[index+k]!=VAL__BADD) && (dksquid[k]!=VAL__BADD)) {
                  res_data[index+k] += dksquid[k]*gainbuf[j] + offsetbuf[j];
                }
              }
            }
          }
        }
      }

      /* Make a copy of the model to check convergence */
      if( noi ) {
        memcpy( model_data_copy, model_data, nmodel*sizeof(*model_data_copy) );
      }

      /* Then re-fit and remove the dark squid signal */
      msgOutif( MSG__VERB, "", "   cleaning detectors", status );
      smf_clean_dksquid( res->sdata[idx], SMF__Q_MOD, 0,
                         model->sdata[idx], 0, 0, 0, status );

      /* How has the model changed? */
      if( noi ) {

        smf_get_dims( noi->sdata[idx],  NULL, NULL, NULL, &nointslice,
                      NULL, &noibstride, &noitstride, status);
        noi_data = (double *)(noi->sdata[idx]->pntr)[0];

        /* Loop over column */
        bolcounter = 0;
        for( i=0; (*status==SAI__OK)&&(i<ncol); i++ ) {

          /* get pointer to the dark squid and gain/offset buffers this col */
          dksquid = model_data;
          dksquid += i*(ntslice+nrow*3);
          gainbuf = dksquid + ntslice;
          offsetbuf = gainbuf + nrow;

          cdksquid = model_data_copy;
          cdksquid += i*(ntslice+nrow*3);
          cgainbuf = cdksquid + ntslice;
          coffsetbuf = cgainbuf + nrow;

          /* Loop over rows */
          for( j=0; (*status==SAI__OK)&&(j<nrow); j++ ) {

            /* Index in data array to start of the bolometer */
            if( SC2STORE__COL_INDEX ) {
              index = i*nrow + j;
            } else {
              index = i + j*ncol;
            }
            index *= ntslice;

            /* Continue if the bolo is OK */
            if( !(qua_data[index]&SMF__Q_BADB) && (gainbuf[j]!=VAL__BADD) &&
                (offsetbuf[j]!=VAL__BADD) ) {

              for( k=jf1; k<=jf2; k++ ) {

                if( !(qua_data[bolcounter*bstride+k*tstride]&SMF__Q_GOOD) ) {
                  dchisq += ( (dksquid[k]*gainbuf[j] + offsetbuf[j]) -
                              (cdksquid[k]*cgainbuf[j] + coffsetbuf[j])) *
                    ( (dksquid[k]*gainbuf[j] + offsetbuf[j]) -
                      (cdksquid[k]*cgainbuf[j] + coffsetbuf[j]) ) /
                    noi_data[bolcounter*noibstride + (k%nointslice)*noitstride];

                  ndchisq++;
                }
              }
            }

            bolcounter++;
          }
        }
      }
    }
  }

  /* Print normalized residual chisq for this model */
  if( (*status==SAI__OK) && noi && (ndchisq>0) ) {
    dchisq /= (double) ndchisq;
    msgOutiff( MSG__VERB, "", "    normalized change in model: %lf", status,
               dchisq );
  }

  if( kmap ) kmap = astAnnul( kmap );
  model_data_copy = astFree( model_data_copy );
}
