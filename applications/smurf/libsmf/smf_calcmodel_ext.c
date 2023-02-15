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
*     smf_calcmodel_ext( ThrWorkForce *wf, smfDIMMData *dat, int
*                        chunk, AstKeyMap *keymap, smfArray
*                        **allmodel, int flags, int *status)

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
*     2011-04-15 (TIMJ):
*        Set quality to SMF__Q_EXT if there is a bad extinction correction
*        value.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     Copyright (C) 2005-2008 Particle Physics and Astronomy Research Council.
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

/* Prototypes for local static functions. */
static void smf1_calcmodel_ext( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfCalcModelExtData {
   double *model_data;
   double *res_data;
   int flags;
   dim_t d1;
   dim_t d2;
   smf_qual_t *qua_data;
} SmfCalcModelExtData;

#define FUNC_NAME "smf_calcmodel_ext"

void smf_calcmodel_ext( ThrWorkForce *wf,
                        smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap __attribute__((unused)),
                        smfArray **allmodel, int flags, int *status) {

  /* Local Variables */
  int iw;                       /* Thread index */
  dim_t idx=0;                  /* Index within subgroup */
  SmfCalcModelExtData *job_data = NULL;  /* Array of job descriptions */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  dim_t ndata;                  /* Number of data points */
  int nw;                       /* Number of worker threads */
  SmfCalcModelExtData *pdata;   /* Pointer to next job description */
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  smf_qual_t *qua_data=NULL; /* Pointer to quality data */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  dim_t sampstep;              /* Samples per worker thread */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  qua = dat->qua[chunk];
  model = allmodel[chunk];

  /* Ensure everything is in bolo-order */
  smf_model_dataOrder( wf, dat, allmodel, chunk, SMF__RES|SMF__QUA, 0, status);

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Allocate job data for threads. */
  job_data = astCalloc( nw, sizeof(*job_data) );

  /* Loop over index in subgrp (subarray) */
  for( idx=0; idx<res->ndat && *status == SAI__OK; idx++ ) {

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

      /* Find how many samples to process in each worker thread. */
      sampstep = ndata/nw;
      if( sampstep == 0 ) {
         sampstep = 1;
         nw = (int) ndata;
      }

      /* Set up information describing the job to be done by each worker
         thread. */
      for( iw = 0; iw < nw; iw++ ) {
        pdata = job_data + iw;
        pdata->d1 = iw*sampstep;
        if( iw < nw - 1 ) {
          pdata->d2 = pdata->d1 + sampstep - 1;
        } else {
          pdata->d2 = ndata - 1 ;
        }

        pdata->model_data = model_data;
        pdata->res_data = res_data;
        pdata->qua_data = qua_data;
        pdata->flags = flags;

        /* Submit the job to the workforce. */
        thrAddJob( wf, 0, pdata, smf1_calcmodel_ext, 0, NULL, status );
      }

      /* Wait for all jobs to complete. */
      thrWait( wf, status );
    }
  }

  /* Free the job data. */
  job_data = astFree( job_data );
}





static void smf1_calcmodel_ext( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_calcmodel_ext

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_calcmodel_ext.

*  Invocation:
*     smf1_calcmodel_ext( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfCalcModelExtData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfCalcModelExtData *pdata;
   double *pm;
   double *pr;
   dim_t idata;
   smf_qual_t *pq;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfCalcModelExtData *) job_data_ptr;

/* Pointer to the first quality, model and residual value to be processed
   by this thread. */
   pq = pdata->qua_data + pdata->d1;
   pr = pdata->res_data + pdata->d1;
   pm = pdata->model_data + pdata->d1;

/* Apply the extinction correction */
   if( !( pdata->flags & SMF__DIMM_INVERT ) ) {

/* Loop over all data samples being processed by this thread. */
      for( idata = pdata->d1; idata <= pdata->d2; idata++,pq++,pr++,pm++ ) {

/* If the sample is not flagged and the extinction is good, apply the
   extinction factor. Otherwise, ensure the sample is flagged. */
         if( !( *pq & SMF__Q_MOD ) ) {
            if( *pm == VAL__BADD ) {
               *pq |= SMF__Q_EXT;
            } else {
               *pr *= *pm;
            }

         } else if( *pm == VAL__BADD ) {
            *pq |= SMF__Q_EXT;
         }
      }

/* Undo the extinction correction */
   } else {
      for( idata = pdata->d1; idata <= pdata->d2; idata++,pq++,pr++,pm++ ) {
         if( !(*pq & SMF__Q_MOD) && *pm > 0 ) {
            if( *pm != VAL__BADD ) *pr /= *pm;
         }
      }
   }
}


