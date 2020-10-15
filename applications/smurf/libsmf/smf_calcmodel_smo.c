/*
*+
*  Name:
*     smf_calcmodel_smo

*  Purpose:
*     Model to smooth the time series foreach bolometer

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
*     Smooth the time series for each bolometer and subtract it from the original.

*  Notes:
*     This model can not (yet) introduce bad samples into the residual model. This
*     is because it only smoothes data that can be fitted and that data can always be
*     subtracted from the input. One difference between this model and others is that
*     SMF__Q_FIT is used throughout rather than using SMF__Q_FIT for fitting the
*     data and SMF__Q_MOD for correcting the data. This is because we do not really
*     fit to the data at all and so can not interpolate over data that have been
*     ignored by the fit. To handle that we would have to fit a function to the
*     smoothed data to bridge the gap.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-05-27 (TIMJ):
*        Initial version
*     2010-06-01 (TIMJ):
*        Simplify logic and use quality bits.
*     2010-06-10 (TIMJ):
*        Fix precedence bug in checking !SMF__Q_MOD
*        Change from smf_boxcar1 to smf_tophat1 because the latter
*        behaves better at the ends (despite being 10 times slower).
*     2010-06-11 (TIMJ):
*        Tweak quality handling one more time
*     2010-06-29 (DSB):
*        - Add option for median smoothing.
*        - Re-structure to use multi-threading.
*     2012-11-27 (DSB):
*        Ensure "res" is assigned before using it.
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

#include <strings.h>

#define FUNC_NAME "smf_calcmodel_smo"

/* Local data types: */
typedef struct smfCalcmodelSmoJobData {
   dim_t b1;
   dim_t b2;
   dim_t nbolo;
   dim_t ntslice;
   double *model_data;
   double *res_data;
   smf_filt_t filter_type;
   dim_t boxcar;
   dim_t bstride;
   dim_t tstride;
   smf_qual_t *qua_data;
} smfCalcmodelSmoJobData;

/* Local prototypes: */
static void smf1_calcmodel_smo_job( void *job_data, int *status );

/* Main entry point. */
void smf_calcmodel_smo( ThrWorkForce *wf, smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap, smfArray **allmodel,
                        int flags __attribute__((unused)),
                        int *status) {

  /* Local Variables */
  dim_t bstride;                /* bolo stride */
  dim_t boxcar = 0;             /* size of boxcar smooth window */
  smf_filt_t filter_type;       /* The type of smoothing to perform */
  dim_t i;                      /* Loop counter */
  dim_t idx=0;                  /* Index within subgroup */
  int iworker;                  /* Owkrer index */
  smfCalcmodelSmoJobData *job_data=NULL; /* Pointer to all job data structures */
  AstKeyMap *kmap=NULL;         /* Pointer to PLN-specific keys */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  double *model_data_copy=NULL; /* Copy of model_data for one bolo */
  dim_t nbolo=0;                /* Number of bolometers */
  dim_t ndata=0;                /* Total number of data points */
  int notfirst=0;               /* flag for delaying until after 1st iter */
  dim_t ntslice=0;              /* Number of time slices */
  int nworker;                  /* No. of worker threads in supplied Workforce */
  smfCalcmodelSmoJobData *pdata=NULL; /* Pointer to current data structure */
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  smf_qual_t *qua_data=NULL;    /* Pointer to quality data */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  dim_t step;                   /* Number of bolometers per thread */
  dim_t tstride;                /* Time slice stride in data array */
  const char * typestr = NULL;  /* smo.type value */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  qua = dat->qua[chunk];

  /* Obtain pointer to sub-keymap containing PLN parameters. Something will
     always be available.*/
  astMapGet0A( keymap, "SMO", &kmap );

  /* Are we skipping the first iteration? */
  astMapGet0I(kmap, "NOTFIRST", &notfirst);

  if( notfirst && (flags & SMF__DIMM_FIRSTITER) ) {
    msgOutif( MSG__VERB, "", FUNC_NAME
              ": skipping SMO this iteration", status );
    return;
  }

  /* Get the boxcar size */
  if( kmap ) smf_get_nsamp( kmap, "BOXCAR", res->sdata[0], &boxcar, status );

  /* Get the type of smoothing filter to use. Anthing that is not "MEDIAN" is mean */
  filter_type = SMF__FILT_MEAN;
  if (astMapGet0C( kmap, "TYPE", &typestr ) ) {
    if (strncasecmp( typestr, "MED", 3 ) == 0 ) {
      filter_type = SMF__FILT_MEDIAN;
    }
  }

  /* Assert bolo-ordered data */
  smf_model_dataOrder( wf, dat, allmodel, chunk, SMF__RES|SMF__QUA,
                       0, status );

  smf_get_dims( res->sdata[0],  NULL, NULL, NULL, &ntslice,
                &ndata, NULL, NULL, status);

  model = allmodel[chunk];

  msgOutiff(MSG__VERB, "",
            "    Calculating smoothed model using boxcar of width %" DIM_T_FMT " time slices",
            status, boxcar);

  /* Create structures used to pass information to the worker threads. */
  nworker = wf ? wf->nworker : 1;
  job_data = astMalloc( nworker*sizeof( *job_data ) );

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

    if( (res_data == NULL) || (model_data == NULL) || (qua_data == NULL) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": Null data in inputs", status);
    } else {

      /* Uncomment to aid debugging */
      /*
      smf_write_smfData( res->sdata[idx], NULL, qua_data, "res_in",
                         NULL, 0, 0, MSG__VERB, 0, NULL, NULL, status );
      */

      if( *status == SAI__OK ) {
        /* Place last iteration back into residual if this is a smoothable section of the time series */
        for (i=0; i< ndata; i++) {
          if ( !(qua_data[i]&SMF__Q_FIT)  && res_data[i] != VAL__BADD && model_data[i] != VAL__BADD ) {
            res_data[i] += model_data[i];
          }
        }
      }

      /* Uncomment to aid debugging */
      /*
      smf_write_smfData( model->sdata[idx], NULL, qua_data, "model_b4",
                         NULL, 0, 0, MSG__VERB, 0, NULL, NULL, status );

      smf_write_smfData( res->sdata[idx], NULL, qua_data, "res_b4",
                         NULL, 0, 0, MSG__VERB, 0, NULL, NULL, status );
      */

      /* Determine which bolometers are to be processed by which threads. */
      step = nbolo/nworker;
      if( step < 1 ) step = 1;

      for( iworker = 0; iworker < nworker; iworker++ ) {
        pdata = job_data + iworker;
        pdata->b1 = iworker*step;
        pdata->b2 = pdata->b1 + step - 1;
      }

      /* Ensure that the last thread picks up any left-over bolometers */
      pdata->b2 = nbolo - 1;

      /* Store all the other info needed by the worker threads, and submit the
         jobs to apply the smoothing. */
      for( iworker = 0; iworker < nworker; iworker++ ) {
         pdata = job_data + iworker;

         pdata->boxcar = boxcar;
         pdata->bstride = bstride;
         pdata->bstride = bstride;
         pdata->filter_type = filter_type;
         pdata->model_data = model_data;
         pdata->nbolo = nbolo;
         pdata->nbolo = nbolo;
         pdata->ntslice = ntslice;
         pdata->ntslice = ntslice;
         pdata->qua_data = qua_data;
         pdata->qua_data = qua_data;
         pdata->res_data = res_data;
         pdata->res_data = res_data;
         pdata->tstride = tstride;
         pdata->tstride = tstride;

         thrAddJob( wf, THR__REPORT_JOB, pdata, smf1_calcmodel_smo_job,
                      0, NULL, status );
      }
      thrWait( wf, status );

      /* Uncomment to aid debugging */
      /*
      smf_write_smfData( res->sdata[idx], NULL, qua_data, "res_af",
                         NULL, 0, 0, MSG__VERB, 0, NULL, NULL, status );
      smf_write_smfData( model->sdata[idx], NULL, qua_data, "model_af",
                         NULL, 0, 0, MSG__VERB, 0, NULL, NULL, status );
      */

    }
  }

  /* Free work space (astFree returns without action if a NULL pointer is
     supplied). */
  model_data_copy = astFree( model_data_copy );
  job_data = astFree( job_data );

  /* Annul AST Object pointers (astAnnul reports an error if a NULL pointer
     is supplied). */
  if( kmap ) kmap = astAnnul( kmap );
}





static void smf1_calcmodel_smo_job( void *job_data, int *status ) {
/*
*  Name:
*     smf1_calcmodel_smo_job

*  Purpose:
*     Smooth each bolometer.

*  Invocation:
*     void smf1_calcmodel_smo_job( void *job_data, int *status )

*  Arguments:
*     job_data = void * (Given)
*        Pointer to the data needed by the job. Should be a pointer to a
*        smfCalcmodelSmoJobData structure.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine runs in a worker thread to smooth the specified bolometer
*     time streams using the specified filter.

*/

/* Local Variables: */
   dim_t b1;
   dim_t b2;
   dim_t i;
   dim_t nbolo;
   dim_t ntslice;
   double *boldata = NULL;
   double *model_data;
   double *res_data;
   double *w1 = NULL;
   dim_t *w3 = NULL;
   smf_filt_t filter_type;
   dim_t boxcar;
   dim_t bstride;
   dim_t tstride;
   dim_t *w2 = NULL;
   smfCalcmodelSmoJobData *pdata;
   smf_qual_t *bolqua = NULL;
   smf_qual_t *qua_data;
   struct timeval tv1;
   struct timeval tv2;

   /* Check inherited status */
   if( *status != SAI__OK ) return;

   /* Get a pointer to the job data, and then extract its contents into a
      set of local variables. */
   pdata = (smfCalcmodelSmoJobData *) job_data;

   b1 = pdata->b1;
   b2 = pdata->b2;
   boxcar = pdata->boxcar;
   bstride = pdata->bstride;
   filter_type = pdata->filter_type;
   model_data = pdata->model_data;
   nbolo = pdata->nbolo;
   ntslice = pdata->ntslice;
   qua_data = pdata->qua_data;
   res_data = pdata->res_data;
   tstride = pdata->tstride;

   /* Check we have something to do. */
   if( b1 < nbolo ) {

      /* Debugging message indicating thread started work */
      msgOutiff( SMF__TIMER_MSG, "", "smf_calcmodel_smo: thread starting on bolos %" DIM_T_FMT
                 " -- %" DIM_T_FMT,
                 status, b1, b2 );
      smf_timerinit( &tv1, &tv2, status);

      /* Allocate work space */
      boldata = astMalloc( ntslice*sizeof( *boldata ) );
      bolqua  = astMalloc( ntslice*sizeof( *bolqua ) );
      if( filter_type == SMF__FILT_MEDIAN ) {
        w1 = astMalloc( boxcar*sizeof( *w1 ) );
        w2 = astMalloc( boxcar*sizeof( *w2 ) );
        w3 = astMalloc( boxcar*sizeof( *w3 ) );
      }

      /* Loop round all the bolometers to be processed by this thread. */
      for( i = b1; i <= b2 && *status == SAI__OK; i++ ) {

        /* Smooth the data and subtract it from res */
        dim_t j;
        dim_t boloff = i*bstride;

        /* Copy the data for this bolometer into a temporary buffer */
        for (j=0; j<ntslice; j++) {
          dim_t thisidx = boloff+j*tstride;
          boldata[j] = res_data[thisidx];
          bolqua[j] = qua_data[thisidx];
        }

        /* Smooth that bolometer time series */
        if( filter_type == SMF__FILT_MEDIAN ) {
           smf_median_smooth( (dim_t) boxcar, SMF__FILT_MEDIAN, 0.0, ntslice,
                              res_data+boloff, qua_data+boloff, 1, SMF__Q_FIT,
                              boldata, w1, w2, w3, status );
        } else {
           smf_tophat1D( boldata, ntslice, boxcar, bolqua, SMF__Q_FIT,
                         0.0, status );
        }

        /* Remove this model from the residual data and copy the data to the model */
        for (j=0; j<ntslice; j++) {
          dim_t thisidx = boloff+j*tstride;

          /* Modify RES if this section has smoothable quality */
          if (! (qua_data[thisidx] & SMF__Q_FIT) ) {
            if (boldata[j] == VAL__BADD) {
              /* Need to set to bad quality but since SMO can not
                 introduce bad values we shouldn't get here. For now
                 we use the COM quality bit just in case. */
              qua_data[thisidx] |= SMF__Q_COM;
            } else if (res_data[thisidx] != VAL__BADD) {
              res_data[thisidx] -= boldata[j];
            }
            /* Store the model */
            model_data[thisidx] = boldata[j];
          } else {
            /* store bad value in the model */
            model_data[thisidx] = VAL__BADD;
          }
        }
      }

/* Free work space. */
      w1 = astFree( w1 );
      w2 = astFree( w2 );
      w3 = astFree( w3 );
      boldata = astFree( boldata );
      bolqua = astFree( bolqua );

/* Report the time taken in this thread. */
      msgOutiff( SMF__TIMER_MSG, "",
                 "smf_calcmodel_smo: thread finishing bolos %" DIM_T_FMT
                 " -- %" DIM_T_FMT "(%.3f sec)",
                 status, b1, b2, smf_timerupdate( &tv1, &tv2, status ) );
   }
}

