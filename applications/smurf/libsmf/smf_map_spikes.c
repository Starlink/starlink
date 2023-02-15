/*
*+
*  Name:
*     smf_map_spikes

*  Purpose:
*     Flag outlier data points that land in each pixel of the map

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_map_spikes( ThrWorkForce *wf, smfData *data, smfData *variance,
*                     int *lut, smf_qual_t mask, double *map,
*                     double *mapweight, int *hitsmap, double *mapvar,
*                     double thresh, dim_t *nflagged, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to thread workforce.
*     data = smfData* (Given)
*        Pointer to data stream to be flagged. Quality will be flagged
*        with SMF__Q_SPIKE.
*     variance = smfData* (Given) Pointer to smfData containing
*        variance. Can be either 2d (one value for each bolo), or 3d
*        (time-varying for each bolo). In the former case ndims should
*        still be 3, but the length of the time dimension should be 0
*        (e.g. a NOI model component created by smf_model_create).
*     lut = int* (Given)
*        1-d LUT for indices of data points in map (same dimensions as data)
*     mask = smf_qual_t (Given)
*        Define which bits in quality are relevant to ignore data in
*        the calculation.
*     map = double* (Given)
*        The current map estimate (can be NULL).
*     mapweight = double* (Returned)
*        Relative weighting for each pixel in map.
*     hitsmap = int* (Given)
*        Number of samples that land in a pixel.
*     mapvar = double* (Given)
*        Variance of each pixel in map
*     thresh = doublge (Given)
*        N-sigma threshold for spike detection
*     nflagged = dim_t * (Returned)
*        The number of new samples that were flagged. May be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine flags data points that are thresh-sigma away from
*     the value of the map in each pixel. After running smf_rebinmap1,
*     we have an estimated uncertainty in the weighted mean of each
*     map pixel, sigma_m, and we also know the number of hits,
*     N. Therefore we can estimate the population variance of weighted
*     data points that went into each map pixel as
*
*                          sigma^2 = N *sigma_m^2
*
*     The population we are considering are weighted data points,
*     which are simply the differences between the samples that went
*     into the average, and the average itself (the map) multiplied by
*     the weight (1 / bolometer noise^2) and some normalization
*     factor. We simply check to see if the difference between these
*     weighted data points and the map exceeds the requested threshold
*     beyond the population variance.
*
*     If map is NULL, it is assumed that the data have already had the
*     map value subtracted, and are assumed to be scattered about zero.

*  Authors:
*     EC: Edward Chapin (UBC)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2009-08-06 (EC):
*        Initial version.
*     2010-09-24 (EC):
*        Finish and get it working properly.
*     2011-03-09 (EC):
*        Instead of using an average weightnorm, now calculate weighted
*        sample normalization on per-pixel basis using mapweight
*     2012-05-22 (DSB):
*        Multi-thread.

*  Copyright:
*     Copyright (C) 2009-2011 University of British Columbia
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

#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Prototypes for local static functions. */
static void smf1_map_spikes( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfMapSpikesData {
   dim_t b1;
   dim_t b2;
   dim_t ntslice;
   dim_t vntslice;
   double *dat_data;
   double *map;
   double *mapvar;
   double *mapweight;
   double *var_data;
   double threshsq;
   int *hitsmap;
   int *lut_data;
   int nflag;
   dim_t bstride;
   dim_t tstride;
   dim_t vbstride;
   dim_t vtstride;
   smf_qual_t *qua_data;
   smf_qual_t mask;
} SmfMapSpikesData;




#define FUNC_NAME "smf_map_spikes"

void smf_map_spikes( ThrWorkForce *wf, smfData *data, smfData *variance,
                     int *lut, smf_qual_t mask, double *map, double *mapweight,
                     int *hitsmap, double *mapvar, double thresh,
                     dim_t *nflagged, int *status ) {

  /* Local Variables */
  double *dat=NULL;          /* Pointer to data array */
  dim_t bolostep;            /* Bolos per worker thread */
  dim_t bstride;            /* bolo stride of data */
  dim_t dsize;               /* total number of elements in data */
  dim_t tstride;            /* tstride of data */
  int iw;                    /* Thread index */
  dim_t nbolo;               /* number of bolos */
  dim_t nflag=0;            /* Number of samples flagged */
  dim_t ntslice;             /* number of time slices */
  int nw;                    /* Number of worker threads */
  smf_qual_t * qual = NULL;  /* Quality to update for flagging */
  double threshsq;           /* square of thresh */
  double *var=NULL;          /* Pointer to variance array */
  dim_t vbstride;           /* bolo stride of variance */
  dim_t vnbolo;              /* number of bolos in variance */
  dim_t vntslice;            /* number of bolos in variance */
  dim_t vtstride;           /* tstride of variance */
  SmfMapSpikesData *job_data = NULL; /* Data for jobs */
  SmfMapSpikesData *pdata;   /* Data for job */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Check inputs */
  if( !data || !variance || !lut || !mapvar || !hitsmap || !mapweight ) {
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME ": Null inputs", status );
    return;
  }

  if( (!data->pntr[0]) || (!variance->pntr[0]) ) {
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME ": supplied data or variance is empty", status );
    return;
  }

  dat = data->pntr[0];
  qual = smf_select_qualpntr( data, NULL, status );
  smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, &dsize, &bstride,
                &tstride, status );

  var = variance->pntr[0];
  smf_get_dims( variance, NULL, NULL, &vnbolo, &vntslice, NULL, &vbstride,
                &vtstride, status );

  if( thresh <= 0 ) {
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME ": thresh must be > 0!", status );
    return;
  }

  threshsq = thresh*thresh;

  /* Check that the variance dimensions are compatible with data */
  if( (vnbolo != nbolo) || ( (vntslice>1) && (vntslice!=ntslice) ) ) {
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME ": variance dimensions incompatible with data",
           status );
    return;
  }

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Find how many bolometers to process in each worker thread. */
  bolostep = nbolo/nw;
  if( bolostep == 0 ) bolostep = 1;

  /* Allocate job data for threads, and store the range of bolos to be
     processed by each one. Ensure that the last thread picks up any
     left-over bolos. */
  job_data = astCalloc( nw, sizeof(*job_data) );
  if( *status == SAI__OK ) {

    for( iw = 0; iw < nw; iw++ ) {
       pdata = job_data + iw;
       pdata->b1 = iw*bolostep;
       if( iw < nw - 1 ) {
          pdata->b2 = pdata->b1 + bolostep - 1;
       } else {
          pdata->b2 = nbolo - 1 ;
       }

       /* Store other values common to all jobs. */
       pdata->ntslice = ntslice;
       pdata->vntslice = vntslice;
       pdata->qua_data = qual;
       pdata->dat_data = dat;
       pdata->var_data = var;
       pdata->bstride = bstride;
       pdata->tstride = tstride;
       pdata->vbstride = vbstride;
       pdata->vtstride = vtstride;
       pdata->map = map;
       pdata->mapvar = mapvar;
       pdata->mapweight = mapweight;
       pdata->threshsq = threshsq;
       pdata->hitsmap = hitsmap;
       pdata->lut_data = lut;
       pdata->mask = mask;

       /* Submit the job to the workforce. */
       thrAddJob( wf, 0, pdata, smf1_map_spikes, 0, NULL, status );
    }

    /* Wait for all jobs to complete. */
    thrWait( wf, status );

    /* Accumulate the results from all the worker threads. */
    for( iw = 0; iw < nw; iw++ ) {
       pdata = job_data + iw;
       nflag += pdata->nflag;
    }

    /* Free the job data. */
    job_data = astFree( job_data );
  }

  /* Return nflagged if requested */
  if( nflagged ) *nflagged = nflag;
}









static void smf1_map_spikes( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_map_spikes

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_map_spikes.

*  Invocation:
*     smf1_map_spikes( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfMapSpikesData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfMapSpikesData *pdata;
   dim_t ibolo;
   dim_t itime;
   double *pd;
   double *pv;
   double popvar;
   double thisweight;
   double woffsq;
   int *pl;
   dim_t ibase;
   dim_t vi;
   smf_qual_t *pq;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfMapSpikesData *) job_data_ptr;

/* Initialise returned values. */
   pdata->nflag = 0;

/* Loop round all bolos to be processed by this thread, maintaining the
   index of the first time slice for the current bolo. */
   ibase = pdata->b1*pdata->bstride;
   for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {

/* Get a pointer ot the first quality value for the current bolo, and
   check that the whole bolometer has not been flagged as bad. */
      pq = pdata->qua_data + ibase;
      if( !( *pq & SMF__Q_BADB ) ) {

/* Get a pointer to the first residual for the current bolo, and then
   loop round all time slices. */
         pd = pdata->dat_data + ibase;
         pv = pdata->var_data + ibolo*pdata->vbstride;
         pl = pdata->lut_data + ibase;
         for( itime = 0; itime < pdata->ntslice; itime++ ) {

/* Check the sample falls on the map, that it is not flagged, and has a
   non-zero variance. */
            vi = ( itime % pdata->vntslice )*pdata->vtstride;
            if( *pl != VAL__BADI && !( *pq & pdata->mask ) && pv[ vi ] != 0 &&
                pdata->mapvar[ *pl ] != VAL__BADD ) {

/* What is the estimated population variance? */
               popvar = pdata->mapvar[ *pl ]*pdata->hitsmap[ *pl ];

/* Estimate the weighted sample offset (squared) from the mean. This
   definition is slightly odd, but I arrived at it by figuring out the
   factor you need to multiply (data - map) by in order to plug it
   straight into the sum_i (residual_i)^2 / N for the variance and get
   the same answer as the population variance. */
               thisweight = 1.0 / pv[ vi ];
               woffsq = pdata->map ? *pd - pdata->map[ *pl ] : *pd;
               woffsq *= woffsq * thisweight * pdata->hitsmap[ *pl ] /
                         pdata->mapweight[ *pl ];

/* Flag it if it's an outlier */
               if( woffsq > pdata->threshsq*popvar ) {
                  *pq |= SMF__Q_SPIKE;
                  pdata->nflag++;
               }
            }

/* Move data, quality and LUT pointers on to the next time slice. */
            pd += pdata->tstride;
            pq += pdata->tstride;
            pl += pdata->tstride;
         }
      }

/* Increment the index of the first value associated with the next
   bolometer. */
      ibase += pdata->bstride;
   }
}







