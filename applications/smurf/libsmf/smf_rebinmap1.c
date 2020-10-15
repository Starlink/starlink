/*
*+
*  Name:
*     smf_rebinmap1

*  Purpose:
*     Accumulate data directly into a map using a LUT

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_rebinmap1( ThrWorkForce *wf, smfData *data, smfData *variance, int *lut,
*                    dim_t tslice1, dim_t tslice2, int trange, int *whichmap,
*                    dim_t nmap, smf_qual_t mask, int sampvar, int flags,
*                    double *map, double *mapweight, double *mapweightsq,
*                    int *hitsmap, double *mapvar, dim_t msize,
*                    double chunkfactor, double *scalevariance, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to thread workforce.
*     data = smfData* (Given)
*        Pointer to data stream to be re-gridded
*     variance = smfData* (Given)
*        Pointer to smfData containing variance (ignore if NULL pointer). Can
*        be either 2d (one value for each bolo), or 3d (time-varying for
*        each bolo). In the former case ndims should still be 3, but the
*        length of the time dimension should be 0 (e.g. a NOI model component
*        created by smf_model_create).
*     lut = int* (Given)
*        1-d LUT for indices of data points in map (same dimensions as data)
*     tslice1 = dim_t (Given)
*        If tslice2 >= tslice1 and trange set, regrid to tslice1 to tslice2
*     tslice2 = dim_t (Given)
*        If tslice2 >= tslice1 and trange set, regrid to tslice1 to tslice2
*     trange = int (Given)
*        If set, regrid from tslice1 to tslice2
*     whichmap = int * (Given)
*        If set, whichmap is a 1d array for each time slice with an integer
*        index indicating which map the samples at this time slice are
*        associated with. In this case, map, mapweight, mapweightsq, hitsmap,
*        and mapvar are all intepreted as containing nmap sequentially allocated
*        maps of length msize. The starting element for the i'th map indicated
*        by whichmap therefore occurs at map[i*msize].
*     nmap = dim_t (Given)
*        If whichmap is specified, the number of maps being rebinned.
*     mask = smf_qual_t (Given)
*        Use with qual to define which bits in quality are relevant to
*        ignore data in the calculation.
*     int sampvar (Given)
*        If set, calculate mapvar from the (weighted) sample variance of data
*        that land in the pixel. Otherwise a theoretical variance is calculated
*        by propagating the variance on each sample into the pixel.
*     int flags (Given)
*        Flags to control the rebinning process (see astRebin flags)
*     map = double* (Returned)
*        The output map array
*     mapweight = double* (Returned)
*        Relative weighting for each pixel in map.
*     mapweightsq = double* (Returned)
*        Relative weighting squared for each pixel in map.
*     hitsmap = unsigned int* (Returned)
*        Number of samples that land in a pixel.
*     mapvar = double* (Returned)
*        Variance of each pixel in map. May be NULL if no map variances
*        are required.
*     msize = dim_t (Given)
*        Number of pixels in map
*     chunkfactor = double (Given)
*        The calibration correction factor to use for the current chunk.
*        The time-series data is multiplied by this factor before being
*        used. Any supplied time-stream variances are mutiplied by this
*        factor squared before being used.
*     scalevariance = double* (Returned)
*        If sampvar set, calculate average scale factor to be applied
*        to input variances such that error propagation would give the
*        same variance as that calculated from the sample scatter in
*        each pixel.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function does a simple regridding of data into a map. If a
*     variance array is supplied it is used to calculate weights. Optionally
*     return a hitsmap (number of samples that land in a pixel). Data can
*     be directed into multiple different maps by specifying "whichmap",
*     in which case all of the map-sized buffers (map, mapweight, mapweightsq,
*     hitsmap, and mapvar) are assumed to contain nmap contiguous maps.
*     Additionally, all map-sized buffers must contain nw copies of the map so
*     that each thread can accumulate into its own map.
*
*  Authors:
*     EC: Edward Chapin (UBC)
*     AGM: Gaelen Marsden (UBC)
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-05-17 (EC):
*        Initial version.
*     2006-08-16 (EC):
*        Rebin the case that no variance array is given
*     2008-01-22 (EC):
*        Added hitsmap calculation
*     2008-04-03 (EC):
*        - Added QUALITY to interface
*     2008-04-23 (EC):
*        Added sample variance calculation
*     2008-04-29 (EC):
*        Flag map/weight/variance pixels with < SMF__MINSTATSAMP hits as bad
*     2008-07-03 (EC):
*        Use dim_t for dsize/msize
*     2009-07-30 (EC):
*        Use smfDatas for data & variance in preparation for 2d variance arrays
*     2010-04-13 (EC):
*        Add ability to regrid a time range (tslice1, tslice2, trange)
*     2010-05-28 (EC):
*        Keep track of sum(weights^2) for test code using alternative sample
*        variance formula (#define __SMF_REBINMAP__SAMPLE_STANDARD_DEVIATION)
*     2010-09-24 (EC):
*        Add weightnorm to interface
*     2010-11-15 (EC):
*        Add whichmap to interface
*     2011-10-18 (EC):
*        Change to "weighted incremental" from "naive" variance algorithms
*     2012-5-16 (DSB):
*        Use threads.
*     2013-5-28 (DSB):
*        Ensure the "no variances" cases use the same weighted incremental
*        algorithm used by the other cases. Prior to this chance, the "no
*        variances" cases produced unnormalised maps.
*     2013-10-25 (AGM):
*        Alternate method for multithreading. Each thread now operates on a
*        separate chunk of data rather than a seperate chunk of map. Input
*        maps now need to be nw times longer so that each thread can
*        accumulate into its own map.
*     2014-2-13 (DSB):
*        Allow NULL to be supplied for mapvar.
*     2014-5-15 (DSB):
*        Check for bad and negative bolometer variance values, as well as
*        zero values.
*     2014-12-15 (DSB):
*        Do not attempt to run surplus threads when making a map from very
*        few bolometers (smaller than the number of threads).
*     2018-04-10 (DSB):
*        Added parameter "chunkfactor".
*     {enter_further_changes_here}

*  Notes:
*     If the variance map is calculated from the scatter of data in
*     each pixel, rather than using the Gaussian error propagation
*     formula, the expression used is the "weighted sample variance"
*     divided by the number samples used. Naively the calculation
*     would be something like
*
*                  sigma^2 =  sum(w_i)*sum(w_i*x_i^2) - [sum(w_i*x_i)]^2
*                             ------------------------------------------
*                                          N*[sum(w_i)]^2
*
*     Where  sigma^2 = estimated variance on the mean in this pixel
*                w_i = i'th sample weight (1/variance if supplied, 1 otherwise)
*                x_i = i'th data sample
*                  N = number of samples in this pixel
*

*     However, this algorithm can be numerically unstable because of
*     the difference of two potentially very large numbers. Therefore
*     in practice we use something called the "weighted incremental
*     algorithm" to obtain the population variance estimate, and then
*     we divide the final answer by N to get the variance of the
*     mean. The algorithm as described in Wikipedia
*     http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Weighted_incremental_algorithm
*     and attributed to D.H.D. West, 1979, Communications of the ACM,
*     22, 9, 532-535, "Updating Mean and Variance Estimates: An
*     Improved Method" goes like this:
*
*     sumweight = 0
*     mean = 0
*     M2 = 0
*
*     loop over data samples and weights x_i and w_i:
*         temp = sumweight + w_i
*         delta = x_i - mean
*         R = delta * w_i / temp
*         mean = mean + R
*         M2 = M2 + sumweight * delta * R
*         sumweight = temp
*
*     variance_n = M2/sumweight
*     variance = variance_n * N / (N-1)
*
*     Where N is the number of samples, and we set the map pixel value
*     to mean, and its variance to... variance.
*
*     Since changing to this algorithm from the naive one we now calculate
*     an un-used sum "mapweightsq". At some point we should drop it from
*     the interface, but for now it is left in for compatibility.

*  Copyright:
*     Copyright (C) 2006-2011 University of British Columbia
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
#include "star/thr.h"
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"



/* Local data types */
typedef struct smfRebinMap1Data {
   dim_t mbufsize;
   dim_t msize;
   dim_t nbolo;
   dim_t t1;
   dim_t t2;
   dim_t vntslice;
   double *dat;
   double *map;
   double *mapvar;
   double *mapweight;
   double *mapweightsq;
   double *var;
   double chunkfactor;
   double scalevar;
   double scaleweight;
   int *hitsmap;
   int *lut;
   int *whichmap;
   int operation;
   dim_t dbstride;
   dim_t dtstride;
   dim_t p1;
   dim_t p2;
   dim_t vbstride;
   dim_t vtstride;
   smf_qual_t *qual;
   smf_qual_t mask;
   int nw;
   int iw;
} SmfRebinMap1Data;

static void smf1_rebinmap1( void *job_data_ptr, int *status );


#define FUNC_NAME "smf_rebinmap1"

void smf_rebinmap1( ThrWorkForce *wf, smfData *data, smfData *variance, int *lut,
                    dim_t tslice1, dim_t tslice2, int trange, int *whichmap,
                    dim_t nmap, smf_qual_t mask, int sampvar, int flags,
                    double *map, double *mapweight, double *mapweightsq,
                    int *hitsmap, double *mapvar, dim_t msize,
                    double chunkfactor, double *scalevariance, int *status ) {

  /* Local Variables */
  SmfRebinMap1Data *job_data = NULL;
  SmfRebinMap1Data *pdata;
  double *dat=NULL;          /* Pointer to data array */
  dim_t dbstride;           /* bolo stride of data */
  dim_t dtstride;           /* tstride of data */
  int iw;                    /* Thread index */
  dim_t mbufsize;            /* Size of full (multi-map) map buffers */
  dim_t nbolo;               /* number of bolos */
  dim_t ntslice;             /* number of time slices */
  int nw;                    /* Number of worker threads */
  dim_t pixstep;            /* Number of map pixels per thread */
  dim_t bolostep;            /* Number of bolos per thread */
  smf_qual_t * qual = NULL;  /* Quality pointer */
  double scalevar;           /* variance scale factor */
  double scaleweight;        /* weights for calculating scalevar */
  dim_t t1, t2;             /* range of time slices to re-grid */
  double *var=NULL;          /* Pointer to variance array */
  dim_t vbstride;           /* bolo stride of variance */
  dim_t vnbolo;              /* number of bolos in variance */
  dim_t vntslice;            /* number of bolos in variance */
  dim_t vtstride;           /* tstride of variance */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Check inputs */
  if( !data || !map || !lut || !mapweight || !mapweightsq || !hitsmap ) {
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME ": Null inputs", status );
    return;
  }

  if( !data->pntr[0] ) {
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME ": supplied data is empty", status );
    return;
  }

  dat = data->pntr[0];
  qual = smf_select_qualpntr( data, NULL, status );
  smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, NULL, &dbstride,
                &dtstride, status );

  /* Size of full map buffers */
  if( whichmap ) {
    mbufsize = nmap * msize;
  } else {
    mbufsize = msize;
  }

  if( variance ) {
    var = variance->pntr[0];
    smf_get_dims( variance, NULL, NULL, &vnbolo, &vntslice, NULL, &vbstride,
                  &vtstride, status );

    /* Check that the variance dimensions are compatible with data */
    if( (*status==SAI__OK) &&
        ((vnbolo != nbolo) || ((vntslice>1)&&(vntslice!=ntslice))) ) {
      *status = SAI__ERROR;
      errRep(" ", FUNC_NAME ": variance dimensions incompatible with data",
             status );
      return;
    }
  }

  /* Range of time slices to regrid */
  if( trange ) {

    if( tslice2 >= ntslice ) {
      *status = SAI__ERROR;
      errRepf( "", FUNC_NAME ": tslice2 (%zu) can't be >= ntslice (%zu)",
               status, tslice2, ntslice );
      return;
    }

    if( tslice1 > tslice2  ) {
      *status = SAI__ERROR;
      errRepf( "", FUNC_NAME ": tslice1 (%zu) > tslice2 (%zu)",
               status, tslice1, tslice2 );
      return;
    }

    t1 = tslice1;
    t2 = tslice2;
  } else {
    t1 = 0;
    t2 = ntslice-1;
  }

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* If this is the first data to be accumulated zero the arrays */
  if( flags & AST__REBININIT ) {
    memset( map, 0, nw*mbufsize*sizeof(*map) );
    memset( mapweight, 0, nw*mbufsize*sizeof(*mapweight) );
    memset( mapweightsq, 0, nw*mbufsize*sizeof(*mapweightsq) );
    if( mapvar ) memset( mapvar, 0, nw*mbufsize*sizeof(*mapvar) );
    memset( hitsmap, 0, nw*mbufsize*sizeof(*hitsmap) );
  }

  /* Find how many bolos to process in each worker thread. */
  bolostep = nbolo/nw;
  if ( bolostep == 0 ) {
    bolostep = 1;
    nw = (int) nbolo;
  }

  /* Allocate job data for threads, and store the range of bolos to be
     processed by each one. Ensure that the last thread picks up any
     left-over bolos. */
  job_data = astCalloc( nw, sizeof(*job_data) );
  if( *status == SAI__OK ) {
    for( iw = 0; iw < nw; iw++ ) {
      pdata = job_data + iw;
      pdata->p1 = iw*bolostep;
      if( iw < nw - 1 ) {
        pdata->p2 = pdata->p1 + bolostep - 1;
      } else {
        pdata->p2 = nbolo - 1 ;
      }

/* Store other values common to all jobs. */
      pdata->msize = msize;
      pdata->nbolo = nbolo;
      pdata->t1 = t1;
      pdata->t2 = t2;
      pdata->vntslice = vntslice;
      pdata->dat = dat;
      pdata->map = map;
      pdata->mapvar = mapvar;
      pdata->mapweightsq = mapweightsq;
      pdata->mapweight = mapweight;
      pdata->var = var;
      pdata->hitsmap = hitsmap;
      pdata->lut = lut;
      pdata->whichmap = whichmap;
      pdata->dbstride = dbstride;
      pdata->dtstride = dtstride;
      pdata->vbstride = vbstride;
      pdata->vtstride = vtstride;
      pdata->mask = mask;
      pdata->qual = qual;
      pdata->chunkfactor = chunkfactor;
      pdata->mbufsize = mbufsize;
      pdata->nw = nw; /* used for final summing/rescaling */
      pdata->iw = iw; /* so the thread knows with chunk it's working on */
    }
  }

  if( var ) {
    /* Accumulate data and weights in the case that variances are given*/

    if( sampvar ) {

      /* Measure weighted sample variance for varmap */
      if( qual ) {       /* QUALITY checking version */

        /* Set up jobs to add the previous estimate of COM back on to the
           residuals, and then wait for the jobs to complete. These jobs also
           clear any SMF__Q_COM flags set by previous iterations. */
        for( iw = 0; iw < nw; iw++ ) {
           pdata = job_data + iw;
           pdata->operation = 1;
           thrAddJob( wf, 0, pdata, smf1_rebinmap1, 0, NULL, status );
        }
        thrWait( wf, status );

      } else {           /* VAL__BADD checking version */
        for( iw = 0; iw < nw; iw++ ) {
           pdata = job_data + iw;
           pdata->operation = 2;
           thrAddJob( wf, 0, pdata, smf1_rebinmap1, 0, NULL, status );
        }
        thrWait( wf, status );

      }

    } else {
      /* Otherwise use simple error propagation for varmap */

      if( qual ) {       /* QUALITY checking version */
        for( iw = 0; iw < nw; iw++ ) {
           pdata = job_data + iw;
           pdata->operation = 3;
           thrAddJob( wf, 0, pdata, smf1_rebinmap1, 0, NULL, status );
        }
        thrWait( wf, status );

      } else {           /* VAL__BADD checking version */
        for( iw = 0; iw < nw; iw++ ) {
           pdata = job_data + iw;
           pdata->operation = 4;
           thrAddJob( wf, 0, pdata, smf1_rebinmap1, 0, NULL, status );
        }
        thrWait( wf, status );

      }
    }

  } else {
    /* Accumulate data and weights when no variances are given. In this case
       the variance map is always estimated from the sample variance */

    if( qual ) {       /* QUALITY checking version */
      for( iw = 0; iw < nw; iw++ ) {
        pdata = job_data + iw;
        pdata->operation = 5;
        thrAddJob( wf, 0, pdata, smf1_rebinmap1, 0, NULL, status );
      }
      thrWait( wf, status );

    } else {           /* VAL__BADD checking version */
      for( iw = 0; iw < nw; iw++ ) {
        pdata = job_data + iw;
        pdata->operation = 6;
        thrAddJob( wf, 0, pdata, smf1_rebinmap1, 0, NULL, status );
      }
      thrWait( wf, status );
    }
  }

  /* If this is the last data to be accumulated re-normalize */
  if( flags & AST__REBINEND ) {

  /* Find how many buffer pixels to process in each worker thread. */
    pixstep = mbufsize/nw;
    if( pixstep == 0 ) pixstep = 1;

    for( iw = 0; iw < nw; iw++ ) {
      pdata = job_data + iw;
      pdata->p1 = iw*pixstep;
      if( iw < nw - 1 ) {
        pdata->p2 = pdata->p1 + pixstep - 1;
      } else {
        pdata->p2 = mbufsize - 1 ;
      }
    }

    if( sampvar || !var ) {

      for( iw = 0; iw < nw; iw++ ) {
        pdata = job_data + iw;
        pdata->operation = 7;
        thrAddJob( wf, 0, pdata, smf1_rebinmap1, 0, NULL, status );
      }
      thrWait( wf, status );

      scaleweight=0;
      scalevar=0;
      for( iw = 0; iw < nw; iw++ ) {
        pdata = job_data + iw;
        scaleweight += pdata->scaleweight;
        scalevar += pdata->scalevar;
      }

      /* Re-normalize scalevar */
      if( scaleweight ) {
        scalevar /= scaleweight;

        if( scalevariance && mapvar ) {
          *scalevariance = scalevar;
        }
      }

    } else {
      /* Re-normalization for error propagation case */

      for( iw = 0; iw < nw; iw++ ) {
        pdata = job_data + iw;
        pdata->operation = 8;
        thrAddJob( wf, 0, pdata, smf1_rebinmap1, 0, NULL, status );
      }
      thrWait( wf, status );

    }
  }

  job_data = astFree( job_data );

}







static void smf1_rebinmap1( void *job_data_ptr, int *status ) {

/* Local Variables: */
   SmfRebinMap1Data *pdata;
   dim_t di;                  /* data array index */
   dim_t vi;                  /* variance array index */
   double R;                  /* Another temp variable for variance calc */
   double cf2;                /* Squared calibration correction */
   double cf;                 /* Calibration correction */
   double delta;              /* Offset for weighted mean */
   double mapacc;             /* map accumulator */
   double temp;               /* Temporary calculation */
   double thisweight;         /* The weight at this point */
   double varacc;             /* variance accumulator */
   double weightacc;          /* weights accumulator */
   double weightsqacc;        /* weights squared accumulator */
   int hitsacc;               /* hits accumulator */
   int imap;                  /* Submap index */
   dim_t ibolo;              /* Bolometer index */
   dim_t ipix;               /* Map pixel index */
   dim_t itime;              /* Time slice index */
   dim_t tipix;              /* index into sub map */
   dim_t tmap0;              /* index for start of current submap */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfRebinMap1Data *) job_data_ptr;

   cf = pdata->chunkfactor;
   cf2 = cf*cf;

/* Map variance from spread of input values - quality checking version.
   ================================================================== */
   if( pdata->operation == 1 ) {

/* offset into tmaps for this thread */
     tmap0 = pdata->iw * pdata->mbufsize;

/* Loop round bolometer chunk. */
      for( ibolo = pdata->p1; ibolo <= pdata->p2; ibolo++ ) {

/* Skip bad bolometers. */
         if( !( (pdata->qual)[ ibolo*(pdata->dbstride) ] & SMF__Q_BADB ) ) {

/* Loop round all time slices being included in the map. */
            for( itime = pdata->t1; itime <= pdata->t2; itime++ ) {

/* Get the 1D vector index of the data sample. */
               di = ibolo*pdata->dbstride + itime*pdata->dtstride;

/* Get the corresponding map pixel index. */
               ipix = pdata->lut[ di ];

/* Test that pixel falls in map range */
               if(  pdata->lut[ di ] >= 0 && ipix < pdata->msize ) {

/* Get the 1D vector index of the corresponding variance value. */
                  vi = ibolo*pdata->vbstride +
                       ( itime % pdata->vntslice )*pdata->vtstride;

/* Get the offset to the start of the buffer in which to store the pixel
   value. */
                  if( pdata->whichmap ) {
                    if( pdata->whichmap[ itime ] != VAL__BADI ) {
                      ipix += pdata->whichmap[ itime ]*pdata->msize;
                    } else {
                      ipix = SMF__BADDIMT;
                    }
                  }

/* Check that the data and variance values are valid */
                  if( !( pdata->qual[ di ] & pdata->mask ) &&
                       ( pdata->var[ vi ] > 0.0 ) &&
                       ( pdata->var[ vi ] != VAL__BADD ) &&
                       ( ipix != SMF__BADDIMT ) ) {

/* index into (temporary) sub map */
                     tipix = tmap0 + ipix;

/* Update things. */
                     pdata->hitsmap[ tipix ]++;
                     thisweight = 1/(pdata->var[ vi ]*cf2);

/* Weighted incremental algorithm */
                     temp = pdata->mapweight[ tipix ] + thisweight;
                     delta = pdata->dat[ di ]*cf - pdata->map[ tipix ];
                     R = delta * thisweight / temp;
                     pdata->map[ tipix ] += R;
                     if( pdata->mapvar ) pdata->mapvar[ tipix ] +=
                                         pdata->mapweight[ tipix ]*delta*R;
                     pdata->mapweight[ tipix ] = temp;

/* We don't need this sum anymore, but calculate it for consistency with the
   interface */
                     pdata->mapweightsq[ tipix ] += thisweight*thisweight;
                  }
               }
            }
         }
      }


/* Map variance from spread of input values - VAL__BADD checking version.
   ==================================================================== */
   } else if( pdata->operation == 2 ) {

      tmap0 = pdata->iw * pdata->mbufsize;

      for( ibolo = pdata->p1; ibolo <= pdata->p2; ibolo++ ) {
         for( itime = pdata->t1; itime <= pdata->t2; itime++ ) {

            di = ibolo*pdata->dbstride + itime*pdata->dtstride;
            ipix = pdata->lut[ di ];
            if(  pdata->lut[ di ] >= 0 && ipix < pdata->msize ) {
               vi = ibolo*pdata->vbstride +
                    ( itime % pdata->vntslice )*pdata->vtstride;

               if( pdata->whichmap ) {
                 if( pdata->whichmap[ itime ] != VAL__BADI ) {
                   ipix += pdata->whichmap[ itime ]*pdata->msize;
                 } else {
                   ipix = SMF__BADDIMT;
                 }
               }

               if( pdata->dat[ di ] != VAL__BADD &&
                   pdata->var[ vi ] != VAL__BADD &&
                   pdata->var[ vi ] > 0.0 && ipix != SMF__BADDIMT ) {

                  tipix = tmap0 + ipix;

                  pdata->hitsmap[ tipix ]++;
                  thisweight = 1/(pdata->var[ vi ]*cf2);

                  temp = pdata->mapweight[ tipix ] + thisweight;
                  delta = pdata->dat[ di ]*cf - pdata->map[ tipix ];
                  R = delta * thisweight / temp;
                  pdata->map[ tipix ] += R;
                  if( pdata->mapvar ) pdata->mapvar[ tipix ] += pdata->mapweight[ tipix ]*delta*R;
                  pdata->mapweight[ tipix ] = temp;

                  pdata->mapweightsq[ tipix ] += thisweight*thisweight;
               }
            }
         }
      }

/* Map variance from error propagation - quality checking version.
   ============================================================== */
   } else if( pdata->operation == 3 ) {

      tmap0 = pdata->iw * pdata->mbufsize;

      for( ibolo = pdata->p1; ibolo <= pdata->p2; ibolo++ ) {
         if( !( pdata->qual[ ibolo*pdata->dbstride ] & SMF__Q_BADB ) ) {
            for( itime = pdata->t1; itime <= pdata->t2; itime++ ) {
               di = ibolo*pdata->dbstride + itime*pdata->dtstride;
               ipix = pdata->lut[ di ];
               if( pdata->lut[ di ] >= 0 && ipix < pdata->msize ) {
                  vi = ibolo*pdata->vbstride +
                       ( itime % pdata->vntslice )*pdata->vtstride;
                  if( pdata->whichmap ) {
                    if( pdata->whichmap[ itime ] != VAL__BADI ) {
                      ipix += pdata->whichmap[ itime ]*pdata->msize;
                    } else {
                      ipix = SMF__BADDIMT;
                    }
                  }

                  if( !( pdata->qual[ di ] & pdata->mask ) &&
                       ( pdata->var[ vi ] > 0.0 ) &&
                       ( pdata->var[ vi ] != VAL__BADD ) &&
                       ( ipix != SMF__BADDIMT ) ) {

                     tipix = tmap0 + ipix;

                     thisweight = 1/(pdata->var[ vi ]*cf2);
                     pdata->map[ tipix ] += thisweight*pdata->dat[ di ]*cf;
                     pdata->mapweight[ tipix ] += thisweight;
                     pdata->mapweightsq[ tipix ] += thisweight*thisweight;
                     pdata->hitsmap[ tipix ]++;
                  }
               }
            }
         }
      }

/* Map variance from error propagation - VAL__BADD checking version.
   ============================================================== */
   } else if( pdata->operation == 4 ) {

      tmap0 = pdata->iw * pdata->mbufsize;

      for( ibolo = pdata->p1; ibolo <= pdata->p2; ibolo++ ) {
         for( itime = pdata->t1; itime <= pdata->t2; itime++ ) {
            di = ibolo*pdata->dbstride + itime*pdata->dtstride;
            ipix = pdata->lut[ di ];
            if( pdata->lut[ di ] >= 0 && ipix < pdata->msize ) {
               vi = ibolo*pdata->vbstride +
                    ( itime % pdata->vntslice )*pdata->vtstride;
               if( pdata->whichmap ) {
                 if( pdata->whichmap[ itime ] != VAL__BADI ) {
                   ipix += pdata->whichmap[ itime ]*pdata->msize;
                 } else {
                   ipix = SMF__BADDIMT;
                 }
               }

               if( pdata->dat[ di ] != VAL__BADD &&
                   pdata->var[ vi ] != VAL__BADD &&
                   pdata->var[ vi ] > 0.0 && ipix != SMF__BADDIMT  ) {

                  tipix = tmap0 + ipix;

                  thisweight = 1/(pdata->var[ vi ]*cf2);
                  pdata->map[ tipix ] += thisweight*pdata->dat[ di ]*cf;
                  pdata->mapweight[ tipix ] += thisweight;
                  pdata->mapweightsq[ tipix ] += thisweight*thisweight;
                  pdata->hitsmap[ tipix ]++;
               }
            }
         }
      }

/* No variances - quality checking version.
   ========================================= */
   } else if( pdata->operation == 5 ) {

      tmap0 = pdata->iw * pdata->mbufsize;

      for( ibolo = pdata->p1; ibolo <= pdata->p2; ibolo++ ) {
         if( !( pdata->qual[ ibolo*pdata->dbstride ] & SMF__Q_BADB ) ) {
            for( itime = pdata->t1; itime <= pdata->t2; itime++ ) {
               di = ibolo*pdata->dbstride + itime*pdata->dtstride;
               ipix = pdata->lut[ di ];
               if( pdata->lut[ di ] >= 0 && ipix < pdata->msize ) {
                  if( pdata->whichmap ) {
                    if( pdata->whichmap[ itime ] != VAL__BADI ) {
                      ipix += pdata->whichmap[ itime ]*pdata->msize;
                    } else {
                      ipix = SMF__BADDIMT;
                    }
                  }

                  if( !( pdata->qual[ di ] & pdata->mask ) &&
                       ( ipix != SMF__BADDIMT ) ) {
                     tipix = tmap0 + ipix;
                     pdata->hitsmap[ tipix ]++;
                     temp = pdata->mapweight[ tipix ] + 1.0;
                     delta = pdata->dat[ di ]*cf - pdata->map[ tipix ];
                     R = delta / temp;
                     pdata->map[ tipix ] += R;
                     if( pdata->mapvar ) pdata->mapvar[ tipix ] +=
                                         pdata->mapweight[ tipix ]*delta*R;
                     pdata->mapweight[ tipix ] = temp;

/* We don't need this sum anymore, but calculate it for consistency with the
   interface */
                     pdata->mapweightsq[ tipix ]++;
                  }
               }
            }
         }
      }

/* No variances - VAL__BADD checking version.
   ========================================= */
   } else if( pdata->operation == 6 ) {

      tmap0 = pdata->iw * pdata->mbufsize;

      for( ibolo = pdata->p1; ibolo <= pdata->p2; ibolo++ ) {
         for( itime = pdata->t1; itime <= pdata->t2; itime++ ) {
            di = ibolo*pdata->dbstride + itime*pdata->dtstride;
            ipix = pdata->lut[ di ];
            if( pdata->lut[ di ] >= 0 && ipix < pdata->msize ) {
               if( pdata->whichmap ) {
                 if( pdata->whichmap[ itime ] != VAL__BADI ) {
                   ipix += pdata->whichmap[ itime ]*pdata->msize;
                 } else {
                   ipix = SMF__BADDIMT;
                 }
               }

               if( pdata->dat[ di ] != VAL__BADD &&
                   ipix != SMF__BADDIMT ) {
                  tipix = tmap0 + ipix;
                  pdata->hitsmap[ tipix ]++;
                  temp = pdata->mapweight[ tipix ] + 1.0;
                  delta = pdata->dat[ di ]*cf - pdata->map[ tipix ];
                  R = delta / temp;
                  pdata->map[ tipix ] += R;
                  if( pdata->mapvar ) pdata->mapvar[ tipix ] += pdata->mapweight[ tipix ]*delta*R;
                  pdata->mapweight[ tipix ] = temp;

/* We don't need this sum anymore, but calculate it for consistency with the
   interface */
                  pdata->mapweightsq[ tipix ]++;
               }
            }
         }
      }


/* Final normalisation - input sample variance
   ========================================= */
   } else if( pdata->operation == 7 ) {

      pdata->scaleweight=0;
      pdata->scalevar=0;

      for( ipix = pdata->p1; ipix <= pdata->p2; ipix++ ) {

/* sum submaps */
        mapacc = 0.0;
        weightacc = 0.0;
        varacc = 0.0;
        hitsacc = 0;
        weightsqacc = 0.0;

/* loop over nw submaps and accumulate */
        for( imap = 0; imap < pdata->nw; imap++ ) {

          tmap0 = imap * pdata->mbufsize;

          thisweight = pdata->mapweight[ tmap0+ipix ];

          if (thisweight > 0.0) {
            hitsacc += pdata->hitsmap[ tmap0+ipix ];

            temp = thisweight * pdata->map[ tmap0+ipix ];
            mapacc += temp; /* mean accumulator */
            weightacc += thisweight; /* weight accumulator */
            if( pdata->mapvar ) varacc +=
              pdata->mapvar[ tmap0+ipix ] + temp * pdata->map[ tmap0+ipix ];

            weightsqacc += thisweight*thisweight;
          }
        }

        pdata->mapweight[ ipix ] = weightacc;
        pdata->mapweightsq[ ipix ] = weightsqacc;
        pdata->hitsmap[ ipix ] = hitsacc;

        if( weightacc > 0 ) {
          pdata->map[ ipix ] = mapacc / weightacc;
          if( pdata->mapvar ) pdata->mapvar[ ipix ] = varacc - mapacc * mapacc / weightacc;
        }


/* If 0 weight set pixels to bad */
         if( pdata->mapweight[ ipix ] == 0.0 ) {
            pdata->mapweight[ ipix ] = VAL__BADD;
            pdata->map[ ipix ] = VAL__BADD;
            if( pdata->mapvar ) pdata->mapvar[ ipix ] = VAL__BADD;

/* Otherwise re-normalize... although variance only reliable if we had
   enough samples */
         } else if( pdata->hitsmap[ ipix ] >= SMF__MINSTATSAMP ) {
            if( pdata->mapvar ) {
               double variance_n = pdata->mapvar[ ipix ]/pdata->mapweight[ ipix ];
               pdata->mapvar[ ipix ] = variance_n/( (double)(pdata->hitsmap[ ipix ]) - 1 );

/* Work out average scale factor so that supplied weights would produce the
   same map variance estimate as the sample variance calculation that we just
   did. The average value is weighted by number of hits in the pixel to weight
   well-sampled pixels more heavily */
               pdata->scalevar += pdata->hitsmap[ ipix ]*pdata->mapvar[ ipix ]
                                                        *pdata->mapweight[ ipix ];
            }
            pdata->scaleweight += pdata->hitsmap[ ipix ];
         } else {
            if( pdata->mapvar ) pdata->mapvar[ ipix ] = VAL__BADD;
         }
      }

/* Final normalisation - error propagation
   ========================================= */
   } else if( pdata->operation == 8 ) {

      for( ipix = pdata->p1; ipix <= pdata->p2; ipix++ ) {

/* sum submaps */
        mapacc = 0.0;
        weightacc = 0.0;
        hitsacc = 0;
        weightsqacc = 0.0;

/* loop over nw submaps and accumulate */
        for( imap = 0; imap < pdata->nw; imap++ ) {

            tmap0 = imap * pdata->mbufsize;

            thisweight = pdata->mapweight[ tmap0+ipix ];

            if (thisweight > 0.0) {
               hitsacc += pdata->hitsmap[ tmap0+ipix ];
               mapacc += pdata->map[ tmap0+ipix ]; /* mean accumulator */
               weightacc += thisweight; /* weight accumulator */
               weightsqacc += thisweight*thisweight;
            }
         }

         pdata->map[ ipix ] = mapacc;
         pdata->mapweight[ ipix ] = weightacc;
         pdata->mapweightsq[ ipix ] = weightsqacc;
         pdata->hitsmap[ ipix ] = hitsacc;

/* If 0 weight set pixels to bad */
         if( pdata->mapweight[ ipix ] == 0.0 ) {
            pdata->mapweight[ ipix ] = VAL__BADD;
            pdata->mapweightsq[ ipix ] = VAL__BADD;
            pdata->map[ ipix ] = VAL__BADD;
            if( pdata->mapvar ) pdata->mapvar[ ipix ] = VAL__BADD;

/* Otherwise re-normalize */
         } else {
            if( pdata->mapvar ) pdata->mapvar[ ipix ] = 1.0/pdata->mapweight[ ipix ];
            pdata->map[ ipix ] /= pdata->mapweight[ ipix ];
         }
      }

/* Report an error if the worker was to do an unknown job.
   ====================================================== */
   } else {
      *status = SAI__ERROR;
      errRepf( "", "smf1_rebinmap1: Invalid operation (%d) supplied.",
               status, pdata->operation );
   }
}
