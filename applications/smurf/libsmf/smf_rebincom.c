/*
*+
*  Name:
*     smf_rebincom

*  Purpose:
*     Accumulate COM/GAI data directly into a map using a LUT

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_rebincom( ThrWorkForce *wf, smfData *comdata, smfData *gaidata,
*                   smfData *vardata, smf_qual_t *qual, smfData *lutdata,
*                   int flags, double *map, double *mapwgt, double *mapwgt2,
*                   double *mapvar, dim_t msize, dim_t gain_box, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to thread workforce.
*     comdata = smfData * (Given)
*        Pointer to COM data stream to be re-gridded
*     gaidata = smfData * (Given)
*        Pointer to GAI data stream to be re-gridded. May be NULL.
*     vardata = smfData* (Given)
*        Pointer to smfData containing variance (ignore if NULL pointer). Can
*        be either 2d (one value for each bolo), or 3d (time-varying for
*        each bolo). In the former case ndims should still be 3, but the
*        length of the time dimension should be 0 (e.g. a NOI model component
*        created by smf_model_create).
*     qual = smf_qual_t * (Given)
*        Pointer to quality array to use.
*     lutdata = smfData * (Given)
*        1-d LUT for indices of data points in map.
*     int flags (Given)
*        Flags to control the rebinning process (see astRebin flags)
*     map = double * (Returned)
*        The output map array. The supplied array should have "nw*msize"
*        elements, where "nw" is the number of worker threads in the supplied
*        workforce. The final map is returned in the first "msize" elements.
*     mapwgt = double * (Returned)
*        Relative weighting for each pixel in map. The supplied array should
*        have "nw*msize" elements, where "nw" is the number of worker threads
*        in the supplied workforce. The final map is returned in the first
*        "msize" elements.
*     mapwgt2 = double * (Returned)
*        Relative weighting squared for each pixel in map. The supplied array
*        should have "nw*msize" elements, where "nw" is the number of worker
*        threads in the supplied workforce. The final map is returned in the
*        first "msize" elements.
*     mapvar = double * (Returned)
*        Variance of each pixel in map. The supplied array should have
*        "nw*msize" elements, where "nw" is the number of worker threads in
*        the supplied workforce. The final map is returned in the first
*        "msize" elements.
*     msize = dim_t (Given)
*        Number of pixels in the output map.
*     gain_box = dim_t (Given)
*        The number of adjacent time slices for which the same GAI values
*        are used.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function expands the 1D COM model out to a 3D model using the
*     GAI model to determine the scale factor for each bolometer at each time
*     slice and regrids the 3D model into a map.

*  Authors:
*     DSB: David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     26-MAY-2021 (DSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2021 East Asian Observatory
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
#include "sae_par.h"
#include "star/thr.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Prototypes for local static functions. */
static void smf1_rebincom( void *job_data_ptr, int *status );

/* Local data types */
typedef struct SmfRebinComData {
   dim_t gain_box;
   dim_t msize;
   dim_t nbolo;
   dim_t ntslice;
   dim_t p1;
   dim_t p2;
   dim_t t1;
   dim_t t2;
   double *com;
   double *gai;
   double *map;
   double *mapvar;
   double *mapwgt;
   double *mapwgt2;
   int *lut;
   int nblock;
   int nw;
   int oper;
   size_t bstride;
   size_t gbstride;
   size_t gcstride;
   size_t tstride;
   smf_qual_t *qual;
   dim_t vntslice;
   size_t vbstride;
   size_t vtstride;
   double *var;
} SmfRebinComData;

void smf_rebincom( ThrWorkForce *wf, smfData *comdata, smfData *gaidata,
                   smfData *vardata, smf_qual_t *qual, smfData *lutdata,
                   int flags, double *map, double *mapwgt, double *mapwgt2,
                   double *mapvar, dim_t msize, dim_t gain_box, int *status ){

/* Local Variables */
   SmfRebinComData *job_data = NULL;
   SmfRebinComData *pdata;
   dim_t nbolo;
   dim_t ntslice;
   dim_t step;
   double *com=NULL;
   double *gai=NULL;
   int *lut;
   int iw;
   int nblock;
   int nw;
   size_t bstride;
   size_t gbstride;
   size_t gcstride;
   size_t pixstep;
   size_t tstride;
   size_t vbstride;
   dim_t vnbolo;
   dim_t vntslice;
   size_t vtstride;
   double *var;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Check inputs */
   if( !comdata || !map || !mapvar || !lutdata || !mapwgt || !mapwgt2 ) {
     *status = SAI__ERROR;
     errRep(" ", "smf_rebincom: Null inputs", status );
     return;
   }

   if( !comdata->pntr[0] || (gaidata && !gaidata->pntr[0]) ) {
     *status = SAI__ERROR;
     errRep(" ", "smf_rebincom: supplied data is empty", status );
     return;
   }

/* Get pointers to the com, lut and gai arrays to use. */
   com = comdata->pntr[0];
   lut = lutdata->pntr[0];
   gai = gaidata ? gaidata->pntr[0] : NULL;

/* Get the dimensions and strides of the data array etc (use LUT since COM
   is 1D). */
   smf_get_dims( lutdata, NULL, NULL, &nbolo, &ntslice, NULL, &bstride,
                 &tstride, status );

/* Get info about variances, if supplied. */
   if( vardata ) {
      var = vardata->pntr[0];
      smf_get_dims( vardata, NULL, NULL, &vnbolo, &vntslice, NULL, &vbstride,
                    &vtstride, status );

/* Check that the variance dimensions are compatible with data */
      if( (*status==SAI__OK) &&
          ((vnbolo != nbolo) || ((vntslice>1)&&(vntslice!=ntslice))) ) {
         *status = SAI__ERROR;
         errRep(" ", "smf_rebincom: variance dimensions incompatible with data",
                status );
      }
   } else {
      var = NULL;
      vbstride = 0;
      vnbolo = 0;
      vntslice = 0;
      vtstride = 0;
   }

/* Get the strides in the GAI model. */
   if( gaidata ) {
      smf_get_dims( gaidata,  NULL, NULL, NULL, NULL, NULL, &gbstride,
                    &gcstride, status );
   } else {
      gbstride = 0;
      gcstride = 0;
   }

/* If gain_box is zero, use a value of ntslice, so that a single box
   will be used covering the whole time stream. */
   if( gain_box == 0 ) gain_box = ntslice;

/* Find the number of blocks of time slices per bolometer. Each block
   contains "gain_box" time slices (except possibly for the last time slice
   which may contain more than gain_box but will be less than 2*gain_box).
   Each block of time slices from a single bolometer has its own gain, offset
   and correlation values. */
   nblock = ntslice/gain_box;
   if( nblock == 0 ) nblock = 1;

/* How many threads do we get to play with */
   nw = wf ? wf->nworker : 1;

/* If this is the first data to be accumulated zero the arrays */
   if( flags & AST__REBININIT ) {
      memset( map, 0, nw*msize*sizeof(*map) );
      memset( mapwgt, 0, nw*msize*sizeof(*mapwgt) );
      memset( mapwgt2, 0, nw*msize*sizeof(*mapwgt2) );
      if( mapvar ) memset( mapvar, 0, nw*msize*sizeof(*mapvar) );
   }

/* Find how many time slices to process in each worker thread. */
   step = ntslice/nw;
   if( step == 0 ) {
      step = 1;
      nw = ntslice;
   }

/* Allocate job data for threads, and store the range of time slices to be
   processed by each one. Ensure that the last thread picks up anyleft-over
   time slices. */
   job_data = astCalloc( nw, sizeof(*job_data) );
   if( *status == SAI__OK ) {
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->t1 = iw*step;
         if( iw < nw - 1 ) {
            pdata->t2 = pdata->t1 + step - 1;
         } else {
            pdata->t2 = ntslice - 1;
         }

/* Store other values common to all jobs. */
         pdata->bstride = bstride;
         pdata->com = com;
         pdata->gai = gai;
         pdata->gain_box = gain_box;
         pdata->gbstride = gbstride;
         pdata->gcstride = gcstride;
         pdata->lut = lut;
         pdata->map = map + iw*msize;
         pdata->mapvar = mapvar + iw*msize;
         pdata->mapwgt = mapwgt + iw*msize;
         pdata->mapwgt2 = mapwgt2 + iw*msize;
         pdata->msize = msize;
         pdata->nblock = nblock;
         pdata->nbolo = nbolo;
         pdata->ntslice = ntslice;
         pdata->nw = nw;
         pdata->qual = qual;
         pdata->tstride = tstride;
         pdata->vntslice = vntslice;
         pdata->var = var;
         pdata->vbstride = vbstride;
         pdata->vtstride = vtstride;

/* Submit jobs to include the supplied data in the running sum arrays.
   Each thread stores its running sums in a different section of the
   "map..." arrays. */
         pdata->oper = 1;
         thrAddJob( wf, 0, pdata, smf1_rebincom, 0, NULL, status );
      }

/* Wait for all jobs to finish. */
      thrWait( wf, status );

/* If this is the last data to be accumulated re-normalize */
      if( flags & AST__REBINEND ) {

/* Find how many pixels to process in each worker thread. */
         pixstep = msize/nw;
         if( pixstep == 0 ) pixstep = 1;

/* Submit jobs to sum the runnings sums from all threads and normalise
   the total sums, storing them back at the start of the "map..." arrays. */
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->p1 = iw*pixstep;
            if( iw < nw - 1 ) {
               pdata->p2 = pdata->p1 + pixstep - 1;
            } else {
               pdata->p2 = msize - 1 ;
            }
            pdata->map = map;
            pdata->mapvar = mapvar;
            pdata->mapwgt = mapwgt;
            pdata->mapwgt2 = mapwgt2;
            pdata->oper = 2;
            thrAddJob( wf, 0, pdata, smf1_rebincom, 0, NULL, status );
         }

/* Wait for all jobs to finish. */
         thrWait( wf, status );
      }
   }

/* Free resources. */
   job_data = astFree( job_data );
}







static void smf1_rebincom( void *job_data_ptr, int *status ) {

/* Local Variables: */
   SmfRebinComData *pdata;
   dim_t ibolo;
   dim_t ip;
   dim_t itime;
   dim_t p1;
   dim_t p2;
   double *pc;
   double *pm;
   double *pmv;
   double *pmw;
   double *pmw2;
   double *pwg;
   double *pwoff;
   double comvalue;
   double s1;
   double s2;
   double s3;
   double s4;
   double wgt;
   int *pl0;
   int *pl;
   int iw;
   int nw;
   size_t iv0;
   size_t msize;
   smf_qual_t *pq;
   smf_qual_t *pq0;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfRebinComData *) job_data_ptr;

/* Add pixels into the running sums
   ================================ */
   if( pdata->oper == 1 ) {
      dim_t t1 = pdata->t1;
      dim_t t2 = pdata->t2;
      size_t bstride = pdata->bstride;
      size_t tstride = pdata->tstride;
      size_t gbstride = pdata->gbstride;
      size_t gcstride = pdata->gcstride;
      dim_t ntslice = pdata->ntslice;
      dim_t vntslice = pdata->vntslice;
      int nblock = pdata->nblock;
      dim_t nbolo = pdata->nbolo;
      dim_t gain_box = pdata->gain_box;
      size_t vbstride = pdata->vbstride;
      size_t vtstride = pdata->vtstride;
      double *var = pdata->var;

/* Allocate work space */
      double *wg = astMalloc( ( t2 - t1 + 1 )*sizeof( *wg ) );
      double *woff = astMalloc( ( t2 - t1 + 1 )*sizeof( *woff ) );

/* Loop over all bolometers. */
      pq0 = pdata->qual + pdata->tstride*t1;
      pl0 = pdata->lut + pdata->tstride*t1;
      iv0 = 0;
      for( ibolo = 0; ibolo < nbolo && *status == SAI__OK; ibolo++ ) {

/* Skip bad bolometers */
         if( !(*pq0 & SMF__Q_BADB ) ) {

/* Get the gain and offset for each time slice being processed by this
   thread. They are returned in "wg" and "woff". */
            smf_gandoff( ibolo, t1, t2, ntslice, gbstride, gcstride,
                         pdata->gai, nblock, gain_box, wg, woff, NULL, status );

/* Loop round all time slices to be processed by the current thread. */
            pc = pdata->com + t1;
            pq = pq0;
            pl = pl0;
            pwg = wg;
            pwoff = woff;
            for( itime = t1; itime <= t2; itime++ ){

/* Check the sample does not fall outside the map, the model value is
   good and the time slample is good. */
               if( *pl != VAL__BADI && *pc != VAL__BADD &&
                   !( *pq & SMF__Q_MOD ) ){

/* Get the COM value scaled for the bolometer. */
                  comvalue = (*pwg)*(*pc) + (*pwoff);

/* Get the weight. */
                  if( var ){
                     wgt = var[ iv0 + ( itime % vntslice )*vtstride ];
                     if( wgt != VAL__BADD && wgt > 0.0 ) {
                        wgt = 1.0/wgt;
                     } else {
                        wgt = 0.0;
                     }
                  } else {
                     wgt = 1.0;
                  }

/* Update the running sum maps. */
                  if( wgt > 0.0 ){
                     (pdata->map)[ *pl ] += wgt*comvalue;
                     (pdata->mapvar)[ *pl ] += wgt*comvalue*comvalue;
                     (pdata->mapwgt)[ *pl ] += wgt;
                     (pdata->mapwgt2)[ *pl ] += wgt*wgt;
                  }
               }

/* Update pointers for the next time slice */
               pq += tstride;
               pl += tstride;
               pc++;
               pwg++;
               pwoff++;
            }
         }

/* Next bolometer. */
         pq0 += bstride;
         pl0 += bstride;
         iv0 += vbstride;
      }

/* Free resources */
      wg = astFree( wg );
      woff = astFree( woff );

/* Accumulate the running sums from all threads and normalise.
   ========================================================== */
   } else if( pdata->oper == 2 ) {
      p1 = pdata->p1;
      p2 = pdata->p2;
      nw = pdata->nw;
      msize = pdata->msize;

/* Loop over all pixels to be processed by this thread. */
      for( ip = p1; ip <= p2; ip++ ){

/* Add up the contributions from all threads. */
         pm = pdata->map + ip;
         pmv = pdata->mapvar + ip;
         pmw = pdata->mapwgt + ip;
         pmw2 = pdata->mapwgt2 + ip;
         s1 = 0.0;
         s2 = 0.0;
         s3 = 0.0;
         s4 = 0.0;
         for( iw = 0; iw < nw; iw++ ){
            s1 += *pm;
            s2 += *pmv;
            s3 += *pmw;
            s4 += *pmw2;

/* Move on to the next thread. */
            pm += msize;
            pmv += msize;
            pmw += msize;
            pmw2 += msize;
         }

/* Store the normalised values in the first map. */
         if( s3 > 1 ) {
            s1 /= s3;
            (pdata->mapwgt)[ ip ] = s3;
            (pdata->mapvar)[ ip ] = s2/s3 - s1*s1;
            (pdata->mapvar)[ ip ] *= s4/(s3*s3);
            (pdata->map)[ ip ] = s1;
         } else {
            (pdata->mapwgt)[ ip ] = 0.0;
            (pdata->mapvar)[ ip ] = VAL__BADD;
            (pdata->map)[ ip ] = VAL__BADD;
         }
      }

/* Report an error if the worker was to do an unknown job.
   ====================================================== */
   } else {
      *status = SAI__ERROR;
      errRepf( "", "smf1_rebincom: Invalid oper (%d) supplied.",
               status, pdata->oper );
   }
}
