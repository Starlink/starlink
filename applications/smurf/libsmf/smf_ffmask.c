/*
*+
*  Name:
*     smf_ffmask

*  Purpose:
*     Mask out feaures within a time-series that have a given size.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     size_t smf_ffmask( ThrWorkForce *wf, double *data, smf_qual_t *qua,
*                        dim_t nbolo, dim_t ntslice, size_t tstride,
*                        size_t bstride, size_t t_first, size_t t_last,
*                        size_t box, double clip, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     data = smfData * (Given and Returned)
*        Pointer to the start of the data array. Values masked by this
*        function are set to VAL__BADD in the returned array.
*     qua = smf_qual_t * (Given)
*        Pointer to the start of the quality array.
*     nbolo = dim_t (Given)
*        The number of bolometers in the array.
*     ntslice = dim_t (Given)
*        The number of time slices in the array.
*     tstride = size_t (Given)
*        The number of array elements between adjacent time slices in a
*        single bolometer.
*     bstride = size_t (Given)
*        The number of array elements between adjacent bolometers at a
*        single time slice.
*     t_first = size_t (Given)
*        The index of the first time slice to use.
*     t_last = size_t (Given)
*        The index of the last time slice to use.
*     box = size_t (Given)
*        The number of time slices corresponding to the feature size to
*        be masked.
*     clip = double (Given)
*        The number of standard deviations at which the residuals should
*        be clipped on each iteration.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     The number of samples masked by the function.

*  Description:
*     Each bolometer time-stream in the supplied array is masked
*     independently as follows:
*     - The time stream is smoothed using a top hat function of width
*     "box" time-slices.
*     - The residuals between the original and smoothed data are found,
*     and the RMS of these residuals is found.
*     - The supplied data array values are set bad for any residuals
*     greater than "clip" times the RMS.
*     - If this results in any points being set bad, the whole process is
*     repeated again.

*  Authors:
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     8-SEP-2021 (DSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2021 East Asian Observatory.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif


/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "star/thr.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Prototypes for local static functions. */
static void smf1_ffmask( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfFfmaskData {
   dim_t b1;
   dim_t b2;
   dim_t ntslice;
   double *data;
   double clip;
   size_t box;
   size_t bstride;
   size_t nbad;
   size_t t_first;
   size_t t_last;
   size_t tstride;
   smf_qual_t *qua;
} smfFfmaskData;

size_t smf_ffmask( ThrWorkForce *wf, double *data, smf_qual_t *qua,
                   dim_t nbolo, dim_t ntslice, size_t tstride,
                   size_t bstride, size_t t_first, size_t t_last,
                   size_t box, double clip, int *status ){

/* Local Variables: */
   smfFfmaskData *job_data = NULL;
   smfFfmaskData *pdata;
   int iw;
   int nw;
   size_t bstep;
   size_t nbad = 0;

/* Check inherited status. */
   if( *status != SAI__OK ) return 0;

/* How many threads do we get to play with */
   nw = wf ? wf->nworker : 1;

/* Find how many bolometers to process in each worker thread. */
   if( (int) nbolo >= nw ) {
      bstep = nbolo/nw;
   } else {
      bstep = 1;
      nw = nbolo;
   }

/* Allocate job data for threads. */
   job_data = astMalloc( nw*sizeof(*job_data) );

/* Store the range of bolos to be processed by each one. Ensure that
   the last thread picks up any left-over bolos. */
   if( *status == SAI__OK ) {
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->b1 = iw*bstep;
         if( iw < nw - 1 ) {
            pdata->b2 = pdata->b1 + bstep - 1;
         } else {
            pdata->b2 = nbolo - 1;
         }

/* Initialise other values required by the worker thread. */
         pdata->ntslice = ntslice;
         pdata->bstride = bstride;
         pdata->tstride = tstride;
         pdata->data = data;
         pdata->qua = qua;
         pdata->t_first = t_first;
         pdata->t_last = t_last;
         pdata->clip = clip;
         pdata->box = box;

/* Submit a job to mask the range of bolometers from b1 to b2. */
         thrAddJob( wf, 0, pdata, smf1_ffmask, 0, NULL, status );
      }

/* Wait for all jobs to finish. */
      thrWait( wf, status );

/* Get the total number of values set bad by this function. */
      nbad = 0;
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         nbad += pdata->nbad;
      }
   }

/* Free resources. */
   job_data = astFree( job_data );

   return nbad;
}

static void smf1_ffmask( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_ffmask

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_ffmask.

*  Invocation:
*     smf1_ffmask( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = smfFfmaskData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   dim_t b1;
   dim_t b2;
   dim_t ibolo;
   dim_t ntslice;
   double *pd0;
   double *pd;
   double *pw;
   double *work;
   double clip;
   double lim;
   double rms;
   double s1;
   int done;
   size_t box;
   size_t bstride;
   size_t itime;
   size_t nbad;
   size_t nt;
   size_t s2;
   size_t t_first;
   size_t t_last;
   size_t tstride;
   smfFfmaskData *pdata;
   smf_qual_t *pq0;
   smf_qual_t *pq;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (smfFfmaskData *) job_data_ptr;

/* Save some commonly used values in local variables. */
   bstride = pdata->bstride;
   tstride = pdata->tstride;
   b1 = pdata->b1;
   b2 = pdata->b2;
   t_first = pdata->t_first;
   t_last = pdata->t_last;
   ntslice = pdata->ntslice;
   clip = pdata->clip;
   box = pdata->box;
   nt = t_last - t_first + 1;

/* Initialise the number of values set bad by this function. */
   nbad = 0;

/* Allocate work space. */
   work = astMalloc( nt*sizeof( *work ) );
   if( *status == SAI__OK ) {

/* Loop round all bolos to be processed by this thread. */
      pd0 = pdata->data + b1*bstride + t_first*tstride;
      pq0 = pdata->qua + b1*bstride + t_first*tstride;
      for( ibolo = b1; ibolo <= b2; ibolo++ ) {

/* Check that the bolometer has not been flagged as entirely bad. */
         if( !( *pq0 & SMF__Q_BADB ) ){

/* Loop until no more data values are rejected. */
            done = 0;
            while( !done ){

/* Copy the data to a new array. */
               pd = pd0;
               pq = pq0;
               pw = work;
               for( itime = t_first; itime <= t_last; itime++,pw++){
                  if( !( (*pq) & SMF__Q_GOOD ) && *pd != VAL__BADD ) {
                     *pw = *pd;
                  } else {
                     *pw = VAL__BADD;
                  }
                  pd += tstride;
                  pq += tstride;
               }

/* Smooth the data in "work" using a top-hat filter. */
               smf_boxcar1D( work, nt, 1, box, NULL, 0, 1, NULL, status );

/* Replace the values in "work" with the residuals. Get the sums needed
   to find the RMS residual. */
               s1 = 0.0;
               s2 = 0;
               pd = pd0;
               pq = pq0;
               pw = work;
               for( itime = t_first; itime <= t_last; itime++,pw++){
                  if( !( (*pq) & SMF__Q_GOOD ) && *pd != VAL__BADD ) {
                     *pw -= *pd;
                     s1 += (*pw)*(*pw);
                     s2++;
                  } else {
                     *pw = VAL__BADD;
                  }
                  pd += tstride;
                  pq += tstride;
               }

/* Find the RMS residual. */
               if( s2 > SMF__MINSTATSAMP ){
                  rms = sqrt( s1/s2 );
               } else {
                  msgOutiff( MSG__VERB, "", "smf_ffmask: flagging "
                             "entire bad bolo %" DIM_T_FMT ", due to "
                             "insufficient samples", status, ibolo );

                  pq = pdata->qua + b1*bstride;
                  for( itime = 0; itime < ntslice; itime++ ){
                    *pq |= SMF__Q_BADB;
                     pq += tstride;
                  }

                  break;
               }

/* For all residuals greater than "clip" RMS, set the corresponding data
   value bad. */
               done = 1;
               lim = rms*clip;
               pd = pd0;
               pw = work;
               for( itime = t_first; itime <= t_last; itime++,pw++){
                  if( *pw != VAL__BADD && fabs( *pw ) > lim ){
                     nbad++;
                     *pd = VAL__BADD;
                     done = 0;
                  }
                  pd += tstride;
               }
            }
         }

/* Move pointers on to the start of the next next bolometer time stream. */
         pd0 += bstride;
         pq0 += bstride;
      }
   }

/* Return the number of values set bad by this function. */
   pdata->nbad = nbad;

/* Free resources. */
   work = astFree( work );
}


