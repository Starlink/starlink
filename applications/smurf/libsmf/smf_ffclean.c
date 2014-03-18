/*
*+
*  Name:
*     smf_ffclean

*  Purpose:
*     Mask small features in a map

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     double *smf_ffclean( ThrWorkForce *wf, const double *map,
*                          const double *mapvar, const dim_t dims[2],
*                          dim_t box, dim_t box0, double thresh,
*                          int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     map = const double * (Given)
*        The map to mask.
*     mapvar = const double * (Given)
*        The variance map. May be NULL. If supplied, the returned map
*        holds the masked SNR ratio, rather than the masked map value.
*     dims = dim_t[2] (Given)
*        The dimensions of the map.
*     box = dim_t (Given)
*        The number of pixels across the filter box. If even, the next
*        largest odd value is used.
*     box0 = dim_t (Given)
*        If greater than 1, the supplied map (or SNR map if "mapvar" is
*        given) will first smoothed before starting the ffclean algorithm
*        using a box filter of width "box0" pixels.
*     thresh = double (Given)
*        The number of sigma at which to clip.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     A pointer to the returned array. Should be freed using astFree when
*     no longer needed.

*  Description:
*     The returned map is a copy of the supplied map (or the supplied SNR
*     map if "mapvar" is supplied), but with pixels set to VAL__BADD for
*     features that have a spatial extent of less than "box" pixels. The
*     algorithm is similar to that used by the kappa:ffclean command.
*
*     The returned map is initialised to be a copy of the supplied map. The
*     returned map is then smoothed using a box filter of size specified
*     by "box". The residuals between the smoothed and original are found,
*     and all pixels with residuals greater than "thresh" times the noise
*     level are set bad in the returned map. The RMS of the remaining
*     residuals is also found. The process then repeats, using the new
*     returned map. This iterative loop terminates when an interation
*     introduces fewer than 10 new bad pixels. The noise level used to
*     determine the residual threshold is the RMS of the remaining
*     residuals found on the previous iteration.
*
*     A significant difference between kappa:ffclean and this function
*     is that this function only reject positive outliers, whereas
*     kappa:ffclean rejects both positive and negative outliers. This
*     means it only reject spositive features in the map (i.e. sources).
*     This helps to stop areas of negative bowling being masked out as
*     source regions.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-MAR-2014 (DSB):
*        Original version.
*     18-MAR-2014 (DSB):
*        - Allow SNR map to be masked and returned.
*        - Only clip positive outliers.
*        - Added argument "box0"/
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2014 Science & Technology Facilities Council.
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
#include "ast.h"
#include "mers.h"
#include "star/thr.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define MAXITER 20

/* Data types */
typedef struct smfFFCleanJobData {
   const double *mapvar;
   double *resid;
   double *result;
   double lim;
   double sum1;
   double sum2;
   int oper;
   size_t nsum;
   size_t pixhi;
   size_t pixlo;
} smfFFCleanJobData;

/* Prototypes for local functions */
static void smf1_ffclean_job( void *job_data, int *status );

/* Main entry point. */
double *smf_ffclean( ThrWorkForce *wf, const double *map, const double *mapvar,
                     const dim_t dims[2], dim_t box, dim_t box0, double thresh,
                     int *status ){

/* Local Variables: */
   double *result = NULL;
   double *residuals = NULL;
   double lim = 0.0;
   double mean;
   double sum1;
   double sum2;
   double variance;
   int iter;
   int iw;
   int nw;
   size_t nel;
   size_t nsum = 0;
   size_t nsum_old;
   size_t pixstep;
   smfFFCleanJobData *job_data;
   smfFFCleanJobData *pdata;

/* Check inherited status. */
   if( *status != SAI__OK || box < 1 ) return result;

/* Allocate the returned array holding a copy of the supplied array. */
   nel = dims[ 0 ]*dims[ 1 ];
   result = astStore( NULL, map, nel*sizeof( *result ) );

/* How many threads do we get to play with */
   nw = wf ? wf->nworker : 1;

/* Allocate job data for threads. */
   job_data = astCalloc( nw, sizeof(*job_data) );
   if( *status == SAI__OK ) {

/* Decide how many pixels to process in each thread. */
      pixstep = nel/nw;
      if( pixstep == 0 ) pixstep = 1;

/* Store information needed by all jobs, and if required, initiate jobs
   to convert the returned array from data values to SNR values. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->pixlo = iw*pixstep;
         if( iw == nw - 1 ) {
            pdata->pixhi = nel - 1;
         } else {
            pdata->pixhi = pdata->pixlo + pixstep - 1;
         }

         if( mapvar ) {
            pdata->oper = 0;
            pdata->mapvar = mapvar;
            pdata->result = result;
            thrAddJob( wf, 0, pdata, smf1_ffclean_job, 0, NULL, status );
         }
      }

/* Wait for the jobs to finish. */
      if( mapvar ) thrWait( wf, status );

/* If required, smooth the map before starting the cleaning operation. */
      if( box0 > 1 ) {
         double *newres = smf_tophat2( wf, result, dims, box0, 0, 0.0, status );
         (void) astFree( result );
         result = newres;
      }

/* Loop until converged, or the max number of iterations is reached. */
      for( iter = 0; iter < MAXITER && *status == SAI__OK; iter++ ) {

/* The result array currently holds all the input pixel values that have
   not yet been rejected. Smooth it, and get the residuals by removing
   the smoothed version from the result array. */
         residuals = smf_tophat2( wf, result, dims, box, 1, 1.0E-6, status );

/* If this is the first iteration, we do not yet have any clipping
   limits. So we need to get the statistics of the residuals explicitly. */
         if( iter == 0 ) {
            for( iw = 0; iw < nw; iw++ ) {
               pdata = job_data + iw;
               pdata->oper = 1;
               pdata->resid = residuals;
               thrAddJob( wf, 0, pdata, smf1_ffclean_job, 0, NULL, status );
            }
            thrWait( wf, status );

            sum1 = 0.0;
            sum2 = 0.0;
            nsum = 0;
            for( iw = 0; iw < nw; iw++ ) {
               pdata = job_data + iw;
               sum1 += pdata->sum1;
               sum2 += pdata->sum2;
               nsum += pdata->nsum;
            }

            if( nsum == 0 && *status == SAI__OK ) {
               *status = SAI__ERROR;
               errRep( "", "smf_ffclean: All data rejected.", status );
               break;
            }

            mean = sum1/nsum;
            variance = sum2/nsum - mean*mean;
            lim = sqrt( variance )*thresh;
         }

/* Save the number of remaining pixels. */
         nsum_old = nsum;

/* Set the results array bad at every pixel for which the residual
   is greater than "lim". At the same time, form the statistics of the
   remaining pixels for use on the next iteration. */
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->lim = lim;
            pdata->oper = 2;
            pdata->resid = residuals;
            pdata->result = result;
            thrAddJob( wf, 0, pdata, smf1_ffclean_job, 0, NULL, status );
         }
         thrWait( wf, status );

         sum1 = 0.0;
         sum2 = 0.0;
         nsum = 0;
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            sum1 += pdata->sum1;
            sum2 += pdata->sum2;
            nsum += pdata->nsum;
         }

         if( nsum == 0 ) {
            *status = SAI__ERROR;
            errRep( "", "smf_ffclean: All data rejected.", status );
            break;
         }

         mean = sum1/nsum;
         variance = sum2/nsum - mean*mean;
         lim = sqrt( variance )*thresh;

/* Free the current residuals array. */
         residuals = astFree( residuals );

/* Report progress. */
         msgOutiff( MSG__DEBUG, "", "smf_ffclean: iter=%d sigma=%g nrej=%zu\n",
                    status, iter, sqrt( variance ), nsum );

/* No more iterations if the number of remaining samples has changed by
   fewer than 10. */
         if( abs( nsum - nsum_old ) < 10 ) break;
      }
   }

/* Free resources. */
   job_data = astFree( job_data );
   residuals = astFree( residuals );

/* Return the pointer to the returned array. */
   return result;
}

static void smf1_ffclean_job( void *job_data, int *status ) {
/*
*  Name:
*     smf1_ffclean_job

*  Purpose:
*     Perform various parallel operations for snf_ffclean.

*  Invocation:
*     void smf1_ffclean_job( void *job_data, int *status )

*  Arguments:
*     job_data = void * (Given)
*        Pointer to the data needed by the job. Should be a pointer to a
*        smfFFCleanJobData structure.
*     status = int * (Given and Returned)
*        Pointer to global status.

*/

/* Local Variables: */
   const double *p2;
   double *p0;
   double *p1;
   size_t ipix;
   smfFFCleanJobData *pdata;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer to the job data. */
   pdata = (smfFFCleanJobData *) job_data;

/* Convert result from data value to SNR. */
   if( pdata->oper == 0  ) {
      p0 = pdata->result + pdata->pixlo;
      p2 = pdata->mapvar + pdata->pixlo;
      for( ipix = pdata->pixlo; ipix <= pdata->pixhi; ipix++,p0++,p2++ ) {
         if( *p0 != VAL__BADD && *p2 != VAL__BADD && *p2 > 0.0 ) {
            *p0 /= sqrt( *p2 );
         } else {
            *p0 = VAL__BADD;
         }
      }

/* Find the stats for the resid array. */
   } else if( pdata->oper == 1  ) {

      pdata->sum1 = 0.0;
      pdata->sum2 = 0.0;
      pdata->nsum = 0;

      p0 = pdata->resid + pdata->pixlo;
      for( ipix = pdata->pixlo; ipix <= pdata->pixhi; ipix++,p0++ ) {
         if( *p0 != VAL__BADD ) {
            pdata->sum1 += *p0;
            pdata->sum2 += (*p0)*(*p0);
            (pdata->nsum)++;
         }
      }


/* Set the result array bad at every pixel for which the residual
   is greater than "lim". At the same time, form the statistics of the
   remaining pixels. */
   } else if( pdata->oper == 2  ) {

      pdata->sum1 = 0.0;
      pdata->sum2 = 0.0;
      pdata->nsum = 0;

      p0 = pdata->resid + pdata->pixlo;
      p1 = pdata->result + pdata->pixlo;
      for( ipix = pdata->pixlo; ipix <= pdata->pixhi; ipix++,p0++,p1++ ) {
         if( *p0 != VAL__BADD && *p1 != VAL__BADD ) {
            if( *p0 > pdata->lim ) {
               *p1 = VAL__BADD;
            } else {
               pdata->sum1 += *p0;
               pdata->sum2 += (*p0)*(*p0);
               (pdata->nsum)++;
            }
         } else {
            *p1 = VAL__BADD;
         }
      }

   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( "", "smf1_ffclean: Invalid operation (%d) supplied.",
               status, pdata->oper );
   }
}


