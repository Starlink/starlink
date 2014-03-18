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
*                          const dim_t dims[2], dim_t box, double thresh,
*                          int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     map = const double * (Given)
*        The map to mask.
*     dims = dim_t[2] (Given)
*        The dimensions of the map.
*     box = dim_t (Given)
*        The number of pixels across the filter box. If even, the next
*        largest odd value is used.
*     thresh = double (Given)
*        The number of sigma at which to clip.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     A pointer to the returned array. Should be freed using astFree when
*     no longer needed.

*  Description:
*     The returned map is a copy of the supplied map, but with pixel ses
*     to VAL__BADD fo feratures that have a spatial extent of less than
*     "box" pixels. TYhe algorithm is siliar to that used by the
*     kappa:ffclean command.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-MAR-2014 (DSB):
*        Original version.
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
double *smf_ffclean( ThrWorkForce *wf, const double *map, const dim_t dims[2],
                     dim_t box, double thresh, int *status ){

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

/* Allocate the returned array holding a copy of the supplied aray. */
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

/* Set up jobs to smooth each row. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->pixlo = iw*pixstep;
         if( iw == nw - 1 ) {
            pdata->pixhi = nel - 1;
         } else {
            pdata->pixhi = pdata->pixlo + pixstep - 1;
         }
      }

/* Loop until converged, or the max number of iterations is reached. */
      for( iter = 0; iter < MAXITER && *status == SAI__OK; iter++ ) {

/* The result array currently holds all the input pixel values that have
   not yet been rejected. Smooth it, and get the residuals by removing
   the smoothed version from the result array. */
         residuals = smf_tophat2( wf, result, dims, box, 1, status );

/* If this is the first iteration, we do not yet have any clipping
   limits. So we need to get the statistics of the residuals explicitly. */
         if( iter == 0 ) {
            for( iw = 0; iw < nw; iw++ ) {
               pdata = job_data + iw;
               pdata->resid = residuals;
               pdata->oper = 1;
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

/* Set the results array bad at every pixel for which the absolute residual
   is greater than "lim". At the same time, form the statistics of the
   remaining pixels for use on the next iteration. */
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->resid = residuals;
            pdata->result = result;
            pdata->lim = lim;
            pdata->oper = 2;
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
                    status, iter, sqrt( variance ), nsum_old - nsum );

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
   double *p0;
   double *p1;
   size_t ipix;
   smfFFCleanJobData *pdata;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer to the job data. */
   pdata = (smfFFCleanJobData *) job_data;

/* Find the stats for the resid array. */
   if( pdata->oper == 1  ) {

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


/* Set the result array bad at every pixel for which the absolute residual
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
            if( fabs( *p0 ) > pdata->lim ) {
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


