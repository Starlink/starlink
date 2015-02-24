/*
*+
*  Name:
*     smf_tophat2

*  Purpose:
*     Smooth a 2D map using a tophat filter.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     double *smf_tophat2( ThrWorkForce *wf, const double *map, dim_t dims[2],
*                          dim_t box, int sub, double wlim, int niter,
*                          int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     map = const double * (Given)
*        The map to smooth.
*     dims = dim_t[2] (Given)
*        The dimensions of the map.
*     box = dim_t (Given)
*        The number of pixels across the filter box. If even, the next
*        largest odd value is used.
*     sub = int (Given)
*        If zero, the returned array contains the smoothed array. If
*        non-zero, the returned array contains the supplied array
*        minus the smoothed array. Not used if niter is greater than 1
*        (in which case the smoothed array is always returned).
*     wlim = double (Given)
*        The fraction of good input pixels for a good output pixel. If
*        zero, the bad pixel mask is copied from the input to the output.
*     niter = int (Given)
*        The number of times to apply the top hat filter.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     A pointer to the returned array. Should be freed using astFree when
*     no longer needed.

*  Description:
*     The supplied map is smoothed using a 2D box filter of the specified
*     size. A newly allocated map is returned holding either the
*     smoothed map of the difference between the original and smoothed maps.
*     The filter may be applied more than once (see "niter").

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-MAR-2014 (DSB):
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

/* Data types */
typedef struct smfTophat2JobData {
   const double *in;
   dim_t chi;
   dim_t clo;
   dim_t hbox;
   dim_t ncol;
   dim_t nrow;
   dim_t rhi;
   dim_t rlo;
   double *dout;
   double *wout;
   double wlim;
   int oper;
   int sub;
} smfTophat2JobData;

/* Prototypes for local functions */
static void smf1_tophat2_job( void *job_data, int *status );

/* Main entry point. */
double *smf_tophat2( ThrWorkForce *wf, const double *map, const dim_t dims[2],
                     dim_t box, int sub, double wlim, int niter, int *status ){

/* Local Variables: */
   double *work = NULL;
   smfTophat2JobData *job_data;
   smfTophat2JobData *pdata;
   int nw;
   int iter;
   int iw;
   int rowstep;
   int colstep;
   double *temp1 = NULL;
   double *temp2 = NULL;
   const double *in = NULL;
   double *result = NULL;

/* Check inherited status. */
   if( *status != SAI__OK || box < 1 ) return result;

/* Allocate the returned array, and a work array, initialising them both
   to hold zeros. */
   temp1 = astCalloc( dims[ 0 ]*dims[ 1 ], sizeof( *temp1 ) );
   work = astCalloc( dims[ 0 ]*dims[ 1 ], sizeof( *work ) );
   if( niter > 1 ) temp2 = astCalloc( dims[ 0 ]*dims[ 1 ], sizeof( *temp2 ) );

/* How many threads do we get to play with */
   nw = wf ? wf->nworker : 1;

/* Allocate job data for threads. */
   job_data = astCalloc( nw, sizeof(*job_data) );
   if( *status == SAI__OK ) {

/* Decide how many rows and columns to process in each thread. */
      colstep = dims[ 0 ]/nw;
      if( colstep == 0 ) colstep = 1;
      rowstep = dims[ 1 ]/nw;
      if( rowstep == 0 ) rowstep = 1;

/* Do the required number of smoothings. */
      for( iter = 0; iter < niter; iter++ ) {

/* Set the input and output arrays for the next iteration. */
         if( iter == 0 ) {
            result = temp1;
            in = map;
         } else if( iter % 2 == 0 ) {
            result = temp1;
            in = temp2;
         } else {
            in = temp1;
            result = temp2;
         }

/* Set up jobs to smooth each row. */
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;

            pdata->rlo = iw*rowstep;
            pdata->clo = iw*colstep;
            if( iw == nw - 1 ) {
               pdata->rhi = dims[ 1 ] - 1;
               pdata->chi = dims[ 0 ] - 1;
            } else {
               pdata->rhi = pdata->rlo + rowstep - 1;
               pdata->chi = pdata->clo + colstep - 1;
            }

            pdata->nrow = dims[ 1 ];
            pdata->ncol = dims[ 0 ];
            pdata->hbox = box/2;
            pdata->sub = ( niter == 1 ) ? sub : 0;
            pdata->in = in;
            pdata->dout = result;
            pdata->wout = work;
            pdata->wlim = wlim;
            pdata->oper = 0;
            thrAddJob( wf, 0, pdata, smf1_tophat2_job, 0, NULL, status );
         }

/* Wait for the workforce to complete all jobs. */
         thrWait( wf, status );

/* Set up jobs to smooth each column. */
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->oper = 1;
            thrAddJob( wf, 0, pdata, smf1_tophat2_job, 0, NULL, status );
         }

/* Wait for the workforce to complete all jobs. */
         thrWait( wf, status );
      }
   }

/* Free resources. */
   job_data = astFree( job_data );
   work = astFree( work );
   if( temp1 != result ) temp1 = astFree( temp1 );
   if( temp2 != result ) temp2 = astFree( temp2 );

/* Return the pointer to the returned array. */
   return result;
}

static void smf1_tophat2_job( void *job_data, int *status ) {
/*
*  Name:
*     smf1_tophat2_job

*  Purpose:
*     Perform various parallel operations for snf_tophat2.

*  Invocation:
*     void smf1_tophat2_job( void *job_data, int *status )

*  Arguments:
*     job_data = void * (Given)
*        Pointer to the data needed by the job. Should be a pointer to a
*        smfTophat2JobData structure.
*     status = int * (Given and Returned)
*        Pointer to global status.

*/

/* Local Variables: */
   const double *p0;
   const double *p1;
   const double *pdb;
   const double *pdin;
   const double *pdt;
   const double *pin;
   const double *pl;
   const double *pr;
   const double *pwb;
   const double *pwin;
   const double *pwt;
   dim_t icol;
   dim_t irow;
   dim_t ncol;
   double *pdout;
   double *pwout;
   double *work;
   double sum;
   double wlim;
   double wsum;
   int cl;
   int cr;
   int rb;
   int rt;
   size_t offset;
   smfTophat2JobData *pdata;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer to the job data. */
   pdata = (smfTophat2JobData *) job_data;

/* Smooth along the rows. */
   if( pdata->oper == 0  ) {

/* Loop round all rows being processed by this thread. */
      offset =  pdata->rlo*pdata->ncol;
      for( irow = pdata->rlo; irow <= pdata->rhi; irow++ ) {

/* Pointers to the first input and output values in the current row. */
         pin = pdata->in + offset;
         pdout = pdata->dout + offset;
         pwout = pdata->wout + offset;

/* Sum the good values in the first half box of the row. */
         sum = 0.0;
         wsum = 0.0;
         for( icol = 0; icol <= pdata->hbox; icol++ ) {
            if( pin[ icol ] != VAL__BADD ) {
               sum += pin[ icol ];
               wsum += 1.0;
            }
         }

/* Initialise the column index of the left-most (oldest) and right-most
   (youngest) column in the box to represent the starting position where
   the box is centred on the first pixel in the row. */
         cl = -pdata->hbox;
         cr = pdata->hbox;

/* Get pointers to the first and last column values currently included
   in the box. */
         pl = pin + cl;
         pr = pin + cr;

/* Loop round all columns. */
         for( icol = 0; icol < pdata->ncol; icol++ ) {

/* Store the current values in the output array. */
            *(pdout++) = sum;
            *(pwout++) = wsum;

/* Remove the left value from the box. */
            if( cl >= 0 && *pl != VAL__BADD ) {
               sum -= *pl;
               wsum -= 1.0;
            }
            pl++;
            cl++;

/* Add the next right value into the box. */
            pr++;
            cr++;
            if( cr < (int) pdata->ncol && *pr != VAL__BADD ) {
               sum += *pr;
               wsum += 1.0;
            }
         }

/* Update offset to start of next row. */
         offset += pdata->ncol;
      }

/* Smooth along the columns. */
   } else if( pdata->oper == 1  ) {

/* Allocate a work array to hold a single column of smoothed values. */
      work = astMalloc( pdata->nrow*sizeof( *work ) );
      if( *status == SAI__OK ) {

/* The number of good input pixels needed for a good output pixel. */
         if( pdata->wlim > 0.0 ) {
            wlim = 1 + 2*pdata->hbox;
            if( pdata->wlim <= 1.0 ) {
               wlim = pdata->wlim*(wlim*wlim);
               if( wlim < 1 ) wlim = 0.001;
            } else {
               wlim = wlim*wlim;
            }

         } else {
            wlim = 0.0;
         }

/* Note the step between row values. */
         ncol = pdata->ncol;

/* Loop round all columns being processed by this thread. */
         for( icol = pdata->clo; icol <= pdata->chi; icol++ ) {

/* Pointers to the first input and output values in the current column. */
            pin = pdata->in + icol;
            pdin = pdata->dout + icol;
            pwin = pdata->wout + icol;
            pdout = work;

/* Sum the data values and counts in the first half box of the column. */
            sum = 0.0;
            wsum = 0.0;

            p0 = pdin;
            p1 = pwin;

            for( irow = 0; irow <= pdata->hbox; irow++ ) {
               sum += *p0;
               wsum += *p1;

               p0 += ncol;
               p1 += ncol;
            }

/* Initialise the row index of the bottom (oldest) and top (youngest)
   row in the box to represent the starting position where the box is
   centred on the first pixel in the column. */
            rb = -pdata->hbox;
            rt = pdata->hbox;

/* Get pointers to the bottom and top row values currently included
   in the box. */
            pdb = pdin + rb*ncol;
            pdt = pdin + rt*ncol;

            pwb = pwin + rb*ncol;
            pwt = pwin + rt*ncol;

/* Loop round all rows. */
            for( irow = 0; irow < pdata->nrow; irow++ ) {

/* Store the mean of the data value current in the box. */
               if( wlim > 0.0 ) {
                  if( wsum > wlim ) {
                     *(pdout++) = sum/wsum;
                  } else {
                     *(pdout++) = VAL__BADD;
                  }

               } else if( *pin != VAL__BADD ) {
                  if( wsum > 0.0 ) {
                     *(pdout++) = sum/wsum;
                  } else {
                     *(pdout++) = VAL__BADD;
                  }

               } else {
                  *(pdout++) = VAL__BADD;
               }

/* Remove the bottom value from the box. */
               if( rb >= 0 ) {
                  sum -= *pdb;
                  wsum -= *pwb;
               }
               pdb += ncol;
               pwb += ncol;
               rb++;

/* Add the next top value into the box. */
               pdt += ncol;
               pwt += ncol;
               rt++;
               if( rt < (int) pdata->nrow ) {
                  sum += *pdt;
                  wsum += *pwt;
               }

               pin += ncol;
            }

/* Finally copy the smoothed values from the work array to the returned
   array, subtracting from the original if required. */
            pdin = work;
            pdout = pdata->dout + icol;
            if( pdata->sub ) {
               p0 = pdata->in + icol;
               for( irow = 0; irow < pdata->nrow; irow++ ) {
                  if( *p0 != VAL__BADD && *pdin != VAL__BADD ) {
                     *pdout = *p0 - *pdin;
                  } else {
                     *pdout = VAL__BADD;
                  }

                  pdin++;
                  pdout += ncol;
                  p0 += ncol;
               }

            } else {
               for( irow = 0; irow < pdata->nrow; irow++ ) {
                  *pdout = *(pdin++);
                  pdout += ncol;
               }
            }
         }

/* Free the work array. */
         work = astFree( work );
      }

   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( "", "smf1_tophat2: Invalid operation (%d) supplied.",
               status, pdata->oper );
   }
}

