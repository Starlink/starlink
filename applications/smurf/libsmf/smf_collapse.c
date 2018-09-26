/*
*+
*  Name:
*     smf_collapse

*  Purpose:
*     Collapse a 3D array along a specified axis.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_collapse( ThrWorkForce *wf, const double *array,
*                        const smf_qual_t *qua, smf_qual_t mask, dim_t nx,
*                        dim_t ny, dim_t nz, int axis, double **mean,
*                        double **sigma, int **ngood, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     array = const double * (Given)
*        The 3D data array to be collapsed.
*     qua = const smf_qual_t * (Given)
*        Optional pointer to a quality array, the same shape and size as
*        "array".
*     mask = smf_qual_t (Given)
*        The mask defining quality values to exclude.
*     nx = dim_t (Given)
*        The first dimension of the 3D data array.
*     ny = dim_t (Given)
*        The second dimension of the 3D data array.
*     nz = dim_t (Given)
*        The third dimension of the 3D data array.
*     axis = int (Given)
*        The zero-based index of the axis to collapse.
*     mean = double ** (Returned)
*        Address of a variable holding a pointer to a 2D array to receive
*        the mean value in each collapse column. If the pointer in the
*        variable is NULL, a new array will be allocated and returned
*        (which should be freed using astFree when no longer needed). If
*        "mean" itself is NULL, no mean values are returned.
*     sigma = double ** (Returned)
*        Address of a variable holding a pointer to a 2D array to receive
*        the (sample) standard deviation of each collapse column. If the
*        pointer in the variable is NULL, a new array will be allocated and
*        returned (which should be freed using astFree when no longer
*        needed). If "sigma" itself is NULL, no standard deviations are
*         =returned.
*     ngood = int ** (Returned)
*        Address of a variable holding a pointer to a 2D array to receive
*        the number of good values in each collapse column. If the pointer
*        in the variable is NULL, a new array will be allocated and returned
*        (which should be freed using astFree when no longer needed). If
*        "ngood" itself is NULL, no values are returned.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine produces 2D maps holding statistics for each column in
*     a 3D array.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JUN-2015 (DSB):
*        Original version.
*     26-SEP-2018 (DSB):
*        Fix memory leak (job_data).
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2015 East Asian Observatory.
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
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"
#include "libsmf/smf_err.h"

/* Local data types: */
typedef struct smfCollapseJobData {
   dim_t b1;
   dim_t b2;
   const smf_qual_t *qua;
   smf_qual_t mask;
   const double *array;
   double *mean;
   double *sigma;
   int *ngood;
   dim_t nb1;
   dim_t ntime;
   size_t bstride1;
   size_t bstride2;
   size_t tstride;
} smfCollapseJobData;

static void smf1_collapse_job( void *job_data, int *status );

void smf_collapse( ThrWorkForce *wf, const double *array, const smf_qual_t *qua,
                   const smf_qual_t mask, dim_t nx, dim_t ny, dim_t nz, int axis,
                   double **mean, double **sigma, int **ngood, int *status ){

/* Local Variables */
   dim_t bstep;
   dim_t nb1;
   dim_t nb2;
   dim_t nbolo;
   dim_t ntime;
   double *wmean;
   double *wsigma;
   int *wngood;
   int iworker;
   int nworker;
   size_t bstride1;
   size_t bstride2;
   size_t tstride;
   smfCollapseJobData *job_data;
   smfCollapseJobData *pdata;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Get the dimensions of hte returned arrays, and the strides between
   column elements n the 3D array. We refer to the collapse axis as the
   "time" axis and the others as the "bolometer" axes. */
   if( axis == 2 ) {
      nb1 = nx;
      nb2 = ny;
      ntime = nz;
      bstride1 = 1;
      bstride2 = nx;
      tstride = nx*ny;
   } else if( axis == 1 ) {
      nb1 = nx;
      nb2 = nz;
      ntime = ny;
      bstride1 = 1;
      bstride2 = nx*ny;
      tstride = nx;
   } else if( axis == 0 ) {
      nb1 = ny;
      nb2 = nz;
      ntime = nx;
      bstride1 = nx;
      bstride2 = nx*ny;
      tstride = 1;
   } else {
      nb1 = 0;
      nb2 = 0;
      ntime = 0;
      bstride1 = 0;
      bstride2 = 0;
      tstride = 0;

      *status = SAI__ERROR;
      errRepf( "", "smf_collapse: Supplied axis index (%d) is illegal - must "
              "be 0, 1 or 2.", status, axis );
   }

/* Allocate the 2D arrays to return. */
   nbolo = nb1*nb2;
   if( mean && *mean ) {
      wmean = *mean;
   } else {
      wmean = astMalloc( nbolo*sizeof( *wmean ) );
   }

   if( sigma && *sigma ) {
      wsigma = *sigma;
   } else {
      wsigma = astMalloc( nbolo*sizeof( *wsigma ) );
   }

   if( ngood && *ngood ) {
      wngood = *ngood;
   } else {
      wngood = astMalloc( nbolo*sizeof( *wngood ) );
   }

/* Create structures used to pass information to the worker threads. */
   nworker = wf ? wf->nworker : 1;
   job_data = astMalloc( nworker*sizeof( *job_data ) );

/* Determine which bolometers are to be processed by which threads. */
   if( *status == SAI__OK ) {
      bstep = nbolo/nworker;
      if( bstep < 1 ) bstep = 1;

      for( iworker = 0; iworker < nworker; iworker++ ) {
         pdata = job_data + iworker;

         pdata->b1 = iworker*bstep;

         if( iworker < nworker - 1 ) {
            pdata->b2 = pdata->b1 + bstep - 1;
         } else {
            pdata->b2 = nbolo - 1;
         }

         pdata->array = array;
         pdata->qua = qua;
         pdata->mean = wmean;
         pdata->sigma = wsigma;
         pdata->ngood = wngood;
         pdata->nb1 = nb1;
         pdata->ntime = ntime;
         pdata->bstride1 = bstride1;
         pdata->bstride2 = bstride2;
         pdata->tstride = tstride;
         pdata->mask = mask;

         thrAddJob( wf, 0, pdata, smf1_collapse_job, 0, NULL, status );
      }
      thrWait( wf, status );
   }

/* Free resources. */
   job_data = astFree( job_data );

/* Return any requested arrays, and free the others. */
   if( mean ){
      if( *mean == NULL ) *mean = wmean;
   } else {
      wmean = astFree( wmean );
   }

   if( sigma ){
      if( *sigma == NULL ) *sigma = wsigma;
   } else {
      wsigma = astFree( wsigma );
   }

   if( ngood ){
      if( *ngood == NULL ) *ngood = wngood;
   } else {
      wngood = astFree( wngood );
   }

}


static void smf1_collapse_job( void *job_data, int *status ) {
/*
*  Name:
*     smf1_collapse_job

*  Purpose:
*     Collapse a group of columns.

*  Invocation:
*     void smf1_collapse_job( void *job_data, int *status )

*  Arguments:
*     job_data = void * (Given)
*        Pointer to the data needed by the job. Should be a pointer to a
*        smfCollapseJobData structure.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine does the work for smf_collapse. It collapses a group
*     of columns.

*/

/* Local Variables: */
   const double *array;
   const double *pd;
   const smf_qual_t *pq;
   const smf_qual_t *qua;
   smf_qual_t mask;
   dim_t b1;
   dim_t b2;
   dim_t ib1;
   dim_t ib2;
   dim_t ibolo;
   dim_t itime;
   dim_t n;
   dim_t nb1;
   dim_t ntime;
   double *mean;
   double *sigma;
   double mval;
   double s1;
   double s2;
   double sval;
   int *ngood;
   size_t bstride1;
   size_t bstride2;
   size_t iv;
   size_t tstride;
   smfCollapseJobData *pdata;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Copy values into local variables. */
   pdata = (smfCollapseJobData *) job_data;
   array = pdata->array;
   b1 = pdata->b1;
   b2 = pdata->b2;
   qua = pdata->qua;
   mean = pdata->mean;
   sigma = pdata->sigma;
   ngood = pdata->ngood;
   nb1 = pdata->nb1;
   ntime = pdata->ntime;
   bstride1 = pdata->bstride1;
   bstride2 = pdata->bstride2;
   tstride = pdata->tstride;
   mask = pdata->mask;

/* Loop round all columsn to be collapsed by this thread. */
   for( ibolo = b1; ibolo <= b2; ibolo++ ) {

/* Get the pixel indices of the current column on the two axes spanned
   by the non-collapsed axes in the input cube. */
      ib2 = ibolo/nb1;
      ib1 = ibolo - ib2*nb1;

/* Get pointers to the first data and quality value in the column. */
      pd = array + ib1*bstride1 + ib2*bstride2;
      pq = qua ? qua + ib1*bstride1 + ib2*bstride2 : NULL;

/* Initialise running sums fore the current column. */
      s1 = 0.0;
      s2 = 0.0;
      n = 0;

/* Loop round all values in the column. */
      for( itime = 0; itime < ntime; itime++ ) {

/* If the current value is goo, add it into the running sums. */
         if( *pd != VAL__BADD && ( !qua || !(*pq & mask ) ) ) {
            s1 += *pd;
            s2 += (*pd)*(*pd);
            n++;
         }

/* Move pointers on to the next value in the column. */
         pd += tstride;
         pq += tstride;
      }

/* Calculate the mean and (sample) standard deviation for the current
   column. */
      if( n > 0 ) {
         mval = s1/n;
         if( n > 1 ) {
            sval = s2/n - mval*mval;
            sval *= ( (double) n )/( n - 1.0 );
            sval = sqrt( sval );
         } else {
            sval = 0.0;
         }
      } else {
         mval = VAL__BADD;
         sval = VAL__BADD;
      }

/* Get the vector index of the current column within the returned 2D arrays. */
      iv = ib1 + nb1*ib2;

/* Store the required values in the returned arrays. */
      if( mean ) mean[ iv ] = mval;
      if( sigma ) sigma[ iv ] = sval;
      if( ngood ) ngood[ iv ] = n;
   }
}











