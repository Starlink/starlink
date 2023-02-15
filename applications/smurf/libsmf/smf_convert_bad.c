/*
*+
*  Name:
*     smf_convert_bad

*  Purpose:
*     Convert inf/NaN values to VAL__BADD in smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_convert_bad( ThrWorkForce *wf, smfData *data, int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     data = smfData* (Given)
*        Pointer to double precision smfData to be updated
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine checks DATA and VARIANCE components for inf/NaN and changes
*     them to VAL__BADD so that they can be recognized by SMURF

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-11-03 (EC):
*        Initial version.
*     2012-05-26 (DSB):
*        Multi-thread.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia. All Rights Reserved.

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
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Other includes */

/* Prototypes for local static functions. */
static void smf1_convert_bad( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfConvertBadData {
   double *d;
   dim_t d1;
   dim_t d2;
} smfConvertBadData;

#define FUNC_NAME "smf_convert_bad"

void smf_convert_bad( ThrWorkForce *wf, smfData *data, int *status ) {
  dim_t i;                      /* loop counter */
  dim_t ndata;                  /* Number of data points */
  double *val=NULL;             /* Pointer to data */
  int nw;                       /* Number of worker threads */
  int iw;                       /* Thread index */
  smfConvertBadData *job_data = NULL;  /* Array of job descriptions */
  smfConvertBadData *pdata;     /* Pointer to next job description */
  dim_t sampstep;              /* Number of samples per thread */



  if ( *status != SAI__OK ) return;

  /* Check for NULL pointer */
  if( !data ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData pointer is NULL", status );
    return;
  }

  /* Check for double-precision data */
  if( data->dtype != SMF__DOUBLE ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Data is not double precision.",
            status );
    return;
  }

  /* Data dimensions */
  ndata = 1;
  for( i=0; i<data->ndims; i++ ) ndata *= data->dims[i];

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Find how many samples to process in each worker thread. */
  sampstep = ndata/nw;
  if( sampstep == 0 ) {
     sampstep = 1;
     nw = (int) ndata;
  }

  /* Allocate job data for threads. */
  job_data = astCalloc( nw, sizeof(*job_data) );
  if( *status == SAI__OK ) {

  /* Convert +/-inf or NaN to VAL__BADD. Loop to do data and variance
     arrays.  */
    for( i=0; i<2; i++ ) {
      val = data->pntr[i];
      if( val ) {

        for( iw = 0; iw < nw; iw++ ) {
          pdata = job_data + iw;
          pdata->d1 = iw*sampstep;
          if( iw < nw - 1 ) {
            pdata->d2 = pdata->d1 + sampstep - 1;
          } else {
            pdata->d2 = ndata - 1 ;
          }
          pdata->d = val;

          /* Submit the job to the workforce. */
          thrAddJob( wf, 0, pdata, smf1_convert_bad, 0, NULL, status );
        }

        /* Wait for all jobs to complete. */
        thrWait( wf, status );
      }
    }

    /* Free job data */
    job_data = astFree( job_data );
  }
}



static void smf1_convert_bad( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_convert_bad

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_convert_bad.

*  Invocation:
*     smf1_convert_bad( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = smfConvertBadData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   smfConvertBadData *pdata;
   double *pd;
   dim_t i;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (smfConvertBadData *) job_data_ptr;

/* Loop round all values to be processed by this thread. */
   pd = pdata->d + pdata->d1;
   for( i = pdata->d1; i <= pdata->d2; i++,pd++ ) {

/* If the value is infinite set it to VAL__BADD. */
      if( !isfinite( *pd ) ) *pd = VAL__BADD;

   }
}

