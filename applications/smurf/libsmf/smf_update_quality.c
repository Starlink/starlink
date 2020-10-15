/*
*+
*  Name:
*     smf_update_quality

*  Purpose:
*     Update the quality array associated with a smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_update_quality( ThrWorkForce *wf, smfData *data, int syncbad,
*                         const int *badmask, smf_qual_t addqual, double badfrac,
*                         int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     data = smfData* (Given)
*        Pointer to smfData that will contain the updated QUALITY array
*     syncbad = int (Given)
*        If set ensure that every bad pixel (VAL__BADx) in the data array
*        has a corresponding quality of SMF__Q_BADDA, and vice-versa.
*     badmask = const int* (Given)
*        Integer array with same dimensions as bolometers.
*        Each position that is bad will set SMF__Q_BADB for all data
*        for that detector. Can be NULL. The value for non-bad pixels does
*        not matter.
*     addqual = smf_qual_t (Given)
*        By default SMF__Q_BADB is used to indicate an entire bolometer has
*        been masked. This parameter allows additional quality to be associated
*        to allow the type of mask to be specified.
*     badfrac = double (Given)
*        If nonzero, fraction of samples for entire bolo to be flagged as bad
*        using SMF__Q_BADB.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine updates an existing QUALITY array. The quality
*     associated with the smfData will be modified. A
*     mask indicating which bolometers are bad and should be
*     completely ignored (SMF__Q_BADB) may be supplied. Additionally,
*     the routine will ensure that QUALITY has SMF__Q_BADDA set for
*     each bad data point (VAL__BADD). If no DATA or QUALITY
*     arrays are associated with the smfData bad
*     status is set (SAI__ERROR) and the function returns.

*  Notes:
*     - If badfrac is true but syncbad is false, the data array will be checked
*       for badness in addition to the quality array.

*  Authors:
*     EC: Ed Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-02-01 (EC):
*        Initial version.
*     2008-03-03 (EC):
*        Added target to interface
*     2008-03-25 (EC):
*        Added syncbad to interface
*     2008-12-01 (TIMJ):
*        - rewrite masking loop to use less code, especially in tordered
*          vs bolordered switching.
*        - no longer malloc a local mask array
*        - input mask is now const and also an int array
*        - sense of badness for mask has changed. BAD now means bad rather
*          than non-zero.
*        - remove requirement for DOUBLE
*     2008-12-03 (TIMJ):
*        Use modified smf_get_dims
*     2008-12-12 (TIMJ):
*        Check data array when badfrac is true but syncbad is false.
*     2010-03-19 (EC):
*        Rename SMF__Q_BADS to SMF__Q_BADDA
*     2010-07-06 (TIMJ):
*        Add ability to use additional quality for the output mask
*        and not just SMF__Q_BADB.
*     2010-07-07 (TIMJ):
*        New quality sidecar scheme
*     2014-01-14 (DSB):
*        Multi-thread.
*     2014-08-21 (DSB):
*        If syncbad is non-zero, assign bad data values to all BADDA samples.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008,2014 Science and Technology Faciltiies Council.
*     Copyright (C) 2008,2010 University of British Columbia.
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
#include "star/ndg.h"
#include "star/thr.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Other includes */

/* Prototypes for local static functions. */
static void smf1_update_quality( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfUpdateQualityData {
   const int *badmask;
   dim_t badthresh;
   dim_t ntslice;
   double *ddata;
   int *idata;
   int operation;
   int syncbad;
   dim_t b1;
   dim_t b2;
   dim_t bstride;
   dim_t i1;
   dim_t i2;
   dim_t tstride;
   smf_qual_t *qual;
   smf_qual_t addqual;
} SmfUpdateQualityData;

#define FUNC_NAME "smf_update_quality"

void smf_update_quality( ThrWorkForce *wf, smfData *data, int syncbad,
			 const int *badmask, smf_qual_t addqual, double badfrac,
			 int *status ) {

  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Number of data points */
  dim_t ntslice;                /* Number of time slices */
  dim_t bstride;               /* bol stride */
  dim_t tstride;               /* time slice stride */
  smf_qual_t *qual=NULL;        /* Pointer to the QUALITY array */
  SmfUpdateQualityData *job_data = NULL;
  SmfUpdateQualityData *pdata;
  int nw;
  dim_t istep;
  dim_t bstep;
  int iw;

  if ( *status != SAI__OK ) return;

  /* Check for QUALITY */
  qual = smf_select_qualpntr( data, NULL, status );
  if (!qual) {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "smfData does not contain a QUALITY component",
              status);
    }
    return;
  }

  /* Check for DATA */
  if( !data->pntr[0] ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "smfData does not contain a DATA component", status );
    return;
  }

  /* Check for valid badfrac */
  if( (badfrac < 0) || (badfrac > 1) ) {
    msgSeti( "BADFRAC", badfrac );
    errRep(FUNC_NAME,
           "Invalid badfrac: ^BADFRAC. Must be in range (0 -- 1).", status);
  }

  /* Calculate data dimensions */
  smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, &ndata, &bstride,
                &tstride, status );

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Find how many elements to process in each worker thread. */
  istep = ndata/nw;
  if( istep == 0 ) istep = 1;

  /* Find how many bolometers to process in each worker thread. */
  bstep = nbolo/nw;
  if( bstep == 0 ) bstep = 1;

  /* Allocate job data for threads, and store common values. Ensure that the
     last thread picks up any left-over elements or bolometers.  */
  job_data = astCalloc( nw, sizeof(*job_data) );
  if( *status == SAI__OK ) {
    for( iw = 0; iw < nw; iw++ ) {
      pdata = job_data + iw;
      pdata->i1 = iw*istep;
      pdata->b1 = iw*bstep;
      if( iw < nw - 1 ) {
        pdata->i2 = pdata->i1 + istep - 1;
        pdata->b2 = pdata->b1 + bstep - 1;
      } else {
        pdata->i2 = ndata - 1;
        pdata->b2 = nbolo - 1;
      }
      pdata->qual = qual;
    }
  }

  if( *status == SAI__OK ) {
    /* some pointers to the data array if needed */
    double * ddata = NULL;
    int * idata = NULL;

    /* we will need the data array if we are checking it for bad values
       or looking for bad fraction */
    if (syncbad || badfrac) {
      smf_select_pntr( data->pntr, data->dtype, &ddata, NULL,
                       &idata, NULL, status);
    }

    /* Synchronize SMF__Q_BADDA quality and VAL__BADD in data array */
    if( syncbad ) {
      if (data->dtype == SMF__DOUBLE) {
        for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->operation = 1;
            pdata->ddata = ddata;
            thrAddJob( wf, 0, pdata, smf1_update_quality, 0, NULL, status );
        }
        thrWait( wf, status );

      } else if (data->dtype == SMF__INTEGER) {
        for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->operation = 2;
            pdata->idata = idata;
            thrAddJob( wf, 0, pdata, smf1_update_quality, 0, NULL, status );
        }
        thrWait( wf, status );

      } else {
        msgSetc( "TYP", smf_dtype_string( data, status ));
        *status = SAI__ERROR;
        errRep( "",FUNC_NAME " data is of unsupported type (^TYP)",
                status);
        return;
      }
    }

    /* Apply badmask if available */
    if( badmask || badfrac ) {

      /* calculate the badfraction threshold in terms of number of bad
         found so that we do not have to continually divide to calculate
         the current fraction */
      dim_t badthresh = ntslice;
      /* special case 0 */
      if (badfrac) badthresh = badfrac * (double)ntslice;

      /* Submit the jobs and wait for them all to finish. */
      for( iw = 0; iw < nw; iw++ ) {
          pdata = job_data + iw;
          pdata->operation = 3;
          pdata->badthresh = badthresh;
          pdata->addqual = addqual;
          pdata->ntslice = ntslice;
          pdata->bstride = bstride;
          pdata->tstride = tstride;
          pdata->badmask = badmask;
          pdata->syncbad = syncbad;
          pdata->idata = idata;
          pdata->ddata = ddata;
          thrAddJob( wf, 0, pdata, smf1_update_quality, 0, NULL, status );
      }
      thrWait( wf, status );
    }
  }

  job_data = astFree( job_data );
}



static void smf1_update_quality( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_update_quality

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_update_quality.

*  Invocation:
*     smf1_update_quality( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfUpdateQualityData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfUpdateQualityData *pdata;
   const int *p4;
   double *p1;
   int *p3;
   dim_t b1;
   dim_t b2;
   dim_t i1;
   dim_t i2;
   dim_t i;
   smf_qual_t *p2;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfUpdateQualityData *) job_data_ptr;

   i1 = pdata->i1;
   i2 = pdata->i2;
   b1 = pdata->b1;
   b2 = pdata->b2;

   if( pdata->operation == 1 ){
      p1 = pdata->ddata + i1;
      p2 = pdata->qual + i1;
      for( i = i1; i <= i2; i++,p1++,p2++) {
         if( *p1 == VAL__BADD ) {
            *p2 |= SMF__Q_BADDA;
         } else if( *p2 & SMF__Q_BADDA ) {
            *p1 = VAL__BADD;
         }
      }

   } else if( pdata->operation == 2 ){
      p3 = pdata->idata + i1;
      p2 = pdata->qual + i1;
      for( i = i1; i <= i2; i++,p3++,p2++) {
         if( *p3 == VAL__BADI ) {
            *p2 |= SMF__Q_BADDA;
         } else if( *p2 & SMF__Q_BADDA ) {
            *p3 = VAL__BADI;
         }
      }

   } else if( pdata->operation == 3 ){
      dim_t badthresh = pdata->badthresh;
      dim_t j;
      dim_t nbad;
      dim_t ntslice = pdata->ntslice;
      int syncbad = pdata->syncbad;
      dim_t bstride = pdata->bstride;
      dim_t tstride = pdata->tstride;
      smf_qual_t addqual = pdata->addqual;

/* Loop over detector */
      p1 = pdata->ddata;
      p2 = pdata->qual;
      p3 = pdata->idata;
      p4 = pdata->badmask;

      for( i = b1; i <= b2; i++ ) {
         dim_t c = bstride * i;  /* constant offset for this bolometer */
         int isbad = 0;

/* preset bad flag based on mask (if defined) */
         if( p4 && p4[ i ] == VAL__BADI) isbad = 1;

/* Update badmask if badfrac specified */
         if( ( badthresh < ntslice ) && !isbad ) {
            nbad = 0;

/* Loop over samples and count the number with SMF__Q_BADDA set. Note that
   if syncbad is false we also check the data array. */
            for( j = 0; j < ntslice; j++ ) {
               dim_t ind = tstride*j + c;
               if( p2[ ind ] & SMF__Q_BADDA ) {
                  nbad++;
               } else if( !syncbad ) {
                  if( p3 && p3[ ind ] == VAL__BADI) {
                     nbad++;
                  } else if( p1 && p1[ ind ] == VAL__BADD) {
                     nbad++;
                  }
               }
            }
            if( nbad > badthresh ) isbad = 1;
         }

/* Now apply the badmask */
         if( isbad ) {
            smf_qual_t outqual = SMF__Q_BADB | addqual;
            for( j = 0; j < ntslice; j++ ) {
               p2[ tstride*j + c ] |= outqual;
            }
         }
      }

   } else {
      *status = SAI__ERROR;
      errRepf( "", "smf1_update_quality: Invalid operation (%d) supplied.",
               status, pdata->operation );
   }
}



