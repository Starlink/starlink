
/*
*+
*  Name:
*     smf_check_quality

*  Purpose:
*     Check for consistency between quality and data array

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     dim_t smf_check_quality( ThrWorkForce *wf, smfData *data,
*                               int showbad, int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     data = smfData* (Given)
*        Pointer to smfData
*     showbad = int (Given)
*        If set, display locations where they where inconsistencies found.
*     status = int* (Given and Returned)
*        Pointer to global status.

* Return Value:
*     dim_t = count of inconsistent samples found.

*  Description:
*     Traverse the data and quality arrays. Any VAL__BADD encountered without
*     SMF__Q_BADDA set are inconsistencies. Also, any non-finite
*     values in the data array other than VAL__BADD (e.g. inf/NaN) are
*     considered inconsistencies as they should have been converted
*     previously using smf_convert_bad. Finally, every instance of
*     SMF__Q_BADDA flag must match a VAL__BADD in the data array.

*  Notes:
*     Will only handle SMF__DOUBLE data type. Will set bad status otherwise.
*     Returns 0 if bad status.

*  Authors:
*     EC: Ed Chapin (UBC)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-03-31 (EC):
*        Initial version.
*     2010-10-08 (TIMJ):
*        Make 3 times faster:
*         - loop over a single index rather than over bolos and time slices
*         - chain if tests rather than doing all 3 every time
*     2011-04-06 (TIMJ):
*        Summarize where the bad values are coming from in VERBOSE mode.
*     2012-05-22 (DSB):
*        Multi-thread the chi-squared calculation.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011-2012 Science & Technology Facilities Council.
*     Copyright (C) 2010 University of British Columbia.
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
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_check_quality"

/* Prototypes for local static functions. */
static void smf1_check_quality( void *job_data_ptr, int *status );
static void smf__index_to_tbol( dim_t bstride, dim_t tstride, dim_t bufpos,
                                dim_t *bolnum, dim_t *tslice, int *status );

/* Local data types */
typedef struct smfCheckQualityData {
   double *d;
   int showbad;
   dim_t bstride;
   dim_t d1;
   dim_t d2;
   dim_t nbad;
   dim_t ninf;
   dim_t nnan;
   dim_t nqualincon;
   dim_t tstride;
   smf_qual_t *qual;
} SmfCheckQualityData;



dim_t smf_check_quality( ThrWorkForce *wf, smfData *data, int showbad,
                          int *status ) {

  double *d=NULL;               /* Pointer to data array */
  dim_t nbad=0;                /* inconsistency counter */
  dim_t nnan = 0;              /* Number of nan values found */
  dim_t ninf = 0;              /* Number of inf values found */
  dim_t nqualincon = 0;        /* Number of inconsistent bad/qual */
  dim_t ndata;                  /* Number of data points */
  dim_t bstride;               /* bol stride */
  dim_t tstride;               /* time slice stride */
  smf_qual_t *qual=NULL;        /* Pointer to the QUALITY array */
  int nw;                       /* Number of worker threads */
  int iw;                       /* Thread index */
  SmfCheckQualityData *job_data = NULL;  /* Array of job descriptions */
  SmfCheckQualityData *pdata;   /* Pointer to next job description */
  dim_t sampstep;              /* Number of samples per thread */

  if ( *status != SAI__OK ) return 0;

  /* Check for DATA */
  if( !data ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL data supplied", status );
    return 0;
  }

  if( !data->pntr[0] ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData does not contain a DATA component",
            status );
    return 0;
  }

  if( data->dtype != SMF__DOUBLE ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData does not have type SMF__DOUBLE", status );
    return 0;
  }

  d = (double *) data->pntr[0];

  /* Check for QUALITY */
  qual = smf_select_qualpntr( data, NULL, status );

  if( !qual ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL quality supplied", status);
    return 0;
  }


  /* Calculate data dimensions */
  smf_get_dims( data,  NULL, NULL, NULL, NULL, &ndata, &bstride,
                &tstride, status );

  if( *status == SAI__OK ) {

    /* How many threads do we get to play with */
    nw = wf ? wf->nworker : 1;

    /* Find how many samples to process in each worker thread. */
    sampstep = ndata/nw;
    if( sampstep == 0 ) sampstep = 1;

    /* Allocate job data for threads, and store the range of samples to be
       processed by each one. Ensure that the last thread picks up any
       left-over samples. */
    job_data = astCalloc( nw, sizeof(*job_data) );
    if( *status == SAI__OK ) {
      for( iw = 0; iw < nw; iw++ ) {
        pdata = job_data + iw;
        pdata->d1 = iw*sampstep;
        if( iw < nw - 1 ) {
          pdata->d2 = pdata->d1 + sampstep - 1;
        } else {
          pdata->d2 = ndata - 1 ;
        }

        /* Store other values common to all jobs. */
        pdata->qual = qual;
        pdata->d = d;
        pdata->showbad = showbad;
        pdata->bstride = bstride;
        pdata->tstride = tstride;

        /* Submit the job to the workforce. */
        thrAddJob( wf, 0, pdata, smf1_check_quality, 0, NULL, status );
      }

      /* Wait for all jobs to complete. */
      thrWait( wf, status );

      /* Accumulate the results from all the worker threads. */
      for( iw = 0; iw < nw; iw++ ) {
        pdata = job_data + iw;
        nbad += pdata->nbad;
        nnan += pdata->nnan;
        ninf += pdata->ninf;
        nqualincon += pdata->nqualincon;
      }

      /* Free the job data. */
      job_data = astFree( job_data );
    }
  }

  if (nbad > 0) {
    msgOutiff( MSG__VERB, "", "Quality inconsistency found: %zu DATA/QUAL, %zu NaN, %zu Inf",
               status, nqualincon, nnan, ninf );
  }

  return nbad;
}


static void smf__index_to_tbol( dim_t bstride, dim_t tstride, dim_t bufpos,
                                dim_t *bolnum, dim_t *tslice, int *status ) {

  dim_t bol = 0;
  dim_t tpos = 0;

  if (bolnum) *bolnum = 0;
  if (tslice) *tslice = 0;
  if (*status != SAI__OK) return;

  if (tstride == 1) {

    /* integer arithmetic - truncate the int */
    bol  = bufpos / bstride;
    tpos = bufpos - ( bol * bstride );


  } else if (bstride == 1) {

    tpos = bufpos / tstride;
    bol  = bufpos - ( tpos * tstride );

  } else {
    *status = SAI__ERROR;
    errRep( "", "One of bstride and tstride must be 1", status );
  }

  /* Sanity check */
  if (*status == SAI__OK) {
    dim_t ij;
    ij = bol * bstride + tpos * tstride;

    if (ij != bufpos) {
      *status = SAI__ERROR;
      errRepf("", "Internal error calculating bolometer and time slice from index "
              " (%zu != %zu from bstride of %zu and tstride of %zu)",
              status, bufpos, ij, bstride, tstride );
    }
  }

  if (bolnum) *bolnum = bol;
  if (tslice) *tslice = tpos;

}




static void smf1_check_quality( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_check_quality

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_check_quality.

*  Invocation:
*     smf1_check_quality( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfCheckQualityData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfCheckQualityData *pdata;
   double *pd;
   int badqual;
   int isbad;
   dim_t bolnum;
   dim_t i;
   dim_t tslice;
   smf_qual_t *pq;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfCheckQualityData *) job_data_ptr;

/* Initialise returned chisquared increment and count of values. */
   pdata->nbad = 0;
   pdata->nnan = 0;
   pdata->ninf = 0;
   pdata->nqualincon = 0;

/* Initialise pointers to the first data and quality value to be
   processed by this thread. */
   pq = pdata->qual + pdata->d1;
   pd = pdata->d + pdata->d1;

/* Loop round all values to be processed by this thread. */
   for( i = pdata->d1; i <= pdata->d2; i++, pq++,pd++ ) {
      isbad = 0;
      badqual = (*pq) & SMF__Q_BADDA;

/* Do the finite test first since if it is not finite there is no point
   testing if it is BADD as well */
      if( !isfinite( *pd ) ) {
         isbad = 1;

         if( isnan( *pd ) ) {
            pdata->nnan++;
         } else {
            pdata->ninf++;
         }

         if( pdata->showbad ) {
            smf__index_to_tbol( pdata->bstride, pdata->tstride, i, &bolnum,
                                &tslice, status );
            msgOutf( "", "b%zu t%zu: non-finite %s value encountered",
                     status, bolnum, tslice, (isnan(*pd) ? "NaN" : "Inf") );
         }

      } else if( *pd == VAL__BADD && !badqual ) {
         isbad = 1;
         if( pdata->showbad ) {
            smf__index_to_tbol( pdata->bstride, pdata->tstride, i, &bolnum,
                                &tslice, status );
            msgOutf( "", "b%zu t%zu: VAL__BADD without SMF__Q_BADDA",
                     status, bolnum, tslice );
         }

      } else if( badqual && *pd != VAL__BADD ) {
         isbad = 1;
         pdata->nqualincon++;
         if( pdata->showbad ) {
            smf__index_to_tbol( pdata->bstride, pdata->tstride, i, &bolnum,
                                &tslice, status );
            msgOutf( "", "b%zu t%zu: SMF__Q_BADDA without VAL__BADD",
                     status, bolnum, tslice );
         }

      }
      if( isbad ) pdata->nbad++;
   }
}

