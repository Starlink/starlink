/*
*+
*  Name:
*     smf_qualstats

*  Purpose:
*     Produce statistics on quality flagging in a single time series array.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_qualstats( ThrWorkForce *wf, smf_qfam_t qfamily, int nopad,
*                    const smf_qual_t *qual, dim_t nbolo, dim_t bstride,
*                    dim_t ntslice, dim_t tstride,
*                    dim_t qcount[SMF__NQBITS], dim_t *ngoodbolo,
*                    dim_t *nmap, dim_t *nmax, dim_t *tpad, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     qfamily = smf_qfam_t (Given)
*        Quality family associated with this quality array.
*     nopad = int (Given)
*        If true the padded region will not be included in the quality count.
*     qual = const smf_qual_t * (Given)
*        Pointer to quality array
*     nbolo = dim_t (Given)
*        Number of bolometers
*     bstride = dim_t (Given)
*        How many elements to skip to get to the next bolometer at a given
*        time slice.
*     ntslice = dim_t (Given)
*        Number of time slices
*     tstride = dim_t (Given)
*        How many elements to skip to get to the next time slice for the
*        current bolometer.
*     qcount = dim_t[SMF__NQBITS] (Returned)
*        Pointer to array that will count number of occurences of each
*        quality bit in qual. Will only use the number of elements determined
*        by the quality family.
*     ngoodbolo = dim_t* (Returned)
*        If specified, return number of bolometers that are flagged as good.
*     nmap = dim_t* (Returned)
*        If specified, return total number of samples that could be used
*        in the map (i.e., no quality bits in SMF__Q_GOOD set).
*     nmax = dim_t* (Returned)
*        If specified, return the maximum theoretical number of samples that
*        could be used for a map -- excluding only SMF__Q_PAD|SMF__Q_APOD
*        (padding/apodization).
*     tpad = dim_t * (Returned)
*        Number of slices of padding. Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Traverse the quality array of a time series and count the number
*     of occurrences of each quality bit.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-03-16 (EC):
*        Initial Version
*     2010-03-19 (EC):
*        Track samples that could go into the map (nmap, nmax)
*     2010-06-23 (TIMJ):
*        Add quality family support.
*     2010-07-16 (TIMJ):
*        Add ability to not include padded data. Add tpad parameter.
*     2010-10-08 (TIMJ):
*        Handle common cases with special code.
*     2012-5-31 (DSB):
*        Multi-thread.

*  Copyright:
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
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Prototypes for local static functions. */
static void smf1_qualstats( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfQualStatsData {
   dim_t b1;
   dim_t b2;
   dim_t bstride;
   dim_t nqbits;
   dim_t numgoodbolo;
   dim_t nummap;
   dim_t nummax;
   dim_t qcount[SMF__NQBITS];
   dim_t slice_end;
   dim_t slice_start;
   dim_t tstride;
   const smf_qual_t *qual;
} SmfQualStatsData;

#define FUNC_NAME "smf_qualstats"

void smf_qualstats( ThrWorkForce *wf, smf_qfam_t qfamily, int nopad,
                    const smf_qual_t *qual, dim_t nbolo, dim_t bstride,
                    dim_t ntslice, dim_t tstride, dim_t qcount[SMF__NQBITS],
                    dim_t *ngoodbolo, dim_t *nmap, dim_t *nmax,
                    dim_t *tpad, int *status ) {

  /* Local Variables */
  dim_t bolostep;               /* Number of bolos per thread */
  dim_t k;                     /* Loop counter */
  dim_t numgoodbolo=0;
  dim_t nummap=0;
  dim_t nummax=0;
  dim_t nqbits = 0;            /* Number of quality bits in this family */
  dim_t slice_start = 0;       /* First time slice to analyse */
  dim_t slice_end = 0;         /* last time slice */
  int nw;                       /* Number of worker threads */
  int iw;                       /* Thread index */
  SmfQualStatsData *job_data;
  SmfQualStatsData *pdata;

  /* init */
  if (tpad) *tpad = 0;
  if (nmax) *nmax = 0;
  if (nmap) *nmap = 0;
  if (ngoodbolo) *ngoodbolo = 0;

  /* Main routine */
  if (*status != SAI__OK) return;

  if( !qual ) {
    *status = SAI__ERROR;
     errRep(" ", FUNC_NAME
            ": NULL qual pointer supplied.", status);
    return;
  }

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Find how many bolometers to process in each worker thread. */
  bolostep = nbolo/nw;
  if( bolostep == 0 ) bolostep = 1;

  /* Initialize the counters */
  nqbits = smf_qfamily_count( qfamily, status );
  memset( qcount, 0, nqbits*sizeof(*qcount) );

  /* Determine start and end time slices */
  if (nopad) {
    smf_get_goodrange( qual, ntslice, tstride, SMF__Q_PAD, &slice_start,
                       &slice_end, status );
  } else {
    slice_start = 0;
    slice_end = ntslice-1;
  }

  /* Allocate job data for threads, and store the range of bolos to be
     processed by each one. Ensure that the last thread picks up any
     left-over bolos. */
  job_data = astCalloc( nw, sizeof(*job_data) );
  if( *status == SAI__OK ) {
    for( iw = 0; iw < nw; iw++ ) {
      pdata = job_data + iw;
      pdata->b1 = iw*bolostep;
      if( iw < nw - 1 ) {
         pdata->b2 = pdata->b1 + bolostep - 1;
      } else {
         pdata->b2 = nbolo - 1 ;
      }

      /* Store other values common to all jobs. */
      pdata->bstride = bstride;
      pdata->tstride = tstride;
      pdata->slice_start = slice_start;
      pdata->slice_end = slice_end;
      pdata->qual = qual;
      pdata->nqbits = nqbits;

      /* Submit the job to the workforce. */
      thrAddJob( wf, 0, pdata, smf1_qualstats, 0, NULL, status );
    }

    /* Wait for all jobs to complete. */
    thrWait( wf, status );

    /* Accumulate the results from all the worker threads. */
    for( iw = 0; iw < nw; iw++ ) {
       pdata = job_data + iw;
       numgoodbolo += pdata->numgoodbolo;
       nummap += pdata->nummap;
       nummax += pdata->nummax;
       for( k = 0; k < nqbits; k++ ) {
          qcount[ k ] += pdata->qcount[ k ];
       }
    }

    /* Free the job data. */
    job_data = astFree( job_data );
  }

  /* Return extra requested values */
  if( ngoodbolo ) {
    *ngoodbolo = numgoodbolo;
  }

  if( nmap ) {
    *nmap = nummap;
  }

  if( nmax ) {
    *nmax = nummax;
  }

  if (tpad) {
    if (nopad) {
      /* we got this directly */
      *tpad = slice_start +  ( (ntslice -1) - slice_end );
    } else {
      /* which bit is SMF__Q_PAD? */
      dim_t whichbit = smf_qual_to_bit( SMF__Q_PAD, status );
      *tpad = qcount[whichbit] / nbolo;
    }
  }

}






static void smf1_qualstats( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_qualstats

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_qualstats.

*  Invocation:
*     smf1_qualstats( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfQualStatsData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfQualStatsData *pdata;
   dim_t ibolo;
   dim_t itime;
   dim_t ibase;
   dim_t k;
   const smf_qual_t *pq;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfQualStatsData *) job_data_ptr;

/* Initialise returned values. */
   pdata->numgoodbolo = 0;
   pdata->nummap = 0;
   pdata->nummax = 0;
   memset( pdata->qcount, 0, pdata->nqbits*sizeof( *pdata->qcount ) );

/* Loop round all bolos to be processed by this thread, maintaining the
   index of the first time slice from the current bolo. */
   ibase = pdata->b1*pdata->bstride;
   for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {

/* Get a pointer to the first used quality value for the current bolo, and
   count the number of good bolometers. */
      pq = pdata->qual + ibase;
      if( !( *pq & SMF__Q_BADB ) ) pdata->numgoodbolo++;

/* Increment the pointer to the first slice to be used from the current
   bolometer. */
      pq += pdata->tstride*pdata->slice_start;

/* Loop round all time slices to be processed. */
      for( itime = pdata->slice_start; itime <= pdata->slice_end; itime++ ) {

/* Count samples for nmap and nmax */
         if( !(*pq & SMF__Q_GOOD) ) pdata->nummap++;
         if( !(*pq & SMF__Q_BOUND) ) pdata->nummax++;

/* If the quality is 0 then we already know the answer without looping over
   all the bits */
         if ( *pq != 0) {

/* Handle some of the simplest cases explicitly to prevent a loop over
   all bits. In cases where only a single bit is set this saves a lot of
   time. BADDA+BADBOL is also very common */
            switch( *pq ) {

               case BIT_TO_VAL(0):
                  pdata->qcount[0]++;
                  break;

               case BIT_TO_VAL(1):

/* we do not need to worry about exceeding qcount bounds since we will only
   be accessing it if the case statement is true */
                  pdata->qcount[1]++;
                  break;

               case (BIT_TO_VAL(0)|BIT_TO_VAL(1)):
                  pdata->qcount[0]++;
                  pdata->qcount[1]++;
                  break;

               default:

/* Loop over bits */
                  for( k = 0; k < pdata->nqbits; k++ ) {
                     if( *pq & BIT_TO_VAL(k) ) {
                        pdata->qcount[k]++;
                     }
                  }
            }
         }

/* Increment the quality pointer to the next time slice. */
         pq += pdata->tstride;
      }

/* Increment the index of the first value associated with the next
   bolometer. */
      ibase += pdata->bstride;
   }
}

