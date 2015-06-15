/*
*+
*  Name:
*     smf_clean_pca_chunks

*  Purpose:
*     Clean smfData by removing the strongest correlations using PCA in chunks

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_clean_pca_chunks( ThrWorkForce *wf, smfData *data, size_t chunklen,
*                           double thresh, int sub, AstKeyMap *keymap, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData * (Given)
*        Pointer to the input smfData (assume that bolometer means have been
*        removed)
*     chunklen = size_t (Given)
*        Chunk length for the PCA cleaning in time slices.
*     thresh = double (Given)
*        Outlier threshold for amplitudes to remove from data for cleaning
*     sub = int (Given)
*        If non-zero, the values returned in "data" are the supplied data
*        values minus the select PCA components. If zero, the values returned
*        in "data" are the select PCA components themselves.
*     keymap = AstKeyMap * (Given)
*        Keymap containing parameters that control how flagbad works. See
*        smf_find_gains for details.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is a wrapper function to the lower-level smf_clean_pca function.
*     It runs the PCA cleaning operation independently on every chunklen
*     samples (using parallelization). The very last chunk processed will
*     contain any left-over samples (i.e. it will process something in
*     the range chunklen to 2*chunklen-1).

*  Notes:
*     The input bolometer time series are assumed to have had their
*     means removed before entry.

*  Authors:
*     EC: Ed Chapin (UBC)
*     DSB: David Berry (JAC, Hawaii)

*  History:
*     2011-10-12 (EC):
*        Initial version
*     2012-11-12 (DSB):
*        Fill gaps in the data before cleaning.
*     2015-06-15 (DSB):
*        Add argument "sub".

*  Copyright:
*     Copyright (C) 2011 University of British Columbia.
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
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "prm_par.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* ------------------------------------------------------------------------ */
/* Local variables and functions */

/* Structure containing information about blocks of time slices to be
   processed by each thread. All threads read/write to/from mutually
   exclusive parts of data. */

typedef struct smfPCAChunkData {
  smfData *data;          /* The data to be processed */
  int ijob;               /* Job identifier */
  AstKeyMap *keymap;      /* Keymap containing parameters */
  size_t t1;              /* Index of first time slice for chunk */
  size_t t2;              /* Index of last time slice */
  double thresh;          /* PCA threshold */
  int sub;                /* Return cleaned data? (return components otherwise) */
} smfPCAChunkData;

/* Function to be executed in thread: FFT all of the bolos from b1 to b2 */

void smfPCAChunkParallel( void *job_data_ptr, int *status );

void smfPCAChunkParallel( void *job_data_ptr, int *status ) {
  dim_t ntslice;
  smfPCAChunkData *pdata;

  if( *status != SAI__OK ) return;

  /* Pointer to the data that this thread will process */
  pdata = job_data_ptr;

  /* Check for valid inputs */
  if( !pdata ) {
    *status = SAI__ERROR;
    errRep( "", "smfPCAParallel: No job data supplied", status );
    return;
  }

  if( !pdata->data ) {
    *status = SAI__ERROR;
    errRep( "", "smfPCAParallel: No smfData supplied for cleaning", status );
    return;
  }

  smf_get_dims( pdata->data, NULL, NULL, NULL, &ntslice, NULL, NULL, NULL,
                status );

  /* if t1 past end of the work, nothing to do so we return */
  if( pdata->t1 >= ntslice ) {
    msgOutif( MSG__DEBUG, "",
              "smfPCAParallel: nothing for thread to do, returning",
              status);
    return;
  }

  /* Debugging message indicating thread started work */
  msgOutf( "", "smfPCAChunkParallel: start PCA cleaning time slices %zu -- %zu",
           status, pdata->t1, pdata->t2 );

  /* PCA clean this chunk */
  smf_clean_pca( NULL, pdata->data, pdata->t1, pdata->t2, pdata->thresh,
                 NULL, NULL, 0, pdata->sub, pdata->keymap, status );

  /* Debugging message indicating thread finished work */
  msgOutiff( MSG__DEBUG, "",
             "smfPCAChunkParallel: finished PCA cleaning time slices "
             "%zu -- %zu", status, pdata->t1, pdata->t2 );

}

/* ------------------------------------------------------------------------ */


#define FUNC_NAME "smf_clean_pca_chunks"

void smf_clean_pca_chunks( ThrWorkForce *wf, smfData *data, size_t chunklen,
                           double thresh, int sub, AstKeyMap *keymap, int *status ) {

  size_t clen=0;          /* Local chunk length */
  size_t i;               /* Loop counter */
  smfPCAChunkData *job_data=NULL;/* job data */
  size_t nchunks;         /* Number of chunks */
  dim_t ntslice;          /* number of time slices */
  int nw;                 /* total available worker threads */
  smfPCAChunkData *pdata=NULL; /* Pointer to job data */

  if (*status != SAI__OK) return;

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Check for NULL smfData pointer */
  if( !data || !data->pntr[0]) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME
            ": possible programming error, NULL data supplied", status );
    return;
  }

  if( thresh < 0 ) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME
            ": thresh < 0", status );
    return;
  }

  smf_get_dims( data, NULL, NULL, NULL, &ntslice, NULL, NULL, NULL, status );


  if( *status == SAI__OK ) {
    if( !chunklen ) {
      clen = ntslice;
    } else {
      clen = chunklen;
    }

    if( clen < 2 ) {
      *status = SAI__ERROR;
      errRep( " ", FUNC_NAME
              ": chunklen must be >= 2", status );
      return;

    }
  } else {
    goto CLEANUP;
  }

  if( data->ndims != 3 ) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME
            ": possible programming error, smfData should be 3-dimensional",
            status );
    return;
  }

  if( data->dtype != SMF__DOUBLE ) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME
            ": possible programming error, smfData should be double precision",
            status );
    return;
  }

  if( ntslice < clen ) {
    *status = SAI__ERROR;
    errRepf( " ", FUNC_NAME ": smfData has length %zu < %zu (chunklen)",
             status, ntslice, clen );
    goto CLEANUP;
  }

  /* Fill any gaps or padding with interpolated data values plus noise. */
  smf_fillgaps( wf, data, SMF__Q_PAD | SMF__Q_GAP, status );

  /* Set up the division of labour for threads: independent blocks of time.
     Unlike much of SMURF, we're going to make proper use of the worker
     thread queue: we may submit more jobs than workers. We immediately
     submit jobs as they are set up. If, however, we only have a single
     chunk, let smf_clean_pca parallelize internally to make it go
     faster. */

  nchunks = ntslice / clen;

  if( nchunks == 1 ) {
    msgOutif( MSG__VERB, "", FUNC_NAME
             ": only 1 chunk, smf_clean_pca will parallelize internally",
             status );
    smf_clean_pca( wf, data, 0, 0, thresh, NULL, NULL, 0, sub, keymap, status);
  } else {
    msgOutiff( MSG__VERB, "", FUNC_NAME
             ": will clean %zu separate time chunks", status, nchunks );
    job_data = astCalloc( nchunks, sizeof(*job_data) );

    for( i=0; (*status==SAI__OK)&&(i<nchunks); i++ ) {
      pdata = job_data + i;

      /* Blocks of time slices */
      pdata->t1 = i*clen;
      pdata->t2 = (i+1)*clen - 1;

      /* Ensure that the last thread picks up any left-over tslices */
      if( (i==(nchunks-1)) && (pdata->t1<(ntslice-1)) ) {
        pdata->t2 = ntslice - 1;
      }

      pdata->data = data;
      pdata->keymap = astCopy(keymap);
      pdata->thresh = thresh;
      pdata->sub = sub;

      pdata->ijob = thrAddJob( wf, THR__REPORT_JOB, pdata, smfPCAChunkParallel,
                               0, NULL, status );
    }

    /* Wait until all of the submitted jobs have completed */
    thrWait( wf, status );
  }

 CLEANUP:
  if( job_data ) {
    for( i=0; (i<nchunks); i++ ) {
      pdata = job_data + i;
      if( pdata->keymap ) pdata->keymap = astAnnul( pdata->keymap );
    }
    job_data = astFree(job_data);
  }

}
