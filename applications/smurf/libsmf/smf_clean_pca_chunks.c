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
*                           double thresh, int submean, int sub, AstKeyMap *keymap,
*                           int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData * (Given)
*        Pointer to the input smfData. Any bad or flagged samples are
*        replaced by interpolated data before doing the PCA analysis (the
*        interpolated values are included in the analysis). The mean
*        bolometer values need not have been removed prior to calling
*        this function, as the means are removed following gap-filling.
*     chunklen = size_t (Given)
*        Chunk length for the PCA cleaning in time slices. If zero, a
*        single chunk covering the whole time series is used.
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
*     2016-09-28 (DSB):
*        Add argument "submean".
*     2016-10-12 (DSB):
*        Remove argument "submean". Remove gap-filling and mean-removal
*        (now both done in smf_clean_pca).

*  Copyright:
*     Copyright (C) 2016 East Asian Observatory.
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
  AstKeyMap *keymap;      /* Keymap containing parameters */
  double thresh;          /* PCA threshold */
  int oper;               /* Operation to perform */
  int sub;                /* Return cleaned data? (return components otherwise) */
  size_t t1;              /* Index of first time slice for chunk */
  size_t t2;              /* Index of last time slice */
  smfData *data;          /* The data to be processed */
} smfPCAChunkData;

void smfPCAChunkParallel( void *job_data_ptr, int *status );

void smfPCAChunkParallel( void *job_data_ptr, int *status ) {
  dim_t ntslice;
  size_t bstride;

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

  smf_get_dims( pdata->data, NULL, NULL, NULL, &ntslice, NULL, &bstride,
                NULL, status );

  /* Do PCA cleaning */
  if( pdata->oper == 0 ) {

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
                    0, 0.0, NULL, NULL, 0, pdata->sub, pdata->keymap,
                    ~SMF__Q_BADB, status );

     /* Debugging message indicating thread finished work */
     msgOutiff( MSG__DEBUG, "",
                "smfPCAChunkParallel: finished PCA cleaning time slices "
                "%zu -- %zu", status, pdata->t1, pdata->t2 );

  } else if( *status == SAI__OK ) {
     *status = SAI__ERROR;
     errRepf( "", "smf_clean_pca_chunks: Bad thread operation %d.",
              status, pdata->oper );
  }

}

/* ------------------------------------------------------------------------ */


#define FUNC_NAME "smf_clean_pca_chunks"

void smf_clean_pca_chunks( ThrWorkForce *wf, smfData *data, size_t chunklen,
                           double thresh, int sub, AstKeyMap *keymap, int *status ) {

  dim_t ntslice;                 /* Number of time slices */
  size_t i;                      /* Loop counter */
  size_t nchunks;                /* Number of chunks */
  smfPCAChunkData *job_data=NULL;/* job data */
  smfPCAChunkData *pdata=NULL;   /* Pointer to job data */

  if (*status != SAI__OK) return;

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

  if( *status == SAI__OK ) {
    if( chunklen && chunklen < 2 ) {
      *status = SAI__ERROR;
      errRep( " ", FUNC_NAME ": chunklen must be >= 2", status );
      return;
    }
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

  smf_get_dims( data, NULL, NULL, NULL, &ntslice, NULL, NULL, NULL, status );

  if( chunklen && ntslice < chunklen ) {
    *status = SAI__ERROR;
    errRepf( " ", FUNC_NAME ": smfData has length %zu < %zu (chunklen)",
             status, ntslice, chunklen );
  }

  /* Set up the division of labour for threads: independent blocks of time.
     Unlike much of SMURF, we're going to make proper use of the worker
     thread queue: we may submit more jobs than workers. We immediately
     submit jobs as they are set up. If, however, we only have a single
     chunk, let smf_clean_pca parallelize internally to make it go
     faster. */

  nchunks = chunklen ? ntslice / chunklen : 1;

  if( nchunks == 1 ) {
    msgOutif( MSG__VERB, "", FUNC_NAME
             ": only 1 chunk, smf_clean_pca will parallelize internally",
             status );
    smf_clean_pca( wf, data, 0, 0, thresh, 0, 0.0, NULL, NULL, 0, sub, keymap,
                   ~SMF__Q_BADB, status);
  } else {
    msgOutiff( MSG__VERB, "", FUNC_NAME
             ": will clean %zu separate time chunks", status, nchunks );
    job_data = astCalloc( nchunks, sizeof(*job_data) );

    for( i=0; (*status==SAI__OK)&&(i<nchunks); i++ ) {
      pdata = job_data + i;

      /* Blocks of time slices */
      pdata->t1 = i*chunklen;
      pdata->t2 = (i+1)*chunklen - 1;

      /* Ensure that the last thread picks up any left-over tslices */
      if( (i==(nchunks-1)) && (pdata->t1<(ntslice-1)) ) {
        pdata->t2 = ntslice - 1;
      }

      pdata->data = data;
      pdata->keymap = astCopy(keymap);
      pdata->thresh = thresh;
      pdata->sub = sub;
      pdata->oper = 0;

      thrAddJob( wf, 0, pdata, smfPCAChunkParallel, 0, NULL, status );
    }

    /* Wait until all of the submitted jobs have completed */
    thrWait( wf, status );

    /* Free resources. */
    for( i=0; (i<nchunks); i++ ) {
      pdata = job_data + i;
      if( pdata->keymap ) pdata->keymap = astAnnul( pdata->keymap );
    }
    job_data = astFree( job_data );
  }

}
