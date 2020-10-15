
/*
*+
*  Name:
*     SC2THREADTEST

*  Purpose:
*     Task for testing speeds of different threading schemes

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_sc2threadtest( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine tests schemes for visiting large quantities of
*     SCUBA-2 data using multiple threads. This is a developer tool.

*  ADAM Parameters:
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     NCHUNKS = _INTEGER (Read)
*          Number of time chunks. [2]
*     NSUB = _INTEGER (Read)
*          Number of subarrays. [4]
*     NTHREAD = _INTEGER (Read)
*          Number of threads to use. [2]
*     TSTEPS = _INTEGER (Read)
*          Number of time samples in simulated data chunk. [6000]

*  Authors:
*     Edward Chapin (UBC)
*     COBA: Coskun Oba (UoL)
*     {enter_new_authors_here}

*  History:
*     2008-11-14 (EC):
*        Initial version -- copy thread infrastructure from makemap
*     2010-01-15 (EC):
*        Add test for merging status from several threads
*     2010-09-21 (COBA):
*        Add SMF__NOCREATE_FTS

*  Copyright:
*     Copyright (C) 2008-2010 University of British Columbia.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>
#include <sys/time.h>
#include <time.h>
#include <math.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/kaplibs.h"
#include "star/atl.h"
#include "star/one.h"
#include "star/thr.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"
#include "smurf_typ.h"
#include "libsmf/smf_typ.h"

#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"

#include "libsc2sim/sc2sim.h"

#define FUNC_NAME "smurf_sc2threadtest"
#define TASK_NAME "SC2THREADTEST"

/* --------------------------------------------------------------------------*/

/* Structure used to pass data divided into time-chunks to different threads */
typedef struct smfTimeChunkData {
  int type;                 /* 0=reorder, 1=boxcar smooth */
  dim_t chunk1;            /* Index of first chunk handled by this thread */
  dim_t chunk2;            /* Index of last chunk handled by this thread */
  smfArray **data;          /* Pointer to master array of smfArrays */
  int ijob;                 /* Job identifier */
  dim_t nchunks;           /* Total number of chunks in data */
} smfTimeChunkData;


/* Function that does some simple math on every sample within the relevant
   range for a single thread -- uses parallel processing of different
   time chunks */

void smfParallelTime( void *job_data_ptr, int *status );

void smfParallelTime( void *job_data_ptr, int *status ) {
  smfArray **array=NULL;       /* array of smfArrays that we're working on */
  smfTimeChunkData *data=NULL; /* Pointer to job data */
  smfFilter *filt=NULL;        /* Frequency domain filter */
  dim_t i;                    /* Loop counter */
  dim_t j;                    /* Loop counter */
  dim_t k;                    /* Loop counter */
  dim_t nbolo;                 /* Number of bolos in smfData */
  dim_t ndata;                 /* Size of DATA component in smfData */
  dim_t ntslice;               /* Number of time slices in smfData */
  dim_t nsub;                  /* Number of subarrays */
  double *val=NULL;            /* Pointer to DATA component of smfData */

  if( *status != SAI__OK ) return;

  /* Pointer to the data that this thread will process */
  data = job_data_ptr;

  /* Check for valid inputs */
  if( !data ) {
    *status = SAI__ERROR;
    errRep( "", "smfParallelTime: job_data_ptr is NULL.", status );
    return;
  }

  if( !(array=data->data) ) {
    *status = SAI__ERROR;
    errRep( "", "smfParallelTime: data array is NULL.", status );
    return;
  }

  /* Message indicating the thread started */
  msgSetk( "C1", data->chunk1);
  msgSetk( "C2", data->chunk2);
  msgOutif( MSG__DEBUG, "",
            "-- parallel time: thread starting on chunks ^C1 -- ^C2",
            status );

  /* Loop over time chunk. Some chunks may be flagged to skip if
     chunk1=nchunks */
  for( i=data->chunk1; (i<=data->chunk2)&&(i<data->nchunks)&&(*status==SAI__OK);
       i++ ) {

    nsub = array[i]->ndat;

    /* Initialize filter -- assume same data dimensions for all subarrays */
    if( data->type == 2 ) {
      filt = smf_create_smfFilter( array[i]->sdata[0], status );
      smf_filter_ident( filt, 1, status );
    }

    /* Loop over subarray */
    for( j=0; (j<nsub)&&(*status==SAI__OK); j++ ) {
      /* Change to bolo-ordered data */

      if( data->type == 0 ) {
        /* --- Re-order the data --- */
        smf_dataOrder( NULL, array[i]->sdata[j], 0, status );
      }

      smf_get_dims( array[i]->sdata[j], NULL, NULL, &nbolo, &ntslice, &ndata,
                    NULL, NULL, status );

      if( data->type == 1 ) {
        /* --- Boxcar smooth the data --- */
        for( k=0; (*status==SAI__OK)&&(k<nbolo); k++ ) {
          val = array[i]->sdata[j]->pntr[0];
          val += k*ntslice;

          /* Boxcar smooth each bolometer by 500 samples */
          smf_boxcar1D( val, ntslice, 1, 500, NULL, 0, 1, NULL, status );
        }
      }

      if( data->type == 2 ) {
        /* --- FFT filter the data --- */
        smf_filter_execute( NULL, array[i]->sdata[j], filt, 0, 0, status );
      }
    }

    if( filt ) filt = smf_free_smfFilter( filt, status );
  }


  /* Message indicating the thread started */
  msgSetk( "C1", data->chunk1);
  msgSetk( "C2", data->chunk2);
  msgOutif( MSG__DEBUG, "",
            "-- parallel time: thread finished chunks ^C1 -- ^C2",
            status );

  /* Try setting some status in thread to test merging mechanism */
  *status = SMF__INSMP;
  errRepf( " ", "Set SMF__INSMP in thread chunks %zu -- %zu", status,
           data->chunk1, data->chunk2 );
}

/* --------------------------------------------------------------------------*/

void smurf_sc2threadtest( int *status ) {

  /* Local Variables */
  smfArray **res=NULL;       /* array of smfArrays of test data */
  smfData *data=NULL;        /* Pointer to SCUBA2 data struct */
  dim_t datalen;             /* Number of data points */
  smfFilter *filt=NULL;      /* Frequency domain filter */
  dim_t i;                  /* Loop counter */
  dim_t j;                  /* Loop counter */
  smfTimeChunkData *job_data=NULL; /* Array of pointers for job data */
  dim_t joblen;             /* Number of chunks per job */
  dim_t k;                  /* Loop counter */
  dim_t nchunks;            /* Number of chunks */
  dim_t nsub;               /* Number of subarrays */
  int nthread;               /* Number of threads */
  smfTimeChunkData *pdata=NULL; /* Pointer to data for single job */
  int temp;                  /* Temporary integer */
  dim_t tsteps;             /* How many time steps in chunk */
  struct timeval tv1, tv2;   /* Timers */
  ThrWorkForce *wf = NULL;   /* Pointer to a pool of worker threads */

  double *dat=NULL;
  dim_t nbolo;
  dim_t ntslice;
  dim_t ndata;
  dim_t bstride;
  dim_t tstride;
  dim_t offset;

  if (*status != SAI__OK) return;

  /* Get input parameters */
  parGdr0i( "NTHREAD", 1, 1, NUM__MAXI, 1, &nthread, status );
  parGdr0i( "TSTEPS", 6000, 0, NUM__MAXI, 1, &temp, status );
  tsteps = (dim_t) temp;
  parGdr0i( "NCHUNKS", 1, 1, NUM__MAXI, 1, &temp, status );
  nchunks = (dim_t) temp;
  parGdr0i( "NSUB", 1, 1, 4, 1, &temp, status );
  nsub = (dim_t) temp;

  msgSeti("N",nthread);
  msgOut( "", TASK_NAME ": Running test with ^N threads", status );

  /*** TIMER ***/
  smf_timerinit( &tv1, &tv2, status );

  /* Create some fake test data in the form of an array of smfArrays */

  msgSetk("T",tsteps);
  msgSetk("C",nchunks);
  msgSetk("NS",nsub);
  msgOut( "", TASK_NAME
          ": Creating ^NS subarrays of data with ^C chunks * ^T samples",
          status );

  res = astCalloc( nchunks, sizeof(*res) );

  for( k=0; (*status==SAI__OK)&&(k<nchunks); k++ ) {

    res[k] = smf_create_smfArray( status );

    for( i=0; (*status==SAI__OK)&&(i<nsub); i++ ) {
      /* Create individual smfDatas and add to array */
      data = smf_create_smfData( SMF__NOCREATE_FILE |
                                 SMF__NOCREATE_DA |
                                 SMF__NOCREATE_FTS, status );

      if( *status==SAI__OK ) {
        data->dtype=SMF__DOUBLE;
        data->ndims=3;
        data->dims[0]=40;
        data->dims[1]=32;
        data->dims[2]=(dim_t) tsteps;
        datalen=1;
        data->isFFT=-1;
        for( j=0; j<data->ndims; j++ ) datalen *= data->dims[j];

        data->hdr->steptime = 0.005;

        data->pntr[0] = astCalloc( datalen, smf_dtype_sz(data->dtype,status) );
        data->qual = astCalloc( datalen, sizeof(*data->qual) );
      }

      smf_addto_smfArray( res[k], data, status );
    }
  }

  /*** TIMER ***/
  msgOutf( "", "** %f seconds generating data", status,
           smf_timerupdate(&tv1,&tv2,status) );

  msgOut( "", TASK_NAME
          ": Starting test 1 __parallel time: dataOrder__", status );

  /* Create a pool of threads. */
  wf = thrGetWorkforce( nthread, status );

  /* Work out number of chunks per thread */
  joblen = nchunks/nthread;
  if( joblen == 0 ) joblen = 1; /* At least one chunk per thread */

  /* The first test will process separate time chunks of data in
     parallel, re-ordering each to bolo-ordered format. All subarrays
     and an integer number of input file chunks all go into a single
     thread. Start by allocating and initializing a number of
     smfTimeChunkData's that hold the information required for each
     thread */

  job_data = astCalloc( nthread, sizeof(*job_data) );

  for( i=0; (i<(dim_t)nthread) && (*status==SAI__OK); i++ ) {
    pdata = job_data + i;

    pdata->type = 0;                /* Start with a data re-order */
    pdata->data = res;              /* Pointer to main data array */
    pdata->chunk1 = i*joblen;       /* Index of first chunk for job */
    pdata->nchunks = nchunks;       /* Total number of time chunks in data */
    pdata->ijob = -1;               /* Flag job as available to do work */

    /* The last thread has to pick up the remainder of chunks */
    if( i==(dim_t)(nthread-1) ) pdata->chunk2=nchunks-1;
    else pdata->chunk2 = (i+1)*joblen-1; /* Index of last chunk for job */

    /* Ensure a valid chunk range, or set to a length that we know to ignore */
    if( pdata->chunk1 >= nchunks ) {
      pdata->chunk1 = nchunks;
      pdata->chunk2 = nchunks;
    } else if( pdata->chunk2 >= nchunks ) {
      pdata->chunk2 = nchunks-1;
    }

    if( pdata->chunk1 >= nchunks ) {
      /* Nothing for this thread to do */
      msgSetk( "W", i+1);
      msgOutif( MSG__DEBUG, "",
                "-- parallel time: skipping thread ^W, nothing to do",
                status);
    } else {
      /* Since we know there is one job_data per thread, just submit jobs
         immediately */
      pdata->ijob = thrAddJob( wf, THR__REPORT_JOB, pdata, smfParallelTime,
                                 0, NULL, status );
    }
  }

  /* Wait until all of the submitted jobs have completed */
  thrWait( wf, status );

  /* Annul the bad status that we set in smfParallelTime */
  if( *status == SMF__INSMP ) {
    errAnnul( status );
    msgOut( "", " *** Annulled SMF__INSMP set in smfParallelTime *** ",
            status );
  } else {
    msgOut( "", " *** Flushing good status *** ", status );
    errFlush( status );
  }

  /*** TIMER ***/
  msgOutf( "", "** %f seconds to complete test", status,
           smf_timerupdate(&tv1,&tv2,status) );

  /* The second test will boxcar smooth bolometers from time chunks in
     parallel */

  msgOut( "", TASK_NAME
          ": Starting test 2 __parallel time: boxcar smooth__", status );

  for( i=0; (i<(dim_t)nthread) && (*status==SAI__OK); i++ ) {
    pdata = job_data + i;

    pdata->type = 1;                /* Boxcar smooth */

    if( pdata->chunk1 >= nchunks ) {
      /* Nothing for this thread to do */
      msgSetk( "W", i+1);
      msgOutif( MSG__DEBUG, "",
                "-- parallel time: skipping thread ^W, nothing to do",
                status);
    } else {
      /* Since we know there is one job_data per thread, just submit jobs
         immediately */
      pdata->ijob = thrAddJob( wf, THR__REPORT_JOB, pdata, smfParallelTime,
                                 0, NULL, status );
    }
  }

  /* Wait until all of the submitted jobs have completed */
  thrWait( wf, status );

  /*** TIMER ***/
  msgOutf( "", "** %f seconds to complete test", status,
           smf_timerupdate(&tv1,&tv2,status) );

  msgOut( "", TASK_NAME
          ": *** Next 2 tests will be done twice due to FFTW planning *****",
          status );

  for( k=0; k<2; k++ ) {

    /* The third test will FFT filter bolometers from time chunks in
       parallel */

    msgOut( "", TASK_NAME
            ": Starting test 3 __parallel time: FFT filter__", status );

    for( i=0; (i<(dim_t)nthread) && (*status==SAI__OK); i++ ) {
      pdata = job_data + i;

      pdata->type = 2;                /* FFT filter */

      if( pdata->chunk1 >= nchunks ) {
        /* Nothing for this thread to do */
        msgSetk( "W", i+1);
        msgOutif( MSG__DEBUG, "",
                  "-- parallel time: skipping thread ^W, nothing to do",
                  status);
      } else {
        /* Since we know there is one job_data per thread, just submit jobs
           immediately */
        pdata->ijob = thrAddJob( wf, THR__REPORT_JOB, pdata, smfParallelTime,
                                 0, NULL, status );
      }
    }

    /* Wait until all of the submitted jobs have completed */
    thrWait( wf, status );

    /*** TIMER ***/
    msgOutf( "", "** %f seconds to complete test", status,
             smf_timerupdate(&tv1,&tv2,status) );

    msgOut( "", TASK_NAME
            ": Starting test 4 __FFTW filter using internal threading__",
            status );

    for( i=0; (*status==SAI__OK)&&(i<nchunks); i++ ) {
      filt = smf_create_smfFilter( res[i]->sdata[0], status );
      smf_filter_ident( filt, 1, status );

      for( j=0; (*status==SAI__OK)&&(j<nsub); j++ ) {
        msgOutiff( MSG__DEBUG, "", "  filter chunk %zu/%zu, bolo %zu/%zu",
                   status, i+1, nchunks, j+1, nsub );
        smf_filter_execute( wf, res[i]->sdata[j], filt, 0, 0, status );
      }

      if( filt ) filt = smf_free_smfFilter( filt, status );
    }
    /*** TIMER ***/
    msgOutf( "", "** %f seconds to complete test", status,
             smf_timerupdate(&tv1,&tv2,status) );
  }

  msgOut( "", TASK_NAME
          ": **************************************************************",
          status );

  /* Series of short single-thread array index tests */
  data = res[0]->sdata[0];
  dat = data->pntr[0];

  smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, &ndata, &bstride,
                &tstride, status );

  msgOut("","Array index test #1: two multiplies in inner loop",status);
  smf_timerinit( &tv1, &tv2, status );
  for( i=0; i<nbolo; i++ ) {
    for( j=0; j<ntslice; j++ ) {
      dat[i*bstride + j*tstride] += 5;
    }
  }
  msgOutf( "", "** %f seconds to complete test", status,
           smf_timerupdate(&tv1,&tv2,status) );

  msgOut("","Array index test #2: only index increments",status);
  smf_timerinit( &tv1, &tv2, status );
  for( i=0; i<nbolo*bstride; i+=bstride ) {
    for( j=i; j<(i+ntslice*tstride); j+=tstride ) {
      dat[j] += 5;
    }
  }
  msgOutf( "", "** %f seconds to complete test", status,
           smf_timerupdate(&tv1,&tv2,status) );

  msgOut("","Array index test #3: one multiply in outer loop",status);
  smf_timerinit( &tv1, &tv2, status );
  offset = 0;
  for( i=0; i<nbolo; i++ ) {
    offset = i*bstride;
    for( j=0; j<ntslice; j++ ) {
      dat[offset] += 5;
      offset += tstride;
    }
  }
  msgOutf( "", "** %f seconds to complete test", status,
           smf_timerupdate(&tv1,&tv2,status) );

  /* Clean up */
  if( res ) {
    for( i=0; i<nchunks; i++ ) {
      if( res[i] ) {
        smf_close_related( wf, &res[i], status );
      }
    }
    res = astFree( res );
  }
  job_data = astFree( job_data );

  /* Ensure that FFTW doesn't have any used memory kicking around */
  fftw_cleanup();

}
