/*
*+
*  Name:
*     smf_fft_filter

*  Purpose:
*     Filter smfData in the frequency domain using the FFTW3 library

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:

*     smf_filter_execute( smfWorkForce *wf, smfData *data, smfFilter *filt, 
*                         int *status )

*  Arguments:
*     wf = smfWorkForce * (Given)
*        Pointer to a pool of worker threads that will do the re-binning.
*     data = smfData * (Given and Returned)
*        The data to be filtered (performed in-place)
*     srate = double (Given)
*        If nonzero specifies sample rate of data in Hz (otherwise taken
*        from JCMTState associated with data).
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Filter a smfData. If a pool of threads is supplied (wf), this routine
*     will filter multiple blocks of bolometers in parallel. If a NULL pointer
*     is supplied smf_filter_execute will not use any of the smf_threads 
*     routines. However, this function is thread safe, so that
*     smf_filter_execute can be called as part of a higher-level parallelized
*     routine in this case. 

*  Notes:
*     Currently getting sporadic segfaults when wf != NULL

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-09-27 (EC):
*        Initial Version
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-06-06 (EC):
*        -Renamed from smf_fft_filter to smf_filter_execute
*        -Modified interface to take external smfFilter
*        -Handle real and complex filters
*     2008-06-10 (EC):
*        Move normalization to smf_filter_ident
*     2008-06-11 (EC):
*        Switched to "guru" FFTW interface to facilitate future multi-threading
*     2008-06-12 (EC):
*        Switch to split real/imaginary arrays for smfFilter
*     2008-06-13 (EC):
*        Only create plans once since we're using guru interface
*     2008-07-21 (EC):
*        Only filter bolo if SMF__Q_BADB isn't set.
*     2009-04-27 (EC):
*        - Modified so that it can optionally use multiple threads internally
*        - Each thread gets its own plan, rather than reusing same one

*  Copyright:
*     Copyright (C) 2007-2009 University of British Columbia.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* System includes */
#include <pthread.h>

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"
#include "fftw3.h"

/* SMURF includes */
#include "libsmf/smf_typ.h"
#include "libsmf/smf.h"

/* ------------------------------------------------------------------------ */
/* Local variables and functions */

pthread_mutex_t smf_filter_execute_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Structure containing information about blocks of bolos to be
   filtered by each thread. All threads read/write to/from mutually
   exclusive parts of the master smfData so we don't need to make
   local copies of the entire smfData. The filter and plans are only
   read inside a thread and therefore don't need to be local copies
   either. */
typedef struct smfBoloChunkData {
  size_t b1;               /* Index of first bolometer to be filtered */
  size_t b2;               /* Index of last bolometer to be filtered */
  smfData *data;           /* Pointer to master smfData */
  double *data_fft_r;      /* Real part of the data FFT (1-bolo) */
  double *data_fft_i;      /* Imaginary part of the data FFT (1-bolo)*/
  smfFilter *filt;         /* Pointer to the filter */
  int ijob;                /* Job identifier */
  fftw_plan plan_forward;  /* for forward transformation */
  fftw_plan plan_inverse;  /* for inverse transformation */
} smfBoloChunkData;

/* Function to be executed in thread: filter all of the bolos from b1 to b2
   using the same filt */

void smfParallelFilt( void *job_data_ptr, int *status );

void smfParallelFilt( void *job_data_ptr, int *status ) {
  double ac, bd, aPb, cPd;      /* Components for complex multiplication */
  double *base=NULL;            /* Pointer to start of current bolo in array */
  size_t bstride;               /* Bolometer stride */
  double *data_fft_r=NULL;      /* Real part of the data FFT */
  double *data_fft_i=NULL;      /* Imaginary part of the data FFT */
  smfData *data=NULL;           /* smfData that we're working on */
  smfBoloChunkData *pdata=NULL; /* Pointer to job data */
  smfFilter *filt=NULL;         /* Frequency domain filter */
  size_t i;                     /* Loop counter */
  size_t j;                     /* Loop counter */
  dim_t ntslice;                /* Number of time slices */
  unsigned char *qua=NULL;      /* pointer to quality */

  if( *status != SAI__OK ) return;

  /* Pointer to the data that this thread will process */
  pdata = job_data_ptr;

  /* Check for valid inputs */
  if( !pdata ) {
    *status = SAI__ERROR;
    errRep( "", "smfParallelFilt: No job data supplied", status );
    return;
  }

  data = pdata->data;
  qua = data->pntr[2];
  filt = pdata->filt;
  data_fft_r = pdata->data_fft_r;
  data_fft_i = pdata->data_fft_i;

  if( !data ) {
    *status = SAI__ERROR;
    errRep( "", "smfParallelFilt: No valid smfData supplied", status );
    return;
  }

  if( !filt ) {
    *status = SAI__ERROR;
    errRep( "", "smfParallelFilt: No valid smfFilter supplied", status );
    return;
  }

  if( !data_fft_r || !data_fft_i ) {
    *status = SAI__ERROR;
    errRep( "", "smfParallelFilt: Invaid data_fft_* pointers provided", 
            status );
    return;
  }

  /* Debugging message indicating thread started work */
  smf_mutex_lock( &smf_filter_execute_mutex, status );
  msgOutiff( MSG__DEBUG, "", 
             "smfParallelFilt: thread starting on bolos %zu -- %zu",
             status, pdata->b1, pdata->b2 );
  smf_mutex_unlock( &smf_filter_execute_mutex, status );

  /* Filter the data one bolo at a time */
  smf_get_dims( data, NULL, NULL, NULL, &ntslice, NULL, &bstride, NULL, 
                status );

  for( i=pdata->b1; (*status==SAI__OK) && (i<=pdata->b2); i++ ) 
    if( !qua || !(qua[i*bstride]&SMF__Q_BADB) ) { /* Check for bad bolo flag */
      
      /* Obtain pointer to the correct chunk of data */
      //base = &((double *)data->pntr[0])[i*bstride];
      base = data->pntr[0];
      base += i*bstride;

      /* Execute forward transformation using the guru interface */
      fftw_execute_split_dft_r2c( pdata->plan_forward, base, 
                                  data_fft_r, data_fft_i );
      
      /* Apply the frequency-domain filter */
      if( filt->isComplex ) {
        for( j=0; j<filt->dim; j++ ) {
          /* Complex times complex, using only 3 multiplies */
          ac = data_fft_r[j] * filt->real[j];
          bd = data_fft_i[j] * filt->imag[j];
          
          aPb = data_fft_r[j] + data_fft_i[j];
          cPd = filt->real[j] + filt->imag[j];
          
          data_fft_r[j] = ac - bd;
          data_fft_i[j] = aPb*cPd - ac - bd;
        }
      } else { 
        for( j=0; j<filt->dim; j++ ) {
          /* Complex times real */
          data_fft_r[j] *= filt->real[j];
          data_fft_i[j] *= filt->real[j];
        }
      }    

      /* Perform inverse transformation using guru interface */
      fftw_execute_split_dft_c2r( pdata->plan_inverse, data_fft_r, 
                                  data_fft_i, base );
    }

  smf_mutex_lock( &smf_filter_execute_mutex, status );
  msgOutiff( MSG__DEBUG, "", 
             "smfParallelFilt: thread finishing bolos %zu -- %zu",
             status, pdata->b1, pdata->b2 );
  smf_mutex_unlock( &smf_filter_execute_mutex, status );

}

/* ------------------------------------------------------------------------ */

#define FUNC_NAME "smf_filter_execute"

void smf_filter_execute( smfWorkForce *wf, smfData *data, smfFilter *filt, 
                         int *status ) {

  /* Local Variables */
  fftw_iodim dims;                /* I/O dimensions for transformations */
  int i;                          /* Loop counter */
  smfBoloChunkData *job_data=NULL;/* Array of job data for each thread */
  dim_t nbolo=0;                  /* Number of bolometers */
  dim_t ndata=0;                  /* Total number of data points */
  int nw;                         /* Number of worker threads */
  dim_t ntslice=0;                /* Number of time slices */
  smfBoloChunkData *pdata=NULL;   /* Pointer to current job data */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Check for NULL pointers */
  if( !data ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL smfData pointer", status );
  }

  if( !filt ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL smfFilter pointer", status );
  }

  /* Ensure that the smfData is ordered correctly (bolo ordered) */
  smf_dataOrder( data, 0, status );

  /* Obtain the dimensions of the array being filtered */
  if( *status == SAI__OK ) {
    if( data->ndims == 1 ) {
      /* If 1-dimensional assume the dimension is time */
      ntslice = data->dims[0];
      ndata = ntslice;
      nbolo = 1;
    } else if( data->ndims == 3 ) {
      /* If 3-dimensional this should be bolo-ordered at this point */
      ntslice = data->dims[0];
      nbolo = data->dims[1]*data->dims[2];
      ndata = nbolo*ntslice;
    } else {
      /* Can't handle other dimensions */
      *status = SAI__ERROR;
      msgSeti("NDIMS",data->ndims);
      errRep( "", FUNC_NAME ": Can't handle ^NDIMS dimensions.", status);
    }

    /* Check that the filter dimensions are appropriate for the data */
    if( ntslice != filt->ntslice ) {
      *status = SAI__ERROR;
      msgSeti("DATALEN",ntslice);
      msgSeti("FILTLEN",filt->ntslice);
      errRep( "", FUNC_NAME 
             ": Filter for length ^FILTLEN doesn't match data length ^FILTLEN",
             status);
    }
  }

  if( *status != SAI__OK ) return;

  /* Describe the input and output array dimensions for FFTW guru interface.
     - dims describes the length and stepsize of time slices within a bolometer 
     - howmany_dims gives the number of bolos, and stride from one to the next*/

  dims.n = ntslice;
  dims.is = 1;
  dims.os = 1;

  /* Set up the job data */
  job_data = smf_malloc( nw, sizeof(*job_data), 1, status );
  for( i=0; (*status==SAI__OK)&&i<nw; i++ ) {
    pdata = job_data + i;

    pdata->b1 = i*(nbolo/nw);
    pdata->b2 = (i+1)*(nbolo/nw)-1;
    /* Ensure that the last thread gets the remainder of the bolos */
    if( i==(nw-1) ) {
      pdata->b2=nbolo-1;
    }
    pdata->data = data;
    pdata->data_fft_r = smf_malloc( filt->dim, sizeof(*pdata->data_fft_r), 0, 
                                    status );
    pdata->data_fft_i = smf_malloc( filt->dim, sizeof(*pdata->data_fft_i), 0, 
                                    status );
    pdata->filt = filt;
    pdata->ijob = -1;   /* Flag job as ready to start */


    /* Setup forward FFT plan using guru interface. Requires protection
       with a mutex */
    smf_mutex_lock( &smf_filter_execute_mutex, status );

    if( *status == SAI__OK ) {
      /* Just use the data_fft_* arrays from the first chunk of job data since
         the guru interface allows you to use the same plans for multiple
         transforms. */
      pdata->plan_forward = fftw_plan_guru_split_dft_r2c( 1, &dims, 0, NULL,
                                                          data->pntr[0], 
                                                          pdata->data_fft_r,
                                                          pdata->data_fft_i, 
                                                          FFTW_ESTIMATE | 
                                                          FFTW_UNALIGNED );
    }
    
    smf_mutex_unlock( &smf_filter_execute_mutex, status );
    
    if( !pdata->plan_forward && (*status == SAI__OK) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME 
              ": FFTW3 could not create plan for forward transformation",
              status);
    }
    
    /* Setup inverse FFT plan using guru interface */
    smf_mutex_lock( &smf_filter_execute_mutex, status );
    
    if( *status == SAI__OK ) {
      pdata->plan_inverse = fftw_plan_guru_split_dft_c2r( 1, &dims, 0, NULL,
                                                          pdata->data_fft_r,
                                                          pdata->data_fft_i, 
                                                          data->pntr[0], 
                                                          FFTW_ESTIMATE | 
                                                          FFTW_UNALIGNED);
    }
    
    smf_mutex_unlock( &smf_filter_execute_mutex, status );
    
    if( !pdata->plan_inverse && (*status==SAI__OK) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME 
              ": FFTW3 could not create plan for inverse transformation",
              status);
    }

  }
 
  /* Execute the filter */
  if( nw > 1 ) {
    /* Submit jobs in parallel if we have multiple worker threads */
    for( i=0; (*status==SAI__OK)&&i<nw; i++ ) {
      pdata = job_data + i;
      pdata->ijob = smf_add_job( wf, SMF__REPORT_JOB, pdata, smfParallelFilt, 
                                 NULL, status );
    }
    /* Wait until all of the submitted jobs have completed */
    smf_wait( wf, status );
  } else {
    /* If there is only a single thread call smfParallelFilt directly */
    pdata = job_data;
    smfParallelFilt( job_data, status );
  }

  /* Clean up the job data array */
  if( job_data ) {
    for( i=0; i<nw; i++ ) {
      pdata = job_data + i;
      if( pdata->data_fft_r ) pdata->data_fft_r = smf_free( pdata->data_fft_r, 
                                                            status);
      if( pdata->data_fft_i ) pdata->data_fft_i = smf_free( pdata->data_fft_i, 
                                                            status);
      /* Destroy the plans */ 
      smf_mutex_lock( &smf_filter_execute_mutex, status );
      fftw_destroy_plan( pdata->plan_forward );
      fftw_destroy_plan( pdata->plan_inverse );
      smf_mutex_unlock( &smf_filter_execute_mutex, status );
    }
    job_data = smf_free( job_data, status );
  }

}
