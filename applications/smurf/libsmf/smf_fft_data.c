/*
*+
*  Name:
*     smf_fft_data

*  Purpose:
*     Calculate the forward or inverse FFT of a smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_fft_data( ThrWorkForce *wf, smfData *indata,
*                          smfData *outdata, int inverse, dim_t len,
*                          int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     indata = smfData * (Given)
*        Pointer to the input smfData
*     outdata = smfData * (Given)
*        If supplied, this is a container for the output of the routine. If
*        not supplied, a new smfData will be allocated and returned. In either
*        case, return value is a pointer to the transformed data.
*     inverse = int (Given)
*        If set perform inverse transformation. Otherwise forward.
*     len = dim_t (Given)
*        Number of samples over which to apply apodization. Can be set to
*        SMF__MAXAPLEN in which case the routine will automatically apodize
*        the entire data stream (maximum valid value of len). Set it to
*        zero to prevent apodisation (e.g. if the data has already been
*        apodised). Set it to SMF__BADSZT to cause the paddded regions
*        to be filled with artificial data based on the current contents
*        of the smfData (no apodising is performed in this case).
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     Pointer to the smfData containing the forward or inverse
*     transformed data (newly created if outdata not specified).

*  Description:
*     Perform the forward or inverse FFT of a smfData. In the real
*     domain the data are 3-d time-series cube (either x,y,time or
*     time,x,y depending on isTordered flag), or a 2-d map. The
*     frequency domain representation of the time-series data is 4-d
*     (always frequency,x,y,component -- i.e. bolo-ordered). Component
*     is an axis of length 2 containing the real and imaginary parts.
*     The frequency domain representation of a map is 3-d
*     (x-frequency, y-frequency, component).  Inverse transforms
*     always leave time-series data in bolo-ordered format. If the
*     data are already transformed, this routine returns a NULL
*     pointer.  If a non-null pointer wf is supplied, tell FFW to use
*     multiple threads.

*  Notes:
*     No WCS is produced for inverse transformed data.

*  Authors:
*     Ed Chapin (UBC)
*     COBA: Coskun Oba (UoL)
*     {enter_new_authors_here}

*  History:
*     2008-07-09 (EC):
*        Initial version
*     2008-07-23 (EC):
*        Forward transformations now seem to work.
*     2008-07-28 (EC):
*        -Calculate correct ntslice for inverse.
*        -Code stub for generation of 4-d WCS of forward transformation.
*     2008-07-29 (EC):
*        Calculate WCS for 4-d transformed data.
*     2008-07-29 (TIMJ):
*        Steptime is now in smfHead.
*     2008-10-17 (AGG):
*        Set NaN and Inf to VAL__BADD when normalizing
*     2008-11-03 (EC):
*        Move conversion of NaN/Inf-->VAL__BADD to smf_convert_bad.c
*     2008-11-20 (TIMJ):
*        "Close" the smfData instead of "free".
*     2009-10-8 (DSB):
*         Use a smf job context to ensure that thrWait only waits for the jobs
*         submitted within this function.
*     2009-10-28 (DSB):
*         Use sc2ast_make_bolo_frame.
*     2010-06-28 (DSB):
*         Added arguments quality and len (needed since with the new
*         filtering scheme, the data may not have been apodised on entry
*         to this function).
*     2010-07-01 (TIMJ):
*        Make sure that quality in smfData really is used.
*     2010-07-02 (TIMJ):
*        Use const quality
*     2010-08-31 (EC):
*        Do the re-ordering as part of the deepcopy if needed
*     2010-09-21 (COBA):
*        Add SMF__NOCREATE_FTS
*     2010-09-29 (DSB):
*        Allow padding with artificial data.
*     2010-10-19 (COBA):
*        Removed SMF__NOCREATE_FTS in order to propagate the smfFts data.
*     2011-04-14 (DSB):
*        Ensure gaps are filled even if apodisation and zero padding are being
*        used.
*     2011-20-14 (DSB):
*        If the input smfData is time ordered, we need to re-order the
*        quality array when copying it into the FFTed smfData (which is
*        always bolometer ordered).
*     2011-04-25 (TIMJ):
*        Fix stride argument to smf_dataOrder_array
*     2011-06-25 (EC):
*        If input data are SMF__INTEGER they will be converted to SMF__DOUBLE
*     2011-09-20 (EC):
*        Record whether we have FFT data or not in the FITS header
*     2011-09-23 (EC):
*        Support 2D FFTs
*     2011-10-04 (EC):
*        Add outdata
*     2011-03-05 (DSB):
*        Do not attempt to take the FFT of bad bolometer time streams since
*        they generate NaNs in FFTW.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2011 Science and Technology Facilities Council.
*     Copyright (C) 2008,2010-2011 University of British Columbia.
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

/* System includes */
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "prm_par.h"
#include "fftw3.h"
#include "star/atl.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* Data Acquisition Includes */
#include "sc2da/sc2ast.h"

/* ------------------------------------------------------------------------ */
/* Local variables and functions */

pthread_mutex_t smf_fft_data_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Structure containing information about blocks of bolos to be
   FFT'd by each thread. All threads read/write to/from mutually
   exclusive parts of data and retdata so we don't need to make
   local copies of entire smfDatas. */
typedef struct smfFFTData {
  dim_t b1;                     /* Index of first bolometer to be FFT'd */
  dim_t b2;                     /* Index of last bolometer to be FFT'd */
  smfData *data;                /* Pointer to input data */
  int ijob;                     /* Job identifier */
  int inverse;                  /* Set if this is inverse transformation */
  dim_t nbolo;                  /* Number of detectors  */
  dim_t fdims[2];               /* Lengths of frequency axes */
  int ndims;                    /* Number of real space dimensions */
  dim_t rdims[2];               /* Lengths of real-space axes */
  fftw_plan plan;               /* FFTW plan */
  smfData *retdata;             /* Pointer to returned FFT'd data */
} smfFFTData;

/* Function to be executed in thread: FFT all of the bolos from b1 to b2 */

void smfFFTDataParallel( void *job_data_ptr, int *status );

void smfFFTDataParallel( void *job_data_ptr, int *status ) {
  double *baseR=NULL;           /* base pointer to real part of fourier data */
  double *baseI=NULL;           /* base pointer to imag part of fourier data */
  double *baseD=NULL;           /* base pointer to real-space data */
  dim_t i;                      /* Loop counter */
  dim_t nf=0;
  dim_t ntslice=0;
  smfFFTData *pdata = NULL;     /* Pointer to job data */

   if( *status != SAI__OK ) return;

  /* Pointer to the data that this thread will process */
  pdata = job_data_ptr;

  /* Check for valid inputs */
  if( !pdata ) {
    *status = SAI__ERROR;
    errRep( "", "smfFFTDataParallel: No job data supplied", status );
    return;
  }

  if( !pdata->data || !pdata->retdata ) {
    *status = SAI__ERROR;
    errRep( "", "smfFFTDataParallel: Invalid smfData pointers", status );
    return;
  }

  nf = 1;
  for( i=0; i<pdata->ndims; i++ ) nf *= pdata->fdims[i];

  /* Debugging message indicating thread started work */
  if( pdata->ndims == 1 ) {
    msgOutiff( SMF__TIMER_MSG, "",
               "smfFFTDataParallel: thread starting on bolos %zu -- %zu",
               status, pdata->b1, pdata->b2 );


    /* if b1 past end of the work, nothing to do so we return */
    if( pdata->b1 >= pdata->nbolo ) {
      msgOutif( SMF__TIMER_MSG, "",
                "smfFFTDataParallel: nothing for thread to do, returning",
                status);
      return;
    }

    ntslice = pdata->rdims[0];
  } else {
    msgOutif( MSG__DEBUG, "", "smfFFTDataParallel: starting FFT of map",
              status );
  }

   if( pdata->inverse ) {        /* Perform inverse fft */

     for( i=pdata->b1; i<=pdata->b2; i++ ) {
       /* Transform bolometers one at a time */
       baseR = pdata->data->pntr[0];
       baseD = pdata->retdata->pntr[0];

       if( pdata->ndims == 1 ) {
         /* Multiple time-series for different bolometers */
         baseR += i*nf;
         baseI = baseR + nf*pdata->nbolo;
         baseD += i*ntslice;
       } else {
         /* Single map */
         baseI = baseR + nf;
       }

       fftw_execute_split_dft_c2r( pdata->plan, baseR, baseI, baseD );
     }
   } else {                      /* Perform forward fft */
     for( i=pdata->b1; i<=pdata->b2; i++ ) {
       /* Transform bolometers one at a time */
       baseD = pdata->data->pntr[0];
       baseR = pdata->retdata->pntr[0];

       if( pdata->ndims == 1 ) {
         /* Multiple time-series for different bolometers */
         baseD += i*ntslice;
         baseR += i*nf;
         baseI = baseR + nf*pdata->nbolo;
       } else {
         /* Single map */
         baseI = baseR + nf;
       }

       /* Skip bad bolometers. These will have been filled with bad values. */
       if( baseD[ 0 ] != VAL__BADD ) {
         fftw_execute_split_dft_r2c( pdata->plan, baseD, baseR, baseI );

       /* For safety, fill thre returned arrays with zeros for bad bolometers. */
       } else {
         memset( baseR, 0, sizeof(*baseR)*nf );
         memset( baseI, 0, sizeof(*baseR)*nf );
       }
     }
   }

   /* Debugging message indicating thread finished work */
   if( pdata->ndims == 1 ) {
     msgOutiff( SMF__TIMER_MSG, "",
                "smfFFTDataParallel: thread finishing bolos %zu -- %zu",
                status, pdata->b1, pdata->b2 );
   } else {
     msgOutif( MSG__DEBUG, "", "smfFFTDataParallel: finished FFT of map",
                status );
   }
}

/* ------------------------------------------------------------------------ */


#define FUNC_NAME "smf_fft_data"

smfData *smf_fft_data( ThrWorkForce *wf, smfData *indata, smfData *outdata,
                       int inverse, dim_t len, int *status ) {
  double *baseR=NULL;           /* base pointer to real part of fourier data */
  double *baseI=NULL;           /* base pointer to imag part of fourier data */
  double *baseD=NULL;           /* base pointer to real space data */
  smfData *data=NULL;           /* pointer to bolo-ordered data */
  double df[2]={0,0};           /* Frequency step size Hz (1-d) 1/arcsec (2-d)*/
  fftw_iodim *dims=NULL;        /* FFTW I/O dimensions for transformations */
  dim_t fdims[2];               /* Frequency dimensions */
  dim_t ibolo;                  /* Bolometer counter */
  int i;                        /* Loop counter */
  dim_t inbstr;                 /* Bolometer stride in input data */
  dim_t intstr;                 /* Time slice stride in input data */
  int isFFT=0;                  /* Are the input data freq. domain? */
  dim_t j;                      /* Loop counter */
  smfFFTData *job_data=NULL;    /* Array of job data for each thread */
  dim_t nbolo=0;                /* Number of detectors  */
  dim_t ncols=0;                /* Number of columns */
  int ndims;                    /* Number of real-space dimensions */
  dim_t nf=0;                   /* Number of frequencies in FFT */
  int njobs=0;                  /* Number of jobs to be processed */
  double norm=1.;               /* Normalization factor for the FFT */
  dim_t nr=0;                   /* Number of elements in real space */
  dim_t nretdata=0;             /* Number of data points returned data array */
  dim_t nrows=0;                /* Number of rows */
  dim_t ntslice=0;              /* Number of time slices */
  int nw;                       /* Number of worker threads */
  smfFFTData *pdata=NULL;       /* Pointer to current job data */
  dim_t rdims[2];               /* real-space dimensions */
  smfData *retdata=NULL;        /* Pointer to new transformed smfData */
  dim_t step;                   /* step size for dividing up work */
  double *val=NULL;             /* Element of data to be normalized */

  if (*status != SAI__OK) return NULL;

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Check for NULL pointer */
  if( indata == NULL ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData pointer is NULL", status );
    return NULL;
  }

  /* Call smf_isfft to check to see if we have real-space or FFT
     data, and obtain dimensions. */

  isFFT = smf_isfft( indata, rdims, &nbolo, fdims, indata->hdr ? df : NULL,
                     &ndims, status );

  if( *status != SAI__OK ) return NULL;

  nf = 1;
  nr = 1;
  for( j=0; j<ndims; j++ ) {
    nf *= fdims[j];
    nr *= rdims[j];
  }

  if( ndims == 1 ) {
    /* Time-series data. Determine dimensions */
    smf_get_dims( indata, &nrows, &ncols, &nbolo, &ntslice, NULL, &inbstr,
                  &intstr, status );
  }

  /* If the data are already transformed to the requested domain return
     a NULL pointer */

  if( (*status==SAI__OK) && (isFFT != inverse) ) {
    retdata = NULL;
    goto CLEANUP;
  }

  /* Create a copy of the input data since FFT operations often do
     calculations in-place. The returned FFT smfData should not
     have quality so we only need quality for apodizing and that
     apodization will not be propagated back to the input
     smfData since we work on the copy. We therefore create
     a workspace copy of the quality. Re-ordering is also done
     if needed to ensure the copy is bolometer ordered. Finally, if
     the data are raw integers, the deepcopy will convert them
     to doubles. */
  data = smf_deepcopy_smfData( wf, indata, 1,
                               SMF__NOCREATE_VARIANCE |
                               SMF__NOCREATE_QUALITY |
                               SMF__NOCREATE_FILE |
                               SMF__NOCREATE_DA,
                               1, 0, status );

  /* Create some quality. We only apodize, pad or fill if we are doing a
     forward FFT of time-series data. */
  if( !inverse && (ndims == 1) ) {
    smf_qual_t *inqual = smf_select_qualpntr( indata, NULL, status );

    /* we know that "data" does not have a quality component because
       we did a deepcopy without copying it. Ensure that the output has
       quality with bolometer order. */

    if (inqual) {
      data->qual = smf_dataOrder_array( wf, inqual, SMF__QUALTYPE,
                                        SMF__QUALTYPE, nbolo*nr, nr,
                                        nbolo, intstr, inbstr, 1, nr, 0, 0,
                                        status );
    }


   /* If we are padding with artificial data, we must not apodise.
      Re-create the padding based on the current contents of the
      smfData (this also fill any gaps in the data). */
    if( len == SMF__BADSZT ) {
      if( data->qual ) {
        smf_fillgaps( wf, data, SMF__Q_PAD | SMF__Q_GAP, status );
      } else {
        *status = SAI__ERROR;
        errRep( "", FUNC_NAME
                ": trying to fill gaps, but no quality array supplied",
                status );
      }

    /* If required, fill the data (excluding padding) and apodise the data */
    } else if( len > 0 ) {
      if( data->qual ) {
        smf_fillgaps( wf, data, SMF__Q_GAP, status );
        smf_apodize( data, len, 1, status );
      } else {
        *status = SAI__ERROR;
        errRep( "", FUNC_NAME
                ": trying to fill gaps and apodize but no quality array "
                "supplied", status );
      }
    }

    /* Put a bad value at the start of each bad bolometer so that they can
       be identified in the worker thread. */
    if( data-> qual ) {
      for( ibolo = 0; ibolo < nbolo; ibolo++ ) {
        if( (data->qual)[ ibolo*inbstr ] & SMF__Q_BADB ) {
          ((double*)(data->pntr[0]))[ ibolo*inbstr ] = VAL__BADD;
        }
      }
    }

    if (data && data->qual) data->qual = astFree( data->qual );
  }

  if( outdata && (*status==SAI__OK) ) {
    /* If the caller supplies the output container we check dimensions */
    dim_t o_fdims[2];
    dim_t o_nbolo;
    int o_ndims;
    dim_t o_rdims[2];
    int o_isfft;

    o_isfft = smf_isfft( outdata, o_rdims, &o_nbolo, o_fdims, NULL, &o_ndims,
                         status );

    if( o_isfft == inverse ) {
      *status = SAI__ERROR;
      errRepf( "", FUNC_NAME
               ": inverse is %i but supplied output container is also %i",
               status, inverse, o_isfft );
    }

    if( o_nbolo != nbolo ) *status = SAI__ERROR;

    for( j=0; (j<ndims) && (*status==SAI__OK); j++ ) {
      if( o_rdims[j] != rdims[j] ) *status = SAI__ERROR;
    }

    if( *status != SAI__OK ) {
      errRep( "", FUNC_NAME
              ": supplied output container has incorrect dimensions", status);
    } else {
      retdata = outdata;
    }
  } else {

    /* Otherwise create a new smfData, copying over everything except
       for the bolo data itself */

    retdata = smf_deepcopy_smfData( wf, data, 0,
                                    SMF__NOCREATE_DATA |
                                    SMF__NOCREATE_VARIANCE |
                                    SMF__NOCREATE_QUALITY |
                                    SMF__NOCREATE_FILE |
                                    SMF__NOCREATE_DA,
                                    0, 0, status );

    if( *status == SAI__OK ) {

      /* Allocate space for the transformed data */

      if( inverse ) {
        if( ndims == 1 ) {
          /* Doing an inverse FFT to the time-domain */
          retdata->ndims = 3;
          retdata->dims[0] = rdims[0];
          retdata->dims[SC2STORE__COL_INDEX+1] = ncols;
          retdata->dims[SC2STORE__ROW_INDEX+1] = nrows;

          retdata->lbnd[0] = 1;
          retdata->lbnd[1] = 0;
          retdata->lbnd[2] = 0;
        } else {
          /* Doing an inverse FFT to a map */
          retdata->ndims = 2;
          retdata->dims[0] = rdims[0];
          retdata->dims[1] = rdims[1];

          retdata->lbnd[0] = data->lbnd[0];
          retdata->lbnd[1] = data->lbnd[1];
        }
      } else {
        if( ndims == 1 ) {
          /* Doing a forward FFT of time-series to the frequency domain */
          retdata->ndims = 4;
          retdata->dims[0] = nf;
          retdata->dims[SC2STORE__COL_INDEX+1] = ncols;
          retdata->dims[SC2STORE__ROW_INDEX+1] = nrows;
          retdata->dims[3] = 2;

          retdata->lbnd[0] = 1;
          retdata->lbnd[1] = 0;
          retdata->lbnd[2] = 0;
          retdata->lbnd[3] = 1;
        } else {
          /* Doing a forward FFT of a map to the frequency domain */
          retdata->ndims = 3;
          retdata->dims[0] = fdims[0];
          retdata->dims[1] = fdims[1];
          retdata->dims[2] = 2;

          /* Just set the frequency axis lbnds to those in real-space so
             that we can recover them later */
          retdata->lbnd[0] = data->lbnd[0];
          retdata->lbnd[1] = data->lbnd[1];
          retdata->lbnd[2] = 1;
        }
      }

      nretdata=1;
      for( j=0; j<retdata->ndims; j++ ) {
        nretdata *= retdata->dims[j];
      }

      retdata->pntr[0] = astCalloc(nretdata,
                                   smf_dtype_sz(retdata->dtype,status));

    /* Returned data is always bolo-ordered (ignored for maps) */
       retdata->isTordered=0;
    }
  }

  /* Describe the array dimensions for FFTW guru interface
     - dims describes the length and stepsize for both the input and
     output axes
     - we are using the Starlink array ordering (in case you get confused
     by how the strides are setup compared to the "normal" FFTW way...
     but we are using the guru interface so we can do whatever we want)
  */

  dims = astCalloc( ndims, sizeof(*dims) );
  if( *status == SAI__OK ) {

    /* Forward and inverse strides differ. According to the FFTW
       docs, the lengths of the last complex (transformed) axis is
       interpreted as dims.n/2+1 to account for the saved
       space... but the strides themselves are interpreted literally
       (shorter for the complex data), hence the slightly confusing
       mixture of rdims and fdims depending on whether we are doing
       a forward or inverse transform.
    */

    (dims[0]).n = (int) rdims[0];
    (dims[0]).is = 1;
    (dims[0]).os = 1;

    for( j=1; j<ndims; j++ ) {
      (dims[j]).n = (int) rdims[j];

      if( inverse ) {
        (dims[j]).is = (int)fdims[j-1]*(dims[j-1]).is;
        (dims[j]).os = (int)rdims[j-1]*(dims[j-1]).os;
      } else {
        (dims[j]).is = (int)rdims[j-1]*(dims[j-1]).is;
        (dims[j]).os = (int)fdims[j-1]*(dims[j-1]).os;
      }
    }

    /* Set up the job data */

    if( ndims == 1 ) {
      /* For time-series parallellize over groups of bolometers */
      if( nw > (int) nbolo ) {
        step = 1;
      } else {
        step = nbolo/nw;
        if( !step ) {
          step = 1;
        }
      }
    } else {
      /* Otherwise transforms of maps are done in a single thread */
      nw = 1;
      step = 0;
    }

    job_data = astCalloc( nw, sizeof(*job_data) );

    for( i=0; (*status==SAI__OK)&&i<nw; i++ ) {
      pdata = job_data + i;

      if( ndims == 1 ) {
        /* If time-series set up division of labour by bolo grous */
        pdata->b1 = i*step;
        pdata->b2 = (i+1)*step-1;

        /* if b1 is greater than the number of bolometers, we've run out
           of jobs... */
        if( pdata->b1 >= nbolo ) {
          break;
        }

        /* Ensure that the last thread picks up any left-over bolometers */
        if( (i==(nw-1)) && (pdata->b1<(nbolo-1)) ) {
          pdata->b2=nbolo-1;
        }
      } else {
        /* Otherwise set bolo ranges to 0 so that we only go once through
           the transformation loop in amfFFTDataParallel */
        pdata->b1 = 0;
        pdata->b2 = 0;
      }

      /* increase the jobs counter */
      njobs++;

      pdata->data = data;
      pdata->ijob = -1;   /* Flag job as ready to start */
      pdata->inverse = inverse;
      pdata->nbolo = nbolo;
      pdata->ndims = ndims;
      memcpy( pdata->rdims, rdims, sizeof(pdata->rdims) );
      memcpy( pdata->fdims, fdims, sizeof(pdata->fdims) );
      pdata->retdata = retdata;

      if( inverse ) {        /* Performing inverse fft */
        /* Setup inverse FFT plan using guru interface */
        baseR = data->pntr[0];
        if( ndims == 1 ) baseI = baseR + nf*nbolo;
        else baseI = baseR + nf;
        baseD = retdata->pntr[0];

        thrMutexLock( &smf_fft_data_mutex, status );

        pdata->plan = fftw_plan_guru_split_dft_c2r( ndims, dims, 0, NULL,
                                                    baseR, baseI,
                                                    baseD,
                                                    FFTW_ESTIMATE |
                                                    FFTW_UNALIGNED );
        thrMutexUnlock( &smf_fft_data_mutex, status );
      } else {               /* Performing forward fft */
        /* Setup forward FFT plan using guru interface */
        baseD = data->pntr[0];
        baseR = retdata->pntr[0];
        if( ndims == 1 ) baseI = baseR + nf*nbolo;
        else baseI = baseR + nf;

        thrMutexLock( &smf_fft_data_mutex, status );
        pdata->plan = fftw_plan_guru_split_dft_r2c( ndims, dims, 0, NULL,
                                                    baseD,
                                                    baseR, baseI,
                                                    FFTW_ESTIMATE |
                                                    FFTW_UNALIGNED );
        thrMutexUnlock( &smf_fft_data_mutex, status );


        /* Set up WCS for Fourier Space data */

        if( indata->hdr && retdata->hdr ) {
          if( ndims == 1 ) {
            /* Transform of a time-series cube */

            if( *status == SAI__OK ) {
              AstUnitMap *cmapping=NULL;    /* Mapping grid to curframe3d */
              AstFrame *curframe1d=NULL;    /* 1d frame (real/imag coeff) */
              AstFrame *curframe2d=NULL;    /* 2d frame (bolo x,y) */
              AstCmpFrame *curframe3d=NULL; /* 3d frame (x,y,coeff) */
              AstCmpFrame *curframe4d=NULL; /* full 4-d FFT frame*/
              AstCmpMap *fftmapping=NULL;   /* Mapping GRID to curframe2d */
              AstCmpMap *mapping3d=NULL;    /* Mapping 3d GRID to FREQ, X, Y */
              AstZoomMap *scalemapping=NULL;/* Scale grid coordinates by df */
              AstSpecFrame *specframe=NULL; /* Current Frame of 1-D spectrum */
              AstCmpMap *specmapping=NULL;  /* Mapping from GRID to FREQ */
              AstFrameSet *fftwcs=NULL;      /* WCS for 4d FFT data */
              double zshift;                /* Amount to shift freq. origin */
              AstShiftMap *zshiftmapping=NULL; /* shift origin of freq GRID */
              AstMapping *zshiftmapping2=NULL; /* shift origin of bolo GRID */

              /* Start an AST context */
              astBegin;

              /* Create a new astFrameSet containing a 4d base GRID frame */
              fftwcs = astFrameSet( astFrame( 4, "Domain=GRID" ), " " );

              /* Get a Frame describing bolometer rows and columns, and a
                 Mapping from 2D GRID coords to this BOLO Frame. */
              sc2ast_make_bolo_frame( &curframe2d, &zshiftmapping2, status );

              /* The current frame will have frequency along the first axis,
                 x, y bolo coordinates along the second and third axes,
                 and the component along a fourth axis of length two (real,
                 imaginary coefficients). */

              specframe = astSpecFrame( "System=freq,Unit=Hz,"
                                        "StdOfRest=Topocentric" );

              curframe3d = astCmpFrame( specframe, curframe2d, " " );
              curframe1d = astFrame( 1,
                                     "Domain=COEFF,label=Real/Imag component");
              curframe4d = astCmpFrame( curframe3d, curframe1d, " " );

              /* The mapping from 4d grid coordinates to (frequency, x,
                 y, coeff) is accomplished with a shift and a zoommap
                 for the 1st dimension, and a shift for the others */

              zshift = -1;
              zshiftmapping = astShiftMap( 1, &zshift, " " );
              scalemapping = astZoomMap( 1, df[0], " " );
              specmapping = astCmpMap( zshiftmapping, scalemapping, 1, " " );

              mapping3d = astCmpMap( specmapping, zshiftmapping2, 0, " " );

              cmapping = astUnitMap( 1, " " );
              fftmapping = astCmpMap( mapping3d, cmapping, 0, " " );

              /* Add the curframe4d with the fftmapping to the frameset */
              astAddFrame( fftwcs, AST__BASE, fftmapping, curframe4d );

              /* Export the frameset before ending the AST context */
              astExport( fftwcs );
              astEnd;

              /* Free the old TSWCS if it exists, and insert the new FFTWCS */
              if( retdata->hdr->tswcs ) {
                retdata->hdr->tswcs = astAnnul(retdata->hdr->tswcs);
              }
              retdata->hdr->tswcs = fftwcs;
            }
          } else if( (ndims == 2) && (indata->hdr->wcs) ) {

            /* FFT of a map

               Note to self: maybe we can encode the lbnd values for
               the real-space map as a mean phase-shift for all of the
               Fourier components, and then recover them when we
               transform back.
            */

            AstCmpFrame *curframe=NULL;
            AstFrame *curframe_c=NULL;
            AstFrame *curframe_x=NULL;
            AstFrame *curframe_y=NULL;
            AstCmpMap *fftmapping=NULL;
            AstFrameSet *fftwcs=NULL;
            AstUnitMap *scalemap_c=NULL;
            AstZoomMap *scalemap_x=NULL;
            AstZoomMap *scalemap_y=NULL;

            /* Start an AST context */
            astBegin;

            /* Create a new frameset containing a 3d base GRID frame */
            fftwcs = astFrameSet( astFrame( 3, "Domain=GRID" ), " " );

            /* The current frame will consist of two spatial frequencies,
               and a fourier component */
            curframe_x =  astFrame( 1,"label=Spatial Frequency,Unit=1/arcsec");
            curframe_y =  astFrame( 1,"label=Spatial Frequency,Unit=1/arcsec");
            curframe_c =  astFrame( 1,"Domain=COEFF,label=Real/Imag component");

            curframe = astCmpFrame( astCmpFrame( curframe_x, curframe_y, " " ),
                                    curframe_c, " " );

            /* The mapping will scale by the spatial frequency step sizes
               for the first two axes, and we just use a unit mapping for
               the component axis */

            scalemap_x = astZoomMap( 1, df[0], " " );
            scalemap_y = astZoomMap( 1, df[1], " " );
            scalemap_c = astUnitMap( 1, " " );

            fftmapping = astCmpMap( astCmpMap( scalemap_x, scalemap_y, 0, " " ),
                                    scalemap_c, 0, " " );

            /* Add curframe with the fftmapping to the frameset */
            astAddFrame( fftwcs, AST__BASE, fftmapping, curframe );

            /* Export and store it in the place of TSWCS in the header. We
               do this so that we can preserve the 2D map WCS, and since
               it is 3-d smf_write_smfData will use it automagically */
            astExport( fftwcs );
            astEnd;

            if( retdata->hdr->tswcs ) {
              retdata->hdr->tswcs = astAnnul(retdata->hdr->tswcs);
            }
            retdata->hdr->tswcs = fftwcs;
          }
        }
      }

      if( !pdata->plan ) {
        *status = SAI__ERROR;
        errRep("", FUNC_NAME
               ": FFTW3 could not create plan for transformation",
               status);
      }
    }
  }

  /* Do the FFTs */
  thrBeginJobContext( wf, status );
  for( i=0; (*status==SAI__OK)&&i<njobs; i++ ) {
    pdata = job_data + i;
    pdata->ijob = thrAddJob( wf, THR__REPORT_JOB, pdata,
                               smfFFTDataParallel, 0, NULL, status );
  }

  /* Wait until all of the submitted jobs have completed */
  thrWait( wf, status );
  thrEndJobContext( wf, status );

  /* Each sample needs to have a normalization applied if forward FFT */
  if( (*status==SAI__OK) && !inverse ) {
    norm = 1. / (double) nr;
    val = retdata->pntr[0];

    for( j=0; j<nretdata; j++ ) {
      *val *= norm;
      val++;
    }
  }

  /* If we get here with good status, set isFFT to the length of the
     last real space dimensions if doing a forward transform (and -1 if
     inverse transform) */
  if( (*status==SAI__OK) && retdata ) {
    if( inverse ) {
      retdata->isFFT=-1;
    } else {
      retdata->isFFT = rdims[ndims-1];
    }

    /* Set the FITS keyword */
    if( retdata->hdr && retdata->hdr->fitshdr ) {
      atlPtfti( retdata->hdr->fitshdr, "ISFFT", (int)retdata->isFFT,
                "-1 if real space, 0 if unknown, >0 if FFT", status );
    }
  }

 CLEANUP:
  if( data ) smf_close_file( wf, &data, status );
  if( dims ) dims = astFree( dims );

  /* Clean up the job data array */
  if( job_data ) {
    for( i=0; i<njobs; i++ ) {
      pdata = job_data + i;
      /* Destroy the plans */
      thrMutexLock( &smf_fft_data_mutex, status );
      fftw_destroy_plan( pdata->plan);
      thrMutexUnlock( &smf_fft_data_mutex, status );
    }
    job_data = astFree( job_data );
  }

  return retdata;

}
