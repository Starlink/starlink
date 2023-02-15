/*
*+
*  Name:
*     smf_filter_execute

*  Purpose:
*     Filter smfData in the frequency domain using the FFTW3 library

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:

*     smf_filter_execute( ThrWorkForce *wf, smfData *data, smfFilter *filt,
*                         int complement, int whiten, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData * (Given and Returned)
*        The data to be filtered (performed in-place)
*     filter = smfFilter* (Given and Returned)
*        A smfFilter to apply to the supplied data
*     complement = int (Given)
*        If 1 set the filter to its complement (and whitening filter
*        if selected) before applying to the data.
*        If -1 set the filter back to its original state, and then apply it.
*     whiten = int (Given)
*        If set, prior to applying the supplied filter, apply a whitening
*        filter to each bolometer,
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Filter a smfData. If a pool of threads is supplied (wf), this routine
*     will filter multiple blocks of bolometers in parallel. If a NULL pointer
*     is supplied smf_filter_execute will not use any of the "thr" threads
*     routines. However, this function is thread safe, so that
*     smf_filter_execute can be called as part of a higher-level parallelized
*     routine in this case. If a 2d map is supplied, the data and filter will
*     be passed gracefully to smf_filter2d_execute, and the "whiten" parameter
*     will be ignored.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     David S Berry (JAC, Hawaii)
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
*     2010-04-01 (EC):
*        - add option of an external quality array
*     2010-06-22 (DSB):
*        Alternative approach to handling missing data - store zero for
*        missing values and then correct by normalising the filtered data
*        using a filtered mask.
*     2010-06-25 (DSB):
*        Doing apodisation here, each time the filter is applied,
*        rather than as a pre-processing step.
*     2010-09-09 (DSB):
*        Apodise all data regardless of quality, and report an error if a
*        bad value is encountered.
*     2010-10-12 (EC):
*        Optionally apply bolo-specific whitening filters before the
*        smfFilter
*     2010-10-13 (EC):
*        If complement set, apply complement of both the supplied and
*        whitening filters.
*     2011-04-14 (DSB):
*        - Do gap filling here.
*        - Use smf_apodize to apodise the data.
*     2011-04-26 (DSB):
*        Remove the effects of apodisation from the filtered data.
*     2011-10-26 (EC):
*        Once again apply normalization here immediately after transforms.
*     2013-12-02 (DSB):
*        Changed so that it can be used on a smfData with no quality (e.g. the
*        COM model).
*     2014-8-18 (DSB):
*        Ensure any BADDA samples have a bad data value on exit.

*  Copyright:
*     Copyright (C) 2011-2014 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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
   local copies of the entire smfData. */
typedef struct smfFilterExecuteData {
  dim_t b1;               /* Index of first bolometer to be filtered */
  dim_t b2;               /* Index of last bolometer to be filtered */
  int complement;          /* Using complementary filter */
  smfData *data;           /* Pointer to master smfData */
  double *data_fft_r;      /* Real part of the data FFT (1-bolo) */
  double *data_fft_i;      /* Imaginary part of the data FFT (1-bolo)*/
  smfFilter *filt;         /* Pointer to the filter */
  int ijob;                /* Job identifier */
  fftw_plan plan_forward;  /* for forward transformation */
  fftw_plan plan_inverse;  /* for inverse transformation */
  smf_qual_t *qua;         /* quality pointer */
  int whiten;              /* should the data be whitened? */
} smfFilterExecuteData;

/* Function to be executed in thread: filter all of the bolos from b1 to b2
   using the same filt */

void smfFilterExecuteParallel( void *job_data_ptr, int *status );

void smfFilterExecuteParallel( void *job_data_ptr, int *status ) {
  double ac, bd, aPb, cPd;      /* Components for complex multiplication */
  double *base=NULL;            /* Pointer to start of current bolo in array */
  dim_t bstride;               /* Bolometer stride */
  double *data_fft_r=NULL;      /* Real part of the data FFT */
  double *data_fft_i=NULL;      /* Imaginary part of the data FFT */
  smfData *data=NULL;           /* smfData that we're working on */
  double *dmask = NULL;         /* Array holding data values to be filtered */
  double fac;                   /* Factor for scaling the ral and imag parts */
  double fr;                    /* Real filter value */
  double fi;                    /* Imaginary filter value */
  double *inv_filt_r = NULL;    /* Array holding inverted real filter values */
  double *inv_filt_i = NULL;    /* Array holding inverted imaginary filter values */
  smfFilterExecuteData *pdata=NULL; /* Pointer to job data */
  smfFilter *filt=NULL;         /* Frequency domain filter */
  dim_t i;                     /* Loop counter */
  int iloop;                    /* Loop counter */
  int invert;                   /* Invert the filter to produce a low pass filter? */
  dim_t j;                     /* Loop counter */
  double *mask = NULL;          /* Array holding mask values to be filtered */
  dim_t nbolo;                  /* Number of bolometers */
  int newalg;                   /* Use new algorithm for handling missing data? */
  dim_t ntslice;                /* Number of time slices */
  smf_qual_t *qua=NULL;         /* pointer to quality */
  smf_qual_t *qbase=NULL;       /* pointer to first quality element */
  double rad;                   /* The modulus of the complex filter value */
  double unit;                  /* Unit filter value */
  double *use_filt_r = NULL;    /* Array holding real filter values to use */
  double *use_filt_i = NULL;    /* Array holding imaginary filter values to use */


  if( *status != SAI__OK ) return;

  /* Pointer to the data that this thread will process */
  pdata = job_data_ptr;

  /* Check for valid inputs */
  if( !pdata ) {
    *status = SAI__ERROR;
    errRep( "", "smfFilterExecuteParallel: No job data supplied", status );
    return;
  }

  data = pdata->data;
  filt = pdata->filt;
  data_fft_r = pdata->data_fft_r;
  data_fft_i = pdata->data_fft_i;
  qua = pdata->qua;

  if( !data ) {
    *status = SAI__ERROR;
    errRep( "", "smfFilterExecuteParallel: No valid smfData supplied",
            status );
    return;
  }

  if( !filt ) {
    *status = SAI__ERROR;
    errRep( "", "smfFilterExecuteParallel: No valid smfFilter supplied",
            status );
    return;
  }

  if( !data_fft_r || !data_fft_i ) {
    *status = SAI__ERROR;
    errRep( "", "smfFilterExecuteParallel: Invaid data_fft_* pointers provided",
            status );
    return;
  }

  /* Debugging message indicating thread started work */
  msgOutiff( SMF__TIMER_MSG, "",
             "smfFilterExecuteParallel: thread starting on bolos %zu -- %zu",
             status, pdata->b1, pdata->b2 );


  /* Filter the data one bolo at a time */
  smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, NULL, &bstride, NULL,
                status );

  /* if b1 past end of the work, nothing to do so we return */
  if( pdata->b1 >= nbolo ) {
    msgOutif( SMF__TIMER_MSG, "",
               "smfFilterExecuteParallel: nothing for thread to do, returning",
               status);
    return;
  }

  /* We have two alternative ways to handle mising data - the original,
     which is to apodise the data and then replace missing data with
     artificial noisey data before filtering it, or the new, which is to
     replace missing data with zeros before filtering it and then correct
     for this by normalising the filtered data using a mask filtered in
     the same way as the data. If "wlim" is VAL__BADD we use the original
     method. */
  newalg = ( filt->wlim != VAL__BADD );

  /* Missing or flagged data will be set to zero before filtering.
     The effect of this on the filtered data is removed by dividing the
     filtered data by the filtered mask (the mask is an array that is 1.0
     for usable data and 0.0 for missing data). However, if the total
     data sum in the effective smoothing kernel (i.e. the inverse fourier
     transformation of the filter function) is zero, then the filtered
     mask will be full of zeros, except maybe for values near to missing
     data, and so cannot be used to normalised the filtered data. Any
     filter that removes the DC level (i.e. a high pass or notch filter)
     will do this. So if we are doing a high pass or notch filter, we
     first invert the filter so that it becomes a low pass filter
     (i.e. we use filter (1-F), where F is the high pass filter), then
     filter both data and mask using the inverted filter, then normalised
     the filtered data by dividing it by the filtered mask, and then
     subtract the normalised filtered data from the original data. We
     have a high pass or notch filter if the first element of the filter
     has value zero. In this case we will use the inverted filter. */
  invert = ( newalg && filt->real[ 0 ] == 0.0 );
  if( filt->isComplex && filt->imag[ 0 ] != 0.0 ) invert = 0;

  /* If required, create arrays holding the inverted filter values */
  if( invert ) {
    unit = 1.0;

    if( filt->isComplex ) {

      inv_filt_r = astMalloc( filt->fdims[0]*sizeof( *inv_filt_r ) );
      inv_filt_i = astMalloc( filt->fdims[0]*sizeof( *inv_filt_i ) );
      if( *status == SAI__OK ) {

        for( j = 0; j < filt->fdims[0]; j++ ) {
          fr = filt->real[ j ];
          fi = filt->imag[ j ];
          rad = sqrt( fr*fr + fi*fi );

          if( rad != 0.0 ) {
            fac = unit/rad - 1.0;
            inv_filt_r[ j ] = fr*fac;
            inv_filt_i[ j ] = fi*fac;
          } else {
            inv_filt_r[ j ] = unit;
            inv_filt_i[ j ] = 0.0;
          }

        }

        use_filt_r = inv_filt_r;
        use_filt_i = inv_filt_i;
      }

    } else {

      inv_filt_r = astMalloc( filt->fdims[0]*sizeof( *inv_filt_r ) );
      if( *status == SAI__OK ) {

        for( j = 0; j < filt->fdims[0]; j++ ) {
          fr = filt->real[ j ];
          inv_filt_r[ j ] = unit - fr;
        }

        use_filt_r = inv_filt_r;
      }
    }

  /* Otherwise, use the supplied filter values */
  } else {
    use_filt_r = filt->real;
    use_filt_i = filt->imag;
  }

  /* Allocate work arrays to hold the mask and the masked data array */
  if( newalg ) {
    mask = astMalloc( ntslice*sizeof( *mask ) );
    dmask = astMalloc( ntslice*sizeof( *dmask ) );
  }

  /* Loop round filtering all bolometers. */
  for( i=pdata->b1; (*status==SAI__OK) && (i<=pdata->b2); i++ ) {
    qbase = qua ? qua + i*bstride : NULL;

    if( !qbase || !(qbase[0]&SMF__Q_BADB) ) { /* Check for bad bolo flag */

      /* Obtain pointer to the correct chunk of data */
      base = data->pntr[0];
      base += i*bstride;

      /* Create a mask array that is the same length as the data array,
         with 1.0 at every usable data value and 0.0 at every unusable
         data value. At the same time, create a new data array by
         multiplying the original data array by this mask. */
      if( newalg ) {
         for( j = 0; j < ntslice; j++ ){

            if( base[ j ] == VAL__BADD ){
               if( *status == SAI__OK ){
                  *status = SAI__ERROR;
                  errRep( "", "VAL__BADD value encountered in smf_filter_execute "
                          "(programming error).", status );
               }
               break;

            } else if( !qbase || !( qbase[ j ] & SMF__Q_GOOD ) ) {
               mask[ j ] = 1.0;
               dmask[ j ] = base[ j ];

            } else {
               mask[ j ] = 0.0;
               dmask[ j ] = 0.0;
            }
         }

      } else {
         dmask = base;
      }

      /* Loop to filter first the masked data array, and then the mask,
         using the same filter. */
      for( iloop = 0; iloop < ( newalg ? 2 : 1 ); iloop++ ) {
         base = iloop ? mask : dmask;

         /* Execute forward transformation using the guru interface */
         fftw_execute_split_dft_r2c( pdata->plan_forward, base,
                                     data_fft_r, data_fft_i );

         /* Apply 1/N normalization */
         if( *status == SAI__OK ) {
           double val = 1. / (double) filt->rdims[0];
           for( j=0; j<filt->fdims[0]; j++ ) {
             data_fft_r[j] *= val;
             data_fft_i[j] *= val;
           }
         }

         /* Whiten the power spectrum if requested. */
         if( (iloop==0) && (pdata->whiten) ) {
           smf_whiten( data_fft_r, data_fft_i, filt->df[0], filt->fdims[0], 50,
                       pdata->complement, status );
         }

         /* Apply the frequency-domain filter. Skip this step if the
            filter values are NULL (i.e. if we are only whitening) */
         if( use_filt_r && (*status==SAI__OK) ) {
           if( filt->isComplex ) {
             for( j=0; j<filt->fdims[0]; j++ ) {
               /* Complex times complex, using only 3 multiplies */
               ac = data_fft_r[j] * use_filt_r[j];
               bd = data_fft_i[j] * use_filt_i[j];

               aPb = data_fft_r[j] + data_fft_i[j];
               cPd = use_filt_r[j] + use_filt_i[j];

               data_fft_r[j] = ac - bd;
               data_fft_i[j] = aPb*cPd - ac - bd;
             }
           } else {
             for( j=0; j<filt->fdims[0]; j++ ) {
               /* Complex times real */
               data_fft_r[j] *= use_filt_r[j];
               data_fft_i[j] *= use_filt_r[j];
             }
           }
         }

         /* Perform inverse transformation using guru interface */
         fftw_execute_split_dft_c2r( pdata->plan_inverse, data_fft_r,
                                     data_fft_i, base );
      }


      /* Divide the filtered masked data array by the filtered mask,
         thus normalising the filtered data and removing any ringing
         introduced by missing data values. The wlim parameter allows
         control of what happens close to missing data - a value of zero
         means "retain as much data as possible, even though the data
         close to a missing section may be biassed by the asymetric
         distribution of usable data values", a value of 1.0 "retain only
         data that is definately not biassed by the asymetric
	 distribution of usable data values", value inbetween 0.0 and 1.0
	 are mid way between these extremes. If we inverted the filter
	 before filtering, we now need to compensate for the inversion by
	 subtracting the filtered (and normalised) data from the supplied
	 data. */
      if( newalg ) {
        base = data->pntr[0];
        base += i*bstride;

        if( invert ) {

          for( j = 0; j < ntslice; j++ ){
            if( mask[ j ] >= filt->wlim && mask[ j ] != 0.0 ) {
              base[ j ] -= dmask[ j ]/mask[ j ];

            /* If the current filtered data value has contributions from
               an insufficient faction of good input data values, flag
               it. */
            } else if( qbase ) {
               qbase[ j ] |= SMF__Q_FILT;
            }
          }

        } else {

          for( j = 0; j < ntslice; j++ ){
            if( mask[ j ] >= filt->wlim && mask[ j ] != 0.0 ) {
              base[ j ] = dmask[ j ]/mask[ j ];
            } else if( qbase ) {
              qbase[ j ] |= SMF__Q_FILT;
            }
          }

        }
      }

      /* If a quality array is present ensure any BADDA samples have a
         bad data value on exit. */
      if( qbase ) {
        base = data->pntr[0];
        base += i*bstride;
        for( j = 0; j < ntslice; j++ ){
          if( qbase[ j ] & SMF__Q_BADDA ) base[ j ] = VAL__BADD;
        }
      }
    }
  }

  /* Free work arrays */
  if( newalg ) {
     mask = astFree( mask );
     dmask = astFree( dmask );
     inv_filt_r = astFree( inv_filt_r );
     inv_filt_i = astFree( inv_filt_i );
  }

  msgOutiff( SMF__TIMER_MSG, "",
             "smfFilterExecuteParallel: thread finishing bolos %zu -- %zu",
             status, pdata->b1, pdata->b2 );

}

/* ------------------------------------------------------------------------ */

#define FUNC_NAME "smf_filter_execute"

void smf_filter_execute( ThrWorkForce *wf, smfData *data, smfFilter *filt,
                         int complement, int whiten, int *status ) {

  /* Local Variables */
  dim_t apod_length=0;            /* apodization length */
  fftw_iodim dims;                /* I/O dimensions for transformations */
  dim_t first;                    /* First sample apodization at start */
  int i;                          /* Loop counter */
  smfFilterExecuteData *job_data=NULL;/* Array of job data for each thread */
  dim_t last;                     /* Last sample apodization at end */
  dim_t nbolo=0;                  /* Number of bolometers */
  dim_t ndata=0;                  /* Total number of data points */
  int nw;                         /* Number of worker threads */
  dim_t ntslice=0;                /* Number of time slices */
  smf_qual_t *qua=NULL;           /* Pointer to quality flags */
  smfFilterExecuteData *pdata=NULL; /* Pointer to current job data */
  dim_t step;                     /* step size for dividing up work */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Check for NULL pointers */
  if( !data ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL smfData pointer", status );
    return;
  }

  if( !filt ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL smfFilter pointer", status );
    return;
  }

  if( filt->ndims == 2 ) {
    smf_filter2d_execute( wf, data, filt, complement, status );
    return;
  }

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Ensure that the smfData is ordered correctly (bolo ordered) */
  smf_dataOrder( wf, data, 0, status );

  /* Obtain the dimensions of the array being filtered */
  smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, &ndata, NULL, NULL, status);

  if( *status != SAI__OK ) return;

  /* Using complement of the filter? */
  if( complement ) smf_filter_complement( filt, status );

  /* Pointers to quality */
  qua = smf_select_qualpntr( data, NULL, status );

  /* Determine the first and last samples to apodize (after padding), if
     any. Assumed to be the same for all bolometers. */
  if( qua ) {
     smf_get_goodrange( qua, ntslice, 1, SMF__Q_PAD, &first, &last, status );
  } else {
     first = 0;
     last = ntslice - 1;
  }

  /* Can we apodize? */
  apod_length = filt->apod_length;
  if( *status == SAI__OK ) {
    if( apod_length == SMF__MAXAPLEN ) {
      apod_length = (last-first+1)/2;
      msgOutiff( MSG__DEBUG, "", FUNC_NAME
                 ": Using maximum apodization length, %zu samples.",
                 status, apod_length );
    } else if( (last-first+1) < (2*apod_length) && apod_length != SMF__BADSZT ){
      *status = SAI__ERROR;
      errRepf("", FUNC_NAME
              ": Can't apodize, not enough samples (%zu < %zu).", status,
              last-first+1, 2*apod_length);
    }
  }

  /* If apodising is switched off, fill gaps in the data and re-create
     the artifical data used for padding based on the current contents of
     the smfData. */
  if( apod_length == SMF__BADSZT ) {
    smf_fillgaps( wf, data, SMF__Q_PAD | SMF__Q_GAP, status );

  /* If apodising is switched on, fill the data (retaining the zero padding)
     and apodise the data. */
  } else {
    smf_fillgaps( wf, data, SMF__Q_GAP, status );
    if( apod_length > 0 ) smf_apodize( data, apod_length, 1, status );
  }

  /* Describe the input and output array dimensions for FFTW guru interface.
     - dims describes the length and stepsize of time slices within a bolometer
  */

  dims.n = (int) ntslice;
  dims.is = 1;
  dims.os = 1;

  /* Set up the job data */

  if( nw > (int) nbolo ) {
    step = 1;
  } else {
    step = nbolo/nw;
  }

  job_data = astCalloc( nw, sizeof(*job_data) );
  for( i=0; (*status==SAI__OK)&&i<nw; i++ ) {
    pdata = job_data + i;

    pdata->b1 = i*step;
    pdata->b2 = (i+1)*step-1;

    /* Ensure that the last thread picks up any left-over bolometers */
    if( (i==(nw-1)) && (pdata->b1<(nbolo-1)) ) {
      pdata->b2=nbolo-1;
    }
    pdata->data = data;
    pdata->qua = qua;

    pdata->data_fft_r = astMalloc(filt->fdims[0]*sizeof(*pdata->data_fft_r));
    pdata->data_fft_i = astMalloc(filt->fdims[0]*sizeof(*pdata->data_fft_i));
    pdata->filt = filt;
    pdata->whiten = whiten;
    pdata->complement = complement;
    pdata->ijob = -1;   /* Flag job as ready to start */

    /* Setup forward FFT plan using guru interface. Requires protection
       with a mutex */
    thrMutexLock( &smf_filter_execute_mutex, status );

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

    thrMutexUnlock( &smf_filter_execute_mutex, status );

    if( !pdata->plan_forward && (*status == SAI__OK) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME
              ": FFTW3 could not create plan for forward transformation",
              status);
    }

    /* Setup inverse FFT plan using guru interface */
    thrMutexLock( &smf_filter_execute_mutex, status );

    if( *status == SAI__OK ) {
      pdata->plan_inverse = fftw_plan_guru_split_dft_c2r( 1, &dims, 0, NULL,
                                                          pdata->data_fft_r,
                                                          pdata->data_fft_i,
                                                          data->pntr[0],
                                                          FFTW_ESTIMATE |
                                                          FFTW_UNALIGNED);
    }

    thrMutexUnlock( &smf_filter_execute_mutex, status );

    if( !pdata->plan_inverse && (*status==SAI__OK) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME
              ": FFTW3 could not create plan for inverse transformation",
              status);
    }

  }

  /* Execute the filter */
  for( i=0; (*status==SAI__OK)&&i<nw; i++ ) {
    pdata = job_data + i;
    pdata->ijob = thrAddJob( wf, THR__REPORT_JOB, pdata,
                               smfFilterExecuteParallel, 0,
                               NULL, status );
  }

  /* Wait until all of the submitted jobs have completed */
  thrWait( wf, status );

  /* Clean up the job data array */
  if( job_data ) {
    for( i=0; i<nw; i++ ) {
      pdata = job_data + i;
      if( pdata->data_fft_r ) pdata->data_fft_r = astFree( pdata->data_fft_r );
      if( pdata->data_fft_i ) pdata->data_fft_i = astFree( pdata->data_fft_i );

      /* Destroy the plans */
      thrMutexLock( &smf_filter_execute_mutex, status );
      fftw_destroy_plan( pdata->plan_forward );
      fftw_destroy_plan( pdata->plan_inverse );
      thrMutexUnlock( &smf_filter_execute_mutex, status );
    }
    job_data = astFree( job_data );
  }

  /* Return the filter to its original state if required */
  if( complement == -1 ) smf_filter_complement( filt, status );


  /* Remove the effects of the apodisation from the filtered data. */
  if( apod_length != SMF__BADSZT && apod_length > 0 ) {
     smf_apodize( data, apod_length, 0, status );
  }

}
