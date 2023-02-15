/*
*+
*  Name:
*     smf_bolonoise

*  Purpose:
*     Obtain bolometer noise properties from power spectrum

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_bolonoise( ThrWorkForce *wf, smfData *data,
*                    double gfrac, dim_t window, double f_low,
*                    double f_white1, double f_white2,
*                    int nep, dim_t len, double *whitenoise, double *fratio,
*                    smfData **fftpow, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     data = smfData * (Given)
*        Pointer to the input smfData.
*     gfrac = double (Given)
*        The fraction of supplied bolometer samples that must be good for
*        a power to be determined. Any bolometer which has fewer than
*        this fraction of good values is left with a VAL__BADD value in
*        "whitemoise". Supply a negative value if you do not want this
*        check to be performed.
*     window = dim_t (Given)
*        Width of boxcar smooth to apply to power spectrum before measurement
*     f_low = double (Given)
*        Frequency (Hz) at which to measure low-frequency power
*     f_white1 = double (Given)
*        Lower frequency edge of window for calculating average white noise
*     f_white2 = double (Given)
*        Upper frequency edge of window for calculating average white noise
*     nep = int (Given)
*        If set, calculate whitenoise in 1 second of averaged time-series data
*        by dividing by the sample rate.
*     len = dim_t (Given)
*        Number of samples over which to apply apodization. Can be set to
*        SMF__MAXAPLEN in which case the routine will automatically apodize
*        the entire data stream (maximum valid value of len). Set it to
*        zero to prevent apodisation (e.g. if the data has already been
*        apodised). Set it to SMF__BADSZT to cause the paddded regions
*        to be filled with artificial data based on the current contents
*        of the smfData (no apodising is performed in this case).
*     whitenoise = double* (Returned)
*        Externally allocated array (nbolos) that will hold estimates of
*        the mean-square variances in bolo signals produced by white noise.
*        Can be NULL.
*     fratio = double* (Returned)
*        Externally allocated array (nbolos) that will hold ratios of noise at
*        f_low to the average from f_white1 to f_white2. Can be NULL.
*     fftpow = smfData ** (Returned)
*        Power spectrum of the time series returned in a smfData. Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Calculate the power spectrum for each detector. Optionally smooth
*     the power spectrum with a boxcar filter. Measure the noise at a single
*     low-frequency (f_low), and the average white-noise level over a range
*     f_white1 to f_white2. The ratio of the first to the second number
*     (fratio) is an estimate of the low-frequency noise of the detector
*     and can be used to flag dead detectors, the idea being that little
*     or no optical power from the sky reaches them so they have relatively
*     flat power spectra.
*
*     Arrays containing the white noise level (whitenoise) and
*     low-to-high frequency power ratio (fratio) can also be returned
*     containing values for each bolometer. Whitenoise by default
*     estimates the variance in the time-domain samples caused by the
*     "white" part of the spectrum as the average in the mean square
*     power between f_white1 and and f_white, and then assuming it holds
*     at all frequencies in the data (i.e. multiply the power by nsamples).
*
*     Setting the "NEP" flag calculates the effective variance
*     expected in an average of 1 second of time stream data
*     (i.e. appropriate for calculating NEP and NEFD) by dividing by
*     the sample rate.
*
*     If either the low-frequency or white noise power are zero, and
*     a quality array exists, the bolometer in question will have the
*     SMF__Q_BADB flag set.

*  Notes:

*  Authors:
*     EC: Ed Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-09-06 (EC):
*        Initial version
*     2008-10-22 (EC):
*        Added NEP flag
*     2008-11-19 (TIMJ):
*        Close pow smfData rather than just freeing it.
*     2008-12-17 (EC):
*        -Use b/tstride
*        -If no power detected, instead of bad status, set SMF__Q_BADB
*     2009-10-01 (EC):
*        - multiply white noise by total number of samples rather than
*          measurement bandwidth.
*     2009-10-06 (TIMJ):
*        - continue even if not enough samples
*        - update bad values after FFT
*     2009-10-13 (TIMJ):
*        Allow the caller to retain the FFT. Move NEP parameter
*        before return values.
*     2010-07-07 (TIMJ):
*        Can not have const input because we update its quality.
*        No longer explicit sidecar quality as argument.
*     2011-02-01 (TIMJ):
*        Remove flagratio argument.
*     2011-03-04 (EC):
*        Account for smf_fft_cart2pol behaviour change when PSD requested

*  Copyright:
*     Copyright (C) 2008-2009,2011 University of British Columbia.
*     Copyright (C) 2008-2011 Science and Technology Facilities Council.
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

#define FUNC_NAME "smf_bolonoise"

void smf_bolonoise( ThrWorkForce *wf, smfData *data, double gfrac,
                    dim_t window, double f_low,
                    double f_white1, double f_white2,
                    int nep, dim_t len, double *whitenoise, double *fratio,
                    smfData **fftpow,int *status ) {

  double *base=NULL;       /* Pointer to base coordinates of array */
  dim_t bstride;          /* bolometer index stride */
  double df=1;             /* Frequency step size in Hz */
  dim_t i;                /* Loop counter */
  dim_t i_low;            /* Index in power spectrum to f_low */
  dim_t i_w1;             /* Index in power spectrum to f_white1 */
  dim_t i_w2;             /* Index in power spectrum to f_white2 */
  dim_t j;                /* Loop counter */
  dim_t mingood;          /* Min. required no. of good values in bolometer */
  dim_t nbolo;             /* Number of bolometers */
  dim_t ndata;             /* Number of data points */
  dim_t nf=0;              /* Number of frequencies */
  dim_t ngood;            /* Number of good samples */
  dim_t ntslice;           /* Number of time slices */
  double p_low;            /* Power at f_low */
  double p_white;          /* Average power from f_white1 to f_white2 */
  smfData *pow=NULL;       /* Pointer to power spectrum data */
  smf_qual_t *qua=NULL; /* Pointer to quality component */
  double steptime=1;       /* Length of a sample in seconds */
  dim_t tstride;          /* time index stride */

  if (*status != SAI__OK) return;

  /* Check inputs */
  if (!smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status )) return;

  if( !data->hdr ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData has no header", status );
    return;
  }

  /* Obtain dimensions */
  smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, &ndata, &bstride, &tstride,
                status );

  if( *status==SAI__OK ) {
    steptime = data->hdr->steptime;
    if( steptime < VAL__SMLD ) {
      *status = SAI__ERROR;
      errRep("",  FUNC_NAME ": FITS header error, STEPTIME must be > 0",
             status);
    } else {
      /* Frequency steps in the FFT */
      df = 1. / (steptime * (double) ntslice );
    }
  }

  /* Initialize arrays */
  if( whitenoise ) for(i=0; i<nbolo; i++) whitenoise[i] = VAL__BADD;
  if( fratio ) for(i=0; i<nbolo; i++) fratio[i] = VAL__BADD;

  /* FFT the data and convert to polar power spectral density form */
  pow = smf_fft_data( wf, data, NULL, 0, len, status );
  smf_convert_bad( wf, pow, status );
  smf_fft_cart2pol( wf, pow, 0, 1, status );

  {
    dim_t fdims[2];
    smf_isfft( pow, NULL, NULL, fdims, NULL, NULL, status );
    if( *status == SAI__OK ) nf=fdims[0];
  }

  /* Check for reasonble frequencies, and integer offsets in the array */
  i_low = smf_get_findex( f_low, df, nf, status );
  i_w1 = smf_get_findex( f_white1, df, nf, status );
  i_w2 = smf_get_findex( f_white2, df, nf, status );

  /* Get the quality pointer from the smfData so that we can mask known
     bad bolometer. */
  qua = smf_select_qualpntr( data, NULL, status );

  /* The minimum required number of good values in a bolometer. */
  mingood = ( gfrac > 0.0 ) ? ntslice*gfrac : 0;

  /* Loop over detectors */
  for( i=0; (*status==SAI__OK)&&(i<nbolo); i++ )
    if( !qua || !(qua[i*bstride]&SMF__Q_BADB) ) {

    /* Pointer to start of power spectrum */
    base = pow->pntr[0];
    base += nf*i;

    /* Smooth the power spectrum */
    smf_boxcar1D( base, nf, 1, window, NULL, 0, 1, NULL, status );

    /* Measure the power */
    if( *status == SAI__OK ) {
      p_low = base[i_low];
      smf_stats1D( base+i_w1, 1, i_w2-i_w1+1, NULL, 0, 0, &p_white, NULL, NULL,
                   &ngood, status );

      /* It's OK if bad status was generated as long as a mean was calculated */
      if( *status==SMF__INSMP ) {
        errAnnul( status );
        /* if we had no good data there was probably a problem with SMF__Q_BADB
           so we simply go to the next bolometer */
        if (ngood == 0) continue;
      }

      /* Count the number of initially good values for the current
         bolometer. */
      if( (*status==SAI__OK) && qua ){
         ngood = 0;
         for( j=0; j<ntslice; j++ ) {
           if( qua[i*bstride + j*tstride] == 0 ) ngood++;
         }

         /* Set bolometer to bad if no power detected, or the number of good
            values is too low.  */
         if( (p_low <= 0) || (p_white <= 0) || (ngood < mingood) ) {
           for( j=0; j<ntslice; j++ ) {
             qua[i*bstride + j*tstride] |= SMF__Q_BADB;
           }
         }
      }
    }

    if( (*status==SAI__OK) && (!qua || !(qua[i*bstride]&SMF__Q_BADB)) ) {

      /* Power ratio requested */
      if ( fratio ) {
        fratio[i] = p_low/p_white;
      }

      /* Store values */
      if( whitenoise ) {
        /* Integrate the PSD by multiplying the average white noise
           level by total number of samples and the frequency spacing:
           this calculates the time-domain variance (in 200 Hz SCUBA-2
           samples for example) assuming this level holds at all
           frequencies. */

        whitenoise[i] = p_white * ntslice * df;

        /* If NEP set, scale this to variance in a 1-second average by
           dividing by the sampling frequency (equivalent to
           multiplying by sample length). */

        if( nep ) {
          whitenoise[i] *= steptime;
        }
      }
    }
  }

  /* Clean up if the caller does not want to take over the power spectrum */
  if( pow ) {
    if (fftpow) {
      *fftpow = pow;
    } else {
      smf_close_file( wf, &pow, status );
    }
  }
}
