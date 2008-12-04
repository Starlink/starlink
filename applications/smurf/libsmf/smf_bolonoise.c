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
*     smf_bolonoise( smfData *data, size_t window, double f_low, 
*                    double f_white1, double f_white2, double flagratio,
*                    double *whitenoise, double *fratio, int nep, int *status )

*  Arguments:
*     data = smfData * (Given)
*        Pointer to the input smfData.
*     window = size_t (Given)
*        Width of boxcar smooth to apply to power spectrum before measurement
*     f_low = double (Given)
*        Frequency (Hz) at which to measure low-frequency power
*     f_white1 = double (Given)
*        Lower frequency edge of window for calculating average white noise
*     f_white2 = double (Given)
*        Upper frequency edge of window for calculating average white noise
*     flagratio = double (Given)
*        If nonzero, limit for fratio below which bolo is flagged as bad
*     whitenoise = double* (Given)
*        Estimate of variance in bolo signals produced by white noise. 
*        Can be NULL.
*     fratio = double* (Given)
*        Array containing ratio of noise at f_low to the average from
*        f_white1 to f_white2. Can be NULL.
*     nep = int (Given)
*        If set, calculate whitenoise in 1 second of averaged time-series data.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:

*  Description: 
*     Calculate the power spectrum for each detector. Optionally smooth
*     the power spectrum with a boxcar filter. Measure the noise at a single
*     low-frequency (f_low), and the average white-noise level over a range
*     f_white1 to f_white2. The ratio of the first to the second number
*     (fratio) is an estimate of the low-frequency noise of the detector
*     an can be used to flag dead detectors, the idea being that little
*     or no optical power from the sky reaches them so they have relatively
*     flat power spectra. Arrays containing the white noise level (whitenoise)
*     and low-to-high frequency power ratio (fratio) can also be returned
*     containing values for each bolometer. Whitenoise by default estimates
*     the variance in the time-domain samples caused by the "white" part of
*     the spectrum (i.e. on a per-sample basis). Setting the "NEP" flag
*     calculates the effective variance expected in an average of 1 second
*     of time stream data (i.e. appropriate for calculating NEP and NEFD). 

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

*  Copyright:
*     Copyright (C) 2008 University of British Columbia.
*     Copyright (C) 2008 Science and Technology Facilities Council.
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

void smf_bolonoise( smfData *data, size_t window, double f_low, 
                    double f_white1, double f_white2, double flagratio,
                    double *whitenoise, double *fratio, int nep, int *status ) {

  double *base=NULL;       /* Pointer to base coordinates of array */
  double df=1;             /* Frequency step size in Hz */
  double fr;               /* Ratio of p_low to p_white */
  size_t i;                /* Loop counter */
  size_t i_low;            /* Index in power spectrum to f_low */
  size_t i_w1;             /* Index in power spectrum to f_white1 */
  size_t i_w2;             /* Index in power spectrum to f_white2 */
  int isTordered;          /* Order of the bolometer data */
  size_t j;                /* Loop counter */
  dim_t nbolo;             /* Number of bolometers */
  dim_t ndata;             /* Number of data points */
  dim_t nf;                /* Number of frequencies */
  dim_t ngood;             /* Number of good samples */
  dim_t ntslice;           /* Number of time slices */
  double p_low;            /* Power at f_low */
  double p_white;          /* Average power from f_white1 to f_white2 */
  smfData *pow=NULL;       /* Pointer to power spectrum data */
  unsigned char *qua=NULL; /* Pointer to quality component */
  double steptime=1;       /* Length of a sample in seconds */

  if (*status != SAI__OK) return;

  /* Check inputs */
  smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status );

  if( !data->hdr ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData has no header", status );
    return;
  }

  /* Obtain dimensions */
  smf_get_dims( data, &nbolo, &ntslice, &ndata, NULL, NULL, status );

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

  qua = data->pntr[2];
  isTordered = data->isTordered;

  /* Initialize arrays */
  if( whitenoise ) for(i=0; i<nbolo; i++) whitenoise[i] = VAL__BADD;
  if( fratio ) for(i=0; i<nbolo; i++) fratio[i] = VAL__BADD;

  /* FFT the data and convert to polar power form */
  pow = smf_fft_data( data, 0, status );
  smf_fft_cart2pol( pow, 0, 1, status );
  smf_isfft( pow, NULL, NULL, &nf, status );

  /* Check for reasonble frequencies, and integer offsets in the array */
  i_low = smf_get_findex( f_low, df, nf, status );
  i_w1 = smf_get_findex( f_white1, df, nf, status );
  i_w2 = smf_get_findex( f_white2, df, nf, status );

  /* Loop over detectors */
  for( i=0; (*status==SAI__OK)&&(i<nbolo); i++ ) 
    if( !qua || 
        (isTordered && !(qua[i]&SMF__Q_BADB)) |
        (!isTordered && !(qua[i*ntslice]&SMF__Q_BADB)) ) {

    /* Pointer to start of power spectrum */
    base = pow->pntr[0];
    base += nf*i;

    /* Smooth the power spectrum */
    smf_boxcar1D( base, nf, window, NULL, 0, status );

    /* Measure the power */
    if( *status == SAI__OK ) {
      p_low = base[i_low];
      smf_stats1( base, i_w1, i_w2-i_w1+1, NULL, 0, &p_white, NULL, &ngood,
                  status );

      /* It's OK if bad status was generated as long as a mean was calculated */
      if( (*status==SMF__INSMP) && (ngood>0) ) {
        errAnnul( status );
      }

      /* Generate an error if power <= 0 */
      if( (*status==SAI__OK) && ( (p_low<=0) || (p_white<=0) ) ) {
        *status = SAI__ERROR;
        msgSeti("BOLO",i);
        errRep( "", FUNC_NAME ": power <= 0 detected in bolometer ^BOLO", 
                status);
      }
    }

    if( *status == SAI__OK ) {
      /* Power ratio */
      fr = p_low/p_white;

      /* Flag bad bolometer */
      if( fratio && qua && (fr < flagratio) ) {
        if( isTordered ) {
          for( j=0; j<ntslice; j++ ) {
            qua[nbolo*j + i] |= SMF__Q_BADB;
          }
        } else {
          for( j=0; j<ntslice; j++ ) {
            qua[j + i*ntslice] |= SMF__Q_BADB;
          }
        }
      }

      /* Store values */
      if( whitenoise ) {
        /* Multiply average by bandwidth to get estimate of signal variance */
        whitenoise[i] = p_white * 2 * (i_w2-i_w1+1);
        /* If NEP set, scale this to variance in 1-second average by dividing
           by the number of samples per second */
        if( nep ) {
          whitenoise[i] /= (1./steptime);
        }
      }
      if( fratio ) fratio[i] = fr;
    }

  }
  
  /* Clean up */
  if( pow ) smf_close_file( &pow, status );
}
