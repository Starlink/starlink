/*
*+
*  Name:
*     smf_whiten

*  Purpose:
*     Low-level routine to apply whitening factors to the FFT of bolometer data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_whiten( double *re, double *im, double df, dim_t nf, size_t box,
*                 double scale, int complement, int *status );

*  Arguments:
*     re = double * (Given and Returned)
*        Real part of the FFT.
*     im = double * (Given and Returned)
*        Imaginary part of the FFT.
*     df = double (Given)
*        Frequency step size in FFT
*     nf = dim_t (Given)
*        number of frequencies in the FFT
*     box = size_t (Given)
*        rolling box size for identifying 1/f region
*     scale = double (Given)
*        If non-zero apply this scale factor to the amplitude of each element
*        in the FFT (e.g. the 1/ntslice normalization required if no smfFilter
*        is being applied to the data.)
*     complement = int (Given)
*        If set, apply the complement of the whitening filter, where the
*        reference value is taken to be the value of scale (1 otherwise).
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:

*  Description:
*     The purpose of this function is to fit a model to the power spectrum
*     of a bolometer, e.g. a 1/f component at low-frequencies transitioning
*     to white component at high-frequencies. The whitening filter is the
*     filter which, when applied to the data, results in a white power
*     spectrum.

*  Notes:
*     If the 1/f spectrum appears to extend into the white-noise band,
*     or returns an exponent >= 0 (i.e. increasing instead of decreasing),
*     the function will simply return without doing anything to the data.

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-10-12 (EC):
*        Initial version
*     2010-10-13 (EC):
*        - add complement to interface
*        - add sanity checks for 1/f fitting frequencies and exponent
*     {enter_further_changes_here}

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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* System includes */
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "fftw3.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define SMF__WHITEN_WHITESNR 5        /* n-sigma clip for white noise level */
#define SMF__WHITEN_KNEESNR 5         /* SNR threshold for finding 1/f knee */
#define SMF__WHITEN_KNEETHRESH 0.25   /* fraction of samples below kneesnr */

#define FUNC_NAME "smf_whiten"

void smf_whiten( double *re, double *im, double df, dim_t nf, size_t box,
                 double scale, int complement, int *status ) {

  int converged;         /* Has white noise level calc converged? */
  double fit[2];         /* fit coefficients */
  size_t i;              /* Loop counter */
  size_t i_whi;          /* Index of high-freq. edge for white-noise level */
  size_t i_wlo;          /* Index of low-freq. edge for white-noise level */
  size_t nbad;           /* Number of outliers in white noise calc */
  double *pspec=NULL;    /* Power spectrum of the bolometer */
  smf_qual_t *qual=NULL; /* Quality to go with pspec */
  double sigma;          /* Uncertainty in white noise level */
  size_t thisnbad;       /* Outliers in white noise calc, this iteration */
  double white;          /* White noise level */
  double *x=NULL;
  double *y=NULL;

  if (*status != SAI__OK) return;

  if( !re || !im ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL pointer supplied.", status );
    return;
  }

  if( (df<=0) || (nf<1) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": invalid frequency information supplied.", status );
    return;
  }

  if( box*2 > nf ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": box too small compared to number of frequencies",
            status );
    return;
  }

  /* Calculate the power spectrum and a smoothed version. Also create a
     quality array to go with it. */
  pspec = astMalloc( nf*sizeof(*pspec) );
  qual = astCalloc( nf, sizeof(*qual) );

  if( *status == SAI__OK ) {
    for( i=0; i<nf; i++ ) {
      pspec[i] = re[i]*re[i] + im[i]*im[i];
    }
  }

  /* Measure the white noise level in some frequency range of the data
     well above the 1/f knee, but below the point at which the
     anti-aliasing filter begins to attenuate the data, using
     iterative rejection of outliers */

  i_wlo = smf_get_findex( 5, df, nf, status );
  i_whi = smf_get_findex( 20, df, nf, status );

  converged = 0;
  nbad = 0;

  while( (!converged) && (*status==SAI__OK) ) {
    double thresh;

    smf_stats1D( pspec+i_wlo, 1, i_whi-i_wlo+1, qual, 1, SMF__Q_SPIKE, &white,
                 &sigma, NULL, status );

    if( *status==SAI__OK ) {

      thresh = white + SMF__WHITEN_WHITESNR*sigma;
      thisnbad = 0;

      for( i=i_wlo; i<=i_whi; i++ ) {
        if( pspec[i] > thresh ) {
          qual[i] = SMF__Q_SPIKE;
          thisnbad++;
        }
      }

      if( thisnbad==nbad ) {
        converged = 1;
      }

      nbad = thisnbad;
    }
  }

  /* Now identify and fit the 1/f part of the power spectrum. We use a
     rolling box and increase the frequency of its centre until the fraction
     of samples below some threshold above the white level is exceeded.
     We then fit a straight line to the logarithm of the x- and y-axes. */

  if( *status == SAI__OK ) {
    double A;
    double B;
    size_t nfit;
    size_t ngood = 0;
    size_t nused;
    double thresh = white + SMF__WHITEN_KNEESNR*sigma;

    /* Initialize ngood -- skip the DC term in the FFT */
    for( i=1; i<(1+box); i++ ) {
      if( pspec[i] > thresh ) {
        ngood++;
      }
    }

    /* Continuing from element 2 go until we hit the end or we reach
       the threshold number of samples below the threshold SNR above
       the white noise level. */

    for( i=2; (i<(nf-(box-1))) && (ngood >= SMF__WHITEN_KNEETHRESH*box); i++ ) {
      /* If the previous first element was good remove it */
      if( pspec[i-1] >= thresh ) {
        ngood--;
      }

      /* If the new last element is good add it */
      if( pspec[i+box-1] >= thresh ) {
        ngood++;
      }
    }

    /* We will fit the power-law from elements 1 to nfit-1. We then
       evaluate the fitted power law as

                       y = A * x^B

       where x is the index in the frequency array
             A = exp( fit[0] )
             B = fit[1]                                           */

    nfit = i+box/2;

    /* If we've entered the white-noise band in order to fit the 1/f
       noise give up */
    if( i >= i_wlo ) {
      goto CLEANUP;
    }

    /* Now fit a straight line to the log of the two axes */
    x = astMalloc( (nfit-1)*sizeof(*x) );
    y = astMalloc( (nfit-1)*sizeof(*y) );

    if( *status == SAI__OK ) {
      for( i=0; i<(nfit-1); i++ ) {
        x[i] = log(i+1);
        y[i] = log(pspec[i+1]);
      }
    }

    smf_fit_poly1d( 1, nfit-1, 0, x, y, NULL, NULL, fit, NULL, NULL, &nused,
                    status );

    if( *status == SAI__OK ) {
      double amp;
      double thescale = 1.;

      A = (exp(fit[0]));
      B = fit[1];

      /* if B isn't negative the fit might be garbage, or else there just
         isn't very much low frequency noise. In this case bail out. */
      if( B >= 0 ) {
        goto CLEANUP;
      }

      /* When we divide the power spectrum by the whitening filter we
         want it to result in the white noise level, so we divide A by
         white to get the right final amplitude. Also, we are going to
         whiten the FFT, not its power spectrum, so we also take the
         square root of the fitting function */

      A = sqrt(A / white);
      B = B / 2.;

      /* Now we apply the whitening, divided both the real and imaginary
         parts by (1 + A * x^B) (and we explicitly set the 0th element to 0).
         Also apply the scale factor here if it was requested */

      re[0] = 0;
      im[0] = 0;

      if( scale ) thescale = scale;

      if( complement ) {
        /* Applying the complement of the whitening filter */
        for( i=1; i<nf; i++ ) {
          amp = thescale * ( 1. - 1. / (1. + A * pow( (double) i, B )) );
          re[i] *= amp;
          im[i] *= amp;
        }
      } else {
        /* Applying the whitening filter */
        for( i=1; i<nf; i++ ) {
          amp = thescale / (1. + A * pow( (double) i, B ));
          re[i] *= amp;
          im[i] *= amp;
        }
      }
    }
  }

  /* Clean up */
 CLEANUP:
  pspec = astFree( pspec );
  qual = astFree( qual );
  x = astFree( x );
  y = astFree( y );
}
