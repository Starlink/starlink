/*
*+
*  Name:
*     smf_fit_pspec

*  Purpose:
*     Low-level routine to fit white noise + 1/f component to power spectrum

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_fit_pspec( const double *pspec, dim_t nf, dim_t box, double df,
*                    double minfreq, double whitefreq, double maxfreq,
*                    double *a, double *b, double *w, int *status );

*  Arguments:
*     pspec = const double * (Given)
*        Power spectral data to fit (assumed to run from 0 to Nyquist,
*        with length nf)
*     nf = dim_t (Given)
*        number of discrete frequencies in pspec
*     box = dim_t (Given)
*        rolling box size for identifying 1/f region
*     df = double (Given)
*        Frequency step size in pspec
*     minfreq = double (Given)
*        Lowest frequency in pspec to fit (can be 0, but will internally be
*        set to first non-zero frequency). Same units as df.
*     whitefreq = double (Given)
*        Frequency above which white noise level will be fit. Same units as df.
*     maxfreq = double (Given)
*        Maximum frequency in pspec to fit. If 0, defaults to
*        nf*df. Same units as df.
*     a = double * (Returned)
*        The amplitude of the 1/f component. Can be NULL.
*     b = double * (Returned)
*        The exponent of the 1/f component. Can be NULL.
*     w = double * (Returned)
*        The white noise level. Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:

*  Description:
*     The purpose of this function is to fit a model to the power spectrum
*     of a bolometer including a 1/f component at low-frequencies transitioning
*     to white component at high-frequencies:
*
*                             P(f) = a * f^b + w
*
*     The white-noise level is established from the mean at
*     frequencies from whitefreq to maxfreq. f itself is calculated as
*     the index of pspec * df. The 1/f component is fit from minfreq
*     to an upper frequency that is dynamically selected to include
*     data that are mostly above W, and at frequencies <= whitefreq.
*
*     If an increasing power spectrum is measured at low frequencies,
*     or if the automatic high-frequency cutoff for the 1/f fitting
*     drifts above the lower-edge of the specified white-noise region,
*     the fit will fail. In any case where the fit fails, a, b, and w
*     will be set to VAL__BADD and status will be set to SMF__BADFIT.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2011-10-05 (EC):
*        Initial version factored out of smf_whiten
*     2011-10-11 (EC):
*        Set status to SMF__BADFIT if fitting fails
*     {enter_further_changes_here}

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

/* System includes */
#include <stdlib.h>
#include <stdint.h>
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

#define SMF__FPSPEC_WHITESNR 5        /* n-sigma clip for white noise level */
#define SMF__FPSPEC_KNEESNR 5         /* SNR threshold for finding 1/f knee */
#define SMF__FPSPEC_KNEETHRESH 0.25   /* fraction of samples below kneesnr */

#define FUNC_NAME "smf_fit_pspec"


void smf_fit_pspec( const double *pspec, dim_t nf, dim_t box, double df,
                    double minfreq, double whitefreq, double maxfreq,
                    double *a, double *b, double *w, int *status ) {

  double A=VAL__BADD;    /* Amplitude of 1/f component */
  double B=VAL__BADD;    /* Exponent of 1/f component */
  int converged;         /* Has white noise level calc converged? */
  double fit[2];         /* fit coefficients */
  dim_t i;              /* Loop counter */
  dim_t i_flo;          /* Index of lowest frequency for calculating 1/f  */
  dim_t i_whi;          /* Index of high-freq. edge for white-noise level */
  dim_t i_wlo;          /* Index of low-freq. edge for white-noise level */
  dim_t nbad;           /* Number of outliers in white noise calc */
  smf_qual_t *qual=NULL; /* Quality to go with pspec */
  double sigma;          /* Uncertainty in white noise level */
  dim_t thisnbad;       /* Outliers in white noise calc, this iteration */
  double white=VAL__BADD;/* White noise level */
  double *x=NULL;        /* Independent axis for 1/f fit */
  double *y=NULL;        /* Dependent axis for 1/f fit */

  if (*status != SAI__OK) return;

  if( (df<=0) || (nf<1) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": invalid frequency information supplied.", status );
    return;
  }

  /* Convert frequencies to array indices (but only consider above DC) */
  i_flo = smf_get_findex( minfreq, df, nf, status );
  i_wlo = smf_get_findex( whitefreq, df, nf, status );

  if( !maxfreq ) {
    /* If maxfreq is 0 set to Nyquist */
    i_whi = nf-1;
  } else {
    i_whi = smf_get_findex( maxfreq, df, nf, status );
  }

  if( i_flo+box*2 > nf ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": box too large compared to number of frequencies",
            status );
    return;
  }

  /* Create a local quality array to go with pspec */
  qual = astCalloc( nf, sizeof(*qual) );

  if( i_flo == 0 ) i_flo = 1;

  if( (*status==SAI__OK) && ((i_flo > i_wlo) || (i_wlo > i_whi)) ) {
    *status = SAI__ERROR;
    errRepf( "", FUNC_NAME
            ": must have i_maxfreq (%zu) > i_whitefreq (%zu) > "
             "i_minfreq (%zu)", status, i_whi, i_wlo, i_flo );
  }

  /* Calculate the white noise level */
  converged = 0;
  nbad = 0;

  while( (!converged) && (*status==SAI__OK) ) {
    double thresh;

    smf_stats1D( pspec+i_wlo, 1, i_whi-i_wlo+1, qual, 1, SMF__Q_SPIKE, &white,
                 &sigma, NULL, NULL, status );

    if( *status==SAI__OK ) {

      thresh = white + SMF__FPSPEC_WHITESNR*sigma;
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
    dim_t nfit;
    dim_t ngood = 0;
    int64_t nused;
    double thresh = white + SMF__FPSPEC_KNEESNR*sigma;

    /* Initialize ngood -- skip the DC term in the FFT */
    for( i=i_flo; i<(i_flo+box); i++ ) {
      if( pspec[i] > thresh ) {
        ngood++;
      }
    }

    /* Continuing from next element, and go until we hit the end or we reach
       the threshold number of samples below the threshold SNR above
       the white noise level. */

    for( i=i_flo+1; (i<(nf-(box-1))) && (ngood >= SMF__FPSPEC_KNEETHRESH*box);
         i++ ) {
      /* If the previous first element was good remove it */
      if( pspec[i-1] >= thresh ) {
        ngood--;
      }

      /* If the new last element is good add it */
      if( pspec[i+box-1] >= thresh ) {
        ngood++;
      }
    }

    /* We will fit the power-law from elements i_flo to nfit-1. We then
       evaluate the fitted power law as

                       y = A * x^B

       where x is the index in the frequency array
             A = exp( fit[0] )
             B = fit[1]                                           */

    nfit = i+box/2-i_flo;

    msgOutiff( MSG__DEBUG, "", FUNC_NAME
               ": i_flow=%zu nfit=%zu i_wlo=%zu i_whi=%zu\n", status,
               i_flo, nfit-1, i_wlo, i_whi);


    /* If we've entered the white-noise band in order to fit the 1/f
       noise give up */
    if( i >= i_wlo ) {
      *status = SMF__BADFIT;
      errRep( "", FUNC_NAME
              ": unable to fit 1/f spectrum with provided frequency ranges",
              status);
      goto CLEANUP;
    }

    /* Now fit a straight line to the log of the two axes */
    x = astMalloc( (nfit-1)*sizeof(*x) );
    y = astMalloc( (nfit-1)*sizeof(*y) );

    if( *status == SAI__OK ) {
      for( i=0; i<(nfit-1); i++ ) {
        x[i] = log((i+i_flo)*df);
        y[i] = log(pspec[i+i_flo]);
      }
    }

    smf_fit_poly1d( 1, nfit-1, 0, 0, x, y, NULL, NULL, fit, NULL, NULL, &nused,
                    status );

    if( *status == SAI__OK ) {

      /* if the exponent is positive the fit is either garbage, or
         else there just isn't very much low frequency noise compared
         to the white noise level. Set B to 0 but generate a warning
         message */

      if( fit[1] >= 0 ) {
        *status = SMF__BADFIT;
        errRep( "", FUNC_NAME
                ": fit to 1/f component encountered rising spectrum!",
                status);
        goto CLEANUP;
      } else {
        B = fit[1];
      }

      A = (exp(fit[0]));
    }
  }

  /* Return fit values */
  if( a ) *a = A;
  if( b ) *b = B;
  if( w ) *w = white;

 CLEANUP:

  qual = astFree( qual );
  x = astFree( x );
  y = astFree( y );
}
