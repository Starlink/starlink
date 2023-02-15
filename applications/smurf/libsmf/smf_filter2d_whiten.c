/*
*+
*  Name:
*     smf_filter2d_whiten

*  Purpose:
*     Produce a 2-d map-whitening filter

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_filter2d_whiten( ThrWorkForce *wf, smfFilter *filt, smfData *map,
*                          double minfreq, double maxfreq, dim_t smooth,
*                          int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     filt = smfFilter * (Given and Returned)
*        Pointer to smfFilter to be modified
*     map = smfData * (Given)
*        2-d map in which to measure the whitening filter parameters
*     minfreq = double (Given)
*        Minimum spatial frequency to consider for the fit. Can be 0. (1/arcsec)
*     maxfreq = double (Given)
*        Maximum spatial frequency to consider for the fit. If 0 assumed
*        to be Nyquist frequency. If greater than Nyquist it will automatically
*        be truncated to Nyquist. (1/arcsec)
*     dim_t smooth (Given)
*        If set, instead of using fitted model, smooth
*        azimuthally-averaged angular power spectrum (at resolution of
*        the filter, not the reference map) by box of this size and
*        use that to estimate the whitening filter.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:

*  Description:
*     This function calculates the azimuthally-averaged angular power
*     spectrum, and then fits a white component + 1/f component within
*     the spatial frequency range indicated by minfreq and
*     maxfreq. The complement of this spectrum is then applied to the
*     supplied filter.  The supplied "map" does not need to have the
*     same dimensions as the map to which "filt" will ultimately be
*     applied as it is simply used to fit the coefficients for the
*     model spectrum. An alternative is to set smooth, and then the
*     whitening filter will be taken literally as the complement of
*     the (square root) of the smoothed azimuthally-average power
*     spectrum. Good if the simple model is not a good fit. Note that
*     the normalization in this case is still taken as the white noise
*     level from the fitted model.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2011-10-04 (EC):
*        Initial version
*     2012-01-04 (EC):
*        Add "smooth" option
*     2012-01-20 (EC):
*        First place pspec in high-res array, then do the smooth
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011-2012 University of British Columbia.
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

#define FUNC_NAME "smf_filter2d_whiten"

void smf_filter2d_whiten( ThrWorkForce *wf, smfFilter *filt, smfData *map,
                          double minfreq, double maxfreq, dim_t smooth,
                          int *status ) {

  double A;                     /* Amplitude 1/f component */
  double B;                     /* exponent of 1/f component */
  double df;                    /* Frequency spacing in ref pspec */
  dim_t i;                      /* Loop counter */
  dim_t j;                      /* Loop counter */
  int ndims=0;                  /* Number of real-space dimensions */
  dim_t nf=0;                   /* Number of frequencies in ref pspec */
  smfData *map_fft=NULL;        /* FFT of the map */
  smfData *pspec=NULL;          /* Az-averaged PSPEC of map */
  double *pspec_data=NULL;      /* Pointer to DATA comp. of pspec */
  double *smoothed_filter=NULL; /* Smoothed power spectrum */
  double W;                     /* White noise level */

  if( *status != SAI__OK ) return;

  if( !filt ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL smfFilter supplied.", status );
    return;
  }

  if( filt->ndims != 2 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": supplied filter is not for 2-d map.",
            status );
    return;
  }

  if( smf_isfft( map, NULL, NULL, NULL, NULL, &ndims, status ) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": supplied map is FFT instead of real space!",
            status );
    return;
  }

  /* Check for reasonable frequencies -- noting that maxfreq=0 defaults to
     nyquist. */
  if( maxfreq && (maxfreq < minfreq) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": maxfreq < minfreq!", status );
    return;
  }

  /* Calculate azimuthally-averaged angular power spectrum */
  map_fft = smf_fft_data( wf, map, NULL, 0, 0, status );
  smf_fft_cart2pol( wf, map_fft, 0, 1, status );
  pspec = smf_fft_2dazav( map_fft, &df, status );
  if( *status == SAI__OK ) {
    nf = pspec->dims[0];
    pspec_data = pspec->pntr[0];
  }
  smf_close_file( wf, &map_fft, status );

  /* Fit power-law + white level model */
  smf_fit_pspec( pspec_data, pspec->dims[0], 10, df, minfreq, 0.1,
                 maxfreq, &A, &B, &W, status );

  msgOutiff( MSG__DEBUG, "", FUNC_NAME
             ": P(f) = %lg*f^%lg + %lg\n", status, A, B, W );

  if( smooth ) {
    int whichaxis=0; /* index of higher-resolution axis */
    double df_s;     /* Frequency spacing of smoothed_filter */
    double nf_s;     /* Size of the smoothed 1-d filter */

    /* Re-sample the radial power spectrum measured in the map on to
       an array that corresponds to the highest-resolution (longer
       real-space) dimension of the supplied filter */

    if( filt->rdims[1] > filt->rdims[0] ) {
      whichaxis = 1;
    }

    df_s = filt->df[whichaxis];
    nf_s = filt->rdims[whichaxis]/2 + 1;

    smoothed_filter = astMalloc( nf_s*sizeof(*smoothed_filter) );

    if( *status == SAI__OK ) {
      /* Nearest-neighbour... */
      for( i=0; i<nf_s; i++ ) {
        dim_t nearest = round(i * df_s / df );
        if( nearest >= nf ) nearest = nf-1;
        smoothed_filter[i] = pspec_data[nearest];
      }
    }

    /* Smooth pspec to estimate the radial power spectrum, but normalize
       by the white-noise level from the fit to preserve
       normalization -------------------------------------------------------- */

    smf_tophat1D( smoothed_filter, nf_s, smooth, NULL, 0, 0.5, status );

    /* Replace bad values (where not enough values to calculate median) with
       the original values. Then normalize by the white noise level,
       and take inverse square root */
    if( *status == SAI__OK ) {
      for( i=0; i<nf_s; i++ ) {
        if( smoothed_filter[i] != VAL__BADD ) {
          smoothed_filter[i] = 1./sqrt(smoothed_filter[i]/W);
        } else if( i < smooth ) {
          /* Filter to 0 at low frequencies where we can't estimate median */
          smoothed_filter[i] = 0;
        } else {
          /* Close to Nyquist we set the filter gain to 1 so that it doesn't
             do anything */
          smoothed_filter[i] = 1;
        }
      }
    }

    /* Copy radial filter into 2d filter */
    if( *status == SAI__OK ) {
      dim_t d;       /* radial distance (FFT pixels) in smoothed filter */
      double model;   /* the value of the model (smoothed filter) at d */
      double x;       /* x- spatial frequency */
      double y;       /* y- spatial frequency */

      for( i=0; i<filt->fdims[0]; i++ ) {
        x =  FFT_INDEX_TO_FREQ(i,filt->rdims[0]) * filt->df[0];

        for( j=0; j<filt->fdims[1]; j++ ) {
          y =  FFT_INDEX_TO_FREQ(j,filt->rdims[1]) * filt->df[1];
          d = (dim_t) round(sqrt(x*x + y*y)/df_s);

          if( d < nf_s) {
            model = smoothed_filter[d];
          } else {
            model = 1;
          }

          filt->real[i + j*filt->fdims[0]] *= model;
          if( filt->imag ) filt->imag[i + j*filt->fdims[0]] *= model;
        }
      }
    }
  } else {
    /* --- otherwise use the smooth fitted model ---------------------------- */

    if( *status == SAI__OK ) {
      double d;
      double model;
      double x;
      double y;

      /* We fit a model to the power spectrum, but we want to apply its
         complement to the FFT of the data. So we normalize by the
         white-noise level, and take the square root of the model before
         writing its complement to the filter buffer */

      A = sqrt(A / W);
      B = B / 2.;

      for( i=0; i<filt->fdims[0]; i++ ) {
        x =  FFT_INDEX_TO_FREQ(i,filt->rdims[0]) * filt->df[0];

        for( j=0; j<filt->fdims[1]; j++ ) {
          y =  FFT_INDEX_TO_FREQ(j,filt->rdims[1]) * filt->df[1];
          d = sqrt(x*x + y*y);
          model = 1. + A * pow(d,B);

          filt->real[i + j*filt->fdims[0]] /= model;
          if( filt->imag ) filt->imag[i + j*filt->fdims[0]] /= model;
        }
      }
    }
  }

  /* Clean up */
  if( pspec ) smf_close_file( wf, &pspec, status );
  if( smoothed_filter ) smoothed_filter = astFree( smoothed_filter );
}
