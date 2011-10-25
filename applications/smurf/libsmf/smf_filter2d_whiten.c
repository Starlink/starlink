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
*                          double minfreq, double maxfreq, int *status );

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
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:

*  Description:
*     This function calculates the azimuthally-averaged angular power
*     spectrum, and then fits a white component + 1/f component within
*     the spatial frequency range indicated by minfreq and maxfreq. The
*     complement of this spectrum is then applied to the supplied filter.
*     The supplied "map" does not need to have the same dimensions as the
*     map to which "filt" will ultimately be applied as it is simply used
*     to fit the coefficients for the model spectrum.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2011-10-04 (EC):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011University of British Columbia.
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

#define FUNC_NAME "smf_filter2d_whiten"

void smf_filter2d_whiten( ThrWorkForce *wf, smfFilter *filt, smfData *map,
                          double minfreq, double maxfreq, int *status ) {

  double A;                     /* Amplitude 1/f component */
  double B;                     /* exponent of 1/f component */
  double df;                    /* Frequency spacing */
  size_t i;                     /* Loop counter */
  size_t j;                     /* Loop counter */
  size_t ndims=0;               /* Number of real-space dimensions */
  smfData *map_fft=NULL;        /* FFT of the map */
  smfData *pspec=NULL;          /* Az-averaged PSPEC of map */
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

  /* Check for reasonable frequencies */
  if( maxfreq < minfreq ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": maxfreq < minfreq!", status );
    return;
  }

  /* Calculate azimuthally-averaged angular power spectrum */
  map_fft = smf_fft_data( wf, map, NULL, 0, 0, status );
  smf_fft_cart2pol( map_fft, 0, 1, status );
  pspec = smf_fft_2dazav( map_fft, &df, status );
  smf_close_file( &map_fft, status );

  /* Fit model */
  smf_fit_pspec( pspec->pntr[0], pspec->dims[0], 10, df, minfreq, 0.1, maxfreq,
                 &A, &B, &W, status );

  msgOutiff( MSG__DEBUG, "", FUNC_NAME
             ": P(f) = %lg*f^%lg + %lg\n", status, A, B, W );

  /* Apply complement of model to the supplied filter */
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
