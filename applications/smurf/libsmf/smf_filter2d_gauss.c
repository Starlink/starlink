/*
*+
*  Name:
*     smf_filter2d_gauss

*  Purpose:
*     Apply a Gaussian to a 2-d smfFilter

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_filter2d_gauss( smfFilter *filt, double fwhm, int *status );

*  Arguments:
*     filt = smfFilter * (Given and Returned)
*        Pointer to 2d smfFilter to be modified
*     fwhm = double (Given)
*        Full-width at half-maximum in real-space of the smoothing kernel
*        (arcsec)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:

*  Description:
*     This function applies an azimuthally symmetric Gaussian to a 2d
*     filter.  Since the FFT of a Gaussian is itself a Gaussian we ask
*     for the scale of the filter in the simpler units of a real-space
*     Gaussian (fwhm). In frequency space the FWHM is simply 1/fwhm
*     (arcsec^-1). The Gaussian is peak-normalized.  This function
*     will operate on either real or complex-valued filters, and will
*     not alter the data type. Bad status is set if filt is NULL, or
*     the filter is 0 length. If filt->buf is NULL it is first
*     initialized by calling smf_filter_ident (type=real).

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2011-10-28 (EC):
*        Initial version based on smf_filter2d_edge.c
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

#define FUNC_NAME "smf_filter2d_gauss"

void smf_filter2d_gauss( smfFilter *filt, double fwhm, int *status ) {

  double dsq;                   /* magnitude^2 spatial frequency */
  double fwhm_f;                /* fwhm in frequency space */
  double g;                     /* Gaussian evaluated at d */
  dim_t i;                     /* Loop counter */
  dim_t j;                     /* Loop counter */
  double sig_f;                 /* sigma in frequency space */
  double sig_f_sq;              /* sig_f squared */
  double x;                     /* x-component spatial frequency */
  double y;                     /* y-component spatial frequency */

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

  if( fwhm < 0 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": fwhm must be >= 0", status );
    return;
  }

  /* If filt->real is NULL, create a real identity filter first */
  if( !filt->real ) {
    smf_filter_ident( filt, 0, status );
  }

  if( *status != SAI__OK ) return;

  /* Loop over all spatial frequencies in the filter, determine their
     magnitudes in arcsec, and calculate/apply the amplitude of the
     Gaussian */

  fwhm_f = 1/fwhm;
  sig_f = fwhm_f/2.3548200;
  sig_f_sq  = sig_f * sig_f;

  for( i=0; i<filt->fdims[0]; i++ ) {
    x =  FFT_INDEX_TO_FREQ(i,filt->rdims[0]) * filt->df[0];

    for( j=0; j<filt->fdims[1]; j++ ) {
      y =  FFT_INDEX_TO_FREQ(j,filt->rdims[1]) * filt->df[1];
      dsq = x*x + y*y;

      g = exp( -dsq/(2.*sig_f_sq) );

      filt->real[i + j*filt->fdims[0]] *= g;
      if( filt->isComplex ) {
        filt->imag[i + j*filt->fdims[0]] *= g;
      }

    }
  }

}
