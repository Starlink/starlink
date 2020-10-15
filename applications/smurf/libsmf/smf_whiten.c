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
*     smf_whiten( double *re, double *im, double df, dim_t nf, dim_t box,
*                 int complement, int *status );

*  Arguments:
*     re = double * (Given and Returned)
*        Real part of the FFT.
*     im = double * (Given and Returned)
*        Imaginary part of the FFT.
*     df = double (Given)
*        Frequency step size in FFT
*     nf = dim_t (Given)
*        number of frequencies in the FFT
*     box = dim_t (Given)
*        rolling box size for identifying 1/f region
*     complement = int (Given)
*        If set, apply the complement of the whitening filter.
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
*     2011-10-06 (EC):
*        Moved power spectrum fitting into smf_fit_pspec
*     2011-10-26 (EC):
*        Remove scale from interface as it is no longer needed
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

#define FUNC_NAME "smf_whiten"

void smf_whiten( double *re, double *im, double df, dim_t nf, dim_t box,
                 int complement, int *status ) {

  double A;              /* Amplitude of 1/f component */
  double B;              /* Exponent of 1/f component */
  dim_t i;              /* Loop counter */
  double *pspec=NULL;    /* Power spectrum of the bolometer */
  double white;          /* White noise level */

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

  /* Calculate the power spectrum and fit a model to it. Currently have
     the white-noise measurement band hard-wired to 5--20 Hz */

  pspec = astMalloc( nf*sizeof(*pspec) );

  if( *status == SAI__OK ) {
    for( i=0; i<nf; i++ ) {
      pspec[i] = re[i]*re[i] + im[i]*im[i];
    }
  }

  smf_fit_pspec( pspec, nf, box, df, 0, 5, 20, &A, &B, &white, status );

  /* If the fit failed all we will do is multiply by 1 */

  if( *status == SMF__BADFIT ) {
    A = 1;
    B = 0;
    white = 1;
    errAnnul( status );
  }

  if( *status == SAI__OK ) {
    double amp;

    /* When we divide the power spectrum by the whitening filter we
       want it to result in the white noise level, so we divide A by
       white to get the right final amplitude. Also, we are going to
       whiten the FFT, not its power spectrum, so we also take the
       square root of the fitting function */

    A = sqrt(A / white);
    B = B / 2.;

    /* Now we apply the whitening, divided both the real and imaginary
       parts by (1 + A * x^B) (and we explicitly set the 0th element
       to 0). */

    re[0] = 0;
    im[0] = 0;

    if( complement ) {
      /* Applying the complement of the whitening filter */
      for( i=1; i<nf; i++ ) {
        amp = 1. - 1. / (1. + A * pow( (double) i*df, B ));
        re[i] *= amp;
        im[i] *= amp;
      }
    } else {
      /* Applying the whitening filter */
      for( i=1; i<nf; i++ ) {
        amp = 1. / (1. + A * pow( (double) i*df, B ));
        re[i] *= amp;
        im[i] *= amp;
      }
    }
  }

  /* Clean up */
  pspec = astFree( pspec );
}
