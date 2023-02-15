/*
*+
*  Name:
*     smf_filter_delay

*  Purpose:
*     Apply a time-domain delay using a frequency-domain filter

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_filter_delay( smfFilter *filt, double delay, int *status );

*  Arguments:
*     filt = smfFilter * (Given and Returned)
*        Pointer to smfFilter to be modified
*     delay = double (Given)
*        Delay in seconds (can be negative to shift in opposite direction)
*     status = int* (Given and Returned)
*        Pointer to global status

*  Return Value:

*  Description:
*     This filter applies a delay to the time-series. A truncated sinc
*     function kernel is used for interpolation to handle non-integer
*     numbers of samples in the delay.  The delay is periodic (e.g.,
*     data shifted beyond the end of the time-series will appear at
*     the beginning), so delays larger than the length of the entire
*     time-series are meaningless and will generate bad status.

*  Notes:

*  Authors:
*     Edward Chapin
*     {enter_new_authors_here}

*  History:
*     2012-11-14 (EC):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.

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
#include "ast.h"
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "fftw3.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_filter_delay"

void smf_filter_delay( smfFilter *filt, double delay, int *status ) {
  double delay_samp;     /* Delay in samples */
  dim_t i;              /* Loop counter */
  smfData *sfunc=NULL;   /* delayed sinc function resampling kernel */
  smfData *sfunc_ft=NULL;/* FFT of sfunc */
  double tstep;          /* Length of sample in seconds */

  if( *status != SAI__OK ) return;

  if( !delay ) return;

  if( !filt ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "NULL smfFilter supplied.", status );
    return;
  }

  if( filt->ndims != 1 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": function only generates filters for time-series",
            status );
    return;
  }

  if( !filt->fdims[0] ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": 0-length smfFilter supplied.",
            status );
    return;
  }

  /* Calculate delay in terms of number of samples */

  if( !filt->df[0] ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": frequency spacing is 0 in filter?",
            status );
    return;
  }

  if( !filt->rdims[0] ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": real space length is 0 in filter?",
            status );
    return;
  }

  tstep = 1./(filt->rdims[0]*filt->df[0]);
  delay_samp = delay/tstep;

  if( fabs(delay_samp) >= filt->rdims[0] ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": requested delay is longer than the time-series",
            status );
    return;
  }

  /* If filt->real is NULL, create a complex identity filter first. Similarly,
     if the filter is currently only real-valued, add an imaginary part. */

  if( !filt->real ) {
    smf_filter_ident( filt, 1, status );
    if( *status != SAI__OK ) return;
  } else if( !filt->imag ) {
    filt->imag = astCalloc( filt->fdims[0], sizeof(*filt->imag) );
    if( *status != SAI__OK ) return;
    filt->isComplex = 1;
  }

  /* Create time-series of delayed sinc function */
  sfunc = smf_create_smfData( SMF__NOCREATE_DA | SMF__NOCREATE_FILE |
                              SMF__NOCREATE_HEAD,
                              status );

  if( *status == SAI__OK ) {
    sfunc->dtype = SMF__DOUBLE;
    sfunc->ndims = 3;
    sfunc->isTordered = 1;
    sfunc->dims[0] = 1;
    sfunc->dims[1] = 1;
    sfunc->dims[2] = filt->rdims[0];
    sfunc->isFFT = -1;
  }

  sfunc->pntr[0] = astCalloc( filt->rdims[0],
                              smf_dtype_sz(sfunc->dtype,status) );

  if( *status == SAI__OK ) {
    for( i=0; i<filt->rdims[0]; i++ ) {
      double *d=sfunc->pntr[0];
      double delay_wrapped;
      double dist;
      double N;

      N = filt->rdims[0];

      if( delay_samp < 0 ) {
        delay_wrapped = delay_samp + N;
      } else {
        delay_wrapped = delay_samp;
      }

      dist = fabs((double) i - delay_wrapped);

      if( dist > N/2 ) {
        dist = N - dist;
      }

      d[i] = sin(AST__DPI * dist) / ( AST__DPI * dist );
    }
  }

  /* Take the FFT to determine the Fourier space coefficients */

  sfunc_ft = smf_fft_data( NULL, sfunc, NULL, 0, 0, status );

  /* Multiply existing filter by new filter coefficients */

  if( *status == SAI__OK ) {
    double *re=NULL;
    double *im=NULL;

    /* Pointers to real and imaginary parts of transformed sinc function */
    re = sfunc_ft->pntr[0];
    im = re + filt->fdims[0];

    for( i=0; i<filt->fdims[0]; i++ ) {
      double ac, bd, aPb, cPd;

      ac = filt->real[i] * re[i];
      bd = filt->imag[i] * im[i];

      aPb = filt->real[i] + filt->imag[i];
      cPd = re[i] + im[i];

      filt->real[i] = ac - bd;
      filt->imag[i] = aPb*cPd - ac -bd;
    }
  }

  /* Clean up */

  smf_close_file( NULL, &sfunc, status );
  smf_close_file( NULL, &sfunc_ft, status );
}
