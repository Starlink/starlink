/*
*+
*  Name:
*     smf_filter2d_edge

*  Purpose:
*     Apply a hard low- a high-pass edge to a 2-d smfFilter

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_filter2d_edge( smfFilter *filt, double f, int lowpass, int *status );

*  Arguments:
*     filt = smfFilter * (Given and Returned)
*        Pointer to 2d smfFilter to be modified
*     f = double (Given)
*        Frequency of the edge in 1/arcsec
*     lowpass = int (Given)
*        Set flag if edge is low-pass. Otherwise high-pass is assumed.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:

*  Description:
*     This function applies a hard low- or high-pass cut at the specified
*     edge frequency. The power at all frequencies below (or above) this
*     edge are set to zero if lowpass is 0 (or non-zero). The conversion
*     of the frequency to a discrete bin in the filter is accomplished by
*     rounding. The range of frequencies set to 0 include the bin corresponding
*     to the edge frequency. This function will operate on either real or
*     complex-valued filters, and will not alter the data type. Bad status is
*     set if filt is NULL, or the filter is 0 length. If filt->buf is NULL it
*     is first initialized by calling smf_filter_ident (type=real).


*  Notes:

*  Authors:
*     EC: Ed Chapin (UBC)
*     DSB: David S Berry (EAO):
*     {enter_new_authors_here}

*  History:
*     2011-10-26 (EC):
*        Initial version based in 1d smf_filter_edge
*     2018-10-04 (DSB):
*        Correct comments and prologue to refer to argument "lowpass"
*        instead of "highpass".
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

#define FUNC_NAME "smf_filter2d_edge"

void smf_filter2d_edge( smfFilter *filt, double f, int lowpass, int *status ) {

  double d;                     /* magnitude spatial frequency */
  dim_t i;                     /* Loop counter */
  dim_t j;                     /* Loop counter */
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

  /* If filt->real is NULL, create a real identity filter first */
  if( !filt->real ) {
    smf_filter_ident( filt, 0, status );
  }

  if( *status != SAI__OK ) return;

  /* Loop over all spatial frequencies in the filter, determine their
     magnitudes, and decide whether to zero each element of the filter
     based on the supplied cutoff */

  for( i=0; i<filt->fdims[0]; i++ ) {
    x =  FFT_INDEX_TO_FREQ(i,filt->rdims[0]) * filt->df[0];

    for( j=0; j<filt->fdims[1]; j++ ) {
      y =  FFT_INDEX_TO_FREQ(j,filt->rdims[1]) * filt->df[1];
      d = sqrt(x*x + y*y);

      if( (lowpass && (d >= f)) || (!lowpass && (d < f)) ) {
        filt->real[i + j*filt->fdims[0]] = 0;
        if( filt->isComplex ) {
          filt->imag[i + j*filt->fdims[0]] = 0;
        }
      }

    }
  }

}
