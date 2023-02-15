/*
*+
*  Name:
*     smf_filter_edge

*  Purpose:
*     Apply a hard or soft low- or high-pass edge to a 1-d smfFilter at
*     given frequency

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_filter_edge( smfFilter *filt, double f, int order, int lowpass,
*                      int *status );

*  Arguments:
*     filt = smfFilter * (Given and Returned)
*        Pointer to smfFilter to be modified
*     f = double (Given)
*        Frequency of the edge in Hz
*     order = int (Given)
*        If zero or negative, a hard-edged filter is used. Otherwise,
*        a Butterworth filter is used with the specified order. Higher
*        order produce sharper cut-offs with more ringing.
*     highpass = int (Given)
*        Set flag if edge is low-pass. Otherwise high-pass is assumed.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function applies a hard or soft, low- or high-pass cut at the
*     specified edge frequency. For a hard-edged filter, the power at all
*     frequencies below (or above) the edge are set to zero if highpass is
*     0 (or non-zero). For a soft-edge filter, a Butterworth filter is used.
*     The conversion of the frequency to a discrete bin in the filter is
*     accomplished by rounding. For a hard-edge, the range of frequencies set
*     to 0 include the bin corresponding to the edge frequency. This function
*     will operate on either real or complex-valued filters, and will not
*     alter the data type. Bad status is set if filt is NULL, or the filter
*     is 0 length. If filt->buf is NULL it is first initialized by calling
*     smf_filter_ident
*     (type=real).

*  Authors:
*     EC: Ed Chapin (UBC)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-06-11 (EC):
*        Initial version
*     2008-06-12 (EC):
*        -Switch to split real/imaginary arrays for smfFilter
*     2013-02-18 (DSB):
*        Allow soft-edged filters to be used.
*     2013-10-21 (DSB):
*        Changed soft-edged filters to use a standard zero-order
*        Butterworth filter.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.
*     Copyright (C) 2013 Science & Technology Facilities Council.
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

#define FUNC_NAME "smf_filter_edge"

void smf_filter_edge( smfFilter *filt, double f, int order, int lowpass,
                      int *status ) {
  double rat;           /* Ratio of channel freq to edge freq */
  dim_t base;          /* Index to start of memory to be zero'd */
  dim_t i;             /* Channel index */
  dim_t iedge;         /* Index corresponding to the edge frequency */
  dim_t len;           /* Length of memory to be zero'd */

  if( *status != SAI__OK ) return;

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

  /* If filt->real is NULL, create a real identity filter first */
  if( !filt->real ) {
    smf_filter_ident( filt, 0, status );
    if( *status != SAI__OK ) return;
  }

  /* Calculate offset of edge frequency in filter */
  iedge = smf_get_findex( f, filt->df[0], filt->fdims[0], status );

  /* Hard-edged... */
  if( order <= 0 ) {

    /* Since we're zero'ing a continuous piece of memory, just use memset */

    if( lowpass ) { /* Zero frequencies beyond edge */
      base = iedge;
      len = filt->fdims[0] - iedge;
    } else {        /* Zero frequencies from 0 to edge */
      base = 0;
      len = iedge + 1;
    }

    memset( ((unsigned char *) filt->real) + base*sizeof(*filt->real), 0,
            len*sizeof(*filt->real) );
    if( filt->isComplex ) {
      memset( ((unsigned char *) filt->imag) + base*sizeof(*filt->imag), 0,
              len*sizeof(*filt->imag) );
    }

  /* Soft-edged */
  } else {

    if( lowpass ) {
      for( i = 0; i < filt->fdims[0]; i++ ) {
        rat = (double) i / (double) iedge;
        rat *= rat;
        if( order > 1 ) rat = pow( rat, order );
        (filt->real)[ i ] = 1.0/( 1.0 + rat );
      }

    } else {
      for( i = 0; i < filt->fdims[0]; i++ ) {
        rat = (double) i / (double) iedge;
        rat *= rat;
        if( order > 1 ) rat = pow( rat, order );
        (filt->real)[ i ] = rat/( 1.0 + rat );
      }
    }

    /* For complex filters, copy the real part into the imaginary part. */
    if( filt->isComplex ) {
      memcpy( filt->imag, filt->real, filt->fdims[0]*sizeof(*filt->imag) );
    }
  }
}
