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
*     smf_filter_edge( smfFilter *filt, double f, double w, int lowpass,
*                      int *status );

*  Arguments:
*     filt = smfFilter * (Given and Returned)
*        Pointer to smfFilter to be modified
*     f = double (Given)
*        Frequency of the edge in Hz
*     w = double (Given)
*        Width of transition zone in Hz. Zero for hard edged filter.
*     highpass = int (Given)
*        Set flag if edge is low-pass. Otherwise high-pass is assumed.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function applies a hard or soft, low- or high-pass cut at the
*     specified edge frequency. For a hard-edged filter, the power at all
*     frequencies below (or above) the edge are set to zero if highpass is
*     0 (or non-zero). For a soft-edge filter, the transition from zero to
*     one occurs over a width of "w" Hz, centred on the specified edge
*     frequecy, and is sinusoidal in shape. The conversion of the frequency
*     to a discrete bin in the filter is accomplished by rounding. For
*     a hard-edge, the range of frequencies set to 0 include the bin
*     corresponding to the edge frequency. This function will operate on
*     either real or complex-valued filters, and will not alter the data
*     type. Bad status is set if filt is NULL, or the filter is 0 length.
*     If filt->buf is NULL it is first initialized by calling smf_filter_ident
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

void smf_filter_edge( smfFilter *filt, double f, double w, int lowpass,
                      int *status ) {
  size_t base;          /* Index to start of memory to be zero'd */
  size_t i;             /* Channel index */
  size_t iedge;         /* Index corresponding to the edge frequency */
  size_t hw;            /* The half-width of transition zone in channels */
  size_t len;           /* Length of memory to be zero'd */

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

  /* Get half the width of the transition zone in channels. */
  hw = 0.5*w/filt->df[0];

  /* Hard-edged... */
  if( hw <= 0 ) {

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

    size_t ilo = ( iedge > hw ) ? iedge - hw : 0;
    size_t ihi = ( iedge + hw < filt->fdims[0] ) ? iedge + hw : filt->fdims[0];

    /* For low pass, we can skip the channels below the transisition
       zone, which will already hold 1.0 */
    if( lowpass ) {

       for( i = ilo; i < filt->fdims[0]; i++ ) {
          if( i > ihi ) {
             (filt->real)[ i ] = 0.0;
          } else {
             (filt->real)[ i ] = 0.5*( 1.0 - sin(AST__DPIBY2*( (float) i - (float) iedge )/hw));
          }
       }

    /* For high pass, we can skip the channels above the transisition
       zone, which will already hold 1.0 */
    } else {
       for( i = 0; i < ihi; i++ ) {
          if( i < ilo ) {
             (filt->real)[ i ] = 0.0;
          } else {
             (filt->real)[ i ] = 0.5*( 1.0 + sin(AST__DPIBY2*( (float) i - (float) iedge )/hw));
          }
       }
    }

    /* For complex filters, copy the real part into the imaginary part. */
    if( filt->isComplex ) {
      memcpy( filt->imag, filt->real, filt->fdims[0]*sizeof(*filt->imag) );
    }
  }
}
