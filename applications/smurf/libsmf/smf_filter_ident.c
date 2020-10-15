/*
*+
*  Name:
*     smf_filter_ident

*  Purpose:
*     Set a smfFilter to the real or complex identity filter

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_filter_ident( smfFilter *filt, int complex, int *status );

*  Arguments:
*     filt = smfFilter * (Given and Returned)
*        Pointer to smfFilter to be modified
*     complex = int (Given)
*        If set filter is complex-valued. Otherwise real (double).
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:

*  Description:
*     This function sets a filter to the identity. Multiplying the FFT
*     of a bolometer by this filter will have no effect. If the filter already
*     contained data it will be re-set, and the data type changed if needed.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-06-05 (EC):
*        Initial version
*     2008-06-10 (EC):
*        Move normalization here from smf_filter_execute
*     2008-06-12 (EC):
*        -Switch to split real/imaginary arrays for smfFilter
*     2011-10-03 (EC):
*        Handle 2-d map filters
*     2011-10-26 (EC):
*        Once again move normalization back to smf_filter_execute for clarity
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008, 2011 University of British Columbia.
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

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "fftw3.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_filter_ident"

void smf_filter_ident( smfFilter *filt, int complex, int *status ) {
  dim_t i;         /* Loop counter */
  dim_t nfdata;   /* Number of frequency-space data values */

  if (*status != SAI__OK) return;

  if( !filt ) {
    *status = SAI__ERROR;
    errRep("",  FUNC_NAME ": NULL smfFilter supplied.", status );
    return;
  }

  if( !filt->rdims[0] || !filt->ndims ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": 0-length smfFilter supplied.", status );
    return;
  }

  /* The filter values are initialized to a real value of 1 */
  nfdata=1;
  for( i=0; i<filt->ndims; i++ ) {
    nfdata *= filt->fdims[i];
  }

  filt->real = astMalloc( nfdata*sizeof(*filt->real) );

  if( *status == SAI__OK ) {
    for( i=0; i<nfdata; i++ ) {
      filt->real[i] = 1;
    }
  }

  if( complex ) {
    filt->imag = astCalloc( nfdata, sizeof(*filt->imag) );
    filt->isComplex = 1;
  }
}
