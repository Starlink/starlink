/*
*+
*  Name:
*     smf_filter_complement

*  Purpose:
*     Change a filter to its complement

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_filter_complement( smfFilter *filt, int *status );

*  Arguments:
*     filt = smfFilter * (Given and Returned)
*        Pointer to smfFilter to be modified
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:

*  Description:
*     This function calculates the complementary filter as
*               new_k = ( 1 - old_k ),
*     where new_k is the new filter value at frequency k, and old_k is
*     the previous value. The data type will be preserved.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-09-23 (EC):
*        Initial version
*     2011-10-03 (EC):
*        Handle 2-d map filters
*     2011-10-26 (EC):
*        smfFilters no longer perform the normalization of the FFT
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

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "fftw3.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_filter_complement"

void smf_filter_complement( smfFilter *filt, int *status ) {
  dim_t i;         /* Loop counter */
  dim_t nfdata;    /* Number of frequency-space data points */
  double ref;       /* Reference value for complement */

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

  /* If this is an un-initialized filter simply return */
  if( !filt->real ) return;

  /* The reference value for the complement is 1 */
  nfdata=1;
  for( i=0; i<filt->ndims; i++ ) {
    nfdata *= filt->fdims[i];
  }

  ref = 1.;

  for( i=0; i<nfdata; i++ ) {
    filt->real[i] = ref - filt->real[i];
  }

  if( filt->isComplex ) {
    for( i=0; i<nfdata; i++ ) {
      filt->imag[i] = -filt->imag[i];
    }
  }
}
