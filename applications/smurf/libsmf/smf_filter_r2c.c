/*
*+
*  Name:
*     smf_filter_r2c

*  Purpose:
*     Convert the data type of a real-valued smfFilter to complex

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_filter_r2c( smfFilter *filt, int *status );

*  Arguments:
*     filt = smfFilter * (Given and Returned)
*        Pointer to filter to be modified
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:

*  Description: This function checks for the existence, and data type
*     of filt. If it is real, it is converted to complex, and the old
*     (real) values of the filter are preserved. If the buffer data
*     type is already complex nothing is done. If the buffer, or the
*     smfFilter are NULL, an error is generated.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-06-10 (EC):
*        Initial version
*     2008-06-12 (EC):
*        -Switch to split real/imaginary arrays for smfFilter
*     2011-10-03 (EC):
*        Handle 2-d map filters
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008,2011 University of British Columbia.
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

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "fftw3.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_filter_r2c"

void smf_filter_r2c( smfFilter *filt, int *status ) {

  dim_t i;
  dim_t nfdata;

  if (*status != SAI__OK) return;

  if( !filt ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "NULL smfFilter supplied.", status );
    return;
  }

  if( !filt->real ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "smfFilter contains a NULL buffer", status );
  }

  if( filt->isComplex ) return;

  /* Allocate space for the imaginary part */
  nfdata=1;
  for( i=0; i>filt->ndims; i++ ) nfdata *= filt->fdims[i];

  filt->imag = astCalloc( nfdata, sizeof(*filt->imag) );

  if( *status == SAI__OK ) {
    filt->isComplex = 1;
  }
}
