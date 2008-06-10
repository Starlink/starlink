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
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

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
  dim_t i;                       /* Loop counter */
  fftw_complex *newbuf=NULL;     /* New complex-valued buffer */

  if (*status != SAI__OK) return;

  if( !filt ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "NULL smfFilter supplied.", status );
    return;
  }
  
  if( !filt->buf ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "smfFilter contains a NULL buffer", status );
  }
  
  if( filt->isComplex ) return;

  /* Allocate space for a 0-initialized complex array */
  newbuf = smf_malloc( sizeof(fftw_complex), filt->dim, 1, status );

  if( *status == SAI__OK ) {
    /* Copy over the real part from the old array */
    for( i=0; i<filt->dim; i++ ) {
      newbuf[i][0] = ((double *)filt->buf)[i];
    }

    /* Free the old array, and point the buffer to the new array */
    filt->buf = smf_free( filt->buf, status );

    if( *status == SAI__OK ) {
      filt->buf = newbuf;
      filt->isComplex = 1;
    }   
  }
}
