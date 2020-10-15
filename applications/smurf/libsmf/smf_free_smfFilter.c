/*
*+
*  Name:
*     smf_free_smfFilter

*  Purpose:
*     Free a smfFilter struct

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     filt = smf_free_smfFilter( smfFilter *filt, int *status );

*  Arguments:
*     filt = smfFilter * (Given)
*        Pointer to smfFilter to be freed
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     The value of filt upon completion (NULL if successful)

*  Description:
*     This function frees resources associated with smfFilter.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-06-05 (EC):
*        Initial version
*     2008-06-12 (EC):
*        Switch to split real/imaginary arrays for smfFilter
*     2008-06-23 (EC):
*        Free WCS
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

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_free_smfFilter"

smfFilter *smf_free_smfFilter( smfFilter *filt, int *status __attribute__((unused)) ) {

  /* Try to free filt even if bad status on entry */

  if( filt ) {
    filt->real = astFree( filt->real );
    filt->imag = astFree( filt->imag );
    if( filt->wcs) filt->wcs = astAnnul( filt->wcs );
    filt = astFree( filt );
  }

  return filt;

}
