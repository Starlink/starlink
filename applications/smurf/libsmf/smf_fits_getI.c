/*
*+
*  Name:
*     smf_fits_getI

*  Purpose:
*     Obtain an integer FITS item value from a header

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_fits_getI( const smfHead * hdr, const char * name, int * result,
*                    int * status );

*  Arguments:
*     hdr = const smfHdr* (Given)
*        Header struct. Assumed to contain a FitsChan in the hdr slot
*     name = const char * (Given)
*        Name of the FITS Item to retrieve.
*     result = int * (Returned)
*        Pointer of integer to store the result.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function looks up a FITS header item and stores the result
*     in the variable provided.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2005-11-07 (TIMJ):
*        Initial version.
*     2005-11-29 (TIMJ):
*        Indicate consting in prolog.
*     2006-07-31 (TIMJ):
*        Use SMF__NOKWRD error condition.
*     2008-12-17 (TIMJ):
*        Use smf_validate_smfHead.
*     {enter_further_changes_here}

*  Notes:
*     - See also smf_fits_getD and smf_fits_getS

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
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
#include "ast.h"
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_fits_getI"

void smf_fits_getI (const smfHead *hdr, const char * name, int * result, int * status ) {

  if (*status != SAI__OK) return;
  if (!smf_validate_smfHead(hdr, 1, 0, status)) return;

  if ( !astGetFitsI( hdr->fitshdr, name, result) ) {
    if ( *status == SAI__OK) {
      *status = SMF__NOKWRD;
      msgSetc("FITS", name );
      errRep(FUNC_NAME, "Unable to retrieve item ^FITS from header",
	     status);
    }
  }

  return;
}
