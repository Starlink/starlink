/*
*+
*  Name:
*     smf_getfitsi

*  Purpose:
*     Obtain an integer FITS keyword value from a header, handling undef values.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_getfitsi( const smfHead *hdr, const char *name, int *result,
*                   int *status );

*  Arguments:
*     hdr = const smfHdr * (Given)
*        Header struct. An error is reported if it does not contain a
*        FitsChan in the hdr slot
*     name = const char * (Given)
*        Name of the FITS keyword to retrieve.
*     result = int * (Given and Returned)
*        Pointer to a int in which to store the result. The supplied
*        int value is left unchanged if the keyword has an undefined
*        value in the FitsChan.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     This function searches the supplied header for a named FITS keyword.
*     If the keyword is not found, an error is reported. If the keyword
*     is found, but has no associated value, the contents of the supplied
*     result buffer is left unchanged, but no error is reported. If the
*     keyword has a defined value, it is returned in the buffer.
*
*     This function is similar to smf_fits_getI except that it leaves the
*     returned buffer unchanged if the keyword has an undefined value.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     2-DEC-2008 (DSB):
*        Initial version, based on smf_get_fitsD by TIMJ.
*     17-DEC-2008 (TIMJ):
*        Use smf_validate_smfHead.
*     25-MAY-2009 (TIMJ):
*        Copy from smf_getfitsd
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008, 2009 Science & Technology Facilities Council.
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
#define FUNC_NAME "smf_getfitsi"

void smf_getfitsi( const smfHead *hdr, const char *name, int *result,
                   int *status ) {

/* Local Variables; */
   int there;       /* Was the keyword found in the FitsChan? */

/* Check the inherited status */
   if (*status != SAI__OK) return;
   if (!smf_validate_smfHead(hdr, 1, 0, status)) return;

/* If the header contains a defined value for the keyword, put it into
   the returned results buffer. */
   if( astTestFits( hdr->fitshdr, name, &there ) ) {
      (void) astGetFitsI( hdr->fitshdr, name, result );

/* Otherwise, if the keyword was not found, report an error. */
   } else if( !there && *status == SAI__OK ) {
      *status = SMF__NOKWRD;
      msgSetc( "FITS", name );
      errRep( FUNC_NAME, "Unable to retrieve item ^FITS from header",
	     status);
   }
}
