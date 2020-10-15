/*
*+
*  Name:
*     smf_fits_getS

*  Purpose:
*     Obtain a character string FITS item value from a header

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_fits_getS( const smfHead * hdr, const char * name, char * result,
*                    dim_t len, int * status );

*  Arguments:
*     hdr = const smfHdr* (Given)
*        Header struct. Assumed to contain a FitsChan in the hdr slot
*     name = const char * (Given)
*        Name of the FITS Item to retrieve.
*     result = char * (Returned)
*        Pointer to string buffer. To guarantee it is large enough
*        to recieve the result, allocate at least 72 characters.
*     len = dim_t (Given)
*        Allocated length of "result" buffer. Including space for NUL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function looks up a FITS header item and stores the result
*     in the string variable provided.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2005-11-08 (TIMJ):
*        Initial version.
*     2005-11-29 (TIMJ):
*        Indicate consting in prolog
*     2006-01-24 (TIMJ):
*        Create string version from Double.
*     2006-01-26 (TIMJ):
*        Fix silly string copy bug.
*     2006-07-31 (TIMJ):
*        Use SMF__NOKWRD error condition.
*     2006-08-02 (TIMJ):
*        astGetFitsS does not trim trailing space.
*     2008-12-17 (TIMJ):
*        use one_strlcpy. Use smf_validate_smfHead.
*     {enter_further_changes_here}

*  Notes:
*     - if the supplied buffer is too small to receive the string, the
*       result will be truncated and status will be set to ONE__TRUNC.
*     - See also smf_fits_getI and smf_fits_getD

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

/* System includes */
#include <string.h>

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"
#include "star/one.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_fits_getS"

void smf_fits_getS (const smfHead *hdr, const char * name, char * result,
		    dim_t len, int * status ) {
  char * astres; /* Pointer to AST static buffer */
  int i;         /* Loop counter */

  /* Set a default value */
  result[0] = '\0';

  /* Check entry status */
  if (*status != SAI__OK) return;
  if (!smf_validate_smfHead(hdr, 1, 0, status)) return;

  if ( !astGetFitsS( hdr->fitshdr, name, &astres) ) {
    if ( *status == SAI__OK) {
      *status = SMF__NOKWRD;
      msgSetc("FITS", name );
      errRep(FUNC_NAME, "Unable to retrieve item ^FITS from header",
	     status);
    }
  }

  /* if status is good, copy the result into the output buffer */
  one_strlcpy( result, astres, len, status );
  if (*status == SAI__OK) {
      /* AST does not seem to trim trailing spaces from FITS cards */
      for (i=(int)strlen(astres); i >= 0; i--) {
        /* if we are not at a space or nul break from loop */
        if ( result[i] != ' ' && result[i] != '\0') {
          break;
        }
        result[i] = '\0';
      }
  }

  return;
}
