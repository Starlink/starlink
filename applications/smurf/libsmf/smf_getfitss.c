/*
*+
*  Name:
*     smf_fits_getS

*  Purpose:
*     Obtain a character string FITS item value from a header, handling undef

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_getfitss( const smfHead * hdr, const char * name, char * result,
*                    dim_t len, int * status );

*  Arguments:
*     hdr = const smfHdr* (Given)
*        Header struct. Assumed to contain a FitsChan in the hdr slot
*     name = const char * (Given)
*        Name of the FITS Item to retrieve.
*     result = char * (Returned)
*        Pointer to string buffer. To guarantee it is large enough
*        to recieve the result, allocate at least 72 characters. The supplied
*        contents are left unchanged if the keyword has an undefined value.
*     len = dim_t (Given)
*        Allocated length of "result" buffer. Including space for NUL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function looks up a FITS header item and stores the result
*     in the string variable provided. If the keyword is not found an error
*     is reported. If the keyword is found, but has no associated value, the
*     contents of the supplied results buffer is left unchanged, but no
*     error is reported. If the keyword has a defined value, is is returned
*     in the buffer.
*
*     This function is similar to smf_fits_getS except that it leaves the
*     returned buffer unchanged if the keyword has an undefined value.

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
*        Handle undef values and rename function.
*        Use smf_validate_smfHead.
*     {enter_further_changes_here}

*  Notes:
*     - if the supplied buffer is too small to receive the string, the
*       result will be truncated and status will be set to ONE__TRUNC.
*     - See also smf_fits_getS

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
#define FUNC_NAME "smf_getfitss"

void smf_getfitss (const smfHead *hdr, const char * name, char * result,
                   dim_t len, int * status ) {
  char * astres; /* Pointer to AST static buffer */
  int i;         /* Loop counter */
  int there;     /* Is the header present? */

  /* Check entry status */
  if (*status != SAI__OK) return;
  if (!smf_validate_smfHead(hdr, 1, 0, status)) return;

  if ( astTestFits( hdr->fitshdr, name, &there ) ) {
    astGetFitsS( hdr->fitshdr, name, &astres );

    /* copy out of buffer */
    one_strlcpy( result, astres, len, status );

    /* AST does not seem to trim trailing spaces from FITS cards */
    if (*status == SAI__OK) {
      for (i=(int)strlen(astres); i >= 0; i--) {
        /* if we are not at a space or nul break from loop */
        if ( result[i] != ' ' && result[i] != '\0') {
          break;
        }
        result[i] = '\0';
      }
    }
  } else if (!there && *status == SAI__OK) {
    *status = SMF__NOKWRD;
    msgSetc("FITS", name );
    errRep(FUNC_NAME, "Unable to retrieve item ^FITS from header",
           status);
  }

  return;
}
