/*
*+
*  Name:
*     smf_fits_getF

*  Purpose:
*     Obtain a float FITS item value from a header

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_fits_getF( const smfHead * hdr, const char * name, float * result, int * status );

*  Arguments:
*     hdr = const smfHdr* (Given)
*        Header struct. Assumed to contain a FitsChan in the hdr slot
*     name = const char * (Given)
*        Name of the FITS Item to retrieve.
*     result = float * (Returned)
*        Pointer of float to store the result.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function looks up a FITS header item and stores the result
*     in the variable provided.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-01-11 (AGG):
*        Initial version, copy from smf_fits_getD
*     {enter_further_changes_here}

*  Notes:
*     - See also smf_fits_getI, smf_fits_getS and smf_fits_getD

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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
#include "ast.h"
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_fits_getF"

void smf_fits_getF (const smfHead *hdr, const char * name, float * result, int * status ) {

  if (*status != SAI__OK) return;

  if ( hdr == NULL ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME,
	    "Supplied hdr is a NULL pointer. Possible programming error.",
	    status);
    return;
  }

  if ( hdr->fitshdr == NULL ) {
    * status = SAI__ERROR;
    errRep( FUNC_NAME,
	    "No FitsChan associated with supplied header. Possible programming error.",
	    status );
    return;
  }

  if ( !astGetFitsF( hdr->fitshdr, name, result) ) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      msgSetc("FITS", name );
      errRep(FUNC_NAME, "Unable to retrieve item ^FITS from header",
	     status);
    }
  }

  return;
}
