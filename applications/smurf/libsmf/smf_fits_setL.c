/*
*+
*  Name:
*     smf_fits_setL

*  Purpose:
*     Set a logical FITS item value in a header

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_fits_setL( const smfHead *hdr, const char *name, int value, 
*	             const char *comment, int overwrite, int *status ) 

*  Arguments:
*     hdr = const smfHdr* (Given)
*        Header struct. Assumed to contain a FitsChan in the hdr slot
*     name = const char * (Given)
*        Name of the FITS Item to set.
*     value = int (Given)
*        1 (true) or 0 (false)
*     comment = char * (Given)
*        Pointer to string containing comment (NULL if no comment)
*     overwrite = int (Given)
*        If non-zero search for existing FITS keyword with this name and
*        replace it. Otherwise just add new keyword at current position.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function creates (or re-sets) a boolean FITS keyword to the
*     supplied value.

*  Authors:
*     Edward Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2007-11-27 (EC):
*        Initial version
*     2008-12-17 (TIMJ):
*        Use smf_validate_smfHead.
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2007 University of British Columbia.
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
#include "smf_err.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_fits_setL"

void smf_fits_setL( const smfHead *hdr, const char *name, int value, 
		    const char *comment, int overwrite, int *status ) {
  
  if (*status != SAI__OK) return;
  if (!smf_validate_smfHead(hdr, 1, 0, status)) return;

  /* If overwrite set, locate card with same keyword */
  if( overwrite ) {
    astClear( hdr->fitshdr, "Card" );
    astFindFits( hdr->fitshdr, name, NULL, 1);
  }

  /* Now write to the fitschan */
  astSetFitsL( hdr->fitshdr, name, value, comment, overwrite );

  /* Trap Ast error */
  if( !astOK ) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      msgSetc("FITS", name );
      errRep(FUNC_NAME, "Unable to set item ^FITS in header",
	     status);
    }
  }

 return;
}
