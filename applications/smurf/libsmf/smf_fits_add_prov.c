/*
*+
*  Name:
*     smf_fits_add_prov

*  Purpose:
*     Add provenance information to output FITS header

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_fits_add_prov( AstFitsChan * hdr, const char * keyroot,
*                    const AstKeyMap * obsidmap, int * status );

*  Arguments:
*     hdr = const AstFitsChan * (Given)
*        FITS header to be modified.
*     keyroot = const char * (Given)
*        Root of FITS keywords to be created. Will be prepended
*        to "CNT" for the count and 5 digit number to indicate
*        position. Must be 3 characters or less.
*     obsidmap = const AstKeyMap * (Given)
*        Keymap for tracking information. Probably created by
*        smf_fits_outhdr. The keys (not the values) will be written.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function is used to populate the output FITS header with
*     the observation identifiers tracking data provenance. The provenance
*     should be available from an AST KeyMap and will be obtained by
*     looking at the keys and not the values. Can be used for writing OBSID
*     and file provenance.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Brad Cavanagh (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2007-03-20 (TIMJ):
*        Initial version. Refactored from code by
*        Brad Cavanagh from smurf_makecube.c
*     2007-06-22 (TIMJ):
*        More generic so as to support OBS* and PRV*
*     {enter_further_changes_here}

*  Notes:
*     - See Also smf_fits_outhdr for construction of a suitable keymap.
*     - The keys are stored in the output header using keys XXXnnnnn
*     where nnnnn is a zero-padded number. The number of keys involved
*     in the observation is stored in the XXXCNT header.

*  Copyright:
*     Copyright (C) 2007 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2007 Science and Technology Facilities Council.
*     All Rights Reserved.

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

#include <stdio.h>
#include "smf.h"
#include "mers.h"
#include "sae_par.h"
#include "ast.h"
#include "star/atl.h"

void
smf_fits_add_prov( AstFitsChan * hdr, const char * keyroot,
		   const AstKeyMap * keymap, int * status ) {

  int i;                     /* Loop counter */
  int len;                   /* Length of string returned from snprintf */
  char keyword[12];          /* XXXCNT and XXXnnnnn string buffer */
  char comment[72];          /* Buffer for comment */
  const char *id = NULL;     /* Value of current ID */
  int count = 0;             /* Number of unique keys from keymap */

  if ( *status != SAI__OK ) return;

  /* Check that the keyword root is short enough */
  if (keyroot == NULL) {
    *status = SAI__ERROR;
    errRep( "", "NULL key root given to smf_fits_add_prov", status );
    return;
  }
  if (strlen(keyroot) > 3) {
    *status = SAI__ERROR;
    msgSeti( "LEN", (int)strlen(keyroot));
    errRep( "", "Root of FITS keyword was ^LEN characters but should"
	    " not exceed 3 characters", status);
    return;
  }

  /* See how many keys we actually have and write XXXCNT header
     (we know that keyroot is small enough) */
  snprintf(keyword, 9, "%sCNT", keyroot);
  snprintf(comment, 72, "Number of unique %s entries", keyroot);
  count = astMapSize( keymap );
  atlPtfti( hdr, keyword, count, comment, status );

  /* build up comment for this item */
  snprintf(comment, 72, "%s value from component observation", keyroot);

  /* Now extract the OBSIDs from the map */
  for( i = 0; i < count; i++ ) {
    len = snprintf( keyword, 9, "%s%05d", keyroot, i+1 );
    if( len != 8 ) {
      if( *status == SAI__OK ) {
	*status = SAI__ERROR;
	msgSeti( "N", i );
	msgSetc( "KEY", keyroot);
	errRep( "", "Buffer overflow in setting ^KEYnnnnn to ^N", status );
	break;
      }
    }
    /* and store in the header */
    id = astMapKey( keymap, i );
    atlPtfts( hdr, keyword, id, comment, status );
  }

}
