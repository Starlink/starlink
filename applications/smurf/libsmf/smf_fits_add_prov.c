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
*     smf_fits_add_prov( AstFitsChan * hdr, const AstKeyMap * obsidmap,
*                    int * status );

*  Arguments:
*     hdr = const AstFitsChan * (Given)
*        FITS header to be modified.
*     obsidmap = const AstKeyMap * (Given)
*        Keymap for tracking OBSID information. Probably created by
*        smf_fits_outhdr.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function is used to populate the output FITS header with
*     the observation identifiers tracking data provenance. The provenance
*     should be available from an AST KeyMap indexed by OBSID.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Brad Cavanagh (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2007-03-20 (TIMJ):
*        Initial version. Refactored from code by
*        Brad Cavanagh from smurf_makecube.c
*     {enter_further_changes_here}

*  Notes:
*     - See Also smf_fits_outhdr for construction of a suitable keymap.
*     - The OBSIDs are stored in the output header using keys OBSnnnnn
*     where nnnnn is a zero-padded number. The number of OBSIDs involved
*     in the observation is stored in the PROVCNT header.

*  Copyright:
*     Copyright (C) 2007 Particle Physics and Astronomy Research Council.
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
smf_fits_add_prov( AstFitsChan * hdr, const AstKeyMap * obsidmap,
		 int * status ) {

  int i;                     /* Loop counter */
  int len;                   /* Length of string returned from snprintf */
  char obshdr[12];           /* OBSnnnnn string buffer */
  const char *obsid = NULL;        /* Value of current OBSID */
  int provcnt = 0;           /* Number of unique OBSIDs from input files */

  if ( *status != SAI__OK ) return;

  /* See how many OBSIDs we actually have and write PROVCNT header */
  provcnt = astMapSize( obsidmap );
  atlPtfti( hdr, "PROVCNT", provcnt,
	    "Number of unique OBSIDs", status );

  /* Now extract the OBSIDs from the map */
  for( i = 0; i < provcnt; i++ ) {
    len = snprintf( obshdr, 9, "OBS%05d", i+1 );
    if( len != 8 ) {
      if( *status == SAI__OK ) {
	*status = SAI__ERROR;
	msgSeti( "N", i );
	errRep( "", "Buffer overflow in setting OBSnnnnn to ^N", status );
	break;
      }
    }
    /* and store in the header */
    obsid = astMapKey( obsidmap, i );
    atlPtfts( hdr, obshdr, obsid,
	      "OBSID from component observation", status );
  }

}
