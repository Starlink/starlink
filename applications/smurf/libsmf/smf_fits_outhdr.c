/*
*+
*  Name:
*     smf_fits_outhdr

*  Purpose:
*     Form output file FITS header from unique content of input files

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_fits_outhdr( const AstFitsChan * inhdr, AstFitsChan ** outhdr,
*                    AstKeyMap ** obsidmap, int * status );

*  Arguments:
*     inhdr = const AstFitsChan * (Given)
*        FITS header to be merged.
*     outhdr = AstFitsChan ** (Given & Returned)
*        Output FITS header. *outhdr should be NULL for the first
*        call to this routine (it will then be populated with a copy of
*        the first inhdr).
*     obsidmap = AstKeyMap ** (Given & Returned)
*        Keymap for tracking OBSID information. *obsidmap hould be NULL for the
*        first call to this routine.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function is used to build up an output FITS header (as a 
*     FitsChan) by merging content from all the input FITS headers.
*     Values in the FITS headers that differ are removed. It
*     should be called once for each input FITS header. Additionally, the
*     routine keeps track of all the OBSIDs present in the input FITS headers
*     such that the input data sets can be tracked into the output file
*     (very important for data provenance tracking).

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     David Berry (JAC, UCLan)
*     Brad Cavanagh (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2007-03-20 (TIMJ):
*        Initial version. Refactored from code by David Berry
*        and Brad Cavanagh from smurf_makecube.c
*     2007-06-22 (TIMJ):
*        Use OBSIDSS in preference to OBSID. Try to build OBSID from
*        OBSID and SUBSYSNR if OBSID is missing.
*     {enter_further_changes_here}

*  Notes:
*     - See smf_fits_add_prov for a routine on adding provenance information
*     to the output FITS header.

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
#include "smf.h"
#include "sae_par.h"
#include "ast.h"
#include "star/atl.h"
#include "merswrap.h"

void
smf_fits_outhdr( AstFitsChan * inhdr, AstFitsChan ** outhdr,
		 AstKeyMap ** obsidmap, int * status ) {

   AstFitsChan *temphdr = NULL;/* FitsChan holding temporary FITS headers */
   char *obsid = NULL;         /* Value of current file's OBSIDSS header */
   char tempobs[80];           /* Temp space if we need to build OBSIDSS */
   char obsidss[80];           /* Temp space for the built OBSIDSS */

  if ( *status != SAI__OK ) return;

/* If this is the first file, get a copy of the input NDFs FITS extension
   (held in a FitsChan). This FitsChan will be used to hold the FITS
   header for the output NDF. Also create the output key map at this point. */

      if( *outhdr == NULL ) {
         *outhdr = astCopy( inhdr );

         *obsidmap = astKeyMap( "" );

/* If this is not the first file, merge the input NDF's FITS extension
   into the output NDF's FITS extension by removing any headers from the
   output FITS extension that do not have identical values in the input
   FITS extension. */
      } else {

         atlMgfts( 3, inhdr, *outhdr, &temphdr, status );
         (void) astAnnul( *outhdr );
         *outhdr = temphdr;
      }

      /* Work out the OBSID - note that we obtain the value from the input FITS
	 header so that we do not risk losing the value after merginf of the
	 FITS headers. CADC require that it is OBSIDSS that is unique and the
	 thing that should be tracked. */
      if( astGetFitsS( inhdr, "OBSIDSS", &obsid ) ) {
	astMapPut0I( *obsidmap, obsid, 1, NULL );
      } else if ( astGetFitsS( inhdr, "OBSID", &obsid ) ) {
	/* Have an OBSID - so look for a SUBSYSNR - note that
	   SCUBA-2 will not have one but we guarantee that it will
	   have OBSIDSS so we should not end up in this branch */
	/* need a local copy */
	strncpy( tempobs, obsid, 80);
	if ( astGetFitsS( inhdr, "SUBSYSNR", &obsid) ) {
	  snprintf( obsidss, 80, "%s_%s", tempobs, obsid );
	  astMapPut0I( *obsidmap, obsidss, 1, NULL );
	} else {
	  if (*status == SAI__OK) {
	    *status = SAI__ERROR;
	    errRep("", "Could not form OBSIDSS value since OBSIDSS is missing and so is SUBSYSNR", status);
	  }
	}

      }
}
