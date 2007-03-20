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
*     {enter_further_changes_here}

*  Notes:
*     - See smf_fits_add_prov for a routine on adding provenance information
*     to the output FITS header.

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
#include "smf.h"
#include "sae_par.h"
#include "ast.h"
#include "star/atl.h"

void
smf_fits_outhdr( AstFitsChan * inhdr, AstFitsChan ** outhdr,
		 AstKeyMap ** obsidmap, int * status ) {

   AstFitsChan *temphdr = NULL;/* FitsChan holding temporary FITS headers */
   char *obsid = NULL;         /* Value of current file's OBSID header */

  if ( *status != SAI__OK ) return;

/* If this is the first file, get a copy of the input NDFs FITS extension
   (held in a FitsChan). This FitsChan will be used to hold the FITS
   header for the output NDF. Add this NDF's OBSID FITS header to the
   KeyMap, if there is an OBSID header. */
      if( *outhdr == NULL ) {
         *outhdr = astCopy( inhdr );

         *obsidmap = astKeyMap( "" );

         if( astGetFitsS( *outhdr, "OBSID", &obsid ) ) {
            astMapPut0I( *obsidmap, obsid, 1, NULL );
         }

/* If this is not the first file, merge the input NDF's FITS extension
   into the output NDF's FITS extension by removing any headers from the
   output FITS extension that do not have identical values in the input
   FITS extension. Also, check to see if the OBSID FITS header is unique
   and, if so, place it in the KeyMap. Do so before merging the headers
   in case this information is lost upon merging. */
      } else {

         if( astGetFitsS( inhdr, "OBSID", &obsid ) ) {
           astMapPut0I( *obsidmap, obsid, 1, NULL );
         }

         atlMgfts( 3, inhdr, *outhdr, &temphdr, status );
         (void) astAnnul( *outhdr );
         *outhdr = temphdr;
      }


}
