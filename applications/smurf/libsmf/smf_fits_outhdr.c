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
*                      AstKeyMap ** obsidmap, int * status );

*  Arguments:
*     inhdr = const AstFitsChan * (Given)
*        FITS header to be merged.
*     outhdr = AstFitsChan ** (Given & Returned)
*        Output FITS header. *outhdr should be NULL for the first
*        call to this routine (it will then be populated with a copy of
*        the first inhdr).
*     obsidmap = AstKeyMap ** (Given & Returned)
*        Keymap for tracking OBSID information. *obsidmap should be NULL 
*        for the first call to this routine. Also, the supplied pointer 
*        itself may be NULL, in which case no KeyMap is created.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function is used to build up an output FITS header (as a 
*     FitsChan) by merging content from all the input FITS headers.
*     Values in the FITS headers that differ are removed. It should be 
*     called once for each input FITS header. Additionally, the routine 
*     keeps track of all the OBSIDs present in the input FITS headers
*     such that the input data sets can be tracked into the output file
*     (very important for data provenance tracking).

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David Berry (JAC, UCLan)
*     BRADC: Brad Cavanagh (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2007-03-20 (TIMJ):
*        Initial version. Refactored from code by David Berry
*        and Brad Cavanagh from smurf_makecube.c
*     2007-06-22 (TIMJ):
*        Use OBSIDSS in preference to OBSID. Try to build OBSID from
*        OBSID and SUBSYSNR if OBSID is missing.
*     2008-03-27 (DSB):
*        Use smf_getobsidss to determine the OBSIDSS value.
*     2008-04-25 (DSB):
*        Allow a NULL pointer to be supplied for "obsidmap".
*     2008-06-10 (DSB):
*        Remove ASTWARN cards form the output header.
*     2008-07-31 (TIMJ):
*        Change getobsidss API to be thread-safe.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2007 Science and Technology Facilities Council.
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
#include "smf.h"
#include "sae_par.h"
#include "ast.h"
#include "star/atl.h"

void smf_fits_outhdr( AstFitsChan * inhdr, AstFitsChan ** outhdr,
                      AstKeyMap ** obsidmap, int * status ) {

/* Local Variables: */
   char obsidss[SZFITSCARD];     /* Somewhere to put the obsidss */
   AstFitsChan *temphdr = NULL;  /* FitsChan holding temporary FITS headers */

/* Check inherited status. */
   if ( *status != SAI__OK ) return;

/* If this is the first file, get a copy of the input NDFs FITS extension
   (held in a FitsChan). This FitsChan will be used to hold the FITS
   header for the output NDF. Also create the output key map at this point. */
   if( *outhdr == NULL ) {
      *outhdr = astCopy( inhdr );
      if( obsidmap ) *obsidmap = astKeyMap( "" );

/* If this is not the first file, merge the input NDF's FITS extension
   into the output NDF's FITS extension by removing any headers from the
   output FITS extension that do not have identical values in the input
   FITS extension. */
   } else {
      atlMgfts( 3, inhdr, *outhdr, &temphdr, status );
      (void) astAnnul( *outhdr );
      *outhdr = temphdr;
   }

/* Remove any ASTWARN cards from the output header, but retain them 
   within the input header. Any such warnings in the input header will 
   be displayed when the input NDF is closed using smf_close_file. This
   helps to track down bugs caused by keywords unintentionally having
   undefined values in an input NDF. */
   astClear( *outhdr, "Card" );
   while( astFindFits( *outhdr, "ASTWARN", NULL, 0 ) ){
      astDelFits( *outhdr );
   }

/* Work out the OBSID - note that we obtain the value from the input FITS
   header so that we do not risk losing the value after merging of the
   FITS headers. CADC require that it is OBSIDSS that is unique and the
   thing that should be tracked. */
   if( obsidmap ) astMapPut0I( *obsidmap, 
                               smf_getobsidss( inhdr, NULL, 0, obsidss,
                                               sizeof(obsidss), status ),
                               1, NULL );
}
