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
*                      int * status );

*  Arguments:
*     inhdr = const AstFitsChan * (Given)
*        FITS header to be merged.
*     outhdr = AstFitsChan ** (Given & Returned)
*        Output FITS header. *outhdr should be NULL for the first
*        call to this routine (it will then be populated with a copy of
*        the first inhdr).
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function is used to build up an output FITS header (as a
*     FitsChan) by merging content from all the input FITS headers.
*     Values in the FITS headers that differ are removed. It should be
*     called once for each input FITS header.

*     Keywords that relate to state from the beginning and end of the
*     observation are propogated explicitly to the merged header such
*     that the values from the newest and oldest header are retained
*     each time. This allows for DATE-OBS and DATE-END headers to be
*     propogated correctly, in addition to headers describing the
*     azimuth, and elevation, temperature and windspeed at the start
*     and end of the combined header.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David Berry (JAC, UCLan)
*     BRADC: Brad Cavanagh (JAC, Hawaii)
*     GSB: Graham Bell (JAC, Hawaii)
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
*     2009-06-17 (TIMJ):
*        Now correctly retain START and END FITS headers during merge
*        so that we end up with a DATE-OBS and DATE-END (among others)
*        in output maps/cubes.
*        Remove obsid map argument since provenance is now handled using
*        NDG.
*     2009-07-06 (TIMJ):
*        Call atlRmblft to remove contiguous blank lines on input header.
*        This will be called by atlMgfts but only if we are merging headers.
*     2010-04-22 (TIMJ):
*        Allow header merging even if a date is not available.
*     2013-05-31 (GSB):
*        Copy START and END FITS headers as floating point if possible.
*     2014-05-08 (GSB):
*        Add OBSGEO headers to the list of items to keep from the end of
*        the group.
*     2016-02-25 (DSB):
*        Add SEQSTART/SEQEND to the list of START and END FITS headers.
*     2017-01-10 (GSB):
*        Add DTAI to begin headers list (for consistency with DUT1).
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2007-2010 Science & Technology Facilities Council.
*     Copyright (C) 2016-2017 East Asian Observatory.
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
#include "smf.h"
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "star/atl.h"
#include "smf_err.h"

static void
smf__fits_copy_items( AstFitsChan * fromfits, AstFitsChan * tofits,
                      const char ** items, int * status );

void smf_fits_outhdr( AstFitsChan * inhdr, AstFitsChan ** outhdr,
                      int * status ) {

/* Local Variables: */
   AstFitsChan *temphdr = NULL;  /* FitsChan holding temporary FITS headers */

/* List of BEGIN  headers that are retained even if different */
   const char * begin_items[] = {
     "DATE-OBS",
     "DUT1",
     "DTAI",
     "LOFREQS",
     "AMSTART",
     "AZSTART",
     "ELSTART",
     "HSTSTART",
     "LSTSTART",
     "TSPSTART",
     "ATSTART",
     "HUMSTART",
     "BPSTART",
     "WNDSPDST",
     "WNDDIRST",
     "TAU225ST",
     "TAUDATST",
     "WVMTAUST",
     "WVMDATST",
     "SEEINGST",
     "FRLEGTST",
     "BKLEGTST",
     "SEQSTART",
     NULL
   };
   const char * end_items[] = {
     "DATE-END",
     "LOFREQE",
     "AMEND",
     "AZEND",
     "ELEND",
     "HSTEND",
     "LSTEND",
     "TSPEND",
     "ATEND",
     "HUMEND",
     "BPEND",
     "WNDSPDEN",
     "WNDDIREN",
     "TAU225EN",
     "TAUDATEN",
     "WVMTAUEN",
     "WVMDATEN",
     "SEEINGEN",
     "FRLEGTEN",
     "BKLEGTEN",
     "SEQEND",
     "OBSGEO-X",
     "OBSGEO-Y",
     "OBSGEO-Z",
     NULL
   };

/* Check inherited status. */
   if ( *status != SAI__OK ) return;

/* If this is the first file, get a copy of the input NDFs FITS extension
   (held in a FitsChan). This FitsChan will be used to hold the FITS
   header for the output NDF. Also remove contiguous blank lines. */
   if( *outhdr == NULL ) {
      *outhdr = astCopy( inhdr );
      atlRmblft( *outhdr, status );

/* If this is not the first file, merge the input NDF's FITS extension
   into the output NDF's FITS extension by removing any headers from the
   output FITS extension that do not have identical values in the input
   FITS extension. */
   } else {
     smfHead hdr;
     double mjdnew = 0.0;
     double mjdref = 0.0;
     AstFitsChan * begfits = NULL;
     AstFitsChan * endfits = NULL;

     /* need to make sure that the merging will not remove headers that need
      to be retained covering start and end state. This means that we take a copy
      of the input header and manually synchronize END/START headers before calling
      the ATL merge routine. */

     /* Do not have access to smfData so need to set one up or duplicate code
        in smf_find_dateobs */
     if (*status == SAI__OK) {
       hdr.allState = NULL;
       hdr.fitshdr = inhdr;
       smf_find_dateobs( &hdr, &mjdnew, NULL, status );
       hdr.fitshdr = *outhdr;
       smf_find_dateobs( &hdr, &mjdref, NULL, status );
       if (*status == SMF__NOKWRD) {
         /* if there is no date information we just do what we can */
         errAnnul( status );
         mjdnew = 0.0;
         mjdref = 0.0;
       }

     }

     if (mjdnew < mjdref) {
       /* input header is older than merged header:
          Copy beginfits from INPUT to MERGE
          Copy endfits from MERGE to INPUT
       */
       begfits = astCopy( inhdr );
       endfits = astCopy( *outhdr );
     } else {
       /* input header is newer than merged header:
          Copy beginfits from MERGE to INPUT
          Copy endfits from INPUT to MERGE

          Do this even if dates are identical or if we could not read a date.
        */
       begfits = astCopy( *outhdr );
       endfits = astCopy( inhdr );
     }

     /* oldfits gets the END items from newfits.
        newfits gets the BEGIN items from oldfits*/
     smf__fits_copy_items( begfits, endfits, begin_items, status );
     smf__fits_copy_items( endfits, begfits, end_items, status );

     /* now we can merge oldfits and newfits */
      atlMgfts( 3, begfits, endfits, &temphdr, status );
      (void) astAnnul( begfits );
      (void) astAnnul( endfits );
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

}


/*
  copy the cards with keywords listed in "items" from "fromfits"
  to "tofits".
 */

static void
smf__fits_copy_items( AstFitsChan * fromfits, AstFitsChan * tofits,
                      const char ** items, int * status ) {
  dim_t i = 0;
  double value;

  if (*status != SAI__OK) return;

  while ( items[i] != NULL ) {
    char card[ 81 ];

    /* reset the position each time since we can not be sure that
       "items" is in order */
    astClear( fromfits, "Card" );

    /* Look in fromfits for the card */
    if ( astFindFits( fromfits, items[i], card, 0  ) ) {

      /* now look in tofits for the card */
      astClear( tofits, "Card" );
      if ( astFindFits( tofits, items[i], NULL, 0 ) ) {
        /* and if we find it replace it */
        if (astGetI(fromfits, "CardType") == AST__FLOAT
            && astGetFitsF(fromfits, NULL, &value)) {
          astSetFitsF(tofits, items[i], value, NULL, 1);
        }
        else {
          astPutFits( tofits, card, 1 );
        }
      }
    }

    i++;
  }

}
