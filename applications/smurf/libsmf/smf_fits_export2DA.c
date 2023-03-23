/*
*+
*  Name:
*     smf_fits_export2DA

*  Purpose:
*     Convert AstFitsChan FITS headers to character array

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_fits_export2DA ( const AstFitsChan *fitschan, dim_t *ncards,
*                          char *fitsrec,
*                          int *status )

*  Arguments:
*     fitschan = const AstFitsChan* (Given)
*        AstFitsChan containing FITS headers
*     ncards = size_T* (Returned)
*        Number of cards in FITS header
*     fitsrec = char* (Returned)
*        Character array for converted FITS headers. This should be large enough
*        to hold SC2STORE__MAXFITS headers.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine converts the contents of an AstFitsChan
*     into a character array for storage by the SCUBA-2 DA system.

*  Authors:
*     Tim Jenness (TIMJ)
*     Jen Balfour (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-11-01 (JB):
*        Original version
*     2007-10-22 (TIMJ):
*        Tweak for new sc2store interface.
*     2007-10-31 (TIMJ):
*        Use dim_t to match sc2store.
*     2012-01-20 (TIMJ):
*        Some files have many consecutive blank lines that make the
*        header longer than sc2store can handle. We give ourselves
*        some breathing space by stripping out duplicate blank lines
*        in the FitsChan.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007, 2012 Science and Technology Facilities Council.
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

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

/* Standard includes */
#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"
#include "star/one.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_fits_export2DA"

/* Need to suppress warnings about bad pragmas temporarily since
   "-Wstringop-truncation" (used below0 was not recognised by earlier
   versions of gcc */
#pragma GCC diagnostic ignored "-Wpragmas"
#pragma GCC diagnostic ignored "-Wstringop-truncation"

void smf_fits_export2DA ( AstFitsChan *fitschan, dim_t *ncards,
                          char *fitsrec,
                          int *status ) {

  /* Local variables */
  char blank[SZFITSCARD+1];/* Reference blank card */
  char card[SZFITSCARD+1];/* temporary buffer for current card */
  int found;              /* Boolean to indicate if a card was found */
  dim_t i;               /* Loop counter */
  dim_t ncopied = 0;     /* How many cards were copied */
  dim_t numcards = 0;    /* How many cards are in the FitsChan */
  char *outpos = NULL;    /* current position in output buffer */
  int prevblank = 0;      /* Was this previously blank? */
  char *tempfits = NULL;  /* intermediate buffer for FITS cards */

  *ncards = 0;
  /* Check status */
  if (*status != SAI__OK) return;

  /* Fill the blank card */
  for (i=0; i<SZFITSCARD;i++) {
    blank[i] = ' ';
  }
  blank[SZFITSCARD] = '\0';

  /* Find the number of cards in this AstFitsChan and create a
     buffer for internal use. We do not yet worry about the allocated
     size of fitsrec because we might be compressing the array to
     get rid of multiple blank lines */
  numcards = astGetI ( fitschan, "Ncard" );
  tempfits = astMalloc( ( 1 + numcards * SZFITSCARD ) * sizeof(*tempfits) );

   /* Rewind */
   astClear ( fitschan, "Card");

   if (*status == SAI__OK) {
     /* Retrieve all the FITS headers and store them in the character array.
        We compress consecutive blank cards. */
     ncopied = 0;
     outpos = tempfits;
     prevblank = 0;
     for ( i = 0; i <= numcards; i++ ) {
       found = astFindFits ( fitschan, "%f", card, 1 );
       if ( found ) {
         int isblank = 0;
         if (strncmp( card, blank, SZFITSCARD ) == 0 ) isblank = 1;

         /* skip if this is blank and before was */
         if (isblank && prevblank) continue;
         prevblank = isblank;

         /* Now copy in the card and increment the pointer */
         strncpy ( outpos, card, SZFITSCARD );

         ncopied++;
         outpos += SZFITSCARD;
       } else {
         break;
       }
     }

     /* Guarantee to terminate the buffer */
     *outpos = '\0';

     /* make sure that it is no larger than the maximum allowed */
     if ( ncopied > SC2STORE__MAXFITS ) {
       *status = SAI__ERROR;
       msgSeti("NC", (int)numcards);
       msgSeti("MC", SC2STORE__MAXFITS);
       errRep( FUNC_NAME,
               "Number of FITS cards ^NC exceeds maximum allowed (^MC)",
               status );
       tempfits = astFree( tempfits );
       return;
     }

     /* Copy into the output buffer */
     one_strlcpy( fitsrec, tempfits, SZFITSCARD * SC2STORE__MAXFITS + 1, status );
   }

   tempfits = astFree(tempfits);
   *ncards = ncopied;
}
