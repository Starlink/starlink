/*
*+
*  Name:
*     smf_fits_crchan

*  Purpose:
*     Helper routine for creating an AST FitsChan from FITS headers

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_fits_crchan( int nfits, const char * headrec, AstFitsChan **fchan, int *status );

*  Arguments:
*     nfits = int (Given)
*        Number of FITS headers to extract. If a single buffer is used
*        (see headrec) it can be set to 0 to be calculated explicitly.
*        If an explicit value is used for a single buffer, a check is made
*        to ensure that nfits does not exceed the buffer. If a nul-embedded
*        buffer is supplied and nfits is 0, it is explicitly set to 1.
*     headrec = const char * (Given)
*        Buffer to fits header. Can be either a block of nfits*80
*        headers with a single nul terminator (determined by the string
*        length being greater than 80), or a buffer with embedded nulls
*        containing nfits headers (Determined by ths string length less
*        than 81).
*     fchan = AstFitsChan** (Returned)
*        AST Fits Chan containing the FITS information
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine for reading FITS information from an NDF. It can
*     work with NDF "standard" FITS buffer (80 * nfits null-terminated buffer)
*     or with a buffer containing embedded nuls (as used to be used by sc2store).


*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2005-11-04 (AGG):
*        Initial test version
*     2005-11-07 (TIMJ):
*        Fix logic that ditinguishes sc2store from datGetc
*     2005-11-29 (TIMJ):
*        Use astPutFits rather than a callback (much easier)
*     2006-06-02 (TIMJ):
*        Fix compiler warnings. Check status.
*     2007-10-22 (TIMJ):
*        Tidy prologue. Make more robust by checking nfits value.
*     2007-10-31 (TIMJ):
*        Fix compiler warning.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007 Science and Technology Facilities Council.
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
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

#include "ast.h"
#include "ndf.h"
#include "star/hds.h"
#include "sae_par.h"
#include <stdlib.h>
#include "mers.h"
#include <string.h>
#include "smf.h"

void smf_fits_crchan( dim_t nfits, const char * headrec, AstFitsChan ** fits,
		      int *status ) {

  /* Sc2store header records are each null terminated at the last non-whitespace
     character so we need to search for a null in the first 81 characters to
     distinguish this from the "read a block" approach. The quick way to do this
     is to use strlen since we know that all the strings are terminated somewhere.
  */

  const char *card = NULL;
  dim_t i;
  dim_t step = 0;
  dim_t len;

  if (*status != SAI__OK) return;

  len = strlen( headrec );

  if ( len <= SZFITSCARD + 1 ) {
    /* individually terminated or a single string */
    step = SZFITSCARD + 1;

    /* nfits can not be trusted */
    if (nfits == 0) nfits = 1;

  } else {
    step = SZFITSCARD;

    /* check nfits */
    if ( (nfits * SZFITSCARD) > len ) nfits = 0;
    if (nfits == 0) nfits = (int)len / SZFITSCARD;
  }

  /* Create the empty fitschan. */
  *fits = astFitsChan( NULL, NULL, " " );

  /* Fill it */
  card = headrec;
  for (i = 0; i < nfits; i++ ) {
    astPutFits( *fits, card, 0 );
    card += step;
  }

  return;

}
