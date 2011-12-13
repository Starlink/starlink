/*
*+
*  Name:
*     smf_fits_updateU

*  Purpose:
*     Set value for a keyword to undef, creating if necessary

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_fits_updateU( smfHead * hdr, const char * name,
*                           const char * comment, int *status );

*  Arguments:
*     hdr = smfHead * (Given)
*        smfHead containing a FitsChan.
*     name = const char * (Given)
*        Name of the FITS Item to undefine.
*     comment = const char * (Given)
*        If a new header is to be created use this comment, else if the
*        card already exists this value will be ignored.
*     status = int * (Given & Returned)
*        Pointer to global status

*  Returned Value:
*     Returns 1 if a new header was created, -1 if the header was updated and
*     0 on error.

*  Description:
*     Undefines the value associated with a particular header item. If the header
*     item does not exist a new one is created.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     2009-05-26 (TIMJ):
*        Initial version.
*     2009-06-26 (TIMJ):
*        Copy from smf_fits_updateD

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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

#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "prm_par.h"
#include "mers.h"

#include "smf.h"

int
smf_fits_updateU( smfHead * hdr, const char * name,
                  const char * comment, int *status ) {
  int retval = 0;                 /* Return value */
  AstFitsChan * fits;             /* FitsChan to be modified */
  const char * newcomment = NULL; /* Comment to be applied to card */

  if (*status != SAI__OK) return retval;
  if (!smf_validate_smfHead(hdr, 1, 0, status)) return retval;

  fits = hdr->fitshdr;

  /* start from beginning of header since we can not be sure where we are starting
     from */
  astClear( fits, "Card" );

  /* Look for the current version of the card */
  if ( astFindFits( fits, name, NULL, 0 ) ) {
    /* found the card - so updating it (no need for comment) */
    retval = -1;
  } else {
    /* new card. So need the supplied comment */
    retval = 1;
    newcomment = comment;
  }

  /* always use overwrite = true since if the card is missing we'll be at the end
     of the header and it will make no difference */
  astSetFitsU( fits, name, newcomment, 1 );

  return retval;
}
