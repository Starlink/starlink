/*
*+
*  Name:
*     smf_is_wvm_usable

*  Purpose:
*     Is the WVM thought to be usable for this data set?

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     isusable = smf_is_wvm_usable( const smfHead* hdr, int *status );

*  Arguments:
*     hdr = const smfHead * (Given)
*        Header to examine to determine relevant information (date)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Looks at the date in the header and compares that with a list of nights known
*     for an unstable or missing WVM. Assumes that unlisted nights are good.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - Initially done with a static list of unstable nights hard-coded into source file.
*     - Nights supplied by Daniel Berke 2013-06-19

*  History:
*     2013-06-21 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2013 Science and Technology Facilities Council.
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

typedef struct {
    int date_start;
    int date_end;
} date_range;

int
smf_is_wvm_usable( const smfHead * hdr, int *status ) {

  size_t i = 0;
  int utdate;           /* UTDATE FITS header */
  int isusable = 1;     /* Default to probably being usable */

  /* Initially just have a static list in the source code */
  const int badwvm[] = {
    20120912,
    20130119,
    20130120,
    20130223,
    20130224,
    20130225,
    20130308,
    20130309,
    20130315,
    20130316,
    20130319,
    20130320,
    20130321,
    20130322,
    20130323,
    20130324,
    20130325,
    20130327,
    20130328,
    20130329,
    20130330,
    20130331,
    20130401,
    20130402,
    20130403,
    20130404,
    20130405,
    20130406,
    20130407,
    20130408,
    20130508,
    20130509,
    20130510,
    20130511,
    20130512,
    20130513,
    20130514,
    20130523,
    20130717,
    20140128,
    20140827,
    20140829,
    20140830,
    0    /* Sentinenl */
  };

  /* Also list date ranges for times the WVM is unusable for extended
     periods. */
  const date_range badwvm_range[] = {
    {20150401, 20151231}, /* Black WVM installed, unknown if working. */
    {0, 0},               /* End marker */
  };

  if (*status != SAI__OK) return 0;

  /* Get the UT date from the header */
  smf_getfitsi( hdr, "UTDATE", &utdate, status );

  for (i = 0; badwvm[i] != 0; i++) {
    if (utdate == badwvm[i]) {
      isusable = 0;
      break;
    }
  }

  for (i = 0; badwvm_range[i].date_start && isusable; i ++) {
    if (utdate >= badwvm_range[i].date_start &&
        utdate <= badwvm_range[i].date_end) {
      isusable = 0;
    }
  }

  return isusable;
}
