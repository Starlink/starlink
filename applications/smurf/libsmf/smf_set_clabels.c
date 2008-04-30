/*
 *+
 *  Name:
 *     smf_set_clabels

 *  Purpose:
 *     Set units, label and title in smfHead

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Library routine

 *  Invocation:
 *     smf_set_clabels( const char title[], const char label[],
 *                      const char units[], smfHead *hdr, int * status );

 *  Arguments:
 *     title = const char[] (Given)
 *        Data title. Not modified if NULL.
 *     label = const char[] (Given)
 *        Data label. Not modified if NULL.
 *     units = const char[] (Given)
 *        Data units. Not modified if NULL.
 *     hdr = const smfHead* (Given)
 *        Header to be modified. No action if hdr is NULL.
 *     status = int (Given & Returned)
 *        inherited status.

 *  Description:
 *     Copies the title, label and units into a smfHead.

 *  Authors:
 *     Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     2008-04-30 (TIMJ):
 *        Original version.

*  Copyright:
 *     Copyright (C) 2008 Science and Technology Facilities Council.
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

 *-
 */

/* System includes */
#include <string.h>

/* Starlink includes */
#include "ndf.h"
#include "sae_par.h"

#include "smf.h"
#include "smf_typ.h"

void smf_set_clabels( const char title[], const char label[], const char units[],
		      smfHead* hdr, int * status ) {

  if (*status != SAI__OK) return;
  if (hdr == NULL) return;

  if (title) {
    strncpy( hdr->title, title, SMF__CHARLABEL);
    (hdr->title)[SMF__CHARLABEL-1] = '\0';
  }
  if (label) {
    strncpy( hdr->dlabel, label, SMF__CHARLABEL);
    (hdr->dlabel)[SMF__CHARLABEL-1] = '\0';
  }
  if (units) {
    strncpy( hdr->units, units, SMF__CHARLABEL);
    (hdr->units)[SMF__CHARLABEL-1] = '\0';
  }

  return;
}
