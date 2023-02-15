/*
 *+
 *  Name:
 *     smf_check_units

 *  Purpose:
 *     Compare current data units with previous data units

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Library routine

 *  Invocation:
 *     smf_check_units( dim_t count, char current[],
 *                      const smfHead *hdr, int * status );

 *  Arguments:
 *     count = dim_t (Given)
 *        If count equals 1 the units are read and stored in current.
 *        Else units are compared.
 *     current = char[] (Given & Returned)
 *        Current unit string. Will be set if first is true. Else
 *        will be compared to the new value. Must be at least SMF__CHARLABEL
 *        characters long.
 *     hdr = const smfHead* (Given)
 *        Header from which to extract units either for comparison or
 *        for initial retrieval.
 *     status = int (Given & Returned)
 *        inherited status.

 *  Description:
 *     First time through, copies the data units to the buffer and returns.
 *     On subsequent calls compares the supplied units string with the value
 *     in the smfHead and sets status to bad if they differ.

 *  Authors:
 *     Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     2008-05-01 (TIMJ):
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
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA

 *  Bugs:

 *-
 */

/* System includes */
#include <string.h>

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "star/one.h"

#include "smf.h"
#include "smf_typ.h"

void smf_check_units( dim_t count, char current[],
		      smfHead* hdr, int * status ) {


  if (*status != SAI__OK) return;
  if (hdr == NULL) return;

  /* Check units are consistent */
  if (count == 1) {
    one_strlcpy( current, hdr->units, SMF__CHARLABEL, status );
  } else {
    if (strcmp( current, hdr->units) != 0 ) {
      *status = SAI__ERROR;
      msgSetk( "I", count);
      msgSetc( "PRV", current );
      msgSetc( "CUR", hdr->units );
      errRep( "", "Data units inconsistency. Previously got '^PRV'"
	      " but file ^I had units of '^CUR'", status);
    }
  }

  return;
}
