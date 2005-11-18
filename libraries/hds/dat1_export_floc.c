
#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <string.h>

#include "ems.h"

#include "hds1.h"
#include "rec.h"
#include "dat1.h"
#include "hds_types.h"
#include "dat_err.h"

/*
 *+
 *  Name:
 *    dat1_export_floc

 *  Purpose:
 *    Export a fortran HDS locator buffer from C

 *  Invocation:
 *    dat1_export_floc( HDSLoc* clocator, int free, int len, char flocator[DAT__SZLOC], int * status );

 *  Description:
 *    This function should be used to populate a Fortran HDS locator buffer
 *    from a C struct.

 *  Arguments

 *    HDSLoc * clocator = Given
 *       Locator to be exported.
 *    int free = Given
 *       If true (1) the C locator is freed using dat1_free_hdsloc once the
 *       Fortran locator is populated. If false, the locator memory is not
 *       touched.
 *    int len = Given
 *       Size of Fortran character buffer. Sanity check.
 *    char flocator[DAT__SZLOC] = Returned
 *       Fortran character string buffer. Should be at least DAT__SZLOC
 *       characters long. If clocator is NULL, fills the buffer with DAT__NOLOC.
 *    int * status = Given & Returned
 *       Inherited status. If status is bad the Fortran locator will be
 *       filled with DAT__NOLOC. The memory associated with clocator will
 *       be freed if free is true.

 *  Authors:
 *    Tim Jenness (JAC, Hawaii)

 *  History:
 *    16-NOV-2005 (TIMJ):
 *      Initial version

 *  Notes:
 *    Fortran locator string must be preallocted. The C locator can be freed
 *    at the discretion of the caller. This simplifies code in the fortran
 *    interface wrapper.

 *  Copyright:
 *    Copyright (C) 2005 Particle Physics and Astronomy Research Council.
 *    All Rights Reserved.

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

void dat1_export_floc ( HDSLoc *clocator, int free, int loc_length, char flocator[DAT__SZLOC], int * status) {

/* Validate the locator length.                                             */
  if (*status == DAT__OK && loc_length != DAT__SZLOC ) {
    *status = DAT__LOCIN;
    emsSeti( "LEN", loc_length );
    emsSeti( "SZLOC", DAT__SZLOC );
    emsRep( "DAT1_IMPORT_FLOC", "Locator length is ^LEN not ^SZLOC", status);
  };

/* If OK, then extract the information from the locator string (necessary   */
/* to ensure that data alignment is correct, as the string will normally be */
/* stored externally in a Fortran CHARACTER variable).                      */

  if ( *status == DAT__OK && clocator != NULL ) {
    memmove( flocator, clocator, sizeof( struct LOC ) );
  } else {
    strncpy( flocator, DAT__NOLOC, DAT__SZLOC );
  }


  /* Free regardless of status */
  if (free) dat1_free_hdsloc( &clocator );

  return;
}
