
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

#include "hds_fortran.h"

/*
 *+
 *  Name:
 *    datExportFloc

 *  Purpose:
 *    Export from a C HDS Locator to a Fortran HDS locator

 *  Language:
 *    Starlink ANSI C

 *  Invocation:
 *    datExportFloc( HDSLoc** clocator, int free, int len, char flocator[DAT__SZLOC], int * status );

 *  Description:
 *    This function should be used to populate a Fortran HDS locator buffer
 *    (usually a Fortran CHARACTER string of size DAT__SZLOC) from a C HDS
 *    locator structure. This function is also available in the
 *    HDS_EXPORT_CLOCATOR macro defined in hds_fortran.h.

 *  Arguments:
 *    HDSLoc ** clocator = Given & Returned
 *       Pointer to Locator to be exported. If the memory is freed, the
 *       locator will be set to NULL on return, else it will be untouched.
 *    int free = Given
 *       If true (1) the C locator is freed once the
 *       Fortran locator is populated. If false, the locator memory is not
 *       touched.
 *    int len = Given
 *       Size of Fortran character buffer to recieve the locator. Sanity check.
 *    char flocator[DAT__SZLOC] = Returned
 *       Fortran character string buffer. Should be at least DAT__SZLOC
 *       characters long. If clocator is NULL, fills the buffer
 *       with DAT__NOLOC.
 *    int * status = Given & Returned
 *       Inherited status. If status is bad the Fortran locator will be
 *       filled with DAT__NOLOC. The memory associated with clocator will
 *       be freed if free is true regardless of status.

 *  Authors:
 *    TIMJ: Tim Jenness (JAC, Hawaii)

 *  History:
 *    16-NOV-2005 (TIMJ):
 *      Initial version
 *    18-NOV-2005 (TIMJ):
 *      Make semi public to allow other Fortran wrappers to use this function.
 *      Rename from dat1_export_floc to datExportFloc

 *  Notes:
 *    - Fortran locator string must be preallocted. The C locator can be freed
 *    at the discretion of the caller. This simplifies code in the Fortran
 *    interface wrapper.
 *    - This routine is intended to be used solely for
 *    wrapping Fortran layers from C. "Export" means to export a native
 *    C locator to Fortran.
 *    - There is no Fortran eqiuvalent to this routine.

 *  See Also:
 *    datImportFloc

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
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

void datExportFloc ( HDSLoc **clocator, int free, int loc_length, char flocator[DAT__SZLOC], int * status) {

/* Validate the locator length.                                             */
  if (*status == DAT__OK && loc_length != DAT__SZLOC ) {
    *status = DAT__LOCIN;
    emsSeti( "LEN", loc_length );
    emsSeti( "SZLOC", DAT__SZLOC );
    emsRep( "datExportFloc", "Locator length is ^LEN not ^SZLOC", status);
  };

/* If OK, then extract the information from the locator string (necessary   */
/* to ensure that data alignment is correct, as the string will normally be */
/* stored externally in a Fortran CHARACTER variable).                      */

  if ( *status == DAT__OK && *clocator != NULL ) {
    memmove( flocator, *clocator, sizeof( struct LOC ) );
  } else {
    strncpy( flocator, DAT__NOLOC, DAT__SZLOC );
  }


  /* Free regardless of status */
  if (free) dat1_free_hdsloc( clocator );

  return;
}
