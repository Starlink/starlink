
#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <string.h>

#include "ems.h"
#include "star/one.h"
#include "sae_par.h"

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

 *  Type of Module:
 *    Library routine

 *  Invocation:
 *    datExportFloc( HDSLoc** clocator, int free, int len, char flocator[DAT__SZLOC], int * status );

 *  Description:
 *    This function should be used to populate a Fortran HDS locator buffer
 *    (usually a Fortran CHARACTER string of size DAT__SZLOC) from a C HDS
 *    locator structure. This function is also available in the
 *    HDS_EXPORT_CLOCATOR macro defined in hds_fortran.h.

 *  Arguments:
 *    clocator = HDSLoc ** (Given and Returned)
 *       Pointer to the locator to be exported. See the "free" description below
 *       as to whether this locator will be annulled or not.
 *    free = int (Given)
 *       If true (1) the C locator is nulled out once the
 *       Fortran locator is populated. If false, the locator memory is not
 *       touched. Regardless, the locator itself is not annulled as it is
 *       now simply referenced from the Fortran.
 *    len = int (Given)
 *       Size of Fortran character buffer to receive the locator. Sanity check.
 *       Should be DAT__SZLOC.
 *    flocator = char [DAT__SZLOC] (Returned)
 *       Fortran character string buffer. Should be at least DAT__SZLOC
 *       characters long. If clocator is NULL, fills the buffer
 *       with DAT__NOLOC.
 *    status = int* (Given and Returned)
 *       Inherited status. If status is bad the Fortran locator will be
 *       filled with DAT__NOLOC. The memory associated with clocator will
 *       be freed if free is true regardless of status.

 *  Authors:
 *    TIMJ: Tim Jenness (JAC, Hawaii)
 *    TIMJ: Tim Jenness (Cornell)
 *    {enter_new_authors_here}

 *  History:
 *    16-NOV-2005 (TIMJ):
 *      Initial version
 *    18-NOV-2005 (TIMJ):
 *      Make semi public to allow other Fortran wrappers to use this function.
 *      Rename from dat1_export_floc to datExportFloc
 *    2014-09-07 (TIMJ):
 *      Rewrite to store the pointer (as string) directly in the Fortran
 *      string buffer rather than the contents of the struct.
 *     {enter_further_changes_here}

 *  Notes:
 *    - Fortran locator string must be preallocted. The C locator can be freed
 *    at the discretion of the caller. This simplifies code in the Fortran
 *    interface wrapper.
 *    - This routine is intended to be used solely for
 *    wrapping Fortran layers from C. "Export" means to export a native
 *    C locator to Fortran.
 *    - There is no Fortran eqiuvalent to this routine.
 *    - This routine differs from the previous HDS implementation in that the address
 *    of the supplied pointer is stored in the Fortran string buffer and not the contents
 *    of the struct. This is done to constrain the required size of the locator
 *    in Fortran to allow the locator structure to change without forcing a full recompile
 *    of everything.

 *  See Also:
 *    datImportFloc

 *  Copyright:
 *    Copyright (C) 2014 Cornell University
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

  /* Validate the locator length */
  if (*status == SAI__OK && loc_length != DAT__SZLOC ) {
    *status = DAT__LOCIN;
    emsRepf( "datExportFloc", "Locator length is %d not %d", status,
            loc_length, DAT__SZLOC);
  }

  /* if everything is okay we store the pointer location in the Fortran
     locator */
  if ( *status == SAI__OK && *clocator != NULL ) {

    /* We export from C by storing the pointer of the C struct in the
       Fortran character buffer. We can not store a clone in the Fortran
       locator because clones are documented to not clone mapped status
       and DAT_MAP / DAT_UNMAP will fail for clones. We just store the
       supplied locator and null out the C version if we are being requested
       to free it. Note that if we free=false the caller should not then
       annul the locator as that would mess up the Fortran side. If the current
       scheme does not work, we could try assigning the clone to the caller and
       the original to the fortran locator but this requires some thought. */

    int n_written = one_snprintf(flocator, loc_length, "%p", status, *clocator );

    if (loc_length > 1 + n_written) {
      memset(flocator + n_written + 1, '\0', loc_length - (n_written + 1));
    }

  } else {
    strncpy( flocator, DAT__NOLOC, DAT__SZLOC );
  }

  /* Null out the caller if requested. Do not annul as we have stored
     the original pointer in the Fortran layer */
  if (free) *clocator = NULL;

  return;
}
