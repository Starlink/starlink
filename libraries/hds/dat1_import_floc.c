
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
 *    dat1_import_floc

 *  Purpose:
 *    Import a fortran HDS locator buffer into C

 *  Invocation:
 *    dat1_import_floc( const char flocator[DAT__SZLOC], int len, HDSLoc *clocator, int * status);

 *  Description:
 *    This function should be used to convert a Fortran HDS locator
 *    (implemented as a string buffer) to a C locator struct. It is
 *     for internal usage by HDS only. The public version is datImportFloc.

 *  Arguments
 *    char flocator[DAT__SZLOC] = Given
 *       Fortran character string buffer. Should be at least DAT__SZLOC
 *       characters long.
 *    int len = Given
 *       Size of Fortran character buffer. Sanity check.
 *    HDSLoc ** clocator = Returned
 *       Fills the HDSLoc struct with the contents of the fortran buffer.
 *       The C struct will not be malloced by this routine.
 *    int *status = Given and Returned
 *       Inherited status. Attempts to execute even if status is not DAT__OK
 *       on entry.

 *  Returned:
 *    int status = Inherited status value

 *  Authors:
 *    Tim Jenness (JAC, Hawaii)
 *    David Berry (JAC, Preston)

 *  History:
 *    16-NOV-2005 (TIMJ):
 *      Initial version
 *    27-JAN-2006 (DSB)
 *      Attempt to execute even if status is set on entry.

 *  Notes:
 *    Does not check the contents of the locator for validity but does check for
 *    common Fortran error locators such as DAT__ROOT and DAT__NOLOC.

 *  See Also:
 *    - datImportFloc
 *    - datExportFloc
 *    - dat1_free_hdsloc

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

int dat1_import_floc ( const char flocator[DAT__SZLOC], int loc_length, HDSLoc * clocator, int * status) {

/* Validate the locator length.                                             */
  if (loc_length != DAT__SZLOC ) {
    if (*status == DAT__OK ) {
       *status = DAT__LOCIN;
       emsSeti( "LEN", loc_length );
       emsSeti( "SZLOC", DAT__SZLOC );
       emsRep( "DAT1_IMPORT_FLOC", "Locator length is ^LEN not ^SZLOC", status);
    }
    return *status;
  };

  /* Check obvious error conditions */
  if (strncmp( DAT__ROOT, flocator, loc_length) == 0 ){
    if( *status == DAT__OK ) {
       *status = DAT__LOCIN;
       emsRep( "datImportFloc_ROOT", "Input HDS Locator corresponds to DAT__ROOT but that can only be used from NDF", status );
    }
    return *status;
  }

  /* Check obvious error conditions */
  if (strncmp( DAT__NOLOC, flocator, loc_length) == 0 ){
    if( *status == DAT__OK ) {
       *status = DAT__LOCIN;
       emsRep( "datImportFloc_NOLOC", "Input HDS Locator corresponds to DAT__NOLOC but status is good (Possible programming error)", status );
    }
    return *status;
  }

/* If OK, then extract the information from the locator string (necessary   */
/* to ensure that data alignment is correct, as the string will normally be */
/* stored externally in a Fortran CHARACTER variable).                      */
/* We do this copy regardless of status since this is sometimes required
   and it can't hurt if we have allocated memory                            */
  memmove( clocator, flocator, sizeof( struct LOC ) );

  return *status;
}
