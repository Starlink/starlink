
#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <string.h>

#include "ems.h"
#include "sae_par.h"

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
 *    clocator = dat1_import_floc( const char flocator[DAT__SZLOC], int len, int * status);

 *  Description:
 *    This function should be used to convert a Fortran HDS locator
 *    (implemented as a string buffer) to a C locator struct.

 *  Arguments
 *    flocator = const char * (Given)
 *       Fortran character string buffer. Should be at least DAT__SZLOC
 *       characters long.
 *    len = int (Given)
 *       Size of Fortran character buffer. Sanity check.
 *    status = int * (Given and Returned)
 *       Inherited status. Attempts to execute even if status is not SAI__OK
 *       on entry.

 *  Returned Value:
 *    clocator = HDSLoc *
 *       C HDS locator corresponding to the Fortran locator.
 *       Should be freed by using datAnnul().

 *  Authors:
 *    Tim Jenness (JAC, Hawaii)
 *    David Berry (JAC, Preston)

 *  History:
 *    16-NOV-2005 (TIMJ):
 *      Initial version
 *    27-JAN-2006 (DSB)
 *      Attempt to execute even if status is set on entry.
 *    2014-09-07 (TIMJ):
 *      Complete rewrite to now extract pointer value from buffer. API change
 *      to return the pointer directly.

 *  Notes:
 *    - Does not check the contents of the locator for validity but does check for
 *      common Fortran error locators such as DAT__ROOT and DAT__NOLOC.
 *    - For internal usage by HDS only. The public version is datImportFloc.
 *    - Differs from the original HDS API in that it returns the locator.
 *    - Attempts to execute even if status is bad.
 *    - This change has been made to allow the struct to be extended without forcing
 *      a recompile when DAT__SZLOC changes.

 *  See Also:
 *    - datImportFloc
 *    - datExportFloc

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

HDSLoc *
dat1_import_floc ( const char flocator[DAT__SZLOC], int loc_length, int * status) {

  long ptr_as_long = 0;
  HDSLoc * clocator = NULL;

  /* Validate the locator length. */
  if (loc_length != DAT__SZLOC ) {
    if (*status == SAI__OK ) {
       *status = DAT__LOCIN;
       emsRepf( "DAT1_IMPORT_FLOC", "Locator length is %d not %d", status,
                loc_length, DAT__SZLOC);
    }
    return NULL;
  };

  /* Check obvious error conditions */
  if (strncmp( DAT__ROOT, flocator, loc_length) == 0 ){
    if( *status == SAI__OK ) {
       *status = DAT__LOCIN;
       emsRep( "dat1ImportFloc_ROOT",
               "Input HDS Locator corresponds to DAT__ROOT but that can only be used from NDF",
               status );
    }
    return NULL;
  }

  /* Check obvious error conditions */
  if (strncmp( DAT__NOLOC, flocator, loc_length) == 0 ){
    if( *status == SAI__OK ) {
       *status = DAT__LOCIN;
       emsRep( "datImportFloc_NOLOC",
               "Input HDS Locator corresponds to DAT__NOLOC but status is good (Possible programming error)",
               status );
    }
    return NULL;
  }

  /* Everything seems to be okay so now convert the string buffer to the
     required pointer. We ignore status as sometimes we need to try
     to get the value regardless (otherwise DAT_ANNUL from Fortran would
     never succeed). */

  ptr_as_long = strtol( flocator, NULL, 16 );

  if (ptr_as_long == 0) {
    /* This should not have happened */
    if (*status == SAI__OK) {
      *status = DAT__LOCIN;
      emsRep("dat1_import_floc_3",
             "Error importing locator from Fortran", status );
      return NULL;
    }
  }

  /* Do the cast */
  clocator = (HDSLoc *)ptr_as_long;
  return clocator;
}
