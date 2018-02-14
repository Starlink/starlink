
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

#include "hds_fortran.h"

/*
 *+
 *  Name:
 *    datImportFloc

 *  Purpose:
 *    Import a Fortran HDS locator buffer into C

 *  Invocation:
 *    datImportFloc( const char flocator[DAT__SZLOC], int loc_length, HDSLoc **clocator, int * status);

 *  Description:
 *    This function should be used to convert a Fortran HDS locator
 *    (implemented as a string buffer) to a C locator struct.
 *    This function is also available via the
 *    HDS_IMPORT_FLOCATOR macro defined in hds_fortran.h.

 *  Arguments
 *    flocator = const char [DAT__SZLOC] (Given)
 *       Fortran character string buffer. Should be at least DAT__SZLOC
 *       characters long.
 *    loc_length = int (Given)
 *       Size of Fortran character buffer. Sanity check.
 *       Should be DAT__SZLOC.
 *    clocator = HDSLoc ** (Returned)
 *       Assigned to the C version of the Fortran locator.
 *       Must be NULL on entry. Memory is not allocated by this routine
 *       and the locator must not be annulled whilst the Fortran
 *       locator is still being used.
 *    status = int * (Given and Returned)
 *       Inherited status. Attempts to excecute even if status is not SAI__OK
 *       on entry.

 *  Authors:
 *    Tim Jenness (JAC, Hawaii)
 *    David Berry (JAC, Preston)
 *    Tim Jenness (Cornell)

 *  History:
 *    16-NOV-2005 (TIMJ):
 *      Initial version
 *    18-NOV-2004 (TIMJ):
 *      Rename from dat1_import_floc so that it can be made public for
 *      fortran wrappers.
 *    27-JAN-2006 (DSB):
 *      Attempt to execute even if status is set on entry.
 *    27-JAN-2006 (TIMJ):
 *      Slight reworking of DSB's patch, to allow the memory
 *      to be freed if dat1_import_floc failed to copy anything
 *      into the empty struct. Otherwise the uninitialised struct
 *      can cause real problems later on.
 *    23-FEB-2006 (TIMJ):
 *      Use starmem
 *    22-OCT-2010 (DSB):
 *      Return a NULL pointer without error if the supplied pointer is
 *      DAT__NOLOC.
 *    2014-09-07 (TIMJ):
 *      Complete rewrite. No long allocate and memcpy, just extract the
 *      pointer directly from the buffer.

 *  Notes:
 *    - A NULL pointer is returned if the supplied locator is DAT__NOLOC.
 *    - Does check the contents of the locator for validity to avoid
 *      uncertainty when receiving uninitialized memory.
 *    - The expectation is that this routine is used solely for C
 *      interfaces to Fortran library routines.
 *    - The locator returned by this routine has not been allocated
 *      independently. It is the same locator tracked by the Fortran
 *      layer. Do not annul this locator if the Fortran layer is still
 *      tracking it.

 *  See Also:
 *    - datExportFloc

 *  Copyright:
 *    Copyright (C) 2014 Cornell University.
 *    Copyright (C) 2010 Science & Technology Facilities Council.
 *    Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
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

void datImportFloc ( const char flocator[DAT__SZLOC], int loc_length, HDSLoc ** clocator, int * status) {
  int lstat;

  /* Check that we have a null pointer for HDSLoc */
  if ( *clocator != NULL ) {
    if( *status == SAI__OK ) {
       *status = DAT__WEIRD;
       emsRep( "datImportFloc", "datImportFloc: Supplied C locator is non-NULL",
      	       status);
    }
    return;
  }

  /* For these HDS/HDF5 locators that are either hex pointers or "<xxx_xxx>" strings
     we can validate them immediately */
  if (flocator[0] != '0' && flocator[0] != '<') {
    if (*status == SAI__OK) {
      char flocstr[DAT__SZLOC+1];
      *status = DAT__WEIRD;
      memmove( flocstr, flocator, DAT__SZLOC);
      emsRepf( "datImportFloc_2",
               "datImportFloc: Supplied Fortran locator looks to be corrupt: '%s'",
               status, flocstr );
    }
    return;
  }

  /* Return the NULL pointer unchanged if the supplied locator is DAT__NOLOC. */
  if( strncmp( DAT__NOLOC, flocator, loc_length ) ) {

     /* Now import the Fortran locator - this will work even if status
        is bad on entry but it is possible for this routine to set status
        as well. We need to be able to determine whether the status was
        set bad by this routine, since that will result in garbage in the
        HDS locator. */
     emsMark();
     lstat = SAI__OK;
     *clocator = dat1_import_floc( flocator, loc_length, &lstat);
     if (lstat != SAI__OK) {

       /* Annul all this if status was already bad, since we do not
          want the extra meaningless messages on the stack. If status
          was good, we retain everything */
       if (*status == SAI__OK) {
         *status = lstat;
       } else {
         emsAnnul(&lstat);
       }
     }
     emsRlse();
  }

  return;
}
