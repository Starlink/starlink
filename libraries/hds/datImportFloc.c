
#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <string.h>

#include "ems.h"
#include "star/mem.h"

#include "hds1.h"
#include "rec.h"
#include "dat1.h"
#include "hds_types.h"
#include "dat_err.h"

#include "hds_fortran.h"

/*
 *+
 *  Name:
 *    datImportFloc

 *  Purpose:
 *    Import a Fortran HDS locator buffer into C with malloc

 *  Invocation:
 *    datImportFloc( const char flocator[DAT__SZLOC], int loc_length, HDSLoc **clocator, int * status);

 *  Description:
 *    This function should be used to convert a Fortran HDS locator
 *    (implemented as a string buffer) to a C locator struct. The C locator
 *    is malloced by this routine. The memory will be freed when datAnnul
 *    is called. This function is also available via the
 *    HDS_IMPORT_FLOCATOR macro defined in hds_fortran.h.

 *  Arguments
 *    const char flocator[DAT__SZLOC] = Given
 *       Fortran character string buffer. Should be at least DAT__SZLOC
 *       characters long.
 *    int loc_length = Given
 *       Size of Fortran character buffer. Sanity check.
 *    HDSLoc ** clocator = Returned
 *       Fills the HDSLoc struct with the contents of the fortran buffer.
 *       Locator struct is malloced by this routine and should be freed
 *       either with dat1_free_hdsloc or dat1_export_loc. *clocator must
 *       be NULL on entry. If status is set by this routine, the struct
 *       will not be malloced on return.
 *    int *status = Given and Returned
 *       Inherited status. Attempts to excecute even if status is not SAI__OK
 *       on entry.

 *  Authors:
 *    Tim Jenness (JAC, Hawaii)
 *    David Berry (JAC, Preston)

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

 *  Notes:
 *    - A NULL pointer is returne dif the supplied locator is DAT__NOLOC.
 *    - Does not check the contents of the locator for validity.
 *    - The expectation is that this routine is used solely for C
 *      interfaces to Fortran library routines.

 *  See Also:
 *    - datExportFloc

 *  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.
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
 *     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *     MA 02111-1307, USA

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

void datImportFloc ( const char flocator[DAT__SZLOC], int loc_length, HDSLoc ** clocator, int * status) {
  int lstat;

  /* Check that we have a null pointer for HDSLoc */
  if ( *clocator != NULL ) {
    if( *status == DAT__OK ) {
       *status = DAT__WEIRD;
       emsRep( "datImportFloc", "datImportFloc: Supplied C locator is non-NULL",
      	       status);
    }
    return;
  }

/* Return the NULL pointer unchanged if the supplied locator is DAT__NOLOC. */
  if( strncmp( DAT__NOLOC, flocator, loc_length ) ) {

  /* See if we need to allocate memory for the locator struct */
  /* Allocate some memory to hold the C structure */

     *clocator = MEM_MALLOC( sizeof( struct LOC ) );

     if (*clocator == NULL ) {
       if( *status == DAT__OK ) {
          *status = DAT__NOMEM;
          emsRep( "datImportFloc", "datImportFloc: No memory for C locator struct",
                  status);
       }
       return;
     }

     /* Now import the Fortran locator - this will work even if status
        is bad on entry but it is possible for this routine to set status
        as well. We need to be able to determine whether the status was
        set bad by this routine, since that will result in garbage in the
        HDS locator. */
     emsMark();
     lstat = DAT__OK;
     dat1_import_floc( flocator, loc_length, *clocator, &lstat);
     if (lstat != DAT__OK) {
       /* free the memory and trigger a NULL pointer */
       dat1_free_hdsloc( clocator );

       /* Annul all this if status was already bad, since we do not
          want the extra meaningless messages on the stack. If status
          was good, we retain everything */
       if (*status == DAT__OK) {
         *status = lstat;
       } else {
         emsAnnul(&lstat);
       }
     }
     emsRlse();
  }

  return;
}
