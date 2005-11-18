
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
 *    Import a fortran HDS locator buffer into C with malloc

 *  Invocation:
 *    dat1_import_floc( char flocator[DAT__SZLOC], int len, HDSLoc **clocator, int * status);

 *  Description:
 *    This function should be used to convert a Fortran HDS locator 
 *    (implemented as a string buffer) to a C locator struct. The C locator
 *    is malloced by this routine.

 *  Arguments
 *    char flocator[DAT__SZLOC] = Given
 *       Fortran character string buffer. Should be at least DAT__SZLOC
 *       characters long.
 *    int len = Given
 *       Size of Fortran character buffer. Sanity check.
 *    HDSLoc ** clocator = Returned
 *       Fills the HDSLoc struct with the contents of the fortran buffer.
 *       Locator struct is malloced by this routine and should be freed
 *       either with dat1_free_hdsloc or dat1_export_loc. *clocator must
 *       be NULL on entry. If status is set by this routine, the struct
 *       will not be malloced on return.
 *    int *status = Given and Returned
 *       Inherited status. Returns without action if status is not DAT__OK

 *  Authors:
 *    Tim Jenness (JAC, Hawaii)

 *  History:
 *    16-NOV-2005 (TIMJ):
 *      Initial version

 *  Notes:
 *    Does not check the contents of the locator for validity.

 *  See Also:
 *    - dat1_import_floc
 *    - dat1_export_floc
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
 *     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *     MA 02111-1307, USA

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

void dat1_impalloc_floc ( char flocator[DAT__SZLOC], int loc_length, HDSLoc ** clocator, int * status) {

  if (*status != DAT__OK) return;

  /* Check that we have a null pointer for HDSLoc */
  if ( *clocator != NULL ) {
    *status = DAT__WEIRD;
    emsRep( "DAT1_IMPALLOC_FLOC", "Supplied C locator is non-NULL", status);
    return;
  }

  /* See if we need to allocate memory for the locator struct */
  /* Allocate some memory to hold the C structure */

  *clocator = malloc( sizeof( struct LOC ) );

  if (*clocator == NULL ) {
    *status = DAT__NOMEM;
    emsRep( "DAT1_IMPORT_FLOC", "No memory for C locator struct", status);
    return;
  }

  /* Now import the Fortran locator */
  dat1_import_floc( flocator, loc_length, *clocator, status);

  /* Clean up on error */
  if ( *status != DAT__OK ) {
    dat1_free_hdsloc( clocator );
  }

  return;
}
