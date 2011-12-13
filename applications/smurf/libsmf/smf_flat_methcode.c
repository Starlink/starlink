/*
*+
*  Name:
*     smf_flat_methcode

*  Purpose:
*     Return enum value corresponding to supplied flatfield method string

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_flatmeth smf_flatmeth_string( const char * flatname, int * status );

*  Arguments:
*     flatname = const char * (Given)
*        Flatfield method to parse
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_flatmeth = Enum value corresponding to string. Status is set if
*           the string is not recognized.

*  Description:
*     This function returns a enum code corresponding to the supplied
*     flatfield method. Returns SMF__FLATMETH_NULL if the string is not recognized.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-03-09 (TIMJ):
*        Initial version

*  Notes:
*     - See also smf_flat_methstring

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
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
*     {note_any_bugs_here}
*-
*/

/* System includes */
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"

smf_flatmeth smf_flat_methcode ( const char * flatname, int * status ) {

  /* Set a default value */
  smf_flatmeth retval = SMF__FLATMETH_NULL;

  /* Check entry status */
  if (*status != SAI__OK) return retval;

  if (strcmp( flatname, "POLYNOMIAL" ) == 0) {
    retval = SMF__FLATMETH_POLY;
  } else if ( strcmp( flatname, "TABLE" ) == 0 ) {
    retval = SMF__FLATMETH_TABLE;
  } else if ( strcmp( flatname, "NULL" ) == 0 ) {
    retval = SMF__FLATMETH_NULL;
  } else {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRepf( "", "Do not recognize flatfield method: %s",
               status, flatname);
    }
  }

  return retval;
}
