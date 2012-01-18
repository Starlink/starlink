/*
*+
*  Name:
*     smf_flat_methstring

*  Purpose:
*     Return string representation of flatfield method

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     const char * smf_flatmeth_string( smf_flatmeth flatmeth, int * status );

*  Arguments:
*     flatmeth = smf_flatmeth (Given)
*        Flatfield method to convert to string.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     const char* = Pointer to constant string corresponding to flatfield method.
*                   Can be NULL pointer if type unknown.

*  Description:
*     This function returns a string representation of the
*     flatfield method. Returns NULL pointer if the data type is not recognized.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-03-09 (TIMJ):
*        Initial version, copied from smf_dtype_str

*  Notes:
*     - See also smf_dtype_str

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

const char * smf_flat_methstring( smf_flatmeth flatmeth, int * status ) {

  /* Set a default value */
  const char * retval = NULL;

  /* Check entry status */
  if (*status != SAI__OK) return retval;

  /* now switch on data type */
  switch( flatmeth ) {
  case SMF__FLATMETH_NULL:
    retval = "NULL";
    break;
  case SMF__FLATMETH_TABLE:
    retval = "TABLE";
    break;
  case SMF__FLATMETH_POLY:
    retval = "POLYNOMIAL";
    break;
  }

  return retval;
}
