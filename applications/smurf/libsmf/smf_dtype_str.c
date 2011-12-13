/*
*+
*  Name:
*     smf_dtype_string

*  Purpose:
*     Return string representation of data type

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     const char * smf_dtype_str( smf_dtype type, int * status );

*  Arguments:
*     type = smf_dtype (Given)
*        Type code to convert to string.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     const char* = Pointer to constant string corresponding to data type.
*                   Can be NULL pointer if type unknown.

*  Description:
*     This function returns a string representation of the underlying
*     data type. Returns NULL pointer if the data type is not recognized.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-07-16 (TIMJ):
*        Initial version, copied from smf_dtype_string. Add _UBYTE

*  Notes:
*     - See also smf_dtype_string

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
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
#include "ast.h"
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_dtype_string"

const char * smf_dtype_str( smf_dtype type, int * status ) {

  /* Set a default value */
  const char * retval = NULL;

  /* Check entry status */
  if (*status != SAI__OK) return retval;

  /* now switch on data type */
  switch( type ) {
  case SMF__NULL:
    retval = NULL;
    break;
  case SMF__INTEGER:
    retval = "_INTEGER";
    break;
  case SMF__FLOAT:
    retval = "_REAL";
    break;
  case SMF__DOUBLE:
    retval = "_DOUBLE";
    break;
  case SMF__USHORT:
    retval = "_UWORD";
    break;
  case SMF__UBYTE:
    retval = "_UBYTE";
    break;
  default:
    retval = NULL;
  }

  return retval;
}
