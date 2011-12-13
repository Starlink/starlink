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
*     const char * smf_dtype_string( const smfData * data, int * status );

*  Arguments:
*     data = const smfData* (Given)
*        Data struct to examine
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     const char* = Pointer to constant string corresponding to data type.
*                   Can be NULL pointer if type unknown.

*  Description:
*     This function returns a string representation of the underlying
*     data type associated with the smfData struct. Returns NULL pointer
*     if the data type is not recognized.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-01-24 (TIMJ):
*        Initial version.
*     2008-07-16 (TIMJ):
*        Call smf_dtype_str

*  Notes:
*     - See also smf_dtype_check, smf_dtype_str

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

const char * smf_dtype_string( const smfData* data, int * status ) {

  /* Set a default value */
  const char * retval = NULL;

  /* Check entry status */
  if (*status != SAI__OK) return retval;

  /* check that we have a smfData */
  if ( data == NULL ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME,
	    "Supplied smfData is a NULL pointer. Possible programming error.",
	    status);
    return retval;
  }

  retval = smf_dtype_str( data->dtype, status );
  return retval;
}
