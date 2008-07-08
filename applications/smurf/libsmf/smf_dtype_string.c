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
*     const char * smf_dtype_check( const smfData * data, int * status );

*  Arguments:
*     data = const smfData* (Given)
*        Data struct to check
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

*  Notes:
*     - See also smf_dtype_check

*  Copyright:
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

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
  char * retval = NULL;
  
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

  /* now switch on data type */
  switch( data->dtype ) {
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
  default:
    retval = NULL;
  }

  return retval;
}
