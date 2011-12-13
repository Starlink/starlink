/*
*+
*  Name:
*     smf_dtype_check

*  Purpose:
*     Verify that a smfData is of the correct data type

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_dtype_check( const smfData * data, const char * type,
*                          smf_dtype itype, int *status );

*  Arguments:
*     data = const smfData* (Given)
*        Data struct to check
*     type = const char* (Given)
*        Type string for comparison. Should match the NDF type strings
*        _DOUBLE, _INTEGER, _REAL, _UWORD. Can be NULL pointer if
*        itype is to be used instead.
*     itype = smf_dtype (Given)
*        Numerical constant form of the data type (as defined in
*        smf_typ.h). eg SMF__INTEGER, SMF__DOUBLE. These types are
*        used instead of the type string if the string is a NULL
*        pointer.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     int = True if the data type is as specified. Otherwise false.

*  Description:
*     This function compares the data type of the struct with a string
*     form. Returns true if the types are the same, else returns false.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-01-24 (TIMJ):
*        Initial version.

*  Notes:
*     - See also smf_dtype_size

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
#define FUNC_NAME "smf_dtype_check"

int smf_dtype_check(const smfData* data, const char * type, smf_dtype itype,
		    int * status ) {

  /* Set a default value */
  int retval = 0;
  const char * this = NULL;

  /* Check entry status */
  if (*status != SAI__OK) return retval;

  /* check that we have a smfData */
  if ( data == NULL ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME
	    ": Supplied smfData is a NULL pointer. Possible programming error.",
	    status);
    return 0;
  }

  if (type != NULL) {
  /* now get the string version */
    this = smf_dtype_string( data, status );
    if (*status == SAI__OK &&
	this != NULL &&
	strncmp(this, type, strlen(this)) == 0) {
      retval = 1;
    }
  } else {
    if (data->dtype == itype) {
      retval = 1;
    }
  }

  return retval;
}
