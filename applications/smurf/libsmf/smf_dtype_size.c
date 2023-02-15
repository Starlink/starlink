/*
*+
*  Name:
*     smf_dtype_size

*  Purpose:
*     Return size of data type primitive given smfData in bytes

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     dim_t smf_dtype_size( const smfData * data, int * status );

*  Arguments:
*     data = const smfData* (Given)
*        Data struct to check
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     dim_t smf_dtype_size
*        Number of bytes used to represent the data type. Returns 0
*        if data type not recognized.


*  Description:
*     This function returns the number of bytes of the underlying
*     data type. Status is set to bad if the data type is not understood.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-01-24 (TIMJ):
*        Initial version.
*     2007-06-13 (EC):
*        Moved the switch on dtype to new routine smf_dtype_sz.c

*  Notes:
*     - See also smf_dtype_check, smf_dtype_string

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
#define FUNC_NAME "smf_dtype_size"

dim_t smf_dtype_size( const smfData* data, int * status ) {
  smf_dtype itype;

  /* Set a default value */
  dim_t retval = 0;

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

  itype = data->dtype;

  retval = smf_dtype_sz( itype, status );

  return retval;
}
