/*
*+
*  Name:
*     smf_dtype_fromstring

*  Purpose:
*     Return integer constant corresponding to string

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     type = smf_dtype_fromstring( const char * type, int * status );

*  Arguments:
*     data = const char * type (Given)
*        String representation of the data type. "_DOUBLE", "_INTEGER",
*        "_UWORD" etc.
*     status = int* (Given and Returned)
*        Pointer to global status. Status is set to SMF__BDTYP if the
*        type is not recognized.

*  Return Value:
*     itype = smf_dtype
*        Internal numeric constant corresponding to the supplied type.
*        Returns SMF__NULL if type is unknown.

*  Description:
*     This function returns the internal integer constant corresponding
*     to the given string. The string must be in the standard HDS format
*     (e.g. _DOUBLE for "double").

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-01-26 (TIMJ):
*        Initial version.

*  Notes:
*     - See also smf_dtype_string for the reverse.

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
#define FUNC_NAME "smf_dtype_fromstring"

smf_dtype
smf_dtype_fromstring( const char * dtype, int * status ) {
  smf_dtype itype = SMF__NULL;
  if (*status != SAI__OK) return itype;
  if (!dtype) return itype;

  if ( strncmp(dtype, "_DOUBLE", 7 ) == 0 ){
    itype = SMF__DOUBLE;
  } else if ( strncmp(dtype, "_REAL", 5 ) == 0 ) {
    itype = SMF__FLOAT;
  } else if ( strncmp(dtype, "_INTEGER", 8 ) == 0 ){
    itype = SMF__INTEGER;
  } else if ( strncmp(dtype, "_UWORD", 6 ) == 0 ) {
    itype = SMF__USHORT;
  } else {
    if ( *status == SAI__OK) {
      *status = SMF__BDTYP;
      msgSetc( "TYP", dtype);
      errRep( FUNC_NAME, "Data Type, '^TYP', is not supported",status);
    }
  }

  return itype;
}
