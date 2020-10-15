/*
*+
*  Name:
*     smf_dtype_sz

*  Purpose:
*     Return size of data type primitive given smf_dtype in bytes

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     dim_t smf_dtype_sz( const smf_dtype dtype, int * status );

*  Arguments:
*     dtype = const smf_dtype (Given)
*        Data stype to check
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
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-06-13 (EC):
*        Initial version factors code out from smf_dtype_size.c
*     2008-02-01 (EC):
*        Added SMF__UBYTE

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

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_dtype_sz"

dim_t smf_dtype_sz( const smf_dtype dtype, int *status ) {

  dim_t retval = 0;

  if (*status != SAI__OK) return retval;

  /* now switch on data type */
  switch( dtype ) {
  case SMF__INTEGER:
    retval = sizeof(int);
    break;
  case SMF__FLOAT:
    retval = sizeof(float);
    break;
  case SMF__DOUBLE:
    retval = sizeof(double);
    break;
  case SMF__USHORT:
    retval = sizeof(unsigned short);
    break;
  case SMF__UBYTE:
    retval = sizeof(unsigned char);
    break;
  default:
    retval = 0;
    *status = SMF__BDTYP;
    msgSeti( "TC", dtype );
    errRep(FUNC_NAME, "Unable to determine size of datatype. Data typecode was ^TC", status );
  }

  return retval;
}
