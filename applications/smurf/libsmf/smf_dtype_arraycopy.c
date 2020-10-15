/*
*+
*  Name:
*     smf_dtype_arraycopy

*  Purpose:
*     Copies an input to output array with type conversion and VAL__BAD
*     checking.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_dtype_arraycopy( void *ary_out, const smf_dtype dtype_out,
*                               void *ary_in, const smf_dtype dtype_in,
*                               dim_t nelem, int *status );

*  Arguments:
*     ary_out = void * ( Given and Returned )
*        Pointer to output array
*     dtype_out = const smf_dtype (Given)
*        Data type to convert to
*     ary_in  = void * ( Given )
*        Pointer to input array
*     dtype_in = const smf_dtype (Given)
*        Data type to convert from
*     nelem = dim_t ( Given )
*        Number of elements to copy
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function copies the data from an array of one datatype to
*     an array of another datatype. VAL__BADx values in the input
*     array will result in bad values in the output data array.
*     Status is set to bad if the type conversion can not be done.

*  Authors:
*     Remo Tilanus (JAC)
*     {enter_new_authors_here}

*  History:
*     2011-02-16 (RPT):
*        Initial version

*  Notes:

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

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_dtype_arraycopy"

void smf_dtype_arraycopy( void *ary_out, const smf_dtype dtype_out,
                          void *ary_in, const smf_dtype dtype_in,
                          dim_t nelem, int *status )
{

  double         *ary_double;        /* Pointer to a double array used for
                                        up-conversion */
  float          *ary_float;         /* Pointer to real array */
  int            *ary_int;           /* Pointer to integer array */
  unsigned short *ary_ushort;         /* Pointer to unsigned short array */
  unsigned char  *ary_ubyte;         /* Pointer to unsigned byte array */


  if (*status != SAI__OK) return;


  msgOutiff( MSG__DEBUG, " ",
	     "Copying and converting data from type %d to %d",
	     status, dtype_in, dtype_out );


  /* Simple solution if both are the same */
  if ( dtype_out == dtype_in ) {
    int dsize = (int) smf_dtype_sz( dtype_out, status );
    if ( *status != SAI__OK) return;
    memcpy( ary_out, ary_in, nelem * dsize );
    return;
  }

  /* Set up intermediate array of output type is not a double */
  if ( dtype_out != SMF__DOUBLE ) {
    ary_double = astMalloc( nelem*sizeof( *ary_double ) );
  } else {
    ary_double = ary_out;
  }


  /* UP-CONVERT TO DOUBLES */

  /* Initialize output array to BAD values */
  for ( dim_t i = 0; i < nelem; i++ ) {
    ary_double[i] = VAL__BADD;
  }

  if ( *status != SAI__OK) return;

  switch( dtype_in ) {
  case SMF__FLOAT:
    ary_float = ary_in;
    for ( dim_t i = 0; i < nelem; i++ ) {
      if ( ary_float[i] != VAL__BADR )
	ary_double[i] = (double) ary_float[i];
    }
    break;
  case SMF__INTEGER:
    ary_int = ary_in;
    for ( dim_t i = 0; i < nelem; i++ ) {
      if ( ary_int[i] != VAL__BADI )
	ary_double[i] = (double) ary_int[i];
    }
    break;
  case SMF__USHORT:
    ary_ushort = ary_in;
    for ( dim_t i = 0; i < nelem; i++ ) {
      if ( ary_ushort[i] != VAL__BADUW )
	ary_double[i] = (double) ary_ushort[i];
    }
    break;
  case SMF__UBYTE:
    ary_ubyte = ary_in;
    for ( dim_t i = 0; i < nelem; i++ ) {
      if ( ary_ubyte[i] != VAL__BADUB )
	ary_double[i] = (double) ary_ubyte[i];
    }
    break;
  default:
    *status = SMF__BDTYP;
    msgSeti( "TC", dtype_in );
    errRep(FUNC_NAME,
	   "Input datatype not implemented. Data typecode was ^TC",
	   status );
  }


  /* We are done if the output data type was doubles */
  if ( dtype_out == SMF__DOUBLE ) return;


  /* DOWN-CONVERT TO DATATYPE */

  if ( *status != SAI__OK) return;

  switch( dtype_out ) {
  case SMF__FLOAT:
    ary_float = ary_out;
    for ( dim_t i = 0; i < nelem; i++ ) {
      if ( ary_double[i] != VAL__BADD ) {
	ary_float[i] = (float) ary_double[i];
      } else {
	ary_float[i] = VAL__BADR;
      }
    }
    break;
  case SMF__INTEGER:
    ary_int = ary_out;
    for ( dim_t i = 0; i < nelem; i++ ) {
      if ( ary_double[i] != VAL__BADD ) {
	ary_int[i] = (int) NINT(ary_double[i]);
      } else {
	ary_int[i] = VAL__BADI;
      }
    }
    break;
  case SMF__USHORT:
    ary_ushort = ary_out;
    for ( dim_t i = 0; i < nelem; i++ ) {
      if ( ary_double[i] != VAL__BADD ) {
	ary_ushort[i] = (unsigned short) NINT( fabs(ary_double[i]) );
      } else {
	ary_ushort[i] = VAL__BADUW;
      }
    }
    break;
  case SMF__UBYTE:
    ary_ubyte = ary_out;
    for ( dim_t i = 0; i < nelem; i++ ) {
      if ( ary_double[i] != VAL__BADD ) {
	ary_ubyte[i] = (unsigned char) NINT( fabs(ary_double[i]) );
      } else {
	ary_ubyte[i] = VAL__BADUB;
      }
    }
    break;
  default:
    *status = SMF__BDTYP;
    msgSeti( "TC", dtype_out );
    errRep(FUNC_NAME,
	   "Output datatype not implemented. Data typecode was ^TC",
	   status );
  }

  /* Free memory */
  ary_double = astFree( ary_double);
}
