#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void aryStype( const char *ftype, Ary *ary, int *status ) {
/*
*+
*  Name:
*     aryStype

*  Purpose:
*     Set a new type for an array.

*  Synopsis:
*     void aryStype( const char *ftype, Ary *ary, int *status )

*  Description:
*     This function sets a new full type for an array, causing its data
*     storage type to be changed. If the array's pixel values are
*     defined, then they will be converted from the old type to the new
*     one.  If they are undefined, then no conversion will be
*     necessary.  Subsequent enquiries will reflect the new type.
*     Conversion may be performed between any types supported by the
*     ary_ routines, including from a non-complex type to a complex
*     type (and vice versa).

*  Parameters:
*     ftype
*        The new full type specification for the array (e.g.  '_REAL'
*        or 'COMPLEX_INTEGER').
*     ary
*        Array identifier.
*     status
*        The global status.

*  Notes:
*     -  This function may only be used to change the type of a base
*     array. If it is called with an array which is not a base array,
*     then it will return without action. No error will result.
*     -  An error will result if the array, or any part of it, is
*     currently mapped for access (e.g. through another identifier).
*     -  If the type of an array is to be changed without its pixel
*     values being retained, then a call to aryReset should be made
*     beforehand. This will avoid the cost of converting all the
*     values.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
*      All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local variables: */
   AryACB *acb;               /* Array ACB */
   char type[DAT__SZTYP+1];   /* Array numeric type */
   int cmplx;                 /* Whether the array is to be complex */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check the full type for validity. */
   ary1Vftp( ftype, sizeof(type), type, &cmplx, status );

/* Import the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, 0, 1, status );

/* Check that TYPE access to the array is available. */
   ary1Chacc( acb, "TYPE", status );

/* Set the new array type. */
   ary1Stp( type, cmplx, acb, status );

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryStype: Error setting new (full) type for an array.",
              status );
      ary1Trace( "aryStype", status );
   }

}
