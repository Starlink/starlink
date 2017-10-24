#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void arySbnd( int ndim, const hdsdim *lbnd, const hdsdim *ubnd, Ary *ary,
              int *status ) {
/*
*+
*  Name:
*     arySbnd

*  Purpose:
*     Set new pixel-index bounds for an array.

*  Synopsis:
*     void arySbnd( int ndim, const hdsdim *lbnd, const hdsdim *ubnd,
*                   Ary *ary, int *status )

*  Description:
*     This function sets new pixel-index bounds for an array (or array
*     section). The number of array dimensions may also be changed.  If
*     a base array is specified, then a permanent change is made to the
*     actual data object and this will be apparent through any other
*     array identifiers which refer to it.  However, if an identifier
*     for an array section is specified, then its bounds are altered
*     without affecting other arrays.

*  Parameters:
*     ndim
*        New number of array dimensions.
*     lbnd
*        Array holding the new lower pixel-index bounds of the array.
*     ubnd
*        Array holding the new upper pixel-index bounds of the array.
*     ary
*        Array identifier.
*     status
*        The global status.

*  Notes:
*     -  The bounds of an array section cannot be altered while it is
*     mapped for access through the identifier supplied to this
*     routine.
*     -  The bounds of a base array cannot be altered while any part of
*     it is mapped for access (i.e. even through another identifier).
*     -  The array's pixel values (if defined) will be retained for
*     those pixels which lie within both the old and new bounds. Any
*     pixels lying outside the new bounds will be lost (and cannot
*     later be recovered by further changes to the array's bounds).
*     Any new pixels introduced where the new bounds extend beyond the
*     old ones will be assigned the "bad" value, and the subsequent
*     value of the bad-pixel flag will reflect this.
*     -  If the bounds of a base array are to be altered and retention
*     of the existing pixel values is not required, then a call to
*     aryReset should be made before calling this routine. This will
*     eliminate any processing which might otherwise be needed to
*     retain the existing values. This step is not necessary with an
*     array section, where no processing of pixel values takes place.

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
   AryACB *acb;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check the new bounds for validity. */
   ary1Vbnd( ndim, lbnd, ubnd, status );

/* Import the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, 0, 1, status );

/* Check that BOUNDS access to the array is available. */
   ary1Chacc( acb, "BOUNDS", status );

/* Set the new bounds. */
   if( *status == SAI__OK ){
      ary1Sbnd( ndim, lbnd, ubnd, acb, status );
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "arySbnd: Error setting new pixel-index bounds for an "
              "array.", status );
      ary1Trace( "arySbnd", status );
   }

}
