#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"
#include "dat_par.h"

void aryShift( int nshift, const hdsdim *shift, Ary *ary, int *status ) {
/*
*+
*  Name:
*     aryShift

*  Purpose:
*     Apply pixel-index shifts to an array.

*  Synopsis:
*     void aryShift( int nshift, const hdsdim *shift, Ary *ary, int *status )

*  Description:
*     This function applies pixel-index shifts to an array. An integer
*     shift is applied to each dimension so that its pixel-index
*     bounds, and the indices of each pixel, change by the amount of
*     shift applied to the corresponding dimension. The array's pixels
*     retain their values and none are lost.

*  Parameters:
*     nshift
*        Number of dimensions to which shifts are to be applied (i.e. the
*        length of array "shift"). This must not exceed the number of array
*        dimensions. If fewer shifts are supplied than there are dimensions
*        in the array, then the extra dimensions will not be shifted.
*     shift
*        An array holding the pixel-index shifts to be applied to each
*        dimension.
*     ary
*        Array identifier.
*     status
*        The global status.

*  Notes:
*     -  Pixel-index shifts applied to a base array will affect the
*     appearance of that array as seen by all base-array identifiers
*     associated with it. However, array sections derived from that
*     base array will remain unchanged (as regards both pixel-indices
*     and data content).
*     -  Pixel-index shifts cannot be applied to a base array while any
*     part of it is mapped for access (i.e. even through another
*     identifier).
*     -  Pixel-index shifts applied to an array section only affect
*     that section itself, and have no effect on other array
*     identifiers.
*     -  Pixel-index shifts cannot be applied to an array section while
*     it is mapped for access through the identifier supplied to this
*     routine.

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

/* If the number of shifts specified is not positive, then report an error. */
   if( nshift <= 0 ){
      *status = ARY__SFTIN;
      msgSeti( "BADNSFT", nshift );
      errRep( " ", "Invalid number of shifts (^BADNSFT) specified (possible "
              "programming error).", status );

/* Import the array identifier. */
   } else {
      acb = (AryACB *) ary1Impid( ary, 1, 0, 1, status );
      if( *status == SAI__OK ){

/* Check that the number of shifts does not exceed the number of array
   dimensions and report an error if it does. */
         if( nshift > acb->ndim ){
            *status = ARY__SFTIN;
            msgSeti( "BADNSFT", nshift );
            msgSeti( "NDIM", acb->ndim );
            errRep( " ", "Number of shifts specified (^BADNSFT) exceeds the "
                    "number of array dimensions (^NDIM) (possible "
                    "programming error).", status );

/* Check that SHIFT access to the array is available. */
         } else {
            ARY__DCB_LOCK_MUTEX;

            ary1Chacc( acb, "SHIFT", status );

/* Apply the shifts to the array's ACB entry. */
            ary1Sft( nshift, shift, acb, status );

            ARY__DCB_UNLOCK_MUTEX;
         }
      }
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryShift: Error applying pixel-index shifts to an array.",
              status );
      ary1Trace( "aryShift", status );
   }

}
