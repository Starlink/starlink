#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"

void ary1Sft( int nshift, const hdsdim *shift, AryACB *acb, int *status ) {
/*
*+
*  Name:
*     ary1Sft

*  Purpose:
*     Apply pixel index shifts to an array entry in the ACB.

*  Synopsis:
*     void ary1Sft( int nshift, const hdsdim *shift, AryACB *acb, int *status )

*  Description:
*     This function applies a set of pixel index shifts to an array
*     identified by its entry in the ACB. An integer shift is applied
*     to each dimension so that the array maintains the same data
*     content, although its bounds (and the indices of each pixel)
*     change by the amount of the shift applied to the corresponding
*     dimension. If the array is not a base array, then only one entry
*     in the ACB is affected. If it is a base array, then the shifts
*     are applied to the actual data object and all ACB entries which
*     refer to that object are also updated to reflect the change.

*  Parameters:
*     nshift
*        Number of dimensions to which shifts are to be applied. If
*        more shifts are specified than there are dimensions in the
*        array, then the excess shifts are disregarded. If fewer shifts
*        are specified, then the extra dimensions are not shifted.
*     shift
*        Array holding the of shifts to be applied to each dimension.
*     acb
*        The array ACB.
*     status
*        The global status.

*  Notes:
*     -  Note that applying a shift to a base array affects the pixel
*     indices of all other direct references to the same base array,
*     but does not affect the pixel indices of cuts derived from it
*     (these remain unchanged as regards both data content and pixel
*     indices).
*     -  Applying a shift to a cut always affects that array only.

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
   AryACB *acbt;              /* ACB entry to test */
   AryDCB *dcb;               /* DCB */
   int i;                     /* Loop counter for dimensions */
   int iacbt;                 /* Index of test ACB */
   int n;                     /* Number of axes to use */
   int next;                  /* Index of next ACB entry */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If the array is a cut, then check to see if it is mapped for access. No
   pixel shifts can be applied if it is, so report an error. */
   if( acb->cut ){
      if( acb->mcb ){
         *status = ARY__ISMAP;
         dcb = acb->dcb;
         datMsg( "ARRAY", dcb->loc );
         errRep( " ", "The array ^ARRAY is currently mapped for access through"
                 " the specified identifier (possible programming error).",
                 status );

/* If it is not mapped, then apply the pixel index shifts to the array
   bounds and to the accumulated pixel shifts stored in the ACB. */
      } else {
         n = ( acb->ndim < nshift ) ? acb->ndim : nshift;
         for( i = 0; i < n; i++ ) {
            acb->lbnd[ i ] += shift[ i ];
            acb->ubnd[ i ] += shift[ i ];
            acb->shift[ i ] += shift[ i ];
         }
      }

/* If the array is a base array, then check to see if any mapped access to
   it is currently in effect. If so, then pixel index shifts cannot be
   applied, so report an error. */
   } else {
      dcb = acb->dcb;
      if( ( dcb->nread != 0 ) ||( dcb->nwrite != 0 ) ){
         *status = ARY__ISMAP;
         datMsg( "ARRAY", dcb->loc );
         errRep( " ", "The base array '^ARRAY' is currently mapped for access,"
                 " perhaps through another identifier (possible programming"
                 " error).", status );

/* Apply the pixel index shifts to the data object. */
      } else {
         ary1Dsft( nshift, shift, dcb, status );

/* Loop through all the ACB entries. We lock a mutex first to ensure that
   no other thread is currently accessing the slot array. */
         ARY__ACB_LOCK_MUTEX;
         iacbt = -1;
         next = 0;
         while( 1 ) {
            acbt = ary1Nxtsl( ARY__ACBTYPE, iacbt, &next, status );
            if( ( *status == SAI__OK ) && ( next != -1 ) ){

/* Select those entries which refer to the data object being shifted and
   which describe a base array. */
               iacbt = next;
               if( acbt->dcb == dcb && !acbt->cut  ){

/* Apply the pixel index shifts to the array bounds and to the accumulated
   pixel shifts stored in the ACB. */
                  n = ( acbt->ndim < nshift ) ? acbt->ndim : nshift;
                  for( i = 0; i < n; i++ ) {
                     acbt->lbnd[ i ] += shift[ i ];
                     acbt->ubnd[ i ] += shift[ i ];
                     acbt->shift[ i ] += shift[ i ];
                  }
               }

            } else {
               break;
            }
         }
         ARY__ACB_UNLOCK_MUTEX;
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Sft", status );

}
