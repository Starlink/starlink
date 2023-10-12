#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void aryOffs( Ary *ary1, Ary *ary2, int mxoffs, hdsdim *offs, int *status ) {
/*
*+
*  Name:
*     aryOffs

*  Purpose:
*     Obtain the pixel offset between two arrays.

*  Synopsis:
*     void aryOffs( Ary *ary1, Ary *ary2, int mxoffs, hdsdim *offs,
*                   int *status )

*  Description:
*     This function returns the pixel offset for each requested dimension
*     between two arrays. These values are the offsets which should be
*     added to the pixel indices of the first array to obtain the
*     indices of the corresponding pixel in the second array.

*  Parameters:
*     iary1
*        First array identifier.
*     iary2
*        Second array identifier.
*     mxoffs
*        Maximum number of pixel offsets to return (i.e. the declared
*        size of the supplied "offs" array).
*     offs
*        Returned holding an array of pixel offsets for each dimension.
*     status
*        The global status.

*  Notes:
*     -  The two array identifiers supplied need not refer to the same
*     base array (although they may often do so). If they do not, then
*     the offset between the pixels in each array is determined by
*     matching the pixel indices of their respective base arrays.
*     -  Note that non-zero pixel offsets may exist even for dimensions
*     which exceed the dimensionality of either of the two arrays
*     supplied. The symbolic constant ARY__MXDIM may be used to declare
*     the size of the OFFS argument so that it will be able to hold the
*     maximum number of non-zero offsets that this routine can return.

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
   AryACB *acb1;
   AryACB *acb2;
   AryDCB *dcb1;
   AryDCB *dcb2;
   int i;
   int n;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the array identifiers. */
   acb1 = (AryACB *) ary1Impid( ary1, 1, 1, 1, status );
   acb2 = (AryACB *) ary1Impid( ary2, 1, 1, 1, status );
   if( *status == SAI__OK ){
      ARY__DCB_LOCK_MUTEX;

/* Obtain pointers to the data objects. */
      dcb1 = acb1->dcb;
      dcb2 = acb2->dcb;

/* Loop to calculate the offset for each possible array dimension. */
      n = ( mxoffs < ARY__MXDIM ) ? mxoffs : ARY__MXDIM;
      for( i = 0; i < n; i++ ){

/* Combine the accumulated pixel-index shifts for each array and its data
   object. */
         offs[ i ] = ( acb2->shift[ i ] - dcb2->shift[ i ] ) -
                     ( acb1->shift[ i ] - dcb1->shift[ i ] );
      }

      ARY__DCB_UNLOCK_MUTEX;

/* Pad remaining values with zero. */
      for( ; i < mxoffs; i++ ) offs[ i ] = 0;
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryOffs: Error obtaining the pixel offset between two"
              " arrays.", status );
      ary1Trace( "aryOffs", status );
   }

}
