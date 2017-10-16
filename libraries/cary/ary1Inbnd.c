#include "sae_par.h"
#include "ary1.h"
#include "dat_par.h"

void ary1Inbnd( int ndim1, const hdsdim *lbnd1, const hdsdim *ubnd1, int ndim2,
                const hdsdim *lbnd2, const hdsdim *ubnd2, int *inside, int *status ) {
/*
*+
*  Name:
*     ary1Inbnd

*  Purpose:
*     Test if the bounds of one array lie inside those of another.

*  Synopsis:
*     void ary1Inbnd( int ndim1, const hdsdim *lbnd1, const hdsdim *ubnd1, int ndim2,
*                     const hdsdim *lbnd2, const hdsdim *ubnd2, int *inside,
*                     int *status )

*  Description:
*     The routine checks to see if the second set of array bounds
*     supplied lie inside the first set. For this purpose, "inside"
*     means that there are no pixels in the second array which are not
*     present in the first. If the arrays are of different
*     dimensionality, then the bounds of the array with lower
*     dimensionality are padded with 1's before testing them. The array
*     bounds information supplied is not checked for validity.

*  Parameters:
*     ndim1
*        Number of dimensions for the first array.
*     lbnd1
*        Lower bounds of the first array.
*     ubnd1
*        Upper bounds of the first array.
*     ndim2
*        Number of dimensions for the second array.
*     lbnd2
*        Lower bounds of the second array.
*     ubnd2
*        Upper bounds of the second array.
*     inside
*        Returned holding a flag indicating whether the second array
*        lies inside the first one.
*     status
*        The global status.

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
   hdsdim l1;                 /* Lower bound of first array */
   hdsdim l2;                 /* Lower bound of second array */
   hdsdim u1;                 /* Upper bound of first array */
   hdsdim u2;                 /* Upper bound of second array */
   int i;                     /* Loop counter for dimensions */
   int maxdim;                /* Max of ndim1 and ndim2 */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise. */
   *inside = 1;

/* Loop to test each relevant dimension. */
   maxdim = ( ndim1 > ndim2 ) ? ndim1 : ndim2;
   for( i = 0; i < maxdim; i++ ){

/* Obtain the bounds of the first array in each dimension, padding with 1's
   if necessary. */
      if( i <= ndim1 ){
         l1 = lbnd1[ i ];
         u1 = ubnd1[ i ];
      } else {
         l1 = 1;
         u1 = 1;
      }

/* Similarly, obtain the bounds of the second array. */
      if( i <= ndim2 ){
         l2 = lbnd2[ i ];
         u2 = ubnd2[ i ];
      } else {
         l2 = 1;
         u2 = 1;
      }

/* Test to see if the extent of the second array lies inside that of the
   first array. Return with *inside set to zero. if this is not true for
   any dimension. */
      if( ( l1 > l2 ) || ( u1 < u2 ) ){
         *inside = 0;
         break;
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Inbnd", status );

}
