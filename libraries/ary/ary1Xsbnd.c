#include "sae_par.h"
#include "ary1.h"
#include "dat_par.h"

void ary1Xsbnd( int ndim1, const hdsdim *lbnd1, const hdsdim *ubnd1, int ndim2,
                const hdsdim *lbnd2, const hdsdim *ubnd2, int ndim, hdsdim *lbnd,
                hdsdim *ubnd, int *exist, int *status ) {
/*
*+
*  Name:
*     ary1Xsbnd

*  Purpose:
*     Calculate bounds of the intersection set between two arrays.

*  Synopsis:
*     void ary1Xsbnd( int ndim1, const hdsdim *lbnd1, const hdsdim *ubnd1, int ndim2,
*                     const hdsdim *lbnd2, const hdsdim *ubnd2, int ndim,
*                     hdsdim *lbnd, hdsdim *ubnd, int *exist, int *status )

*  Description:
*     The routine calculates the bounds of the region in common between
*     two arrays, whose lower and upper bounds are supplied. If the
*     dimensionalities of the two arrays differ, then the bounds of the
*     array with lower dimensionality are padded with 1's before
*     calculating the intersection region. The resulting bounds are
*     returned up to the dimensionality limit set by the size of the
*     output arrays; if these require more values than are available,
*     then they are padded with 1's.  If no intersection region exists,
*     then the logical "exist" flag is set to zero to indicate this.
*     No checks are performed to ensure that the array bounds supplied
*     are valid.

*  Parameters:
*     ndim1
*        Number of dimensions for first array.
*     lnmd1
*        Lower bounds of first array.
*     ubnd1
*        Upper bounds of first array.
*     ndim2
*        Number of dimensions for second array.
*     lbnd2
*        Lower bounds of second array.
*     ubnd2
*        Upper bounds of second array.
*     ndim
*        The number of elements in output "lbnd" and "ubnd" arrays.
*     lbnd
*        Returned holding the Lower bounds of the intersection region (if
*        it exists).
*     ubnd
*        Returned holding the Upper bounds of the intersection region (if
*        it exists).
*     exist
*        Returned holding a flag indicating whether the intersection region
*        exists.
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
   hdsdim l;                  /* Lower bound of intersection region */
   hdsdim u1;                 /* Upper bound of first array */
   hdsdim u2;                 /* Upper bound of second array */
   hdsdim u;                  /* Upper bound of intersection region */
   int i;                     /* Loop counter for dimensions */
   int maxdim;                /* Max of ndim, ndim1 and ndim2 */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise. */
   *exist = 1;

/* Loop to process each relevant dimension. */
   maxdim = ndim;
   if( ndim1 > maxdim ) maxdim = ndim1;
   if( ndim2 > maxdim ) maxdim = ndim2;
   for( i = 0; i < maxdim; i++ ){

/* Obtain the bounds of the first array in this dimension, padding with 1's
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

/* Calculate the bounds of the overlap region. */
      l = ( l1 > l2 ) ? l1 : l2;
      u = ( u1 > u2 ) ? u2 : u1;

/* If there is no overlap, then return with "exist" set to zero. */
      if( l > u ){
         *exist = 0;
         break;

/* Put the bounds of the overlap region into the output arrays, if there is
   room. */
      } else if( i <= ndim ){
         lbnd[ i ] = l;
         ubnd[ i ] = u;
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Xsbnd", status );

}
