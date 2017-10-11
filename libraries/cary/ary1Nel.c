#include "sae_par.h"
#include "ary1.h"
#include "dat_par.h"

void ary1Nel( int ndim, const hdsdim *lbnd, const hdsdim *ubnd, size_t *el,
              int *status ) {
/*
*+
*  Name:
*     ary1Nel

*  Purpose:
*     Calculate the number of elements in an array.

*  Synopsis:
*     void ary1Nel( int ndim, const hdsdim *lbnd, const hdsdim *ubnd,
*                   size_t *el, int *status )

*  Description:
*     The function calculates the number of elements in a
*     multi-dimensional array from the lower and upper bounds
*     information. The bounds information is not checked for validity.

*  Parameters:
*     ndim
*        Number of array dimensions.
*     lbnd
*        Lower array bounds.
*     ubnd
*        Upper array bounds.
*     el
*        Returned holding the number of elements in the array.
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
   int i;                     /* Loop counter for dimensions */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise. */
   *el = 1;

/* Multiply together the array extents in each dimension. */
   for( i = 0 ; i < ndim; i++ ){
      *el *= ( ubnd[ i ] - lbnd[ i ] + 1 );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Nel", status );

}
