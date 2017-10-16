#include "sae_par.h"
#include "ary1.h"
#include "dat_par.h"

void ary1Pbnd( AryACB *acb, int *prim, int *status ) {
/*
*+
*  Name:
*     ary1Pbnd

*  Purpose:
*     Determine if array bounds are consistent with a primitive array.

*  Synopsis:
*     void ary1Pbnd( AryACB *acb, int *prim, int *status )

*  Description:
*     This function returns a boolean falg indicating whether the bounds
*     of an array are consistent with that array being a primitive
*     array. The value 1 is returned if the lower pixel index
*     bound in each array dimension is 1. Otherwise, zero is
*     returned. The array is identified to this routine by its ACB.

*  Parameters:
*     acb
*        The array ACB.
*     prim
*        Returned holkding a flag indicating whether the array bounds are
*        consistent with a primitive array.
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
   *prim = 1;

/* Loop to inspect the lower bound of each array dimension. */
   for( i = 0; i < acb->ndim; i++ ){

/* The array cannot be primitive if the lower bound is not 1. */
      if( acb->lbnd[ i ] != 1 ){
         *prim = 0;
         break;
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Pbnd", status );

}
