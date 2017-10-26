#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void arySize( Ary *ary, size_t *npix, int *status ) {
/*
*+
*  Name:
*     arySize

*  Purpose:
*     Determine the size of an array.

*  Synopsis:
*     void arySize( Ary *ary, int npix, int *status )

*  Description:
*     This function returns the number of pixels in the array whose
*     identifier is supplied (i.e. the product of its dimensions).

*  Parameters:
*     ary
*        Array identifier.
*     npix
*        Returned holding the number of pixels in the array.
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
   AryACB *acb;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, 1, 1, status );

/* Calculate the number of array elements from the bounds information in
   the ACB. */
   ary1Nel( acb->ndim, acb->lbnd, acb->ubnd, npix, status );

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "arySize: Error obtaining array size information.", status );
      ary1Trace( "arySize", status );
   }

}
