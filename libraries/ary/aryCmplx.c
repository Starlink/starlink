#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void aryCmplx( Ary *ary, int *cmplx, int *status ) {
/*
*+
*  Name:
*     aryCmplx

*  Purpose:
*     Determine whether an array holds complex values.

*  Synopsis:
*     void aryCmplx( Ary *ary, int *cmplx, int *status )

*  Description:
*     This function returns a logical value indicating whether an array
*     holds complex values.

*  Parameters:
*     ary
*        Array identifier.
*     cmplx
*        Returned holding a flag indicating whether the array holds
*        complex values.
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
   AryDCB *dcb;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, 1, 1, status );
   if( *status == SAI__OK ){

/* Get the DCB index for the data object. */
      dcb = acb->dcb;

/* Ensure that data type information is available. */
      ary1Dtyp( dcb, status );

/* Assign the result. */
      *cmplx = dcb->complex;
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryCmplx: Error determining if an array holds complex "
              "values.", status );
      ary1Trace( "aryCmplx", status );
   }

}
