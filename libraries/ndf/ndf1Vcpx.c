#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Vcpx( NdfACB *acb, int *cmplx, int *status ){
/*
*+
*  Name:
*     ndf1Vcpx

*  Purpose:
*     Determine whether the variance component of an NDF is complex.

*  Synopsis:
*     void ndf1Vcpx( NdfACB *acb, int *cmplx, int *status )

*  Description:
*     This function returns a logical value indicating whether the variance
*     component of an NDF is complex. The NDF is identified by its index in
*     the ACB.

*  Parameters:
*     acb
*        Pointer to the ACB entry identifying the NDF.
*     *cmplx
*        Returned holding the whether the variance component is complex.
*     *status
*        The global status.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   int valid;            /* Whether ARY_ system ID is valid */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that variance information is available in the DCB and ACB. */
   ndf1Vimp( acb, status );

/* Obtain an index to the data object entry in the DCB. */
   dcb = acb->dcb;

/* See if the DCB ARY_ system identifier for the variance array is
   valid. */
   valid = aryValid( dcb->vid, status );
   if( *status == SAI__OK ) {

/* If so, then enquire whether the array is complex. */
      if( valid ) {
         aryCmplx( dcb->vid, cmplx, status );

/* Otherwise, use the default value stored in the DCB. */
      } else {
         *cmplx = dcb->vcpx;
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Vcpx", status );

}

