#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void aryState( Ary *ary, int *state, int *status ) {
/*
*+
*  Name:
*     aryState

*  Purpose:
*     Determine the state of an array (defined or undefined).

*  Synopsis:
*     void aryState( Ary *ary, int *state, int *status )

*  Description:
*     This function returns a flag indicating whether an array's
*     pixel values are currently defined.

*  Parameters:
*     ary
*        Array identifier.
*     state
*        Returned hoplding a flag indicating whether the array's
*        pixel values are defined.
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

/* Inport the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, 1, 1, status );
   if( *status == SAI__OK ){
      ARY__DCB_LOCK_MUTEX;

/* Ensure that state information is available in the DCB. */
      dcb = acb->dcb;
      ary1Dsta( dcb, status );

/* Obtain the array's state. */
      if( *status == SAI__OK ){
         *state = dcb->state;
      }

      ARY__DCB_UNLOCK_MUTEX;
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryState: Error obtaining array state information.",
              status );
      ary1Trace( "aryState", status );
   }

}
