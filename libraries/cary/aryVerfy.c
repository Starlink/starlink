#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"

void aryVerfy( Ary *ary, int *status ) {
/*
*+
*  Name:
*     aryVerfy

*  Purpose:
*     Verify that an array's data structure is correctly constructed.

*  Synopsis:
*     void aryVerfy( Ary *ary, int *status )

*  Description:
*     This function checks that the data structure containing an array is
*     correctly constructed and that the array's pixel values are
*     defined. It also checks for the presence of any "rogue"
*     components in the data structure. If an anomaly is found, then an
*     error results. Otherwise, the routine returns without further
*     action.

*  Parameters:
*     ary
*        Array identifier.
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

/* Obtain an index to the data object entry in the DCB and verify the
   object's structure. */
      dcb = acb->dcb;
      ary1Dvfy( dcb, status );
      if( *status == SAI__OK ){

/* Ensure that state information is available in the DCB. */
         ary1Dsta( dcb, status );

/* If the object's data values are undefined, then report an error. */
         if( *status == SAI__OK ){
            if( !dcb->state ){
               *status = ARY__UNDEF;
               datMsg( "ARRAY", dcb->loc );
               errRep( " ", "The array ^ARRAY is in an undefined state.",
                       status );
            }
         }
      }
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryVerfy: Error verifying array data structure.", status );
      ary1Trace( "aryVerfy", status );
   }

}
