#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"

void aryReset( Ary *ary, int *status ) {
/*
*+
*  Name:
*     aryReset

*  Purpose:
*     Reset an array to an undefined state.

*  Synopsis:
*     void aryReset( Ary *ary, int *status )

*  Description:
*     This function resets an array so that its values become undefined.
*     Its use is advisable before making format changes to an array if
*     retention of the existing values is not required (e.g. before
*     changing its data type with the aryStype function); this will
*     avoid the cost of converting the existing values.

*  Parameters:
*     ary
*        Array identifier.
*     status
*        The global status.

*  Notes:
*     -  This function may only be used to reset the state of a base
*     array. If an array section is supplied, then it will return
*     without action. No error will result.
*     -  An array cannot be reset while it is mapped for access. This
*     routine will fail if this is the case.

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

/* Import the array identifier and check that WRITE access to the array is
   permitted. */
   acb = (AryACB *) ary1Impid( ary, 1, 0, 1, status );
   ary1Chacc( acb, "WRITE", status );
   if( *status == SAI__OK ){

/* If an array section has been specified, then there is nothing to do.
   However, a check is still performed to ensure that the array is not
   mapped through the identifier supplied. Report an error if it is. */
      if( acb->cut ){
         if( acb->mcb ){
            *status = ARY__ISMAP;
            dcb = acb->dcb;
            datMsg( "ARRAY", dcb->loc );
            errRep( " ", "The array ^ARRAY is mapped for access through the"
                    "specified identifier (possible programming error).",
                    status );
         }

/* If the array is a base array, then check that there is no mapped access
   to any part of it. */
      } else {
         dcb = acb->dcb;
         if( ( dcb->nread != 0 ) || ( dcb->nwrite != 0 ) ){

/* Report an error if there is. */
            *status = ARY__ISMAP;
            dcb = acb->dcb;
            datMsg( "ARRAY", dcb->loc );
            errRep( " ", "The base array ^ARRAY is mapped for access, perhaps"
                    "through another identifier (possible programming"
                    "error).", status );

/* Reset the array's state to "undefined". */
         } else {
            ary1Drst( dcb, status );

/* Set its bad pixel flag to .TRUE.. */
            ary1Sbd( 1, acb, status );
         }
      }
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryReset: Error resetting an array to an undefined state.",
              status );
      ary1Trace( "aryReset", status );
   }

}
