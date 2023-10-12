#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void aryClone( Ary *ary1, Ary **ary2, int *status ) {
/*
*+
*  Name:
*     aryClone

*  Purpose:
*     Clone an array identifier.

*  Synopsis:
*     void aryClone( Ary *ary1, Ary **ary2, int *status )

*  Description:
*     This function produces a "cloned" copy of an array identifier (i.e.
*     it produces a new identifier describing an array with identical
*     attributes to the original).

*  Parameters:
*     ary1
*        Array identifier to be cloned.
*     ary2
*        Returned holding the cloned identifier.
*     status
*        The global status.

*  Notes:
*     -  If this routine is called with "status" set, then a value of
*     NULL will be returned for the "ary2" argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.

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
   AryACB *acb1;            /* Original ACB */
   AryACB *acb2;            /* Cloned ACB */

/* Set an initial value of NULL for the returned ary2 value. */
   *ary2 = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the original array identifier. */
   acb1 = (AryACB *) ary1Impid( ary1, 1, 1, 1, status );

   ARY__DCB_LOCK_MUTEX;

/* Produce a cloned copy of its ACB entry. */
   ary1Cln( acb1, &acb2, status );

   ARY__DCB_UNLOCK_MUTEX;

/* Export an identifier for the new array. */
   *ary2 = ary1Expid( (AryObject *) acb2, status );

/* If an error occurred, then reset the "ary2" argument to NULL, report
   context information and call the error tracing routine. */
   if( *status != SAI__OK ){
      *ary2 = NULL;
      errRep( " ", "aryClone: Error cloning array identifier.", status );
      ary1Trace( "aryClone", status );
   }

}
