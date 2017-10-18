#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void aryImprt( HDSLoc *loc, Ary **ary, int *status ) {
/*
*+
*  Name:
*     aryImprt

*  Purpose:
*     Import an array into the ARY_ system from HDS.

*  Synopsis:
*     void aryImprt( HDSLoc *loc, Ary **ary, int *status )

*  Description:
*     This function imports an array into the ARY_ system from HDS and
*     issues an identifier for it. The array may then be manipulated by
*     the ARY_ routines.

*  Parameters:
*     loc
*        HDS locator to an array structure.
*     ary
*        Returned holding an array identifier for the array structure.
*     status
*        The global status.

*  Notes:
*     -  The locator supplied as input to this routine may later be
*     annulled without affecting the subsequent behaviour of the ARY_
*     system.
*     -  If this routine is called with "status" set, then a value of
*     NULL will be returned for the "ary" argument, although no
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
   AryACB *acb;

/* Set an initial value for the "ary" argument. */
   *ary = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the array structure into the ACB. */
   ary1Imp( loc, &acb, status );

/* Export an identifier for the new array. */
   *ary = ary1Expid( (AryObject *) acb, status );

/* If an error occurred, then reset the IARY value and report context
   information. */
   if( *status != SAI__OK ){
      *ary = NULL;
      errRep( " ", "aryImprt: Error importing an array structure from HDS.",
              status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "aryImprt", status );

}
