#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void aryBase( Ary *ary1, Ary**ary2, int *status ) {
/*
*+
*  Name:
*     aryBase

*  Purpose:
*     Obtain an identifier for a base array.

*  Synopsis:
*     void aryBase( Ary *ary1, Ary**ary2, int *status )

*  Description:
*     This function returns an identifier for the base array with which
*     an array section is associated.

*  Parameters:
*     ary1
*        Identifier for an existing array section (the function will also
*        work if this is already a base array).
*     ary2
*        Returned holding an identifier for the base array with which the
*        section is associated.
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
   AryACB *acb1;              /* Pointer to input Access Control Block */
   AryACB *acb2;              /* Pointer to returned Access Control Block */
   AryDCB *dcb;               /* Pointer to shared Data Control Block */

/* Set an initial value for the IARY2 argument. */
   *ary2 = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the input array identifier. */
   acb1 = (AryACB *) ary1Impid( ary1, 1, 1, 1, status );
   if( *status == SAI__OK ){
      ARY__DCB_LOCK_MUTEX;

/* Obtain a pointer to the shared Data Control Block. */
      dcb = acb1->dcb;

/* Create a new base array entry in the ACB to describe it. */
      ary1Crnba( dcb, &acb2, status );
      if( *status == SAI__OK ){

/* Transfer the access control flags from the old ACB entry to the new one. */
         acb2->access = acb1->access;

/* Export an identifier for the new base array. */
         *ary2 = ary1Expid( (AryObject *) acb2, status );

/* If an error occurred, then annul the new ACB entry. */
         if( *status != SAI__OK ) {
            ARY__ACB_LOCK_MUTEX;
            ary1Anl( acb2, status );
            ARY__ACB_UNLOCK_MUTEX;
         }
      }

      ARY__DCB_UNLOCK_MUTEX;
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryBase: Error obtaining identifier for a base array.",
              status );
      ary1Trace( "aryBase", status );
   }

}
