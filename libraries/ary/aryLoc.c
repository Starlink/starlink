#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"

void aryLoc( Ary *ary, HDSLoc **loc, int *status ) {
/*
*+
*  Name:
*     aryLoc

*  Purpose:
*     Obtain an HDS locator for an array.

*  Synopsis:
*     void aryLoc( Ary *ary, HDSLoc **loc, int *status )

*  Description:
*     This function returns an HDS locator for the data object referred to
*     by the supplied ARY identifier.

*  Parameters:
*     ary
*        Array identifier.
*     loc
*        Returned holding the HDS locator. It should be annulled using
*        datAnnul when no longer needed. A value of NULL will be returned
*        if an error occurs.
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

/* Initialise */
   *loc = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, 1, 1, status );
   if( *status == SAI__OK ){

/* Get the DCB index for the data object. */
      dcb = acb->dcb;

/* Clone the data object locator. */
      datClone( dcb->loc, loc, status );
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryLoc: Error obtaining an HDS locator for an array.",
              status );
      ary1Trace( "aryLoc", status );
   }

}
