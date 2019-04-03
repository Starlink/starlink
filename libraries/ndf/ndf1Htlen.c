#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Htlen( NdfDCB *dcb, size_t *htlen, int *status ){
/*
*+
*  Name:
*     ndf1Htlen

*  Purpose:
*     Return the length of the text in the current history record.

*  Synopsis:
*     void ndf1Htlen( NdfDCB *dcb, size_t *htlen, int *status )

*  Description:
*     This function returns the length of the text in the current history
*     record, as determined from the HDS locators stored in the DCB.

*  Parameters:
*     dcb
*        Pointer to the NDF whose history is to be modified.
*     *htlen
*        Returned holding the length of the text in the current history
*        record.
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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   HDSLoc *cell = NULL;  /* Locator for array sell */
   HDSLoc *loc = NULL;   /* Temporary locator */
   hdsdim sub;           /* Array subscript */

/* Initialise */
   *htlen = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that history information is available in the DCB. */
   ndf1Dh( dcb, status );
   if( *status == SAI__OK ) {

/* Check if a history component is present, otherwise there is nothing
   more to do. */
      if( dcb->hloc ) {

/* Get a locator to the current record. */
         sub = dcb->hnrec;
         if( sub >= 1 ) {
            datCell( dcb->hrloc, 1, &sub, &cell, status );

/* Obtain a locator to the TEXT component, and get its HDS character
   length. */
            datFind( cell, "TEXT", &loc, status );
            datClen( loc, htlen, status );

/* Annul the locators. */
            datAnnul( &loc, status );
            datAnnul( &cell, status );
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Htlen", status );

}

