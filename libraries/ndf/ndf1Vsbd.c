#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Vsbd( int bad, NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1Vsbd

*  Purpose:
*     Set the bad pixel flag for the variance component of an NDF.

*  Synopsis:
*     void ndf1Vsbd( int bad, NdfACB *acb, int *status )

*  Description:
*     This function sets a value for the logical bad pixel flag of an NDF's
*     variance component. The NDF is identified by its ACB entry.

*  Parameters:
*     bad
*        The value to be set.
*     acb
*        Pointer to the NDF's ACB entry.
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
   int there;            /* Whether the variance array exists */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If the variance array is mapped for access, then set the ACB bad
   pixel flag for the mapped values and note it has been modified. */
   if( acb->vmap ) {
      acb->vmbad = bad;
      acb->vmbmd = 1;

/* Otherwise, ensure that variance information is available in the DCB
   and ACB. */
   } else {
      ndf1Vimp( acb, status );

/* See if the ARY_ system identifier for the variance array is valid.
   If not, then the array does not exist. */
      there = aryValid( acb->vid, status );
      if( *status == SAI__OK ) {

/* If it exists, then set its bad pixel flag (if it does not exist,
   then the variance component is undefined, so its bad pixel flag
   cannot be changed and remains at non-zero). */
         if( there ) arySbad( bad, acb->vid, status );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Vsbd", status );

}

