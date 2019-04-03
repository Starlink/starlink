#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Vstyp( NdfACB *acb, char *type, size_t type_length, int *status ){
/*
*+
*  Name:
*     ndf1Vstyp

*  Purpose:
*     Determine the numeric data type of the scaled variance component of
*     an NDF.

*  Synopsis:
*     void ndf1Vstyp( NdfACB *acb, char *type, size_t type_length, int *status )

*  Description:
*     This function returns the numeric data type of the scaled variance
*     component of an NDF identified by its index in the ACB. The data type
*     is returned as an upper case character string. The returned type
*     describes the values stored in the array, before they are unscaled
*     using the associated scale and zero values. Use ndf1Vtyp if you need
*     the data type of the array after it has been unscaled.

*  Parameters:
*     acb
*        Pointer to the ACB entry identifying the NDF.
*     type
*        Pointer to an array in which to return a null terminated string
*        holding the numeric data type.
*     type_length
*        The length of the supplied 'type' array. This should include
*        room for the terminating null.
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
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   int valid;            /* Whether ARY_ system ID is valid */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that variance information is available in the DCB and ACB. */
   ndf1Vimp( acb, status );

/* Obtain an index to the data object entry in the DCB. */
   dcb = acb->dcb;

/* See if the ARY_ system identifier for the variance array is valid. */
   valid = aryValid( dcb->vid, status );
   if( *status == SAI__OK ) {

/* If so, then enquire the data type of the array. */
      if( valid ) {
         arySctyp( dcb->vid, type, status );

/* Otherwise, use the default value stored in the DCB. */
      } else {
         ndf1Ccpy( dcb->vtyp, type, type_length, status );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Vstyp", status );

}

