#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Vfrm( NdfACB *acb, char *form, size_t form_length, int *status ){
/*
*+
*  Name:
*     ndf1Vfrm

*  Purpose:
*     Determine the storage form of the variance component of an NDF.

*  Synopsis:
*     void ndf1Vfrm( NdfACB *acb, char *form, size_t form_length, int *status )

*  Description:
*     This function returns the storage form of the variance component of
*     an NDF identified by its index in the ACB. The storage form is
*     returned as an upper case character string.

*  Parameters:
*     acb
*        Pointer to the ACB entry identifying the NDF.
*     form
*        Pointer to an array in which to return a null terminated string
*        holding the storage form.
*     form_length
*        The length of the supplied 'form' array. This should include
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

/* If so, then enquire the storage form of the array. */
      if( valid ) {
         aryForm( dcb->vid, form, status );

/* Otherwise, use the default value stored in the DCB. */
      } else {
         ndf1Ccpy( dcb->vfrm, form, form_length, status );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Vfrm", status );

}

