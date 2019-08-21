#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ary.h"

void ndf1Awtyp( int iax, NdfACB *acb, char *type, size_t type_length,
                int *status ){
/*
*+
*  Name:
*     ndf1Awtyp

*  Purpose:
*     Obtain the numeric type of an axis width array.

*  Synopsis:
*     void ndf1Awtyp( int iax, NdfACB *acb, char *type, size_t type_length,
*                     int *status )

*  Description:
*     This function returns the numeric type of an NDF axis width array as
*     an upper case character string. The NDF is identified by its entry in
*     the ACB.

*  Parameters:
*     iax
*        Zero-based index of the NDF axis for which information is
*        required.
*     acb
*        Pointer to the NDF entry in the ACB.
*     type
*        Pointer to an array in which to return a null terminated string
*        holding axis width array numeric type (upper case).
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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain an index to the data object entry in the DCB. */
   dcb = acb->dcb;

/* Ensure that axis width array information is available. */
   ndf1Daw( iax, dcb, status );
   if( *status == SAI__OK ) {

/* If the axis width array exists, then determine its numeric type
   directly. */
      if( dcb->awid[ iax ] ) {
         aryType( dcb->awid[ iax ], type, status );

/* Otherwise, obtain the numeric type from the DCB. */
      } else {
         ndf1Ccpy( dcb->awtyp[ iax ], type, type_length, status );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Awtyp", status );

}

