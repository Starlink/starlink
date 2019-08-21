#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Vftp( NdfACB *acb, char *ftype, size_t ftype_length, int *status ){
/*
*+
*  Name:
*     ndf1Vftp

*  Purpose:
*     Determine the full data type of the variance component of an NDF.

*  Synopsis:
*     void ndf1Vftp( NdfACB *acb, char *ftype, size_t ftype_length,
*                    int *status )

*  Description:
*     This function returns the full data type of the variance component of
*     an NDF identified by its index in the ACB. The data type is returned
*     as an upper case character string.

*  Parameters:
*     acb
*        Pointer to the ACB entry identifying the NDF.
*     ftype
*        Pointer to an array in which to return a null terminated string
*        holding the full data type.
*     ftype_length
*        The length of the supplied 'ftype' array. This should include
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
   char *text;           /* Dynamically allocated string */
   int nc;               /* Length of string */
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

/* If so, then enquire the full data type of the array. */
      if( valid ) {
         aryFtype( dcb->vid, ftype, status );

/* Otherwise, use the default attributes stored in the DCB to return an
   appropriate full data type string. */
      } else {
         if( dcb->vcpx ) {
            text = astAppendStringf( NULL, &nc, "COMPLEX%s", dcb->vtyp );
            ndf1Ccpy( text, ftype, ftype_length, status );
            text = astFree( text );
         } else {
            ndf1Ccpy( dcb->vtyp, ftype, ftype_length, status );
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Vftp", status );

}

