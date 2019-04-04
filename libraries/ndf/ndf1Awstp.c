#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "ary.h"
#include "mers.h"
#include "star/util.h"

void ndf1Awstp( const char *type, int iax, NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1Awstp

*  Purpose:
*     Set a new numeric type for an axis width array.

*  Synopsis:
*     void ndf1Awstp( const char *type, int iax, NdfACB *acb, int *status )

*  Description:
*     This function sets a new numeric type for an NDF's axis width array.
*     If the array exists, then its existing values will be converted as
*     necessary.

*  Parameters:
*     type
*        Pointer to a null terminated string holding the new numeric type
*        (case insensitive).
*     iax
*        Zero-based index of the NDF axis whose width array numeric type is
*        to be modified.
*     acb
*        Pointer to the NDF entry in the ACB.
*     *status
*        The global status.

*  Notes:
*     -  The numeric type of an axis width array cannot be changed with
*     this function while mapped access to any part of it is in effect.
*     -  This function may only be used to set a numeric type for the axis
*     width array of a base NDF. It returns without action if the NDF
*     supplied is a section.

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

/* Ensure that axis width array information is available in the DCB. */
   ndf1Daw( iax, dcb, status );
   if( *status == SAI__OK ) {

/* Check that the axis width array is not mapped for access through
   this ACB entry. Report an error if it is. */
      if( acb->awmap[ iax ] ) {
         *status = NDF__ISMAP;
         msgSeti( "AXIS", iax );
         ndf1Amsg( "NDF", acb );
         errRep( " ", "The width array for axis ^AXIS of the NDF structure "
                 "^NDF is already mapped for access through the specified "
                 "identifier (possible programming error).", status );

/* Check that the NDF is not a section. There is nothing to do if it is. */
      } else if( !acb->cut ) {

/* Check that there are no other mappings to this axis width array.
   Report an error if there are. */
         if( dcb->nawmp[ iax ] != 0 ) {
            *status = NDF__ISMAP;
            msgSeti( "AXIS", iax );
            ndf1Dmsg( "NDF", dcb );
            errRep( " ", "The width array for axis ^AXIS of the NDF "
                    "structure ^NDF is already mapped for access through "
                    "another identifier (possible programming error).", status );

/* If the axis width array exists, then set a new numeric type for it. */
         } else if( dcb->awid[ iax ] ) {
            aryStype( type, dcb->awid[ iax ], status );

/* Otherwise, set a new default value for its numeric type in the DCB
   (in upper case). */
         } else {
            star_strlcpy( dcb->awtyp[ iax ], type, sizeof( dcb->awtyp[ iax ] ) );
            astChrCase( NULL, dcb->awtyp[ iax ], 1, 0 );
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Awstp", status );

}

