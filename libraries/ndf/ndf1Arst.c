#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include "mers.h"

void ndf1Arst( NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1Arst

*  Purpose:
*     Reset an NDF's axis component to an undefined state.

*  Synopsis:
*     void ndf1Arst( NdfACB *acb, int *status )

*  Description:
*     This function resets an NDF's axis coordinate system to an undefined
*     state by erasing the AXIS component in the data object (along with
*     all its sub-components).  All necessary cleaning up operations are
*     performed.

*  Parameters:
*     acb
*        Pointer to the NDF entry in the ACB.
*     *status
*        The global status.

*  Notes:
*     -  An axis component cannot be reset while mapped access to any of
*     its arrays (or any part of them) is in effect. An error will rssult
*     if this is the case.
*     -  An axis component can only be reset via a base NDF. If the NDF
*     supplied is a section, then this function will return without action.
*     No error will result.

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
   int iax;              /* Loop counter for axes */
   int ndim;             /* Number of NDF dimensions */
   int there;            /* Does an axis structure exist? */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain the number of NDF dimensions. */
   aryNdim( acb->did, &ndim, status );
   if( *status == SAI__OK ) {

/* Loop through all possible NDF axes, resetting the axis label, units,
   variance and width components. */
      for( iax = 0; iax < ndim; iax++ ){
         ndf1Acrst( iax, NDF__ALAB, acb, status );
         ndf1Acrst( iax, NDF__AUNI, acb, status );
         ndf1Avrst( iax, acb, status );
         ndf1Awrst( iax, acb, status );
      }
   }

/* Obtain an index to the data object entry in the DCB. */
   if( *status == SAI__OK ) {
      dcb = acb->dcb;

/* Loop to check the axis data array for each NDF dimension to
   determine whether it is mapped for access. Report an error if it is. */
      for( iax = 0; iax < ndim; iax++ ){
         if( acb->admap[ iax ] ) {
            *status = NDF__ISMAP;
            msgSeti( "AXIS", iax );
            ndf1Amsg( "NDF", acb );
            errRep( " ", "The centre array for axis ^AXIS of the NDF "
                    "structure ^NDF is already mapped for access through "
                    "the specified identifier (possible programming "
                    "error).", status );
            break;
         }
      }
   }

/* Check whether this is an NDF section. There is nothing more to do if
   it is. */
   if( *status == SAI__OK ) {
      if( !acb->cut ) {

/* Loop through each possible NDF dimension, checking whether the axis
   data array currently has mappings associated with it. Report an
   error if it does. */
         for( iax = 0; iax < ndim; iax++ ){
            if( dcb->nadmp[ iax ] != 0 ) {
               *status = NDF__ISMAP;
               msgSeti( "AXIS", iax );
               ndf1Dmsg( "NDF", dcb );
               errRep( " ", "The centre array for axis ^AXIS of the NDF "
                       "structure ^NDF is already mapped for access "
                       "through another identifier (possible programming "
                       "error).", status );
               break;
            }
         }

/* Initially assume there is no axis structure present. */
         if( *status == SAI__OK ) {
            there = 0;

/* Loop to reset the data array on each axis. Ensure that axis data
   array information is available in the DCB. */
            for( iax = 0; iax < ndim; iax++ ){
               ndf1Dad( iax, dcb, status );

/* If the axis data array exists, then note that an NDF axis structure
   must also exist. */
               if( *status == SAI__OK ) {
                  if( dcb->adid[ iax ] ) {
                     there = 1;

/* Obtain the axis data array storage form and numeric type and store
   these in the DCB in case the array must later be re-created. */
                     aryForm( dcb->adid[ iax ], dcb->adfrm[ iax ], status );
                     aryType( dcb->adid[ iax ], dcb->adtyp[ iax ], status );

/* Delete the axis data array and annul the locator to the axis
   structure element which contains it. */
                     aryDelet( dcb->adid + iax, status );
                     datAnnul( dcb->aloc + iax, status );
                  }
               }
            }

/* If an axis structure exists, then erase it. */
            if( *status == SAI__OK ) {
               if( there ) datErase( dcb->loc, "AXIS", status );
            }
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Arst", status );

}

