#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "star/cmp.h"
#include "mers.h"
#include "ary_err.h"
#include <string.h>

void ary1Cpscl( AryDCB *dcb1, AryDCB *dcb2, int *status ) {
/*
*+
*  Name:
*     ary1Cpscl

*  Purpose:
*     Copy scale information from one DCB entry to another.

*  Synopsis:
*     void ary1Cpscl( AryDCB *dcb1, AryDCB *dcb2, int *status )

*  Description:
*     This function copies the information describing the scale and zero
*     terms for a supplied data array (identified by its DCB entry) to
*     another existing DCB entry.

*  Parameters:
*     dcb1
*        The DCB entry of the array to be copied.
*     dcb2
*        The DCB entry to recieve the copy.
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
   HDSLoc *loc2 = NULL;
   int there;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that form information is available in the input DCB entry. */
   ary1Dfrm( dcb1, status );
   if( *status == SAI__OK ){

/* If the first DCB entry is a scaled array then we copy the scale
   information. */
      if( !strcmp( dcb1->form, "SCALED" ) ){

/* Report an error if we are trying to store scale information in a complex
   valued array. */
         ary1Dtyp( dcb1, status );
         if( dcb1->complex && *status == SAI__OK ){
            *status = ARY__USFRM;
            errRep( " ", "Complex scaled arrays are currently unsupported by "
                    "the ARY library.", status );
         }

/* Ensure scaling information is available for dcb1. */
         ary1Dscl( dcb1, status );

/* Create a new temporary HDS structure to hold a copy of the scale
   information, and store the new locator in the output DCB entry. */
         datTemp( "SCZR", 0, 0, &dcb2->scloc, status );

/* Get a locator to the input SCALE value and copy it to the temporary HDS
   structure created above, and to the data object. Then annul the
   locator. */
         datFind( dcb1->scloc, "SCALE", &loc2, status );
         datCopy( loc2, dcb2->scloc, "SCALE", status );
         datCopy( loc2, dcb2->loc, "SCALE", status );
         datAnnul( &loc2, status );

/* Do the same for the ZERO value. */
         datFind( dcb1->scloc, "ZERO", &loc2, status );
         datCopy( loc2, dcb2->scloc, "ZERO", status );
         datCopy( loc2, dcb2->loc, "ZERO", status );
         datAnnul( &loc2, status );

/* Indicate that scaling information is now available in the output DCB
   entry. */
         dcb2->kscl = 1;

/* Ensure the storage form is now scaled in both the data object and the
   DCB. */
         datThere( dcb2->loc, "VARIANT", &there, status );
         if( there ) datErase( dcb2->loc, "VARIANT", status );
         datNew0C( dcb2->loc, "VARIANT", 6, status );
         cmpPut0C( dcb2->loc, "VARIANT", "SCALED", status );
         strcpy( dcb2->form, "SCALED" );

/* If the input DCB entry is not for a scaled array, then ensure the output
   DCB entry has no scale information. */
      } else {
         if( dcb2->scloc ) datAnnul( &dcb2->scloc, status );
         dcb2->kscl = 0;
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Cpscl", status );

}
