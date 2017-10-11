#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"
#include "prm_par.h"
#include <string.h>

void ary1Dscl( AryDCB *dcb, int *status ) {
/*
*+
*  Name:
*     ary1Dscl

*  Purpose:
*     Ensure that scaling information is available for a data object.

*  Synopsis:
*     void ary1Dscl( AryDCB *dcb, int *status )

*  Description:
*     This function ensures that values are available for the scale factor
*     and zero offset associated an entry in the DCB. The routine does
*     nothing if this information is already available. Otherwise, it
*     obtains the information by inspecting the data object itself and
*     stores the results in the DCB. Only those checks necessary to obtain
*     the scaling information are performed on the data object.

*  Parameters:
*     dcb
*        The data object (DCB).
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
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine.

*-
*/

/* Local variables: */
   HDSLoc *loc2 = NULL;       /* Locator to SCALE or ZERO component */
   int there;                 /* Whether a component exists */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Do nothing if scaling information is already available in the DCB. */
   if( !dcb->kscl ){

/* Ensure that form information is available. */
      ary1Dfrm( dcb, status );

/* Do nothing if an error has occurred. */
      if( *status != SAI__OK ) {

/* Scaled arrays.
   ============== */
      } else if( !strcmp( dcb->form, "SCALED" ) ){

/* Create a temporary HDS structure to hold the scale and zero terms in
   their original data type. */
         datTemp( "SCZR", 0, 0, &dcb->scloc, status );

/* See if there is a SCALE component present in the data object */
         datThere( dcb->loc, "SCALE", &there, status );
         if( *status != SAI__OK ) {

/* If not, report an error. */
         } else if( !there ){
            *status = ARY__SCLIN;
            datMsg( "ARRAY", dcb->loc );
            errRep( " ", "The SCALE component in missing in the scaled array"
                    "structure ^ARRAY.", status );

/* If there is, copy it to the temporary structure stored in the DCB. */
         } else {
            datFind( dcb->loc, "SCALE", &loc2, status );
            datCopy( loc2, dcb->scloc, "SCALE", status );
            datAnnul( &loc2, status );
         }

/* See if there is a ZERO component present in the data object */
         datThere( dcb->loc, "ZERO", &there, status );
         if( *status != SAI__OK ) {

/* If not, report an error. */
         } else if( !there ){
            *status = ARY__SCLIN;
            datMsg( "ARRAY", dcb->loc );
            errRep( " ", "The ZERO component in missing in the scaled array"
                    "structure ^ARRAY.", status );

/* If there is, copy it to the temporary structure stored in the DCB. */
         } else {
            datFind( dcb->loc, "ZERO", &loc2, status );
            datCopy( loc2, dcb->scloc, "ZERO", status );
            datAnnul( &loc2, status );
         }

/* Verify the scale and zero values. */
         ary1Vscl( dcb->scloc, status );

/* If OK, indicate that the scaling information is now available. */
         if( *status == SAI__OK ) dcb->kscl = 1;

/* Simple, delta and primitive arrays
   ==================================== */
      } else if( !strcmp( dcb->form, "SIMPLE" ) ||
                 !strcmp( dcb->form, "DELTA" ) ||
                 !strcmp( dcb->form, "PRIMITIVE" ) ){
         dcb->scloc = NULL;

/* If the form information in the DCB was not valid, then report an error.
   ================================================================ */
      } else {
         *status = ARY__FATIN;
         msgSetc( "BADFORM", dcb->form );
         errRep( " ", "Unsupported array form '^BADFORM' found in Data Control"
                 "Block (internal programming error).", status );
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dscl", status );

}
