#include <string.h>
#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"

void ary1Dsta( AryDCB *dcb, int *status ) {
/*
*+
*  Name:
*     ary1Dsta

*  Purpose:
*     Ensure that state information is available for a data object.

*  Synopsis:
*     void ary1Dsta( AryDCB *dcb, int *status )

*  Description:
*     The routine ensures that information about the state (i.e.
*     defined or undefined) of an array is available. It does nothing
*     if the state information is already available in the DCB.
*     Otherwise, it obtains this information by inspecting the data
*     object itself, storing the result in the DCB. Only those checks
*     needed for obtaining the state information are performed on the
*     data object.

*  Parameters:
*     dcb
*        Pointer to the DCB.
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

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Do nothing if state information is already available in the DCB. */
   if( !dcb->kstate ){

/* Ensure that form information is available for the data object. */
      ary1Dfrm( dcb, status );
      if( *status != SAI__OK ) goto L999;

/* Primitive arrays.
   ================ */
      if( !strcmp( dcb->form, "PRIMITIVE" ) ){

/* Ensure that type (and complexity) information and component locators are
   available. */
         ary1Dtyp( dcb, status );

/* Test the state of the non-imaginary component. */
         datState( dcb->dloc, &dcb->state, status );

/* Simple, scaled and delta arrays.
   ================================ */
      } else if( !strcmp( dcb->form, "SIMPLE" ) ||
                 !strcmp( dcb->form, "SCALED" ) ||
                 !strcmp( dcb->form, "DELTA" ) ){

/* Ensure that type (and complexity) information and component locators are
   available. */
         ary1Dtyp( dcb, status );

/* Test the state of the non-imaginary component. */
         datState( dcb->dloc, &dcb->state, status );
         if( *status != SAI__OK ) goto L999;

/* If the array is complex and the non-imaginary component is defined, then
   test the imaginary component also (both must be defined if the entire
   array is to be defined). */
         if( dcb->complex && dcb->state ){
            datState( dcb->iloc, &dcb->state, status );
         }

/* If the form entry in the DCB is not valid, then report an error. */
      } else {
         *status = ARY__FATIN;
         msgSetc( "BADFORM", dcb->form );
         errRep( "ARY1_DSTA_FRM",
                 "Unsupported array form '^BADFORM' found in Data Control "
                 "Block (internal programming error).", status );
      }

/* Note if state information is now available in the DCB. */
L999:
      dcb->kstate = ( *status == SAI__OK );

/* Note if the data object's values have been initialised. */
      if( *status == SAI__OK ) dcb->init = dcb->state;
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dsta", status );

}
