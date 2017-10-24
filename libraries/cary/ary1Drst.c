#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"
#include <string.h>

void ary1Drst( AryDCB *dcb, int *status ) {
/*
*+
*  Name:
*     ary1Drst

*  Purpose:
*     Reset the state of a data object to "undefined".

*  Synopsis:
*     void ary1Drst( AryDCB *dcb, int *status )

*  Description:
*     This function sets the HDS state of the data component(s) of an
*     array to "undefined" and reflects this change in the data
*     object's DCB entry.

*  Parameters:
*     dcb
*        Index to data object entry in the DCB.
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

/* Ensure that form information is available in the DCB. */
   ary1Dfrm( dcb, status );
   if( *status == SAI__OK ){

/* Primitive arrays.
   ================ */
      if( !strcmp( dcb->form, "PRIMITIVE" ) ){

/* Ensure that data type information and component locators are available
   in the DCB. */
         ary1Dtyp( dcb, status );
         if( *status == SAI__OK ){

/* Reset the state of the HDS data component. */
            if( dcb->dloc ) datReset( dcb->dloc, status );
         }

/* Simple and scaled arrays.
   ========================= */
      } else if( !strcmp( dcb->form, "SIMPLE" ) ||
                 !strcmp( dcb->form, "SCALED" ) ){

/* Ensure that data type information and component locators are available
   in the DCB. */
         ary1Dtyp( dcb, status );
         if( *status == SAI__OK ){

/* Reset the state of the HDS data component(s). */
            if( dcb->dloc ){
               datReset( dcb->dloc, status );
               if( dcb->complex ) datReset( dcb->iloc, status );
            }
         }

/* Delta arrays.
   ============= */
      } else if( !strcmp( dcb->form, "DELTA" ) ){
         if( *status == SAI__OK ){
            *status = ARY__CMPAC;
            datMsg( "A", dcb->loc );
            errRep( " ", "The array ^A is stored using DELTA compression and"
                    " therefore cannot be reset (DELTA compressed arrays are"
                    " read-only).", status );
         }

/* If the array form was not recognised, then report an error. */
      } else {
         *status = ARY__FATIN;
         msgSetc( "B", dcb->form );
         errRep( " ", "Unsupported array form '^B' found in Data Control"
                 " Block (internal programming error).", status );
      }
   }

/* Reset the data object's DCB state and initialisation entry and note
   whether the information is valid. */
   dcb->state = 0;
   dcb->init = 0;
   dcb->kstate = ( *status == SAI__OK );

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Drst", status );

}
