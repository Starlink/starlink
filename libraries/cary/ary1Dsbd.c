#include <string.h>
#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "star/cmp.h"
#include "mers.h"
#include "ary_err.h"

void ary1Dsbd( int bad, AryDCB *dcb, int *status ) {
/*
*+
*  Name:
*     ary1Dsbd

*  Purpose:
*     Set the bad pixel flag for a data object.

*  Synopsis:
*     void ary1Dsbd( int bad, AryDCB *dcb, int *status )

*  Description:
*     The routine sets the bad pixel flag to a specified value for a
*     DCB.

*  Parameters:
*     bad
*        Bad pixel flag value to be set.
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

/* Ensure that the current value of the data object's bad pixel flag is
   known. */
   ary1Dbad( dcb, status );
   if( *status == SAI__OK ){

/* There is nothing to do unless the current value differs from that being
   set. */
      if( ( dcb->bad && !bad ) || ( !dcb->bad && bad ) ){

/* If a new value must be set, then ensure that form information is
   available for the data object. */
         ary1Dfrm( dcb, status );
         if( *status == SAI__OK ){

/* Primitive, scaled, delta and simple arrays.
   ===========================================
   These are all processed here. */
            if( ( !strcmp( dcb->form, "PRIMITIVE" ) ) ||
                ( !strcmp( dcb->form, "SCALED" ) ) ||
                ( !strcmp( dcb->form, "DELTA" ) ) ||
                ( !strcmp( dcb->form, "SIMPLE" ) ) ){

/* If the array is primitive, then it must first be converted to simple
   storage form in order to set the bad pixel flag to .FALSE.. Check if
   the data object is mapped for access. Report an error if it is. */
               if( !strcmp( dcb->form, "PRIMITIVE" ) ){
                  if( ( dcb->nwrite != 0 ) || ( dcb->nread != 0 ) ){
                     *status = ARY__ISMAP;
                     datMsg( "ARRAY", dcb->loc );
                     errRep( "ARY1_DSBD_MAP",
                             "The array ^ARRAY is mapped for access,"
                             "perhaps through another identifier (possible"
                             "programming error).", status );

/* Otherwise, perform the conversion. */
                  } else {
                     ary1Dp2s( dcb, status );
                  }

/* If an error occurred during form conversion, then report context
   information. */
                  if( *status != SAI__OK ){
                     errRep( "ARY1_DSBD_CVT",
                             "Unable to perform implicit conversion from"
                             "'PRIMITIVE' to 'SIMPLE' array storage form.",
                             status );
                  }
               }

/* We can now deal with simple arrays alone. Ensure that a logical
   BAD_PIXEL component is present. */
               cmpMod( dcb->loc, "BAD_PIXEL", "_LOGICAL", 0, NULL, status );

/* Enter the new value. */
               cmpPut0L( dcb->loc, "BAD_PIXEL", bad, status );

/* Note the new value (and whether it is valid) in the DCB. */
               dcb->bad = bad;
               dcb->kbad = ( *status == SAI__OK );

/* If the array form was not recognised, then report an error. */
            } else {
               *status = ARY__FATIN;
               msgSetc( "BADFORM", dcb->form );
               errRep( "ARY1_DSBD_FORM",
                       "Unsupported array form '^BADFORM' found in Data"
                       "Control Block (internal programming error).",
                       status );
            }
         }
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dsbd", status );

}
