#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"
#include "ary_ast.h"
#include <string.h>

void ary1Chmod( AryACB *acb, const char *mode, int *status ) {
/*
*+
*  Name:
*     ary1Chmod

*  Purpose:
*     Check that the requested array mapping access mode is permitted.

*  Synopsis:
*     void ary1Chmod( AryACB *acb, const char *mode, int *status )

*  Description:
*     This function checks that the requested mapping access mode is
*     permitted for an array. If it is not, then an error will be
*     reported. An error will also reported if the access mode string
*     supplied is not valid.  Otherwise the routine returns without
*     further action.

*  Parameters:
*     acb
*        The ACB.
*     mode
*        The requested access mode; one of "READ", "WRITE" or "UPDATE"
*        (case insensitive).
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
   char umode[ARY__SZACC+1];  /* Upper case version of MODE */
   AryDCB *dcb;               /* The data object (DCB) */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If UPDATE or WRITE access is requested, then check that WRITE access to
   the array is available. */
   if( !strcasecmp( mode, "WRITE" ) || !strcasecmp( mode, "UPDATE" ) ){
      ary1Chacc( acb, "WRITE", status );

/* If WRITE access is available, further check that the array is not stored
   in scaled or delta format. First ensure that form information is
   available in the DCB. Then report an error if the form is SCALED or
   DELTA. */
      dcb = acb->dcb;
      ary1Dfrm( dcb, status );
      if( *status == SAI__OK && ( !strcmp( dcb->form, "SCALED" ) ||
                                  !strcmp( dcb->form, "DELTA" ) ) ){
         astChrCase( mode, umode, 1, sizeof(umode) );
         *status = ARY__CMPAC;
         datMsg( "A", dcb->loc );
         msgSetc( "MODE", umode );
         msgSetc( "F", dcb->form );
         errRep( " ", "The array ^A is stored using ^F compression and "
                 "therefore cannot be mapped for ^MODE access (^F "
                 "compressed arrays are read-only).", status );
      }

/* No action is needed if READ access is requested. */
   } else if( !strcasecmp( mode, "READ" ) ){

/* Report an error if the MODE value supplied is not recognised. */
   } else {
      *status = ARY__FATIN;
      msgSetc( "BADMODE", mode );
      errRep( " ", "Routine ary1Chmod called with an invalid MODE argument of "
              "'^BADMODE' (internal programming error).", status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Chmod", status );

}
