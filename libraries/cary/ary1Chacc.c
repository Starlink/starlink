#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"
#include "ary_ast.h"
#include <string.h>

void ary1Chacc( AryACB *acb, const char *access, int *status ) {
/*
*+
*  Name:
*     ary1Chacc

*  Purpose:
*     Check that a specified type of access to an ACB entry is
*     permitted.

*  Synopsis:
*     void ary1Chacc( AryACB *acb, const char *access, int *status )

*  Description:
*     This function checks that the specified type of access to an ACB
*     entry is permitted. If it is, then it returns without further
*     action, otherwise an error is reported.

*  Parameters:
*     acb
*        The ACB entry.
*     access
*        The type of access required (case insensitive).
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
   char uacc[ARY__SZACC+1];   /* Upper case version of ACCESS */
   AryDCB *dcb;               /* The data object (DCB) */
   int ok;                    /* Whether requested access is permitted */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Determine if the required type of access is available. */
   ary1Accok( acb, access, &ok, status );
   if( *status == SAI__OK ){

/* If it is not, then report an error. */
      if( !ok ){
         *status = ARY__ACDEN;
         dcb = acb->dcb;
         datMsg( "ARRAY", dcb->loc );

/* ...Use an upper case version of the access type. */
         astChrCase( access, uacc, 1, sizeof(uacc) );
         msgSetc( "ACCESS", uacc );
         errRep( " ", "^ACCESS access to the array ^ARRAY is not available or"
                 " had been disabled (possible programming error).", status );
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Chacc", status );

}
