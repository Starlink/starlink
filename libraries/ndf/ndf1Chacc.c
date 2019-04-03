#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Chacc( NdfACB *acb, const char *access, int *status ){
/*
*+
*  Name:
*     ndf1Chacc

*  Purpose:
*     Check that a specified type of access to an ACB entry is permitted.

*  Synopsis:
*     void ndf1Chacc( NdfACB *acb, const char *access, int *status )

*  Description:
*     This function checks that the specified type of access to an ACB
*     entry is permitted. If it is, then it returns without further action,
*     otherwise an error is reported.

*  Parameters:
*     acb
*        Pointer to the ACB entry.
*     access
*        Pointer to a null terminated string holding the type of access
*        required (case insensitive).
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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char uacc[ NDF__SZACC + 1 ];    /* Upper case version of ACCESS */
   int ok;               /* Whether requested access is permitted */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Determine if the required type of access is available. */
   ndf1Accok( acb, access, &ok, status );
   if( *status == SAI__OK ) {

/* If it is not, then report an error. */
      if( !ok ) {
         *status = NDF__ACDEN;
         ndf1Amsg( "NDF", acb );

/* ...Use an upper case version of the access type. */
         astChrCase( access, uacc, 1, sizeof( uacc ) );
         msgSetc( "ACCESS", uacc );
         errRep( " ", "^ACCESS access to the NDF structure ^NDF is not "
                 "available via the specified identifier or has been "
                 "disabled (possible programming error).", status );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Chacc", status );

}

