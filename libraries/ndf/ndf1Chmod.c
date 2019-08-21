#include "sae_par.h"
#include "dat_par.h"
#include "ndf_err.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Chmod( NdfACB *acb, const char *mode, int *status ){
/*
*+
*  Name:
*     ndf1Chmod

*  Purpose:
*     Check that the requested mode of mapped NDF access is permitted.

*  Synopsis:
*     void ndf1Chmod( NdfACB *acb, const char *mode, int *status )

*  Description:
*     This function checks that the requested mode of mapped access is
*     permitted for an NDF. If it is not, then an error will be reported.
*     An error will also reported if the access mode string supplied is not
*     valid.  Otherwise the function returns without further action.

*  Parameters:
*     acb
*        Pointer to The NDF entry in the ACB.
*     mode
*        Pointer to a null terminated string holding the requested access
*        mode; one of "READ", "WRITE" or "UPDATE" (case insensitive).
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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If UPDATE or WRITE access is requested, then check that WRITE access
   to the NDF is available. */
   if( astChrMatch( mode, "WRITE" ) || astChrMatch( mode, "UPDATE" ) ) {
      ndf1Chacc( acb, "WRITE", status );

/* No action is needed if READ access is requested. */
   } else if( astChrMatch( mode, "READ" ) ) {


/* Report an error if the "mode" value supplied is not recognised. */
   } else {
      *status = NDF__FATIN;
      msgSetc( "ROUTINE", "ndf1Chmod" );
      msgSetc( "BADMODE", mode );
      errRep( " ", "Function ^ROUTINE called with an invalid MODE "
              "parameter of '^BADMODE' (internal programming error).", status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Chmod", status );

}

