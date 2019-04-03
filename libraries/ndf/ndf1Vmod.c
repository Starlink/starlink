#include <stdlib.h>
#include "sae_par.h"
#include "ndf_err.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Vmod( const char *mode, char *vmode, size_t vmode_length, int *status ){
/*
*+
*  Name:
*     ndf1Vmod

*  Purpose:
*     Validate an access mode string.

*  Synopsis:
*     void ndf1Vmod( const char *mode, char *vmode, size_t vmode_length,
*                    int *status )

*  Description:
*     This function validates an access mode string, returning an upper
*     case version if it is valid. Otherwise, an error is reported.

*  Parameters:
*     mode
*        Pointer to a null terminated string holding the access mode string
*        to be validated. Valid values are "READ", "UPDATE" or "WRITE"
*        (case insensitive).
*     vmode
*        Pointer to an array in which to return a null terminated string
*        holding the validated access mode string in upper case (not
*        returned if the "mode" value is invalid).
*     vmode_length
*        The length of the supplied 'vmode' array. This should include
*        room for the terminating null.
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

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check the access mode string supplied against each permitted value in
   turn, returning the appropriate validated value. */

/* ...READ access. */
   if( astChrMatch( mode, "READ" ) ) {
      ndf1Ccpy( "READ", vmode, vmode_length, status );

/* ...UPDATE access. */
   } else if( astChrMatch( mode, "UPDATE" ) ) {
      ndf1Ccpy( "UPDATE", vmode, vmode_length, status );

/* ...WRITE access. */
   } else if( astChrMatch( mode, "WRITE" ) ) {
      ndf1Ccpy( "WRITE", vmode, vmode_length, status );

/* If the access mode value was not recognised, then report an error. */
   } else {
      *status = NDF__MODIN;
      msgSetc( "BADMODE", mode );
      errRep( " ", "Invalid access mode '^BADMODE' specified (possible "
              "programming error).", status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Vmod", status );

}

