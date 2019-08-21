#include <stdlib.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"

void ndf1Vstat( const char *state, char *vstate, size_t vstate_length,
                int *status ){
/*
*+
*  Name:
*     ndf1Vstat

*  Purpose:
*     Validate an NDF state string.

*  Synopsis:
*     void ndf1Vstat( const char *state, char *vstate, size_t
*                     vstate_length, int *status )

*  Description:
*     This function validates an NDF state string, returning an upper case
*     version if it is valid. Otherwise, an error is reported.

*  Parameters:
*     state
*        Pointer to a null terminated string holding the NDF state string
*        to be validated. Valid values are "OLD", "NEW" or "UNKNOWN" (case
*        insensitive).
*     vstate
*        Pointer to an array in which to return a null terminated string
*        holding the validated NDF state string in upper case (not returned
*        if the "state" value is invalid).
*     vstate_length
*        The length of the supplied 'vstate' array. This should include
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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check the NDF state string supplied against each permitted value in
   turn, returning the appropriate validated value. */

/* NDF exists: */
   if( ndf1Simlr( state, 1, 0, "OLD", NDF__MINAB ) ) {
      ndf1Ccpy( "OLD", vstate, vstate_length, status );

/* NDF doesn't exist: */
   } else if( ndf1Simlr( state, 1, 0, "NEW", NDF__MINAB ) ) {
      ndf1Ccpy( "NEW", vstate, vstate_length, status );

/* NDF's existence unknown: */
   } else if( ndf1Simlr( state, 1, 0, "UNKNOWN", NDF__MINAB ) ) {
      ndf1Ccpy( "UNKNOWN", vstate, vstate_length, status );

/* If the NDF state was not recognised, then report an error. */
   } else {
      *status = NDF__STAIN;
      msgSetc( "BADSTATE", state );
      errRep( " ", "Invalid NDF state '^BADSTATE' specified (possible "
              "programming error).", status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Vstat", status );

}

