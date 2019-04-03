#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfReset_( int indf, const char *comp, int *status ){
/*
*+
*  Name:
*     ndfReset

*  Purpose:
*     Reset an NDF component to an undefined state.

*  Synopsis:
*     void ndfReset( int indf, const char *comp, int *status )

*  Description:
*     This function resets a component of an NDF so that its value becomes
*     undefined. It may be used to remove unwanted optional NDF components.
*     Its use is also advisable before making format changes to an NDF if
*     retention of the existing values is not required (e.g. before
*     changing the data type of an array component with the ndfStype
*     function); this will avoid the cost of converting the existing
*     values.

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        component to be reset; any NDF component name is valid. No error
*        will result if the component is already undefined.
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be supplied in
*     which case each component will be reset in turn.
*     -  Specifying a component name of "*" will cause all components,
*     except for HISTORY and extensions, to be reset. The former may be
*     reset by specifying its name explicitly, while all extensions may be
*     removed by specifying a component name of "EXTENSION".
*     -  Individual extensions may be removed from an NDF with the ndfXdel
*     function.
*     -  This function may only be used to reset components of a base NDF.
*     If an NDF section is supplied, then it will return without action. No
*     error will result.
*     -  An array component of an NDF cannot be reset while it is mapped
*     for access. Neither can an NDF's axis component be reset while any
*     axis array is mapped for access. This function will fail if either of
*     these conditions occurs.

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
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Check that WRITE access to the NDF is available. */
   ndf1Chacc( acb, "WRITE", status );

/* Reset the NDF component(s). */
   ndf1Rst( acb, comp, status );

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfReset: Error resetting an NDF component to an "
              "undefined state.", status );
      ndf1Trace( "ndfReset", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

