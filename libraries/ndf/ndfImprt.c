#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfImprt_( HDSLoc *loc, int *indf, int *status ){
/*
*+
*  Name:
*     ndfImprt

*  Purpose:
*     Import an NDF into the NDF_ system from HDS.

*  Synopsis:
*     void ndfImprt( HDSLoc *loc, int *indf, int *status )

*  Description:
*     This function imports an NDF into the NDF_ system from HDS and issues
*     an identifier for it. The NDF may then be manipulated by the NDF_
*     functions.

*  Parameters:
*     loc
*        HDS locator to an NDF structure.
*     *indf
*        Returned holding the NDF identifier.
*     *status
*        The global status.

*  Notes:
*     -  THIS ROUTINE IS OBSOLETE. The same effect can be obtained by
*     calling ndfFind with its second (NAME) parameter set to a blank
*     string.

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

/* Local Variables: */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */

/* Set an initial value for the "indf" parameter. */
   *indf = NDF__NOID;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF structure into the ACB. */
   ndf1Imp( loc, &acb, status );

/* Export an identifier for the new NDF. */
   *indf = ndf1Expid( ( NdfObject * ) acb, status );

/* If an error occurred, then reset the "indf" value and report context
   information. */
   if( *status != SAI__OK ) {
      *indf = NDF__NOID;
      errRep( " ", "ndfImprt: Error importing an NDF into the NDF_ system "
              "from HDS.", status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndfImprt", status );

/* Restablish the original AST status pointer */
   NDF_FINAL

}

