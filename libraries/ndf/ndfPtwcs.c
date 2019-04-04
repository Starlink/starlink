#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "ndf_ast.h"
#include "mers.h"

void ndfPtwcs_( const AstFrameSet *iwcs, int indf, int *status ){
/*
*+
*  Name:
*     ndfPtwcs

*  Purpose:
*     Store world coordinate system information in an NDF.

*  Synopsis:
*     void ndfPtwcs( AstFrameSet *iwcs, int indf, int *status )

*  Description:
*     This function stores new world coordinate system (WCS) information in
*     an NDF, over-writing any already present.

*  Parameters:
*     iwcs
*        An AST pointer to a FrameSet (SUN/210) containing information
*        about the new world coordinate systems to be associated with the
*        NDF.
*     indf
*        NDF identifier.
*     *status
*        The global status.

*  Notes:
*     - This function may only be used to store WCS information in a base
*     NDF. If an NDF section is supplied, it will simply return without
*     action.
*     - The AST FrameSet supplied must conform to the various restrictions
*     imposed by the NDF_ system (e.g. on the nature of its base Frame). An
*     error will result if any of these restrictions is not met.
*     - This function makes a copy of the information in the FrameSet
*     supplied, so the original FrameSet may subsequently be modified
*     without affecting the behaviour of the NDF_ system.

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
   NdfACB *acb;          /* Pointer to the NDF entry in the ACB */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Check that WRITE access to the NDF is available. */
   ndf1Chacc( acb, "WRITE", status );

/* Write the information to the NDF (this also validates it and makes a
   copy). */
   ndf1Wrwcs( (AstFrameSet *) iwcs, acb, status );

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfPtwcs: Error storing world coordinate system "
              "information in an NDF.", status );
      ndf1Trace( "ndfPtwcs", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

