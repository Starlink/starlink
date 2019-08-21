#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "ndf_ast.h"
#include "mers.h"

void ndfGtwcs_( int indf, AstFrameSet **iwcs, int *status ){
/*
*+
*  Name:
*     ndfGtwcs

*  Purpose:
*     Obtain world coordinate system information from an NDF.

*  Synopsis:
*     void ndfGtwcs( int indf, AstFrameSet **iwcs, int *status )

*  Description:
*     This function obtains information about the world coordinate systems
*     associated with an NDF and returns an AST pointer to a FrameSet which
*     contains this information. The information may then be accessed using
*     functions from the AST library (SUN/210).

*  Parameters:
*     indf
*        NDF identifier.
*     *iwcs
*        Returned holding the an AST pointer to a FrameSet which contains
*        information about the world coordinate systems associated with the
*        NDF.
*     *status
*        The global status.

*  Notes:
*     - It is the caller's responsibility to annul the AST pointer issued
*     by this function (e.g. by calling "astAnnul") when it is no longer
*     required. The NDF_ system will not perform this task itself.
*     - If this function is called with "status" set, then a value of
*     AST__NULL will be returned for the "iwcs" parameter, although no
*     further processing will occur. The same value will also be returned
*     if the function should fail for any reason. The AST__NULL constant is
*     defined in the header file "ndf_ast.h".

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

/* Initialise the returned AST_ pointer. */
   *iwcs = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Read the WCS information from the NDF. */
   ndf1Rdwcs( acb, iwcs, status );

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfGtwcs: Error obtaining world coordinate system "
              "information from an NDF.", status );
      ndf1Trace( "ndfGtwcs", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

