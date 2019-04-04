#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfIsacc_( int indf, const char *access, int *isacc, int *status ){
/*
*+
*  Name:
*     ndfIsacc

*  Purpose:
*     Determine whether a specified type of NDF access is available.

*  Synopsis:
*     void ndfIsacc( int indf, const char *access, int *isacc, int *status )

*  Description:
*     This function determines whether a specified type of access to an NDF
*     is available, or whether it has been disabled. If access is not
*     available, then any attempt to access the NDF in this way will fail.

*  Parameters:
*     indf
*        NDF identifier.
*     access
*        Pointer to a null terminated string holding the type of NDF access
*        required: "BOUNDS", "DELETE", "SHIFT", "TYPE" or "WRITE" (see the
*        Notes section for details).
*     *isacc
*        Returned holding the whether the specified type of access is
*        available.
*     *status
*        The global status.

*  Notes:
*     The valid access types control the following operations on the NDF:
*     -  "BOUNDS" permits the pixel-index bounds of a base NDF to be
*     altered.
*     -  "DELETE" permits deletion of the NDF.
*     -  "SHIFT" permits pixel-index shifts to be applied to a base NDF.
*     -  "TYPE" permits the data types of an NDF's components to be
*     altered.
*     -  "WRITE" permits new values to be written to the NDF, and the state
*     of any of its components to be reset.

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

/* Determine whether access is available. */
   ndf1Accok( acb, access, isacc, status );

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfIsacc: Error determining whether a specified type "
              "of NDF access is available.", status );
      ndf1Trace( "ndfIsacc", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

