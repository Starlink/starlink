#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfIsbas_( int indf, int *isbas, int *status ){
/*
*+
*  Name:
*     ndfIsbas

*  Purpose:
*     Enquire if an NDF is a base NDF.

*  Synopsis:
*     void ndfIsbas( int indf, int *isbas, int *status )

*  Description:
*     This function returns a logical value indicating whether the NDF
*     whose identifier is supplied is a base NDF (as opposed to an NDF
*     section).

*  Parameters:
*     indf
*        NDF identifier.
*     *isbas
*        Returned holding the whether the NDF is a base NDF.
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

/* Local Variables: */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );
   if( *status == SAI__OK ) {

/* The NDF is a base NDF if it is not a cut. */
      *isbas = ( !acb->cut );
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfIsbas: Error enquiring if an NDF is a base NDF.",
              status );
      ndf1Trace( "ndfIsbas", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

