#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfClone_( int indf1, int *indf2, int *status ){
/*
*+
*  Name:
*     ndfClone

*  Purpose:
*     Clone an NDF identifier.

*  Synopsis:
*     void ndfClone( int indf1, int *indf2, int *status )

*  Description:
*     This function produces a "cloned" copy of an NDF identifier (i.e. it
*     produces a new identifier describing an NDF with identical attributes
*     to the original).

*  Parameters:
*     indf1
*        NDF identifier to be cloned.
*     *indf2
*        Returned holding the cloned identifier.
*     *status
*        The global status.

*  Notes:
*     -  If this function is called with "status" set, then a value of
*     NDF__NOID will be returned for the "indf2" parameter, although no
*     further processing will occur. The same value will also be returned
*     if the function should fail for any reason. The NDF__NOID constant is
*     defined in the header file "ndf.h".

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
   NdfACB *acb1;         /* Original ACB */
   NdfACB *acb2;         /* Cloned ACB */

/* Set an initial value for the "indf2" parameter. */
   *indf2 = NDF__NOID;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the original NDF identifier. */
   ndf1Impid( indf1, &acb1, status );

/* Produce a cloned copy of its ACB entry. */
   ndf1Cln( acb1, &acb2, status );

/* Export an identifier for the new NDF. */
   *indf2 = ndf1Expid( ( NdfObject * ) acb2, status );

/* If an error occurred, then reset the "indf2" parameter to NDF__NOID,
   report context information and call the error tracing function. */
   if( *status != SAI__OK ) {
      *indf2 = NDF__NOID;
      errRep( " ", "ndfClone: Error cloning an NDF identifier.", status );
      ndf1Trace( "ndfClone", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

