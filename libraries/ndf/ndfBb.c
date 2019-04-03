#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfBb_( int indf, unsigned char *badbit, int *status ){
/*
*+
*  Name:
*     ndfBb

*  Purpose:
*     Obtain the bad-bits mask value for the quality component of an NDF.

*  Synopsis:
*     void ndfBb( int indf, unsigned char *badbit, int *status )

*  Description:
*     This function returns an unsigned byte value representing the bad-
*     bits mask associated with the quality component of an NDF.

*  Parameters:
*     indf
*        NDF identifier.
*     *badbit
*        Returned holding the unsigned byte bad-bits mask.
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

/* Local Variables: */
   NdfACB *acb;          /* Pointer to the NDF entry in the ACB */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Obtain the effective bad-bits value. */
   ndf1Gtbb( acb, badbit, status );

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfBb: Error obtaining the bad-bits mask value for the "
              "quality component of an NDF.", status );
      ndf1Trace( "ndfBb", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

