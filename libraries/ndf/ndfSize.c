#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfSize_( int indf, size_t *npix, int *status ){
/*
*+
*  Name:
*     ndfSize

*  Purpose:
*     Determine the size of an NDF.

*  Synopsis:
*     void ndfSize( int indf, size_t *npix, int *status )

*  Description:
*     This function returns the number of pixels in the NDF whose
*     identifier is supplied (i.e. the product of its dimensions).

*  Parameters:
*     indf
*        NDF identifier.
*     *npix
*        Returned holding the number of pixels in the NDF.
*     *status
*        The global status.

*  Notes:
*     -  If this function is called with "status" set, then a value of 1
*     will be returned for the "npix" parameter, although no further
*     processing will occur.  The same value will also be returned if the
*     function should fail for any reason.

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

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Check inherited global status. */
   if( *status == SAI__OK ) {

/* Import the NDF identifier. */
      ndf1Impid( indf, &acb, status );

/* Obtain the NDF size from its data array component. */
      if( *status == SAI__OK ) arySize( acb->did, npix, status );

/* If an error occurred, then report context information and call the
   error tracing function. */
      if( *status != SAI__OK ) {
         errRep( " ", "ndfSize: Error determining the size of an NDF.", status );
         ndf1Trace( "ndfSize", status );
      }
   }

/* Under error conditions, return a "safe" value of "npix". */
   if( *status != SAI__OK ) *npix = 1;

/* Restablish the original AST status pointer */
   NDF_FINAL

}

