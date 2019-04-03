#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"

void ndfXnumb_( int indf, int *nextn, int *status ){
/*
*+
*  Name:
*     ndfXnumb

*  Purpose:
*     Determine the number of extensions in an NDF.

*  Synopsis:
*     void ndfXnumb( int indf, int *nextn, int *status )

*  Description:
*     This function returns the number of extensions present in the NDF
*     whose identifier is supplied.

*  Parameters:
*     indf
*        NDF identifier.
*     *nextn
*        Returned holding the number of extensions present.
*     *status
*        The global status.

*  Notes:
*     If this function is called with "status" set, then a value of zero
*     will be returned for the "nextn" parameter, although no further
*     processing will occur. The same value will also be returned if the
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
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */

/* Set an initial default value for the "nextn" parameter. */
   *nextn = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );
   if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Ensure that extension (MORE) structure information is available in
   the DCB. */
      ndf1Dx( dcb, status );
      if( *status == SAI__OK ) {

/* If the extension (MORE) structure does not exist, then there can be
   no extensions present. Otherwise, enquire how many extension
   components there are. */
         if( dcb->xloc ) datNcomp( dcb->xloc, nextn, status );
      }
   }

/* If an error occurred, then return a zero value for "nextn". */
   if( *status != SAI__OK ) {
      *nextn = 0;

/* Call error tracing function and exit. */
      ndf1Trace( "ndfXnumb", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

