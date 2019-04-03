#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfAnorm_( int indf, int iaxis, int *norm, int *status ){
/*
*+
*  Name:
*     ndfAnorm

*  Purpose:
*     Obtain the logical value of an NDF axis normalisation flag.

*  Synopsis:
*     void ndfAnorm( int indf, int iaxis, int *norm, int *status )

*  Description:
*     This function returns a logical value for the normalisation flag
*     associated with an NDF axis.

*  Parameters:
*     indf
*        NDF identifier.
*     iaxis
*        Number of the axis whose normalisation flag value is required.
*     *norm
*        Returned holding the normalisation flag value.
*     *status
*        The global status.

*  Notes:
*     -  A value of zero may be supplied for the "iaxis" parameter, in
*     which case the function will return the logical "OR" of the
*     normalisation flag values for all the NDF's axes.

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
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   int iax;              /* Loop counter for axes */
   int iax1;             /* First axis to process */
   int iax2;             /* Last axis to process */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Check the axis number for validity. */
   ndf1Van( acb, iaxis, 1, &iax1, &iax2, status );
   if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB and initialise
   the returned result. */
      dcb = acb->dcb;
      *norm = 0;

/* Loop to inspect each requested axis, ensuring that normalisation
   information is available in the DCB. */
      for( iax = iax1; iax <= iax2; iax++ ){
         ndf1Dan( iax, dcb, status );
         if( *status == SAI__OK ) {

/* Set the returned result to non-zero and quit checking when the first
   non-zero normalisation value is obtained. */
            if( dcb->anrm[ iax ] ) {
               *norm = 1;
               break;
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfAnorm: Error obtaining the logical value of an NDF "
              "axis normalisation flag.", status );
      ndf1Trace( "ndfAnorm", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

