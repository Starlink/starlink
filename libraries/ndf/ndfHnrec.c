#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

void ndfHnrec_( int indf, int *nrec, int *status ){
/*
*+
*  Name:
*     ndfHnrec

*  Purpose:
*     Determine the number of NDF history records present.

*  Synopsis:
*     void ndfHnrec( int indf, int *nrec, int *status )

*  Description:
*     This function returns a count of the number of history records
*     present in an NDF.

*  Parameters:
*     indf
*        NDF identifier.
*     *nrec
*        Returned holding the number of history records.
*     *status
*        The global status.

*  Notes:
*     -  The number of records returned may be zero if a history component
*     exists but no history information has yet been entered.
*     -  An error will result if there is no history component present in
*     the NDF.

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

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* If OK, then obtain an index to the data object entry in the DCB. */
   if( *status == SAI__OK ) {
      dcb = acb->dcb;

/* Ensure that history information is available in the DCB. */
      ndf1Dh( dcb, status );
      if( *status == SAI__OK ) {

/* If there is no history component present, then report an error. */
         if( !dcb->hloc ) {
            *status = NDF__NOHIS;
            ndf1Dmsg( "NDF", dcb );
            errRep( " ", "There is no history component present in the NDF "
                    "structure ^NDF (possible programming error).", status );

/* Otherwise, return the record count from the DCB. */
         } else {
            *nrec = dcb->hnrec;
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfHnrec: Error determining the number of NDF history "
              "records present.", status );
      ndf1Trace( "ndfHnrec", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

