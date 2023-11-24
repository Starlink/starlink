#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfBase_( int indf1, int *indf2, int *status ){
/*
*+
*  Name:
*     ndfBase

*  Purpose:
*     Obtain an identifier for a base NDF.

*  Synopsis:
*     void ndfBase( int indf1, int *indf2, int *status )

*  Description:
*     This function returns an identifier for the base NDF with which an
*     NDF section is associated.

*  Parameters:
*     indf1
*        Identifier for an existing NDF section (the function will also
*        work if this is already a base NDF).
*     *indf2
*        Returned holding the identifier for the base NDF with which the
*        section is associated.
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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfACB *acb1;         /* Pointer to input NDF entry in ACB */
   NdfACB *acb2;         /* Pointer to output NDF entry in ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */

/* Set an initial value for the "indf2" parameter. */
   *indf2 = NDF__NOID;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the input NDF identifier. */
   ndf1Impid( indf1, &acb1, status );
   if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb1->dcb;

/* Create a new base NDF entry in the ACB to describe it. */
      NDF__DCB_LOCK_MUTEX;
      ndf1Crnbn( dcb, &acb2, status );
      NDF__DCB_UNLOCK_MUTEX;
      if( *status == SAI__OK ) {

/* Transfer the access control flags from the old ACB entry to the new
   one. */
         acb2->access = acb1->access;

/* Export an identifier for the new base NDF. */
         *indf2 = ndf1Expid( ( NdfObject * ) acb2, status );

/* If an error occurred, then annul the new ACB entry. */
         if( *status != SAI__OK ) ndf1Anl( &acb2, status );
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfBase: Error obtaining an identifier for a base "
              "NDF.", status );
      ndf1Trace( "ndfBase", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

