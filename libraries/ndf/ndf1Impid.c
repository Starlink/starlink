#include "sae_par.h"
#include "dat_par.h"
#include "ndf_err.h"
#include "ndf1.h"
#include "mers.h"

void ndf1Impid( int indf, NdfACB **acb, int *status ){
/*
*+
*  Name:
*     ndf1Impid

*  Purpose:
*     Import an identifier.

*  Synopsis:
*     void ndf1Impid( int indf, NdfACB **acb, int *status )

*  Description:
*     This function converts an NDF identifier, previously issued by
*     ndf1Expid, into a  pointer to an ACB. The identifier value is
*     fully checked and an error is reported if it is not valid.

*  Parameters:
*     indf
*        NDF identifier.
*     *acb
*        Pointer to an entry in the ACB.
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

/* Set an initial value for the "acb" parameter. */
   *acb = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Convert the identifier to an ACB pointer. */
   *acb = (NdfACB *) ndf1Id2ac( indf, 1 );

/* If a valid ACB pointer was not returned, then report an error. */
   if( !ndf1IsValid( (NdfObject *) *acb ) ) {
      *status = NDF__IDINV;
      msgSeti( "INDF", indf );
      errRep( " ", "NDF identifier invalid; its value is ^INDF (possible "
              "programming error).", status );
   }

/* Check the NDF is locked by the current thread. */
   ndf1CheckLocker( *acb, status );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Impid", status );

}

