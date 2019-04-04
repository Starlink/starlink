#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "ndf1_types.h"

void ndfValid_( int indf, int *valid, int *status ){
/*
*+
*  Name:
*     ndfValid

*  Purpose:
*     Determine whether an NDF identifier is valid.

*  Synopsis:
*     void ndfValid( int indf, int *valid, int *status )

*  Description:
*     This function determines whether an NDF identifier is valid (i.e.
*     associated with an NDF).

*  Parameters:
*     indf
*        Identifier to be tested.
*     *valid
*        Returned holding the whether the identifier is valid.
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
   NdfACB *acb;          /* Pointer to associated ACB entry */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Try to convert the identifier into an ACB pointer. */
   acb = (NdfACB *) ndf1Id2ac( indf, 1 );

/* Note whether the attempt succeeded; it did not if a NULL value
   was returned. */
   *valid = ( acb != NULL );

/* Restablish the original AST status pointer */
   NDF_FINAL

}

