#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Accok( NdfACB *acb, const char *access, int *ok, int *status ){
/*
*+
*  Name:
*     ndf1Accok

*  Purpose:
*     Determine whether a specified type of ACB access is available.

*  Synopsis:
*     void ndf1Accok( NdfACB *acb, const char *access, int *ok, int *status )

*  Description:
*     This function returns a logical value indicating whether the
*     specified mode of access to an NDF entry in the ACB is permitted by
*     the current setting of the ACB access control flags.

*  Parameters:
*     acb
*        Pointer to the NDF entry in the ACB.
*     access
*        Pointer to a null terminated string holding the type of access
*        required (case insensitive).
*     *ok
*        Returned holding the whether the specified type of access is
*        available.
*     *status
*        The global status.

*  Notes:
*     BOUNDS and SHIFT access is always permitted if the NDF is not a base
*     NDF, regardless of the state of the corresponding access control
*     flags.

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

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Test the requested access type against each permitted value in turn
   and obtain the value of the associated access control flag. */

/* ...BOUNDS access. */
   if( astChrMatch( access, "BOUNDS" ) ) {
      *ok = ( ( acb->access & NDF__ACC_BOUND ) || acb->cut );

/* ...DELETE access. */
   } else if( astChrMatch( access, "DELETE" ) ) {
      *ok = ( acb->access & NDF__ACC_DELET );

/* ...SHIFT access. */
   } else if( astChrMatch( access, "SHIFT" ) ) {
      *ok = ( ( acb->access & NDF__ACC_SHIFT ) || acb->cut );

/* ...TYPE access. */
   } else if( astChrMatch( access, "TYPE" ) ) {
      *ok = ( acb->access & NDF__ACC_TYPE );

/* ...WRITE access. */
   } else if( astChrMatch( access, "WRITE" ) ) {
      *ok = ( acb->access & NDF__ACC_WRITE );

/* If the access type was not recognised, then report an error. */
   } else {
      *status = NDF__ACCIN;
      msgSetc( "BADACC", access );
      errRep( " ", "Invalid access type '^BADACC' specified (possible "
              "programming error).", status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Accok", status );

}

