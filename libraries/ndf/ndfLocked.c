#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

int ndfLocked_( int indf, int *status ){
/*
*+
*  Name:
*     ndfLocked

*  Purpose:
*     Return information about any lock on an NDF.

*  Synopsis:
*     result = ndfLocked( int indf, int *status )

*  Description:
*     This functions returns information about any lock on the base NDF
*     associated with the supplied NDF identifier. NDFs are locked and
*     unlocked using functions ndfLock and ndfUnlock.

*  Parameters:
*     indf
*        NDF identifier.
*     *status
*        The global status.

*  Returned Value:
*     0 - the base NDF is unlocked
*     1 - the base NDF is locked by the current thread
*    -1 - the base NDF is locked by some other thread

*  Notes:
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory
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
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version.

*-
*/

/* Local Variables: */
   int result;           /* The val;ue to return */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Begin a new error reporting context. */
   errBegin( status );

/* Import the NDF identifier. Cannot use ndf1Impid since the supplied
   identifier may be unlocked, which would cause ndf1Impid to report an
   error. So first convert the identifier to an ACB pointer. */
   acb = (NdfACB *) ndf1Id2ac( indf, 1 );

/* If a valid ACB pointer was not returned, then report an error. */
   if( acb == NULL ) {
      *status = NDF__IDINV;
      msgSeti( "INDF", indf );
      errRep( " ", "NDF identifier invalid; its value is ^INDF (possible "
              "programming error).", status );
   }

/* Get information about any lock on the supplied ACB. */
   result = ndf1Locked( acb );

/* If an error occurred, report context information and call the error
   tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfLocked: Error getting lock information from "
              "an NDF.", status );
      ndf1Trace( "ndfLocked", status );
   }

/* End the error reporting context. */
   errEnd( status );

/* Restablish the original AST status pointer */
   NDF_FINAL

/* Returen the result. */
   return result;
}

