#include <pthread.h>
#include "sae_par.h"
#include "ndf1.h"
#include "mers.h"
#include "ndf_err.h"
#include "ary_err.h"

void ndf1LockAry( Ary *ary, int *status ){
/*
*+
*  Name:
*     ndf1LockAry

*  Purpose:
*     Lock an ARY identifier.

*  Synopsis:
*     void ndf1LockAry( Ary *ary, int *status )

*  Description:
*     This function attempts to lock the supplied ARY identifier. It is a
*     wrapper for aryLock.

*  Parameters:
*     ary
*        The ARY identifier to be locked.
*     status
*        The global status.

*  Notes:
*     -  This function attempts to execute even if an error has already
*     occurred.

*  Copyright:
*      Copyright (C) 2019 East Asian Observatory
*      All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (EAO)

*  History:
*     27-FEB-2019 (DSB):
*        Original version.
*     23-APR-2020 (DSB):
*        Do not test the lock first since the test ignores the MCB within
*        the ARY object. Instead just lock the ARY regardless of its
*        initial lock state (ARY will not report an error if it is already
*        locked).

*-
*/

/* Local variables: */
   int ival;
   int tstat;

/* Check a pointer was supplied. */
   if( !ary ) return;

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();
   *status = SAI__OK;

/* Attempt to lock the ARY for read/write. This will do nothing if it is
   already locked by the current thread. */
   aryLock( ary, 0, status );

/* See if the identifier is now locked by the current thread. */
   ival = aryLocked( ary, status );
   if( ival != 1 && *status == SAI__OK ) {
      *status = ARY__THREAD;
      aryMsg( "O", ary );
      errRep( " ", "Failed to lock ARY array '^O' (NDF programming "
              "error).", status );
   }

/* Annul any error if STATUS was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;
      } else {

/* Call error tracing routine if appropriate. */
         ndf1Trace( "ndf1LockAry", status );
      }
   } else {
      *status = tstat;
   }

/* Release the error stack. */
   errRlse();
}
