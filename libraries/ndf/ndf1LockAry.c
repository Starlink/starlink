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
*     wrapper for aryLock that does nothing if the identifier is already
*     locked (aryLock reports an error).

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

/* See if the identifier is locked by the current thread. */
   ival = aryLocked( ary, status );

/* If it is unlocked, lock it. */
   if( ival == 0 ) {
      aryLock( ary, 0, status );

/* Report an error unless the identifier is already locked by the current
   thread. */
   } else if( ival != -1 && ival != 1 && *status == SAI__OK ) {
      *status = ARY__THREAD;
      aryMsg( "O", ary );
      errRep( " ", "ARY array '^O' cannot be locked - it is already "
              "locked by a different thread. ", status );
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
