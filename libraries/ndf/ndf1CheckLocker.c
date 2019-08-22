#include <pthread.h>
#include "sae_par.h"
#include "ndf1.h"
#include "mers.h"
#include "ndf_err.h"

void ndf1CheckLocker( NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1CheckLocker

*  Purpose:
*     Check the current thread owns the lock on the supplied ACB.

*  Synopsis:
*     void ndf1CheckLocker( NdfACB *acb, int *status )

*  Description:
*     An error is reported if the curent thread does not own the lock on
*     the supplied ACB.

*  Parameters:
*     acb
*        The ACB to be checked.
*     status
*        The global status.

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
   const char *phrase;
   int lock_state;

/* Check inherited status. */
   if( *status != SAI__OK || !acb ) return;

/* Get information about any lock on the supplied ACB. */
   lock_state = ndf1Locked( acb );

/* Report an error if the ACB is not locked by the current thread. */
   if( lock_state <= 0 ) {
      *status = NDF__THREAD;
      ndf1Amsg( "NDF", acb );
      if( lock_state == 0 ){
         phrase = "not locked by any thread";
      } else {
         phrase = "locked by another thread";
      }
      errRepf( " ", "The NDF '^NDF' is %s (programming error).",
               status, phrase );
   }

/* Call error tracing routine if appropriate. */
   if( *status != SAI__ERROR ) ndf1Trace( "ndf1CheckLocker", status );
}

