#include <pthread.h>
#include "sae_par.h"
#include "ndf1.h"
#include "mers.h"
#include "ndf_err.h"

int ndf1Locked( NdfACB *acb ){
/*
*+
*  Name:
*     ndf1Locked

*  Purpose:
*     Returns information about any lock on the supplied ACB.

*  Synopsis:
*     int ndf1Locked( NdfACB *acb )

*  Description:
*     This functions returns information about any lock on the
*     supplied ACB. The lock status of an ACB is inherited from the
*     associated DCB. This means that all ACBs associated with the same
*     DCB will have the same lock status.

*  Parameters:
*     acb
*        The ACB to be checked.

*  Returned Value:
*     0 - the supplied ACB is unlocked
*     1 - the supplied ACB is locked by the current thread
*    -1 - the supplied ACB is locked by some other thread

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
   NdfDCB *dcb;
   int result = 0;

/* Check an ACB was supplied, and that the acb has a dcb. */
   if( !acb || !(acb->dcb) ) return result;

/* Wait until we have sole access to the DCB. */
   dcb = acb->dcb;
   pthread_mutex_lock( &(dcb->mutex) );

/* Get the value to return. */
   if( dcb->locked ){
      if( pthread_equal( dcb->locker, pthread_self() ) ){
         result = 1;
      } else {
         result = -1;
      }
   }

/* Allow other threads to access the DCB. */
   pthread_mutex_unlock( &(dcb->mutex) );

/* Return the result. */
   return result;
}

