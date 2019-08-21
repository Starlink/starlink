#include <pthread.h>
#include "sae_par.h"
#include "ndf1.h"
#include "mers.h"
#include "ndf_err.h"

void ndf1UnlockDCB( NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1UnlockDCB

*  Purpose:
*     Unlock a DCB so it can be locked subsequently by another thread.

*  Synopsis:
*     void ndf1UnlockDCB( NdfObject *object, int *status )

*  Description:
*     This function attempts to unlock the DCB so it can later be
*     locked by another thread. An error is reported if the supplied
*     DCB is currently locked by a thread other than the current thread.

*  Parameters:
*     dcb
*        The DCB to be locked.
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
   int i;
   int j;
   int tstat;

/* Check a DCB pointer was supplied. */
   if( !dcb ) return;

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();
   *status = SAI__OK;

/* Wait until we have sole access to the DCB. */
   pthread_mutex_lock( &(dcb->mutex) );

/* Do nothing if the DCB is currently unlocked. */
   if( dcb->locked ) {

/* If the DCB is locked by the current thread, unlock it. */
      if( pthread_equal( dcb->locker, pthread_self() ) ){
         dcb->locked = 0;

/* Unlock all HDS and ARY objects in the DCB. */
         if( dcb->loc ) ndf1UnlockLoc( dcb->loc, status );
         if( dcb->xloc ) ndf1UnlockLoc( dcb->xloc, status );
         for( i = 0; i < NDF__MXCCN; i++ ) {
            if( dcb->kc[ i ] && dcb->cloc[ i ] ) ndf1UnlockLoc( dcb->cloc[ i ], status );
         }
         if( dcb->qloc ) ndf1UnlockLoc( dcb->qloc, status );
         for( i = 0; i < NDF__MXDIM; i++ ) {
            if( dcb->ka && dcb->aloc[ i ] ) ndf1UnlockLoc( dcb->aloc[ i ], status );
            if( dcb->kax[ i ] && dcb->axloc[ i ] ) ndf1UnlockLoc( dcb->axloc[ i ], status );
            for( j = 0; j < NDF__MXACN; j++ ) {
               if( dcb->kac[ i ][ j ] && dcb->acloc[ i ][ j ] ) {
                  ndf1UnlockLoc( dcb->acloc[ i ][ j ], status );
               }
            }
            if( dcb->kad[ i ] && dcb->adid[ i ] ) ndf1UnlockAry( dcb->adid[ i ], status );
            if( dcb->kav[ i ] && dcb->avid[ i ] ) ndf1UnlockAry( dcb->avid[ i ], status );
            if( dcb->kaw[ i ] && dcb->awid[ i ] ) ndf1UnlockAry( dcb->awid[ i ], status );
         }

         if( dcb->hloc ) ndf1UnlockLoc( dcb->hloc, status );
         if( dcb->hrloc ) ndf1UnlockLoc( dcb->hrloc, status );

         if( dcb->did ) ndf1UnlockAry( dcb->did, status );
         if( dcb->vid ) ndf1UnlockAry( dcb->vid, status );
         if( dcb->qid ) ndf1UnlockAry( dcb->qid, status );

/* Report an error if the object is locked by another thread. */
      } else {
         *status = NDF__FATIN;
         ndf1Dmsg( "NDF", dcb );
         errRep( " ", "The NDF '^NDF' is locked by another thread "
                 "(programming error).", status );
      }
   }

/* Allow other threads to access the DCB. */
   pthread_mutex_unlock( &(dcb->mutex) );

/* Annul any error if STATUS was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;
      } else {

/* Call error tracing routine if appropriate. */
         ndf1Trace( "ndf1UnlockDCB", status );
      }
   } else {
      *status = tstat;
   }

/* Release the error stack. */
   errRlse();
}
