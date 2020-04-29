#include <pthread.h>
#include "sae_par.h"
#include "ndf1.h"
#include "mers.h"
#include "ndf_err.h"

void ndf1UnlockACB( NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1UnlockACB

*  Purpose:
*     Unlock an ACB so it can be locked subsequently by another thread.

*  Synopsis:
*     void ndf1UnlockACB( NdfACB *acb, int *status )

*  Description:
*     This function attempts to unlock the DCB associated with the
*     supplied ACB so it can later be locked using ndf1LockACB by another
*     thread. An error is reported if the DCB is currently locked by a
*     thread other than the current thread. If successfull, all other
*     ACBs associated with the same DCB are annulled. The context level
*     stored in the supplied ACB is set to a special value, NDF__INLIMBO,
*     indicating that the ACB is not in any context.

*  Parameters:
*     acb
*        The ACB to be unlocked.
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
*     29_APR-2020 (DSB):
*        Unlock the mutex before calling ndf1Anl as it may attempt to
*        lock the mutex.
*-
*/

/* Local variables: */
   NdfACB *acbt;
   int astat;
   int i;
   int islot;
   int next;
   int tstat;

/* Check an ACB pointer was supplied. */
   if( !acb ) return;

/* Initialise a status value for annulling ACBs. */
   astat = *status;

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();
   *status = SAI__OK;

/* Loop to examine each active ACB, annulling any that refer to the
   same DCB as the supplied ACB (except the supplied ACB itself). */
   next = 0;
   islot = -1;
   NDF__ACB_LOCK_MUTEX;
   acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
   while( ( *status == SAI__OK ) && ( next != -1 ) ){
      islot = next;
      if( acbt != acb && acbt->dcb == acb->dcb ) {
         NDF__ACB_UNLOCK_MUTEX;
         ndf1Anl( &acbt, &astat );
         NDF__ACB_LOCK_MUTEX;
      }
      acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
   }
   NDF__ACB_UNLOCK_MUTEX;

/* Attempt to unlock the DCB associated with the ACB. */
   ndf1UnlockDCB( acb->dcb, status );

/* Unlock all the HDS and ARY objects in the ACB */
   if( acb->qmtlc ) ndf1UnlockLoc( acb->qmtlc, status );
   if( acb->did ) ndf1UnlockAry( acb->did, status );
   if( acb->dmtid ) ndf1UnlockAry( acb->dmtid, status );
   if( acb->vid ) ndf1UnlockAry( acb->vid, status );
   if( acb->vmtid ) ndf1UnlockAry( acb->vmtid, status );
   if( acb->qid ) ndf1UnlockAry( acb->qid, status );
   if( acb->qmtid ) ndf1UnlockAry( acb->qmtid, status );

   for( i = 0; i < NDF__MXDIM; i++ ) {
      if( acb->admap[ i ] && acb->admid[ i ] ) ndf1UnlockAry( acb->admid[ i ], status );
      if( acb->avmap[ i ] && acb->avmid[ i ] ) ndf1UnlockAry( acb->avmid[ i ], status );
      if( acb->awmap[ i ] && acb->awmid[ i ] ) ndf1UnlockAry( acb->awmid[ i ], status );
   }

/* Change the context level stored in the ACB to a value that indicates the
   ACB is not in any context. */
   acb->ctx = NDF__INLIMBO;

/* If an error occurred while annulling any entry, but "status" has not
   been set, then transfer the bad "astat" value to "status". */
   if( *status == SAI__OK ) {
      if( astat != SAI__OK ) *status = astat;
   }

/* Annul any error if STATUS was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;
      } else {

/* Call error tracing routine if appropriate. */
         ndf1Trace( "ndf1UnlockACB", status );
      }
   } else {
      *status = tstat;
   }

/* Release the error stack. */
   errRlse();
}
