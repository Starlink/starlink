#include <pthread.h>
#include "sae_par.h"
#include "ndf1.h"
#include "mers.h"
#include "ndf_err.h"

void ndf1LockACB( NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1LockACB

*  Purpose:
*     Lock an ACB for use by the current thread.

*  Synopsis:
*     void ndf1LockACB( NdfACB *acb, int *status )

*  Description:
*     This function attempts to lock the DCB associated with the
*     supplied ACB so it can be used only by the current thread. The ACB
*     should previously have been unlocked using ndf1UnlockACB - an error
*     is reported otherwise. If successfull, the context level stored in
*     the supplied ACB is set to the context level in the current thread
*     (each thread has its own context level stored in its thread-specific
*     data area).

*  Parameters:
*     acb
*        The ACB to be locked.
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
   int tstat;

/* Check an ACB pointer was supplied. */
   if( !acb ) return;

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();
   *status = SAI__OK;

/* Unlocking an ACB puts it into the INLIMBO context. If the supplied ACB
   is in some other context, then it is currently locked. Report an error
   unless it is locked by the current thread. */
   if( acb->ctx != NDF__INLIMBO && ndf1Locked( acb ) != 1 ){
      *status = NDF__THREAD;
      ndf1Amsg( "NDF", acb );
      errRepf( " ", "The supplied identifier (%d) for NDF '^NDF' has not "
              "been unlocked (programming error).", status,
              ((NdfObject *)acb)->check );

/* Othrwise, attempt to lock the DCB associated with the ACB. */
   } else {
      ndf1LockDCB( acb->dcb, status );
   }

/* If successfull, lock all the ARY and HDS objects in the ACB. */
   if( *status == SAI__OK ) {
      if( acb->qmtlc ) ndf1LockLoc( acb->qmtlc, status );
      if( acb->did ) ndf1LockAry( acb->did, status );
      if( acb->dmtid ) ndf1LockAry( acb->dmtid, status );
      if( acb->vid ) ndf1LockAry( acb->vid, status );
      if( acb->vmtid ) ndf1LockAry( acb->vmtid, status );
      if( acb->qid ) ndf1LockAry( acb->qid, status );
      if( acb->qmtid ) ndf1LockAry( acb->qmtid, status );

      for( i = 0; i < NDF__MXDIM; i++ ) {
         if( acb->admap[ i ] && acb->admid[ i ] ) ndf1LockAry( acb->admid[ i ],
                                                               status );
         if( acb->avmap[ i ] && acb->avmid[ i ] ) ndf1LockAry( acb->avmid[ i ],
                                                               status );
         if( acb->awmap[ i ] && acb->awmid[ i ] ) ndf1LockAry( acb->awmid[ i ],
                                                               status );
      }
   }

/* If successful, get a  pointer to the thread specific data for
   the current thread and then change the context level stored in
   the ACB to the context level in the current thread. */
   if( *status == SAI__OK ) {
      NDF_GETTSD;
      acb->ctx = NDF_TSD(acbIdctx);
   }

/* Annul any error if STATUS was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;
      } else {

/* Call error tracing routine if appropriate. */
         ndf1Trace( "ndf1LockACB", status );
      }
   } else {
      *status = tstat;
   }

/* Release the error stack. */
   errRlse();
}
