#include <pthread.h>
#include "sae_par.h"
#include "ndf1.h"
#include "mers.h"
#include "ndf_err.h"
#include "dat_err.h"

void ndf1UnlockLoc( HDSLoc *loc, int *status ){
/*
*+
*  Name:
*     ndf1UnlockLoc

*  Purpose:
*     Unlock an HDS locator.

*  Synopsis:
*     void ndf1UnlockLoc( HDSLoc *loc, int *status )

*  Description:
*     This function attempts to unlock the supplied HDS locator. It is a
*     wrapper for datUnlock that does nothing if the locator is already
*     unlocked (datUnlock reports an error).

*  Parameters:
*     loc
*        The locator to be unlocked.
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
   if( !loc ) return;

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();
   *status = SAI__OK;

/* See if the locator is locked by the current thread. */
   ival = datLocked( loc, 0, status );

/* If it is locked by the current thread, unlock it. */
   if( ival == 1 || ival == 3 ) {
      datUnlock( loc, 0, status );

/* If it is locked by a different thread, report an error. */
   } else if( ival != -1 && ival != 0 && *status == SAI__OK ) {
      *status = DAT__THREAD;
      datMsg( "O", loc );
      errRep( " ", "HDS object '^O' cannot be unlocked - it is locked by "
              "a different thread. ", status );
   }

/* Annul any error if STATUS was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;
      } else {

/* Call error tracing routine if appropriate. */
         ndf1Trace( "ndf1UnlockLoc", status );
      }
   } else {
      *status = tstat;
   }

/* Release the error stack. */
   errRlse();
}
