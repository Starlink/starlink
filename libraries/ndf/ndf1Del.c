#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "mers.h"
#include "star/util.h"

void ndf1Del( NdfACB **acb, int *status ){
/*
*+
*  Name:
*     ndf1Del

*  Purpose:
*     Perform a deletion operation on an ACB entry.

*  Synopsis:
*     void ndf1Del( NdfACB **acb, int *status )

*  Description:
*     This function performs a deletion operation on an entry in the ACB.
*     If the specified ACB entry describes a base NDF, then it and all
*     other ACB entries which refer to the same data object are annulled,
*     and the data object is erased.  If the ACB entry does not describe a
*     base NDF, then that ACB entry (alone) is annulled, and the data
*     object is not erased.

*  Parameters:
*     *acb
*        Pointer to the ACB entry on which the deletion operation is to be
*        performed. A value of zero is returned.
*     *status
*        The global status.

*  Notes:
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Side Effects:
*     -   If a data object is erased, then all NDF identifiers which refer
*     to it become invalid.

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
*     29-APR-2020 (DSB):
*        Unlock the mutex before calling ndf1ANl as it may attempt to
*        lock the mutex.

*-
*/

/* Local Variables: */
   NdfACB *acba;         /* ACB to be annulled */
   NdfACB *acbt;         /* ACB to be tested */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   int islot;            /* Slot index */
   int next;             /* Next active ACB slot number */
   int tstat;            /* Temporary status variable */

/* Save the "status" value and mark the error stack. */
   tstat = *status;
   errMark();

/* If the ACB entry does not refer to a base NDF, then simply annul the
   entry. */
   *status = SAI__OK;
   if( (*acb)->cut ) {
      ndf1Anl( acb, status );

/* Otherwise, obtain the index of the data object entry in the DCB and
   set its disposal mode to DELETE. The object will then be erased once
   its reference count drops to zero. */
   } else {
      dcb = (*acb)->dcb;
      star_strlcpy( dcb->dsp, "DELETE", sizeof( dcb->dsp ) );

/* Loop through all active entries in the ACB. */
      islot = -1;
      next = 0;
      NDF__ACB_LOCK_MUTEX;
      acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
      while( ( *status == SAI__OK ) && ( next != -1 ) ){
         islot = next;

/* Select those entries which refer to the data object to be erased and
   annul them (unlocking the mutex first since ndf1Anl may attempt to lock
   it). This process eventually reduces the object's reference count to
   zero, causing it to be erased. */
         if( acbt->dcb == dcb ) {
            acba = acbt;
            NDF__ACB_UNLOCK_MUTEX;
            ndf1Anl( &acba, status );
            NDF__ACB_LOCK_MUTEX;
         }
         acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
      }
      NDF__ACB_UNLOCK_MUTEX;
   }

/* Reset the initial ACB index to zero. */
   *acb = 0;

/* Annul any error if "status" was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ) {
      if( tstat != SAI__OK ) {
         errAnnul( status );
         *status = tstat;

/* Call error tracing function if appropriate. */
      } else {
         ndf1Trace( "ndf1Del", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

}

