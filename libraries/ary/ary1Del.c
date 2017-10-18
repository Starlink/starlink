#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"
#include <string.h>

void ary1Del( AryACB **acb, int *status ) {
/*
*+
*  Name:
*     ary1Del

*  Purpose:
*     Perform a deletion operation on an ACB entry.

*  Synopsis:
*     void ary1Del( AryACB **acb, int *status )

*  Description:
*     This function performs a deletion operation on an entry in the ACB.
*     If the specified ACB entry describes a base array, then it and
*     all other ACB entries which refer to the same data object are
*     annulled, and the data object is erased.  If the ACB entry does
*     not describe a base array, then that ACB entry (alone) is
*     annulled, and the data object is not erased.

*  Parameters:
*     acb
*        The ACB on which the deletion operation is to be performed.
*        A value of NULL is returned.
*     status
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Side Effects:
*     -   If a data object is erased, then all array identifiers which
*     refer to it become invalid.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local variables: */
   AryACB *acbt;              /* ACB entry to be tested */
   AryDCB *dcb;               /* Index to data object entry in the DCB */
   int iacbt;                 /* Index of ACB slot to be tested */
   int next;                  /* Next active ACB slot number */
   int tstat;                 /* Temporary status variable */

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();

/* If the ACB entry does not refer to a base array, then simply annul the
   entry. */
   *status = SAI__OK;
   if( (*acb)->cut ){
      *acb = ary1Anl( *acb, status );

/* Otherwise, obtain the index of the data object entry in the DCB and set
   its disposal mode to DELETE. The object will then be erased once its
   reference count drops to zero. */
   } else {
      dcb = (*acb)->dcb;
      strcpy( dcb->dispose, "DELETE" );

/* Loop through all active entries in the ACB. We lock a mutex first to
   ensure that no other thread is currently accessing the slot array. */
      ARY__ACB_LOCK_MUTEX;
      iacbt = -1;
      next = 0;
      while( 1 ){
         acbt = ary1Nxtsl( ARY__ACBTYPE, iacbt, &next, status );
         if( ( *status == SAI__OK ) && ( next != -1 ) ){
            iacbt = next;

/* Select those entries which refer to the data object to be erased and
   annul them. This process eventually reduces the object's reference
   count to zero, causing it to be erased. */
            if( acbt->dcb == dcb ) acbt = ary1Anl( acbt, status );

         } else {
            break;
         }
      }
      ARY__ACB_UNLOCK_MUTEX;
   }

/* Reset the initial ACB pointer to NULL. */
   *acb = NULL;

/* Annul any error if "status" was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;

/* Call error tracing routine if appropriate. */
      } else {
         ary1Trace( "ary1Del", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

}
