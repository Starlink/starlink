#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ary.h"

void ndf1Qimp( NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1Qimp

*  Purpose:
*     Import information about an NDF's quality component into the ACB.

*  Synopsis:
*     void ndf1Qimp( NdfACB *acb, int *status )

*  Description:
*     This function imports information about an NDF's quality component so
*     that it is available in the ACB. No action is taken if this
*     information is already available. Otherwise, DCB quality information
*     is first acquired. Then, if the quaility array exists, ARY_ system
*     identifiers for relevant sections of it are entered into all the ACB
*     entries which refer to that NDF data object. If the quality array
*     does not exist, then null identifiers (value NULL) are entered
*     instead. The NDF is identified to this function by its ACB entry.

*  Parameters:
*     acb
*        Pointer to the NDF's ACB entry.
*     *status
*        The global status.

*  Notes:
*     -  This function should normally be used instead of a direct call to
*     ndf1Dq followed by updating of a single ACB entry.  It ensures that
*     all the ACB entries which refer to the quality object are updated
*     simultaneously so that their state reflects that of the actual data
*     object.

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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfACB *acbt;         /* ACB object to test */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   NdfDCB *dcbt;         /* DCB object to test */
   int islot;            /* Slot index */
   int next;             /* Next ACB entry to test */
   int there;            /* Whether the quality array exists */
   int valid;            /* Whether ACB array identifier is valid */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain an index to the data object entry in the DCB. */
   dcb = acb->dcb;

/* Check if the DCB already contains quality information. If so, then
   there is nothing to do. */
   if( !dcb->kq ) {

/* Ensure that the DCB does contain quality information. */
      ndf1Dq( dcb, status );

/* See if the quality array identifier in the DCB is valid. If not,
   then the component does not exist. */
      there = aryValid( dcb->qid, status );

/* Loop to identify all the ACB entries which refer to this data object. */
      next = 0;
      islot = -1;
      NDF__ACB_LOCK_MUTEX;
      acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
      while( ( *status == SAI__OK ) && ( next != -1 ) ){
         islot = next;

/* Select those with the correct DCB index. */
         dcbt = acbt->dcb;
         if( dcbt == dcb ) {

/* If the quality component exists, then see if the ACB quality array
   identifier is already valid. */
            if( there ) {
               valid = aryValid( acbt->qid, status );
               if( *status == SAI__OK ) {

/* If not, then create a section of the quality array to match the
   ACB's data array section and enter the resulting ARY_ system
   identifier into the ACB. */
                  if( !valid ) ndf1Ssdup( dcb->qid, acbt->did, &acbt->qid,
                                          status );
               }

/* Otherwise, enter a null identifier. */
            } else {
               acbt->qid = NULL;
            }
         }
         acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
      }
      NDF__ACB_UNLOCK_MUTEX;
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Qimp", status );

}

