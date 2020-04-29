#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

void ndfEnd_( int *status ){
/*
*+
*  Name:
*     ndfEnd

*  Purpose:
*     End the current NDF context.

*  Synopsis:
*     void ndfEnd( int *status )

*  Description:
*     This function ends the current NDF context, causing all NDF
*     identifiers and placeholders created within that context (i.e. since
*     a matching call to ndfBegin) to be annulled. Any mapped values
*     associated with these identifiers are unmapped, and any temporary
*     NDFs which no longer have identifiers associated with them are
*     deleted.

*  Parameters:
*     *status
*        The global status.

*  Notes:
*     -  Matching pairs of calls to ndfBegin and ndfEnd may be nested. An
*     error will be reported if ndfEnd is called without a corresponding
*     call to ndfBegin.
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

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
*        Unlock the mutex before calling ndf1Anl as it may attempt to
*        lock the mutex.
*-
*/

/* Local Variables: */
   NdfACB *acbt;         /* ACB to test */
   NdfPCB *pcbt;         /* PCB to test */
   int astat;            /* Status for annulling table entries */
   int islot;            /* Slot index */
   int next;             /* Next common block slot to consider */
   int tstat;            /* Temporary status variable */

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Get a pointer to thread speciic data */
   NDF_GETTSD;

/* Log any pending error message information for subsequent recording
   in NDF history records. */
   ndf1Hlerr( status );

/* Initialise a status value for annulling ACBs and PCBs. */
   astat = *status;

/* Save the "status" value and mark the error stack. */
   tstat = *status;
   errMark();

/* If the current identifier context level is not positive, then a
   matching call to ndfBegin has been omitted, so report an error. */
   *status = SAI__OK;
   if( NDF_TSD(acbIdctx) <= 1 ) {
      *status = NDF__MSBEG;
      errRep( " ", "ndfEnd called without a corresponding call to ndfBegin "
              "(possible programming error).", status );

/* Otherwise, decrement the current context level. */
   } else {
      NDF_TSD(acbIdctx)--;

/* Loop to examine each active entry in the ACB. */
      next = 0;
      islot = -1;
      NDF__ACB_LOCK_MUTEX;
      acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
      while( ( *status == SAI__OK ) && ( next != -1 ) ){
         islot = next;

/* Ignore ACBs that are not locked by the current thread. */
         if( ndf1Locked( acbt ) == 1 ) {

/* If the context level at which the ACB was made exceeds the new
   "current" context level, then annul the ACB. */
            if( acbt->ctx > NDF_TSD(acbIdctx) ) {
               NDF__ACB_UNLOCK_MUTEX;
               ndf1Anl( &acbt, &astat );
               NDF__ACB_LOCK_MUTEX;
            }
         }

/* Get the next active ACB. */
         acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
      }
      NDF__ACB_UNLOCK_MUTEX;

/* Similarly, loop to examine all the active entries in the PCB. */
      next = 0;
      islot = -1;
      NDF__PCB_LOCK_MUTEX;
      pcbt = ndf1Nxtsl( NDF__PCBTYPE, islot, &next, status );
      while( ( *status == SAI__OK ) && ( next != -1 ) ){
         islot = next;

/* If the context level at which the PCB was made exceeds the new
   "current" context level, then annul the PCB. */
         if( pcbt->ctx > NDF_TSD(acbIdctx) ) ndf1Annpl( 1, &pcbt, &astat );

/* Get the next active PCB. */
         pcbt = ndf1Nxtsl( NDF__PCBTYPE, islot, &next, status );
      }
      NDF__PCB_UNLOCK_MUTEX;
   }

/* If an error occurred while annulling any entry, but "status" has not
   been set, then transfer the bad "astat" value to "status". */
   if( *status == SAI__OK ) {
      if( astat != SAI__OK ) *status = astat;
   }

/* Annul any error if "status" was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ) {
      if( tstat != SAI__OK ) {
         errAnnul( status );
         *status = tstat;

/* If appropriate, report context information and call the error tracing
   function. */
      } else {
         errRep( " ", "ndfEnd: Error ending an NDF context.", status );
         ndf1Trace( "ndfEnd", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

/* Restablish the original AST status pointer */
   NDF_FINAL

}

