#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

int ndfReport_( int report, int *status ){
/*
*+
*  Name:
*     ndfReport

*  Purpose:
*     Report any currently active NDF identifiers.

*  Synopsis:
*     result = ndfReport( int report, int *status );

*  Description:
*     This function displays information about any currently active
*     NDF identifiers.

*  Parameters:
*     report
*        If non-zero, an informational message listing any currently
*        active NDF identifiers is displayed. Nothing is displayed if
*        there are no active identifiers, or if report is zero.
*     *status
*        The global status.

*  Returned Value:
*     The number of active NDF identifiers found.

*  Notes:
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory
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
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version.

*-
*/

/* Local Variables: */
   NdfACB *acbt;
   int islot;
   int ival;
   int next;
   int nactive;
   int tstat;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Get a pointer to thread speciic data */
   NDF_GETTSD;

/* Save the "status" value and mark the error stack. */
   tstat = *status;
   errMark();
   *status = SAI__OK;

/* Initialise the number of active ACBs found. */
   nactive = 0;

/* Loop to examine each active entry in the ACB. */
   next = 0;
   islot = -1;
   NDF__ACB_LOCK_MUTEX;
   acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
   while( ( *status == SAI__OK ) && ( next != -1 ) ){
      islot = next;

/* Increment the number found. */
      nactive++;

/* If required make the report. */
      if( report ) {
         if( nactive == 1 ){
            msgOut( " ", "The following NDF identifiers are currently "
                    "active:", status );
         }
         msgSeti( "ID", ((NdfObject *) acbt)->check );
         ndf1Amsg( "NDF", acbt );
         ival = ndf1Locked( acbt );
         if( ival == 0 ) {
            msgSetc( "L", "Unlocked." );
         } else if( ival > 0 ) {
            msgSetc( "L", "Locked by the current thread." );
         } else {
            msgSetc( "L", "Locked by another thread." );
         }
         if( acbt->ctx == NDF__INLIMBO ){
            msgSetc( "C", "none" );
         } else {
            msgSeti( "C", acbt->ctx );
         }
         msgOut( " ", "   ^ID: '^NDF'   ^L   Context '^C'.", status );
      }

/* Get the next active ACB. */
      acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
   }
   NDF__ACB_UNLOCK_MUTEX;

/* Annul any error if "status" was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ) {
      if( tstat != SAI__OK ) {
         errAnnul( status );
         *status = tstat;

/* If appropriate, report context information and call the error tracing
   function. */
      } else {
         errRep( " ", "ndfReport: Error reporting any unlocked NDFs.", status );
         ndf1Trace( "ndfReport", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

/* Restablish the original AST status pointer */
   NDF_FINAL

/* Return the number of active identifiers found. */
   return nactive;
}

