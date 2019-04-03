#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfSbb_( unsigned char badbit, int indf, int *status ){
/*
*+
*  Name:
*     ndfSbb

*  Purpose:
*     Set a bad-bits mask value for the quality component of an NDF.

*  Synopsis:
*     void ndfSbb( unsigned char badbit, int indf, int *status )

*  Description:
*     This function assigns a new unsigned byte bad-bits mask value to the
*     quality component of an NDF.

*  Notes:
*     -  If WRITE access to the NDF is not available, or if an NDF section
*     is supplied (as opposed to a base NDF), then no permanent change to
*     the data object will be made. In this case, the new bad-bits value
*     will be associated with the NDF identifier and will subsequently be
*     used by other NDF_ functions which access the NDF through this
*     identifier. The new value will also be propagated to any new
*     identifiers derived from it.

*  Parameters:
*     badbit
*        The unsigned byte bad-bits mask value.
*     indf
*        NDF identifier.
*     *status
*        The global status.

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
   HDSLoc *locbb = NULL; /* Locator to BADBITS object */
   NdfACB *acb;          /* Pointer to the NDF entry in the ACB */
   NdfACB *acbt;         /* ACB to test */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   hdsbool_t there;      /* Whether component exists */
   hdsdim dummy;         /* Dummy dimension array */
   int islot;            /* Slot index */
   int modify;           /* Whether to modify the data object */
   int next;             /* Next ACB entry */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );
   if( *status == SAI__OK ) {

/* If an NDF section was specified, then the actual data object value
   is not altered, so set the ACB bad-bits value and note it has been
   set. */
      if( acb->cut ) {
         acb->qbb = badbit;
         acb->isqbb = 1;

/* If a base NDF was specified, then obtain an index to the data object
   entry in the DCB. */
      } else {
         dcb = acb->dcb;

/* See if WRITE access to the NDF is available. */
         ndf1Accok( acb, "WRITE", &modify, status );
         if( *status == SAI__OK ) {

/* If access is not available, then the data object cannot be modified,
   so the DCB over-ride value is set. */
            if( !modify ) {
               dcb->isqbb = 1;
               dcb->ovqbb = badbit;

/* If the data object can be modified, then cancel any previous DCB
   over-ride value. */
            } else {
               dcb->isqbb = 0;
               dcb->ovqbb = 0;

/* Ensure that a quality component exists. */
               ndf1Qcre( acb, status );

/* See if it contains a BADBITS object. */
               datThere( dcb->qloc, "BADBITS", &there, status );
               if( *status == SAI__OK ) {

/* If not, then create one. */
                  if( !there ) {
                     dummy = 0;
                     datNew( dcb->qloc, "BADBITS", "_UBYTE", 0, NULL, status );

                  }

/* Obtain a locator to the BADBITS object. */
                  datFind( dcb->qloc, "BADBITS", &locbb, status );

/* Assign the new value to it and update the value stored in the DCB. */
                  dummy = 0;
                  datPut( locbb, "_UBYTE", 0, &dummy, &badbit, status );
                  if( *status == SAI__OK ) dcb->qbb = badbit;

/* Annul the locator. */
                  datAnnul( &locbb, status );
               }
            }
         }

/* Loop to inspect ACB entries. */
         islot = -1;
         next = 0;
         NDF__ACB_LOCK_MUTEX;
         acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
         while( *status == SAI__OK && next != -1 ) {

/* Identify base ACB entries which refer to the modified data object. */
            islot = next;
            if( ( !acbt->cut ) && ( acbt->dcb == dcb ) ) {

/* Modify those ACB entries to hold the new bad-bits value and to note
   if an over-ride value is in effect. */
               acbt->isqbb = ( !modify );
               acbt->qbb = badbit;
            }
            acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
         }
         NDF__ACB_UNLOCK_MUTEX;
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfSbb: Error setting a bad-bits mask value for the "
              "quality component of an NDF.", status );
      ndf1Trace( "ndfSbb", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

