#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"
#include <string.h>

void ndf1Qump( NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1Qump

*  Purpose:
*     Unmap the quality component of an NDF.

*  Synopsis:
*     void ndf1Qump( NdfACB *acb, int *status )

*  Description:
*     This function unmaps the quality component of an NDF, which has
*     previously been mapped by ndf1Qmap. If write or update access is in
*     effect, then the mapped values are written back to the array. The NDF
*     is identified by its ACB entry.

*  Parameters:
*     acb
*        Pointer to the NDF's ACB entry.
*     *status
*        The global status.

*  Notes:
*     -  This function will attempt to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Prior Requirements:
*     -  The quality component must previously have been mapped.  An error
*     will be reported if this is not the case.

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

*-
*/

/* Local Variables: */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char form[ NDF__SZFRM + 1 ];    /* Quality array storage form */
   int temp;             /* Whether a temporary array is mapped */
   int tstat;            /* Temporary status variable */

/* Save the "status" value and mark the error stack. */
   tstat = *status;
   errMark();

/* Obtain an index to the data object entry in the DCB. */
   dcb = acb->dcb;

/* If the quality array for the specified ACB entry is not mapped, then
   report an error. */
   *status = SAI__OK;
   if( !acb->qmap ) {
      *status = NDF__NTMAP;
      ndf1Amsg( "NDF", acb );
      errRep( " ", "The quality component in the NDF structure ^NDF is not "
              "mapped for access through the specified identifier "
              "(possible programming error).", status );

/* If the quality array has been mapped as an array of logical values,
   then unmap and annul the object holding these values. */
   } else {
      if( !strcmp( acb->qmtyp, "_LOGICAL" ) ) {
         ndf1Antmp( &acb->qmtlc, status );

/* Otherwise, see if the temporary mapped array identifier in the ACB
   is valid. */
      } else {
         temp = aryValid( acb->qmtid, status );
         if( *status == SAI__OK ) {

/* If so, then a temporary array was mapped for read access and can
   be annulled. */
            if( temp ) {
               aryAnnul( &acb->qmtid, status );

/* Otherwise, unmap the ACB's quality array. */
            } else {
               aryUnmap( acb->qid, status );

/* If values have been written back to the array and the array's storage
   form is not primitive, then ensure that its bad pixel flag remains
   set to zero. This must be done via the base array identifier
   stored in the DCB, since setting a bad pixel flag to zero for an
   array section may not affect the base array. */
               aryForm( dcb->qid, form, status );
               if( *status == SAI__OK ) {
                  if( strcmp( form, "PRIMITIVE" ) &&
                         ( !strcmp( acb->qmmod, "WRITE" ) ||
                           !strcmp( acb->qmmod, "UPDATE" ) ) ) {
                     arySbad( 0, dcb->qid, status );
                  }
               }
            }
         }
      }

/* If no error occurred, then note that the array is no longer mapped
   and decrement the DCB mapping counts. */
      if( *status == SAI__OK ) {
         acb->qmap = 0;
         dcb->nqmap--;
         dcb->nmap--;

/* Reset the pointers to the mapped values. */
         acb->qmptr = 0;
      }
   }

/* Annul any error if "status" was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ) {
      if( tstat != SAI__OK ) {
         errAnnul( status );
         *status = tstat;

/* Call the error tracing function if appropriate. */
      } else {
         ndf1Trace( "ndf1Qump", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

}

