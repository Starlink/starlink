#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"

void ndf1Dqanl( NdfDCB *dcb, int del, int *status ){
/*
*+
*  Name:
*     ndf1Dqanl

*  Purpose:
*     Annul the quality data object in an NDF.

*  Synopsis:
*     void ndf1Dqanl( NdfDCB *dcb, int del, int *status )

*  Description:
*     This function performs an annul operation on the DCB items describing
*     the quality data object in an NDF as part of anulling the DCB entry
*     for the NDF itself. If no DCB information about the quality component
*     is available, then no action is taken. Otherwise, the DCB items
*     relating to this component are annulled and the quality data object
*     deleted if necessary.

*  Parameters:
*     dcb
*        Pointer to the DCB entry.
*     del
*        Whether the annul operation is to result in deletion of the data
*        object.
*     *status
*        The global status.

*  Notes:
*     -  This function will attempt to execute even if "status" is set on
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

*-
*/

/* Local Variables: */
   int isacc;            /* Is delete access available? */
   int state;            /* Whether array is in defined state */
   int tstat;            /* Temporary status variable */
   int valid;            /* Whether array identifier is valid */

/* Save the "status" value and mark the error stack. */
   tstat = *status;
   errMark();

/* Check that quality information is available in the DCB. Otherwise
   there is nothing to do. */
   *status = SAI__OK;
   if( dcb->kq ) {

/* See if the DCB locator for the quality component is valid. If not,
   then the quality structure does not exist, so there is nothing to do. */
      if( dcb->qloc ) {

/* See if the DCB ARY_ system identifier for the quality array is valid. */
         valid = aryValid( dcb->qid, status );
         if( *status == SAI__OK ) {

/* If not, then the quality component is undefined, so the quality
   structure must be deleted. */
            if( !valid ) {
               ndf1Antmp( &dcb->qloc, status );

/* If the quality array exists, but deletion is required, then delete
   the array, followed by the quality structure itself. */
            } else {
               if( del ) {
                  aryDelet( &dcb->qid, status );
                  ndf1Antmp( &dcb->qloc, status );

/* Otherwise, see if the array is in the defined state. */
               } else {
                  aryState( dcb->qid, &state, status );
                  if( *status == SAI__OK ) {

/* If so, then it must be kept, so simply annul the DCB identifier for
   it, followed by the DCB locator to the quality structure. */
                     if( state ) {
                        aryAnnul( &dcb->qid, status );
                        datAnnul( &dcb->qloc, status );

/* Otherwise, the whole structure must be deleted as an undefined array
   within the quality structure is not allowed (this should never
   actually need to be done). */
                     } else {
                        aryIsacc( dcb->qid, "DELETE", &isacc, status );
                        if( isacc ) {
                           aryDelet( &dcb->qid, status );
                        } else if( *status == SAI__OK ){
                           *status = NDF__QUDEF;
                           errRep( " ", "Undefined Quality array encountered "
                                   "in read-only NDF.", status );
                        }
                        ndf1Antmp( &dcb->qloc, status );
                     }
                  }
               }
            }
         }
      }

/* Note that DCB quality information is no longer available. */
      dcb->kq = 0;
   }

/* Annul any error if "status" was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ) {
      if( tstat != SAI__OK ) {
         errAnnul( status );
         *status = tstat;

/* Call error tracing function if appropriate. */
      } else {
         ndf1Trace( "ndf1Dqanl", status );
      }
   } else {
      *status = tstat;
   }

/* Release the error stack. */
   errRlse();

}

