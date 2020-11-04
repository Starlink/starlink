#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"

void ndf1Dvanl( NdfDCB *dcb, int del, int *status ){
/*
*+
*  Name:
*     ndf1Dvanl

*  Purpose:
*     Annul the variance data object in an NDF.

*  Synopsis:
*     void ndf1Dvanl( NdfDCB *dcb, int del, int *status )

*  Description:
*     This function performs an annul operation on the DCB items describing
*     the variance data object in an NDF as part of anulling the DCB entry
*     for the NDF itself. If no DCB information about the variance
*     component is available, then no action is taken. Otherwise, the DCB
*     items relating to this component are annulled and the variance data
*     object deleted if necessary.

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

/* Check that variance information is available in the DCB. Otherwise
   there is nothing to do. */
   *status = SAI__OK;
   if( dcb->kv ) {

/* See if the DCB ARY_ system identifier for the variance array is
   valid. If not, then the array does not exist, so there is nothing to
   do. */
      valid = aryValid( dcb->vid, status );
      if( *status == SAI__OK ) {
         if( valid ) {

/* Delete the array if required. */
            if( del ) {
               aryDelet( &dcb->vid, status );

/* Otherwise, see if it is in the defined state. */
            } else {
               aryState( dcb->vid, &state, status );
               if( *status == SAI__OK ) {

/* If so, then it must be kept, so simply annul the DCB identifier for
   it. */
                  if( state ) {
                     aryAnnul( &dcb->vid, status );

/* Otherwise, it must be deleted as an undefined array is not allowed
   (this should never actually need to be done). */
                  } else {
                     aryIsacc( dcb->vid, "DELETE", &isacc, status );
                     if( isacc ) {
                        aryDelet( &dcb->vid, status );
                     } else if( *status == SAI__OK ){
                        *status = NDF__VUDEF;
                        errRep( " ", "Undefined Variance array encountered "
                                "in read-only NDF.", status );
                     }
                  }
               }
            }
         }
      }

/* Note that DCB variance information is no longer available. */
      dcb->kv = 0;
   }

/* Annul any error if "status" was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ) {
      if( tstat != SAI__OK ) {
         errAnnul( status );
         *status = tstat;

/* Call error tracing function if appropriate. */
      } else {
         ndf1Trace( "ndf1Dvanl", status );
      }
   } else {
      *status = tstat;
   }

/* Release the error stack. */
   errRlse();

}

