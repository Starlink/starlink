#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "star/cmp.h"

void ndf1Hincr( NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Hincr

*  Purpose:
*     Increment the history record for an NDF.

*  Synopsis:
*     void ndf1Hincr( NdfDCB *dcb, int *status )

*  Description:
*     This function opens a new record in the history structure for an NDF,
*     ready to receive new history information. If the history record array
*     is not large enough to contain a new record, it is extended. The DCB
*     current history record count is incremented to identify the new
*     record. This function does not create any components within the new
*     record structure.

*  Parameters:
*     dcb
*        Pointer to a DCB entry identifying the NDF whose history is to be
*        incremented.
*     *status
*        The global status.

*  Notes:
*     The history component which this function is to modify must exist
*     before the function is called, and DCB information must exist about
*     it. This function does not check for this itself.

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
   hdsdim dim[ DAT__MXDIM ]; /* Object dimension sizes */
   size_t mxrec;             /* Size of history records array */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Determine the number of elements in the array of history records. */
   datSize( dcb->hrloc, &mxrec, status );
   if( *status == SAI__OK ) {

/* If there is insufficient room for another record, then extend the
   array. */
      if( mxrec < dcb->hnrec + 1 ) {
         dim[ 0 ] = mxrec + dcb->hext;
         datAlter( dcb->hrloc, 1, dim, status );
      }

/* Increment the current record counter, both in the data structure and
   in the DCB. */
      cmpPut0I( dcb->hloc, "CURRENT_RECORD", dcb->hnrec + 1, status );
      if( *status == SAI__OK ) dcb->hnrec++;
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Hincr", status );

}

