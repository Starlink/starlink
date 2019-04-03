#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfHend_( int *status ){
/*
*+
*  Name:
*     ndfHend

*  Purpose:
*     End NDF history recording for the current application.

*  Synopsis:
*     void ndfHend( int *status )

*  Description:
*     This function closes down history recording for the current
*     application by writing default history information (when appropriate)
*     to any NDFs which are still active, and by flagging that a new
*     history record should be created to hold any subsequent history
*     information (ready for the next application). If an application name
*     has been declared via a call to ndfHappn, then that name is cleared.

*  Parameters:
*     *status
*        The global status.

*  Notes:
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  This function should not normally be used by writers of NDF
*     applications. It is provided primarily to allow those writing
*     environment-level software to identify the end of an application to
*     the NDF_ system in circumstances where more than one application may
*     be invoked from within a single executing program. Even then, it will
*     not normally be needed unless NDF data structures are intended to
*     remain open throughout the invocation of several applications and a
*     separate history record is required from each application.

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
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   int islot;            /* Slot index */
   int next;             /* Next DCB slot to use */

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Log any pending error message information for subsequent recording
   in NDF history records. */
   ndf1Hlerr( status );

/* Begin a new error reporting environment. */
   errBegin( status );

/* Loop through all the DCB entries still active within the current
   application. Handle each within its own error reporting environment. */
   islot = -1;
   errBegin( status );
   NDF__DCB_LOCK_MUTEX;
   dcb = ndf1Nxtsl( NDF__DCBTYPE, islot, &next, status );
   while( ( *status == SAI__OK ) && ( next != -1 ) ){
      islot = next;

/* Write default history information (if necessary) to each data
   object. */
      ndf1Hwdef( dcb, " ", status );

/* Dump any logged error message information to the history record. */
      ndf1Hderr( dcb, 0, status );

/* Reset the current history record text width to indicate that no
   current history record exists (so that a new one will subsequently
   be created). Also indicate that new default history information will
   be required, and that he current time should be attached to the next
   history record. */
      dcb->htlen = 0;
      dcb->hdef = 1;
      dcb->htime = -1.0;

/* End the current error reporting environment and return to process
   the next DCB entry. */
      errEnd( status );
      errBegin( status );
      dcb = ndf1Nxtsl( NDF__DCBTYPE, islot, &next, status );
   }
   NDF__DCB_UNLOCK_MUTEX;

/* End the last error reporting environment and clear the current
   default application name used for history recording. */
   errEnd( status );
   NDF__DCB_LOCK_APPMUTEX;
   Ndf_DCB_happn[ 0 ] = 0;
   NDF__DCB_UNLOCK_APPMUTEX;

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfHend: Error ending NDF history recording for the "
              "current application.", status );
      ndf1Trace( "ndfHend", status );
   }

/* End the outer error reporting environment. */
   errEnd( status );

/* Restablish the original AST status pointer */
   NDF_FINAL

}

