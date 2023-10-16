#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "prm_par.h"
#include "ndf.h"
#include "mers.h"

void ndfTemp_( int *place, int *status ){
/*
*+
*  Name:
*     ndfTemp

*  Purpose:
*     Obtain a placeholder for a temporary NDF.

*  Synopsis:
*     void ndfTemp( int *place, int *status )

*  Description:
*     This function returns an NDF placeholder which may be used to create
*     a temporary NDF (i.e. one which will be deleted automatically once
*     the last identifier associated with it is annulled). The placeholder
*     returned by this function may be passed to other functions (e.g.
*     ndfNew or ndfCopy) to produce a temporary NDF in the same way as a
*     new permanent NDF would be created.

*  Parameters:
*     *place
*        Returned holding the placeholder for a temporary NDF.
*     *status
*        The global status.

*  Notes:
*     -  Placeholders are intended only for local use within an application
*     and only a limited number of them are available simultaneously. They
*     are always annulled as soon as they are passed to another function to
*     create a new NDF, where they are effectively exchanged for an NDF
*     identifier.
*     -  If this function is called with "status" set, then a value of
*     NDF__NOPL will be returned for the "place" parameter, although no
*     further processing will occur. The same value will also be returned
*     if the function should fail for any reason. The NDF__NOPL constant is
*     defined in the header file "ndf.h".

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
   NdfPCB *pcb;          /* Pointer to placeholder entry in the PCB */
   hdsdim dummy;         /* Dummy dimension array */

/* Set an initial value for the "place" parameter. */
   *place = NDF__NOPL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Obtain a free slot in the PCB. */
   NDF__PCB_LOCK_MUTEX;
   pcb = ndf1Ffs( NDF__PCBTYPE, status );
   NDF__PCB_UNLOCK_MUTEX;
   if( *status == SAI__OK ) {

/* Create a temporary NDF placeholder object, storing a locator to it
   in the PCB. */
      dummy = 0;
      ndf1Temp( "NDF", 0, &dummy, &pcb->loc, status );

/* Link the locator into a private group to prevent external events
   from annulling it. */
      hdsLink( pcb->loc, "NDF_PCB", status );

/* Note the object to replace the placeholder is to be temporary. */
      pcb->tmp = 1;

/* Export the required placeholder */
      *place = ndf1Expid( ( NdfObject * ) pcb, status );

/* If an error occurred, then annul the PCB entry. */
      if( *status != SAI__OK ) {
         NDF__PCB_LOCK_MUTEX;
         ndf1Annpl( 1, &pcb, status );
         NDF__PCB_UNLOCK_MUTEX;
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfTemp: Error obtaining a placeholder for a temporary "
              "NDF.", status );
      ndf1Trace( "ndfTemp", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

