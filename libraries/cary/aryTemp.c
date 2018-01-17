#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "prm_par.h"

void aryTemp( AryPlace **place, int *status ) {
/*
*+
*  Name:
*     aryTemp

*  Purpose:
*     Obtain a placeholder for a temporary array.

*  Synopsis:
*     void aryTemp( AryPlace **place, int *status )

*  Description:
*     This function returns an array placeholder which may be used to
*     create a temporary array (i.e. one which will be deleted
*     automatically once the last identifier associated with it is
*     annulled). The placeholder returned by this routine may be passed
*     to other routines (e.g. aryNew or aryCopy) to produce a
*     temporary array in the same way as a new permanent array would be
*     created.

*  Parameters:
*     place
*        Returned holding a placeholder for a temporary array.
*     status
*        The global status.

*  Notes:
*     -  Placeholders are intended only for local use within an
*     application and only a limited number of them are available
*     simultaneously. They are always annulled as soon as they are
*     passed to another routine to create a new array, where they are
*     effectively exchanged for an array identifier.
*     -  If this routine is called with STATUS set, then a value of
*     NULL will be returned for the "place" argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
*      All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local variables: */
   hdsdim dummy;              /* Dummy dimension array */
   AryPCB *pcb;               /* Pointer to the PCB object */

/* Set an initial value for the PLACE argument. */
   *place = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain a free slot in the PCB. */
   pcb = (AryPCB *) ary1Ffs( ARY__PCBTYPE, status );
   if( *status == SAI__OK ){

/* Create a temporary array placeholder object, storing a locator to it in
   the PCB. */
      dummy = 0;
      ary1Temp( "ARRAY", 0, &dummy, &pcb->loc, status );

/* Link the locator into a private group to prevent external events from
   annulling it. */
      hdsLink( pcb->loc, "ARY_PCB", status );
      if( *status == SAI__OK ){

/* Note the object to replace the placeholder is to be temporary. */
         pcb->tmp = 1;

/* Get an external identifier fo the PCB, in the form of an AryPlace
   pointer. */
         *place = (AryPlace *) ary1Expid( (AryObject *) pcb, status );

/* If an error occurred, then release the PCB slot. */
      } else {
         pcb = ary1Rls( (AryObject *) pcb, status );
      }
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryTemp: Error obtaining placeholder for temporary array.",
              status );
      ary1Trace( "aryTemp", status );
   }

}
