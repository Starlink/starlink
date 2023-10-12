#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "prm_par.h"

void aryPlace( HDSLoc *loc, const char *name, AryPlace **place, int *status ) {
/*
*+
*  Name:
*     aryPlace

*  Purpose:
*     Obtain an array placeholder.

*  Synopsis:
*     void aryPlace( HDSLoc *loc, const char *name, AryPlace **place, int *status )

*  Description:
*     This function returns an array placeholder. A placeholder is used
*     to identify a position in the underlying data system (HDS) and
*     may be passed to other routines (e.g. aryNew) to indicate where
*     a newly created array should be positioned.

*  Parameters:
*     loc
*        HDS locator to the structure to contain the new array.
*     name
*        Name of the new structure component (i.e. the array).
*     place
*        Returned holding an array placeholder identifying the nominated
*        position in the data system.
*     status
*        The global status.

*  Notes:
*     -  Placeholders are intended only for local use within an
*     application and only a limited number of them are available
*     simultaneously. They are always annulled as soon as they are
*     passed to another routine to create a new array, where they are
*     effectively exchanged for an array identifier.
*     -  If this routine is called with "status" set, then a value of
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
   AryPCB *pcb;               /* The placeholder (PCB) object */

/* Set an initial value for the PLACE argument. */
   *place = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check that a standard HDS component name has been supplied. */
   ary1Chscn( name, status );

/* Obtain a free slot in the PCB. */
   ARY__PCB_LOCK_MUTEX;
   pcb = (AryPCB *) ary1Ffs( ARY__PCBTYPE, status );
   ARY__PCB_UNLOCK_MUTEX;

   if( *status == SAI__OK ){

/* Create a new array placeholder object to reserve a position in the data
   system. */
      dummy = 0;
      datNew( loc, name, "ARRAY", 0, &dummy, status );

/* Obtain a locator to the new object, storing it in the PCB and linking it
   into a private group to prevent external events from annulling it. */
      pcb->loc = NULL;
      datFind( loc, name, &pcb->loc, status );
      hdsLink( pcb->loc, "ARY_PCB", status );
      if( *status == SAI__OK ){

/* Note that the object which replaces the placeholder object should not be
   temporary. */
         pcb->tmp = 0;

/* Get an external identifier fo the PCB, in the form of an AryPlace
   pointer. */
         *place = (AryPlace *) ary1Expid( (AryObject *) pcb, status );

/* If an error occurred, then release the PCB slot. */
      } else {
         ARY__PCB_LOCK_MUTEX;
         pcb = ary1Rls( (AryObject *) pcb, status );
         ARY__PCB_UNLOCK_MUTEX;
      }
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryPlace: Error obtaining array placeholder.", status );
      ary1Trace( "aryPlace", status );
   }

}
