#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"

void ndf1Annpl( int erase, NdfPCB **pcb, int *status ){
/*
*+
*  Name:
*     ndf1Annpl

*  Purpose:
*     Annul an NDF placeholder.

*  Synopsis:
*     void ndf1Annpl( int erase, NdfPCB **pcb, int *status )

*  Description:
*     This function annuls an NDF placeholder, releasing the PCB slot which
*     it occupies and optionally erasing the object associated with it.

*  Parameters:
*     erase
*        Whether to erase the associated object.
*     *pcb
*        Pointer to placeholder entry in the PCB. A value of zero is
*        returned.
*     *status
*        The global status.

*  Notes:
*     -  Regardless of the value of the "erase" parameter, the associated
*     placeholder object will not be deleted unless its entry in the PCB
*     indicates that it was created by the NDF_ system (rather than being a
*     pre-existing object supplied by the caller).
*     -  This function attempts to execute even if "status" is set on
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
*     15-OCT-2020 (DSB):
*        Re-instate the check on the input PCB. This check was present in
*        the Fortran code (ndf1_annpl.f) but was mistakenly omitted in the
*        translation to C.

*-
*/

/* Begin a new error reporting environment. */
   errBegin( status );

/* Check that the PCB supplied is valid and report an error if it is not. */
   if( ! (*pcb) ){
      *status = NDF__FATIN;
      msgSetc( "ROUTINE", "ndf1Annpl" );
      errRep( "NDF1_ANNPL_IPCB", "Routine ^ROUTINE called with an "
              "invalid '*pcb' argument of NULL - internal programming "
              "error.", status );

/* If required, and the placeholder object is a new one created by the
   NDF_ system, then delete the associated object, annulling its locator
   in the process. */
   } else {
      if( erase && (*pcb)->new ) {
         ndf1Delob( &(*pcb)->loc, status );

/* Otherwise, simply annul the locator. */
      } else {
         datAnnul( &(*pcb)->loc, status );
      }

/* Release the PCB slot. */
      *pcb = ndf1Rls( ( NdfObject * ) *pcb, status );
   }

/* Reset the PCB index. */
   *pcb = NULL;

/* Call error tracing function. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Annpl", status );

/* End the error reporting environment. */
   errEnd( status );

}

