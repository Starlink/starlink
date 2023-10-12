#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"

void ary1Annpl( int erase, AryPCB **pcb, int *status ) {
/*
*+
*  Name:
*     ary1Annpl

*  Purpose:
*     Annul a placeholder.

*  Synopsis:
*     void ary1Annpl( int erase, AryPCB **pcb, int *status )

*  Description:
*     The routine annuls a placeholder, releasing the PCB slot which it
*     occupies and optionally erasing the object associated with it.

*  Parameters:
*     erase
*        Whether to erase the associated object.
*     pcb
*        Supplied holding a pointer to the PCB. A NULL pointer zero is returned.
*     status
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

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
   int tstat;                 /* Temporary status variable */

/* Save the status value and mark the error stack. */
   tstat = *status;
   errMark();

/* If required, erase the associated object, annulling it's locator in the
   process. */
   *status = SAI__OK;
   if( erase ){
      ary1Antmp( &(*pcb)->loc, status );

/* Otherwise, simply annul the locator. */
   } else {
      datAnnul( &(*pcb)->loc, status );
   }

/* Release the PCB slot. */
   ARY__PCB_LOCK_MUTEX;
   *pcb = ary1Rls( (AryObject *) *pcb, status );
   ARY__PCB_UNLOCK_MUTEX;

/* Annul any error if STATUS was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;

/* Call error tracing routine if appropriate. */
      } else {
         ary1Trace( "ary1Annpl", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

}
