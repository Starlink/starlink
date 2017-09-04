#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"

void ary1Upsr( char copy, HDSLoc **mloc, int *status ) {
/*
*+
*  Name:
*     ary1Upsr

*  Purpose:
*     Unmap a simple array component mapped for READ access.

*  Synopsis:
*     void ary1Upsr( char copy, HDSLoc **mloc, int *status )

*  Description:
*     The routine unmaps a component of a simple array which has
*     previously been mapped for READ access.

*  Parameters:
*     copy
*        A boolean flag indicating whether mapped access is via a "copy"
*        of the actual data; this indicates whether or not the locator
*        'mloc' is associated with a temporary object.
*     mloc
*        Address of a locator to the HDS object mapped to provide memory
*        locations for the data. This locator will be annulled and reset
*        to a NULL pointer by this routine. If it is associated with a
*        temporary object, then this will be erased.
*     status
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if 'status' is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  No ACB or MCB structures are updated by this routine.

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
   int tstat;                 /* Temporary status value */

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();

/* If access is via a "copy" of the data, then annul the temporary object
   containing the data (this erases the object). */
   *status = SAI__OK;
   if( copy ){
      ary1Antmp( mloc, status );

/* Otherwise, if access is direct via HDS, then annul the locator (causing
   the data to be unmapped). */
   } else {
      datAnnul( mloc, status );
   }

/* Annul any error if STATUS was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;

/* Call error tracing routine if appropriate. */
      } else {
         ary1Trace( "ary1Upsr", status );
      }
   } else {
      *status = tstat;
   }

/* Release the error stack. */
   errRlse();

}
