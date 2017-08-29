#include "sae_par.h"
#include "star/hds.h"
#include "mers.h"

#include "ary1.h"

void ary1Hunmp( HDSLoc *loc, int *status ) {
/*
*+
*  Name:
*     ary1Hunmp

*  Purpose:
*     Unmap an HDS primitive object.

*  Synopsis:
*     void ary1Hunmp( HDSLoc *loc, int *status )

*  Description:
*     The routine unmaps an HDS object which has previously been mapped.

*  Parameters:
*     loc
*        Locator to HDS object.
*     status
*        The global status.

*  Notes:
*     -  This routine exists because the datUnmap routine doesn't
*     execute if STATUS is set on entry. Thus, mapped data could
*     potentially remain unnecessarily mapped following recovery from
*     an error.
*     -  This routine attempts to execute even if STATUS is set on
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
*     23-JUN-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local variables: */
   int tstat;                 /* Temporary status variable */

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();

/* Unmap the HDS object. */
   *status = SAI__OK;
   datUnmap( loc, status );

/* Annul any error if STATUS was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;

/* Call error tracing routine if appropriate. */
      } else {
         ary1Trace( "ary1Hunmp", status );
      }

   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

}
