#include "sae_par.h"
#include "ary1.h"
#include "mers.h"

AryACB *ary1Anl( AryACB *acb, int *status ) {
/*
*+
*  Name:
*     ary1Anl

*  Purpose:
*     Annul an array specified by an ACB.

*  Synopsis:
*     AryACB *ary1Anl( AryACB *acb, int *status )

*  Description:
*     The routine annuls an array specified by its ACB. If the array is
*     currently mapped for access through this ACB, then it is first
*     unmapped. The ACB is then annulled. If, as a result, the
*     reference count for the associated data object drops to zero,
*     then the object will be released from the ARY_ system and may be
*     deleted, according to its disposal mode.

*  Parameters:
*     acb
*        Pointer to the ACB.
*     status
*        The global status.

*  Returned function value:
*     A NULL pointer is always returned.

*  Notes:
*     -  This routine attempts to execute even if 'status' is set on
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

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();

/* If the array is currently mapped through this ACB entry, then unmap it. */
   *status = SAI__OK;
   if( acb->mcb ) ary1Ump( acb, status );

/* Annul the associated data object and set the data object pointer in the
   ACB to NULL. */
   ary1Danl( 1, &acb->dcb, status );

/* Release the ACB. */
   ary1Rls( (AryObject *) acb, status );

/* Annul any error if STATUS was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;

/* Call error tracing routine if appropriate. */
      } else {
         ary1Trace( "ary1Anl", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

   return NULL;
}
