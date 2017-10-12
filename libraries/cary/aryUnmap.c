#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void aryUnmap( Ary *ary, int *status ) {
/*
*+
*  Name:
*     aryUnmap

*  Purpose:
*     Unmap an array.

*  Synopsis:
*     void aryUnmap( Ary *ary, int *status )

*  Description:
*     This function unmaps an array which has previously been mapped for
*     READ, UPDATE or WRITE access.

*  Parameters:
*     ary
*        Array identifier.
*     status
*        The global status.

*  Notes:
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  An error will result if the array has not previously been
*     mapped for access.

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
   AryACB *acb;               /* ACB entry */

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();
   *status = SAI__OK;

/* Import the array identifier and unmap the array. */
   acb = (AryACB *) ary1Impid( ary, 1, 1, 1, status );
   ary1Ump( acb, status );

/* Annul any error if STATUS was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;

/* Report context information and call error tracing routine if
   appropriate. */
      } else {
         errRep( " ", "aryUnmap: Error unmapping an array.",
                 status );
         ary1Trace( "aryUnmap", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

}
