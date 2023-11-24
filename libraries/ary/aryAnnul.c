#include "sae_par.h"
#include "ary1.h"
#include "mers.h"

void aryAnnul( Ary **ary, int *status ) {
/*
*+
*  Name:
*     aryAnnul

*  Purpose:
*     Annul an array pointer.

*  Synopsis:
*     void aryAnnul( Ary **ary, int *status )

*  Description:
*     This function annuls the array pointer supplied so that it is no
*     longer recognised as a valid pointer by the ARY_ routines.
*     Any resources associated with it are released and made available
*     for re-use. If the array is mapped for access, then it is
*     automatically unmapped by this routine.

*  Parameters:
*     ary
*        Address of the array pointer to be annulled. A value of NULL is
*        returned in place of the supplied pointer.
*     status
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if 'status' is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances. In particular, it
*     will fail if the pointer supplied is not initially valid, but
*     this will only be reported if 'status' is set to SAI__OK on entry.
*     -  An error will result if an attempt is made to annul the last
*     remaining pointer associated with an array which is in an
*     undefined state (unless it is a temporary array, in which case it
*     will be deleted at this point).

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
   AryACB *acb;               /* ACB structure describing supplied array */
   int tstat;                 /* Temporary status variable */

/* Return without action if a null pointer is supplied. */
   if( !ary ) return;

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();

/* Import the array pointer. */
   *status = SAI__OK;
   acb = (AryACB *) ary1Impid( *ary, 1, 1, 1, status );

/* Annul the associated ACB entry and reset the array pointer value. */
   if( *status == SAI__OK ){
      ARY__DCB_LOCK_MUTEX;
      ARY__ACB_LOCK_MUTEX;
      acb = ary1Anl( acb, status );
      ARY__ACB_UNLOCK_MUTEX;
      ARY__DCB_UNLOCK_MUTEX;
   }
   *ary = NULL;

/* Annul any error if STATUS was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;

/* Report context information and call error tracing routine if
   appropriate. */
      } else {
         errRep( " ", "aryAnnul: Error annulling array pointer.", status );
         ary1Trace( "aryAnnul", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

}
