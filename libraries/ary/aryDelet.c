#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "dat_par.h"

void aryDelet( Ary **ary, int *status ) {
/*
*+
*  Name:
*     aryDelet

*  Purpose:
*     Delete an array.

*  Synopsis:
*     void aryDelet( Ary **ary, int *status )

*  Description:
*     This function deletes the specified array. If this is a base array,
*     then the associated data object is erased and all array
*     identifiers which refer to it (or to sections derived from it)
*     become invalid. If the array is mapped for access, then it is
*     first unmapped.  If an array section is specified, then this
*     function is equivalent to calling aryAnnul.

*  Parameters:
*     ary
*        Identifier for the array to be deleted. A value of NULL
*        is returned.
*     status
*        The global status.

*  Notes:
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  A value of NULL is always returned for the "ary" argument,
*     even if the function should fail.

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
   AryACB *acb;               /* The ACB for the given array */
   int tstat;                 /* Temporary status variable */

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();

/* Import the array identifier. */
   *status = SAI__OK;
   acb = (AryACB *) ary1Impid( *ary, 1, 0, 1, status );

/* Check that DELETE access to the array is available. */
   ary1Chacc( acb, "DELETE", status );

/* If access is available, then perform a deletion operation on the ACB
   entry. Reset the identifier value. */
   if( *status == SAI__OK ) ary1Del( &acb, status );
   *ary = NULL;

/* Annul any error if "status" was previously bad, otherwise let the new
   error report stand. Release the error stack. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;

/* Report context information and call error tracing routine if
   appropriate. */
      } else {
         errRep( " ", "aryDelet: Error deleting an array.", status );
         ary1Trace( "aryDelet", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

}
