#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"
#include "dat_par.h"
#include <string.h>

void aryNoacc( const char *access, Ary *ary, int *status ) {
/*
*+
*  Name:
*     aryNoacc

*  Purpose:
*     Disable a specified type of access to an array.

*  Synopsis:
*     void aryNoacc( const char *access, Ary *ary, int *status )

*  Description:
*     This function disables the specified type of access to an array,
*     so that any subsequent attempt to access it in that way will fail.
*     Access restrictions imposed on an array identifier by this
*     routine will be propagated to any new identifiers derived from
*     it, and cannot be revoked.

*  Parameters:
*     access
*        The type of access to be disabled: 'BOUNDS', 'DELETE',
*        'MODIFY', 'SCALE', 'SHIFT', 'TYPE' or 'WRITE'.
*     ary
*        Array identifier.
*     status
*        The global status.

*  Notes:
*     Disabling each type of access imposes the following restrictions
*     on an array:
*     -  'BOUNDS' prevents the pixel-index bounds of a base array from
*     being altered.
*     -  'DELETE' prevents the array being deleted.
*     -  'MODIFY' prevents any form of modification to the array (i.e.
*     it disables all the other access types).
*     -  'SCALE' prevents the scale and zero values from being changed.
*     -  'SHIFT' prevents pixel-index shifts from being applied to a
*     base array.
*     -  'TYPE' prevents the data type of the array from being altered.
*     -  'WRITE' prevents new values from being written to the array,
*     or the array's state from being reset.

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
   AryACB *acb;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the array identifier. */
   acb = (AryACB *) ary1Impid( ary, 1, 0, 1, status );
   if( *status == SAI__OK ){

/* Check for each type of access in turn and reset the appropriate access
   control flag. */

/* ...BOUNDS access; prevents the array's bounds from being altered. */
      if( !strcasecmp( access, "BOUNDS" ) ){
         acb->access &= ~ARY__ACC_BOUND;

/* ...DELETE access; prevents the array being deleted. */
      } else if( !strcasecmp( access, "DELETE" ) ){
         acb->access &= ~ARY__ACC_DELET;

/* ...MODIFY access; prevents any form of modification to the array. */
      } else if( !strcasecmp( access, "MODIFY" ) ){
         acb->access &= ~ARY__ACC_BOUND;
         acb->access &= ~ARY__ACC_DELET;
         acb->access &= ~ARY__ACC_SHIFT;
         acb->access &= ~ARY__ACC_TYPE;
         acb->access &= ~ARY__ACC_SCALE;
         acb->access &= ~ARY__ACC_WRITE;

/* ...SCALE access; prevents the array's scale and zero values being
   altered. */
      } else if( !strcasecmp( access, "SCALE" ) ){
         acb->access &= ~ARY__ACC_SCALE;

/* ...SHIFT access; prevents pixel index shifts from being applied to the
   array. */
      } else if( !strcasecmp( access, "SHIFT" ) ){
         acb->access &= ~ARY__ACC_SHIFT;

/* ...TYPE access; prevents the array's data type being altered. */
      } else if( !strcasecmp( access, "TYPE" ) ){
         acb->access &= ~ARY__ACC_TYPE;

/* ...WRITE access; inhibits the writing of new data values or the
   resetting of the array's state. */
      } else if( !strcasecmp( access, "WRITE" ) ){
         acb->access &= ~ARY__ACC_WRITE;

/* If the access type was not recognised, then report an error. */
      } else {
         *status = ARY__ACCIN;
         msgSetc( "BADACC", access );
         errRep( " ", "Invalid access type '^BADACC' specified (possible"
                 " programming error).", status );
      }
   }

/* If an error occurred, then report context information and call the error
   tracing routine. */
   if( *status != SAI__OK ){
      errRep( " ", "aryNoacc: Error disabling access to an array.", status );
      ary1Trace( "aryNoacc", status );
   }

}
