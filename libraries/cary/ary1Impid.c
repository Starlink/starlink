#include <stdio.h>
#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"

AryACB *ary1Impid( const Ary *ary, int checklock, int readonly, int *status ) {
/*
*+
*  Name:
*     ary1Impid

*  Purpose:
*     Import an identifier.

*  Synopsis:
*     AryACB *ary1Impid( const Ary *ary, int checklock, int readonly,
*                        int *status )

*  Description:
*     The routine converts an array identifier, previously issued by
*     ary1Expid into a pointer to the appropriate ACB structure.
*     The identifier value is fully checked and an error is reported if
*     it is not valid.

*  Parameters:
*     ary
*        Array identifier, in the form of a pointer as issued by ary1Expid.
*     checklock
*        If non-zero, check that the supplied array is locked
*        appropriately by the current thread.
*     readonly
*        Only used iof "checklock" is non-zero, in which case it gives
*        the type of lock required; read-only if non-zero, and read-write
*        if zero.
*     status
*        The global status.

*  Returned function value:
*     Pointer to the ACB structure, or NULL if an error occurs.

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

/* Local Variables: */
   AryACB *result = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return result;

/* Convert the identifier to an ACB index. */
   result = ary1Id2ac( ary );

/* If a valid ACB was not returned, then report an error. */
   if( ! result ){
      *status = ARY__IDINV;
      errRep( " ", "Array identifier invalid (possible programming error).",
              status );

/* If required, check that the array is locked appropriately by the
   current thread. */
   } else if( checklock ){
      *status = ARY__FATIN;
      errRep( " ", "ary1Impid: Lock checking not yet implemented!", status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) {
      result = NULL;
      ary1Trace( "ary1Impid", status );
   }

   return result;

}
