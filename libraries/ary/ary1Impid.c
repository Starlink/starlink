#include <stdio.h>
#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"

AryObject *ary1Impid( const void *id, int checklock, int readonly,
                      char isacb, int *status ) {
/*
*+
*  Name:
*     ary1Impid

*  Purpose:
*     Import an identifier.

*  Synopsis:
*     AryObject *ary1Impid( const void *id, int checklock, int readonly,
*                           char isacb, int *status )

*  Description:
*     The routine converts an array identifier, previously issued by
*     ary1Expid into a pointer to the appropriate ACB structure.
*     The identifier value is fully checked and an error is reported if
*     it is not valid.

*  Parameters:
*     id
*        An identifier for the object to be imported, in the form of an
*        Ary or AryPlace pointer as issued by ary1Expid.
*     checklock
*        If non-zero, check that the supplied identifier is locked
*        appropriately by the current thread.
*     readonly
*        Only used if "checklock" is non-zero, in which case it gives
*        the type of lock required; read-only if non-zero, and read-write
*        if zero.
*     isacb
*        If non-zero, the supplied object is expected to be an ACB.
*        Otherwise it is expected to be a PCB.
*     status
*        The global status.

*  Returned function value:
*     Pointer to the ACB or PCB structure, cast to an AryObject,
*     or NULL if an error occurs.

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
*     4-SEP-2017 (DSB):
*        Modified to be able to handle ACB or PCB objects.

*-
*/

/* Local Variables: */
   AryObject *result = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return result;

/* Convert the identifier to an ACB index. */
   result = ary1Id2ac( id, isacb );

/* If a valid ACB was not returned, then report an error. */
   if( !result ){
      *status = ARY__IDINV;
      if( isacb ) {
         msgSetc( "T", "Array" );
      } else {
         msgSetc( "T", "Placeholder" );
      }
      errRep( " ", "Array identifier invalid (possible programming error).",
              status );

/* Otherwise, if the object is of the wrong type, report an error. */
   } else if( isacb && result->type != ARY__ACBTYPE ){
      *status = ARY__IDINV;
      errRep( " ", "Supplied identifier is not an Array identifier.", status );

   } else if( !isacb && result->type != ARY__PCBTYPE ){
      *status = ARY__IDINV;
      errRep( " ", "Supplied identifier is not a Placeholder identifier.", status );

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
