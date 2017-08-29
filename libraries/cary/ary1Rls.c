#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"

void *ary1Rls( AryObject *object, int *status ) {
/*
*+
*  Name:
*     ary1Rls

*  Purpose:
*     Release an ARY "block" structure of any type.

*  Synopsis:
*     void *ary1Rls( AryObject *object, int *status );

*  Description:
*     The routine releases a slot which is currently in use in one of
*     the static arrays used by the ARY_ facility, making it available
*     for re-use.

*  Parameters:
*     object
*        A pointer to the "block" structure to be released. The supplied
*        pointer should be cast from (AryDCB *), (AryACB *), etc, to
*        (AstObject *). All classes of block structure have an AryObject
*        as their first component.
*     status
*        The global status.

*  Returned function value:
*     A NULL pointer is always returned.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no error report will be made if it subsequently
*     fails under these circumstances.

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
   const char *type;
   int tstat;

/* Check an object pointer was supplied. */
   if( !object ) return NULL;

/* Save the STATUS value and mark the error stack. */
   tstat = *status;
   errMark();
   *status = SAI__OK;

/* Get the type of the supplied object - DCB, ACB, MCB or PCB. This also
   helps to guard against random addresses being supplied. */
   if( object->type == ARY__DCBTYPE ) {
      type = "DCB";
   } else if( object->type == ARY__ACBTYPE ) {
      type = "ACB";
   } else if( object->type == ARY__MCBTYPE ) {
      type = "ACB";
   } else if( object->type == ARY__PCBTYPE ) {
      type = "ACB";
   } else {
      *status = ARY__FATIN;
      msgSeti( "T", object->type );
      errRep( " ", "Pointer supplied to function ary1Rls has "
              "illegal type ^T (internal programming error).", status );
   }

/* If the specified object is not in use, then report an error. */
   if( !object->used ){
      if( *status == SAI__OK ) {
         *status = ARY__FATIN;
         msgSetc( "T", type );
         msgSeti( "S", object->slot );
         errRep( " ", "Function ary1Rls called to release an object of "
                 "type '^T' (slot ^S) that is not currently in use "
                 "(internal programming error).", status );
      }

/* Otherwise, release the slot. */
   } else {
      object->used = 0;
   }

/* Annul any error if STATUS was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ){
      if( tstat != SAI__OK ){
         errAnnul( status );
         *status = tstat;
      } else {

/* Call error tracing routine if appropriate. */
         ary1Trace( "ary1Rls", status );
      }
   } else {
      *status = tstat;
   }

/* Release the error stack. */
   errRlse();

   return NULL;
}
