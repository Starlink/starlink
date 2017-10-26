#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"

void ary1Stp( const char *type, int cmplx, AryACB *acb, int *status ) {
/*
*+
*  Name:
*     ary1Stp

*  Purpose:
*     Set a new data type for an array identified by its ACB entry.

*  Synopsis:
*     void ary1Stp( const char *type, int cmplx, AryACB *acb, int *status )

*  Description:
*     This function changes the data type of an array identified by its
*     entry in the ACB. If the array's state is "defined", then the
*     data it contains undergo type conversion, otherwise no conversion
*     is necessary. Conversions from non-complex to complex data types
*     (and vice versa) are handled.

*  Parameters:
*     type
*        The new numeric data type for the array; a primitive numeric
*        HDS data type string (case insensitive).
*     cmplx
*        Whether the new data type is to be complex.
*     acb
*        Pointer to the ACB.
*     status
*        The global status.

*  Notes:
*     -  This routine has no effect if the array is not a base array.

*  Side Effects:
*     -  The array's bad pixel flag may be set if data conversion
*     errors occur.

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
   int dce;                   /* Data conversion error occurred? */
   AryDCB *dcb;               /* Pointer to the data object */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If an array section has been provided, then check to ensure that it is
   not mapped and report an error if it is. Otherwise do nothing, as the
   data type of a section cannot be changed except through the base array. */
   if( acb->cut ){
      if( acb->mcb ){
         *status = ARY__ISMAP;
         dcb = acb->dcb;
         datMsg( "ARRAY", dcb->loc );
         errRep( " ", "The array ^ARRAY is mapped for access through the "
                 "specified identifier (possible programming error).",
                 status );
      }

/* If the array is a base array, then see if any part of it is mapped for
   access. Report an error if it is. */
   } else {
      dcb = acb->dcb;
      if( ( dcb->nread != 0 ) || ( dcb->nwrite != 0 ) ){
         *status = ARY__ISMAP;
         datMsg( "ARRAY", dcb->loc );
         errRep( " ", "The base array '^ARRAY' is mapped for access, perhaps "
                 "through another identifier (possible programming error).",
                 status );

/* Convert the data object to its new type. */
      } else {
         ary1Dstp( type, cmplx, dcb, &dce, status );
         if( *status == SAI__OK ){

/* If data conversion errors occurred, then set the bad pixel flag for the
   array. */
            if( dce ) ary1Sbd( 1, acb, status );
         }
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Stp", status );

}
