#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "star/cmp.h"
#include <string.h>

void ary1Extyp( AryDCB *dcb, char type[DAT__SZTYP+1], int *status ) {
/*
*+
*  Name:
*     ary1Extyp

*  Purpose:
*     Obtain the external numeric type of a possible scaled array.

*  Synopsis:
*     void ary1Extyp( AryDCB *dcb, char type[DAT__SZTYP+1], int *status )

*  Description:
*     This function returns the numeric type of the externally visible
*     data in an array. For SIMPLE and PRIMITIVE arrays this is just the
*     numeric type of the data array. For SCALED and DELTA arrays it is
*     the numeric type of the SCALE value stored in the data object.

*  Parameters:
*     dcb
*        The data object (DCB).
*     type
*        Returned holding the external numeric type of the array.
*     status
*        The global status.

* Prior Requirements:
*     -  The DCB mutex must be locked.

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
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine.

*-
*/

/* Local variables: */
   int there;                /* Does the SCALE component exist? */

   ARY__DCB_ASSERT_MUTEX;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that storage form, type and scaling information is available. */
   ary1Dfrm( dcb, status );
   ary1Dtyp( dcb, status );
   ary1Dscl( dcb, status );

/* For SCALED arrays use the data type of the SCALE value in the scaling
   information. */
   if( !strcmp( dcb->form, "SCALED" ) ){
      cmpType( dcb->scloc, "SCALE", type, status );

/* For DELTA arrays use the data type of the SCALE value in the data
   object, if present. Otherwise use the data type of the DATA array. */
   } else if( !strcmp( dcb->form, "DELTA" ) ){
      datThere( dcb->loc, "SCALE", &there, status );
      if( there ){
         cmpType( dcb->loc, "SCALE", type, status );
      } else {
         ary1Ccpy( dcb->type, DAT__SZTYP + 1, type, status );
      }

/* For othe forms, use the data type of the DATA array. */
   } else {
      ary1Ccpy( dcb->type, DAT__SZTYP + 1, type, status );
   }

}
