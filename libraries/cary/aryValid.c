#include "sae_par.h"
#include "ary1.h"
#include "dat_par.h"

int aryValid( Ary *ary, int *status ) {
/*
*+
*  Name:
*     aryValid

*  Purpose:
*     Determine whether an array identifier is valid.

*  Synopsis:
*     int aryValid( Ary *ary, int *status )

*  Description:
*     Determine whether an array identifier is valid (i.e. associated
*     with an array).

*  Parameters:
*     ary
*        Array identifier to be tested.
*     status
*        The global status.

*  Returned Value:
*     A flag indicating if the identifier is valid.

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
   AryACB *acb;                  /* Associated ACB */

/* Check inherited global status. */
   if( *status != SAI__OK ) return 0;

/* Try to convert the identifier into an ACB index. */
   acb = (AryACB *) ary1Id2ac( ary, 1 );

/* Note whether the attempt succeeded; it did not if a value of NULL was
   returned. */
   return ( acb != NULL );

}
