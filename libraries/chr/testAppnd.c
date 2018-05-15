#include "sae_par.h"
#include "chr.h"
#include "star/util.h"
#include <stdio.h>

void testAppnd( int *status ){
/*
*+
*  Name:
*     testAppnd

*  Purpose:
*     Test chrAppnd

*  Synopsis:
*     void testAppnd( int *status )

*  Description:
*     Test chrAppnd. If any failure occurs, return "status" = SAI__ERROR.
*     Otherwise, "status" is unchanged.

*  Parameters:
*     *status
*        Returned holding the status of the tests.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (EAO)

*  History:
*     15-MAY-2018 (DSB):
*        Original version, based on equivalent Fortran function.

*-
*/

/* Local Variables: */
   char stars[ 11 ];
   size_t ptr1;             /* String index */

   if( *status != SAI__OK ) return;

   star_strlcpy( stars, "*****", sizeof( stars ) );
   ptr1 = 5;
   chrAppnd( "**.", stars, sizeof( stars ), &ptr1 );
   if( strcmp( stars, "*******." ) || ptr1 != 8 ) {
      printf("chr_appnd fails - string is '%s' ptr1 is %zu\n", stars, ptr1 );
      *status = SAI__ERROR;
   }

}

