#include <stdio.h>
#include "star/util.h"
#include "chr.h"
#include "sae_par.h"

void testClean( int *status ){
/*
*+
*  Name:
*     testClean

*  Purpose:
*     Test "chr.h".

*  Synopsis:
*     void testClean( int *status )

*  Description:
*     Test chrClean. If any failure occurs, return "status" = SAI__ERROR.
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

   if( *status != SAI__OK ) return;

   star_strlcpy( stars, "*****", sizeof( stars ) );
   chrClean( stars );
   if( strcmp( stars, "*****" ) ) {
      printf("chrClean fails - string is '%s'.\n", stars );
      *status = SAI__ERROR;
   }

   stars[3] = 12;
   chrClean( stars );
   if( strcmp( stars, "*** *" ) && *status == SAI__OK ) {
      printf("chrClean fails - string is '%s'.\n", stars );
      *status = SAI__ERROR;
   }

}

