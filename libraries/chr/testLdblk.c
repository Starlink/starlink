#include <stdio.h>
#include "sae_par.h"
#include "star/util.h"
#include "chr.h"

void testLdblk( int *status ){
/*
*+
*  Name:
*     testLdblk

*  Purpose:
*     Test "chrLdblk".

*  Synopsis:
*     void testLdblk( int *status )

*  Description:
*     Test "chrLdblk". If any failure occurs, return "status" = SAI__ERROR.
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
   char string[ 121 ];

   if( *status != SAI__OK ) return;

   star_strlcpy( string, "   A B  C   ", sizeof( string ) );

   chrLdblk( string );
   if( strcmp( string, "A B  C   " ) ) {
      printf("chrLdblk fails - string is:'%s'\n", string );
      *status = SAI__ERROR;
   }

}

