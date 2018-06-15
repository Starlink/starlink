#include <stdio.h>
#include "sae_par.h"
#include "chr.h"

void testIsalm( int *status ){
/*
*+
*  Name:
*     testIsalm

*  Purpose:
*     Test "chrIsalm".

*  Synopsis:
*     void testIsalm( int *status )

*  Description:
*     Test "chrIsalm". If any failure occurs, return "status" = SAI__ERROR.
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

   if( *status != SAI__OK ) return;

   if( !chrIsalm( 'A' ) || !chrIsalm( 'a' ) || !chrIsalm( 'Z' ) ||
       !chrIsalm( 'z' ) || !chrIsalm( '0' ) || !chrIsalm( '9' ) ||
       chrIsalm( '!' ) || chrIsalm( '/' ) || chrIsalm( '@' ) ) {
      printf("chr_isalm fails\n");
      *status = SAI__ERROR;
   }

}

