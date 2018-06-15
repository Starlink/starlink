#include <stdio.h>
#include "sae_par.h"
#include "chr.h"

void testLen( int *status ){
/*
*+
*  Name:
*     testLen

*  Purpose:
*     Test "chrLen".

*  Synopsis:
*     void testLen( int *status )

*  Description:
*     Test "chrLen". If any failure occurs, return "status" = SAI__ERROR.
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
   int i;             /* INTEGER value */

   if( *status != SAI__OK ) return;

   i = chrLen( "12345  " );
   if( i != 5 ) {
      printf("chrLen failure, I should be 5, is %d\n", i );
      *status = SAI__ERROR;
   }

   i = chrLen( "  1234  " );
   if( i != 6 ) {
      printf("chrLen failure, I should be 6, is %d\n", i );
      *status = SAI__ERROR;
   }

   i = chrLen( "1234  5" );
   if( i != 7 ) {
      printf("chrLen failure, I should be 7, is %d\n", i );
      *status = SAI__ERROR;
   }

   i = chrLen( "  12  34  " );
   if( i != 8 ) {
      printf("chrLen failure, I should be 8, is %d\n", i );
      *status = SAI__ERROR;
   }

   if( *status != SAI__OK ) printf( "chrLen failed\n" );

}

