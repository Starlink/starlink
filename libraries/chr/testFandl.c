#include <stdio.h>
#include "chr.h"
#include "sae_par.h"

void testFandl( int *status ){
/*
*+
*  Name:
*     testFandl

*  Purpose:
*     Test "chrFandl".

*  Synopsis:
*     void testFandl( int *status )

*  Description:
*     Test "chrFandl". If any failure occurs, return "status" = SAI__ERROR.
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
   const char *string;
   size_t ptr1;
   size_t ptr2;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* First an empty string */
   chrFandl( "  ", &ptr1, &ptr2 );
   if( ptr1 != 1 || ptr2 != 0 ) {
      printf("chrFandl fails - two spaces returns %zu %zu\n", ptr1, ptr2 );
      *status = SAI__ERROR;
   }

/* Now a null string */
   chrFandl( "", &ptr1, &ptr2 );
   if( ptr1 != 1 || ptr2 != 0 ) {
      printf("chrFandl fails - null string returns %zu %zu\n", ptr1, ptr2 );
      *status = SAI__ERROR;
   }

/* Now a normal string including internal spaces */
   string = "   A B  C   ";
   chrFandl( string, &ptr1, &ptr2 );
   if( ptr1 != 3 || ptr2 != 8 ) {
      printf( "chrFandl fails - string is:'%s' pointers are %zu %zu\n",
              string, ptr1, ptr2 );
      *status = SAI__ERROR;
   }

}

