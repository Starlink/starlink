#include <stdio.h>
#include "chr.h"
#include "sae_par.h"

void testFparx( int *status ){
/*
*+
*  Name:
*     testFparx

*  Purpose:
*     Test "chr.h".

*  Synopsis:
*     void testFparx( int *status )

*  Description:
*     Test chrFparx. If any failure occurs, return "status" = SAI__ERROR.
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
   size_t f;
   size_t l;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* First a string without parens */
   chrFparx( "Hello", '(', ')', &f, &l );
   if( l > f ) {
      printf("chrFparx fails - Found paren where none %zu %zu\n", f, l );
      *status = SAI__ERROR;
   }

/* Now a normal string with parens */
   string = "Hello (a,b) Goodbye ";
   chrFparx( string, '(', ')', &f, &l );
   if( ( f != 6 || l != 10 ) && *status == SAI__OK ) {
      printf("chrFparx fails - string is '%s' positions are %zu %zu\n",
             string, f, l );
      *status = SAI__ERROR;
   }

/* Now a string with nested parens */
   string = "call fred( 1.2, '(Goodbye)', status );";
   chrFparx( string, '(', ')', &f, &l );
   if( ( f != 9 || l != 36 ) && *status == SAI__OK ) {
      printf("chrFparx fails - string is '%s' positions are %zu %zu\n",
             string, f, l );
      *status = SAI__ERROR;
   }

}

