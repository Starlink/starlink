#include <stdio.h>
#include "sae_par.h"
#include "chr.h"

void testIsnam( int *status ){
/*
*+
*  Name:
*     testIsnam

*  Purpose:
*     Test "chrIsnam".

*  Synopsis:
*     void testIsnam( int *status )

*  Description:
*     Test "chrIsnam". If any failure occurs, return "status" = SAI__ERROR.
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

   if( !chrIsnam( "NAME01" ) || chrIsnam( "01NAME" ) || chrIsnam( "NAME@" ) ){
      printf("chrIsnam fails\n");
      *status = SAI__ERROR;
   }

}

