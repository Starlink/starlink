#include <math.h>
#include <stdio.h>
#include "sae_par.h"
#include "chr.h"

void testCtod( int *status ){
/*
*+
*  Name:
*     testCtod

*  Purpose:
*     Test chrCtod

*  Synopsis:
*     void testCtod( int *status )

*  Description:
*     Test chrCtod. If any failure occurs, return "status" = SAI__ERROR.
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
   double dp;            /* Double value */
   int istat;            /* Local status */
   double diff;

   if( *status != SAI__OK ) return;

   dp = 0.0;
   istat = SAI__OK;

   chrCtod( "XXX", &dp, &istat );
   if( istat != SAI__ERROR ) {
      printf("chrCtod fails - Error not detected\n");
      *status = SAI__ERROR;
   }

   istat = SAI__OK;
   string = " 3.333333333333";
   chrCtod( string, &dp, &istat );
   diff = fabs( dp - 10.0/3.0 );
   if( ( istat != SAI__OK ) || ( diff > 5.0e-13 ) ) {
      printf( "chrCtod fails - '%s' read as %.20g\n", string, dp );
      *status = SAI__ERROR;
   }

}

