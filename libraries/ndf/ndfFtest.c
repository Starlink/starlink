#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

int main(){
/*
*+
*  Name:
*     ndfTest

*  Purpose:
*     Test installation of NDF From Fortran.

*  Description:
*     This program should be run after building and installing NDF in order
*     to test for correct installation. Note that this is not an exhaustive
*     test of NDF, but only of its installation.

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   int dim[ 2 ] = { 10, 20 };      /* NDF dimensions */
   int el;               /* Number of mapped elements */
   int indf;             /* NDF identifier */
   int isum;             /* Sum of array elements */
   int place;            /* NDF placeholder */
   void *pntr;           /* Pointer to mapped array */

/* Initialise the global status. */
   status = SAI__OK;

/* Create a new file containing an NDF. */
   ndfOpen( NULL, "ndf_test", "WRITE", "NEW", indf, place, status );
   ndfNewp( "_INTEGER", 2, dim, place, indf, status );

/* Map the NDF's data array. */
   ndfMap( indf, "Data", "_REAL", "WRITE", pntr, el, status );

/* Initialise the array. */
   setup( el, pntr, status );

/* Clean up. */
   ndfAnnul( indf, status );

/* Re-open the NDF. */
   ndfOpen( NULL, "ndf_test", "UPDATE", "OLD", indf, place, status );

/* Map its data array. */
   ndfMap( indf, "Data", "_INTEGER", "READ", pntr, el, status );

/* Sum the data elements. */
   sum( el, pntr, isum, status );

/* Clean up, deleting the NDF. */
   ndfDelet( indf, status );

/* Check if the test ran OK. If so, then report success. */
   if( ( status == SAI__OK ) && ( isum == 20100 ) ) {
      printf( "*********************************************\n");
      printf( "*                                           *\n");
      printf( "*  NDF Fortran installation test succeeded  *\n");
      printf( "*                                           *\n");
      printf( "*********************************************\n");

/* Otherwise, report an error. */
   } else {
      if( status == SAI__OK ) status = SAI__ERROR;
      errRep( " ", "ndfTest_F: NDF Fortran installation test failed.", status );
   }

}

void setup( int el, float array[], int *status ){
/*
*+
*  Name:
*     setup

*  Purpose:
*     Initialise an array.

*  Synopsis:
*     void setup( int el, float array[], int *status )

*  Description:
*     Set each element of a 1-dimensional array equal to its element
*     number.

*  Parameters:
*     el
*        Number of array elements.
*     array
*        Returned holding the array to be initialised.
*     *status
*        The global status.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   int i;                /* Loop counter */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise the array. */
   for( i = 0; i < el; i++ ){
      array[ i ] = real( i + 1 );
   }

}


void sum( int el, const int array[], int *isum, int *status ){
/*
*+
*  Name:
*     sum

*  Purpose:
*     Sum the elements of an array.

*  Synopsis:
*     void sum( int el, const int array[], int *isum, int *status )

*  Description:
*     Return the sum of the elements of a 1-dimensional array.

*  Parameters:
*     el
*        Number of array elements.
*     array
*        Array whose elements are to be summed.
*     *isum
*        Returned holding the sum of array elements.
*     *status
*        The global status.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   int i;                /* Loop counter */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise. */
   *isum = 0;

/* Sum the array elements. */
   for( i = 0; i < el; i++ ){
      *isum += array[ i ];
   }

}

