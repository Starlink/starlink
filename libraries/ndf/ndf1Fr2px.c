#include <math.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "ndf_err.h"
#include <math.h>

void ndf1Fr2px( int nax, int ndim, const hdsdim nlbnd[], const hdsdim nubnd[],
                const int isbnd[], double value1[], double value2[],
                int frame1[], int frame2[], int *status ){
/*
*+
*  Name:
*     ndf1Fr2px

*  Purpose:
*     Convert FRACTION axis values to pixel indices.

*  Synopsis:
*     void ndf1Fr2px( int nax, int ndim, const hdsdim nlbnd[],
*                     const hdsdim nubnd[], const int isbnd[],
*                     double value1[], double value2[], int frame1[],
*                     int frame2[], int *status )

*  Description:
*     This function converts any supplied FRACTION values to corresponding
*     pixel indices.

*  Parameters:
*     nax
*        The number of WCS axis bound supplied.
*     ndim
*        The number of pixel axes in the NDF.
*     nlbnd
*        The NDF lower pixel bounds. The supplied "nlbnd" array should have
*        at least "ndim" elements.
*     nubnd
*        The NDF upper pixel bounds. The supplied "nubnd" array should have
*        at least "ndim" elements.
*     isbnd
*        Whether "value1" and "value2" specify the lower and upper bounds
*        directly (i.e. non-zero ==> a ":" separator was given or implied,
*        whereas zero ==> a "~" separator was given). The supplied "isbnd"
*        array should have at least "ndim" elements.
*     value1
*        First value specifying the bound on each axis. The supplied
*        "value1" array should have at least "nax" elements.
*     value2
*        Second value specifying the bound on each axis. The supplied
*        "value2" array should have at least "nax" elements.
*     frame1
*        0 ==> "value1" is to be interpreted as a WCS or axis coordinate
*        value, 1 ==> it is a pixel index, 2 ==> it is a FRACTION value.
*        The supplied "frame1" array should have at least "nax" elements.
*     frame2
*        0 ==> "value2" is to be interpreted as a WCS or axis coordinate
*        value, 1 ==> it is a pixel index, 2 ==> it is a FRACTION value.
*        The supplied "frame2" array should have at least "nax" elements.
*     *status
*        The global status.

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
*     MERCHANTABILITY or FITNESS FOR "a" PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   double a;
   double b;
   int i;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check each supplied axis bound */
   for( i = 0; i < nax; i++ ){

/* Get the constants relating FRACTION value to pixel coord for the
   current pixel axis. */
      if( i < ndim ) {
         b = (double)( nlbnd[ i ] - 1 );
         a = (double)( nubnd[ i ] ) - b;
      } else {
         b = 0.0;
         a = 1.0;
      }

/* First handle cases where we are dealing with a upper and lower bound. */
      if( isbnd[ i ] ) {

/* Convert the lower bound, rounding up to the next higher pixel centre. */
         if( frame1[ i ] == 2 ) {
            value1[ i ] = NDF_NINT( value1[ i ]*a + b ) + 1;
            frame1[ i ] = 1;
         }

/* Convert the upper bound, rounding down to the next lower pixel centre. */
         if( frame2[ i ] == 2 ) {
            value2[ i ] = NDF_NINT( value2[ i ]*a + b );
            frame2[ i ] = 1;
         }

/* Now handle cases where we are dealing with a centre and width. */
      } else {

/* Check the centre value. Skip if it is not a FRACTION value. Otherwise,
   convert the fraction bound into a pixel index. */
         if( frame1[ i ] == 2 ) {
            value1[ i ] = NDF_NINT( value1[ i ]*a + b + 0.5 );
            frame1[ i ] = 1;
         }

/* Convert the fraction range into pixels using an appropriate scaling. */
         if( frame2[ i ] == 2 ) {
            value2[ i ] = NDF_NINT( value2[ i ]*(double)( nubnd[ i ] - nlbnd[ i ] + 1 ) );
            if( value2[ i ] < 1 ) value2[ i ] = 1;
            frame2[ i ] = 1;
         }
      }

   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Fr2PX", status );

}

