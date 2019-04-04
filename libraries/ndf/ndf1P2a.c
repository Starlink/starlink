#include "sae_par.h"
#include "dat_par.h"
#include "ndf_err.h"
#include "ndf1.h"
#include "mers.h"
#include <math.h>

void ndf1P2a( int n, const int ipix[], hdsdim lbnd, hdsdim ubnd, int havcen,
              int havwid, int havvar, const double centre[],
              const double width[], const double varian[], double cen[],
              double wid[], double var[], int *status ){
/*
*+
*  Name:
*     ndf1P2a

*  Purpose:
*     Convert pixel indices into axis centre, width and variance values.

*  Synopsis:
*     void ndf1P2a( int n, const int ipix[], hdsdim lbnd, hdsdim ubnd,
*                   int havcen, int havwid, int havvar, const double
*                   centre[], const double width[], const double varian[],
*                   double cen[], double wid[], double var[], int *status )

*  Description:
*     This function converts a series of pixel indices for a single NDF
*     dimension into the corresponding axis coordinate system values,
*     namely: the pixel centre, width and variance. The function will
*     provide the correct default values if the appropriate axis arrays are
*     not available, and will also extrapolate the axis coordinate system
*     in either direction if necessary.

*  Parameters:
*     n
*        Number of pixel index values to be converted.
*     ipix
*        Array of pixel indices to be converted. The supplied "ipix" array
*        should have at least "n" elements.
*     lbnd
*        Lower pixel index bound for the NDF dimension.
*     ubnd
*        Upper pixel index bound for the NDF dimension.
*     havcen
*        Whether an array of pixel centre positions is available.
*     havwid
*        Whether an array of pixel width values is available.
*     havvar
*        Whether an array of pixel variance values is available.
*     centre
*        Array of pixel centre positions. This is only used if "havcen" is
*        non-zero. The supplied "centre" array should have at least "ubnd -
*        lbnd + 1" elements.
*     width
*        Array of pixel width values. This is only used if both "havcen"
*        and "havwid" are non-zero. The supplied "width" array should have
*        at least "ubnd - lbnd + 1" elements.
*     varian
*        Array of pixel variance values. This is only used if both "havcen"
*        and "havvar" are non-zero. The supplied "varian" array should have
*        at least "ubnd - lbnd + 1" elements.
*     cen
*        Returned holding the array of centre positions for the selected
*        pixels. The supplied "cen" array should have at least "n"
*        elements.
*     wid
*        Returned holding the array of width values for the selected
*        pixels. The supplied "wid" array should have at least "n"
*        elements.
*     var
*        Returned holding the array of variance values for the selected
*        pixels. The supplied "var" array should have at least "n"
*        elements.
*     *status
*        The global status.

*  Notes:
*     -  This function performs checks on the validity of the centre, width
*     and variance values it returns and will report an error if they are
*     invalid.

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   double space;         /* Pixel spacing for extrapolation */
   int i;                /* Loop counter for pixel indices */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If no pixel centre coordinates are provided, then loop to calculate
   all the centre positions, widths and variances directly. */
   if( !havcen ) {
      for( i = 0; i < n; i++ ){
         cen[ i ] = (double)( ipix[ i ] ) - 0.5;
         wid[ i ] = 1.0;
         var[ i ] = 0.0;
      }

/* Otherwise loop to calculate these values from the array(s) provided. */
   } else {
      for( i = 0; i < n; i++ ){

/* First test to see if the pixel index lies below the lower bound of
   the pixel centre array. */
         if( ipix[ i ] < lbnd ) {

/* If so, then obtain the pixel spacing to use for extrapolation from
   the spacing of the two nearest pixel centres, if available. */
            if( ubnd > lbnd ) {
               space = centre[ lbnd ] - centre[ lbnd - 1 ];

/* Check that the spacing is not zero; this indicates invalid centre
   values. */
               if( space == 0.0 ) {
                  *status = NDF__AXVIN;
                  errRep( " ", "Axis CENTRE values do not increase or "
                          "decrease monotonically.", status );
                  goto L99;
               }

/* Use a spacing of unity if only one centre value is available. */
            } else {
               space = 1.0;
            }

/* Extrapolate to obtain the pixel centre position. */
            cen[ i ] = centre[ lbnd - 1 ] - space*(double)( lbnd - ipix[ i ] );

/* If pixel width values have been provided, then use the width of the
   nearest pixel, checking it for validity. */
            if( havwid ) {
               wid[ i ] = width[ lbnd - 1 ];
               if( wid[ i ] < 0.0 ) {
                  *status = NDF__AXVIN;
                  errRep( " ", "Invalid negative axis WIDTH value "
                          "encountered.", status );
                  goto L99;
               }

/* Otherwise, use the pixel spacing to generate a width value. */
            } else {
               wid[ i ] = fabs( space );
            }

/* The extrapolated variance value is zero. */
            var[ i ] = 0.0;

/* If the pixel index lies above the upper bound of the pixel centre
   array, then obtain the pixel spacing to use for extrapolation from
   the spacing of the two nearest pixel centres. */
         } else if( ipix[ i ] > ubnd ) {
            if( ubnd > lbnd ) {
               space = centre[ ubnd - 1 ] - centre[ ubnd - 2 ];

/* Check that the spacing is not zero; this indicates invalid centre
   values. */
               if( space == 0.0 ) {
                  *status = NDF__AXVIN;
                  errRep( " ", "Axis CENTRE values do not increase or "
                          "decrease monotonically.", status );
                  goto L99;
               }

/* Use a spacing of unity if only one centre value is available. */
            } else {
               space = 1.0;
            }

/* Extrapolate to obtain the pixel centre position. */
            cen[ i ] = centre[ ubnd - 1 ] + space*(double)( ipix[ i ] - ubnd );

/* If pixel width values have been provided, then use the width of the
   nearest pixel, checking it for validity. */
            if( havwid ) {
               wid[ i ] = width[ ubnd - 1 ];
               if( wid[ i ] < 0.0 ) {
                  *status = NDF__AXVIN;
                  errRep( " ", "Invalid negative axis WIDTH value "
                          "encountered.", status );
                  goto L99;
               }

/* Otherwise, use the pixel spacing to generate a width value. */
            } else {
               wid[ i ] = fabs( space );
            }

/* The extrapolated variance value is zero. */
            var[ i ] = 0.0;

/* If the pixel index lies within the bounds of the pixel centre array,
   then extract the appropriate centre position. */
         } else {
            cen[ i ] = centre[ ipix[ i ] - 1 ];

/* If pixel width values have been provided, then extract the matching
   pixel width, checking it for validity. */
            if( havwid ) {
               wid[ i ] = width[ ipix[ i ] - 1 ];
               if( wid[ i ] < 0.0 ) {
                  *status = NDF__AXVIN;
                  errRep( " ", "Invalid negative axis WIDTH value "
                          "encountered.", status );
                  goto L99;
               }

/* Otherwise, calculate the width from the centre spacing of
   neighbouring pixels. Use both neighbours if available. */
            } else {
               if( ( ipix[ i ] > lbnd ) && ( ipix[ i ] < ubnd ) ) {
                  wid[ i ] = 0.5*fabs( centre[ ipix[ i ] ] - centre[ ipix[
                                       i ] - 2 ] );

/* Use only one neighbour if the other lies outside the bounds of the
   pixel centre array. */
               } else if( ipix[ i ] > lbnd ) {
                  wid[ i ] = fabs( centre[ ipix[ i ] - 1 ] - centre[ ipix[
                                   i ] - 2 ] );
               } else if( ipix[ i ] < ubnd ) {
                  wid[ i ] = fabs( centre[ ipix[ i ] ] - centre[ ipix[ i ]
                                   - 1 ] );

/* Use a width value of unity if only one centre position has been
   supplied. */
               } else {
                  wid[ i ] = 1.0;
               }
            }

/* If an array of variance values has been provided, then extract the
   appropriate value, checking it for validity. */
            if( havvar ) {
               var[ i ] = varian[ ipix[ i ] - 1 ];
               if( var[ i ] < 0.0 ) {
                  *status = NDF__AXVIN;
                  errRep( " ", "Invalid negative axis VARIANCE value "
                          "encountered.", status );
                  goto L99;
               }

/* Otherwise return a variance value of zero. */
            } else {
               var[ i ] = 0.0;
            }
         }
      }
   }

/* Arrive here if an error occurs. */
L99:

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1P2A", status );

}

