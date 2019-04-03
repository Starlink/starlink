#include "sae_par.h"
#include "dat_par.h"
#include "ndf_err.h"
#include "ndf1.h"
#include <math.h>
#include "mers.h"

void ndf1A2p( int n, const double ax[], hdsdim lbnd, hdsdim ubnd,
              int havcen, int havwid, const double centre[],
              const double width[], int *inc, int ipix0[], double cent0[],
              double space0[], int inpix[], int ipix1[], double cent1[],
              double width1[], int *status ){
/*
*+
*  Name:
*     ndf1A2p

*  Purpose:
*     Convert axis coordinates into pixel indices and related information.

*  Synopsis:
*     void ndf1A2p( int n, const double ax[], hdsdim lbnd, hdsdim ubnd,
*                   int havcen, int havwid, const double centre[],
*                   const double width[], int *inc, int ipix0[],
*                   double cent0[], double space0[], int inpix[],
*                   int ipix1[], double cent1[], double width1[], int *status )

*  Description:
*     This function converts a sequence of axis coordinate values for an
*     NDF axis into corresponding pixel index information. Since NDF pixels
*     may be discontiguous, any particular axis value may lie in a single
*     pixel, in several pixels or in no pixels at all. This function
*     returns information about the index of the pixel lying immediately
*     "below" the axis coordinate and about a second neighbouring pixel
*     which is either the pixel in which the coordinate lies, or the one
*     whose centre lies nearest to the coordinate supplied. A flag
*     indicating whether the coordinate lies within this pixel is also
*     returned.

*  Parameters:
*     n
*        Number of axis coordinate values to be converted.
*     ax
*        Array of axis coordinate values. The supplied "ax" array should
*        have at least "n" elements.
*     lbnd
*        Lower pixel index bound of the NDF dimension to which the axis
*        coordinates refer.
*     ubnd
*        Upper pixel index bound of the NDF dimension to which the axis
*        coordinates refer.
*     havcen
*        Whether an array of pixel centre positions is available for the
*        NDF dimension.
*     havwid
*        Whether an array of pixel width values is available for the NDF
*        dimension. This parameter is not used unless "havcen" is non-zero.
*     centre
*        Array of pixel centre positions, which should either increase or
*        decrease monotonically. This parameter is not used unless "havcen"
*        is set to non-zero. The supplied "centre" array should have at
*        least "ubnd - lbnd + 1" elements.
*     width
*        Array of non-negative pixel width values. This parameter is not
*        used unless both "havcen" and "havwid" are set to non-zero. The
*        supplied "width" array should have at least "ubnd - lbnd + 1"
*        elements.
*     *inc
*        A value of non-zero is returned if the axis centre values (or the
*        defaults used in their place) increase monotonically with pixel
*        index and zero is returned if they decrease monotonically. An
*        error will be reported, and "status" set, if a non-monotonic set
*        of centre values is detected.
*     ipix0
*        Returned holding the returns the index of the NDF pixel "below"
*        the corresponding axis coordinate. If "inc" is non-zero, this will
*        be the index of the pixel with the largest centre position less
*        than or equal to the axis coordinate. If "inc" is zero, it will be
*        the index of the pixel with the smallest centre position greater
*        than or equal to the axis coordinate. The supplied "ipix0" array
*        should have at least "n" elements.
*     cent0
*        Returned holding the returns the centre coordinate of the pixel
*        with index "ipix0". The supplied "cent0" array should have at
*        least "n" elements.
*     space0
*        Returned holding the returns the signed difference between the
*        centre coordinate of the pixel with index "ipix0" + 1 and the
*        centre position of the pixel with index "ipix0". The supplied
*        "space0" array should have at least "n" elements.
*     inpix
*        Returned holding the returns non-zero if the axis coordinate lies
*        within the bounds of a pixel and zero if it does not. The supplied
*        "inpix" array should have at least "n" elements.
*     ipix1
*        Returned holding the if "inpix" is non-zero, then "ipix1" returns
*        the index of the pixel which contains the axis coordinate. If the
*        axis coordinate lies inside more than one pixel, then this index
*        identifies which of these pixels has its centre position nearest
*        to the axis coordinate.  If "inpix" is zero, then "ipix1" returns
*        the index of the pixel whose centre coordinate is nearest to the
*        axis coordinate. The supplied "ipix1" array should have at least
*        "n" elements.
*     cent1
*        Returned holding the returns the value of the centre coordinate
*        for the pixel with index "ipix1". The supplied "cent1" array
*        should have at least "n" elements.
*     width1
*        Returned holding the returns a non-negative value for the width of
*        the pixel with index "ipix1". The supplied "width1" array should
*        have at least "n" elements.
*     *status
*        The global status.

*  Notes:
*     -  This function performs simple checks on the validity of the values
*     it returns.

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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   double cdiff;         /* Difference in pixel centre positions */
   double cen[ 2 ];      /* Pixel centre positions */
   double cnew;          /* Centre position of interpolated pixel */
   double dif[ 2 ];      /* Distance to neighbouring pixels */
   double dinc;          /* Direction of increasing centre values */
   double var[ 2 ];      /* Variance values (unused) */
   double varian;        /* Dummy variance array */
   double vnew;          /* Variance value of interpolated pixel */
   double wid[ 2 ];      /* Width of neighbouring pixels */
   double wnew;          /* Width value of interpolated pixel */
   int below;            /* Pixel lies "below" axis coordinate? */
   int below1;           /* First pixel "below" axis coordinate? */
   int below2;           /* Second pixel "below" axis coordinate? */
   int i;                /* Loop counter for axis coordinates */
   int in[ 2 ];          /* Coordinate inside neighbouring pixel? */
   int inew;             /* Interpolated pixel index */
   int interp;           /* Interpolation (not extrapolating)? */
   int irange[ 2 ];      /* Pixel index search range */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* No pixel centre positions are available.
   =======================================
   In this case, the pixels are taken to be uniformly spaced, of unit
   width and contiguous. Loop to calculate output values for each input
   axis coordinate. */
   if( !havcen ) {
      *inc = 1;
      for( i = 0; i < n; i++ ){

/* Find the index of the pixel whose centre value equals or lies
   immediately below the axis coordinate (ensure that half-integer
   coordinate values are rounded upwards regardless of sign). */
         if( ax[ i ] >= 0.0 ) {
            ipix0[ i ] = NDF_NINT( ax[ i ] );
         } else {
            ipix0[ i ] = (int)( ax[ i ] - 0.5 );
            if( (double)( ipix0[ i ] ) == ax[ i ] - 0.5 ) ipix0[ i ]++;
         }

/* Obtain the centre position for this pixel and the distance to the
   next pixel centre. */
         cent0[ i ] = (double)( ipix0[ i ] ) - 0.5;
         space0[ i ] = 1.0;

/* Find the index of the pixel containing the axis coordinate (ensuring
   that if the coordinate lies on a boundary, then it lies in the lower
   pixel, regardless of sign). */
         inpix[ i ] = 1;
         if( ax[ i ] >= 0.0 ) {
            ipix1[ i ] = (int)( ax[ i ] );
            if( (double)( ipix1[ i ] ) != ax[ i ] ) ipix1[ i ]++;
         } else {
            ipix1[ i ] = (int)( ax[ i ] );
         }

/* Obtain the centre position and width of this pixel. */
         cent1[ i ] = (double)( ipix1[ i ] ) - 0.5;
         width1[ i ] = 1.0;
      }

/* Pixel centre positions are available.
   ====================================
   In this case, check to see if the centre value changes from one end
   of the axis to the other. Report an error if it does not. */
   } else {
      if( ubnd != lbnd ) {
         if( centre[ ubnd - 1 ] == centre[ lbnd - 1 ] ) {
            *status = NDF__AXVIN;
            errRep( " ", "Axis CENTRE values do not increase or decrease "
                    "monotonically.", status );
            goto L99;

/* Determine whether axis centre positions increase or decrease with
   pixel index. If there is a choice, then assume they increase. */
         } else {
            *inc = ( centre[ ubnd - 1 ] > centre[ lbnd - 1 ] );
         }
      } else {
         *inc = 1;
      }

/* Set up an increment value to match the direction of increase in the
   pixel centre positions. */
      if( *inc ) {
         dinc = 1.0;
      } else {
         dinc = -1.0;
      }

/* Loop to process each input axis coordinate. */
      for( i = 0; i < n; i++ ){

/* Set initial lower and upper pixel index limits. These will be
   progressively adjusted until the axis coordinate is found to lie
   between the centre positions of two adjacent pixels. Ensure that
   these initial pixels are not adjacent. */
         irange[ 0 ] = lbnd;
         irange[ 1 ] = NDF_MAX( ubnd, lbnd + 2 );

/* Obtain the centre positions for the two limiting pixels. */
L2:                      /* Start of 'DO WHILE' loop */
         ndf1P2a( 2, irange, lbnd, ubnd, 1, havwid, 0, centre, width,
                  &varian, cen, wid, var, status );
         if( *status != SAI__OK ) goto L99;

/* Calculate the difference between the centre positions at the current
   pixel limits and check this difference has the correct sign. Report
   an error if it does not, as this indicates a non-monotonic variation
   of centre position with pixel index. */
         cdiff = cen[ 1 ] - cen[ 0 ];
         if( dinc*cdiff <= 0.0 ) {
            *status = NDF__AXVIN;
            errRep( " ", "Axis CENTRE values do not increase or decrease "
                    "monotonically.", status );
            goto L99;
         }

/* Loop until the search converges on two adjacent pixels. */
         if( irange[ 1 ] > irange[ 0 ] + 1 ) {

/* Determine if we are interpolating between the centre coordinates of
   the two limiting pixels (as opposed to extrapolating). We are
   interpolating only if the first pixel's coordinate lies "below" the
   required value, while the second one does not (interpolation must be
   achieved before convergence is assured). */
            below1 = dinc*( ax[ i ] - cen[ 0 ] ) >= 0.0;
            below2 = dinc*( ax[ i ] - cen[ 1 ] ) >= 0.0;
            interp = ( below1 && ( !below2 ) );

/* Interpolate (or extrapolate) linearly to estimate the pixel index
   where the centre position matches the axis coordinate. */
            inew = irange[ 0 ] + NDF_NINT( (double)( irange[ 1 ] - irange[ 0 ] )*( ( ax[ i ] - cen[ 0 ] )/cdiff ) );

/* If we are extrapolating (not interpolating), then the algorithm
   cannot converge on this iteration. Instead, we aim to reach a
   position where we can interpolate on the next iteration (and so
   eventually converge). Thus we aim to get one limiting pixel lying
   "below" the required axis coordinate, and the other lying "above". */
            if( !interp ) {

/* If the new pixel index is the same as one of the old pixel indices,
   then widen the search range by one pixel at the appropriate end
   (otherwise there will be no change, hence an infinite loop). Note
   that the pixels cannot become adjacent at this point, so the
   convergence criterion will not be met on this iteration. */
               if( inew == irange[ 0 ] ) {
                  inew--;
               } else if( inew == irange[ 1 ] ) {
                  inew++;
               }

/* Obtain the axis coordinate of the new pixel index and see if it lies
   "below" the required value. */
               ndf1P2a( 1, &inew, lbnd, ubnd, 1, havwid, 0, centre, width,
                        &varian, &cnew, &wnew, &vnew, status );
               if( *status != SAI__OK ) goto L99;
               below = dinc*( ax[ i ] - cnew ) >= 0.0;

/* If so, and the new pixel lies below the initial search range, then
   simply extend the lower pixel of the range. Otherwise move the upper
   bound to this pixel and derive a new lower bound. Again note that
   the limiting pixel indices cannot become adjacent. */
               if( inew < irange[ 0 ] ) {
                  if( below ) {
                     irange[ 0 ] = inew;
                  } else {
                     irange[ 1 ] = inew;
                     irange[ 0 ] = inew - 2;
                  }

/* Modify the range similarly if the new pixel lies above the initial
   search range. */
               } else if( inew > irange[ 1 ] ) {
                  if( below ) {
                     irange[ 0 ] = inew;
                     irange[ 1 ] = inew + 2;
                  } else {
                     irange[ 1 ] = inew;
                  }
               }

/* If we are interpolating, then convergence is now possible. To ensure
   this, we ensure that the search range is narrowed by at least one
   pixel on each iteration (otherwise there will be no change, hence an
   infinite loop). */
            } else {
               if( inew == irange[ 0 ] ) {
                  inew++;
               } else if( inew == irange[ 1 ] ) {
                  inew--;
               }

/* Obtain the new pixel centre location and see if it lies "below" the
   required axis value. */
               ndf1P2a( 1, &inew, lbnd, ubnd, 1, havwid, 0, centre, width,
                        &varian, &cnew, &wnew, &vnew, status );
               if( *status != SAI__OK ) goto L99;
               below = dinc*( ax[ i ] - cnew ) >= 0.0;

/* Update the lower or upper pixel index limit according to the outcome
   of the interpolation. */
               if( below ) {
                  irange[ 0 ] = inew;
               } else {
                  irange[ 1 ] = inew;
               }

/* Return to perform the next iteration. */
            }
            goto L2;
         }

/* Return the index of the pixel lying immediately "below" the axis
   coordinate, the centre position of this pixel, and the distance to
   the next pixel. */
         ipix0[ i ] = irange[ 0 ];
         cent0[ i ] = cen[ 0 ];
         space0[ i ] = cen[ 1 ] - cen[ 0 ];

/* Find the distance between the axis coordinate and the centre
   positions of the two neighbouring pixels. */
         dif[ 0 ] = fabs( ax[ i ] - cen[ 0 ] );
         dif[ 1 ] = fabs( ax[ i ] - cen[ 1 ] );

/* Determine if the axis coordinate lies inside each of these pixels. */
         in[ 0 ] = ( dif[ 0 ] <= ( 0.5*wid[ 0 ] ) );
         in[ 1 ] = ( dif[ 1 ] <= ( 0.5*wid[ 1 ] ) );

/* If it lies nearer the lower pixel, but does not lie inside it and
   instead lies inside the upper pixel, then note this. */
         if( dif[ 0 ] <= dif[ 1 ] ) {
            if( ( !in[ 0 ] ) && in[ 1 ] ) {
               inpix[ i ] = 1;
               ipix1[ i ] = irange[ 1 ];
               width1[ i ] = wid[ 1 ];
               cent1[ i ] = cen[ 1 ];

/* Otherwise, return values appropriate to the nearer (lower) pixel. */
            } else {
               inpix[ i ] = in[ 0 ];
               ipix1[ i ] = irange[ 0 ];
               width1[ i ] = wid[ 0 ];
               cent1[ i ] = cen[ 0 ];
            }

/* If it lies nearer the upper pixel, but does not lie inside it and
   instead lies inside the lower pixel, then note this. */
         } else {
            if( ( !in[ 1 ] ) && in[ 0 ] ) {
               inpix[ i ] = 1;
               ipix1[ i ] = irange[ 0 ];
               width1[ i ] = wid[ 0 ];
               cent1[ i ] = cen[ 0 ];

/* Otherwise, return values appropriate to the nearer (upper) pixel. */
            } else {
               inpix[ i ] = in[ 1 ];
               ipix1[ i ] = irange[ 1 ];
               width1[ i ] = wid[ 1 ];
               cent1[ i ] = cen[ 1 ];
            }
         }
      }
   }

/* Arrive here if an error occurs. */
L99:

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1A2P", status );

}

