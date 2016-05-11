#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "kaplibs.h"
#include <math.h>

void kpg1CrMapD( int nx, int ny, const double x[], const double y[],
                 int box, double *r, int *status ){
/*
*  Name:
*     kpg1CrMapD

*  Purpose:
*     Get an image of the local correlation coefficient between two images.

*  Language:
*     C.

*  Invocation:
*     void kpg1CrMapD( int nx, int ny, const double x[], const double y[],
*                      int box, double *r, int *status )

*  Description:
*     This function returns an array in which each pixel value is the
*     correlation coefficient between corresponding boxes of values in
*     two supplied arrays, centred on the output pixel.

*  Arguments:
*     nx
*        The number of pixels along each row of the suppied arrays.
*     ny
*        The number of pixels along each column of the suppied arrays.
*     x
*        The first image.
*     y
*        The second image.
*     box
*        The linear size of the smoothing box, in pixels.
*     r
*        Returned holding the map of correlation coefficients.
*     status
*        The inherited status.

*  Notes:
*     - The three arays "x", "y" and "r" must all be the same size.
*     - The current algorithm could be made much more efficient. It
*      currently re-evaluates the statistics from scratch for each
*      box of input pixels for each output pixel.

*  Copyright:
*     Copyright (C) 2016 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     11-MAY-2016 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/

/* Local Variables: */
   double *pr;
   const double *px;
   const double *py;
   double den;
   double sx;
   double sx2;
   double sxy;
   double sy;
   double sy2;
   double xmean;
   double ymean;
   int hbox;
   int n;
   size_t ix;
   size_t iy;
   size_t jx;
   size_t jy;
   size_t step;
   size_t xhi;
   size_t xlo;
   size_t xmax;
   size_t yhi;
   size_t ylo;
   size_t ymax;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Some required constants. */
   hbox = box/2;
   box = 2*hbox + 1;
   xmax = nx - 1;
   ymax = ny - 1;

/* Loop round each row in the output map. */
   pr = r;
   for( iy = 0; iy < ny; iy++ ) {

/* Loop round each column within the current row of the output map. */
      for( ix = 0; ix < nx; ix++,pr++ ) {

/* Find the required data sums for the box of (x,y) values centred on the
   current output pixel. */
         xlo = ix - hbox;
         if( xlo < 0 ) xlo = 0;
         xhi = ix + hbox;
         if( xhi > xmax ) xhi = xmax;
         step = nx - ( xhi - xlo + 1);

         ylo = iy - hbox;
         if( ylo < 0 ) ylo = 0;
         yhi = iy + hbox;
         if( yhi > ymax ) yhi = ymax;

         sx = 0.0;
         sy = 0.0;
         sx2 = 0.0;
         sy2 = 0.0;
         sxy = 0.0;
         n = 0;

         px = x + ylo*nx + xlo;
         py = y + ylo*nx + xlo;

         for( jy = ylo; jy <= yhi; jy++ ) {
            for( jx = xlo; jx <= xhi; jx++,px++,py++ ) {
               if( *px != VAL__BADD && *py != VAL__BADD ) {
                  sx += *px;
                  sy += *py;
                  sx2 += (*px)*(*px);
                  sy2 += (*py)*(*py);
                  sxy += (*px)*(*py);
                  n++;
               }
            }
            px += step;
            py += step;
         }

/* Calculate the correlation coefficient. */
         if( n > 0 ) {
            xmean = sx/n;
            ymean = sy/n;
            den = ( sx2 - n*xmean*xmean )*( sy2 - n*ymean*ymean );
            if( den > 0.0 ) {
               *pr = ( sxy - n*xmean*ymean )/sqrt( den );
            } else {
               *pr = VAL__BADD;
            }
         } else {
            *pr = VAL__BADD;
         }
      }
   }
}
