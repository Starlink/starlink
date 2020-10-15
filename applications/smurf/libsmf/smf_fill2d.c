/*
*+
*  Name:
*     smf_fill2d

*  Purpose:
*     Fill holes in a 2darray.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void smf_fill2d( int mingood, int box, double fillval, dim_t nx,
*                      dim_t ny, double *data, double *work, int *status )

*  Arguments:
*     mingood = int (Given)
*        If the input array contains fewer than "mingood" good values
*        (i.e. values that are neither VAL__BADD nor "fillval"), then the
*        output array will be filled entirely with VAL__BADD.
*     box = int (Given)
*        The size of the filter box, in pixels.
*     fillval = double (Given)
*        The pixel value within the input array that is to be replaced.
*        May be VAL__BADD.
*     nx = dim_t (Given)
*        The number of pixels per row in the array.
*     ny = dim_t (Given)
*        The number of rows in the array.
*     data = double * (Given and Returned)
*        The array to fill. Any pixels that are equal to "fillval" on
*        entry, are replaced on exit by a value typical of the surrounding
*        good pixel values. If "fillval" is not VAL__BADD, then any
*        VAL__BADD values in the input are propagated unchanged to the
*        output.
*     work = double * (Given and Returned)
*        A work array with 2*nx*ny elements.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Any pixels that have value "fillbad" in the data array on entry
*     are replaced by values typical of the surrounding data on exit.
*     If "fillval" is not equal to VAL__BADD, any VAL__BADD values are
*     propgated without change to the output array.
*
*     The filling is done iteratively. On each iteration a new array is
*     created which is a copy of the previous array, except that each
*     fillable pixel value is replaced by the mean of the good values in
*     a box of size "box". If there are no good values in the box, then
*     the pixel is left unchanged until a subsequent iteration fills it.
*     On each iteration, each hole in the array shrinks in size by about
*     "box". Iterations continue until there are no holes left.

*  Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1-APR-2014 (DSB):
*        Initial version.

*  Copyright:
*     Copyright (C) 2014 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

void smf_fill2d( int mingood, int box, double fillval, dim_t nx,
                 dim_t ny, double *data, double *work, int *status ){

/* Local Variables. */
   dim_t el;
   dim_t hbox;
   dim_t iel;
   dim_t ix;
   dim_t iy;
   dim_t jx;
   dim_t jxhi;
   dim_t jxlo;
   dim_t jy;
   dim_t jyhi;
   dim_t jylo;
   dim_t mx;
   dim_t ngood;
   dim_t nsum;
   double *p0;
   double *pin;
   double *pout;
   double sum;
   int iter;
   int more;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* The number of pixels in the map. */
   el = nx*ny;

/* Get the half-box size. */
   hbox = box/2;

/* Copy the supplied image into the first half of the supplied work
   array. */
   memcpy( work, data, el*sizeof( *data ) );

/* Loop until all "fillval" pixel values have been replaced. */
   iter = 0;
   more = 1;
   while( more ) {
      iter++;

/* Initially, assume that no more iterations will be required. */
      more = 0;

/* Choose the input and output arrays for this iteration. The output form
   one iteration becomes the input for the next. */
      if( iter % 2 ) {
         pin = work;
         pout = work + el;
      } else {
         pin = work + el;
         pout = work;
      }

/* Loop round all pixels, maintaining the 2D zero-based grid indices of the
   current pixel. */
      ngood = 0;
      ix = 0;
      iy = 0;
      for( iel = 0; iel < el; iel++,pin++,pout++ ) {

/* If the current input pixel value is good, just copy it from input to
   output. Increment the number of such pixels. */
         if( *pin != fillval && *pin != VAL__BADD ) {
            *pout = *pin;
            ngood++;

/* Otherwise, find the mean usable value within a box of half-size hbox
   pixels, centred on the current input value, and copy it to the output. */
         } else {

/* Get the bounds of the box, and truncate it at the edges of the map. */
            jxlo = ix - hbox;
            jxhi = ix + hbox;
            jylo = iy - hbox;
            jyhi = iy + hbox;

            if( jxlo < 0 ) jxlo = 0;
            if( jxhi >= nx ) jxhi = nx - 1;
            if( jylo < 0 ) jylo = 0;
            if( jyhi >= ny ) jyhi = ny - 1;

/* Convert the bounds to be relative to the central pixel. */
            jxlo -= ix;
            jxhi -= ix;
            jylo -= iy;
            jyhi -= iy;

/* Get the width of the truncated box. */
            mx = jxhi - jxlo + 1;

/* Get a pointer to the first pixel in the truncated box. */
            p0 = pin + jylo*nx + jxlo;

/* Initalise the sum of usable values in the box. */
            sum = 0.0;
            nsum = 0;

/* Loop round all pixels in the truncated box. */
            for( jy = jylo; jy <= jyhi; jy++ ) {
               for( jx = jxlo; jx <= jxhi; jx++ ) {

/* If the input pixel value is good and will not be replaced, include it
   in the sums. */
                  if( *p0 != VAL__BADD && *p0 != fillval ) {
                     sum += *p0;
                     nsum++;
                  }

/* Point to the next pixel in the current row. */
                  p0++;
               }

/* Point to the first pixel in the box on the next row. */
               p0 += nx - mx;
            }

/* If the box contains some good values, store the mean value in the
   current output array. */
            if( nsum > 0 ) {
               *pout = sum/nsum;

/* If the box contains no good values, store "fillval" in the current
   output array so that the pixel will be filled on the next iteration.
   Also, indicate that another iteration is required. */
            } else {
               *pout = fillval;
               more = 1;
            }
         }

/* Update the 2D grid indices of the next pixel. */
         if( ++ix == nx ) {
            ix = 0;
            iy++;
         }
      }

/* If there are fewer than mingood good values, fill the returned array
   with bad values and return immediately. */
      if( ngood < mingood ) {
         more = 0;
         p0 = data;
         for( iel = 0; iel < el; iel++ ) {
            *(p0++) = VAL__BADD;
         }
      }
   }

/* Copy the filled values to the returned array. This leaves VAL__BADD
   values in place in the supplied data (only "fillval" pixels are
   replaced by this function). */
   if( ngood >= mingood ) {
      pin = ( iter % 2 ) ? work + el : work;
      pout = data;
      for( iel = 0; iel < el; iel++,pin++,pout++ ) {
         if( *pout == fillval ) *pout = *pin;
      }
   }
}


