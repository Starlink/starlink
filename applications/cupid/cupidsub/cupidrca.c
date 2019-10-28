#include "sae_par.h"
#include "ast.h"
#include "cupid.h"

int *cupidRCA( int *in, int *out, size_t nel, hdsdim dims[ 3 ], size_t skip[ 3 ],
               double thresh, int magic, int on, int off, int centre,
               int *status ){
/*
*+
*  Name:
*     cupidRCA

*  Purpose:
*     Erode or dilate areas within an array using a cellular automata.

*  Language:
*     Starlink C

*  Synopsis:
*     int *cupidRCA( int *in, int *out, size_t nel, hdsdim dims[ 3 ], size_t skip[ 3 ],
*                    double thresh, int magic, int on, int off, int centre,
*                    int *status )

*  Description:
*     This function contracts (erodes) or expands (dilates) the pixels
*     which are marked as "on" in the supplied input array using a cellular
*     automata, and returns the result in an output array of the same shape
*     and size as the input array.
*
*     Each output pixel value is created in turn as follows: If the
*     corresponding input value has value equal to or greater than "magic",
*     then the output pixel is set equal to "magic". Otherwise, a
*     3x3x3 cube (or a 3x3 square for 2D arrays) is defined within the input
*     array which is centred on the position of the current output pixel. The
*     fraction of pixels within this input cube which are flagged as "on" is
*     then counted. If this fraction is larger than "thresh" then the output
*     pixel value is set to "on", otherwise it is set to "off". Optionally,
*     an additional requirement for an output pixel to be set on is that
*     the corresponding input pixel must be on.

*  Parameters:
*     in
*        The input mask array.
*     out
*        The output mask array. If this is NULL a new array will be
*        dynamically allocated.
*     nel
*        The number of elements in the "in" array.
*     dims
*        The number of pixels along each pixel axis of the arrays.
*     skip
*        The increment in 1D vector index required to move a distance of 1
*        pixel along each axis. This allows conversion between indexing
*        the array using a single 1D vector index and using nD coords.
*     thresh
*        The maximum fraction of input edge pixels which does not produce an
*        output "on" pixel.
*     magic
*        The minimum value which should be copied unchanged from "in" to "out".
*     on
*        The value used to represent "on" pixels in input and output arrays.
*     off
*        The value used to represent "off" pixels in the output array (any
*        value not equal to "on" is treated as off in the input array).
*     centre
*        If non-zero, then no output pixel will be set on if the
*        corresponding input pixel is not on.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     A pointer to the output array. This will be equal to "out" if "out"
*     is not NULL. If "out" is NULL, it will point to a newly allocated
*     area of memory which should be freed using astFree when no longer needed.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     19-JAN-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   hdsdim ix;          /* Input pixel GRID index on axis 1 */
   hdsdim iy;          /* Input pixel GRID index on axis 2 */
   hdsdim iz;          /* Input pixel GRID index on axis 3 */
   hdsdim ox;          /* Output pixel GRID index on axis 1 */
   hdsdim oy;          /* Output pixel GRID index on axis 2 */
   hdsdim oz;          /* Output pixel GRID index on axis 3 */
   int *pin0;          /* Pointer to input pixel [0,0,0] */
   int *pin;           /* Pointer to input pixel */
   int *piny;          /* Pointer to input pixel at start of row */
   int *pinz;          /* Pointer to input pixel at start of plane */
   int *pout;          /* Pointer to output pixel */
   int *ret;           /* Pointer to the returned array */
   int sum;            /* No. of edge neighbours */
   int tot;            /* Total no. of neighbours */
   size_t iv;          /* Vector index into input array */

/* Initialise */
   ret = out;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* If no output array was supplied, allocate one now. */
   if( !out ) ret = astMalloc( sizeof( *ret )*nel );

/* Check the memory was allocated. */
   if( ret ) {

/* Get a pointer to the input pixel which would have GRID indices [0,0,0]
   if the input array extended that far (in fact the first pixel in the
   input array has GRID indices [1,1,1]). */
      pin0 = in - skip[ 0 ] - skip[ 1 ] - skip[ 2 ];

/* Store a pointer to the first output pixel. */
      pout = ret;

/* Loop round all elements of the output array. */
      iv = 0;
      for( oz = 1; oz <= dims[ 2 ]; oz++ ) {
         for( oy = 1; oy <= dims[ 1 ]; oy++ ) {
            for( ox = 1; ox <= dims[ 0 ]; ox++, iv++ ) {

/* If the corresponding input pixel is equal to or greater than the magic
   value, copy it to the output. */
               if( in[ iv ] >= magic ){
                  *(pout++) = magic;

/* If the corresponding input pixel is off, then the output must also be
   off if "centre" is true. */
               } else if( centre && in[ iv ] != on ){
                  *(pout++) = off;

/* Otherwise, loop round all input pixels in the neighbourhood of the current
   output pixel, this is a cube of 3x3x3 input pixels, centred on the current
   output pixel. Count how many of these input pixels are set to "on". If
   the current output pixel is close to an edge of the array, there will be
   fewer than 3x3x3 pixels in the cube. Count the total number of pixels
   in the cube. */
               } else {
                  tot = 0;
                  sum = 0;
                  pinz = pin0 + iv;
                  for( iz = oz - 1; iz <= oz + 1; iz++ ) {
                     if( iz >= 1 && iz <= dims[ 2 ] ) {
                        piny = pinz;
                        for( iy = oy - 1; iy <= oy + 1; iy++ ) {
                           if( iy >= 1 && iy <= dims[ 1 ] ) {
                              pin = piny;
                              for( ix = ox - 1; ix <= ox + 1; ix++ ) {
                                 if( ix >= 1 && ix <= dims[ 0 ] ) {
                                    tot++;
                                    if( *pin == on ) sum++;
                                 }
                                 pin++;
                              }
                           }
                           piny = piny + skip[ 1 ];
                        }
                     }
                     pinz = pinz + skip[ 2 ];
                  }

/* If the fraction of neighbouring on pixels is more than "thresh", set the
   output pixel on. Otherwise set it off. Move on to the next output pixel. */
                  *(pout++) = ( ( (float) sum )/( (float) tot ) > thresh ) ? on : off;
               }
            }
         }
      }
   }

/* Return the pointer to the output array. */
   return ret;
}
