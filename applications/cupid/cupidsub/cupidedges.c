#include "sae_par.h"
#include "prm_par.h"
#include "cupid.h"
void cupidEdges( float *mask, size_t nel, hdsdim dims[ 3 ], hdsdim skip[ 3 ],
                 float on, float off, int *status ){
/*
*+
*  Name:
*     cupidEdges
*  Purpose:
*     Find the edges of areas within a constant integer value.
*  Language:
*     Starlink C
*  Synopsis:
*     void cupidEdges( float *mask, size_t nel, hdsdim dims[ 3 ],
*                      hdsdim skip[ 3 ], float on, float off, int *status )
*  Description:
*     This function produces an output array which marks the edges of the
*     areas within the input array that hold a specified "on" value.
*
*     Each output pixel value is created in turn as follows: If the
*     corresponding input pixel has the "on" value and is also
*     completely surrounded by pixel with the "on" value, then the
*     output pixel is set to the "off" value. If the input pixel has
*     the "on" value but has at least one neighbour that is not set to
*     the "on" value, then the output pixel is set to the "on" value.
*     If the input pixel is not set to the "on" value then the output is
*     set to the "off" value.
*  Parameters:
*     mask
*        The mask array.
*     nel
*        The number of elements in the "in" array.
*     dims
*        The number of pixels along each pixel axis of the arrays.
*     skip
*        The increment in 1D vector index required to move a distance of 1
*        pixel along each axis. This allows conversion between indexing
*        the array using a single 1D vector index and using nD coords.
*     on
*        The on value.
*     off
*        The off value.
*     status
*        Pointer to the inherited status value.
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
*     13-JUN-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}
*  Bugs:
*     {note_any_bugs_here}
*-
*/
/* Local Variables: */
   float *pin0;        /* Pointer to input pixel [0,0,0] */
   float *pin;         /* Pointer to input pixel */
   float *piny;        /* Pointer to input pixel at start of row */
   float *pinz;        /* Pointer to input pixel at start of plane */
   float toggle;       /* Flag for pixels to be set off */
   hdsdim ix;          /* Input pixel GRID index on axis 1 */
   hdsdim iy;          /* Input pixel GRID index on axis 2 */
   hdsdim iz;          /* Input pixel GRID index on axis 3 */
   hdsdim ox;          /* Output pixel GRID index on axis 1 */
   hdsdim oy;          /* Output pixel GRID index on axis 2 */
   hdsdim oz;          /* Output pixel GRID index on axis 3 */
   int setoff;         /* Set pixel off? */
   size_t iv;          /* Vector index into input array */
/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;
/* Get a pointer to the input pixel which would have GRID indices [0,0,0]
   if the array extended that far (in fact the first pixel in the
   input array has GRID indices [1,1,1]). */
   pin0 = mask - skip[ 0 ] - skip[ 1 ] - skip[ 2 ];
/* Store a value used to indicate that a pixel was original on but is to
   be switched off. */
   toggle = 0.5*VAL__MAXR;
/* Loop round all elements of the mask. */
   iv = 0;
   for( oz = 1; oz <= dims[ 2 ]; oz++ ) {
      for( oy = 1; oy <= dims[ 1 ]; oy++ ) {
         for( ox = 1; ox <= dims[ 0 ]; ox++, iv++ ) {
/* If the input pixel is not on, set it off. */
            if( mask[ iv ] != on ){
               mask[ iv ] = off;
/* Otherwise, we need to determine if any of the neighbours of the pixel
   are off. Loop round all pixels in the neighbourhood of the current pixel.
   This is a cube of 3x3x3 pixels, centred on the current pixel. Break
   when a pixel is found that is not set to "on" or "toggle" (used to
   represent pixels that were originally on but which are to be turned
   off). Set a flag to indicate if the pixel should be set off. */
            } else {
               setoff = 1;
               pinz = pin0 + iv;
               for( iz = oz - 1; iz <= oz + 1; iz++ ) {
                  if( iz >= 1 && iz <= dims[ 2 ] ) {
                     piny = pinz;
                     for( iy = oy - 1; iy <= oy + 1; iy++ ) {
                        if( iy >= 1 && iy <= dims[ 1 ] ) {
                           pin = piny;
                           for( ix = ox - 1; ix <= ox + 1; ix++ ) {
                              if( ix >= 1 && ix <= dims[ 0 ] ) {
                                 if( *pin != on && *pin != toggle ) {
                                    setoff = 0;
                                    goto L10;
                                 }
                              }
                              pin++;
                           }
                        }
                        piny = piny + skip[ 1 ];
                     }
                  }
                  pinz = pinz + skip[ 2 ];
               }
/* If required, set the pixel to "toggle". */
L10:;
               if( setoff ) mask[ iv ] = toggle;
            }
         }
      }
   }
/* Replace all "toggle" values in the array with "off". */
   pin0 = mask;
   for( iv = 0; iv < nel; iv++, pin0++ ) {
      if( *pin0 == toggle ) *pin0 = off;
   }
}
