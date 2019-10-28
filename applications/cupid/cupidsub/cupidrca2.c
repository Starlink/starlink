#include "sae_par.h"
#include "ast.h"
#include "cupid.h"

int *cupidRCA2( int *in, int *out, size_t nel, hdsdim dims[ 3 ], size_t skip[ 3 ],
                int *status ){
/*
*+
*  Name:
*     cupidRCA2

*  Purpose:
*     Erode or dilate areas within an array using a cellular automata.

*  Language:
*     Starlink C

*  Synopsis:
*     int *cupidRCA2( int *in, int *out, size_t nel, hdsdim dims[ 3 ],
*                     size_t skip[ 3 ], int *status )

*  Description:
*     This function smoothes the boundaries between areas of constant value
*     in an array of integer pixel values.
*
*     Each output pixel value is created in turn as follows: The input
*     values in a 3x3x3 cube of pixels centred on the output pixel are
*     examined, and the output pixel is assigned the most commonly
*     occurring input value.

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
*     2-FEB-2006 (DSB):
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
   int ip;             /* The index of the next party */
   int maxvotes;       /* Vote for currently winning party */
   int np;             /* The number of parties available */
   int nvotes;         /* No. of votes remaining to be counted */
   int party[ 27 ];    /* The pixel value associated with each party */
   int target;         /* No. of votes that guarantees a party wins */
   int votes[ 27 ];    /* The number of votes for each party */
   int winner;         /* Index of winning party */
   size_t iv;          /* Vector index into input array */

/* Initialise */
   ret = out;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* If no output array was supplied, allocate one now. */
   if( !out ) ret = astMalloc( sizeof( *ret )*nel );

/* Check the memory was allocated. */
   if( ret ) {

/* Store the number of votes that will guarantee that a party wins. */
   target = 2;
   if( dims[ 1 ] > 1 ) target = 5;
   if( dims[ 2 ] > 1 ) target = 14;

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

/* Loop round all input pixels in the neighbourhood of the current output
   pixel, this is a cube of 3x3x3 input pixels, centred on the current
   output pixel. */
               nvotes = 0;
               np = 0;
               pinz = pin0 + iv;
               for( iz = oz - 1; iz <= oz + 1; iz++ ) {
                  if( iz >= 1 && iz <= dims[ 2 ] ) {
                     piny = pinz;
                     for( iy = oy - 1; iy <= oy + 1; iy++ ) {
                        if( iy >= 1 && iy <= dims[ 1 ] ) {
                           pin = piny;
                           for( ix = ox - 1; ix <= ox + 1; ix++ ) {
                              if( ix >= 1 && ix <= dims[ 0 ] ) {

/* Each pixel in the 3x3x3 cube will have an integer value. Each distinct
   integer value is associated with a "party",and the number of occurrences
   of the distinct value within the 3x3x3 cube equals the number of "votes"
   for the party. See if the current pixel belongs to a previously found
   party. If so, increment the number of votes for the party. If the number
   of votes cast for any party exceeds half the maximum possible number of
   votes, then it is not possible for another party to win. */
                                 for( ip = 0; ip < np; ip++ ) {
                                    if( party[ ip ] == *pin ) {
                                       if( ++votes[ ip ] >= target ) {
                                          winner = ip;
                                          goto L20;
                                       }
                                       break;
                                    }
                                 }

/* If not, initialise a new party, giving it a single vote. */
                                 if( ip == np ) {
                                    party[ ip ] = *pin;
                                    votes[ ip ] = 1;
                                    np++;
                                 }

/* Increment the total number of votes cast. */
                                 nvotes++;

                              }
                              pin++;
                           }
                        }
                        piny = piny + skip[ 1 ];
                     }
                  }
                  pinz = pinz + skip[ 2 ];
               }

/* We have now considered all the pixels in the 3x3x3 cube. See which
   party got the most votes. */
               maxvotes = 0;
               winner = 0;
               for( ip = 0; ip < np; ip++ ) {

/* See how many votes remain to be counted after the current party has
   been counted. */
                  nvotes -= votes[ ip ];

/* If this party has more votes than any previous party, elect it as the
   new leading party, and note how many votes it got. */
                  if( votes[ ip ] > maxvotes ) {
                     winner = ip;
                     maxvotes = votes[ ip ];

/* If the number of votes remaining to be counted is less than the votes
   cast for this party, then there is no way any other party can win. */
                     if( nvotes < maxvotes ) break;
                  }
               }

/* Jump to here if we find a winner early. */
L20:;
               *(pout++) = party[ winner ];
            }
         }
      }
   }

/* Return the pointer to the output array. */
   return ret;
}
