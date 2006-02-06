#include "sae_par.h"
#include "ast.h"
#include "cupid.h"

int *cupidRCA2( int *in, int *out, int nel, int dims[ 3 ], int skip[ 3 ] ){
/*
*  Name:
*     cupidRCA2

*  Purpose:
*     Erode or dilate areas within an array using a cellular automata.

*  Synopsis:
*     int *cupidRCA2( int *in, int *out, int nel, int dims[ 3 ], 
*                     int skip[ 3 ] )

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

*  Returned Value:
*     A pointer to the output array. This will be equal to "out" if "out"
*     is not NULL. If "out" is NULL, it will point to a newly allocated
*     area of memory which should be freed using astFree when no longer needed.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     2-FEB-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   int *pin0;          /* Pointer to input pixel [0,0,0] */
   int *pin;           /* Pointer to input pixel */
   int *piny;          /* Pointer to input pixel at start of row */
   int *pinz;          /* Pointer to input pixel at start of plane */
   int *pout;          /* Pointer to output pixel */
   int *ret;           /* Pointer to the returned array */
   int ip;             /* The index of the next party */
   int iv;             /* Vector index into input array */
   int ix;             /* Input pixel GRID index on axis 1 */
   int iy;             /* Input pixel GRID index on axis 2 */
   int iz;             /* Input pixel GRID index on axis 3 */
   int maxvotes;       /* Vote for currently winning party */
   int np;             /* The number of parties available */
   int ox;             /* Output pixel GRID index on axis 1 */
   int oy;             /* Output pixel GRID index on axis 2 */
   int oz;             /* Output pixel GRID index on axis 3 */
   int party[ 9 ];     /* The pixel value associated with each party */
   int votes[ 9 ];     /* The number of votes for each party */
   int winner;         /* Index of winning party */

/* Initialise */
   ret = out;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* If no output array was supplied, allocate one now. */
   if( !out ) ret = astMalloc( sizeof( int )*nel );

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

/* Loop round all input pixels in the neighbourhood of the current output 
   pixel, this is a cube of 3x3x3 input pixels, centred on the current
   output pixel. Count how many of these input pixels are set to "on". If
   the current output pixel is close to an edge of the array, there will be
   fewer than 3x3x3 pixels in the cube. Count the total number of pixels
   in the cube. */
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

                                 for( ip = 0; ip < np; ip++ ) {
                                    if( party[ ip ] == *pin ) {
                                       votes[ ip ]++;
                                       break;
                                    }
                                 }

                                 if( ip == np ) {
                                    party[ ip ] = *pin;
                                    votes[ ip ] = 1;
                                    np++;
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
   
               maxvotes = 0;
               for( ip = 0; ip < np; ip++ ) {
                  if( votes[ ip ] > maxvotes ) {
                     maxvotes = votes[ ip ];
                     winner = ip;
                  }
               }

               *(pout++) = party[ winner ];
            }
         }
      }
   }

/* Return the pointer to the output array. */
   return ret;
}

