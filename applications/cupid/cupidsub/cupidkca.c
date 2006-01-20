#include "sae_par.h"
#include "ast.h"
#include "cupid.h"

int *cupidKCA( int *in, int *out, int nel, int dims[ 3 ], int skip[ 3 ],
               int thresh, int peakval ){
/*
*  Name:
*     cupidKCA

*  Purpose:
*     Erode or dilate the edge pixels using a cellular automata.

*  Synopsis:
*     int *cupidKCA( int *in, int *out, int nel, int dims[ 3 ], 
*                    int skip[ 3 ], int thresh, int peakval )

*  Description:
*     This function contracts (erodes) or expands (dilates0 the edge pixels 
*     marked in the supplied input mask array using a cellular automata, 
*     and returns the result in an output array of the same shape and size
*     as the input array.
*
*     Each output pixel value is created in turn as follows: a 3x3x3 cube 
*     (or a 3x3 square for 2D arrays) is defined within the input array which 
*     is centred on the position of the current output pixel. The number of 
*     pixels within this input cube which are flagged as edge pixels is then 
*     counted. If this number is larger than or equal to "thresh" then the 
*     output pixel value is set to CUPID__KEDGE, otherwise it is set to 
*     CUPID__KBACK.
*
*     Note that any input pixel values which are larger than or equal to 
*     "peakval" are copied to the output unchanged, no matter how many
*     neighbouring edge pixels they have.

*  Parameters:
*     in
*        The input mask array.
*     out
*        The output mask array. If this is NULL a new array will be
*        dynamically allocated.
*     nel
*        The number of elements in the "dval" and "dpos" arrays.
*     dims
*        The number of pixels along each pixel axis of the arrays.
*     skip
*        The increment in 1D vector index required to move a distance of 1 
*        pixel along each axis. This allows conversion between indexing
*        the array using a single 1D vector index and using nD coords. 
*     thresh
*        The minimum number of pixels required for an output edge pixel.
*     peakval
*        The minimum value used to flag peak pixels within "in" and "out".

*  Returned Value:
*     A pointer to the output array. This will be equal to "out" if "out"
*     is not NULL. If "out" is NULL, it will point to a newly allocated
*     area of memory which should be freed using astFree when no longer needed.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     19-JAN-2006 (DSB):
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
   int iv;             /* Vector index into input array */
   int ix;             /* Input pixel GRID index on axis 1 */
   int iy;             /* Input pixel GRID index on axis 2 */
   int iz;             /* Input pixel GRID index on axis 3 */
   int ox;             /* Output pixel GRID index on axis 1 */
   int oy;             /* Output pixel GRID index on axis 2 */
   int oz;             /* Output pixel GRID index on axis 3 */
   int sum;            /* No. of edge neighbours */

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

/* If the corresponding input pixel is a peak, so is the output pixel. */
               if( in[ iv ] >= peakval ){
                  *(pout++) = peakval;

/* Otherwise, loop round all input pixels in the neighbourhood of the current 
   output pixel, this is a cube of 3x3x3 input pixels, centred on the current
   output pixel. Count how many of these input pixels are set to 1. */
               } else {
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
                                    if( *pin == CUPID__KEDGE ) sum++;
                                 }
                                 pin++;
                              }
                           }
                           piny = piny + skip[ 1 ];
                        }
                     }
                     pinz = pinz + skip[ 2 ];
                  }
   
/* If the number of neighbouring edge pixel is "thresh" or more, set the
   output pixel as an edge pixel. Move on to the next output pixel. */
                  *(pout++) = ( sum >= thresh ) ? CUPID__KEDGE : CUPID__KBACK;
               }
            }
         }
      }
   }

/* Return the pointer to the output array. */
   return ret;
}

