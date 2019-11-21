#include "sae_par.h"
#include "ast.h"
#include "cupid.h"
#include "star/thr.h"

/* Local data types */
typedef struct CupidRCAData {
   double thresh;
   hdsdim *dims;
   int *in;
   int *ret;
   int centre;
   int magic;
   int off;
   int on;
   size_t *skip;
   size_t p1;
   size_t p2;
} CupidRCAData;

/* Prototypes for local functions */
static void cupidRCAPar( void *job_data_ptr, int *status );

int *cupidRCA( ThrWorkForce *wf, int *in, int *out, size_t nel, hdsdim dims[ 3 ],
               size_t skip[ 3 ], double thresh, int magic, int on, int off, int centre,
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
*     int *cupidRCA( ThrWorkForce *wf, int *in, int *out, size_t nel, hdsdim dims[ 3 ],
*                    size_t skip[ 3 ], double thresh, int magic, int on, int off,
*                    int centre, int *status )

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
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
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
*     Copyright (C) 2019 East Asian Observatory
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
*     31-OCT-2019 (DSB):
*        Multi-threaded.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   CupidRCAData *pdata;
   CupidRCAData *job_data = NULL;
   int *ret;           /* Pointer to the returned array */
   int iw;
   int nw;
   size_t step;

/* Initialise */
   ret = out;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* If no output array was supplied, allocate one now. */
   if( !out ) ret = astMalloc( sizeof( *ret )*nel );

/* How many threads do we get to play with */
   nw = wf ? wf->nworker : 1;

/* Allocate job data for threads. */
   job_data = astCalloc( nw, sizeof(*job_data) );

/* Check the memory was allocated. */
   if( job_data && ret ) {

/* Find how many output pixels to process in each worker thread. */
      step = nel/nw;
      if( step == 0 ) step = 1;

/* Store the range of output pixels to be processed by each one. Ensure that
   the last thread picks up any left-over output pixels. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->p1 = iw*step;
         if( iw < nw - 1 ) {
            pdata->p2 = pdata->p1 + step - 1;
         } else {
            pdata->p2 = nel - 1;
         }

/* Store the other information needed to process the group of output
   pixels. */
         pdata->in = in;
         pdata->ret = ret;
         pdata->skip = skip;
         pdata->dims = dims;
         pdata->thresh = thresh;
         pdata->centre = centre;
         pdata->magic = magic;
         pdata->off = off;
         pdata->on = on;

/* Submit the job to be processed by the next available worker thread. */
         thrAddJob( wf, 0, pdata, cupidRCAPar, 0, NULL, status );
      }

/* Wait for all jobs to complete. */
      thrWait( wf, status );
   }

/* Free the job data */
   job_data = astFree( job_data );

/* Return the pointer to the output array. */
   return ret;
}




static void cupidRCAPar( void *job_data_ptr, int *status ) {
/*
*  Name:
*     cupidRCAPar

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     cupidRCA

*  Invocation:
*     cupidRCAPar( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = CupidRCAData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   CupidRCAData *pdata; /* Structure containing job information */
   double thresh;
   hdsdim *dims;       /* Pointer to array dimensions */
   hdsdim ix;          /* Input pixel GRID index on axis 1 */
   hdsdim iy;          /* Input pixel GRID index on axis 2 */
   hdsdim iz;          /* Input pixel GRID index on axis 3 */
   hdsdim ox;          /* Output pixel GRID index on axis 1 */
   hdsdim oy;          /* Output pixel GRID index on axis 2 */
   hdsdim oz;          /* Output pixel GRID index on axis 3 */
   hdsdim xhi;         /* Highest usable pixel GRID index on axis 1 */
   hdsdim xlo;         /* Lowest usable pixel GRID index on axis 1 */
   hdsdim yhi;         /* Highest usable pixel GRID index on axis 2 */
   hdsdim ylo;         /* Lowest usable pixel GRID index on axis 2 */
   hdsdim zhi;         /* Highest usable pixel GRID index on axis 3 */
   hdsdim zlo;         /* Lowest usable pixel GRID index on axis 3 */
   int *in;
   int *pin0;          /* Pointer to input pixel [0,0,0] */
   int *pin;           /* Pointer to input pixel */
   int *piny;          /* Pointer to input pixel at start of row */
   int *pinz;          /* Pointer to input pixel at start of plane */
   int *pout;          /* Pointer to output pixel */
   int centre;
   int magic;
   int off;
   int on;
   int sum;            /* No. of edge neighbours */
   int tot;            /* Total no. of neighbours */
   size_t *skip;       /* Pointer to array of vector index increments */
   size_t iv;          /* Vector index into input array */
   size_t p2;          /* Vector index of last o/p pixel to process */
   size_t remain;      /* Vector index residual after subtracting higher dims */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (CupidRCAData *) job_data_ptr;

/* Copy values form the structure into local variables */
   p2 = pdata->p2;
   skip = pdata->skip;
   dims = pdata->dims;
   thresh = pdata->thresh;
   centre = pdata->centre;
   magic = pdata->magic;
   off = pdata->off;
   on = pdata->on;
   in = pdata->in;

/* Get a pointer to the input pixel which would have GRID indices [0,0,0]
   if the input array extended that far (in fact the first pixel in the
   input array has GRID indices [1,1,1]). */
   pin0 = in - skip[ 0 ] - skip[ 1 ] - skip[ 2 ];

/* Store a pointer to the first output pixel to be created by this thread. */
   pout = pdata->ret + pdata->p1;

/* Loop round the vector index of all output pixels to be processed by this
   thread. */
   for( iv = pdata->p1; iv <= p2; iv++ ){

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

/* Get the 0-based grid indices corresponding to this vector index. */
         if( skip[ 2 ] ) {
            oz = iv/skip[ 2 ];
            remain = iv - oz*skip[ 2 ];
         } else {
            oz = 0;
            remain = iv;
         }
         if( skip[ 1 ] ) {
            oy = remain/skip[ 1 ];
            ox = remain - oy*skip[ 1 ];
         } else {
            oy = 0;
            ox = remain;
         }

/* Convert to 1-based grid indices. */
         oz++;
         oy++;
         ox++;

/* Get the usable bounds on each axis, cropping at the edges of the array */
         zlo = oz - 1;
         if( zlo < 1 ) zlo = 1;
         zhi = oz + 1;
         if( zhi > dims[ 2 ] ) zhi = dims[ 2 ];

         ylo = oy - 1;
         if( ylo < 1 ) ylo = 1;
         yhi = oy + 1;
         if( yhi > dims[ 1 ] ) yhi = dims[ 1 ];

         xlo = ox - 1;
         if( xlo < 1 ) xlo = 1;
         xhi = ox + 1;
         if( xhi > dims[ 0 ] ) xhi = dims[ 0 ];

/* Initialise. */
         tot = 0;
         sum = 0;

/* Loop round the usable pixels. */
         pinz = pin0 + iv + (zlo - oz + 1 )*skip[ 2 ];
         for( iz = zlo; iz <= zhi; iz++ ) {
            piny = pinz + (ylo - oy + 1 )*skip[ 1 ];
            for( iy = ylo; iy <= yhi; iy++ ) {
               pin = piny + (xlo - ox + 1 )*skip[ 0 ];
               for( ix = xlo; ix <= xhi; ix++ ) {
                  tot++;
                  if( *pin == on ) sum++;
                  pin++;
               }
               piny = piny + skip[ 1 ];
            }
            pinz = pinz + skip[ 2 ];
         }

/* If the fraction of neighbouring on pixels is more than "thresh", set the
   output pixel on. Otherwise set it off. Move on to the next output pixel. */
         *(pout++) = ( ( (float) sum )/( (float) tot ) > thresh ) ? on : off;
      }
   }
}
