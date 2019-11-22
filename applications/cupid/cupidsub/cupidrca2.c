#include "sae_par.h"
#include "ast.h"
#include "cupid.h"
#include "star/thr.h"

/* Local data types */
typedef struct CupidRCA2Data {
   size_t p1;
   size_t p2;
   int *in;
   int *ret;
   size_t *skip;
   hdsdim *dims;
   int target;
} CupidRCA2Data;

/* Prototypes for local functions */
static void cupidRCA2Par( void *job_data_ptr, int *status );



int *cupidRCA2( ThrWorkForce *wf, int *in, int *out, size_t nel,
                hdsdim dims[ 3 ], size_t skip[ 3 ], int *status ){
/*
*+
*  Name:
*     cupidRCA2

*  Purpose:
*     Erode or dilate areas within an array using a cellular automata.

*  Language:
*     Starlink C

*  Synopsis:
*     int *cupidRCA2( ThrWorkForce *wf, int *in, int *out, size_t nel,
*                     hdsdim dims[ 3 ], size_t skip[ 3 ], int *status )

*  Description:
*     This function smoothes the boundaries between areas of constant value
*     in an array of integer pixel values.
*
*     Each output pixel value is created in turn as follows: The input
*     values in a 3x3x3 cube of pixels centred on the output pixel are
*     examined, and the output pixel is assigned the most commonly
*     occurring input value.

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
*     2-FEB-2006 (DSB):
*        Original version.
*     31-OCT-2019 (DSB):
*        Multi-threaded.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   CupidRCA2Data *job_data = NULL;
   CupidRCA2Data *pdata;
   int *ret;
   int iw;
   int nw;
   int target;
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

/* Store the number of votes that will guarantee that a party wins. */
      target = 2;
      if( dims[ 1 ] > 1 ) target = 5;
      if( dims[ 2 ] > 1 ) target = 14;

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
         pdata->target = target;

/* Submit the job to be processed by the next available worker thread. */
         thrAddJob( wf, 0, pdata, cupidRCA2Par, 0, NULL, status );
      }

/* Wait for all jobs to complete. */
      thrWait( wf, status );
   }

/* Free the job data */
   job_data = astFree( job_data );

/* Return the pointer to the output array. */
   return ret;
}



static void cupidRCA2Par( void *job_data_ptr, int *status ) {
/*
*  Name:
*     cupidRCA2Par

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     cupidRCA2

*  Invocation:
*     cupidRCA2Par( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = CupidRCA2Data * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   CupidRCA2Data *pdata; /* Structure containing job information */
   hdsdim *dims;       /* Pointer to array dimensions */
   hdsdim ix;          /* Input pixel GRID index on axis 1 */
   hdsdim iy;          /* Input pixel GRID index on axis 2 */
   hdsdim iz;          /* Input pixel GRID index on axis 3 */
   hdsdim ox;          /* Output pixel GRID index on axis 1 */
   hdsdim oy;          /* Output pixel GRID index on axis 2 */
   hdsdim oz;          /* Output pixel GRID index on axis 3 */
   hdsdim xlo;         /* Lowest usable pixel GRID index on axis 1 */
   hdsdim xhi;         /* Highest usable pixel GRID index on axis 1 */
   hdsdim ylo;         /* Lowest usable pixel GRID index on axis 2 */
   hdsdim yhi;         /* Highest usable pixel GRID index on axis 2 */
   hdsdim zlo;         /* Lowest usable pixel GRID index on axis 3 */
   hdsdim zhi;         /* Highest usable pixel GRID index on axis 3 */
   int *pin0;          /* Pointer to input pixel [0,0,0] */
   int *pin;           /* Pointer to input pixel */
   int *piny;          /* Pointer to input pixel at start of row */
   int *pinz;          /* Pointer to input pixel at start of plane */
   int *pout;          /* Pointer to output pixel */
   int *pparty;        /* Pointer to next party pixel value */
   int *pvotes;        /* Pointer to next party vote */
   int ip;             /* The index of the next party */
   int maxvotes;       /* Vote for currently winning party */
   int np;             /* The number of parties available */
   int nvotes;         /* No. of votes remaining to be counted */
   int party[ 27 ];    /* The pixel value associated with each party */
   int target;         /* No. of votes that guarantees a party wins */
   int votes[ 27 ];    /* The number of votes for each party */
   int winner;         /* Index of winning party */
   size_t *skip;       /* Pointer to array of vector index increments */
   size_t iv;          /* Vector index into input array */
   size_t p2;          /* Vector index of last o/p pixel to process */
   size_t remain;      /* Vector index residual after subtracting higher dims */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (CupidRCA2Data *) job_data_ptr;

/* Copy values from the structure into local variables */
   p2 = pdata->p2;
   skip = pdata->skip;
   dims = pdata->dims;
   target = pdata->target;

/* Get a pointer to the input pixel which would have GRID indices [0,0,0]
   if the input array extended that far (in fact the first pixel in the
   input array has GRID indices [1,1,1]). */
   pin0 = pdata->in - skip[ 0 ] - skip[ 1 ] - skip[ 2 ];

/* Store a pointer to the first output pixel to be created by this thread. */
   pout = pdata->ret + pdata->p1;

/* Loop round the vector index of all output pixels to be processed by this
   thread. */
   for( iv = pdata->p1; iv <= p2; iv++ ){

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

/* Get the bounds on each axis of the 3x3x3 box centred on (ox,oy,oz), cropping
   at the edges of the array */
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

/* Indicate "no votes cast yet" and "no parties found yet". */
      nvotes = 0;
      np = 0;

/* Loop round all input pixels in the neighbourhood of the current output
   pixel. This is the cube of 3x3x3 input pixels, centred on the current
   output pixel. "pin0+iv" points at the first (i.e. bottom left) pixel in
   the box (ignoring array edges). */
      pinz = pin0 + iv + (zlo - oz + 1 )*skip[ 2 ];
      for( iz = zlo; iz <= zhi; iz++ ) {
         piny = pinz + (ylo - oy + 1 )*skip[ 1 ];
         for( iy = ylo; iy <= yhi; iy++ ) {
            pin = piny + (xlo - ox + 1 )*skip[ 0 ];
            for( ix = xlo; ix <= xhi; ix++ ) {

/* Each pixel in the 3x3x3 cube will have an integer value. Each distinct
   integer value is associated with a "party",and the number of occurrences
   of the distinct value within the 3x3x3 cube equals the number of "votes"
   for the party. See if the current pixel belongs to a previously found
   party. If so, increment the number of votes for the party. If the number
   of votes cast for any party exceeds half the maximum possible number of
   votes, then it is not possible for another party to win. */
               pparty = party;
               pvotes = votes;
               for( ip = 0; ip < np; ip++,pparty++,pvotes++ ) {
                  if( *pparty == *pin ) {
                     if( ++(*pvotes) >= target ) {
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
               pin++;
            }
            piny = piny + skip[ 1 ];
         }
         pinz = pinz + skip[ 2 ];
      }

/* We have now considered all the pixels in the 3x3x3 cube. See which
   party got the most votes. */
      maxvotes = 0;
      winner = 0;
      pvotes = votes;
      for( ip = 0; ip < np; ip++,pvotes++ ) {

/* See how many votes remain to be counted after the current party has
   been counted. */
         nvotes -= *pvotes;

/* If this party has more votes than any previous party, elect it as the
   new leading party, and note how many votes it got. */
         if( *pvotes > maxvotes ) {
            winner = ip;
            maxvotes = *pvotes;

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
