#include "sae_par.h"
#include "cupid.h"
#include <limits.h>

int cupidRFillClumps( int *ipa, int *out, size_t nel, int ndim, size_t skip[ 3 ],
                      hdsdim dims[ 3 ], int peakval, int *status ){
/*
*+
*  Name:
*     cupidRFillClumps

*  Purpose:
*     Identify clumps by filling in the volume enclosed by the marked
*     edges.

*  Language:
*     Starlink C

*  Synopsis:
*     int cupidRFillClumps( int *ipa, int *out, size_t nel, int ndim,
*                           size_t skip[ 3 ], hdsdim dims[ 3 ], int peakval,
*                           int *status )

*  Description:
*     This function is supplied with an array in which pixels marking the
*     edges of clumps are flagged using the value CUPID__KEDGE, and
*     pixels marking the peak value within a clump are marked by the
*     value "peakval". It assigns a unique integer index to each peak
*     and then finds the extent of the clump surrounding the peak. In the
*     returned array, all pixels assigned to a peaks clump hold the integer
*     index associated with the clump. Pixels not in any clump have the
*     value -INT_MAX. Edge pixels are not considered to be part of any clump.
*
*     If more than one peak claims a pixel, the pixel is given to the
*     closest peak.
*
*     Note, the algorithm used for filling is not fool-proof and cannot
*     in general deal with clumps which are "S" shaped (for instance).

*  Parameters:
*     ipa
*        Pointer to an array which is the same shape and size as the data
*        array, and which holds a flag for every pixel. On entry, if the
*        pixel is an edge pixel this flag will be CUPID__KEDGE. If it is a
*        peak pixel it will have the value "peakval". Unchanged on exit.
*     out
*        Pointer to an array which is the same shape and size as the data
*        array, and which holds a flag for every pixel. On exit it holds
*        the pixel index (0 or more) at all pixels which are deemed to be
*        within a clump, and -INT_MAX everywhere else.
*     nel
*        The number of elements in "ipa".
*     ndim
*        The number of pixel axes in the data (this can be less than 3).
*     skip
*        The increment in 1D vector index required to move a distance of 1
*        pixel along each axis. This allows conversion between indexing
*        the array using a single 1D vector index and using nD coords. This
*        array should have 3 elements even if there are less than 3 pixel
*        axes, and the extra elements should be filled with zero's.
*     dims
*        The no. of pixels along each pixel axis. This array should have 3
*        elements even if there are less than 3 pixel axes, and the extra
*        elements should be filled with one's.
*     peakval
*        The "ipa" value used to flag peaks.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     The largest integer clump identifier present in the "out" array.
*     The smallest identifier value is zero.

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
*     24-JAN-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   hdsdim *gpeak[ 3 ];   /* Pointers to arrays of peak axis values */
   hdsdim gp[ 3 ];       /* Grid coords of peak position */
   hdsdim ix;            /* The X Grid coord of the current pixel */
   hdsdim iy;            /* The Y Grid coord of the current pixel */
   hdsdim iz;            /* The Z Grid coord of the current pixel */
   int *pa;              /* Pointer to next "ipa" element */
   int ipeak;            /* Index of next clump */
   int npeak;            /* Number of peaks being produced */
   size_t i;             /* Index of next "ipa" element */

/* Initialise */
   ipeak = -1;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ipeak;

/* Fill the output array with -INT_MAX. */
   for( i = 0; i < nel; i++ ) out[ i ] = -INT_MAX;

/* So far we have no peaks */
   npeak = 0;
   gpeak[ 0 ] = astMalloc( sizeof(**gpeak)*30 );
   gpeak[ 1 ] = astMalloc( sizeof(**gpeak)*30 );
   gpeak[ 2 ] = astMalloc( sizeof(**gpeak)*30 );

/* Scan the ipa array looking for peaks. */
   pa = ipa;
   i = 0;

   for( iz = 1; iz <= dims[ 2 ]; iz++ ) {
      for( iy = 1; iy <= dims[ 1 ]; iy++ ) {
         for( ix = 1; ix <= dims[ 0 ]; ix++, i++, pa++ ) {
            if( *pa == peakval ) {

/* Find the integer identifier for this peak. */
               ipeak = npeak++;

/* Save thre grid coords of the peak in the gpeak array, extending it if
   necessary to make room. */
               gpeak[ 0 ] = astGrow( gpeak[ 0 ], npeak, sizeof(**gpeak) );
               gpeak[ 1 ] = astGrow( gpeak[ 1 ], npeak, sizeof(**gpeak) );
               gpeak[ 2 ] = astGrow( gpeak[ 2 ], npeak, sizeof(**gpeak) );
               if( gpeak[ 2 ] ) {
                  gpeak[ 0 ][ ipeak ] = ix;
                  gpeak[ 1 ][ ipeak ] = iy;
                  gpeak[ 2 ][ ipeak ] = iz;
               }

/* Fill the volume between the edges marked in the "ipa" array by first
   moving out away from the peak along a 1D line parallel to the X axis
   until edge pixels are encountered. At each position along this line,
   store the "ipeak" value in the corresponding pixel of the "out" array,
   and then move out away from the position along a 1D line parallel to
   the Y axis until edge pixels are encountered. At each position along
   this line, store the "ipeak" value in the corresponding pixel of the
   "out" array, and then move out away from the position along a 1D line
   parallel to the Z axis until edge pixels are encountered. At each
   position along this line, store the "ipeak" value in the corresponding
   pixel of the "out" array. */
               gp[ 0 ] = ix;
               gp[ 1 ] = iy;
               gp[ 2 ] = iz;
               cupidRFillLine( ipa, out, nel, ndim, skip, dims, gp, i, 0,
                               ipeak, 1, gpeak, status );
            }
         }
      }
   }

/* If we are dealing with 2 or 3 d data, we do the whole process again
   (without re-initialising the "out" array), but this time scanning the
   axes in the order Y, Z, X (instead of X,Y,Z). */
   if( ndim > 1 ) {
      npeak = 0;
      pa = ipa;
      i = 0;

      for( iz = 1; iz <= dims[ 2 ]; iz++ ) {
         for( iy = 1; iy <= dims[ 1 ]; iy++ ) {
            for( ix = 1; ix <= dims[ 0 ]; ix++, i++, pa++ ) {
               if( *pa == peakval ) {
                  ipeak = npeak++;
                  gp[ 0 ] = ix;
                  gp[ 1 ] = iy;
                  gp[ 2 ] = iz;
                  cupidRFillLine( ipa, out, nel, ndim, skip, dims, gp, i, 1,
                                  ipeak, 1, gpeak, status );
               }
            }
         }
      }

/* If we are dealing with 3 d data, we do the whole process again (without
   re-initialising the "out" array), but this time scanning the axes in the
   order Z, X, Y. */
      if( ndim > 2 ) {
         npeak = 0;
         pa = ipa;
         i = 0;

         for( iz = 1; iz <= dims[ 2 ]; iz++ ) {
            for( iy = 1; iy <= dims[ 1 ]; iy++ ) {
               for( ix = 1; ix <= dims[ 0 ]; ix++, i++, pa++ ) {
                  if( *pa == peakval ) {
                     ipeak = npeak++;
                     gp[ 0 ] = ix;
                     gp[ 1 ] = iy;
                     gp[ 2 ] = iz;
                     cupidRFillLine( ipa, out, nel, ndim, skip, dims, gp, i, 2,
                                     ipeak, 1, gpeak, status );
                  }
               }
            }
         }
      }
   }

/* Free resources. */
   gpeak[ 0 ] = astFree( gpeak[ 0 ] );
   gpeak[ 1 ] = astFree( gpeak[ 1 ] );
   gpeak[ 2 ] = astFree( gpeak[ 2 ] );

/* Return the largest identifier present in "out". */
   return ipeak;

}
