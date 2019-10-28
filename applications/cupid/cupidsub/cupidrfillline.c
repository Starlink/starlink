#include "sae_par.h"
#include "cupid.h"
#include <limits.h>

void cupidRFillLine( int *ipa, int *out, size_t nel, int ndim, size_t skip[ 3 ],
                     hdsdim dims[ 3 ], hdsdim gp[ 3 ], size_t iv, int axis, int id,
                     int depth, hdsdim *gpeak[ 3 ], int *status ){
/*
*+
*  Name:
*     cupidRFillLine

*  Purpose:
*     Fill a line through a clump with the clump id value.

*  Language:
*     Starlink C

*  Synopsis:
*     void cupidRFillLine( int *ipa, int *out, size_t nel, int ndim,
*                          size_t skip[ 3 ], hdsdim dims[ 3 ], hdsdim gp[ 3 ],
*                          size_t iv, int axis, int id, int depth,
*                          hdsdim *gpeak[ 3 ], int *status )

*  Description:
*     This function fills the volume between the edges marked in the "ipa"
*     array which surround a given position. Each pixel within the edges
*     is assigned the value "id" in the "out" array (all other elements
*     of "out" are left unchanged).
*
*     The algorithm works by moving out away from the central position "gp"
*     along a 1D line parallel to the axis given by "axis" until pixels
*     marked as edges within "ipa" are encountered. At each position along
*     this line, the "ipeak" value is stored in the corresponding pixel of
*     the "out" array, and the algorithm calls this function recursively
*     to move out away from the position along a 1D line parallel to an
*     orthoganal axis, filling the "out" array with "id" until edge
*     pixels are encountered. That recursive call in turn calls this
*     function recursively to fill out along the third remaining axis.
*
*     If more than one peak claims a pixel, the pixel is given to the
*     closest peak.

*  Parameters:
*     ipa
*        Pointer to an array which is the same shape and size as the data
*        array, and which holds a flag for every pixel. If the pixel is an
*        edge pixel this flag will be CUPID__KEDGE.
*     out
*        Pointer to an array which is the same shape and size as the data
*        array, and which holds a flag for every pixel. Every pixel
*        deemed to be within the clump is assigned the value "id" on exit.
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
*     gp
*        Grid indices of the central position from which the fill is to
*        originate.
*     iv
*        The 1D vector index corresponding to "gp".
*     axis
*        The index of the axis along which to fill.
*     id
*        The identifier value to assign to each filled pixel.
*     depth
*        The depth of recursion into this function.
*     gpeak
*        Stores the grid coords of each peak. The (x,y,z) values for peak
*        "ipeak" are stored at "gpeak[0][ipeak]", "gpeak[1][ipeak]",
*        "gpeak[2][ipeak]".
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
*     2-FEB-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   hdsdim dx;            /* Increment in x */
   hdsdim dy;            /* Increment in y */
   hdsdim dz;            /* Increment in z */
   hdsdim p[ 3 ];        /* Grid indices of next root position */
   int iaxis;            /* Next axis to fill */
   int oldid;            /* Id of peak currently assigned to the pixel */
   size_t dnew;          /* Squared distance to new peak */
   size_t dold;          /* Squared distance to old peak */
   size_t ii;            /* Vector index of next root position */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Ensure we do not change the supplied values. */
   p[ 0 ] = gp[ 0 ];
   p[ 1 ] = gp[ 1 ];
   p[ 2 ] = gp[ 2 ];
   ii = iv;

/* Note the index of the next orthogonal axis. */
   iaxis = axis + 1;
   if( iaxis == 3 ) iaxis = 0;

/* Scan out along the +ve direction of the specified axis, looking for an
   edge pixel, the edge of the array, or a pixel which has already been
   assigned to another clump. */
   while( p[ axis ] <= dims[ axis ] && ipa[ ii ] != CUPID__KEDGE ) {

/* If this pixel is not yet assigned to a clump, store the clump id value. */
      oldid = out[ ii ];
      if( oldid == -INT_MAX ) {
         out[ ii ] = id;

/* If this pixel is already assigned to a clump, choose the clump which
   has the loser peak. */
      } else if( oldid != id ) {
         dx = p[ 0 ] - gpeak[ 0 ][ id ];
         dy = p[ 0 ] - gpeak[ 0 ][ id ];
         dz = p[ 0 ] - gpeak[ 0 ][ id ];
         dnew = dx*dx + dy*dy + dz*dz;

         dx = p[ 0 ] - gpeak[ 0 ][ oldid ];
         dy = p[ 0 ] - gpeak[ 0 ][ oldid ];
         dz = p[ 0 ] - gpeak[ 0 ][ oldid ];
         dold = dx*dx + dy*dy + dz*dz;

         if( dnew < dold ) {
            out[ ii ] = id;
         } else {
            break;
         }
      }

/* Call this function recursively to fill the orthogonal line along the
   next axis, starting at the current position. */
      if( depth < 3 ) {
         cupidRFillLine( ipa, out, nel, ndim, skip, dims, p, ii, iaxis, id,
                         depth + 1, gpeak, status );
      }

/* Move to the next +ve axis position. */
      p[ axis ]++;
      ii += skip[ axis ];
   }

/* Re-instate the original position. */
   p[ 0 ] = gp[ 0 ];
   p[ 1 ] = gp[ 1 ];
   p[ 2 ] = gp[ 2 ];
   ii = iv;

/* Scan out along the -ve direction of the specified axis, looking for an
   edge pixel, the edge of the array, or a pixel which has already been
   assigned to another clump. */
   while( p[ axis ] >= 1 && ipa[ ii ] != CUPID__KEDGE ) {

/* If this pixel is not yet assigned to a clump, store the clump id value. */
      oldid = out[ ii ];
      if( oldid == -INT_MAX ) {
         out[ ii ] = id;

/* If this pixel is already assigned to a clump, choose the clump which
   has the loser peak. */
      } else if( oldid != id ) {
         dx = p[ 0 ] - gpeak[ 0 ][ id ];
         dy = p[ 0 ] - gpeak[ 0 ][ id ];
         dz = p[ 0 ] - gpeak[ 0 ][ id ];
         dnew = dx*dx + dy*dy + dz*dz;

         dx = p[ 0 ] - gpeak[ 0 ][ oldid ];
         dy = p[ 0 ] - gpeak[ 0 ][ oldid ];
         dz = p[ 0 ] - gpeak[ 0 ][ oldid ];
         dold = dx*dx + dy*dy + dz*dz;

         if( dnew < dold ) {
            out[ ii ] = id;
         } else {
            break;
         }
      }

/* Call this function recursively to fill the orthogonal line along the
   next axis, starting at the current position. */
      if( depth < 3 ) {
         cupidRFillLine( ipa, out, nel, ndim, skip, dims, p, ii, iaxis, id,
                         depth + 1, gpeak, status );
      }

/* Move to the next -ve axis position. */
      p[ axis ]--;
      ii -= skip[ axis ];
   }

}
