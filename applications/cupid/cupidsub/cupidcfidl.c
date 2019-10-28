#include "sae_par.h"
#include "prm_par.h"
#include "cupid.h"
#include <string.h>
#include <limits.h>

void cupidCFIdl( CupidPixelSet *ps, int *ipa, int ndim, hdsdim *dims,
                 size_t skip[3], int naxis, CupidPixelSet **clumps,
                 int *status ){
/*
*+
*  Name:
*     cupidCFIdl

*  Purpose:
*     Transfer all the pixels in one PixelSet into another, using the
*     algorithm of the Williams IDL ClumpFind implementation.

*  Language:
*     Starlink C

*  Synopsis:
*     void cupidCFIdl( CupidPixelSet *ps, int *ipa, int ndim, hdsdim *dims,
*                      size_t skip[3], int naxis, CupidPixelSet **clumps,
*                      int *status )

*  Description:
*     This function transfer all the pixels in PixelSet "ps" to one of
*     the adjoining PixelSets (if any). A specific pixel is transfered to
*     the PixelSet that has the closest peak pixel. This is the method
*     used by the IDL implementation of ClumpFind available on Jonathan
*     Williams web site for carving up merged clumps. It differs from the
*     "friends-of-friends" algorithm described in the original Williams et
*     al ApJ paper.

*  Parameters:
*     ps
*        Pointer to the source PixelSet structure containing the pixels to
*        be moved.
*     ipa
*        Pointer to the start of the array holding the integer index
*        (if any) associated with each pixel in the data array. This
*        array should be the same shape and size as the data array.
*     ndim
*        The number of pixel axes in the data array.
*     dims
*        The number of pixels on each pixel axis of the data array. This
*        array should have 3 elements even if "ndim" is less than 3, and
*        the extra elements should be filled with 1's.
*     skip
*        The increment in 1D vector index required to move a distance of 1
*        pixel along each axis. This allows conversion between indexing
*        the array using a single 1D vector index and using nD coords.
*        Unused trailing elements should be filled with zero.
*     naxis
*        Determines which adjoining pixels are considered to be "neighbours"
*        of a specified central pixel. Should be in the range 1 to "ndim".
*        For a pixel to be considered to be a neighbour of another pixel,
*        the two pixels must be no more than 1 pixel away along no more than
*        "naxis" axes.
*     clumps
*        Array holding pointers to all previously defined PixelSets, such
*        that a pointer to the PixelSet with index value "i" is stored at
*        element "i" of the "clumps" array.
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
*     28-APR-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   hdsdim *p;       /* Pointer to array of GRID coords at clump peak */
   hdsdim ix;       /* GRID index on 1st axis */
   hdsdim iy;       /* GRID index on 2nd axis */
   hdsdim iz;       /* GRID index on 3rd axis */
   hdsdim x[ 3 ];   /* GRID indices of current array element */
   int *nebs;       /* Pointer to list of neighbouring clump indices */
   int *v1;         /* Pointer to element at start of this row */
   int *v2;         /* Pointer to element at start of this plane */
   int *v;          /* Pointer to next array element */
   int d2;          /* Squared distance from peak to pixel */
   int d2min;       /* Minimum squared distance found so far */
   int d;           /* Axis increment from peak to pixel */
   int edge;        /* Pixel at upper or lower bound on any axis? */
   int i;           /* Loop count */
   int iclump;      /* Clump index */
   int iclumpmin;   /* Clump index with minimum squared distance */
   int j;           /* Loop count */
   int old_index;   /* Original index value of the transferred pixels */
   int sorted;      /* Is list now sorted? */
   int t;           /* Temporary storage */
   int yedge;       /* Pixel at upper or lower bound on 2nd axis? */
   int zedge;       /* Pixel at upper or lower bound on 3rd axis? */
   size_t iv;       /* 1D vector index */

/* Check inherited status. Also return without action if the supplied
   PointSet is not adjacent to any other clumps. */
   if( *status != SAI__OK || ps->nneb == 0 ) return;

/* The supplied PixelSet contains a list of the indices of all adjoining
   clumps. This list may contain multiple occurences of any given index,
   so first sort the list into increasing order (bubblesort). */
   nebs = ps->nebs;
   j = ps->nneb - 1;
   sorted = 0;
   while( !sorted ) {
      j--;
      sorted = 1;
      for( i = 0; i <= j; i++ ) {
         if( nebs[ i + 1 ] < nebs[ i ] ) {
            t = nebs[ i ];
            nebs[ i ] = nebs[ i + 1 ];
            nebs[ i + 1 ] = t;
            sorted = 0;
         }
      }
   }

/* Now remove multiple instances of each clump index. */
   j = 0;
   for( i = 1; i < ps->nneb; i++ ) {
      if( nebs[ i ] != nebs[ j ] ) nebs[ ++j ] = nebs[ i ];
   }
   ps->nneb = ++j;

/* Get the index value of the source PixelSet. */
   old_index = ps->index;

/* Get a pointer to the first pixel in the source PixelSet bounding box. If
   the data  has less than 3 axes, the unused upper and lower bounds will be
   set to [1,1] and so we can always pretend there are 3 axes. */
   v = ipa + ( ps->lbnd[ 0 ] - 1 ) + ( ps->lbnd[ 1 ] - 1 )*skip[ 1 ] +
             ( ps->lbnd[ 2 ] - 1 )*skip[ 2 ];

/* Loop round the pixels in the source PixelSet bounding box. */
   for( iz = ps->lbnd[ 2 ]; iz <= ps->ubnd[ 2 ]; iz++ ) {
      x[ 2 ] = iz;
      zedge = ( ndim > 2 && ( iz == 1 || iz == dims[ 2 ] ) );
      v2 = v;

      for( iy = ps->lbnd[ 1 ]; iy <= ps->ubnd[ 1 ]; iy++ ) {
         x[ 1 ] = iy;
         yedge = ( ndim > 1 && ( iy == 1 || iy == dims[ 1 ] ) );
         v1 = v;

         for( ix = ps->lbnd[ 0 ]; ix <= ps->ubnd[ 0 ]; ix++ ) {
            x[ 0 ] = ix;
            edge = yedge || zedge || ( ix == 1 || ix == dims[ 0 ] );

/* If the pixel was originally a member of the source PixelSet, then we
   transfer it to the adjoining PixelSet with the closest peak */
            if( *v == old_index ) {
               iv = v - ipa;

/* Loop round every adjoining clump. */
               d2min = INT_MAX;
               iclumpmin = 0;
               for( i = 0; i < ps->nneb; i++ ) {
                  iclump = nebs[ i ];

/* Find the squared distance from the current pixel to the peak pixel in the
   current adjoining clump. */
                  p = clumps[ iclump ]->peak;
                  d = p[ 0 ] - ix;
                  d2 = d*d;
                  d = p[ 1 ] - iy;
                  d2 += d*d;
                  d = p[ 2 ] - iz;
                  d2 += d*d;

/* If this is less than the previous minimum, remember it. */
                  if( d2 < d2min ) {
                     d2min = d2;
                     iclumpmin = iclump;
                  }
               }

/* Transfer the pixel to the new PixelSet. */
               cupidCFAddPixel( ipa, clumps[ iclumpmin ], iv, x, VAL__MIND,
                                edge, status );
            }

/* Get the pointer to the next pixel in the source PixelSet bounding box. */
            v++;
         }
         v = v1 + skip[ 1 ];
      }
      v = v2 + skip[ 2 ];
   }

/* Zero the population of the source PixelSet. */
   ps->pop = 0;

}
