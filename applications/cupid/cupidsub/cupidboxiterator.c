#include "sae_par.h"
#include "ast.h"
#include "cupid.h"

CupidBoxIter *cupidBoxIterator( CupidBoxIter *iter, hdsdim dim[3], hdsdim lbnd[3],
                                hdsdim ubnd[3], int gap, int *status ){
/*
*+
*  Name:
*     cupidBoxIterator

*  Purpose:
*     Create an iterator that iterates round the pixels within a
*     rectangular section of a 3D array.

*  Language:
*     Starlink C

*  Synopsis:
*     CupidBoxIter *cupidBoxIterator( CupidBoxIter *iter, hdsdim dim[3],
*                                     hdsdim lbnd[3], hdsdim ubnd[3], int gap,
*                                     int *status )

*  Description:
*     This function returns a pointer to a structure that can be passed
*     to cupidNextIt in order to determine the next pixel to process in a
*     rectangular section of a 3D array. The pixels are processed in
*     natural fortran order. The structure behaves rather like a Java
*     "Iterator".
*
*     If the supplied region overlaps the edge of the array, then the
*     iterator will skip over pixels that fall outside the bounds of the
*     array.

*  Parameters:
*     iter
*        A pointer to an existing CupidBoxIter structure, or NULL. If
*        supplied, the contents of this structure will be reset to describe
*        the required set of pixels, and the supplied pointer will be
*        returned as the function value. If a NULL pointer is supplied,
*        memory will be allocated to hold a new CupidBoxIter structure to
*        describe the required set of pixels, and this new pointer will be
*        returned a the function value.
*     dim
*        The dimensions of the array in which the pixel reside.
*     lbnd
*        The lower bounds of the rectangular array secton to be iterated
*        over, in grid indices.
*     ubnd
*        The upper bounds of the rectangular array secton to be iterated
*        over, in grid indices.
*     gap
*        The gap between adjacent pixels visited by the iterator. If a
*        value of 1 or less is supplied then all pixels are visited. If
*        (say) a value of 2 is supplied,then every other pixel will be
*        visited, every other row will be visited, and every other plane
*        will be visited.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     A pointer to a structure describing the next pixel to be processed
*     in the rectangular section (this will initially be the pixel
*     specified by "lbnd"). This can be passed to cupidNextIt in order to
*     get a pointer to the array data value, and the explicit (x,y,z)
*     grid indices of the pixel. It should be freed using astFree when no
*     longer needed.

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
*     13-SEP-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   CupidBoxIter *result;
   int i;
   int overlap;
   hdsdim ovlbnd[ 3 ];
   hdsdim ovubnd[ 3 ];

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return NULL;

/* Get a pointer for the returned CupidBoxIter structure, allocating memory
   for a new structure if none was supplied. */
   if( iter ) {
      result = iter;
   } else {
      result = astMalloc( sizeof( CupidBoxIter ) );
   }

/* Check the pointer can be used safely. */
   if( result ) {

/* Get the bounds of the overlap between the supplied subsection and
   the array, and note if there is no overlap. */
      overlap = 1;
      for( i = 0; i < 3; i++ ) {
         if( lbnd[ i ] >= 1 ) {
            ovlbnd[ i ] = lbnd[ i ];
         } else {
            ovlbnd[ i ] = 1;
         }

         if( ubnd[ i ] <= dim[ i ] ) {
            ovubnd[ i ] = ubnd[ i ];
         } else {
            ovubnd[ i ] = dim[ i ];
         }

         if( ovubnd[ i ] < ovlbnd[ i ] ) {
            overlap = 0;
            break;
         }
      }

/* If there is no overlap, set a flag indicating that no pixels remain to
   be visited. */
      if( !overlap ) {
         result->done = 1;

/* Otherwise, indicate there are som epixels to be visited. */
      } else {
         result->done = 0;

/* Store the supplied inter-pixel gap size. */
         result->gap = gap;

/* Store the number of pixels in "gap" x rows of the array. */
         result->xsize = gap*dim[ 0 ];

/* Store the number of pixels in "gap" xy planes of the array. */
         result->xysize = gap*dim[ 0 ]*dim[ 1 ];

/* Store the bounds of the overlap region. */
         result->lbnd0 = ovlbnd[ 0 ];
         result->lbnd1 = ovlbnd[ 1 ];
         result->lbnd2 = ovlbnd[ 2 ];
         result->ubnd0 = ovubnd[ 0 ];
         result->ubnd1 = ovubnd[ 1 ];
         result->ubnd2 = ovubnd[ 2 ];

/* Store the grid indices of the first pixel to be visited. */
         result->xx0 = ovlbnd[ 0 ];
         result->xx1 = ovlbnd[ 1 ];
         result->xx2 = ovlbnd[ 2 ];

/* Store the 1D vector index within the array of the first pixel to be
   visited. */
         result->i = ( ovlbnd[ 0 ] - 1 ) +
                     dim[ 0 ]*( ( ovlbnd[ 1 ] - 1 ) +
                       dim[ 1 ]*( ovlbnd[ 2 ] - 1 ) );

/* Store the index of the first pixel in the first xy plane in the overlap. */
         result->i2 = result->i;

/* Store the index of the first pixel in the first x row in the overlap. */
         result->i3 = result->i;

      }
   }

/* Return the result */
   return result;
}

