#include "sae_par.h"
#include "ast.h"
#include "cupid.h"

int cupidNextIt( CupidBoxIter *iter, hdsdim xx[3], hdsdim *iv, int *status ){
/*
*+
*  Name:
*     cupidNextIt

*  Purpose:
*     Returned the next pixel to be processed in a rectangular subsection
*     of a 3D array.

*  Language:
*     Starlink C

*  Synopsis:
*     int cupidNextIt( CupidBoxIter *iter, hdsdim xx[3], hdsdim *iv, int *status )

*  Description:
*     This function returns the 1D vector index and 3D grid indices of the
*     next pixel to be processed in a rectangular subsection of a 3D array,
*     as specified by a supplied CupidBoxIter structure, which should have
*     been created using cupidBoxIterator, and acts rather like a Java
*     Iterator.
*
*     Upon return, the CupidBoxIter structure is modified so that it
*     refers to the next pixel in the array subsection (in natural fortran
*     order).

*  Parameters:
*     iter
*        Pointer to the CupidBoxIter structure describing the subsection
*        of the array to be processed, as created by cupidBoxIterator.
*     xx
*        Array in which to return the 3D grid indices of the next pixel
*        to be processed.
*     iv
*        Pointer to an int in which to return the 1D vector index of the
*        next pixel to be processed, within the array defined by the "dim"
*        argument passed to cupidBoxIterator when the CupidBoxIter was created.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     A non-zero value is returned if there was at least one more pixel
*     yet to be visited by the iterator on entry to this function. If the
*     supplied iterator has already visited all pixels, then a zero value
*     is returned.

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
   hdsdim t;

/* Abort if an error has already occurred, or if the supplied iterator
   has already visited all the required pixels. . */
   if( *status != SAI__OK || iter->done ) return 0;

/* Copy the grid coords from the supplied CupidBoxIter structure.*/
   xx[ 0 ] = iter->xx0;
   xx[ 1 ] = iter->xx1;
   xx[ 2 ] = iter->xx2;

/* Copy the 1D vector index from the supplied CupidBoxIter structure.*/
   *iv = iter->i;

/* Set a flag if there are no more pixels left. */
   iter->done = 0;

/* If we have not reached the upper bound on the X axis, move on to the next
   X grid value. */
   t = xx[ 0 ] + iter->gap;
   if( t <= iter->ubnd0 ) {
      iter->xx0 = t;
      iter->i += iter->gap;

/* If we have reached the upper bound on the X axis, move the X coord
   back to the lower bound. */
   } else {
      iter->xx0 = iter->lbnd0;

/* If we have not reached the upper bound on the Y axis, move on to the next
   Y grid value, and move the vector index on to the start of the next x row. */
      t = xx[ 1 ] + iter->gap;
      if( t <= iter->ubnd1 ) {
         iter->xx1 = t;
         iter->i = iter->i3 + iter->xsize;
         iter->i3 = iter->i;

/* If we have reached the upper bound on the Y axis, move the Y coord
   back to the lower bound. */
      } else {
         iter->xx1 = iter->lbnd1;

/* If we have not reached the upper bound on the Z axis, move on to the next
   Z grid value, and move the vector index on to the start of the next xy
   plane. */
         t = xx[ 2 ] + iter->gap;
         if( t <= iter->ubnd2 ) {
            iter->xx2 = t;
            iter->i = iter->i2 + iter->xysize;
            iter->i2 = iter->i;
            iter->i3 = iter->i;

/* If we have reached the upper bound on the Z axis, we have finished. */
         } else {
            iter->done = 1;
         }
      }
   }

/* Return the result */
   return 1;
}

