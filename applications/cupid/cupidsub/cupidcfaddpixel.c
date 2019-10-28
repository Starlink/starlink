#include "sae_par.h"
#include "cupid.h"

void cupidCFAddPixel( int *ipa, CupidPixelSet *ps, size_t iv, hdsdim x[3], double d,
                      int edge, int *status ){
/*
*+
*  Name:
*     cupidCFAddPixel

*  Purpose:
*     Add a pixel into a PixelSet.

*  Language:
*     Starlink C

*  Synopsis:
*     void cupidCFAddPixel( int *ipa, CupidPixelSet *ps, size_t iv, hdsdim x[3],
*                           double d, int edge, int *status )

*  Description:
*     This function adds a specified pixel of the input data array into a
*     specified PixelSet.

*  Parameters:
*     ipa
*        Pointer to the start of the array holding the integer index
*        (if any) associated with each pixel in the data array. This
*        shows which clump each pixel belongs to (each clump is identified
*        by a unique integer index). The array should be the same shape and
*        size as the data array. Pixels which have not yet been assigned
*        to a clump are marked with the integer value CUPID__CFNULL.
*     ps
*        Pointer to the PixelSet which is to receive the pixel.
*     iv
*        The 1D vector index of the pixel within the data array and the
*        ipa array.
*     x
*        The GRID indices at the centre of the pixel.
*     d
*        The data value at the pixel.
*     edge
*        A boolean flag indicating if the pixel is adjacent to any edge of
*        the data array.
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
*     11-JAN-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   int i;                /* Loop count */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* If this is not the first pixel to be added to the PixelSet, extend the
   bounding box so that it includes the supplied pixel. Increment the number
   of pixels in the PixelSet at the same time. */
   if( ps->pop++ ) {
      for( i = 0; i < 3; i++ ) {
         if( x[ i ] < ps->lbnd[ i ] ) ps->lbnd[ i ] = x[ i ];
         if( x[ i ] > ps->ubnd[ i ] ) ps->ubnd[ i ] = x[ i ];
      }

/* If this is the first pixel to be added to the PixelSet, initialise the
   bounding box so that it includes just the supplied pixel. */
   } else {
      ps->lbnd[ 2 ] = ps->ubnd[ 2 ] = x[ 2 ];
      ps->lbnd[ 1 ] = ps->ubnd[ 1 ] = x[ 1 ];
      ps->lbnd[ 0 ] = ps->ubnd[ 0 ] = x[ 0 ];
   }

/* If the pixel data value exceed the previous largest pixel value in the
   PixelSet, update the value and position of the peak pixel in the
   PixelSet. */
   if( d > ps->vpeak ) {
      ps->vpeak = d;
      ps->peak[ 0 ]= x[ 0 ];
      ps->peak[ 1 ]= x[ 1 ];
      ps->peak[ 2 ]= x[ 2 ];
   }

/* If the pixel is an edge pixel, indicate that the PixelSet touches the
   edge of the data array. */
   if( edge ) ps->edge = 1;

/* Store the new index value. */
   ipa[ iv ] = ps->index;

}
