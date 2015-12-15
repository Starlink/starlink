/*
*+
*  Name:
*     smf_jsatilexy2i

*  Purpose:
*     Convert the (x,y) indices of a sky tile into a scalar tile index.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_jsatilexy2i( int xt, int yt, smfJSATiling *skytiling, int *status )

*  Arguments:
*     xt = int (Given)
*        The zero-based index of the tile in the X (RA) direction.
*     yt = int (Given)
*        The zero-based index of the tile in the Y (Dec) direction.
*     skytiling = smfJSATiling * (Given)
*        Pointer to a structure holding parameters describing the tiling
*        scheme used for the required JCMT instrument, as returned by
*        function smf_jsatiling.
*     status = int * (Given)
*        Pointer to the inherited status variable.

*  Returned Value:
*     The zero-based scalar tile index of the requested tile. VAL__BADI is
*     returned if no tile has the requested X adn Y offsets.

*  Description:
*     This function returns a scalar integer index for a tile which is
*     offset by given numbers of tiles along the X (RA) and Y (Dec) axes
*     away from the bottom left tile in the all-sky map. If the offsets
*     place the tile in the unused top-right half of the top-right facet,
*     then the index of the corresponding tile in the top-right half of
*     the bottom-left facet is returned. Likewise, if the offsets place the
*     tile in the unused bottom-left half of the bottom-left facet, then
*     the index of the corresponding tile in the bottom-left half of the
*     top-right facet is returned (the top-rihgt and bottom-left facets
*     cover the same area on the sky, and only half is used from each).

*  Authors:
*     DSB: David S Berry (JAC, UCLan)
*     GSB: Graham Bell (JAC)
*     {enter_new_authors_here}

*  History:
*     11-MAR-2011 (DSB):
*        Initial version.
*     29-OCT-2013 (GSB):
*        Use zero-based numbering for JSA tiles and number facets
*        as defined by HEALPix.
*     30-OCT-2013 (GSB):
*        Use nested numbering scheme for JSA tiles.
*     15-DEC-2015 (DSB):
*        If the supplied offsets place the tile in the unsed top-right or
*        bottom-left half-facet, then return the index of the
*        corresponding tile in the used bottom-left or top-right half-facet.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"




#include "libsmf/jsatiles.h"   /* Move this to smf_typ.h and smf.h when done */

/* "Binary Magic Numbers" for interleaving bits. */
#define INTERLEAVE_MAGIC_1 0x00FF00FF
#define INTERLEAVE_MAGIC_2 0x0F0F0F0F
#define INTERLEAVE_MAGIC_3 0x33333333
#define INTERLEAVE_MAGIC_4 0x55555555


int smf_jsatilexy2i( int xt, int yt, smfJSATiling *skytiling, int *status ){

/* Local Variables: */
   int itile;
   int fi;
   int ty;
   int tx;
   int fy;
   int fx;
   int fxy;

/* Initialise the returned tile index. */
   itile = VAL__BADI;

/* Check inherited status */
   if( *status != SAI__OK ) return itile;

/* The facet with the lowest tile indices is split between the bottom
   left and top right corners of the grid. If the supplied offsets refer
   to a pixel in the unused bottom left half of the bottom left facet,
   then modify them to refer to the used bottom left half of the top right
   facet. */
   if( yt < skytiling->ntpf - 1 && xt < skytiling->ntpf - 1 - yt ) {
      xt += 4*skytiling->ntpf;
      yt += 4*skytiling->ntpf;

/* If the supplied offsets refer to a pixel in the unused top right half
   of the top right facet, then modify them to refer to the used top
   right half of the bottom left facet. */
   } else if( yt >= 9*skytiling->ntpf - 1 ||
              xt >= 9*skytiling->ntpf - 1 - yt ) {
      xt -= 4*skytiling->ntpf;
      yt -= 4*skytiling->ntpf;
   }

/* Sanity check. The offsets should now refer to a pixel in the used
   section of the grid. But just in case (for instance, if the supplied
   offsets were outside the bounds of the whole grid), check the offsets
   again, and return a bad tile index if they are not in the used
   section. */
   if( yt < skytiling->ntpf - 1 && xt < skytiling->ntpf - 1 - yt ) {
      itile = VAL__BADI;

   } else if( yt >= 9*skytiling->ntpf - 1 ||
              xt >= 9*skytiling->ntpf - 1 - yt ) {
      itile = VAL__BADI;

/* So the the offsets are now in the used parts of the grid. */
   } else {

/* Get the (x,y) indices of the facet containing the tile. */
      fx = xt/skytiling->ntpf;
      fy = yt/skytiling->ntpf;

/* The top right facet is a copy of the bottom left facet. */
      if( fx == 4 && fy == 4 ) {
         fx = fy = 0;
         xt -= 4*skytiling->ntpf;
         yt -= 4*skytiling->ntpf;
      }

/* Check they are within bounds, but check that xt >= 0 and yt >=0
   since testing fx >= and fy >= 0 will miss fx or fy betwen -1 and 0
   since they are integers. */
      if( xt >= 0 && fx < 5 &&
          yt >= 0 && fy < 5 &&
          fx >= fy - 1 && fx <= fy + 1 ) {

/* Get the scalar zero-based index of the facet within the collection of
   12 facets.  In terms of the equations in the HEALPix paper, (fx - fy)
   is (f_row - 1) [equation 10] and the values of (fx + fy) are a
   permutation of the values of F_2 [equation 12].  Note that the division
   here is an integer division. */
         fxy = fx + fy;
         fi = (5 - (fxy / 2) + ((fxy + 1) % 2)) % 4 + 4 * (1 + fx - fy);

/* Get the scalar zero-based index of the first tile within the facet. */
         itile = fi*skytiling->ntpf*skytiling->ntpf;

/* Get the (x,y) offsets of the tile into the facet. tx is measured
   north-east (left) and ty is measured north-west (up). */
         tx = (fx + 1)*skytiling->ntpf - xt - 1;
         ty = yt - fy*skytiling->ntpf;

/* Get the scalar index of the tile within the facet. Interleave the bits
   of tx and ty (tx gives the even bits) and add this index onto
   the index of the first tile in the facet, to get the index of the
   required tile. Interleaving is performed using the "Binary
   Magic Numbers" method with code based on the public domain
   example (collection (C) 1997-2005 Sean Eron Anderson) available at:
   http://graphics.stanford.edu/~seander/bithacks.html#InterleaveBMN */
         tx = (tx | (tx << 8)) & INTERLEAVE_MAGIC_1;
         tx = (tx | (tx << 4)) & INTERLEAVE_MAGIC_2;
         tx = (tx | (tx << 2)) & INTERLEAVE_MAGIC_3;
         tx = (tx | (tx << 1)) & INTERLEAVE_MAGIC_4;

         ty = (ty | (ty << 8)) & INTERLEAVE_MAGIC_1;
         ty = (ty | (ty << 4)) & INTERLEAVE_MAGIC_2;
         ty = (ty | (ty << 2)) & INTERLEAVE_MAGIC_3;
         ty = (ty | (ty << 1)) & INTERLEAVE_MAGIC_4;

         itile += tx | (ty << 1);
      }
   }

/* Return the tile index. */
   return itile;
}

