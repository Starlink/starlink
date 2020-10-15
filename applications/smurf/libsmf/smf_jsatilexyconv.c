/*
*+
*  Name:
*     smf_jsatilexyconv

*  Purpose:
*     Convert tile (x,y) indices from SMF__JSA_HPX to a requested JSA projection.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_jsatilexyconv( smfJSATiling *skytiling, smf_jsaproj_t proj,
*                             int xt_hpx, int yt_hpx, int raw, int *xt_out,
*                             int *yt_out, int *status )

*  Arguments:
*     skytiling = smfJSATiling * (Given)
*        Pointer to a structure holding parameters describing the tiling
*        scheme used for the required JCMT instrument, as returned by
*        function smf_jsatiling.
*     proj = smf_jsaproj_t (Given)
*        The required projection.
*     xt_hpx = int (Giveb)
*        The zero-based index of a tile in the X (RA) direction, within
*        the SMF__JSA_HPX projection.
*     yt_hpx = int (given)
*        The zero-based index of the tile in the Y (Dec) direction, within
*        the SMF__JSA_HPX projection.
*     raw = int (given)
*        If non-zero, then the whole of the bottom left facet contains
*        tiles, and the top right facet contains no tiles. If zero, then
*        the only the top right half of the bottom left facet, and the
*        bottom left half of the top right facet contains tiles.
*     xt_out = int * (Returned)
*        Address of the integer in which to store the zero-based index
*        of the tile in the X (RA) direction, within the requested
*        projection. This can point to "xt_hpx" if required.
*     yt_out = int * (Returned)
*        Address of the integer in which to store the zero-based index
*        of the tile in the Y (Dec) direction, within the requested
*        projection. This can point to "yt_hpx" if required.
*     status = int * (Given)
*        Pointer to the inherited status variable.

*  Description:
*     This function takes the (x,y) indices of a tile in the SMF__JSA_HPX
*     projection, and returns the indices of the same tile within
*     the projection specified by "proj". The indices of a tile in any
*     projection are the offsets of the tile (in units of whole tiles)
*     from the bottom left corner of the whole sky grid to the bottom left
*     corner of the tile.
*
*     VAL__BADI values are returned if the supplied (x,y) indices do not
*     correspond to a real HPX tile (or if an error occurs).

*  Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-JUN-2014 (DSB):
*        Initial version.
*     1-OCT-2014 (DSB):
*        CHanged to support SMF__JSA_HPX12 projections.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2014 Science & Technology Facilities Council.
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
#include "mers.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/jsatiles.h"

void smf_jsatilexyconv( smfJSATiling *skytiling, smf_jsaproj_t proj,
                        int xt_hpx, int yt_hpx, int raw, int *xt_out,
                        int *yt_out, int *status ){

/* Local Variables: */
   int dx;
   int dy;
   int igore;
   int off;
   int xf;
   int xpy;
   int yf;
   int rot;
   int xtr;
   int ytr;

/* Initialise the returned values. */
   *xt_out = VAL__BADI;
   *yt_out = VAL__BADI;

/* The rotation required to rotate each gore from HPX to north XPH.
   0 = no rotation, 1 = 90 degs ACW rotation, 2 = 180 degs rotation,
   3 = 90 degs CW rotation. */
   int rot_north[ 4 ] = { 0, 1, 2, 3 };

/* The X translation required to move each rotated gore to its position
   within the north XPH projection, given as a multiple of the facet size. */
   int xtr_north[ 4 ] = { 2, 1, -2, -3 };

/* The Y translation required to move each rotated gore to its position
   within the north XPH projection, given as a multiple of the facet size. */
   int ytr_north[ 4 ] = { 0, 1, 0, -3 };

/* The same but for a south pole XPH projection. */
   int rot_south[ 4 ] = { 1, 0, 3, 2 };
   int xtr_south[ 4 ] = { 0, -1, 0, -1 };
   int ytr_south[ 4 ] = { 0, 1, 0, -3 };

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Find the (x,y) offsets (in facets within the SMF__JSA_HPX projection)
   of the facet containing the tile. */
   xf = xt_hpx / skytiling->ntpf;
   yf = yt_hpx / skytiling->ntpf;

/* Return bad (x,y) values for positions that are not on one of the 12 HPX
   facets. */
   if( yf <= xf + 1 && yf >= xf - 1 ) {

/* If required, return bad (x,y) values for positions that are within
   the half facets at bottom left and top right that are not used in
   the SMF__JSA_HPX projection. */
      xpy = xt_hpx + yt_hpx;
      if( raw || ( xpy >= skytiling->ntpf - 1 &&
                   xpy < 9*skytiling->ntpf - 1 ) ) {

/* First do XPH projections. */
         if( proj == SMF__JSA_XPHN || proj == SMF__JSA_XPHS ) {

/* Find the zero-based index of the gore containing the tile. */
            igore = (int)( ( xpy - skytiling->ntpf + 1 )/( 2*skytiling->ntpf ) );

/* Find the (x,y) offsets of the tile away from the bottom left corner of
   the square enclosing the gore within the HPX projection. */
            off = igore*skytiling->ntpf;
            dx = xt_hpx - off;
            dy = yt_hpx - off;

/* Find the rotation and translation necessary to transform the gore from
   HPX into XPH. */
            if( proj == SMF__JSA_XPHN ) {
               rot = rot_north[ igore ];
               xtr = xtr_north[ igore ];
               ytr = ytr_north[ igore ];
            } else {
               rot = rot_south[ igore ];
               xtr = xtr_south[ igore ];
               ytr = ytr_south[ igore ];
            }

/* Rotate the offsets of the tile within the gore bounding-box, so that they
   refer to the required XPH projection. */
            if( rot == 0 ) {                  /* No rotation */
               *xt_out =  dx;
               *yt_out =  dy;
            } else if( rot == 1 ) {           /* 90 degs ACW rotation */
               *xt_out =  2*skytiling->ntpf - 1 - dy;
               *yt_out =  dx;
            } else if( rot == 2 ) {           /* 180 degs ACW rotation */
               *xt_out =  2*skytiling->ntpf - 1 - dx;
               *yt_out =  2*skytiling->ntpf - 1 - dy;
            } else {                          /* 270 degs ACW rotation */
               *xt_out =  dy;
               *yt_out =  2*skytiling->ntpf - 1 - dx;
            }

/* Add on the HPX offsets to the lower left corner of the bounding box. The
   results of this are the offsets of the tile if you were just to rotate
   the gore about its centre without translating it. */
            *xt_out +=  off;
            *yt_out +=  off;

/* Add on the translation that moves the (now rotated) gore from its
   position within the SMF__JSA_HPX projection to its position within
   the requested XPH projection. */
            *xt_out += xtr*skytiling->ntpf;
            *yt_out += ytr*skytiling->ntpf;

/* Now do SMF__JSA_HPX12 projections. */
         } else if( proj == SMF__JSA_HPX12 ) {

/* Moving from SMF__JSA_HPX to SMF__JSA_HPX12 involves moving each facet
   by two facets to the upper right, so that facet 6 moves to the
   position of facet 4, etc. */
            *xt_out = xt_hpx + 2*skytiling->ntpf;
            *yt_out = yt_hpx + 2*skytiling->ntpf;

/* Facets that have been moved beyond the upper right corner of the
   projection plane now need to be wrapped-round to the bottom left corner. */
            if( *xt_out >= 5*skytiling->ntpf || *yt_out >= 5*skytiling->ntpf ) {
               *xt_out -= 4*skytiling->ntpf;
               *yt_out -= 4*skytiling->ntpf;

/* In addition, the top right "facet" (which corresponds to the same
   area of the sky as the bottom left facet) also need to be wrapped-round
   to the bottom left corner. */
            } else if( *xt_out >= 4*skytiling->ntpf &&
                        *yt_out >= 4*skytiling->ntpf ) {
               *xt_out -= 4*skytiling->ntpf;
               *yt_out -= 4*skytiling->ntpf;
            }

/* Tiles that are now in the lower left half of the lower left facet
   (i.e. off the edge of the valid part of the projection) need
   to be moved up to fill the half-facet hole left at the top right.
   Only do this if requested. */
            if( !raw && *yt_out < skytiling->ntpf - 1 &&
                        *xt_out < skytiling->ntpf - 1 - *yt_out ) {
               *xt_out += 4*skytiling->ntpf;
               *yt_out += 4*skytiling->ntpf;
            }

/* Now do SMF__JSA_HPX projections. Just return the supplied tile offsets. */
         } else if( proj == SMF__JSA_HPX ) {
            *xt_out = xt_hpx;
            *yt_out = yt_hpx;

/* Report an error for an unknown projection. */
         } else if( *status == SAI__OK ){
            *status = SAI__ERROR;
            errRepf( "", "smf_jsatilexyconv: Unsupported projection '%d' "
                     "requested.", status, proj );
         }
      }
   }
}

