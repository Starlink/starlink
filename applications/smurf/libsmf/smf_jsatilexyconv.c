/*
*+
*  Name:
*     smf_jsatilexyconv

*  Purpose:
*     Convert (x,y) indices of a tile from HPX to XPH

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_jsatilexyconv( smfJSATiling *skytiling, int usexph,
*                             int xt_hpx, int yt_hpx, int *xt_out,
*                             int *yt_out, int *status )

*  Arguments:
*     skytiling = smfJSATiling * (Given)
*        Pointer to a structure holding parameters describing the tiling
*        scheme used for the required JCMT instrument, as returned by
*        function smf_jsatiling.
*     usexph = int (Given)
*        Indicates the projection into which the supplied HPX tile indices
*        should be converted: negative means north pole XPH, 0 means HPX
*        (i.e. no change), positive means south pole XPH.
*     xt_hpx = int (Giveb)
*        The zero-based index of a tile in the X (RA) direction, within
*        the HPX projection.
*     yt_hpx = int (given)
*        The zero-based index of the tile in the Y (Dec) direction, within
*        the HPX projection.
*     xt_out = int * (Returned)
*        Address of the integer in which to store the zero-based index
*        of the tile in the X (RA) direction, within the projection
*        requested by "usexph". This can point to "xt_hpx" if required.
*     yt_out = int * (Returned)
*        Address of the integer in which to store the zero-based index
*        of the tile in the Y (Dec) direction, within the projection
*        requested by "usexph". This can point to "yt_hpx" if required.
*     status = int * (Given)
*        Pointer to the inherited status variable.

*  Description:
*     This function takes the (x,y) indices of a tile in the HPX
*     projection, and returns the indices of the same tile within
*     the projection specified by "usexph". The indices of a tile in any
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

void smf_jsatilexyconv( smfJSATiling *skytiling, int usexph, int xt_hpx,
                        int yt_hpx, int *xt_out, int *yt_out, int *status ){

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

/* Find the (x,y) offsets (in facets) of the facet containing the tile. */
   xf = xt_hpx / skytiling->ntpf;
   yf = yt_hpx / skytiling->ntpf;

/* Return bad (x,y) values for positions that are not on one of the 12 HPX
   facets. Also reject the half facets at bottom left and top right that
   are not used in JSA all-sky grid. */
   xpy = xt_hpx + yt_hpx;
   if( yf <= xf + 1 && yf >= xf - 1 &&
       xpy >= skytiling->ntpf - 1 && xpy < 9*skytiling->ntpf - 1) {

/* First do XPH projections. */
      if( usexph ) {

/* Find the zero-based index of the gore containing the tile. */
         igore = ( xpy - skytiling->ntpf + 1 )/( 2*skytiling->ntpf );

/* Find the (x,y) offsets of the tile away from the bottom left corner of
   the square enclosing the gore within the HPX projection. */
         off = igore*skytiling->ntpf;
         dx = xt_hpx - off;
         dy = yt_hpx - off;

/* Find the rotation and translation necessary to transform the gore from
   HPX into XPH. */
         if( usexph > 0 ) {
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
         if( rot == 0 ) {            /* No rotation */
            *xt_out =  dx;
            *yt_out =  dy;
         } else if( rot == 1 ) {     /* 90 degs ACW rotation */
            *xt_out =  2*skytiling->ntpf - 1 - dy;
            *yt_out =  dx;
         } else if( rot == 2 ) {     /* 180 degs ACW rotation */
            *xt_out =  2*skytiling->ntpf - 1 - dx;
            *yt_out =  2*skytiling->ntpf - 1 - dy;
         } else {                    /* 270 degs ACW rotation */
            *xt_out =  dy;
            *yt_out =  2*skytiling->ntpf - 1 - dx;
         }

/* Add on the HPX offsets to the lower left corner of the bounding box. The
   results of this are the offsets of the tile if you were just to rotate
   the gore about its centre without translating it. */
         *xt_out +=  off;
         *yt_out +=  off;

/* Add on the translation that moves the (now rotated) gore from its
   position with HPX projection to its position within the XPH
   projection. */
         *xt_out += xtr*skytiling->ntpf;
         *yt_out += ytr*skytiling->ntpf;

/* Now do HPX projections. Just return the supplied HPX tile offsets. */
      } else {
         *xt_out = xt_hpx;
         *yt_out = yt_hpx;
      }
   }
}

