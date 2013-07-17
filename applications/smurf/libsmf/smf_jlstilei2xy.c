/*
*+
*  Name:
*     smf_jlstilei2xy

*  Purpose:
*     Convert a scalar sky tile index into the (x,y) indices of the tile.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_jlstilei2xy( int itile, smfJLSTiling *skytiling, int *xt,
*                           int *yt, int *status )

*  Arguments:
*     itile = int (Given)
*        The one-based index of the tile.
*     skytiling = smfJLSTiling * (Given)
*        Pointer to a structure holding parameters describing the tiling
*        scheme used for the required JCMT instrument, as returned by
*        function smf_jlstiling.
*     xt = int * (Returned)
*        Address of the integer in which to store the zero-based index of
*        the tile in the X (RA) direction.
*     yt = int * (Returned)
*        Address of the integer in which to store the zero-based index of
*        the tile in the Y (Dec) direction.
*     status = int * (Given)
*        Pointer to the inherited status variable.

*  Description:
*     This function returns offsets along the X (RA) and Y (Dec) axes
*     (measured as a number of tiles) from the bottom left tile to the
*     tile with the specified one-based tile index.

*  Authors:
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     12-APR-2011 (DSB):
*        Initial version.
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
#include "mers.h"

/* SMURF includes */
#include "libsmf/smf.h"




#include "libsmf/jlstiles.h"   /* Move this to smf_typ.h and smf.h when done */




void smf_jlstilei2xy( int itile, smfJLSTiling *skytiling, int *xt, int *yt,
                      int *status ){

/* Local Variables: */
   int fi;
   int fx;
   int fy;
   int nsq;
   int tj;
   int xj;
   int yj;

/* Initialise the returned values. */
   *xt = VAL__BADI;
   *yt = VAL__BADI;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get the number of tiles per HPX facet */
   nsq = skytiling->ntpf*skytiling->ntpf;

/* Check the tile index is legal. */
   if( itile >= 1 && itile <= 12*nsq ){

/* Get the zero-based index of the facet contained the tile. */
      fi = ( itile - 1 )/nsq;

/* Get the offset in tiles into this facet of the required tile. */
      tj = ( itile - 1 ) - fi*nsq;

/* Within a facet, tiles are stored raster-fashion from bottom left to
   top right. Get the row and column indices (zero-based) of the tile within
   the facet. */
      yj = tj/skytiling->ntpf;
      xj = tj - yj*skytiling->ntpf;

/* Get he offsets, in tiles, along X and Y, from the bottom left tile of
   the first facet to the bottom left tile of the requested facet. */
      fy = ( ( fi + 1 )/3 )*skytiling->ntpf + 1;
      fx = fy + ( ( fi + 1 )%3 - 1 )*skytiling->ntpf;

/* Add the offsets to the facet onto the offsets within the facet to get
   the total offsets. */
      *xt = fx + xj;
      *yt = fy + yj;

/* The facet with the lowest tile indices is split between the bottom
   left and top right corners of the grid. Move tiles from bottom left
   to top right. */
      if( *yt < skytiling->ntpf - 1 && *xt < skytiling->ntpf - 1 - *yt ) {
         *xt += 4*skytiling->ntpf;
         *yt += 4*skytiling->ntpf;
      }

/* Report an error if the supplied tile index was bad. */
   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti( "I", itile );
      errRep( "", "smf_jlstilei2xy: Illegal tile index (^I) supplied "
              "(programming error).", status );
   }

}

