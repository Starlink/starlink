/*
*+
*  Name:
*     smf_jsaproj

*  Purpose:
*     Choose a JSA projection to map a given set of tiles.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_jsaproj_t proj = smf_jsaproj( int ntile, const int *tiles,
*                                        smfJSATiling *skytiling, int *status )

*  Arguments:
*     ntile = int (Given)
*        The number of tiles supplied in "tiles[]".
*     tiles = const int * (Given)
*        A list of tile indices.
*     skytiling = smfJSATiling * (Given)
*        Pointer to a structure holding parameters describing the tiling
*        scheme used for the required JCMT instrument, as returned by
*        function smf_jsatiling.
*     status = int * (Given)
*        Pointer to the inherited status variable.

*  Returned Value:
*     The type of JSA projection to use.

*  Description:
*     This function determines the type of JSA projection to use for a
*     map that coveres a given set of tiles. The projection that produces
*     the smallest map is chosenb.

*  Authors:
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     1-OCT-2014 (DSB):
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
#include "ast.h"
#include "mers.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/jsatiles.h"

smf_jsaproj_t smf_jsaproj( int ntile, const int *tiles,
                           smfJSATiling *skytiling, int *status ){

/* Local Variables: */
   smf_jsaproj_t result;
   smf_jsaproj_t proj[ 4 ] = { SMF__JSA_HPX, SMF__JSA_HPX12,
                               SMF__JSA_XPHN, SMF__JSA_XPHS };
   int iproj;
   int itile;
   int lbnd[2];
   int ubnd[2];
   int box_lbnd[2];
   int box_ubnd[2];
   size_t box_nel;
   size_t minnel = 0;

/* Initialise the returned value. */
   result = SMF__JSA_HPX;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Try all four JSA projections. */
   for( iproj = 0; iproj < 4; iproj++ ) {

/* Initialise the bounding box. */
      box_lbnd[ 0 ] = INT_MAX;
      box_lbnd[ 1 ] = INT_MAX;
      box_ubnd[ 0 ] = -INT_MAX;
      box_ubnd[ 1 ] = -INT_MAX;

/* Loop over all the supplied tiles. */
      for( itile = 0; itile < ntile; itile++ ) {

/* Get the bounds of the current tile using the current projection. */
         smf_jsatile( tiles[ itile ], skytiling, 0, proj[ iproj ],
                      NULL, NULL, NULL, lbnd, ubnd, status );

/* Extend the bounding box to include the current tile. */
         if( lbnd[ 0 ] < box_lbnd[ 0 ] ) box_lbnd[ 0 ] = lbnd[ 0 ];
         if( lbnd[ 1 ] < box_lbnd[ 1 ] ) box_lbnd[ 1 ] = lbnd[ 1 ];
         if( ubnd[ 0 ] > box_ubnd[ 0 ] ) box_ubnd[ 0 ] = ubnd[ 0 ];
         if( ubnd[ 1 ] > box_ubnd[ 1 ] ) box_ubnd[ 1 ] = ubnd[ 1 ];
      }

/* Get the number of pixels in the bounding box. */
      box_nel = ( box_ubnd[ 0 ] - box_lbnd[ 0 ] + 1 )*( box_ubnd[ 1 ] - box_lbnd[ 1 ] + 1 );

/* If this is smaller than all previous projections, record the current
   projection as the best projection. */
      if( iproj == 0 || box_nel < minnel ) {
         minnel = box_nel;
         result = proj[ iproj ];
      }
   }

   return result;
}



