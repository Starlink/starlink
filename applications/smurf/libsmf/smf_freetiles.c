/*
*+
*  Name:
*     smf_freetiles

*  Purpose:
*     Frees an array of tile description structures.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     tiles = smf_freetiles( smfTile *tiles, int size, int *status )

*  Arguments:
*     tiles = smfTile * (Given)
*        Pointer to the first tile in the array to be freed.
*     size = int (Given)
*        Number of tiles in the array.
*     status = int * (Given and Returned)
*        Inherited status value. This function attempts to execute even
*        if status is set to an error value on entry.

*  Returned Value:
*     A NULL pointer is always returned.

*  Description:
*     This function frees the resources used by an array of smfTile
*     structures. Each of these structures holds a description of an
*     output tile created by makecube.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     4-SEP-2007 (DSB):
*        Initial version.
*     11-OCT-2007 (DSB):
*        Free "jndf" component of smfTile structure.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"

smfTile *smf_freetiles( smfTile *tiles, int size, int *status ){

/* Local Variables */
   int itile;
   smfTile *tile;

/* Return if no structures were supplied. */
   if( !tiles || size == 0 ) return NULL;

/* Loop round the tile structures. */
   tile = tiles;
   for( itile = 0; itile < size; itile++, tile++ ){

/* Free the resources used by this tile. */
      if( tile->map2d ) tile->map2d = astAnnul( tile->map2d );
      if( tile->map3d ) tile->map3d = astAnnul( tile->map3d );
      if( tile->grp ) grpDelet( &(tile->grp), status );
      if( tile->jndf ) tile->jndf = astFree( tile->jndf );
   }

/* Free the memory used to hold the structures. */
   return astFree( tiles );
}
