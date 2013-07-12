/*
*+
*  Name:
*     smf_skytiles_region.

*  Purpose:
*     Find the sky tiles that overlap a given AST Region.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int *smf_skytiles_region( AstRegion *region, smf_inst_t instrument,
*                               int *ntile, int *status );

*  Arguments:
*     region = AstRegion * (Given)
*        The Region.
*     instrument = smf_inst_t (Given)
*        An identifier for the JCMT instrument.
*     ntile = int * (Returned)
*        The number of tiles in the returned list.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     A pointer to a newly allocated array of ints, each being the
*     identifier of a tile that overlaps the given Region. The array
*     should be freed using astFree when no longer needed.

*  Description:
*     This routine returns a list containing the indices of the sky tiles
*     (for a named JCMT instrument) that receive data from a given AST Region.

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     12-JUL-2013 (DSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2013 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/


/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/tiles.h"


int *smf_skytiles_region( AstRegion *region, smf_inst_t instrument,
                          int *ntile, int *status ){

/* Local Variables */
   AstFrameSet *fs;
   AstKeyMap *km;
   AstRegion *region2;
   AstRegion *tregion;
   char text[ 200 ];
   const char *key;
   double *mesh = NULL;
   double *xmesh;
   double *ymesh;
   int *tiles = NULL;
   int i;
   int ineb;
   int itile2;
   int itile;
   int ix;
   int iy;
   int key_index;
   int mapsize;
   int npoint;
   int overlap;
   int ubnd[ 2 ];
   int value;
   int xoff[ 4 ] = { -1, 0, 1, 0 };
   int xt;
   int yoff[ 4 ] = { 0, 1, 0, -1 };
   int yt;
   smfSkyTiling skytiling;

/* Initialise */
   *ntile = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return tiles;

/* Start an AST context so that all AST objects created in this function
   are annulled automatically. */
   astBegin;

/* Get the parameters that define the layout of sky tiles for this
   instrument. */
   smf_skytiling( instrument, &skytiling, status );

/* Create a FrameSet describing the whole sky in which each pixel
   corresponds to a single tile. The current Frame is ICRS (RA,Dec) and
   the base Frame is gird coords in which each grid pixel corresponds to
   a single tile. */
   smf_skytile( 0, &skytiling, NULL, &fs, NULL, ubnd, status );

/* Map the Region using the FrameSet obtained above so that the new Region
   describes offsets in tiles from the lower left tile. */
   astInvert( fs );
   fs = astConvert( region, fs, "SKY" );
   if( !fs && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "", "Supplied Region is not a SkyFrame.", status );
      goto L999;
   }
   region2 = astMapRegion( region, fs, fs );

/* Get a mesh of all-sky "grid" positions (actually tile X and Y indices)
   covering the region */
   astGetRegionMesh( region2, 0, 0, 2, &npoint, NULL );
   mesh = astMalloc( 2*npoint*sizeof( *mesh ) );
   astGetRegionMesh( region2, 0, npoint, 2, &npoint, mesh );

/* Find the index of the tile containing each position, and store them
   in a KeyMap using the tile index as the key and "1" (indicating the
   tile overlaps the region) as the value. The KeyMap is sorted by age
   of entry. Each entry has a key which is an integer tile index, and
   an integer value. If the value is zero, it means the tile does not
   overlap the supplied Region. If the value is positive, it means the
   tile does overlap the supplied Region. If the value is negative, it
   means the tile has not yet been tested to see if it overlaps the
   supplied Region. */
   km = astKeyMap( "SortBy=AgeDown" );
   xmesh = mesh;
   ymesh = mesh + npoint;
   for( i = 0; i < npoint && *status == SAI__OK; i++ ) {
      ix = (int)( *(xmesh++) + 0.5 );
      iy = (int)( *(ymesh++) + 0.5 );
      itile = smf_skytilexy2i( ix, iy, &skytiling, status );
      sprintf( text, "%d", itile );
      astMapPut0I( km, text, 1, NULL );
   }

/* Starting with the oldest entry in the KeyMap, loop round checking all
   entries, in the order they were added, until all have been checked.
   Checking an entry may cause further entries to be added to the end of
   the KeyMap. */
   key_index = 0;
   mapsize = astMapSize( km );
   while( key_index < mapsize && *status == SAI__OK ) {
      key = astMapKey( km, key_index++ );

/* Convert the key string to an integer tile index. */
      itile = atoi( key );

/* Get the integer value associated with the tile. */
      astMapGet0I( km, key, &value );

/* If the tile associated with the current KeyMap entry has not yet been
   tested for overlap with the requested Region (as shown by the entry
   value being -1), test it now. */
      if( value == -1 ) {

/* Get a Region covering the tile. */
         smf_skytile( itile, &skytiling, NULL, NULL, &tregion, ubnd, status );

/* See if this Region overlaps the user supplied region. Set the value of
   the KeyMap entry to +1 or -1 accordingly. */
         overlap = astOverlap( tregion, region );
         if( overlap == 0 ) {
            if( *status == SAI__OK ) {
               *status = SAI__ERROR;
               errRep( "", "Cannot align supplied Region with the sky "
                       "tile coordinate system (programming error).",
                       status );
            }
         } else if( overlap == 1 || overlap == 6 ) {
            value = 0;
         } else {
            value = 1;
         }
         astMapPut0I( km, key, value, NULL );
      }

/* Skip the current KeyMap entry if the corresponding tile does not
   overlap the requested Region (as shown by the entry value being zero). */
      if( value == 1 ) {

/* The current tile overlaps the supplied Region, so add the tile index to
   the returned list of tile indices. */
         tiles = astGrow( tiles, ++(*ntile), sizeof( *tiles ) );
         if( *status == SAI__OK ) {
            tiles[ *ntile - 1 ] = itile;

/* Add the adjoining tiles to the end of the KeyMap so that they will be
   tested in their turn, giving them a value of -1 to indicate that they
   have not yet been tested to see if they overlap the supplied Region.
   Ignore adjoining tiles that are already in the keyMap. */
            smf_skytilei2xy( itile, &skytiling, &xt, &yt, status );
            for( ineb = 0; ineb < 4; ineb++ ) {
               itile2 = smf_skytilexy2i( xt + xoff[ ineb ], yt + yoff[ ineb ],
                                         &skytiling, status );
               if( itile2 != VAL__BADI ) {
                  sprintf( text, "%d", itile2 );
                  if( !astMapHasKey( km, text ) ) {
                     astMapPut0I( km, text, -1, NULL );
                     mapsize++;
                  }
               }
            }
         }
      }
   }

/* Arrive here if an error occurs. */
   L999:;

/* Free resources. */
   mesh = astFree( mesh );

   if( *status != SAI__OK ) {
      tiles = astFree( tiles );
      *ntile = 0;
   }

   astEnd;

   return tiles;
}



