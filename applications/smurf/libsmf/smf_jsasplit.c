/*
*+
*  Name:
*     smf_jsasplit

*  Purpose:
*     Split a supplied NDF up into JSA tiles and create a new NDF for each
*     tile.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_jsasplit( int indf, const char *base, int trim,
*                        smf_inst_t instrument, size_t *ntile,
*                        Grp *grp, int *status )

*  Arguments:
*     indf = int (Given)
*        An identifier for the NDF to be split. This may be 2D or 3D. The
*        NDF is assumed to be gridded on the JSA all-sky pixel grid, and
*        the current Frame of the NDF's WCS FrameSet must contain a SkyFrame
*        (but they need not be axes 1 and 2).
*     base = const char * (Given)
*        The base path for the output NDFs. Each tile index, preceeded by
*        and underscore, will be appended to this base name to create the
*        name of the corresponding output NDF.
*     trim = int (Given)
*        If non-zero, the output NDFs are trimmmed to the edges of the
*        supplied NDF. Otherwise, each output NDF covers the full area of
*        the tile, with unused areas filled with bad values.
*     instrument = smf_inst_t
*        The instrument that created the supplied NDF.
*     ntile = * size_t
*        The number of tiles created.
*     grp = * Grp
*        Pointer to a Grp group. The names of all the tile NDFs created
*        by function are appended to this group.
*     status = int * (Given)
*        Pointer to the inherited status variable.

*  Description:
*     This function splits the supplied NDF up into spatial tiles, where
*     each tile is determined by the JSA tiling scheme for the specified
*     instrument. The NDF is not re-projected, it is assumed that it is
*     already gridded on the JSA all-sky pixel grid.
*
*     A new output NDF is generated for each tile touched by the supplied
*     NDF.
*
*     The zero-based indicies of the created tiles are written to an output
*     paramater called "JSATILELIST".

*  Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     7-NOV-2013 (DSB):
*        Initial version.
*     11-NOV-2013 (DSB):
*        Display tile indicies and write them to an output parameter.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council.
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
#include "par.h"
#include "mers.h"
#include "ndf.h"
#include "star/grp.h"
#include "star/atl.h"
#include "kpg_err.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/jsatiles.h"


void smf_jsasplit( int indf, const char *base, int trim,
                   smf_inst_t instrument, size_t *ntile, Grp *grp,
                   int *status ){

/* Local Variables: */
   AstBox *box;
   AstFitsChan *fc;
   AstFrameSet *tile_wcs;
   AstFrameSet *iwcs;
   AstRegion *region;
   Grp *grpt = NULL;
   char *path;
   char type[ NDF__SZTYP + 1 ];
   double *ipd;
   double dlbnd[3];
   double dubnd[3];
   double gcen[3];
   float *ipf;
   int *created_tiles = NULL;
   int *tiles;
   int axes[3];
   int dims[ 3 ];
   int i;
   int indfo;
   int indfs;
   int indfx;
   int isempty;
   int itile;
   int junk;
   int latax;
   int lbnd[3];
   int lbnd_lon;
   int lbnd_lat;
   int lbndx[ NDF__MXDIM ];
   int lonax;
   int nbase;
   int ndim;
   int ndimx;
   int nsig;
   int ntiles;
   int place;
   int tile_ubnd[2];
   int tile_lbnd[2];
   int tile_index;
   int ubnd[3];
   int ubnd_lon;
   int ubnd_lat;
   int ubndx[ NDF__MXDIM ];
   size_t iel;
   size_t iext;
   size_t nel;
   size_t size;
   smfJSATiling tiling;

/* Initialise */
   *ntile = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an AST context. */
   astBegin;

/* Note the used length of the supplied base string. If it ends with
   ".sdf", reduce it by 4. */
   nbase = astChrLen( base );
   if( !strcmp( base + nbase - 4, ".sdf" ) ) nbase -= 4;

/* Allocate a buffer large enough to hold the full path for an output NDF. */
   path = astMalloc( nbase + 25 );

/* Get the WCS from the NDF. */
   kpg1Gtwcs( indf, &iwcs, status );

/* Get the bounds of the NDF in pixel indices and the the corresponding
   double precision GRID bounds. Also store the GRID coords of the centre.
   Also count the number of significant pixel axes. */
   ndfBound( indf, 3, lbnd, ubnd, &ndim, status );
   nsig = 0;
   for( i = 0; i < ndim; i++ ) {
      dlbnd[ i ] = 0.5;
      dubnd[ i ] = ubnd[ i ] - lbnd[ i ]  + 1.5;
      gcen[ i ] = 0.5*( dlbnd[ i ] + dubnd[ i ] );
      if( ubnd[ i ] > lbnd[ i ] ) nsig++;
   }

/* Create a Box describing the region covered by the NDF pixel grid in
   GRID coords. */
   box = astBox( astGetFrame( iwcs, AST__BASE ), 1, dlbnd, dubnd,
                 AST__NULL, " " );

/* Map this Box into the current WCS Frame of the NDF. */
   region = astMapRegion( box, iwcs, iwcs );

/* If no instrument was specified, we will determine the instrument from
   the contexts of the FITS extension. Copy the NDF FITS extension to a
   FitsChan. Anull the error if the NDF no FITS extension. */
   if( instrument == SMF__INST_NONE && *status == SAI__OK ) {
      kpgGtfts( indf, &fc, status );
      if( *status == KPG__NOFTS ) {
         errAnnul( status );
         fc = NULL;
      }
   } else {
      fc = NULL;
   }

/* Get the parameters of the required tiling scheme. */
   smf_jsainstrument( NULL, fc, instrument, &tiling, status );

/* Get a list of the JSA tiles touched by the supplied NDF. This returns
   the indicies of th elongitude and latitude axes within the current Frame
   of the NDF. */
   tiles = smf_jsatiles_region( region, &tiling, &ntiles, status );

/* Initialise to avoid compiler warnings. */
   lbnd_lon = 0;
   lbnd_lat = 0;
   ubnd_lon = 0;
   ubnd_lat = 0;
   lonax = -1;
   latax = -1;

/* Tell the user what is happening. */
   msgBlank( status );
   msgOutf( "", "Splitting %s up into JSA tiles:", status,
            ( nsig == 2 ) ? "map" : "cube" );

/* Loop round all tiles. */
   for( itile = 0; itile < ntiles && *status == SAI__OK; itile++ ) {
      tile_index = tiles[ itile ];

/* Get the pixel bounds of the current tile within the JSA all-sky pixel
   grid. Also get the (2D) WCS FrameSet for the tile. */
      smf_jsatile( tile_index, &tiling, 0, NULL, &tile_wcs, NULL,
                   tile_lbnd, tile_ubnd, status );

/* If this is the first tile, we find the indicies of the pixel axes in
   the supplied NDF that are most closely aligned with RA and Dec axes in
   the tile (pixel axes 1 and 2 in the tile). */
      if( itile == 0 ) {
         atlPairAxes( iwcs, tile_wcs, gcen, "SKY", axes, status );

         for( i = 0; i < ndim; i++ ) {
            if( axes[ i ] == 1 ) {
               lonax = i;
            } else if( axes[ i ] == 2 ) {
               latax = i;
            }
         }

         if( lonax == -1 || latax == -1 ) {
            if( *status == SAI__OK ) {
               *status = SAI__ERROR;
               ndfMsg( "N", indf );
               errRep( "", "smf_jsasplit: Cannot find the RA and Dec axes "
                       "in '^N'.", status );
            }
            break;

/* Note the bounds of the supplied NDF on the spatial axes. */
         } else {
            lbnd_lon = lbnd[ lonax ];
            lbnd_lat = lbnd[ latax ];
            ubnd_lon = ubnd[ lonax ];
            ubnd_lat = ubnd[ latax ];
         }
      }

/* The supplied NDF is assumed to be gridded on the same grid, so we can
   just use the tile bounds as the pixel bounds of the required section of
   the NDF. */
      lbnd[ lonax ] = tile_lbnd[ 0 ];
      lbnd[ latax ] = tile_lbnd[ 1 ];
      ubnd[ lonax ] = tile_ubnd[ 0 ];
      ubnd[ latax ] = tile_ubnd[ 1 ];

/* Trim to the edges of the supplied NDF if required. */
      if( trim ) {
         if( lbnd[ lonax ] < lbnd_lon ) lbnd[ lonax ] = lbnd_lon;
         if( ubnd[ lonax ] > ubnd_lon ) ubnd[ lonax ] = ubnd_lon;
         if( lbnd[ latax ] < lbnd_lat ) lbnd[ latax ] = lbnd_lat;
         if( ubnd[ latax ] > ubnd_lat ) ubnd[ latax ] = ubnd_lat;
      }

/* Get an identifier for the required section of the input NDF. */
      ndfSect( indf, ndim, lbnd, ubnd, &indfs, status );

/* We need to check it contains some good data, so map the Data array.
   Get the number of elements using ndfDim since the "nel" argument of
   ndfMap (the product of all dimensions) is only an int. */
      ndfDim( indfs, 3, dims, &ndim, status );
      nel = dims[ 0 ];
      nel *= dims[ 1 ];
      nel *= dims[ 2 ];

      isempty = 1;
      ndfType( indfs, "Data", type, sizeof( type ), status );
      if( !strcmp( type, "_REAL" ) ) {
         ndfMap( indfs, "Data", "_REAL", "Read", (void **) &ipf, &junk,
                 status );
         if( *status == SAI__OK ) {
            for( iel = 0; iel < nel; iel++ ) {
               if( *(ipf++) != VAL__BADR ) {
                  isempty = 0;
                  break;
               }
            }
         }
      } else {
         ndfMap( indfs, "Data", "_DOUBLE", "Read", (void **) &ipd, &junk,
                 status );
         if( *status == SAI__OK ) {
            for( iel = 0; iel < nel; iel++ ) {
               if( *(ipd++) != VAL__BADD ) {
                  isempty = 0;
                  break;
               }
            }
         }
      }
      ndfUnmap( indfs, "*", status );

/* Skip empty tiles. */
      if( !isempty ) {
         msgOutf( "", "   tile %d", status, tile_index );

/* Record the index of this tile in the list of created tiles. */
         (*ntile)++;
         created_tiles = astGrow( created_tiles, *ntile,
                                  sizeof( *created_tiles ));
         if( astOK ) {
            created_tiles[ *ntile - 1] = tile_index;

/* Get the full path to the output NDF for the current tile, and create an
   NDF placeholder for it. */
            sprintf( path, "%.*s_%d", nbase, base, tile_index );
            ndfPlace( NULL, path, &place, status );

/* Copy the section of the input NDF to the output NDF. */
            ndfCopy( indfs, &place, &indfo, status );

/* Add the name of this output NDF to the group holding the names of the
   output NDFs that have actually been created. */
            grpPut1( grp, path, 0, status );

/* Add a JSATILE header to the output FITS extension. */
            kpgGtfts( indfo, &fc, status );
            if( *status == KPG__NOFTS ) {
               errAnnul( status );
               fc = astFitsChan( NULL, NULL, " " );
            }
            atlPtfti( fc, "JSATILE", tile_index, "JSA all-sky tile index",
                      status );
            kpgPtfts( indfo, fc, status );
            fc = astAnnul( fc );

/* We now reshape any extension NDFs contained within the output NDF to
   have the same spatial bounds as the main NDF (but only for extension
   NDFs that originally have the same spatial bounds as the supplied NDF).
   Get a group containing paths to all extension NDFs in the output NDF. */
            ndgMoreg( indfo, &grpt, &size, status );

/* Loop round each output extension NDF. */
            for( iext = 1; iext <= size; iext++ ) {
               ndgNdfas( grpt, iext, "Update", &indfx, status );

/* Get its bounds. */
               ndfBound( indfx, NDF__MXDIM, lbndx, ubndx, &ndimx, status );

/* See if this extension NDF has the same bounds on the spatial axes as
   the supplied NDF. */
               if( ndimx > 1 && lbndx[ lonax ] == lbnd_lon &&
                   lbndx[ latax ] == lbnd_lat &&
                   ubndx[ lonax ] == ubnd_lon &&
                   ubndx[ latax ] == ubnd_lat ) {

/* If so, set the bounds of the output extension NDF so that they are
   the same as the tile on the spatial axes. */
                  lbndx[ lonax ] = tile_lbnd[ 0 ];
                  lbndx[ latax ] = tile_lbnd[ 1 ];
                  ubndx[ lonax ] = tile_ubnd[ 0 ];
                  ubndx[ latax ] = tile_ubnd[ 1 ];
                  ndfSbnd( ndimx, lbndx, ubndx, indfx, status );
               }

/* Annul the extension NDF identifier. */
               ndfAnnul( &indfx, status );
            }

/* Free resources associated with the current tile. */
            grpDelet( &grpt, status );
            ndfAnnul( &indfo, status );
         }

      } else {
         msgOutiff( MSG__VERB, "", "   tile %d is empty and so will not be "
                    "created", status, tile_index );
      }
      ndfAnnul( &indfs, status );
      tile_wcs = astAnnul( tile_wcs );
   }
   msgBlank( status );

/* Write the indicies of the created tiles out to a parameter. */
   parPut1i( "JSATILELIST", *ntile, created_tiles, status );

/* Free resources. */
   created_tiles = astFree( created_tiles );
   tiles = astFree( tiles );
   path = astFree( path );

/* End the AST context. */
   astEnd;

}



