/*
*+
*  Name:
*     smf_jsadicer

*  Purpose:
*     Dice a supplied NDF up into JSA tiles and create a new NDF for each
*     tile.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_jsadicer( int indf, const char *base, int trim,
*                        smf_inst_t instrument, size_t *ntile,
*                        Grp *grp, int *status )

*  Arguments:
*     indf = int (Given)
*        An identifier for the NDF to be diced. This may be 2D or 3D. The
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
*        - Display tile indicies and write them to an output parameter.
*        - Re-structured to test for bad values in the supplied NDF
*          rather than the tiles (testing in tiles is a bigger job).
*     17-JAN-2014 (DSB):
*        - Report an error if the NDFs projection is not HEALPix.
*        - Renamed from smf_jsasplit to smf_jsadicer.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2013-2014 Science & Technology Facilities Council.
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
#include "star/one.h"
#include "kpg_err.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/jsatiles.h"


void smf_jsadicer( int indf, const char *base, int trim,
                   smf_inst_t instrument, size_t *ntile, Grp *grp,
                   int *status ){

/* Local Variables: */
   AstBox *box;
   AstFitsChan *fc;
   AstFrameSet *iwcs;
   AstFrameSet *tile_wcs;
   AstRegion *region;
   Grp *grpt = NULL;
   char *path;
   char type[ NDF__SZTYP + 1 ];
   char jsatile_comment[45];
   double *ipd;
   double *pxd;
   double *pyd;
   double *pzd;
   double dlbnd[3];
   double dubnd[3];
   double gcen[3];
   float *ipf;
   float *pxf;
   float *pyf;
   float *pzf;
   int *created_tiles = NULL;
   int *tiles;
   int axes[3];
   int i;
   int indfo;
   int indfs;
   int indfx;
   int isempty;
   int itile;
   int ix;
   int iy;
   int iz;
   int junk;
   int latax;
   int lbnd[3];
   int lbnd_lat;
   int lbnd_lon;
   int lbndx[ NDF__MXDIM ];
   int lonax;
   int nbase;
   int ndim;
   int ndimx;
   int nsig;
   int ntiles;
   int olbnd[ 3 ];
   int oubnd[ 3 ];
   int place;
   int tile_index;
   int tile_lbnd[2];
   int tile_ubnd[2];
   int ubnd[3];
   int ubnd_lat;
   int ubnd_lon;
   int ubndx[ NDF__MXDIM ];
   size_t iext;
   size_t size;
   size_t ystride;
   size_t zstride;
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

/* Report an error if the NDFs projection is not "HEALPix". */
   if( !astChrMatch( astGetC( iwcs, "Projection" ), "HEALPix" ) &&
       *status == SAI__OK ) {
      ndfMsg( "N", indf );
      *status = SAI__ERROR;
      errRep( "", "The input NDF (^N) does not appear to be gridded "
              "on the JSA all-sky pixel grid.", status );
   }

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
   if( ntiles == 0 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "", "smf_jsadicer: No JSA tiles found touching supplied NDF "
              "(programming error).", status );
   }

/* Initialise to avoid compiler warnings. */
   lbnd_lon = 0;
   lbnd_lat = 0;
   ubnd_lon = 0;
   ubnd_lat = 0;
   lonax = -1;
   latax = -1;

/* Get the stride (in elements) between adjacent pixels on the 2nd and 32rd
   pixel axes. The stride on the 1st pixel axis is always 1. */
   ystride = ubnd[ 0 ] - lbnd[ 0 ] + 1;
   zstride = ystride*( ubnd[ 1 ] - lbnd[ 1 ] + 1 );

/* First we need to decide which of these tiles to create. Tiles which
   recieve only bad values are not created. Map the whole supplied NDF
   so that we can check which tiles receive any good values. */
   ndfType( indf, "Data", type, sizeof( type ), status );
   if( !strcmp( type, "_REAL" ) ) {
      ndfMap( indf, "Data", "_REAL", "Read", (void **) &ipf, &junk,
              status );
   } else {
      ndfMap( indf, "Data", "_DOUBLE", "Read", (void **) &ipd, &junk,
              status );
   }

/* Tell the user what is happening. */
   msgBlank( status );
   msgOutf( "", "Dicing %s into JSA tiles:", status,
            ( nsig == 2 ) ? "map" : "cube" );

/* Loop round all tiles that overlap the supplied NDF. */
   for( itile = 0; itile < ntiles && *status == SAI__OK; itile++ ) {
      tile_index = tiles[ itile ];

/* Get the spatial pixel bounds of the current tile within the JSA all-sky
   pixel grid. Also get the (2D) WCS FrameSet for the tile. */
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
               errRep( "", "smf_jsadicer: Cannot find the RA and Dec axes "
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

/* Get the pixel index bounds of the overlap of the NDF and tile. */
      for( i = 0; i < ndim; i++ ) {
         olbnd[ i ] = lbnd[ i ];
         oubnd[ i ] = ubnd[ i ];
      }
      for( ; i < 3; i++ ) {
         olbnd[ i ] = 1;
         oubnd[ i ] = 1;
      }
      if( tile_lbnd[ 0 ] > olbnd[ lonax ] )  olbnd[ lonax ] = tile_lbnd[ 0 ];
      if( tile_lbnd[ 1 ] > olbnd[ latax ] )  olbnd[ latax ] = tile_lbnd[ 1 ];
      if( tile_ubnd[ 0 ] < oubnd[ lonax ] )  oubnd[ lonax ] = tile_ubnd[ 0 ];
      if( tile_ubnd[ 1 ] < oubnd[ latax ] )  oubnd[ latax ] = tile_ubnd[ 1 ];

/* Loop round all pixels in the overlap, looking for a good pixel value. */
      isempty = 1;
      if( !strcmp( type, "_REAL" ) ) {
         pzf = ipf + ( olbnd[ 2 ] - lbnd[ 2 ] )*zstride;
         for( iz = olbnd[ 2 ]; iz <= oubnd[ 2 ] && isempty; iz++ ) {

            pyf = pzf + ( olbnd[ 1 ] - lbnd[ 1 ] )*ystride;
            for( iy = olbnd[ 1 ]; iy <= oubnd[ 1 ] && isempty; iy++ ) {

               pxf = pyf + ( olbnd[ 0 ] - lbnd[ 0 ] );
               for( ix = olbnd[ 0 ]; ix <= oubnd[ 0 ]; ix++ ) {
                  if( *(pxf++) != VAL__BADR ) {
                     isempty = 0;
                     break;
                  }

               }

               pyf += ystride;
            }

            pzf += zstride;
         }

      } else {
         pzd = ipd + ( olbnd[ 2 ] - lbnd[ 2 ] )*zstride;
         for( iz = olbnd[ 2 ]; iz <= oubnd[ 2 ] && isempty; iz++ ) {

            pyd = pzd + ( olbnd[ 1 ] - lbnd[ 1 ] )*ystride;
            for( iy = olbnd[ 1 ]; iy <= oubnd[ 1 ] && isempty; iy++ ) {

               pxd = pyd + ( olbnd[ 0 ] - lbnd[ 0 ] );
               for( ix = olbnd[ 0 ]; ix <= oubnd[ 0 ]; ix++ ) {
                  if( *(pxd++) != VAL__BADD ) {
                     isempty = 0;
                     break;
                  }

               }

               pyd += ystride;
            }

            pzd += zstride;
         }
      }

/* Issue warnings about any empty tiles. */
      if( isempty ) {
         msgOutiff( MSG__VERB, "", "   tile %d is empty and so will not be "
                    "created", status, tile_index );

/* Otherwise, the tile contains some good values so append the index of
   this tile in the list of tiles to be created. */
      } else {
         created_tiles = astGrow( created_tiles, ++(*ntile),
                                  sizeof( *created_tiles ));
         if( *status == SAI__OK ) created_tiles[ *ntile - 1 ] = tile_index;

      }
      tile_wcs = astAnnul( tile_wcs );
   }

/* We can now unmap the supplied NDF. */
   ndfUnmap( indf, "*", status );

/* Loop round all tiles that contain some good values. */
   for( itile = 0; itile < (int) *ntile && *status == SAI__OK; itile++ ) {
      tile_index = created_tiles[ itile ];
      msgOutf( "", "   tile %d", status, tile_index );

/* Get the pixel bounds of the current tile within the JSA all-sky pixel
   grid. */
      smf_jsatile( tile_index, &tiling, 0, NULL, NULL, NULL,
                   tile_lbnd, tile_ubnd, status );

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

/* Get the full path to the output NDF for the current tile, and create an
   NDF placeholder for it. */
      sprintf( path, "%.*s_%d", nbase, base, tile_index );
      ndfPlace( NULL, path, &place, status );

/* Copy the section of the input NDF to the output NDF. */
      ndfCopy( indfs, &place, &indfo, status );

/* Add the name of this output NDF to the group holding the names of the
   output NDFs that have actually been created. */
      if( grp ) grpPut1( grp, path, 0, status );

/* Add a JSATILE header to the output FITS extension. */
      kpgGtfts( indfo, &fc, status );
      if( *status == KPG__NOFTS ) {
         errAnnul( status );
         fc = astFitsChan( NULL, NULL, " " );
      }
      one_snprintf(jsatile_comment, 45, "JSA all-sky tile index (Nside=%i)",
                   status, tiling.ntpf);
      atlPtfti( fc, "JSATILE", tile_index, jsatile_comment, status );
      kpgPtfts( indfo, fc, status );
      fc = astAnnul( fc );

/* We now reshape any extension NDFs contained within the output NDF to
   have the same spatial bounds as the main NDF (but only for extension
   NDFs that originally have the same spatial bounds as the supplied NDF).
   Get a group containing paths to all extension NDFs in the output NDF. */
      ndgMoreg( indfo, &grpt, &size, status );

/* Loop round each output extension NDF. */
      for( iext = 1; iext <= size && *status == SAI__OK; iext++ ) {
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
   the same as the main NDF on the spatial axes. */
            lbndx[ lonax ] = lbnd[ lonax ];
            lbndx[ latax ] = lbnd[ latax ];
            ubndx[ lonax ] = ubnd[ lonax ];
            ubndx[ latax ] = ubnd[ latax ];
            ndfSbnd( ndimx, lbndx, ubndx, indfx, status );
         }

/* Annul the extension NDF identifier. */
         ndfAnnul( &indfx, status );
      }

/* Free resources associated with the current tile. */
      grpDelet( &grpt, status );
      ndfAnnul( &indfo, status );
      ndfAnnul( &indfs, status );
   }
   msgBlank( status );

/* Write the indicies of the created tiles out to a parameter. */
   if( *ntile ) parPut1i( "JSATILELIST", *ntile, created_tiles, status );

/* Free resources. */
   created_tiles = astFree( created_tiles );
   tiles = astFree( tiles );
   path = astFree( path );

/* End the AST context. */
   astEnd;

}



