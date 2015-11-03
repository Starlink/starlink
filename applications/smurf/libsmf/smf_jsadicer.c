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
*                        smf_inst_t instrument, smf_jsaproj_t proj,
*                        size_t *ntile, Grp *grp, int *status )

*  Arguments:
*     indf = int (Given)
*        An identifier for the NDF to be diced. This may be 2D or 3D. The
*        NDF is assumed to be gridded on one of the supported JSA all-sky
*        pixel grids (some flavour of HPX or XPH projection), and the
*        current Frame of the NDF's WCS FrameSet must contain an ICRS
*        SkyFrame (but they need not be axes 1 and 2).
*     base = const char * (Given)
*        The base path for the output NDFs. Each tile index, preceeded by
*        and underscore, will be appended to this base name to create the
*        name of the corresponding output NDF.
*     trim = int (Given)
*        A zero or negative value results in each output NDF covering the
*        full area of the corresponding JSA tile. A value of one results in
*        each output NDF being cropped to the bounds of the supplied NDF. A
*        value of two or more results in each output NDF being cropped to
*        remove any blank borders.
*     instrument = smf_inst_t (Given)
*        The instrument that created the supplied NDF.
*     proj = smf_jsaproj_t (Given)
*        Specified the projection to use for the created tiles. Should
*        always be SMF__JSA_HPX, except for debugging or experiments.
*     ntile = * size_t (Returned)
*        The number of tiles created.
*     grp = * Grp (Returned)
*        Pointer to a Grp group. The names of all the tile NDFs created
*        by function are appended to this group.
*     status = int * (Given)
*        Pointer to the inherited status variable.

*  Description:
*     This function splits the supplied NDF up into spatial tiles, where
*     each tile is determined by the JSA tiling scheme for the specified
*     instrument. It is assumed that the NDF is already gridded on one of
*     the JSA all-sky pixel grids (either HPX or XPH).
*
*     A new output NDF is generated for each tile touched by the supplied
*     NDF. Each output NDF uses the projection requested by "proj". An
*     STC-S polygon is created describing the spatial outline of the good
*     data values in the NDF, and stored in NDF extension OUTLINE.
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
*     30-JAN-2014 (DSB):
*        Changed to allow output NDFs to be trimmed of any bad borders.
*     20-FEB-2014 (DSB):
*        Changed to add a bounding STC-S polygon to each tile NDF. The
*        polygon is stored in extension "OUTLINE" of each NDF.
*     25-FEB-2014 (DSB):
*        The polygon stored in the OUTLINE extension of each NDF is
*        now a convex hull rather than a true outline. This is better
*        for ACSIS data which can have muiple blobs of good data.
*     19-JUN-2014 (DSB):
*        Major changes to allow the input NDF to be in XPH projection.
*     25-JUN-2014 (DSB):
*        Added argument "usexph" to allow the output projection to be
*        chosen.
*     7-JUL-2014 (DSB):
*        Ensure the output NDFs inherited the Epoch value of the input NDF.
*     24-JUL-2014 (DSB):
*        DSBSPECTRUM spectral axes are now handled, in addition to
*        SPECTRUM axes.
*     22-SEP-2014 (DSB):
*        Disable lon/lat wrap-around in the WcsMap class before finding
*        the bounding box of each tile in the output NDF. This is to
*        avoid tailes that span RA=12h being wrapped round to the far
*        side of the projection, thus producing a huge box.
*     30-SEP-2014 (DSB):
*        Only disable lon/lat wrapping for tiles that span RA=12h. The
*        other tiles in the split HPX facet require lon/lat wrapping to
*        be on, in order to work properly.
*     1-OCT-2014 (DSB):
*        Add support for HPX projections centred on RA=12h.
*     6-OCT-2014 (DSB):
*        The split tile (bottom-left/top-right) will have a different WCS
*        to the other since it uses a different reference point. So we need
*        to get"p2pmap" separately for every tile, rather than just re-using
*        the p2pmap from the first tile.
*     8-OCT-2014 (DSB):
*        Remove lon/lat wrapping as a means of handling tiles that stradle
*        RA=12h. It is no longer needed since the introduction of the HPX12
*        projection.
*     3-NOV-2015 (DSB):
*        Ensure the alignment of the input NDF and the JSA grid is not
*        done using offset coordinates (e.g. planet observations).
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

/* Prototypes for local functions. */
static void smf1_jsadicer( int indfo, int *olbnd, int *oubnd,
                           AstMapping *tile_map, AstFrame *tile_frm,
                           AstMapping *p2pmap, void *ipd, void *ipv,
                           unsigned char *ipq, int *status );

/* Main entry */
void smf_jsadicer( int indf, const char *base, int trim, smf_inst_t instrument,
                   smf_jsaproj_t proj, size_t *ntile, Grp *grp, int *status ){

/* Local Variables: */
   AstBox *box;
   AstFitsChan *fc;
   AstFrame *specfrm = NULL;
   AstFrame *tile_frm = NULL;
   AstFrameSet *iwcs;
   AstFrameSet *tfs = NULL;
   AstFrameSet *tile_wcs;
   AstMapping *ndf_map = NULL;
   AstMapping *p2pmap = NULL;
   AstMapping *specmap = NULL;
   AstMapping *tile_map = NULL;
   AstRegion *region;
   Grp *grpt = NULL;
   char *path;
   char dtype[ NDF__SZFTP + 1 ];
   char jsatile_comment[45];
   char type[ NDF__SZTYP + 1 ];
   const char *dom = NULL;
   const char *keyword;
   const char *latsys = NULL;
   const char *lonsys = NULL;
   double *pd;
   double dlbnd[3];
   double dubnd[3];
   double gcen[3];
   double lbnd_in[3];
   double lbnd_out[3];
   double ubnd_in[3];
   double ubnd_out[3];
   float *pf;
   int *created_tiles = NULL;
   int *tiles;
   int axlat;
   int axlon;
   int axspec;
   int bbox[ 6 ];
   int i;
   int ifrm;
   int igrid;
   int indfo;
   int indfs;
   int indfx;
   int inperm[3];
   int ipixel;
   int ishpx;
   int isxph;
   int itile;
   int ix;
   int iy;
   int iz;
   int junk;
   int latax = -1;
   int lbnd[3];
   int lbnd_tile[ 3 ];
   int lbndx[ NDF__MXDIM ];
   int lonax = -1;
   int nbase;
   int ndim;
   int ndimx;
   int nfrm;
   int nsig;
   int ntiles;
   int olbnd[ 3 ];
   int oubnd[ 3 ];
   int outperm[ 3 ];
   int place;
   int qual;
   int tile_index;
   int tile_lbnd[2];
   int tile_ubnd[2];
   int ubnd[3];
   int ubnd_tile[ 3 ];
   int ubndx[ NDF__MXDIM ];
   int var;
   size_t iext;
   size_t size;
   smfJSATiling tiling;
   unsigned char *ipq = NULL;
   void *ipd = NULL;
   void *ipv = NULL;

/* Initialise */
   *ntile = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an AST context. */
   astBegin;

/* Begin an NDF context. */
   ndfBegin();

/* Note the used length of the supplied base string. If it ends with
   ".sdf", reduce it by 4. */
   nbase = astChrLen( base );
   if( !strcmp( base + nbase - 4, ".sdf" ) ) nbase -= 4;

/* Allocate a buffer large enough to hold the full path for an output NDF. */
   path = astMalloc( nbase + 25 );

/* Get the WCS from the NDF. */
   kpg1Gtwcs( indf, &iwcs, status );

/* Note if the NDF projection is HPX or XPH. */
   ishpx = astChrMatch( astGetC( iwcs, "Projection" ), "HEALPix" );
   isxph = astChrMatch( astGetC( iwcs, "Projection" ), "polar HEALPix" );

/* Report an error if the NDFs projection is neither of these. */
   if( !ishpx && !isxph && *status == SAI__OK ) {
      ndfMsg( "N", indf );
      *status = SAI__ERROR;
      errRep( "", "The input NDF (^N) does not appear to be gridded "
              "on the JSA all-sky pixel grid.", status );
   }

/* Ensure the current Frame does not represent offset coords. */
   astSetI( iwcs, "AlignOffset", 0 );
   astSetC( iwcs, "SkyRefIs", "Ignored" );

/* Get the bounds of the NDF in pixel indices and the the corresponding
   double precision GRID bounds (reduce the size of the grid by a small
   amount to avoid problems with tiles that are on the edge of the valid sky
   regions - astMapRegion can report an error for such tiles). Also store
   the GRID coords of the centre. Also count the number of significant
   pixel axes. */
   ndfBound( indf, 3, lbnd, ubnd, &ndim, status );
   nsig = 0;
   for( i = 0; i < ndim; i++ ) {
      dlbnd[ i ] = 0.5 + 0.1;
      dubnd[ i ] = ubnd[ i ] - lbnd[ i ]  + 1.5 - 0.1;
      gcen[ i ] = 0.5*( dlbnd[ i ] + dubnd[ i ] );
      if( ubnd[ i ] > lbnd[ i ] ) nsig++;
   }

/* Find the one-based indices of the RA, Dec and spectral axes in the
   current Frame of the NDF. */
   axlon = 0;
   if( astGetI( iwcs, "IsLonAxis(1)" ) ) {
      axlon = 1;
      lonsys = astGetC( iwcs, "System(1)" );
   } else if( astGetI( iwcs, "IsLonAxis(2)" ) ) {
      axlon = 2;
      lonsys = astGetC( iwcs, "System(2)" );
   } else if( ndim == 3 && astGetI( iwcs, "IsLonAxis(3)" ) ) {
      axlon = 3;
      lonsys = astGetC( iwcs, "System(3)" );
   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "", "smf_jsadicer: Cannot find the longitude axis in the "
              "input NDF.", status );
   }

   axlat = 0;
   if( astGetI( iwcs, "IsLatAxis(1)" ) ) {
      axlat = 1;
      latsys = astGetC( iwcs, "System(1)" );
   } else if( astGetI( iwcs, "IsLatAxis(2)" ) ) {
      axlat = 2;
      latsys = astGetC( iwcs, "System(2)" );
   } else if( ndim == 3 && astGetI( iwcs, "IsLatAxis(3)" ) ) {
      axlat = 3;
      latsys = astGetC( iwcs, "System(3)" );
   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "", "smf_jsadicer: Cannot find the latitude axis in the "
              "input NDF.", status );
   }

   axspec = 6 - axlon - axlat;

/* Report an error if the spatial axes are not ICRS RA and Dec. */
   if( ( lonsys && strcmp( lonsys, "ICRS" ) ) ||
       ( latsys && strcmp( latsys, "ICRS" ) ) ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         ndfMsg( "N", indf );
         errRep( "", "smf_jsadicer: The spatial axes in '^N' are not "
                 "ICRS RA and Dec.", status );
      }
   }

/* Create a Box describing the region covered by the NDF pixel grid in
   GRID coords. */
   box = astBox( astGetFrame( iwcs, AST__BASE ), 1, dlbnd, dubnd,
                 AST__NULL, " " );

/* Map this Box into the current WCS Frame of the NDF. */
   region = astMapRegion( box, iwcs, iwcs );

/* If no instrument was specified, we will determine the instrument from
   the contexts of the FITS extension. Copy the NDF FITS extension to a
   FitsChan. Annul the error if the NDF no FITS extension. */
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

/* Get a list of the JSA tiles touched by the supplied NDF. */
   tiles = smf_jsatiles_region( region, &tiling, &ntiles, status );
   if( ntiles == 0 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "", "smf_jsadicer: No JSA tiles found touching supplied NDF "
              "(programming error).", status );
   }

/* Does the input NDF have a Variance component? */
   ndfState( indf, "Variance", &var, status );

/* Does the input NDF have a Quality component? */
   ndfState( indf, "Quality", &qual, status );

/* Decide on the data type to use: _REAL or _DOUBLE. */
   ndfMtype( "_REAL,_DOUBLE", indf, indf, "Data", type, sizeof(type), dtype,
             sizeof(dtype), status );

/* Tell the user what is happening. */
   msgBlank( status );
   msgOutf( "", "Dicing %s into JSA tiles:", status,
            ( nsig == 2 ) ? "map" : "cube" );

/* Loop round all tiles that overlap the supplied NDF. */
   for( itile = 0; itile < ntiles && *status == SAI__OK; itile++ ) {
      tile_index = tiles[ itile ];

/* Get the spatial pixel bounds of the current tile within the requested
   JSA all-sky projection. Also get the (2D) WCS FrameSet for the tile. */
      smf_jsatile( tile_index, &tiling, 0, proj, NULL, &tile_wcs, NULL,
                   tile_lbnd, tile_ubnd, status );

/* Extract the tile pixel->WCS mapping and WCS Frame. We know the indices
   of the required Frames because they are hard-wired in smf_jsatile. */
      tile_map = astGetMapping( tile_wcs, 3, 2 );
      tile_frm = astGetFrame( tile_wcs, 2 );

/* Find the indices of the grid and pixel frames in the input NDF. */
      ipixel = -1;
      igrid = astGetI( iwcs, "Base" );
      nfrm = astGetI( iwcs, "NFrame" );
      for( ifrm = 0; ifrm < nfrm; ifrm++ ) {
         dom = astGetC( astGetFrame( iwcs, ifrm + 1 ), "Domain" );
         if( astChrMatch( dom, "PIXEL" ) ) ipixel = ifrm + 1;
      }

/* If required, extract the pixel->spectral mapping and spectral frame in
   the input NDF, and add it in parallel with the above tile mapping. */
      if( ndim == 3 ) {
         astSetI( iwcs, "Base", ipixel );
         tfs = atlFrameSetSplit( iwcs, "DSBSPECTRUM SPECTRUM", NULL,
                                 NULL, status );
         astSetI( iwcs, "Base", igrid );
         if( tfs ) {
            specmap = astGetMapping( tfs, AST__BASE, AST__CURRENT );
            specfrm = astGetFrame( tfs, AST__CURRENT );
         } else if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            ndfMsg( "N", indf );
            errRep( "", "smf_jsadicer: Cannot find the spectral axis "
                    "in '^N'.", status );
         }

         tile_map = (AstMapping *) astCmpMap( tile_map, specmap, 0, " " );
         tile_frm = (AstFrame *) astCmpFrame( tile_frm, specfrm, " " );
      }

/* Ensure the Epoch is inherited form the input NDF. */
      astSetD( tile_frm, "Epoch", astGetD( iwcs, "Epoch" ) );

/* Currently tile axis 1 is RA, axis 2 is Dec and axis 3 (if present) is
   spectral. Append a PermMap that re-orders these tile WCS axes to match
   those of the NDF. */
      outperm[ axlon - 1 ] = 1;
      outperm[ axlat - 1 ] = 2;
      outperm[ axspec - 1 ] = 3;
      inperm[ 0 ] = axlon;
      inperm[ 1 ] = axlat;
      inperm[ 2 ] = axspec;
      tile_map = (AstMapping *) astCmpMap( tile_map, astPermMap( ndim, inperm,
                                                                 ndim, outperm,
                                                                 NULL, " " ),
                                           1, " " );
      tile_map = astSimplify( tile_map );

/* Also re-order the WCS axes in the tile frame. */
      astPermAxes( tile_frm, outperm );

/* We want the zero-based indicies of the input pixel axes corresponding
   to ra, dec and spectral. So find the indicies of the pixel axes in the
   supplied NDF that are most closely aligned with each WCS axis. */
      atlPairAxes( iwcs, NULL, gcen, NULL, inperm, status );
      if( inperm[ 0 ] == axlon ) {
         lonax = 0;
      } else if( inperm[ 1 ] == axlon ) {
         lonax = 1;
      } else {
         lonax = 2;
      }
      if( inperm[ 0 ] == axlat ) {
         latax = 0;
      } else if( inperm[ 1 ] == axlat ) {
         latax = 1;
      } else {
         latax = 2;
      }

/* To get the mapping from pixel coords in the input NDF to pixel coords
   in the output NDF, we invert the above mapping so that it goes from WCS
   to pixel, and append it to the end of the NDF pixel->WCS mapping. */
      ndf_map = astGetMapping( iwcs, ipixel, AST__CURRENT );
      astInvert( tile_map );
      p2pmap = (AstMapping *) astCmpMap( ndf_map, tile_map, 1, " " );
      p2pmap = astSimplify( p2pmap );
      astInvert( tile_map );

/* Show the bounds of the tile within the input NDF. */
      msgOutiff( MSG__DEBUG, "", "   tile %d has bounds (%d:%d,%d:%d) "
                 "within the output NDF.", status, tile_index,
                 tile_lbnd[ 0 ], tile_ubnd[ 0 ], tile_lbnd[ 1 ],
                 tile_ubnd[ 1 ] );

/* Next job is to find the pixel bounds of the output NDF to create
   which will hold data for the current tile. First map the pixel bounds
   of the whole tile from output to input. */
      lbnd_in[ 0 ] = tile_lbnd[ 0 ] - 0.5;
      lbnd_in[ 1 ] = tile_lbnd[ 1 ] - 0.5;
      lbnd_in[ 2 ] = lbnd[ 2 ] - 0.5;
      ubnd_in[ 0 ] = tile_ubnd[ 0 ] - 0.5;
      ubnd_in[ 1 ] = tile_ubnd[ 1 ] - 0.5;
      ubnd_in[ 2 ] = ubnd[ 2 ] - 0.5;

      astMapBox( p2pmap, lbnd_in, ubnd_in, 0, 1, lbnd_out + 0,
                 ubnd_out + 0, NULL, NULL );
      astMapBox( p2pmap, lbnd_in, ubnd_in, 0, 2, lbnd_out + 1,
                 ubnd_out + 1, NULL, NULL );
      if( ndim == 3 ) astMapBox( p2pmap, lbnd_in, ubnd_in, 0, 3,
                                 lbnd_out + 2, ubnd_out + 2, NULL,
                                 NULL );


      lbnd_tile[ 0 ] = floor( lbnd_out[ 0 ] ) + 1;
      lbnd_tile[ 1 ] = floor( lbnd_out[ 1 ] ) + 1;
      lbnd_tile[ 2 ] = floor( lbnd_out[ 2 ] ) + 1;
      ubnd_tile[ 0 ] = floor( ubnd_out[ 0 ] ) + 1;
      ubnd_tile[ 1 ] = floor( ubnd_out[ 1 ] ) + 1;
      ubnd_tile[ 2 ] = floor( ubnd_out[ 2 ] ) + 1;

/* Show the bounds of the tile within the input NDF. */
      msgOutiff( MSG__DEBUG, "", "   tile %d has bounds (%d:%d,%d:%d) "
                 "within the input NDF.", status, tile_index,
                 lbnd_tile[ 0 ], ubnd_tile[ 0 ], lbnd_tile[ 1 ],
                 ubnd_tile[ 1 ] );

/* If required, trim the bounds to the extent of the input NDF. */
      if( trim ) {
         if( lbnd_tile[ 0 ] < lbnd[ 0 ] ) lbnd_tile[ 0 ] = lbnd[ 0 ];
         if( lbnd_tile[ 1 ] < lbnd[ 1 ] ) lbnd_tile[ 1 ] = lbnd[ 1 ];
         if( lbnd_tile[ 2 ] < lbnd[ 2 ] ) lbnd_tile[ 2 ] = lbnd[ 2 ];
         if( ubnd_tile[ 0 ] > ubnd[ 0 ] ) ubnd_tile[ 0 ] = ubnd[ 0 ];
         if( ubnd_tile[ 1 ] > ubnd[ 1 ] ) ubnd_tile[ 1 ] = ubnd[ 1 ];
         if( ubnd_tile[ 2 ] > ubnd[ 2 ] ) ubnd_tile[ 2 ] = ubnd[ 2 ];
      }

/* Check there is some overlap. */
      if( lbnd_tile[ 0 ] <= ubnd_tile[ 0 ] &&
          lbnd_tile[ 1 ] <= ubnd_tile[ 1 ] &&
          lbnd_tile[ 2 ] <= ubnd_tile[ 2 ] ){

/* Now need to check if this section of the input NDF contains any good
   values. We also find the bounding box of the good values (within the
   input pixel coordinate system). So first obtain and map the required
   section of the input NDF. */
         ndfSect( indf, ndim, lbnd_tile, ubnd_tile, &indfs, status );
         ndfMap( indfs, "Data", type, "Read", &ipd, &junk, status );
         if( var ) ndfMap( indfs, "Variance", type, "Read", &ipv, &junk, status );
         if( qual ) ndfMap( indfs, "Quality", "_UBYTE", "Read", (void **) &ipq,
                            &junk, status );

/* Initialise the pixel bounds (within the input NDF) of the box holding
   good data values for the current tile. */
         bbox[ 0 ] = INT_MAX;
         bbox[ 1 ] = INT_MAX;
         bbox[ 2 ] = INT_MAX;
         bbox[ 3 ] = -INT_MAX;
         bbox[ 4 ] = -INT_MAX;
         bbox[ 5 ] = -INT_MAX;

/* Loop round all pixels in the section. */
         if( *status == SAI__OK ) {
            if( !strcmp( type, "_REAL" ) ) {
               pf = (float *) ipd;
               for( iz = lbnd_tile[ 2 ]; iz <= ubnd_tile[ 2 ]; iz++ ) {
                  for( iy = lbnd_tile[ 1 ]; iy <= ubnd_tile[ 1 ]; iy++ ) {
                     for( ix = lbnd_tile[ 0 ]; ix <= ubnd_tile[ 0 ]; ix++ ) {
                        if( *(pf++) != VAL__BADR ) {
                           if( ix < bbox[ 0 ] ) bbox[ 0 ] = ix;
                           if( iy < bbox[ 1 ] ) bbox[ 1 ] = iy;
                           if( iz < bbox[ 2 ] ) bbox[ 2 ] = iz;
                           if( ix > bbox[ 3 ] ) bbox[ 3 ] = ix;
                           if( iy > bbox[ 4 ] ) bbox[ 4 ] = iy;
                           if( iz > bbox[ 5 ] ) bbox[ 5 ] = iz;
                        }
                     }
                  }
               }
            } else {
               pd = (double *) ipd;
               for( iz = lbnd_tile[ 2 ]; iz <= ubnd_tile[ 2 ]; iz++ ) {
                  for( iy = lbnd_tile[ 1 ]; iy <= ubnd_tile[ 1 ]; iy++ ) {
                     for( ix = lbnd_tile[ 0 ]; ix <= ubnd_tile[ 0 ]; ix++ ) {
                        if( *(pd++) != VAL__BADD ) {
                           if( ix < bbox[ 0 ] ) bbox[ 0 ] = ix;
                           if( iy < bbox[ 1 ] ) bbox[ 1 ] = iy;
                           if( iz < bbox[ 2 ] ) bbox[ 2 ] = iz;
                           if( ix > bbox[ 3 ] ) bbox[ 3 ] = ix;
                           if( iy > bbox[ 4 ] ) bbox[ 4 ] = iy;
                           if( iz > bbox[ 5 ] ) bbox[ 5 ] = iz;
                        }
                     }
                  }
               }
            }

/* Skip empty tiles. */
            if( bbox[ 0 ] != INT_MAX ) {
               msgOutf( "", "   tile %d", status, tile_index );

/* If required, trim the bounds to the edges of the bounding box. */
               if( trim >= 2 ) {
                  olbnd[ 0 ] = bbox[ 0 ];
                  olbnd[ 1 ] = bbox[ 1 ];
                  olbnd[ 2 ] = bbox[ 2 ];
                  oubnd[ 0 ] = bbox[ 3 ];
                  oubnd[ 1 ] = bbox[ 4 ];
                  oubnd[ 2 ] = bbox[ 5 ];
               } else {
                  olbnd[ 0 ] = lbnd_tile[ 0 ];
                  olbnd[ 1 ] = lbnd_tile[ 1 ];
                  olbnd[ 2 ] = lbnd_tile[ 2 ];
                  oubnd[ 0 ] = ubnd_tile[ 0 ];
                  oubnd[ 1 ] = ubnd_tile[ 1 ];
                  oubnd[ 2 ] = ubnd_tile[ 2 ];
               }

/* Modify these pixel bounds so that they refer to the output NDF. */
               lbnd_in[ 0 ] = olbnd[ 0 ] - 0.5;
               lbnd_in[ 1 ] = olbnd[ 1 ] - 0.5;
               lbnd_in[ 2 ] = olbnd[ 2 ] - 0.5;
               ubnd_in[ 0 ] = oubnd[ 0 ] - 0.5;
               ubnd_in[ 1 ] = oubnd[ 1 ] - 0.5;
               ubnd_in[ 2 ] = oubnd[ 2 ] - 0.5;

               astMapBox( p2pmap, lbnd_in, ubnd_in, 1, 1, lbnd_out + 0,
                          ubnd_out + 0, NULL, NULL );
               astMapBox( p2pmap, lbnd_in, ubnd_in, 1, 2, lbnd_out + 1,
                          ubnd_out + 1, NULL, NULL );
               if( ndim == 3 ) astMapBox( p2pmap, lbnd_in, ubnd_in, 1, 3,
                                          lbnd_out + 2, ubnd_out + 2, NULL,
                                          NULL );

               olbnd[ 0 ] = floor( lbnd_out[ 0 ] ) + 1;
               olbnd[ 1 ] = floor( lbnd_out[ 1 ] ) + 1;
               olbnd[ 2 ] = floor( lbnd_out[ 2 ] ) + 1;
               oubnd[ 0 ] = floor( ubnd_out[ 0 ] ) + 1;
               oubnd[ 1 ] = floor( ubnd_out[ 1 ] ) + 1;
               oubnd[ 2 ] = floor( ubnd_out[ 2 ] ) + 1;

/* Get the full path to the output NDF for the current tile, and create an
   NDF placeholder for it. */
               sprintf( path, "%.*s_%d", nbase, base, tile_index );
               ndfPlace( NULL, path, &place, status );

/* Create a new output NDF by copying the meta-data from the input NDF
   section. */
               ndfScopy( indfs, "Units", &place, &indfo, status );

/* Set the pixel bounds of the output NDF to the values found above and copy
   the input data for the current tile into it. */
               smf1_jsadicer( indfo, olbnd, oubnd, tile_map, tile_frm, p2pmap,
                              ipd, ipv, ipq, status );

/* Add the name of this output NDF to the group holding the names of the
   output NDFs that have actually been created. */
               if( grp ) grpPut1( grp, path, 0, status );

/* Add a TILENUM header to the output FITS extension. */
               kpgGtfts( indfo, &fc, status );
               if( *status == KPG__NOFTS ) {
                  errAnnul( status );
                  fc = astFitsChan( NULL, NULL, " " );

/* If the last card is "END", remove it. */
               } else {
                  astSetI( fc, "Card", astGetI( fc, "NCARD" ) );
                  keyword = astGetC( fc, "CardName" );
                  if( keyword && !strcmp( keyword, "END" ) ) astDelFits( fc );
               }

               one_snprintf(jsatile_comment, 45, "JSA all-sky tile index (Nside=%i)",
                            status, tiling.ntpf);
               atlPtfti( fc, "TILENUM", tile_index, jsatile_comment, status );
               kpgPtfts( indfo, fc, status );
               fc = astAnnul( fc );

/* Now store an STC-S polygon that describes the shortest boundary
   enclosing the good data in the output NDF, and store it as an NDF extension. */
               kpgPutOutline( indfo, 0.5, 1, status );

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
                  if( ndimx > 1 && lbndx[ lonax ] == lbnd[ lonax ] &&
                                   lbndx[ latax ] == lbnd[ latax ] &&
                                   ubndx[ lonax ] == ubnd[ lonax ] &&
                                   ubndx[ latax ] == ubnd[ latax ] ) {

/* If so, change the bounds of the output extension NDF so that they are
   the same as the main NDF on the spatial axes, and map the original
   contents of the NDF onto the new pixel grid. */
                     smf1_jsadicer( indfx, olbnd, oubnd, tile_map, tile_frm, p2pmap,
                                    NULL, NULL, NULL, status );
                  }

/* Annul the extension NDF identifier. */
                  ndfAnnul( &indfx, status );
               }

/* Free resources associated with the current tile. */
               grpDelet( &grpt, status );
               ndfAnnul( &indfo, status );

/* Issue warnings about empty tiles. */
            } else {
               msgOutiff( MSG__VERB, "", "   tile %d is empty and so will not be "
                          "created", status, tile_index );
            }
         }

/* Free the section of the input NDF. */
         ndfAnnul( &indfs, status );

/* Append the index of this tile in the list of tiles to be created. */
         created_tiles = astGrow( created_tiles, ++(*ntile),
                                  sizeof( *created_tiles ) );
         if( *status == SAI__OK ) created_tiles[ *ntile - 1 ] = tile_index;

      } else {
         msgOutiff( MSG__DEBUG, "", "   Tile %d does not overlap the input "
                    "NDF after trimming.", status, tile_index );
      }
   }
   msgBlank( status );

/* Write the indicies of the created tiles out to a parameter. */
   if( *ntile ) parPut1i( "JSATILELIST", *ntile, created_tiles, status );

/* Free resources. */
   created_tiles = astFree( created_tiles );
   tiles = astFree( tiles );
   path = astFree( path );

/* End the NDF context. */
   ndfEnd( status );

/* End the AST context. */
   astEnd;

}



static void smf1_jsadicer( int indfo, int *olbnd, int *oubnd,
                           AstMapping *tile_map, AstFrame *tile_frm,
                           AstMapping *p2pmap, void *ipd, void *ipv,
                           unsigned char *ipq, int *status ){
/*
*  Name:
*     smf1_jsadicer

*  Purpose:
*     Copy one tile from the input NDF into a specified output NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf1_jsadicer( int indfo, int *olbnd, int *oubnd,
*                         AstMapping *tile_map, AstFrame *tile_frm,
*                         AstMapping *p2pmap, void *ipd, void *ipv,
*                         unsigned char *ipq, int *status )

*  Arguments:
*     indfo = int (Given)
*        An identifier for the NDF in which the copied data is to be
*        stored. It's original pixel bounds are used as the bounds of the
*        ipd, ipv and ipq arrays.
*     olbnd = int * (Given)
*        The new lower pixel bounds required for the output NDF. The bounds
*        of the supplied NDF are changed to match these values.
*     oubnd = int * (Given)
*        The new upper pixel bounds required for the output NDF. The bounds
*        of the supplied NDF are changed to match these values.
*     tile_map = AstMapping * (Given)
*        The mapping from pixel coords in the output NDF to WCS coords.
*     tile_frm = AstMapping * (Given)
*        The WCS Frame for the output NDF.
*     p2pmap = AstMapping * (Given)
*        The mapping from pixel coords in the input NDF to pixel coords in
*        the output NDF.
*     ipd = void * (Given)
*        Pointer to the start of the input data array. If this is NULL,
*        the existing contents of the NDF are used as input.
*     ipv = void * (Given)
*        Pointer to the start of the input variance array. Should be NULL
*        if no variances are available.
*     ipq = unsigned char * (Given)
*        Pointer to the start of the input quality array. Should be NULL
*        if no quality is available.
*     status = int * (Given)
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstFrame *use_frm = NULL;
   AstFrameSet *owcs;
   AstMapping *use_map = NULL;
   AstMapping *use_p2pmap = NULL;
   AstShiftMap *sm;
   char type[ NDF__SZTYP + 1 ];
   double shifts[ 3 ];
   int axes[ 2 ];
   int axout[ NDF__MXDIM ];
   int free_arrays;
   int isreal;
   int lbnd_tile[ 3 ];
   int ndim;
   int nel;
   int nin;
   int there;
   int ubnd_tile[ 3 ];
   unsigned char *ipq_out = NULL;
   void *ipd_out = NULL;
   void *ipv_out = NULL;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an AST context. */
   astBegin;

/* Get the NDF data type - _REAL or _DOUBLE. */
   ndfType( indfo, "Data", type, sizeof(type), status );
   isreal = !strcmp( type, "_REAL" );

/* Get the existing bounds of the NDF. */
   ndfBound( indfo, 3, lbnd_tile, ubnd_tile, &ndim, status );

/* If no data array has been supplied, take a copy of the original Data,
   Quality and Variance arrays and use these as the input arrays. */
   if( !ipd ) {
      free_arrays = 1;

      ndfMap( indfo, "Data", type, "Read", &ipd_out, &nel, status );
      ipd = astStore( NULL, ipd_out,
                      nel*(isreal?sizeof(float):sizeof(double)) );
      ndfUnmap( indfo, "Data", status );

      ndfState( indfo, "Variance", &there, status );
      if( there ) {
         ndfMap( indfo, "Variance", type, "Read", &ipv_out, &nel, status );
         ipv = astStore( NULL, ipv_out,
                         nel*(isreal?sizeof(float):sizeof(double)) );
         ndfUnmap( indfo, "Variance", status );
      } else {
         ipv = NULL;
      }

      ndfState( indfo, "Quality", &there, status );
      if( there ) {
         ndfMap( indfo, "Quality", "_UBYTE", "Read", (void **) &ipq_out,
                 &nel, status );
         ipq = astStore( NULL, ipq_out, nel*sizeof(*ipq) );
         ndfUnmap( indfo, "Quality", status );
      } else {
         ipq = NULL;
      }

   } else {
      free_arrays = 0;
   }

/* Set the bounds of the NDF to the required values. */
   ndfSbnd( ndim, olbnd, oubnd, indfo, status );

/* Erase the existing WCS FrameSet and then get the default WCS FrameSet. */
   ndfReset( indfo, "WCS", status );
   ndfGtwcs( indfo, &owcs, status );

/* If the supplied mapping and Frame have two many axes, strip some off.
   The orering of pixel axes in the output JSA tile is hardwired by SMURF
   as (ra,dec,spec). */
   nin = astGetI( tile_map, "Nin" );
   if( nin == 3 && ndim == 2 ) {
      axes[ 0 ] = 1;
      axes[ 1 ] = 2;
      astMapSplit( tile_map, 2, axes, axout, &use_map );
      if( use_map ) {
         use_frm = astPickAxes( tile_frm, 2, axout, NULL );
      } else if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf( " ", "smf1_jsadicer: cannot split mapping (programming "
                  "error).", status );
      }

      astMapSplit( p2pmap, 2, axes, axout, &use_p2pmap );
      if( !use_p2pmap && *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf( " ", "smf1_jsadicer: cannot split mapping (programming "
                  "error).", status );
      }

   } else if( nin == ndim ) {
      use_p2pmap = astClone( p2pmap );
      use_map = astClone( tile_map );
      use_frm = astClone( tile_frm );

   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "smf1_jsadicer: unexpected combination of nin (%d) and "
               "ndim (%d) (programming error).", status, nin, ndim );
   }

/* Add the tile WCS Frame into the output NDF's WCS FrameSet, using "tilemap"
   to connect it to the PIXEL Frame (NDF ensure Frame 2 is the PIXEL
   Frame). */
   astAddFrame( owcs, 2, use_map, use_frm );

/* The astResample function is odd in that it assumes that pixel coords
   are defined such that the centre of pixel "I" has integral pixel
   coord "I" (rather than "I-0.5" as is usual in Starlink). So we need to
   use a half-pixel ShiftMap at start and end of the p2pmap Mapping to
   account for this. */
   shifts[ 0 ] = -0.5;
   shifts[ 1 ] = -0.5;
   shifts[ 2 ] = -0.5;
   sm = astShiftMap( ndim, shifts, " " );
   use_p2pmap = (AstMapping *) astCmpMap( sm, use_p2pmap, 1, " " );
   astInvert( sm );
   use_p2pmap = (AstMapping *) astCmpMap( use_p2pmap, sm, 1, " " );

/* Store this modified WCS FrameSet in the output NDF. */
   ndfPtwcs( owcs, indfo, status );

/* Map the required arrays of the output NDF. */
   ndfMap( indfo, "Data", type, "Write", &ipd_out, &nel, status );
   if( ipv ) ndfMap( indfo, "Variance", type, "Write", &ipv_out, &nel,
                     status );
   if( ipq ) ndfMap( indfo, "Quality", "_UBYTE", "Write",
                      (void **) &ipq_out, &nel, status );

/* Copy the input data values to the output, using nearest neighbour
   interpolation (the mapping should always map input pixel centres onto
   output pixel centres). We can set the "tol" argument non-zero (e.g. 0.1)
   without introducing any error because the the p2pmap mapping will be
   piecewise linear. This gives a factor of about 5 decrease in the time
   spent within astResample. */
   if( !strcmp( type, "_REAL" ) ) {
      (void) astResampleF( use_p2pmap, ndim, lbnd_tile, ubnd_tile, (float *) ipd,
                           (float *) ipv, AST__NEAREST, NULL, NULL,
                           AST__USEBAD, 0.1, 1000, VAL__BADR, ndim,
                           olbnd, oubnd, olbnd, oubnd,
                           (float *) ipd_out, (float *) ipv_out );
   } else {
      (void) astResampleD( use_p2pmap, ndim, lbnd_tile, ubnd_tile, (double *) ipd,
                           (double *) ipv, AST__NEAREST, NULL, NULL,
                           AST__USEBAD, 0.1, 1000, VAL__BADD, ndim,
                           olbnd, oubnd, olbnd, oubnd,
                           (double *) ipd_out, (double *) ipv_out );
   }

   if( ipq ) {
      (void) astResampleUB( use_p2pmap, ndim, lbnd_tile, ubnd_tile, ipq, NULL,
                            AST__NEAREST, NULL, NULL, 0, 0.1, 1000, 0,
                            ndim, olbnd, oubnd, olbnd, oubnd, ipq_out,
                            NULL );
   }

/* Unmap everything the output NDF. */
   ndfUnmap( indfo, "*", status );

/* Free the input arrays if they were allocated in this function. */
   if( free_arrays ) {
      ipd = astFree( ipd );
      ipv = astFree( ipv );
      ipq = astFree( ipq );
   }

/* End the AST context. */
   astEnd;
}

