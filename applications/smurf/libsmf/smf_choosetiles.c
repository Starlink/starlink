/*
*+
*  Name:
*     smf_choosetiles

*  Purpose:
*     Splits the output grid up into a series of smaller tiles and
*     returns a description of each tile.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     tiles = smf_choosetiles( Grp *igrp,  int size, dim_t *lbnd, dim_t *ubnd,
*                              smfBox *boxes, int spread, const double params[],
*                              AstFrameSet *wcsout, int tile_size[2],
*                              int trim, int border, dim_t *ntiles,
*                              int *status )

*  Arguments:
*     igrp = Grp * (Given)
*        Group of input NDFs.
*     size = int (Given)
*        Number of elements in igrp
*     lbnd = dim_t * (Given)
*        Pointer to an array holding the lower pixel index bounds of the
*        full size output grid.
*     ubnd = dim_t * (Given)
*        Pointer to an array holding the upper pixel index bounds of the
*        full size output grid.
*     boxes = smfBox * (Given)
*        Pointer to an array of smfBox structures. The length of this
*        array should be equal to "size". Each element of the array holds
*        the spatial bounding box of the corresponding input file, in the
*        pixel index system of the full size output grid.
*     spread = int (Given)
*        Specifies the scheme to be used for dividing each input data value
*        up amongst the corresponding output pixels. See docs for astRebinSeq
*        (SUN/211) for the allowed values.
*     params = const double[] (Given)
*        An optional pointer to an array of double which should contain any
*        additional parameter values required by the pixel spreading scheme.
*        See docs for astRebinSeq (SUN/211) for further information. If no
*        additional parameters are required, this array is not used and a
*        NULL pointer may be given.
*     wcsout = AstFrameSet * (Given)
*        Pointer to the FrameSet describing the WCS of the output cube.
*     tile_size = int[ 2 ] * (Given and Returned)
*        An array holding the nominal spatial dimensions of each tile, in
*        pixels.  Edge tiles may be thinner if "trim" is non-zero. In order
*        to avoid creating very thin tiles around the edges, the actual tile
*        size used for the edge tiles may be up to 10 % larger than the
*        supplied value. This creation of "fat" edge tiles may be prevented
*        by supplying a negative value for the tile size, in which case edge
*        tiles will never be wider than the supplied absolute value. If the
*        first value is zero, then a single tile containing the entire output
*        array is used, with no padding. The actual used tile sizes are
*        returned.
*     trim = int (Given)
*        If true then the border tiles are trimmed to exclude pixels off
*        the edge of the full size output array.
*     border = int (Given)
*        The size of the overlap reqired between adjacent tiles, in
*        pixels. The actual overlap used will be larger than this (by the
*        width of the spreading kernel) in order to avoid edge effects.
*        However, the extra border (the spreading width) will be trimmed
*        off the tiles before the application terminates.
*     ntiles = dim_t * (Returned)
*        Pointer to an int in which to return the number of tiles needed
*        to cover the full size grid.

*  Returned Value:
*     Pointer to an array of smfTile structures. The length of this array
*     will be returned in "*ntiles". This array of structures should be
*     freed using smf_freetiles when no longer needed.

*  Description:
*     This function divides up the spatial coverage of the full size pixel
*     grid specified by "lbnd" and "ubnd" into a number of rectangular tiles,
*     each with spatial area given by "tile_size". It returns an array of
*     smfTile structures, each of which describes the extent and location of
*     a single tile.
*
*     This function only produces spatial tiling. If the supplied grid has
*     a spectral axis, then each tile will cover the entire spectral range of
*     the full size grid.
*
*     The tiles are ordered in a raster like manner, starting at the lower
*     pixel bounds of the full size array. The full size grid is padded out
*     so that it is an integer multiple of the supplied tile size. The
*     padding is done by adding a border to each edge of the supplied full
*     size grid.
*
*     Each smfTile structure includes the following:
*        - The bounds of the tile specified as pixel indices within the
*        full size grid. These bounds result in the tiles abutting with
*        no gap or overlap.
*        - The bounds of the tile including the requested border, specified
*        as pixel indices within the full size grid.
*        - The bounds of a tile that is further extended by the width of
*        the spreading kernel (i.e. these bounds include both the
*        requested biorder and the kernel width).
*        - The bounds of the tile plus border area, expressed in the GRID
*        coordinate system of the expanded tile area.
*        - A pointer to a Grp group holding the names of the input files
*        that have data falling within the extended tile area.
*        - A pointer to an int array holding the zero-based index within the
*        supplied group of input NDFs (igrp) corresponding to each name
*        in the group described in the previous point.
*        - An AST Mapping that describes the shift in origin of the GRID
*        coordinate system from the full sized output array to the tile.
*        - Flags indicating if the four edges of the tile should be
*        flagged using the BORDER quality.

*  Authors:
*     DSB: David S Berry (JAC, UCLan)
*     EC: Ed Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     24-AUG-2007 (DSB):
*        Initial version.
*     11-OCT-2007 (DSB):
*        Added "jndf" compone to the smfTile structure. This holds the
*        zero-based index within the supplied group of each input NDF that
*        overlaps the tile.
*     26-NOV-2007 (DSB):
*        Added argument "wcsout", and changed the tile positions so that
*        a tile is centred at the SkyRef position.
*     9-JAN-2008 (DSB):
*        Correct check for overlap between input boxes and tile areas.
*     14-JAN-2008 (DSB):
*        Added argument "border".
*     15-JAN-2008 (DSB):
*        - Change the "basic" tile bounds to include the requested border,
*        and add another box to the Tile structure holding the tile
*        bounds without border.
*        - Allow for reference points that are outside the bounds of the
*        full sized output array.
*     26-MAY-2008 (EC):
*        Added is2d parameter
*     05-JUN-2008 (EC):
*        Remove is2d from API, determine number of axes internally
*     06-JUN-2008 (TIMJ):
*        For the 2d case also make sure that the 3rd bounds are 1. Prevents
*        overruns in smf_reshapendf
*     09-JUN-2008 (DSB):
*        The third element of an array is is [ 2 ], not [ 3 ].
*     24-JUL-2008 (TIMJ):
*        Trap moving coordinates (or at least offset coordinate systems).
*     14-JUL-2009 (DSB):
*        Exclude edge tiles that are contained completely within the
*        border region added onto their neighbouring tiles.
*     8-AUG-2009 (DSB):
*        Added qxl/qxu/qyl/qyu tile items to indicate if boundary tiles
*        need to be flagged using the BORDER quality.
*     2009-09-29 (TIMJ):
*        Use ndgCopy rather than smf_grpCopy.
*     5-OCT-2009 (DSB):
*        Allow thin edge tiles to be merged with the adjacent tiles.
*     15-OCT-2009 (DSB):
*        Correct bounds of merged adjacent tiles.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2009 Science & Technology Facilities Council.
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
#include "merswrap.h"
#include "star/ndg.h"

/* SMURF includes */
#include "libsmf/smf.h"

smfTile *smf_choosetiles( Grp *igrp,  int size, dim_t *lbnd,
                          dim_t *ubnd, smfBox *boxes, int spread,
                          const double params[], AstFrameSet *wcsout,
                          int tile_size[ 2 ], int trim, int border,
                          dim_t *ntiles, int *status ){

/* Local Variables */
   AstUnitMap *umap = NULL;
   char *pname = NULL;
   char filename[ GRP__SZNAM + 1 ];
   dim_t ix;
   dim_t iy;
   dim_t numtile[ 2 ];
   dim_t plbnd[ 2 ];
   dim_t pubnd[ 2 ];
   dim_t refpixind;
   dim_t tlbnd[ 3 ];
   dim_t tubnd[ 3 ];
   double refpix[ 3 ];
   double refwcs[ 3 ];
   double shift[ 3 ];
   float *w = NULL;
   float *work = NULL;
   float val;
   int adjust[ 2 ];
   dim_t extend = 0;
   int i;
   int indims;
   int lbin;
   int lbout;
   int lfat[ 2 ];
   int ubin;
   int ubout;
   int ufat[ 2 ];
   dim_t xhi;
   dim_t xlo;
   dim_t yhi;
   dim_t ylo;
   smfBox *box = NULL;
   smfTile *result = NULL;
   smfTile *tile = NULL;

/* Initialise the number of tiles in case an error has already occurred. */
   *ntiles = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return NULL;

/* Make sure that we handle 2d or 3d bounds */
   indims = astGetI(wcsout, "Nin");

/* Copy input bounds to local temp copy */
   for (i = 0; i < 2; i++ ) {
     tlbnd[ i ] = lbnd[ i ];
     tubnd[ i ] = ubnd[ i ];
   }
   if (indims == 3) {
     tlbnd[ 2 ] = lbnd[ 2 ];
     tubnd[ 2 ] = ubnd[ 2 ];
   } else {
     tlbnd[ 2 ] = 1;
     tubnd[ 2 ] = 1;
   }

/* If required, produce a description of a single tile that is just large
   enough to hold the entire output array. */
   if( tile_size[ 0 ] == 0 ) {
      *ntiles = 1;
      result = astMalloc( sizeof( *result ) );
      if( result ) {

/* Store the bordered tile area. */
         for( i = 0; i < 3; i++ ) {
            result->lbnd[ i ] = tlbnd[ i ];
            result->ubnd[ i ] = tubnd[ i ];
         }

/* Store the non-border tile area (the same). */
         for( i = 0; i < 3; i++ ) {
            result->qlbnd[ i ] = tlbnd[ i ];
            result->qubnd[ i ] = tubnd[ i ];
         }

/* Store the extended tile area (the same). */
         for( i = 0; i < 3; i++ ) {
            result->elbnd[ i ] = tlbnd[ i ];
            result->eubnd[ i ] = tubnd[ i ];
         }


/* We do not need to re-map the GRID frame of the full sized output WCS
   FrameSet. */
         result->map2d = NULL;
         result->map3d = NULL;

/* Create a GRP group to hold the names of the input files that have data
   that falls within the bounds of the extended tile area. This is just a
   copy of the supplied group. */
         result->grp = smf_ndg_copy( igrp, 0, 0, 0, status );
         result->size = size;

/* A NULL pointer for "jndf" means that the index of an NDF within a tile
   is the same as within the group of all input NDFs. */
         result->jndf = NULL;
      }

/* If the tile size is non-zero, we split the output array into tiles... */
   } else {

/* Ensure the tile sizes are positive, and set flags indicating if either
   supplied value was negative (indicating that we are free to expand the
   edge tiles by up to 10% to reduce the number of tiles). */
      for( i = 0; i < 2; i++ ){
         if( tile_size[ i ] < 0 ) {
            tile_size[ i ] = -tile_size[ i ];
            adjust[ i ] = 0;
         } else {
            adjust[ i ] = 1;
         }
      }

/* Get the celestial coordinates of the reference position (the SkyRef
   value in the current Frame of the output WCS FrameSet), and find the
   corresponding grid coordinates. */

      if ( strcmp(astGetC( wcsout, "SkyRefIs"), "Origin") == 0) {
        /* these are offset coordinates */
        refwcs[ 0 ] = 0.0;
        refwcs[ 1 ] = 0.0;
      } else {
        /* relative to the sky reference point */
        refwcs[ 0 ] = astGetD( wcsout, "SkyRef(1)" );
        refwcs[ 1 ] = astGetD( wcsout, "SkyRef(2)" );
      }
      refwcs[ 2 ] = AST__BAD;
      astTranN( wcsout, 1, indims, 1, refwcs,
                0, astGetI( wcsout, "Nout" ), 1, refpix );

      if (refpix[ 0 ] == VAL__BADD || refpix[ 0 ] == VAL__BADD) {
        if (*status == SAI__OK) {
          *status = SAI__ERROR;
          errRep(" ", "Error calculating grid coordinates of reference point",
                 status );
          return NULL;
        }
      }

/* Convert grid coords to pixel coords */
      refpix[ 0 ] += tlbnd[ 0 ] - 1.5;
      refpix[ 1 ] += tlbnd[ 1 ] - 1.5;

/* We place the pixel containing the reference position at the centre
   of a tile, and then pad the supplied full size grid bounds by adding a
   border to each edge so that each axis is spanned by an integer number
   of tiles. Do each spatial axis separately. */
      for( i = 0; i < 2; i++ ) {

/* Get the pixel index of the pixel containing the reference point. */
         refpixind = ceil( refpix[ i ] );

/* Find the pixel bounds of a tile centred on this pixel. If the
   tile size is even, we place the reference pixel on the higher of the
   two central pixels. */
         plbnd[ i ] = refpixind - tile_size[ i ]/2;
         pubnd[ i ] = plbnd[ i ] + tile_size[ i ] - 1;

/* Store the number of tiles spanned by this axis. */
         numtile[ i ] = 1;

/* Decrease the lower pixel bounds by a tile size until the whole output
   array is encompassed. Take account of the fact that the lowest tile
   will extend the coverage of the tile ensemble by one border width, so
   we can exclude the last tile if it is contained completely within the
   border added onto the last-but-one tile. */
         while( plbnd[ i ] > tlbnd[ i ] + border ) {
            plbnd[ i ] -= tile_size[ i ];
            numtile[ i ]++;
         }

/* If the ref pixel is outside the full sized output array, we may need
   to exclude tiles that are off the edge. So increase the lower pixel
   bounds by a tile size until the upper bound of the corresponding tile
   is inside the full sized output array. */
         while( plbnd[ i ] + tile_size[ i ] - 1 < tlbnd[ i ] + border ) {
            plbnd[ i ] += tile_size[ i ];
            numtile[ i ]--;
         }

/* If the edge tiles are being trimmed, and we have been told to avoid
   thin edge tiles on this axis, then see if the edge tile is thinner than
   10% of the tile size. If so, reduce the number of tiles on this axis by
   one, and set a flag indicating that the edge tile on the lower bound of
   this axis should be merged with the adjacent tile (i.e. fattened). */
         lfat[ i ] = 0;
         if( trim && adjust[ i ] ) {
            if( tlbnd[ i ] - plbnd[ i ] > 0.9*tile_size[ i ] ) {
               numtile[ i ]--;
               lfat[ i ] = 1;
            }
         }

/* Now modify the upper bounds in the same way. */
         while( pubnd[ i ] < tubnd[ i ] - border ) {
            pubnd[ i ] += tile_size[ i ];
            numtile[ i ]++;
         }

         while( pubnd[ i ] - tile_size[ i ] + 1 > tubnd[ i ] - border ) {
            pubnd[ i ] -= tile_size[ i ];
            numtile[ i ]--;
         }

         ufat[ i ] = 0;
         if( trim && adjust[ i ] ) {
            if( pubnd[ i ] - tubnd[ i ] > 0.9*tile_size[ i ] ) {
               numtile[ i ]--;
               ufat[ i ] = 1;
            }
         }
      }

/* Determine the constant width border by which the basic tile area is to be
   extended to accomodate the specified spreading kernel. We do this by
   rebinning a single non-zero pixel value using the supplied spreading
   scheme, and then determining the width of the resulting non-zero pixel
   values. */
      umap = astUnitMap( 1, " " );
      lbin = 0;
      ubin = 0;
      val = 1.0;

      lbout = -1000;
      ubout = 1000;
      work = astMalloc( sizeof( *work )*( ubout - lbout + 1 ) );
      if( work ) {
         astRebinF( umap, 0.0, 1, &lbin, &ubin, &val, NULL, spread, params, 0,
                    0.0, 0, VAL__BADR, 1, &lbout, &ubout, &lbin, &ubin, work,
                    NULL );

         w = work + 1001;
         while( *w != VAL__BADR && *w != 0.0 ) w++;
         extend = w  - ( work + 1001 );

         umap = astAnnul( umap );
         work = astFree( work );
      }

/* Return the total number of tiles, and create the returned array. */
      *ntiles = numtile[ 0 ]*numtile[ 1 ];
      result = astMalloc( sizeof( *result ) * (*ntiles ) );

/* Store a pointer to the next tile desription to create. */
      tile = result;

/* Initialise the y axis bounds (without border or extension) of the tiles in
   the first row. */
      ylo = plbnd[ 1 ];
      if( numtile[ 1 ] == 1 ) {
         yhi = pubnd[ 1 ];
      } else {
         yhi = ylo + tile_size[ 1 ] - 1;
         if( lfat[ 1 ] ) yhi += tile_size[ 1 ];
      }

/* Loop round each row of tiles. */
      for( iy = 0; iy < numtile[ 1 ] && *status == SAI__OK; iy++ ) {

/* Initialise the x axis bounds (without border or extension) of the tiles in
   the first column. */
         xlo = plbnd[ 0 ];
         if( numtile[ 0 ] == 1 ) {
            xhi = pubnd[ 0 ];
         } else {
            xhi = xlo + tile_size[ 0 ] - 1;
            if( lfat[ 0 ] ) xhi += tile_size[ 0 ];
         }

/* Loop round each tile in the current row. */
         for( ix = 0; ix < numtile[ 0 ]; ix++, tile++ ) {

/* Store the tile area (without border or extension). */
            tile->qlbnd[ 0 ] = xlo;
            tile->qubnd[ 0 ] = xhi;
            tile->qlbnd[ 1 ] = ylo;
            tile->qubnd[ 1 ] = yhi;
            tile->qlbnd[ 2 ] = tlbnd[ 2 ];
            tile->qubnd[ 2 ] = tubnd[ 2 ];

/* Limit the tile area to the bounds of the output cube. */
            if( trim ) {
               if( tile->qlbnd[ 0 ] < tlbnd[ 0 ] ) tile->qlbnd[ 0 ] = tlbnd[ 0 ];
               if( tile->qlbnd[ 1 ] < tlbnd[ 1 ] ) tile->qlbnd[ 1 ] = tlbnd[ 1 ];
               if( tile->qubnd[ 0 ] > tubnd[ 0 ] ) tile->qubnd[ 0 ] = tubnd[ 0 ];
               if( tile->qubnd[ 1 ] > tubnd[ 1 ] ) tile->qubnd[ 1 ] = tubnd[ 1 ];
            }

/* Store the tile area including a spatial border of the requested width. */
            tile->lbnd[ 0 ] = tile->qlbnd[ 0 ] - border;
            tile->ubnd[ 0 ] = tile->qubnd[ 0 ] + border;
            tile->lbnd[ 1 ] = tile->qlbnd[ 1 ] - border;
            tile->ubnd[ 1 ] = tile->qubnd[ 1 ] + border;
            tile->lbnd[ 2 ] = tile->qlbnd[ 2 ];
            tile->ubnd[ 2 ] = tile->qubnd[ 2 ];

            if( trim ) {
               if( tile->lbnd[ 0 ] < tlbnd[ 0 ] ) tile->lbnd[ 0 ] = tlbnd[ 0 ];
               if( tile->lbnd[ 1 ] < tlbnd[ 1 ] ) tile->lbnd[ 1 ] = tlbnd[ 1 ];
               if( tile->ubnd[ 0 ] > tubnd[ 0 ] ) tile->ubnd[ 0 ] = tubnd[ 0 ];
               if( tile->ubnd[ 1 ] > tubnd[ 1 ] ) tile->ubnd[ 1 ] = tubnd[ 1 ];
            }

/* Store the extended tile area (this also includes the above border). */
            tile->elbnd[ 0 ] = tile->lbnd[ 0 ] - extend;
            tile->eubnd[ 0 ] = tile->ubnd[ 0 ] + extend;
            tile->elbnd[ 1 ] = tile->lbnd[ 1 ] - extend;
            tile->eubnd[ 1 ] = tile->ubnd[ 1 ] + extend;
            tile->elbnd[ 2 ] = tlbnd[ 2 ];
            tile->eubnd[ 2 ] = tubnd[ 2 ];

/* Store the bounds of the basic (bordered but non-extended) tile in a grid
   coordinate system that has value (1.0,1.0) at the centre of the first pixel
   in the extended tile. */
            tile->glbnd[ 0 ] = tile->lbnd[ 0 ] - tile->elbnd[ 0 ] + 1;
            tile->gubnd[ 0 ] = tile->ubnd[ 0 ] - tile->elbnd[ 0 ] + 1;
            tile->glbnd[ 1 ] = tile->lbnd[ 1 ] - tile->elbnd[ 1 ] + 1;
            tile->gubnd[ 1 ] = tile->ubnd[ 1 ] - tile->elbnd[ 1 ] + 1;
            tile->glbnd[ 2 ] = tile->lbnd[ 2 ] - tile->elbnd[ 2 ] + 1;
            tile->gubnd[ 2 ] = tile->ubnd[ 2 ] - tile->elbnd[ 2 ] + 1;

/* A ShiftMap that describes the shift in the origin of 2D GRID coordinates
   caused by extracting the extended tile from the full sized output array.
   This is the Mapping from the full size GRID coordinate system to the
   tile GRID coordinate system. */
            shift[ 0 ] = tlbnd[ 0 ] - tile->elbnd[ 0 ];
            shift[ 1 ] = tlbnd[ 1 ] - tile->elbnd[ 1 ];
            shift[ 2 ] = 0.0;
            tile->map2d = (AstMapping *) astShiftMap( 2, shift, " " );
            tile->map3d = (AstMapping *) astShiftMap( 3, shift, " " );

/* Create a GRP group to hold the names of the input files that have data
   that falls within the bounds of the extended tile area. */
            tile->grp = smf_grp_new( igrp, "", status );
            tile->jndf = astMalloc( sizeof( *(tile->jndf) )* size );
            if( tile->jndf ) {

/* Find the input files that may overlap the current extended tile area. */
               box = boxes;
               tile->size = 0;
               for( i = 1; i <= size; i++, box++ ){

/* Does the bounding box for the i'th input file overlap the extended
   tile area? If so, include the name of the input file in the group of
   file names that contribute to the current tile. */
                  if( box->lbnd[ 0 ] <= tile->eubnd[ 0 ] &&
                      box->ubnd[ 0 ] >= tile->elbnd[ 0 ] &&
                      box->lbnd[ 1 ] <= tile->eubnd[ 1 ] &&
                      box->ubnd[ 1 ] >= tile->elbnd[ 1 ] ) {

                     pname = filename;
                     grpGet( igrp, i, 1, &pname, GRP__SZNAM, status );
                     grpPut1( tile->grp, filename, 0, status );
                     tile->jndf[ (tile->size)++ ] = i;
                  }
               }
            }

/* Store flags indicating if the borders should be flagged with the BORDER
   quality. */
            tile->qxu = 1;
            if( ix == 0 ) tile->qxl = ( plbnd[ 0 ] < tlbnd[ 0 ] );
            if( ix == numtile[ 0 ] - 1 ) tile->qxu = ( pubnd[ 0 ] > tubnd[ 0 ] );

            tile->qyu = 1;
            if( iy == 0 ) tile->qyl = ( plbnd[ 1 ] < tlbnd[ 1 ] );
            if( iy == numtile[ 1 ] - 1 ) tile->qyu = ( pubnd[ 1 ] > tubnd[ 1 ] );

/* Store the x axis bounds (without border or extension) of the tiles in
   the next column. */
            xlo = xhi + 1;
            xhi = xlo + tile_size[ 0 ] - 1;
            if( ufat[ 0 ] && ix == numtile[ 0 ] - 2 ) xhi += tile_size[ 0 ];
         }

/* Store the y axis bounds (without border or extension) of the tiles in
   the next row. */
         ylo = yhi + 1;
         yhi = ylo + tile_size[ 1 ] - 1;
         if( ufat[ 1 ] && iy == numtile[ 1 ] - 2 ) yhi += tile_size[ 1 ];
      }
   }

/* Return the result. */
   return result;
}
