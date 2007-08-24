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
*                            smfBox *boxes, int spread, const double params[], 
*                            dim_t tile_size[ 2 ], int *ntiles, int *status )

*  Arguments:
*     igrp = Grp * (Given)
*        Group of input NDFs.
*     size = int (Given)
*        Number of elements in igrp
*     lbnd = dimt_t * (Given)
*        Pointer to an array holding the lower pixel index bounds of the 
*        full size output grid.
*     ubnd = dimt_t * (Given)
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
*     tile_size = dim_t[ 2 ] * (Given)
*        An array holding the spatial dimensions of each tile, in pixels.
*     ntiles = int * (Returned)
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
*        - The bounds of an extended rectangle centred on the tile. This 
*        extended area is equal to the tile area with an additonal
*        constant-width border that is wide enough to accomodate the 
*        kernel specified by "spread".
*        - A pointer to a Grp group holding the names of the input files
*        that have data falling within the extended tile area.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     24-AUG-2007 (DSB):
*        Initial version.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

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

smfTile *smf_choosetiles( Grp *igrp,  int size, dim_t *lbnd, 
                          dim_t *ubnd, smfBox *boxes, int spread, 
                          const double params[], dim_t tile_size[ 2 ], 
                          int *ntiles, int *status ){

/* Local Variables */
   AstUnitMap *umap = NULL;
   char *pname = NULL;
   char filename[ GRP__SZNAM + 1 ]; 
   float *w = NULL;
   float *work = NULL;
   float val;
   int dim;
   int dimpad;
   int extend;
   int i;
   int ix;
   int iy;
   int lbin;
   int lbout;
   int numtile[ 2 ];
   int plbnd[ 2 ];
   int pubnd[ 2 ];
   int ubin;
   int ubout;
   int yhi;
   int ylo;
   smfBox *box = NULL;
   smfTile *result = NULL;
   smfTile *tile = NULL;

/* Initialise the number of tiles in case an error has already occurred. */
   *ntiles = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return NULL;

/* Pad the supplied full size grid bounds by adding a border to each
   edge so that the padded dimension is an integer multiple of the supplied
   tile size. This also finds the number of tiles required along the x
   and y pixel axes. */
   for( i = 0; i < 2; i++ ) {
      dim = ubnd[ i ] - lbnd[ i ] + 1;
      numtile[ i ] = 1 + ( ( dim - 1 )/tile_size[ i ] );
      dimpad =  numtile[ i ]*tile_size[ i ];
      plbnd[ i ] = lbnd[ i ] - ( dimpad - dim )/2;
      pubnd[ i ] = dimpad + plbnd[ i ] - 1;
   }

/* Determine the constant width border by which the basic tile area is to be
   extended to accomodate the specified spreading kernel. We do this by
   rebinnig a single non-zero pixel value using the supplied spreading
   scheme, and then determining the width of the resulting non-zero pixel
   values. */

   umap = astUnitMap( 1, "" );
   lbin = 0;
   ubin = 0;
   val = 1.0;

   lbout = -1000;
   ubout = 1000;
   work = astMalloc( sizeof( float )*( ubout - lbout + 1 ) );

   astRebinF( umap, 0.0, 1, &lbin, &ubin, &val, NULL, spread, params, 0,
              0.0, 0, VAL__BADR, 1, &lbout, &ubout, &lbin, &ubin, work, 
              NULL );

   w = work + 1001;
   while( *w != VAL__BADR && *w != 0.0 ) w++;
   extend = w  - ( work + 1001 );

   umap = astAnnul( umap );
   work = astFree( work );

/* Return the total number of tiles, and create the returned array. */
   *ntiles = numtile[ 0 ]*numtile[ 1 ];
   result = astMalloc( sizeof( smfTile ) * (*ntiles ) );

/* Store a pointer to the next tile desription to create. */
   tile = result;   

/* Loop round each row of tiles. */
   for( iy = 0; iy < numtile[ 1 ]; iy++ ) {

/* Store the y axis bounds of the tiles in this row. */
      ylo = plbnd[ 1 ] + iy*tile_size[ 1 ];
      yhi = tile->ylo + tile_size[ 1 ] - 1;

/* Loop round each tile in the current row. */
      for( ix = 0; ix < numtile[ 0 ]; ix++, tile++ ) {

/* Store the tile area. */
         tile->xlo = plbnd[ 0 ] + ix*tile_size[ 0 ];
         tile->xhi = tile->xlo + tile_size[ 0 ] - 1;
         tile->ylo = ylo;
         tile->yhi = yhi;

/* Store the extended tile area. */
         tile->exlo = tile->xlo - extend;
         tile->exhi = tile->xhi + extend;
         tile->eylo = tile->ylo - extend;
         tile->eyhi = tile->yhi + extend;

/* Create a GRP group to hold the names of the input files that have data
   that falls within the bounds of the extended tile area. */
         tile->grp = grpNew( "", status );

/* Find the input files that may overlap the current extended tile area. */
         box = boxes;
         for( i = 1; i <= size; i++, box++ ){

/* Does the bounding box for the i'th input file overlap the extended
   tile area? If so, include the name of hte i'th input file in the group
   of file names that contribute to the current tile. */
            if( box->lbnd[ 0 ] < tile->exhi &&
                box->ubnd[ 0 ] > tile->exlo &&
                box->lbnd[ 1 ] < tile->eyhi &&
                box->ubnd[ 1 ] > tile->eylo ) {

               pname = filename;
               grpGet( igrp, i, 1, &pname, GRP__SZNAM, status );
               grpPut1( tile->grp, filename, 0, status );

            }
         }
      }
   }

/* Return the result. */
   return result;
}
