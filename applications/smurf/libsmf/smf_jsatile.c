/*
*+
*  Name:
*     smf_jsatile

*  Purpose:
*     Get the definition of a single sky tile.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_jsatile( int itile, smfJSATiling *skytiling, int local_origin,
*                       smf_jsaproj_t proj, AstFitsChan **fc, AstFrameSet **fs,
*                       AstRegion **region, int lbnd[2], int ubnd[2], int *status )

*  Arguments:
*     itile = int (Given)
*        The zero-based index of the tile. Supplying a tile index of -1
*        causes the returned values to cover the whole sky, with a single
*        grid pixel corresponding to a single tile.
*     skytiling = smfJSATiling * (Given)
*        Pointer to a structure holding parameters describing the tiling
*        scheme used for the required JCMT instrument, as returned by
*        function smf_jsatiling.
*     local_origin = int (Given)
*        If non-zero, then the returned header will include values for
*        projection parameters PVi_1 and PVi_2 that causes the origin of
*        grid coordinates to be moved to the centre of the tile. If zero,
*        the origin of pixel coordinates will be at RA=0, Dec=0.
*     proj = smf_jsaproj_t (Given)
*        The projection to use. This can be an HPX projection centred
*        either on RA=0 or RA=12h, or an XPH projection centred either
*        on the north pole or the south pole. When creating an individual
*        JSA tile, SMF__JSA_HPX (HPX centred on RA=0) should always be
*        used. When creating a mosaic of JSA tiles, the projection should
*        be chosen so that no discontinuities cross the mosaic.
*     fc = AstFitsChan ** (Returned)
*        Address at which to return a pointer to a FitsChan containing
*        the FITS headers for the tile. May be NULL.
*     fs = AstFitsChan ** (Returned)
*        Address at which to return a pointer to a FrameSet defining the
*        WCS for the tile. May be NULL. The base Frame (frame 1) will be
*        GRID coords and the current Frame (frame 2) will be ICRS (RA,DEc).
*        There will also be a PIXEL Frame (frame 3).
*     region = AstRegion ** (Returned)
*        Address at which to return a pointer to a Region defining the
*        extent of the tile. May be NULL. The Region will be defined
*        defined within ICRS (RA,DEc).
*     lbnd[ 2 ] = integer (Returned)
*        The lower bounds of the spatial axes of the tile in PIXEL indices.
*     ubnd[ 2 ] = integer (Returned)
*        The upper bounds of the spatial axes of the tile in PIXEL indices.
*     status = int * (Given)
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Region pointer (a Box). The Region will represent ICRS (RA,Dec).

*  Description:
*     This function returns an AST Region describing the outline of the
*     requested sky tile in (RA,Dec), together with the the dimensions of
*     the tile in grid pixels. Th NDF pixel origin is assumed to be at
*     the FITS reference point, as selected by argument "local_origin".

*  Authors:
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     14-APR-2011 (DSB):
*        Initial version.
*     16-JUL-2013 (DSB):
*        Added argument "local_origin". Replaced "dim" by "lbnd" and "ubnd".
*     6-NOV-2013 (DSB):
*        - Document the indices of the GRID, SKY and PIXEL Frames in the
*        returned FrameSet.
*        - Correct definition of PIXEL Frame in returned FrameSet.
*        - Ensure FITS ref. point is placed somewhere in pixel zero.
*     31-MAR-2014 (DSB):
*        Tiles in the split facet (facet 6) use RA=12H as the FITS
*        reference point, and so their pixel bounds need to be
*        corrected to refer to the NDF pixel origin at RA=0h.
*     10-JUN-2014 (DSB):
*        Correct choice of which tiles to move from bottom left to top
*        right of the all sky pixel grid.
*     12-JUN-2014 (DSB):
*        Added argument "usexph".
*     30-SEP-2014 (DSB):
*        Set up correct properties for the JSA_PIXEL Frame, returned within
*        the all-sky WCS.
*     1-OCT-2014 (DSB):
*        Allow an HPX projection centred on RA=12 to be used.
*     3-OCT-2014 (DSB):
*        Most edge tiles are bisected by a discontinuity, which can
*        prevent the tile centre and region being determined. If this
*        occurs, try to determine the centre and region for the requested
*        tile using a different JSA projection (which will have different
*        discontinuities).
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011,2013-2014 Science & Technology Facilities Council.
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

/* Local constants */
#define DELTA 0.05 /* Pixel offset to avoid edges */

void smf_jsatile( int itile, smfJSATiling *skytiling, int local_origin,
                  smf_jsaproj_t proj, AstFitsChan **fc, AstFrameSet **fs,
                  AstRegion **region, int lbnd[2], int ubnd[2], int *status ) {

/* Local Variables: */
   AstFitsChan *lfc = NULL;
   AstFitsChan *tfc = NULL;
   AstFrameSet *lfs = NULL;
   AstFrameSet *tfs = NULL;
   AstRegion *lregion = NULL;
   double crpix1;
   double crpix2;
   double crval1;
   double point1[ 2 ];
   double point2[ 2 ];
   double shift[ 2 ];
   double tmp;
   int edge_tile;
   int icrpix1;
   int icrpix2;
   int icur;
   int isky;
   int move;
   int offset;
   int tubnd[ 2];
   smf_jsaproj_t tproj;

/* Initialise the returned pointers. */
   if( fc ) *fc = NULL;
   if( fs ) *fs = NULL;
   if( region ) *region = NULL;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an AST context. */
   astBegin;

/* Get a FitsChan holding the FITS headers defining the tile WCS and
   extent. */
   lfc = smf_jsatileheader( itile, skytiling, local_origin, proj, &move,
                            status );

/* Store the upper bounds of the tile in GRID coords (later changed to
   PIXEL coords). */
   if( ( !astGetFitsI( lfc, "NAXIS1", ubnd )  ||
         !astGetFitsI( lfc, "NAXIS2", ubnd + 1 ) ) &&
         *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( " ", "Failed to get a tile dimensions (programming error).",
                 status );
   }

/* Get the GRID coords of the FITS reference point. */
   astGetFitsF( lfc, "CRPIX1", &crpix1 );
   astGetFitsF( lfc, "CRPIX2", &crpix2 );

/* Get the RA at the FITS reference point. */
   astGetFitsF( lfc, "CRVAL1", &crval1 );

/* If required, return a deep copy of the FitsChan, before the
   WCS-related cards are removed by the following astRead call. */
   if( fc ) *fc = astCopy( lfc );

/* If required, read a FrameSet from the FITS headers. */
   if( fs || region ) {
      astClear( lfc, "Card" );
      lfs = astRead( lfc );
      if( ! lfs ) {
         if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep( " ", "Failed to get a FrameSet for the tile "
                    "(programming error).", status );
         }
      }

/* Set the Digits attributes of the SkyFrame in order to get 2 decimal
   places in formatted RA seconds fields, and 1 decimal place in
   formatted DEC arc-seconds fields. */
      astSetI( lfs, "Digits(1)", 8 );
      astSetI( lfs, "Digits(2)", 8 );

/* If an allsky grid was created above, it may contain a Frame
   representing the all-sky JSA HPX (SMF__JSA_HPX) pixel grid. This Frame
   will have Domain "XJSA-YJSA". Change its Frame attributes to more useful
   values. */
      if( itile == -1 && proj == SMF__JSA_HPX ) {
         isky = astGetI( lfs, "Current" );
         astSetC( lfs, "Current", "XJSA-YJSA" );
         astSetC( lfs, "Domain", "JSA_PIXEL" );
         astSetC( lfs, "Label(1)", "JSA pixel coordinate 1" );
         astSetC( lfs, "Label(2)", "JSA pixel coordinate 2" );
         astSetC( lfs, "Unit(1)", "pixel" );
         astSetC( lfs, "Unit(2)", "pixel" );
         astSetC( lfs, "Title", "JSA all-sky pixel coordinates" );
         astSetI( lfs, "Current", isky );
      }

/* Loop until we have found a projection in which the tile centre and
   region can be determined (i.e. a projection in which the tile is not an
   edge tile). */
      tfs = astClone( lfs );
      tubnd[ 0 ] = ubnd[ 0 ];
      tubnd[ 1 ] = ubnd[ 1 ];
      tproj = SMF__JSA_NULL;
      edge_tile = 1;
      while( edge_tile && *status == SAI__OK ){

/* Find the sky coords at the centre of the tile and use as the sky
   reference position. If a projection discontinuity passes through the
   tile (i.e. edge tiles) it may not be possible to determine the centre
   coords. Set a flag to indicate this. */
         point1[ 0 ] = ( tubnd[ 0 ] + 1.0 )/2.0;
         point1[ 1 ] = ( tubnd[ 1 ] + 1.0 )/2.0;

         astTran2( tfs, 1, point1, point1 + 1, 1, point2, point2 + 1 );

         if( point2[ 0 ] != AST__BAD && point2[ 1 ] != AST__BAD ) {
            astSetD( lfs, "SkyRef(1)", point2[ 0 ] );
            astSetD( lfs, "SkyRef(2)", point2[ 1 ] );
            edge_tile = 0;
         } else {
            astClear( lfs, "SkyRef" );
            edge_tile = 1;
         }

/* If required, create a Region (a Box) describing the tile in GRID coords.
   GRID coords are described by the base Frame in the FrameSet. Reduce the
   size of the box slightly to avoid bad sky values at the edges of
   extreme tiles. */
         if( region && !edge_tile ) {
            point1[ 0 ] = 0.5 + DELTA;
            point1[ 1 ] = 0.5 + DELTA;
            point2[ 0 ] = tubnd[ 0 ] + 0.5 - DELTA;
            point2[ 1 ] = tubnd[ 1 ] + 0.5 - DELTA;
            lregion = (AstRegion *) astBox(  astGetFrame( tfs, AST__BASE ), 1,
                                             point1, point2, NULL, " " );

/* Map this Box into (RA,Dec) (the current Frame in the FrameSet). If a
   projection discontinuity passes through the tile (i.e. edge tiles) it
   may not be possible to do this mapping. Annul athe error if this
   occrs.*/
            if( *status == SAI__OK ) {
               lregion = astMapRegion( lregion, tfs, tfs );
               if( *status == AST__NODEF ) {
                  errAnnul( status );
                  edge_tile = 1;
               }
            }
         }

/* If this is an edge tile in the current JSA projection, we may not have
   been able to determine the tile centre or sky region. So we now attempt
   to determine them using one of the other JSA projections (the sky Region
   and centre should be the same no matter what projection is used). */
         if( edge_tile ) {

/* Find the next JSA projection to try, Report an error if we have tried
   them all without success. Skip over the supplied projection since we
   have already done it. */
            while( *status == SAI__OK ){
               if( tproj == SMF__JSA_NULL ){
                  tproj = SMF__JSA_HPX;
               } else if( tproj == SMF__JSA_HPX ){
                  tproj = SMF__JSA_HPX12;
               } else if( tproj == SMF__JSA_HPX12 ){
                  tproj = SMF__JSA_XPHN;
               } else if( tproj == SMF__JSA_XPHN ){
                  tproj = SMF__JSA_XPHS;
               } else if( *status == SAI__OK ) {
                  *status = SAI__ERROR;
                  errRepf( "", "Cannot determine the sky region for JSA tile "
                           "%d", status, itile );
               }
               if( tproj != proj ) break;
            }

/* Get a FitsChan holding the FITS headers defining the tile WCS and
   extent in the new projection. */
            tfc = smf_jsatileheader( itile, skytiling, local_origin, tproj,
                                     NULL, status );

/* Store the upper bounds of the tile in new GRID coords. */
            if( ( !astGetFitsI( tfc, "NAXIS1", tubnd )  ||
                  !astGetFitsI( tfc, "NAXIS2", tubnd + 1 ) ) &&
                  *status == SAI__OK ) {
               *status = SAI__ERROR;
               errRep( " ", "Failed to get a tile dimensions (programming error).",
                          status );
            }

/* Read a FrameSet from the FITS headers. */
            astClear( tfc, "Card" );
            tfs = astRead( tfc );
            if( !tfs ) {
               if( *status == SAI__OK ) {
                  *status = SAI__ERROR;
                  errRep( " ", "Failed to get a FrameSet for the tile "
                          "(programming error).", status );
               }
            }
         }
      }
   }

/* Get the integer GRID index that contains the FITS reference point.
   This is basically just the nearest integer to CRPIX, except that
   midway values (i.e. xxx.5) are always rounded in the positive
   direction, regardless of whether CRPIX is positive or negative. */
   if( crpix1 >= 0.0 ) {
      icrpix1 = (int)( crpix1 + 0.5 );
   } else {
      tmp = crpix1 - 0.5;
      icrpix1 = (int) tmp;
      if( icrpix1 == tmp ) icrpix1++;
   }

   if( crpix2 >= 0.0 ) {
      icrpix2 = (int)( crpix2 + 0.5 );
   } else {
      tmp = crpix2 - 0.5;
      icrpix2 = (int) tmp;
      if( icrpix2 == tmp ) icrpix2++;
   }

/* Find the lower bounds of the tile in NDF PIXEL indices. These are
   chosen so that the origin of (RA,Dec) is always somewhere inside pixel
   zero. Note, the FITS reference point may be at either (RA,Dec)=(0h,0deg)
   or at (12h,0deg), depending on the tile. */
   lbnd[ 0 ] = 1 - icrpix1;
   lbnd[ 1 ] = 1 - icrpix2;

/* Now correct the above bounds if the tile is in the lower left facet of
   an HPX projection, which uses a different reference point on the sky. */
   if( ( proj == SMF__JSA_HPX && fabs( crval1 ) > 0.1 ) ||
       ( proj == SMF__JSA_HPX12 && fabs( crval1 ) < 0.1 ) ) {

/* The required shift is the same on both pixel axes, and is the number
   of pixels across two facets. */
      offset = 2*skytiling->ntpf*skytiling->ppt;

/* For tiles that should be in the top right half-facet of the all sky map,
   the current lbnd values are with repect to a pixel origin at the middle
   of the top right diagonal edge of the all-sky map. Shift them to refer to
   the centre of the all sky map. */
      if( move ) {
         lbnd[ 0 ] += offset;
         lbnd[ 1 ] += offset;

/* For tiles in the bottom left half-facet of the all sky map, the current
   lbnd values are with repect to a pixel origin at the middle of the bottom
   left diagonal edge of the all-sky map. Shift them to refer to the
   centre of the all sky map. */
      } else {
         lbnd[ 0 ] -= offset;
         lbnd[ 1 ] -= offset;
      }
   }

/* Find the corresponding upper bounds of the tile in NDF PIXEL indices. */
   ubnd[ 0 ] += lbnd[ 0 ] - 1;
   ubnd[ 1 ] += lbnd[ 1 ] - 1;

/* Add a PIXEL Frame to the returned FrameSet, making sure to leave the
   current Frame unchanged. */
   if( fs ) {
      shift[ 0 ] = lbnd[ 0 ] - 1.5;
      shift[ 1 ] = lbnd[ 1 ] - 1.5;
      icur = astGetI( lfs, "Current" );
      astAddFrame( lfs, AST__BASE, astShiftMap( 2, shift, " " ),
                   astFrame( 2, "Domain=PIXEL" ) );
      astSetI( lfs, "Current", icur );

/* Export the FrameSet pointer so that it is not annulled by the
   following astEnd call, and return it. */
      *fs = lfs;
      astExport( *fs );
   }

/* Export and return the FitsChan and Region if required. */
   if( fc ) astExport( *fc );
   if( region ) {
      *region = lregion;
      astExport( *region );
   }

/* End the AST context. */
   astEnd;

}

