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
*                       int usexph, AstFitsChan **fc, AstFrameSet **fs,
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
*     usexph = int (Given)
*        If zero, the tile is projected into pixel space using the standard
*        HPX projection. If greater than zero, it is projected using an
*        XPH projection centred on the north pole. If less than zero, it is
*        projected using an XPH projection centred on the south pole. The
*        choice of projection changes all the returned values, except for
*        "region".
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
*        The lower bounds of the spatial axes of the tile in PIXEL indicies.
*     ubnd[ 2 ] = integer (Returned)
*        The upper bounds of the spatial axes of the tile in PIXEL indicies.
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
#define DELTA 0.05          /* Pixel offset to avoid edges */

void smf_jsatile( int itile, smfJSATiling *skytiling, int local_origin, int usexph,
                  AstFitsChan **fc, AstFrameSet **fs, AstRegion **region,
                  int lbnd[2], int ubnd[2], int *status ) {

/* Local Variables: */
   AstFitsChan *lfc = NULL;
   AstFrameSet *lfs = NULL;
   AstRegion *lregion = NULL;
   double crpix1;
   double crpix2;
   double crval1;
   double point1[ 2 ];
   double point2[ 2 ];
   double shift[ 2 ];
   double tmp;
   int icrpix1;
   int icrpix2;
   int icur;
   int move;
   int offset;

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
   lfc = smf_jsatileheader( itile, skytiling, local_origin, usexph, &move, status );

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

/* Find the sky coords at the centre of the tile and use as the sky
   reference position. */
      point1[ 0 ] = ( ubnd[ 0 ] + 1.0 )/2.0;
      point1[ 1 ] = ( ubnd[ 1 ] + 1.0 )/2.0;
      astTran2( lfs, 1, point1, point1 + 1, 1, point2, point2 + 1 );
      astSetD( lfs, "SkyRef(1)", point2[ 0 ] );
      astSetD( lfs, "SkyRef(2)", point2[ 1 ] );

/* If required, create a Region (a Box) describing the tile in GRID coords.
   GRID coords are described by the base Frame in the FrameSet. Reduce the
   size of the box slightly to avoid bad sky values at the edges of
   extreme tiles. */
      if( region ) {
         point1[ 0 ] = 0.5 + DELTA;
         point1[ 1 ] = 0.5 + DELTA;
         point2[ 0 ] = ubnd[ 0 ] + 0.5 - DELTA;
         point2[ 1 ] = ubnd[ 1 ] + 0.5 - DELTA;
         lregion = (AstRegion *) astBox(  astGetFrame( lfs, AST__BASE ), 1,
                                          point1, point2, NULL, " " );

/* Map this Box into (RA,Dec) (the current Frame in the FrameSet). */
         lregion = astMapRegion( lregion, lfs, lfs );
      }
   }

/* Get the integer GRID index that contains the FITS reference point.
   This is basically just the nearest integer to CRPIX, except that
   midway values (i.e. xxx.5) are always rounded in the positive
   direction, reradgless of whether CRPIX is positive or negative. */
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

/* Find the lower bounds of the tile in NDF PIXEL indicies. These are
   chosen so that the origin of (RA,Dec) is always somewhere inside pixel
   zero. Note, the FITS reference point may be at either (RA,Dec)=(0h,0deg)
   or at (12h,0deg), depending on the tile. */
   lbnd[ 0 ] = 1 - icrpix1;
   lbnd[ 1 ] = 1 - icrpix2;

/* Now correct the above bounds if the tile is in a facet that uses RA=12H
   as the reference (there is only one such facet - the first facet,
   which is split bottm left and top right of the all-sky map). */
   if( fabs( crval1 ) > 0.1 ) {

/* The required shift is the same on both pixel axes, and is the number
   of pixels across two facets. */
      offset = 2*skytiling->ntpf*skytiling->ppt;

/* For tiles in the top right half-facet of the all sky map, the current
   lbnd values are with repect to a pixel origin at the middle of the top
   right diagonal edge of the all-sky map. Shift them to refer to the
   centre of the all sky map. */
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

/* Find the corresponding upper bounds of the tile in NDF PIXEL indicies. */
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

