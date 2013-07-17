/*
*+
*  Name:
*     smf_jlstile

*  Purpose:
*     Get the definition of a single sky tile.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_jlstile( int itile, smfJLSTiling *skytiling, int local_origin,
*                       AstFitsChan **fc, AstFrameSet **fs, AstRegion **region,
*                       int lbnd[2], int ubnd[2], int *status )

*  Arguments:
*     itile = int (Given)
*        The one-based index of the tile. Supplying a tile index of zero
*        causes the returned values to cover the whole sky, with a single
*        grid pixel corresponding to a single tile.
*     skytiling = smfJLSTiling * (Given)
*        Pointer to a structure holding parameters describing the tiling
*        scheme used for the required JCMT instrument, as returned by
*        function smf_jlstiling.
*     local_origin = int (Given)
*        If non-zero, then the returned header will include values for
*        projection parameters PVi_1 and PVi_2 that causes the origin of
*        grid coordinates to be moved to the centre of the tile. If zero,
*        the origin of pixel coordinates will be at RA=0, Dec=0.
*     fc = AstFitsChan ** (Returned)
*        Address at which to return a pointer to a FitsChan containing
*        the FITS headers for the tile. May be NULL.
*     fs = AstFitsChan ** (Returned)
*        Address at which to return a pointer to a FrameSet defining the
*        WCS for the tile. May be NULL. The base Frame will be GRID
*        coords and the current Frame will be ICRS (RA,DEc). There will
*        also be a PIXEL Frame.
*     region = AstRegion ** (Returned)
*        Address at which to return a pointer to a Region defining the
*        extent of the tile. May be NULL. The Region will be defined
*        defined within ICRS (RA,DEc).
*     lbnd[ 2 ] = integer (Returned)
*        The lower bounds of the tile in PIXEL indicies.
*     ubnd[ 2 ] = integer (Returned)
*        The upper bounds of the tile in PIXEL indicies.
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
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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




#include "libsmf/jlstiles.h"   /* Move this to smf_typ.h and smf.h when done */


/* Local constants */
#define DELTA 0.01          /* Pixel offset to avoid edges */

void smf_jlstile( int itile, smfJLSTiling *skytiling, int local_origin,
                  AstFitsChan **fc, AstFrameSet **fs, AstRegion **region,
                  int lbnd[2], int ubnd[2], int *status ) {

/* Local Variables: */
   AstFitsChan *lfc = NULL;
   AstFrameSet *lfs = NULL;
   AstRegion *lregion = NULL;
   double point1[ 2 ];
   double point2[ 2 ];
   double shift[ 2 ];
   int crpix1;
   int crpix2;
   int icur;

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
   lfc = smf_jlstileheader( itile, skytiling, local_origin, status );

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
   astGetFitsI( lfc, "CRPIX1", &crpix1 );
   astGetFitsI( lfc, "CRPIX2", &crpix2 );

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

/* Find the lower and upper bounds of the tile in NDF PIXEL indicies. The
   origin is placed at the FITS reference pixel. */
   lbnd[ 0 ] = 1 - crpix1;
   lbnd[ 1 ] = 1 - crpix2;
   ubnd[ 0 ] += lbnd[ 0 ] - 1;
   ubnd[ 1 ] += lbnd[ 1 ] - 1;

/* Add a PIXEL Frame to the returned FrameSet, making sure to leave the
   current Frame unchanged. */
   shift[ 0 ] = lbnd[ 0 ] - 1.0;
   shift[ 1 ] = lbnd[ 1 ] - 1.0;
   icur = astGetI( lfs, "Current" );
   astAddFrame( lfs, AST__BASE, astShiftMap( 2, shift, " " ),
                astFrame( 2, "Domain=PIXEL" ) );
   astSetI( lfs, "Current", icur );

/* Return and export the required pointers. */
   if( fc ) astExport( *fc );

   if( fs ) {
      *fs = lfs;
      astExport( *fs );
   }

   if( region ) {
      *region = lregion;
      astExport( *region );
   }

/* End the AST context. */
   astEnd;

}

