/*
*+
*  Name:
*     JSATILELIST

*  Purpose:
*     List the sky tiles that overlap a given set of data files or an
*     AST Region.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_jsatilelist( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine returns a list containing the indices of the sky tiles
*     (for a named JCMT instrument) that overlap a supplied AST Region,
*     map, cube or group of raw data files.

*  ADAM Parameters:
*     IN = LITERAL (Read)
*        Specifies the region of sky for which tiles should be listed. It
*        may be:
*
*        - The name of a text file containing an AST Region. The Region
*        can be either 2D or 3D but must include celestial axes.
*
*        - The path to a 2- or 3-D NDF holding a reduced map or cube. This
*        need not necessarily hold JCMT data, but must have celestial axes
*        in its current WCS Frame.
*
*        - A group of raw JCMT data files.
*
*        - A null (!) value, in which case a polygon region is used
*        as defined by the parameters VERTEX_RA and VERTEX_DEC.
*
*     INSTRUMENT = LITERAL (Read)
*        The JCMT instrument (different instruments have different
*        tiling schemes and pixel sizes). The following instrument
*        names are recognised (unambiguous abbreviations may be
*        supplied): "SCUBA-2(450)", "SCUBA-2(850)", "ACSIS", "DAS". If one
*        or more NDFs are supplied for parameter IN, then a dynamic default
*        is determined if possible from the first NDF. If this cannot be
*        done, or if a Region is supplied for parameter IN, then no dynamic
*        default is provided, and the user is prompted for a value if none
*        was supplied on the command line. []
*     PROJ = LITERAL (Write)
*        The type of JSA projection that should be used to describe the
*        area of sky covered by the returned lost of tiles. Will be one of
*        "HPX", "HPX12", "XPHN" or "XPHS". The choice is made to minimise
*        the possibility of a projection discontinuity falling within the
*        sky area covered by the tiles.
*     TILES(*) = _INTEGER (Write)
*        An output parameter to which is written the list of integer tile
*        indices.
*     VERTEX_DEC(*) = _DOUBLE (Read)
*        The ICRA Dec value at each vertex of a polygon, in degrees. Only
*        used if IN is null.
*     VERTEX_RA(*) = _DOUBLE (Read)
*        The ICRA RA value at each vertex of a polygon, in degrees. Only
*        used if IN is null.

*  Tile Definitions:
*     It should never be necessary to know the specific details of the tiling
*     scheme used by SMURF. But for reference, it works as follows:
*
*     The whole sky is covered by an HPX (HEALPix) projection containing
*     12 basic square facets, the reference point of the projection is put
*     at (RA,Dec)=(0,0) (except for facet six that has a reference
*     point of (12h,0)). The projection plane is rotated by 45 degrees so
*     that the edges of each facet are parallel to X and Y (as in Fig.3 of
*     the A&A paper "Mapping on the HEALPix grid" by Calabretta and Roukema).
*     Each facet is then divided up into NxN tiles, where N is 64 for
*     SCUBA-2 and 128 for ACSIS. Each tile is then divided into PxP pixels,
*     where P is 412 for ACSIS, 825 for SCUBA-2 850 um, 1650 for SCUBA-2
*     450 um. Facets are numbered from 0 to 11 as defined in the HEALPix
*     paper (Gorsky et. al. 2005 ApJ 622, 759) (note that the facet six is
*     split equally into two triangles, one at the bottom left and one at
*     the top right of the projection plane). Within a facet, tiles are
*     indexed using the "nested" scheme described in the HEALPix paper.
*     This starts with pixel zero in the southern corner of the facet.
*     The even bits number the position in the north-east direction and
*     the odd bits number the position in the north-west direction. All
*     the tiles in the first facet come first, followed by all the tiles in
*     the second facet, etc.
*
*     This is a fairly complex scheme. To help understanding, the
*     SMURF:TILEINFO command can create an all-sky map in which each
*     pixel corresponds to a single tile, and has a pixel value equal
*     to the corresponding tile index. Displaying this map can help
*     to visualise the indexing scheme described above.

*  Related Applications:
*     SMURF: MAKECUBE, MAKEMAP, TILEINFO.

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-MAR-2011 (DSB):
*        Original version.
*     12-JUL-2013 (DSB):
*        Added parameter IN.
*     7-NOV-2013 (DSB):
*        - Call smf_jsainstrument to get the instrument and tiling scheme.
*        - Allow this command to be used on a reduced map or cube, or
*        other NDF.
*     5-DEC-2013 (DSB):
*        Added parameter VERTEX_RA and VERTEX_DEC.
*     9-MAY-2014 (DSB):
*        No need to check for science files before calling
*        smf_jsatiles_data. This speeds things up a lot.
*     2-OCT-2014 (DSB):
*        Added PROJ parameter.

*  Copyright:
*     Copyright (C) 2011,2013,2014 Science and Technology Facilities Council.
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


#if HAVE_CONFIG_H
#include <config.h>
#endif

/* System includes */
#include <stdlib.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "sae_par.h"
#include "kpg_err.h"
#include "star/thr.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/jsatiles.h"

#define MAXVERT 100

F77_SUBROUTINE(ast_isaregion)( INTEGER(THIS), INTEGER(STATUS) );
static int jsatilelist_icomp(const void *a, const void *b);


void smurf_jsatilelist( int *status ) {

/* Local Variables */
   AstFitsChan *fc = NULL;
   AstFrameSet *fs = NULL;
   AstObject *obj;
   AstRegion *region;
   Grp *igrp = NULL;
   ThrWorkForce *wf = NULL;
   dim_t lbnd[2];
   dim_t ubnd[2];
   double vertex_data[ 2*MAXVERT ];
   int *tiles = NULL;
   int i;
   int indf;
   int ntile;
   int nvert_dec;
   int nvert_ra;
   size_t size;
   smfJSATiling tiling;
   smf_jsaproj_t proj;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Start a new AST context. */
   astBegin;

/* Find the number of cores/processors available and create a pool of
   threads of the same size. */
   wf = thrGetWorkforce( thrGetNThread( SMF__THREADS, status ), status );

/* Attempt to to get an AST Region. */
   kpg1Gtobj( "IN", "Region",
              (void (*)( void )) F77_EXTERNAL_NAME(ast_isaregion),
              &obj, status );
   region = (AstRegion *) obj;

/* If successful, attempt to access the IN parameter as an NDF. If this
   works, we may be able to determine the instrument by looking at its
   FITS extension. */
   if( *status == SAI__OK && region ) {
      ndfExist( "IN", "Read", &indf, status );

/* If we got an NDF, get a FitsChan holding the contents of its FITS
   extension. Annul the error if the NDF has no FITS extension. */
      if( indf != NDF__NOID ) {
         kpgGtfts( indf, &fc, status );
         if( *status == KPG__NOFTS ) {
            errAnnul( status );
            fc = NULL;
         }
         ndfAnnul( &indf, status );
      }

/* Select a JSA instrument and get the parameters defining the layout of
   tiles for the selected instrument. */
      smf_jsainstrument( "INSTRUMENT", fc, SMF__INST_NONE, &tiling,
                         status );

/* Get the list of identifiers for tiles that overlap the region. */
      tiles = smf_jsatiles_region( region, &tiling, &ntile, status );

/* If a null value was supplied for IN, attempt to get the positions of
   vertices on the sky to define the region. */
   } else if( *status == PAR__NULL ) {
      errAnnul( status );
      parGet1d( "VERTEX_RA", MAXVERT, vertex_data, &nvert_ra, status );
      parGet1d( "VERTEX_DEC", MAXVERT, vertex_data + MAXVERT, &nvert_dec,
                 status );
      if( nvert_ra != nvert_dec && *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf( "", "Differing numbers of RA (%d) and Dec (%d) vertex values "
                 "supplied.", status, nvert_ra, nvert_dec );
      }

/* Convert from degrees to radians. */
      for( i = 0; i < nvert_ra; i++ ) {
         vertex_data[ i ] *= AST__DD2R;
         vertex_data[ MAXVERT + i ] *= AST__DD2R;
      }

/* Select a JSA instrument and get the parameters defining the layout of
   tiles for the selected instrument. */
      smf_jsainstrument( "INSTRUMENT", NULL, SMF__INST_NONE, &tiling,
                         status );

/* Create a frame in which to define the region - we arbitrarily use tile 1. */
      smf_jsatile( 1, &tiling, 0, SMF__JSA_HPX, NULL, &fs, NULL, lbnd, ubnd, status );

/* Create the region. */
      region = (AstRegion *) astPolygon( fs, nvert_ra, MAXVERT, vertex_data, NULL, " " );

/* If the region is unbounded, it is probably because the vertices were
   given in the wrong order. Invert the Polyfon to correct this. */
      if( !astGetI( region, "bounded" ) ) astNegate( region );

/* Get the list of identifiers for tiles that overlap the region. */
      tiles = smf_jsatiles_region( region, &tiling, &ntile, status );
   }

/* If the IN parameter could not be accessed as a Region, annull any error
   and get a group of input data files. */
   if( !region || *status == SAI__ERROR ) {
      if( *status != SAI__OK ) errAnnul( status );
      kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

/* Get the list of identifiers for tiles that receive any data. */
      tiles = smf_jsatiles_data( wf, igrp, size, &tiling, &ntile, status );

/* Check we have at least once science file. */
      if( !tiles && *status == SAI__OK ) {
         msgOutif( MSG__NORM, " ", "None of the supplied input frames were SCIENCE.",
                   status );
      }

/* Delete the group. */
      if( igrp ) grpDelet( &igrp, status);
   }

/* If we got a list of tiles, see what JSA projection should be used to
   describe the mosaic so that any discontinuities are avoided. */
   if( tiles && *status == SAI__OK ) {
      proj = smf_jsaproj( ntile, tiles, &tiling, status );
      parPut0c( "PROJ", smf_jsaproj_tostr( proj ), status );

/* Sort the list of overlapping tiles into ascending order. */
      qsort( tiles, ntile, sizeof( *tiles ), jsatilelist_icomp );

/* Display the list of overlapping tiles. */
      msgBlank( status );
      msgOutf( "", "   %s tiles touched by supplied data:", status,
               tiling.name );
      msgBlank( status );
      for( i = 0; i < ntile; i++ ) {
         msgSeti( "I", tiles[ i ] );
         msgOut( "", "   ^I", status );
      }
      msgBlank( status );

/* Write out the list of overlapping tiles to the output parameter. */
      parPut1i( "TILES", ntile, tiles, status );
   }

/* Free resources. */
   tiles = astFree( tiles );

/* End the AST context. */
   astEnd;

/* Issue a status indication.*/
   msgBlank( status );
   if( *status == SAI__OK ) {
      msgOutif( MSG__VERB, "", "JSATILELIST succeeded.", status);
   } else {
      msgOutif( MSG__VERB, "", "JSATILELIST failed.", status);
   }
}






/* Service function for "qsort", called in smurf_jsatilelist. It sorts
   integers into increasing order. */
static int jsatilelist_icomp(const void *a, const void *b){
   return *((const int *) a) - *((const int *) b);
}
