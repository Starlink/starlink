/*
*+
*  Name:
*     JSATILEINFO

*  Purpose:
*     Return information about a specified sky tile.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_jsatileinfo( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine returns information about a single standard sky tile
*     for a named JCMT instrument (specified by parameter ITILE). This
*     includes the RA and Dec. of the tile centre, the tile size, the tile
*     WCS, and whether the NDF containing the accumulated co-added data
*     for the tile currently exists. If not, there is an option to create
*     it and fill it with zeros (see parameter CREATE). In addition, an
*     NDF can be created containing the tile index at every point on the
*     whole sky (see parameter ALLSKY).
*
*     Also, the bounds of the overlap in pixel indices between the tile
*     and a specified NDF or Region can be found (see parameter TARGET).
*
*     The environment variable JSA_TILE_DIR should be defined prior to
*     using this command, and should hold the path to the directory in
*     which the NDFs containing the accumulated co-added data for each
*     tile are stored. Tiles for a specified instrument will be stored
*     within a sub-directory of this directory (see parameter INSTRUMENT).
*     If JSA_TILE_DIR is undefined, the current directory is used.

*  ADAM Parameters:
*     ALLSKY = NDF (Write)
*        If not null (!), this is the name of a new 2D NDF to create holding
*        the tile index at every point on the sky. The pixel size in this
*        image is much larger than in an individual tile in order to keep
*        the NDF size managable. Each pixel holds the integer index of a
*        tile. Pixels that have no corresponding tile hold a bad value. [!]
*     CREATE = _LOGICAL (Read)
*        Indicates if the NDF containing co-added data for the tile should
*        be created if it does not already exist. If TRUE, and if the
*        tile's NDF does not exist on entry to this command, a new NDF will
*        be created for the tile and filled with zeros. The instrument
*        subdirectory within the JSA_TILE_DIR directory will be created if it
*        does not already exist. [FALSE]
*     DECCEN = _DOUBLE (Write)
*        An output parameter to which is written the ICRS Declination
*        of the tile centre, in radians.
*     EXISTS = _LOGICAL (Write)
*        An output parameter that is set TRUE if the NDF containing
*        co-added data for the tile existed on entry to this command (FALSE
*        otherwise).
*     HEADER = LITERAL (Read)
*        The name of a new text file in which to store the WCS and
*        extent of the tile in the form of a set of FITS-WCS headers. [!]
*     INSTRUMENT = LITERAL (Read)
*        The JCMT instrument (different instruments have different
*        tiling schemes and pixel sizes). The following instrument
*        names are recognised (unambiguous abbreviations may be
*        supplied): "SCUBA-2(450)", "SCUBA-2(850)", "ACSIS", "DAS". NDFs
*        containing co-added data for the selected instrument reside
*        within a corresponding sub-directory of the directory specified
*        by environment variable JSA_TILE_DIR. These sub-directories are
*        called "scuba2-450", "scuba2-850", "acsis" and "das".
*     ITILE = _INTEGER (Read)
*        The index of the tile about which information is required. The
*        first tile has index 0. The  largest allowed tile index is
*        always returned in output parameter MAXTILE. If a null (!)
*        value is supplied for ITILE, the MAXTILE parameter is still
*        written, but the command will then exit immediately without
*        further action (and without error).
*     LBND( 2 ) = _INTEGER (Write)
*        An output parameter to which are written the lower pixel bounds
*        of the NDF containing the co-added data for the tile.
*     LOCAL = _LOGICAL (Read)
*        If TRUE, the FITS reference point is put at the centre of the
*        tile. Otherwise, it is put at RA=0 Dec=0. [FALSE]
*     MAXTILE = _INTEGER (Write)
*        An output parameter to which is written the largest tile index
*        associated with the instrument specified by parameter INSTRUMENT.
*     PROJ = LITERAL (Read)
*        Determines the JSA projection to use. The allowed values are
*        "HPX" (HPX projection centred on RA=0h), "HPX12" (HPX projection
*        centred on RA=12h), "XPHN" (XPH projection centred on the north
*        pole) and "XPHS" (XPH projection centred on the south pole). A
*        null (!) value causes "HPX" to be used. ["HPX"]
*     RACEN = LITERAL (Write)
*        An output parameter to which is written the ICRS Right Ascension
*        of the tile centre, in radians.
*     REGION = LITERAL (Read)
*        The name of a new text file in which to store the WCS and extent
*        of the tile in the form of an AST Region. [!]
*     SIZE = _DOUBLE (Write)
*        An output parameter to which is written the arc-length of each
*        side of a square bounding box for the tile, in radians.
*     TARGET = LITERAL (Read)
*        Defines the region of interest. The pixel index bounds of the area
*        of the specified tile that overlaps this region are found and
*        reported (see parameter TLBND and TUBND). The value supplied for
*        TARGET can be either the name of a text file containing an AST
*        Region, or the path for an NDF, in which case a Region is created
*        that covers the region of sky that maps onto the rectangular
*        pixel grid of the NDF. [!]
*     TILENDF = LITERAL (Write)
*        An output parameter to which is written the full path to the
*        NDF containing co-added data for the tile. Note, it is not
*        guaranteed that this NDF exists on exit from this command (see
*        parameters EXISTS and CREATE).
*     TLBND( 2 ) = _INTEGER (Write)
*        An output parameter to which are written the lower pixel bounds
*        of the area of the tile NDF that overlaps the region specified
*        by parameter TARGET. If there is no overlap, the TLBND values
*        are returned greater than the TUBND values.
*     TUBND( 2 ) = _INTEGER (Write)
*        An output parameter to which are written the upper pixel bounds
*        of the area of the tile NDF that overlaps the region specified
*        by parameter TARGET. If there is no overlap, the TLBND values
*        are returned greater than the TUBND values.
*     UBND( 2 ) = _INTEGER (Write)
*        An output parameter to which are written the upper pixel bounds
*        of the NDF containing the co-added data for the tile.

*  Related Applications:
*     SMURF: MAKECUBE, MAKEMAP, TILELIST.

*  Authors:
*     DSB: David Berry (JAC, UCLan)
*     GSB: Graham Bell (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-FEB-2011 (DSB):
*        Original version.
*     16-JUN-2013 (DSB):
*        Added parameters LOCAL, TARGET, TLBND and TUBND.
*     31-OCT-2013 (GSB):
*        - Change displayed comment after tile index to clarify
*          the range of indices.
*        - Normalize ra,dec for display to screen.
*        - Write normalised (ra,dec) to output parameter.
*     2-NOV-2013 (DSB):
*        Revert to writing unnormalised (ra,dec) to output parameter.
*        Starlink standard behaviour is to normalise (ra,dec) prior
*        to display, not for passing on to future calculations.
*     7-NOV-2013 (DSB):
*        Use smf_jsainstrument to get the instrument and tiling parameters.
*     17-JUN-2014 (DSB):
*        Added parameter USEXPH.
*     8-JUL-2014 (DSB):
*        Change USEXPH to accept "North", "South" or "None" instead of 1,
*        -1 and 0.
*     1-OCT-2014 (DSB):
*        Change USEXPH to PROJ.

*  Copyright:
*     Copyright (C) 2011-2014 Science and Technology Facilities Council.
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
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "par.h"
#include "star/hds.h"
#include "star/atl.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/jsatiles.h"


F77_SUBROUTINE(ast_isaregion)( INTEGER(THIS), INTEGER(STATUS) );


void smurf_jsatileinfo( int *status ) {

/* Local Variables */
   AstCmpRegion *overlap;
   AstFitsChan *fc;
   AstFrameSet *fs;
   AstObject *obj;
   AstRegion *region;
   AstRegion *target;
   HDSLoc *cloc = NULL;
   HDSLoc *xloc = NULL;
   char *jcmt_tiles;
   char *tilendf = NULL;
   char text[ 200 ];
   double dec[ 8 ];
   double dist;
   double dlbnd[ 2 ];
   double dubnd[ 2 ];
   double gx[ 8 ];
   double gy[ 8 ];
   double maxdist;
   double norm_radec[2];
   double point1[ 2 ];
   double point2[ 2 ];
   double ra[ 8 ];
   double ra0;
   double dec0;
   int *ipntr;
   int axes[ 2 ];
   int create;
   int dirlen;
   int el;
   int exists;
   int flag;
   int i;
   int indf1;
   int indf2;
   int indf3;
   int itile;
   int iv;
   int jtile;
   int lbnd[ 2 ];
   int local_origin;
   int nc;
   int place;
   int tlbnd[ 2 ];
   int tubnd[ 2 ];
   int ubnd[ 2 ];
   smf_jsaproj_t proj;
   int xt;
   int yt;
   smfJSATiling skytiling;
   void *pntr;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Start a new AST context. */
   astBegin;

/* Get the instrument to use abnd get the parameters describing the
   layout of its JSA tiles. */
   smf_jsainstrument( "INSTRUMENT", NULL, SMF__INST_NONE, &skytiling,
                      status );

/* Return the maximum tile index. */
   parPut0i( "MAXTILE", skytiling.ntiles - 1, status );

/* Abort if an error has occurred. */
   if( *status != SAI__OK ) goto L999;

/* Decide what sort of projection to use. */
   parChoic( "PROJ", "HPX", "HPX,HPX12,XPHN,XPHS", 1, text, sizeof(text),
             status );
   proj = smf_jsaproj_fromstr( text, 1, status );

/* If required, create an all-sky NDF in which each pixel covers the area
   of a single tile, and holds the integer tile index. The NDF has an
   initial size of 1x1 pixels, but is expanded later to the required size. */
   lbnd[ 0 ] = ubnd[ 0 ] = lbnd[ 1 ] = ubnd[ 1 ] = 1;
   ndfCreat( "ALLSKY", "_INTEGER", 2, lbnd, ubnd, &indf3, status );

/* If a null (!) value was supplied for parameter ALLSKY, annull the
   error and pass on. */
   if( *status == PAR__NULL ) {
      errAnnul( status );

/* Otherwise, create a FrameSet describing the whole sky in which each
   pixel corresponds to a single tile. */
   } else {
      smf_jsatile( -1, &skytiling, 0, proj, NULL, &fs, NULL, lbnd, ubnd,
                   status );

/* Change the bounds of the output NDF. */
      ndfSbnd( 2, lbnd, ubnd, indf3, status );

/* Store the FrameSet in the NDF. */
      ndfPtwcs( fs, indf3, status );

/* Map the data array. */
      ndfMap( indf3, "Data", "_INTEGER", "WRITE/BAD", (void **) &ipntr, &el,
              status );

/* Create all-sky map using XPH projection. */
      if( *status == SAI__OK ) {

/* Loop round every tile index. */
         for( jtile = 0; jtile < skytiling.ntiles; jtile++ ) {

/* Get the zero-based (x,y) indices of the tile within an HPX projection.
   This flips the bottom left half-facet up to the top right. */
            smf_jsatilei2xy( jtile, &skytiling, &xt, &yt, NULL, status );

/* Convert these HPX indices to the corresponding indices within the
   required projection. Note, the lower left facet is split by the above
   call to smf_jsatilei2xy tile (i.e. (xt,yt) indices are *not* in the
   "raw" mode). For instance, (0,0) is not a valid tile. */
            smf_jsatilexyconv( &skytiling, proj, xt, yt, 0, &xt, &yt, status );

/* Get the vector index of the corresponding element of the all-sky NDF. */
            if( proj == SMF__JSA_HPX || proj == SMF__JSA_HPX12 ) {
               iv = xt + 5*skytiling.ntpf*yt;
            } else {
               iv = xt + 4*skytiling.ntpf*yt;
            }

/* Report an error if this element has already been assigned a tile
   index. Otherwise, store the tile index. */
            if( ipntr[ iv ] == VAL__BADI ) {
               ipntr[ iv ] = jtile;
            } else if( *status == SAI__OK ) {
               *status = SAI__ERROR;
               errRepf( "", "%s projection assigns multiple tiles to "
                        "pixel (%d,%d).", status, text, xt, yt );
               break;
            }
         }
      }

/* Store NDF title. */
      sprintf( text, "JSA tile indices for %s data", skytiling.name );
      ndfCput( text, indf3, "TITLE", status );

/* Store the instrument as a component in the SMURF extension. */
      ndfXnew( indf3, "SMURF", "INSTRUMENT", 0, 0, &xloc, status );
      ndfXpt0c( skytiling.name, indf3, "SMURF", "INSTRUMENT", status );
      datAnnul( &xloc, status );

/* Close the NDF. */
      ndfAnnul( &indf3, status );
   }

/* Abort if an error has occurred. */
   if( *status != SAI__OK ) goto L999;

/* Get the zero-based index of the required tile. If a null value is
   supplied, annull the error and skip to the end. */
   parGdr0i( "ITILE", 0, 0, skytiling.ntiles - 1, 0, &itile, status );
   if( *status == PAR__NULL ) {
       errAnnul( status );
       goto L999;
   }

/* See if the pixel origin is to be at the centre of the tile. */
   parGet0l( "LOCAL", &local_origin, status );

/* Display the tile number. */
   msgBlank( status );
   msgSeti( "ITILE", itile );
   msgSeti( "MAXTILE", skytiling.ntiles - 1);
   msgOut( " ", "   Tile ^ITILE (from 0 to ^MAXTILE):", status );

/* Get the FITS header, FrameSet and Region defining the tile, and the tile
   bounds in pixel indices. */
   smf_jsatile( itile, &skytiling, local_origin,  proj, &fc, &fs, &region,
                lbnd, ubnd, status );

/* Write the FITS headers out to a file, annulling the error if the
   header is not required. */
   if( *status == SAI__OK ) {
      atlDumpFits( "HEADER", fc, status );
      if( *status == PAR__NULL ) errAnnul( status );
   }

/* If required, write the Region out to a text file. */
   if( *status == SAI__OK ) {
      atlCreat( "REGION", (AstObject *) region, status );
      if( *status == PAR__NULL ) errAnnul( status );
   }

/* Store the lower and upper pixel bounds of the tile. */
   parPut1i( "LBND", 2, lbnd, status );
   parPut1i( "UBND", 2, ubnd, status );

/* Display pixel bounds on the screen. */
   msgSeti( "XL", lbnd[ 0 ] );
   msgSeti( "XU", ubnd[ 0 ] );
   msgSeti( "YL", lbnd[ 1 ] );
   msgSeti( "YU", ubnd[ 1 ] );
   msgOut( " ", "      Pixel bounds: (^XL:^XU,^YL:^YU)", status );

/* Get the RA,Dec at the tile centre. */
   if( astTest( fs, "SkyRef" ) ) {
      ra0 = astGetD( fs, "SkyRef(1)" );
      dec0 = astGetD( fs, "SkyRef(2)" );

/* Format the central RA and Dec. and display. Call astNorm on the
   coordinates provided that the frame set has the correct number of
   axes (which it should as it comes from smf_jsatile). */
      norm_radec[0] = ra0;
      norm_radec[1] = dec0;
      if (astGetI(fs, "Naxes") == 2) astNorm(fs, norm_radec);
      msgSetc( "RACEN",  astFormat( fs, 1, norm_radec[ 0 ] ));
      msgSetc( "DECCEN",  astFormat( fs, 2, norm_radec[ 1 ] ));
      msgOut( " ", "      Centre (ICRS): RA=^RACEN DEC=^DECCEN", status );

/* Transform a collection of points on the edge of the region (corners and
   side mid-points) from GRID coords to RA,Dec. */
      point1[ 0 ] = 0.5;
      point1[ 1 ] = 0.5;
      point2[ 0 ] = ubnd[ 0 ] - lbnd[ 0 ] + 1;
      point2[ 1 ] = ubnd[ 1 ] - lbnd[ 1 ] + 1;

      gx[ 0 ] = point1[ 0 ];         /* Bottom left */
      gy[ 0 ] = point1[ 1 ];

      gx[ 1 ] = point1[ 0 ];         /* Centre left */
      gy[ 1 ] = gy[ 0 ];

      gx[ 2 ] = point1[ 0 ];         /* Top left */
      gy[ 2 ] = point2[ 1 ];

      gx[ 3 ] = gx[ 0 ];             /* Top centre */
      gy[ 3 ] = point2[ 1 ];

      gx[ 4 ] = point2[ 0 ];         /* Top right */
      gy[ 4 ] = point2[ 1 ];

      gx[ 5 ] = point2[ 0 ];         /* Centre right */
      gy[ 5 ] = gy[ 0 ];

      gx[ 6 ] = point2[ 0 ];         /* Bottom right */
      gy[ 6 ] = point1[ 1 ];

      gx[ 7 ] = gx[ 0 ];             /* Bottom centre */
      gy[ 7 ] = point1[ 1 ];

      astTran2( fs, 8, gx, gy, 1, ra, dec );

/* Find the arc-distance from the centre to the furthest point from the
   centre. */
      point1[ 0 ] = ra0;
      point1[ 1 ] = dec0;
      maxdist = -1.0;

      for( i = 1; i < 8; i++ ) {
         if( ra[ i ] != AST__BAD && dec[ i ] != AST__BAD ) {
            point2[ 0 ] = ra[ i ];
            point2[ 1 ] = dec[ i ];
            dist = astDistance( fs, point1, point2 );
            if( dist > maxdist ) maxdist = dist;
         }
      }

/* Convert from radius to diameter. */
      maxdist *= 2.0;

/* Format this size as a dec value (i.e. arc-distance) and display it. */
      if( maxdist > 0.0 ) {
         msgSetc( "SIZE",  astFormat( fs, 2, maxdist ) );
         msgOut( " ", "      Size: ^SIZE", status );
      } else {
         maxdist = AST__BAD;
         msgOut( " ", "      Size: <unknown>", status );
      }

/* If a discontinuity passes through the tile, the centre and size may be
   unknown. */
   } else {
      ra0 = AST__BAD;
      dec0 = AST__BAD;
      maxdist = AST__BAD;
      msgOut( " ", "      Centre (ICRS): RA=<unknown> DEC=<unknown>", status );
      msgOut( " ", "      Size: <unknown>", status );
   }

/* Write the tile centre ra and dec in radians to the output parameters. */
   parPut0d( "RACEN", norm_radec[ 0 ], status );
   parPut0d( "DECCEN", norm_radec[ 1 ], status );

/* Write the size to the output parameter as radians. */
   parPut0d( "SIZE", maxdist, status );

/* Get the translation of the environment variable JSA_TILE_DIR. */
   jcmt_tiles = getenv( "JSA_TILE_DIR" );

/* Initialise the path to the tile's NDF to hold the root directory.
   Use the current working directory if JSA_TILE_DIR is undefined. */
   if( jcmt_tiles ) {
      nc = 0;
      tilendf = astAppendString( tilendf, &nc, jcmt_tiles );

   } else {

      nc = 512;
      jcmt_tiles = astMalloc( nc );

      while( !getcwd( jcmt_tiles, nc ) ) {
         nc *= 2;
         jcmt_tiles = astRealloc( jcmt_tiles, nc );
      }

      nc = 0;
      tilendf = astAppendString( tilendf, &nc, jcmt_tiles );
      jcmt_tiles = astFree( jcmt_tiles );
   }

/* Complete the path to the tile's NDF. */
   tilendf = astAppendString( tilendf, &nc, "/" );
   tilendf = astAppendString( tilendf, &nc, skytiling.subdir );
   dirlen = nc;
   sprintf( text, "/tile_%d.sdf", itile );
   tilendf = astAppendString( tilendf, &nc, text );

/* Write it to the output parameter. */
   parPut0c( "TILENDF", tilendf, status );

/* See if the NDF exists, and store the flag in the output parameter. */
   exists = access( tilendf, F_OK ) ? 0 : 1;
   parPut0l( "EXISTS", exists, status );

/* If the NDF does not exist, create it if required. */
   parGet0l( "CREATE", &create, status );
   if( !exists && create && *status == SAI__OK ) {

/* Write the NDF info to the screen. */
      msgSetc( "NDF",  tilendf );
      msgOutif( MSG__NORM, " ", "      NDF: ^NDF (created)", status );

/* Temporarily terminate the NDF path at the end of the subdirectory. */
      tilendf[ dirlen ] = 0;

/* Create the required directory (does nothing if the directory
   already exists).  It is given read/write/search permissions for owner
   and group, and read/search permissions for others. */
      (void) mkdir( tilendf, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH );

/* Replace the character temporarily overwritten above. */
      tilendf[ dirlen ] = '/';

/* Now create the tile's NDF. */
      ndfPlace( NULL, tilendf, &place, status );
      ndfNew( skytiling.type, 2, lbnd, ubnd, &place, &indf1, status );

/* Fill its data array with zeros. */
      ndfMap( indf1, "Data", skytiling.type, "WRITE/ZERO", &pntr, &el,
              status );

/* Store the WCS FrameSet. */
      ndfPtwcs( fs, indf1, status );

/* If the instrument jsatiles.have variance, fill the variance array with zeros. */
      if( skytiling.var ) {
         ndfMap( indf1, "Variance", skytiling.type, "WRITE/ZERO", &pntr,
                 &el, status );
      }

/* Create a SMURF extension. */
      ndfXnew( indf1, SMURF__EXTNAME, SMURF__EXTTYPE, 0, NULL, &xloc, status );

/* Store the tile number and instrument name in the extension. */
      datNew0I( xloc, "TILE", status );
      datFind( xloc, "TILE", &cloc, status );
      datPut0I( cloc, itile, status );
      datAnnul( &cloc, status );

      datNew0C( xloc, "INSTRUMENT", strlen( skytiling.name ), status );
      datFind( xloc, "INSTRUMENT", &cloc, status );
      datPut0C( cloc, skytiling.name, status );
      datAnnul( &cloc, status );

/* Create a weights NDF within the SMURF extension, and fill its data
   array with zeros. */
      ndfPlace( xloc, "WEIGHTS", &place, status );
      ndfNew( skytiling.type, 2, lbnd, ubnd, &place, &indf2, status );
      ndfMap( indf2, "Data", skytiling.type, "WRITE/ZERO", &pntr, &el,
              status );
      ndfPtwcs( fs, indf2, status );
      ndfAnnul( &indf2, status );

/* Annul the extension locator and the main NDF identifier. */
      datAnnul( &xloc, status );
      ndfAnnul( &indf1, status );

/* Write the NDF info to the screen. */
   } else {
      msgSetc( "NDF",  tilendf );
      msgSetc( "E",  exists ? "exists" : "does not exist" );
      msgOut( " ", "      NDF: ^NDF (^E)", status );
   }

/* Initialise TBND and TLBND to indicate no overlap. */
   tlbnd[ 0 ] = 1;
   tlbnd[ 1 ] = 1;
   tubnd[ 0 ] = 0;
   tubnd[ 1 ] = 0;

/* Attempt to to get an AST Region (assumed to be in some 2D sky coordinate
   system) using parameter "TARGET". */
   if( *status == SAI__OK ) {
      kpg1Gtobj( "TARGET", "Region",
                 (void (*)( void )) F77_EXTERNAL_NAME(ast_isaregion),
                 &obj, status );

/* Annul the error if none was obtained. */
      if( *status == PAR__NULL ) {
         errAnnul( status );

/* Otherwise, use the supplied object. */
      } else {
         target = (AstRegion *) obj;

/* If the target Region is 3-dimensional, remove the third axis, which
   is assumed to be a spectral axis. */
         if( astGetI( target, "Naxes" ) == 3 ) {
            axes[ 0 ] = 1;
            axes[ 1 ] = 2;
            target = astPickAxes( target, 2, axes, NULL );
         }

/* See if there is any overlap between the target and the tile. */
         overlap = NULL;
         flag = astOverlap( region, target );

         if( flag == 0 ) {
            msgOut( "", "      Cannot convert between the coordinate system of the "
                    "supplied target and the tile.", status );

         } else if( flag == 1 || flag == 6 ) {
            msgOut( "", "      There is no overlap between the target and the tile.",
                    status );

         } else if( flag == 2 ) {
            msgOut( "", "      The tile is contained within the target.",
                    status );
            tlbnd[ 0 ] = lbnd[ 0 ];
            tlbnd[ 1 ] = lbnd[ 1 ];
            tubnd[ 0 ] = ubnd[ 0 ];
            tubnd[ 1 ] = ubnd[ 1 ];

         } else if( flag == 3 ) {
            overlap = astCmpRegion( region, target, AST__AND, " " );

         } else if( flag == 4 ) {
            overlap = astCmpRegion( region, target, AST__AND, " " );

         } else if( flag == 5 ) {
            msgOut( "", "      The target and tile are identical.",
                    status );
            tlbnd[ 0 ] = lbnd[ 0 ];
            tlbnd[ 1 ] = lbnd[ 1 ];
            tubnd[ 0 ] = ubnd[ 0 ];
            tubnd[ 1 ] = ubnd[ 1 ];

         } else if( *status == SAI__OK ) {
            *status = SAI__OK;
            errRepf( "", "Unexpected value %d returned by astOverlap "
                     "(programming error).", status, flag );
         }

/* If a region containing the intersection of the tile and target was
   created above, map it into the grid coordinate system of the tile. */
         if( overlap ) {
            overlap = astMapRegion( overlap, astGetMapping( fs, AST__CURRENT,
                                                            AST__BASE ),
                                    astGetFrame( fs, AST__BASE ) );

/* Get its GRID bounds. */
            astGetRegionBounds( overlap, dlbnd, dubnd );

/* Convert to integer. */
            tlbnd[ 0 ] = ceil( dlbnd[ 0 ] - 0.5 );
            tlbnd[ 1 ] = ceil( dlbnd[ 1 ] - 0.5 );
            tubnd[ 0 ] = ceil( dubnd[ 0 ] - 0.5 );
            tubnd[ 1 ] = ceil( dubnd[ 1 ] - 0.5 );

/* Convert to PIXEL indices within the tile. */
            tlbnd[ 0 ] += lbnd[ 0 ] - 1;
            tlbnd[ 1 ] += lbnd[ 1 ] - 1;
            tubnd[ 0 ] += lbnd[ 0 ] - 1;
            tubnd[ 1 ] += lbnd[ 1 ] - 1;

            msgOutf( "", "      The target overlaps section (%d:%d,%d:%d).",
                     status, tlbnd[ 0 ], tubnd[ 0 ], tlbnd[ 1 ], tubnd[ 1 ] );
         }
      }
   }

/* Store the pixel index bounds of the tiles overlap with the target. */
   parPut1i( "TLBND", 2, tlbnd, status );
   parPut1i( "TUBND", 2, tubnd, status );

/* Arrive here if an error occurs. */
   L999:;

/* Free resources. */
   tilendf = astFree( tilendf );

/* End the AST context. */
   astEnd;

/* Issue a status indication.*/
   msgBlank( status );
   if( *status == SAI__OK ) {
      msgOutif( MSG__VERB, "", "JSATILEINFO succeeded.", status);
   } else {
      msgOutif( MSG__VERB, "", "JSATILEINFO failed.", status);
   }
}


