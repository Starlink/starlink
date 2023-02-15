/*
*+
*  Name:
*     fts2_ast.c

*  Purpose:
*

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*

*  Arguments:
*

*  Description:
*     Modified version of sc2ast.c for FTS2 pipeline.

*  Authors:
*     Tanner Heggie (UoL)
*     COBA: Coskun Oba (UoL)
*     DSB: David Berry (JAC)

*  History :
*     Created: May 17, 2010
*     2010-09-20 (COBA):
*       Replaced PI and PIBY2 with AST__DPI and AST__DPIBY2
*     2011-05-17 (DSB):
*       The SMU corrections are defined in AZEL and so should be applied
*       *after* de-rotating the Cartesian Nasmyth coord. So extract the
*       de-rotation from smurf_maketanmap and do it here instead.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2008 University of Lethbridge. All Rights Reserved.

*  License:
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

/* STANDARD INCLUDES */
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

/* STARLINK INCLUDES */
#include "sae_par.h"
#include "ast.h"
#include "ndf.h"
#include "sc2da/Ers.h"

/* SMURF INCLUDES */
#include "fts2.h"

static char errmess[132];  /* For DRAMA error messages */

void fts2ast_createwcs
(
  sc2ast_subarray_t subnum,
  const JCMTState *state,
  const double instap[2],
  const double telpos[3],
  AstFrameSet **fset,
  int *status
)
{
  static sc2astCache *cache = NULL;
  cache = fts2ast_createwcs2( subnum, state, 0.0, instap, telpos, fset, cache, status );
}

sc2astCache *fts2ast_createwcs2
(
  sc2ast_subarray_t subnum, /* subarray number, 0-7 (given). If SC2AST__NULLSUB is
                             supplied the cached AST objects will be freed. */
  const JCMTState *state, /* Current telescope state (time, pointing etc.) */
  double dut1,            /* UT1-UTC (seconds) */
  const double instap[2], /* Offset of subarray in the focal plane */
  const double telpos[3], /* Geodetic W Lon/Lat/Alt of telescope (deg/deg/ign.)*/
  AstFrameSet **fset,     /* constructed frameset (returned) */
  sc2astCache *cache,    /* A pointer to a structure holding cached info */
  int *status             /* global status (given and returned) */
)
{
  AstMapping *azelmap;
  AstMapping *mapping;
  AstMapping *rotmap;
  AstMatrixMap *flipmap;
  AstPolyMap *polymap;
  AstShiftMap *shiftmap;
  AstZoomMap *zoommap;
  AstShiftMap *zshiftmap;
  double shifts[ 2 ];
  AstShiftMap *instapmap;
  AstShiftMap *jigglemap;
  sc2astCache *result;
  int isub;

  double r;                       // subarray angle
  double rot[4];                  // rotation matrix
  const double rotangle[8] = { 0.0,
                               AST__DPIBY2, 2 * AST__DPIBY2, 3 * AST__DPIBY2,
                               3 * AST__DPIBY2, 2 * AST__DPIBY2, AST__DPIBY2,
                               0.0 };
  double shift[2];
  double zshift[2];
  double flip[4] = {1, 0, 0, -1};

  /*
   * xoff and yoff are the distance in pixel units from the tracking center to the [0][0] pixel in a subarray
   * {s8a    s8b     s8c   s8d    s4a    s4b    s4c    s4d}
   */
  const double xoff[8] = { -41.5,   33.5, 41.5, -33.5, -41.5,  33.5,  41.5, -33.5 };
  const double yoff[8] = { -33.5,  -41.5, 33.5,  41.5,  33.5,  41.5, -33.5, -41.5 };

  /*
   * Check the sub-array number. If it is -1, free the cached AST objects and
   * the cache structure itself, and then return. Otherwise, report an error
   * if the value is illegal. We do this before checking the inherited status
   * so that the memory is freed even if an error has occurred.
   */
  if( subnum == SC2AST__NULLSUB )
  {
    if( cache )
    {
      for( subnum = 0; subnum < SC2AST__NSUB; subnum++ )
      {
        if( cache->map[ subnum ] )
        {
          cache->map[ subnum ] = astAnnul( cache->map[ subnum ] );
        }
        if( cache->frameset[ subnum ] )
        {
          cache->frameset[ subnum ] = astAnnul( cache->frameset[ subnum ] );
        }
      }

      if( cache->azel[ 0 ] ) cache->azel[ 0 ] = astAnnul( cache->azel[ 0 ] );
      if( cache->azel[ 1 ] ) cache->azel[ 1 ] = astAnnul( cache->azel[ 1 ] );
      if( cache->skyframe ) cache->skyframe = astAnnul( cache->skyframe );

      cache = astFree( cache );
    }

    return NULL;
   }
  else if ( subnum < 0 || subnum >= SC2AST__NSUB )
  {
    *status = SAI__ERROR;
    sprintf( errmess, "Sub array number '%d' out of range\n", subnum );
    ErsRep( 0, status, errmess );
    return cache;
  }

  // Now initialize the returned pointers and check the inherited status
  *fset = AST__NULL;
  result = cache;
  if ( *status != SAI__OK )
  {
    return result;
  }


  /* Start an AST context.
   * This means we do not need to worry about annulling AST objects.
   * Note, there should be no "return" statements before the matching call to astEnd.
   */
  astBegin;

  // If no cache was supplied, allocate memory for a new cache and initialize it.
  if( ! cache )
  {
    cache = astMalloc( sizeof( sc2astCache ) );
    if( !cache )
    {
      return result;
    }

    for( isub = 0; isub < 8; isub++ )
    {
      cache->map[ isub ] = NULL;
      cache->frameset[ isub ] = NULL;
      cache->instap_x[ isub ] = AST__BAD;
      cache->instap_y[ isub ] = AST__BAD;
    }

    cache->azel[ 0 ] = NULL;
    cache->azel[ 1 ] = NULL;
    cache->skyframe = NULL;

    // When this function exits, return a pointer to the newly allocated cache structure.
    result = cache;
  }

  /* See if either of the instap values has changed.
   * If so, clear the cache for the requested subarray since the cached Mapping will have the old
   * instap values hard-wired into it.
   */
  if( ( instap && ( instap[ 0 ] != cache->instap_x[ subnum ] || instap[ 1 ] != cache->instap_y[ subnum ] ) ) ||
      ( !instap && ( 0.0 != cache->instap_x[ subnum ] || 0.0 != cache->instap_y[ subnum ] ) ) )
  {
    if( cache->map[ subnum ] )
    {
      cache->map[ subnum ] = astAnnul( cache->map[ subnum ] );
    }
    if( cache->frameset[ subnum ] )
    {
      cache->frameset[ subnum ] = astAnnul( cache->frameset[ subnum ] );
    }
  }

  /* The Mapping from GRID coords to AzEl coords can be thought of as divided
   * into two parts; the early part which goes from GRID to Cartesian Nasmyth
   * coords, and the later part which goes from Cartesian Nasmyth to spherical
   * AzEl coords. The nature of the early part is fixed for each subarray and
   * does not depend on any of the supplied parameters (other than sub-array
   * number). Therefore we can create the early part once for each sub-array
   * and cache them for later use. The later part depends on the "az", "el"
   * and "tai" parameters and so cannot be cached. Create the early part of
   * the required Mapping for the requested sub-array if it has not already
   * been created. The cached Mapping transforms positions within the Frame
   * encapsulated within the cached FrameSet into Cartesian Nasmyth coords in
   * radians.
   */
  if( !cache->map[ subnum ] )
  {
    /* Create an AST frame describing GRID coordinates within the subarray
     * and put it into the cached FrameSet for this subarray. The centre of
     * the first pixel has coords (1.0,1.0) in the GRID Frame.
     */
    cache->frameset[ subnum ] = astFrameSet( astFrame ( 2, "Domain=GRID" ), " " );

    /* We add a dummy 2D Frame to the FrameSet so that there is a Frame to
     * remove on the first call to astRemoveFrame below.
     */
    astAddFrame( cache->frameset[ subnum ], AST__BASE, astUnitMap( 2, " " ), astFrame( 2, " " ) );

    /* The GRID domain locates the [0][0] pixel at coordinates (1,1). Shift
     * these so that the [0][0] pixel is at the origin of a coordinate system.
     */
    zshift[0] = -1.0;
    zshift[1] = -1.0;
    zshiftmap = astShiftMap ( 2, zshift, " " );
    cache->map[ subnum ] = (AstMapping *) zshiftmap;

    // The mm coords now have to be rotated through an angle approximating a multiple of 90 degrees
    r = rotangle[ subnum ];
    if( subnum < 4 )
    {
      // 850 arrays
      rot[ 0 ] =  cos( r );
      rot[ 1 ] = -sin( r );
      rot[ 2 ] =  sin( r );
      rot[ 3 ] =  cos( r );
    }
    else
    {
      // 450 arrays
      rot[ 0 ] = -sin( r );
      rot[ 1 ] =  cos( r );
      rot[ 2 ] =  cos( r );
      rot[ 3 ] =  sin( r );
    }
    rotmap = (AstMapping *) astMatrixMap ( 2, 2, 0, rot, " " );
    cache->map[ subnum ] = (AstMapping *) astCmpMap( cache->map[ subnum ], rotmap, 1, " " );

    /* For each 450/850 subarray, the next Mapping creates FRAME450/FRAME850 (Right, UP) coordinates,
     * which are coordinates in millimetres with origin at the center of the focal plane as seen looking
     * into the elevation bearing.
     */

    shift[ 0 ] = xoff[ subnum ];
    shift[ 1 ] = yoff[ subnum ];
    shiftmap = astShiftMap( 2, shift, " " );
    cache->map[ subnum ] = (AstMapping *) astCmpMap( cache->map[ subnum ], shiftmap, 1, " " );

    printf("Setting up subarray %d coordinates: (%f,%f) rot %f\n", subnum, shift[0], shift[1], r);

    /* The mapping from pixel numbers to millimetres is a simple scaling,
     * because the pixel separation is the same in both coordinates and is
     * accurately constant. A ZoomMap can be used for this.
     */
    zoommap = astZoomMap ( 2, PIX2MM, " " );
    cache->map[ subnum ] = (AstMapping *) astCmpMap( cache->map[ subnum ], zoommap, 1, " " );


    /* Flips the "real" 450/850 image from (Right, UP) coordinates to (Right, Down),
     * as the following polynomial includes a correction for a vertical mirroring.
     */
	flipmap = astMatrixMap(2,2,0,flip," ");
    cache->map[ subnum ] = (AstMapping *) astCmpMap( cache->map[ subnum ], flipmap, 1, " " );


    /* Correct for polynomial distortion. Also includes correction for a vertical flip,
     * introduced in the previous frame, horizontal mirroring within the port, and a
     * conversion from mm to radians.
     * NOTE: POLYNOMIALS NOT CORRECT
     */

    // Port 1
    if (subnum == S8D || subnum == S4A)
	{
	  // Forward coeffs from FRAME850 to Nasmyth

	  // Forward coeffs -- real to ideal
      const double fts2poly_f[] =
      {
        /* x-coordinate */
        -0.0009892300     ,1, 0, 0,
        -2.449149E-005    ,1, 1, 0,
        1.326869E-006     ,1, 0, 1,
        -1.709081E-008    ,1, 2, 0,
        1.312598E-007     ,1, 1, 1,

        3.113639E-008     ,1, 0, 2,
        -5.028503E-010    ,1, 3, 0,
        -2.029447E-010    ,1, 2, 1,
        2.161438E-009     ,1, 1, 2,
        -3.662151E-010    ,1, 0, 3,

        -2.202885E-011    ,1, 4, 0,
        -3.148057E-011    ,1, 3, 1,
        1.687737E-011     ,1, 2, 2,
        -1.888702E-011    ,1, 1, 3,
        1.054700E-012     ,1, 0, 4,

        /* Y-coordinate */
        2.897341E-005     ,2, 0, 0,
        1.051795E-006     ,2, 1, 0,
        -2.124402E-005    ,2, 0, 1,
        -4.224470E-009    ,2, 2, 0,
        -5.139868E-009    ,2, 1, 1,

        1.348916E-007     ,2, 0, 2,
        -1.225325E-009    ,2, 3, 0,
        -8.158399E-010    ,2, 2, 1,
        -1.240002E-009    ,2, 1, 2,
        1.495935E-009     ,2, 0, 3,

        -1.117221E-011    ,2, 4, 0,
        -1.817811E-011    ,2, 3, 1,
        -3.721832E-011    ,2, 2, 2,
        3.114289E-012     ,2, 1, 3,
       -1.020290E-011    ,2, 0, 4
      };

      // Inverse coeffs -- ideal to real
      const double fts2poly_i[] =
      {
        // X-coordinate
        -42.02893         ,1, 0, 0,
        -46179.63         ,1, 1, 0,
        2298.290          ,1, 0, 1,
        -4959418.         ,1, 2, 0,
        1067312.          ,1, 1, 1,

        -298573.8         ,1, 0, 2,
        0.0000000         ,1, 3, 0,
        0.0000000         ,1, 2, 1,
        0.0000000         ,1, 1, 2,
        0.0000000         ,1, 0, 3,

        0.0000000         ,1, 4, 0,
        0.0000000         ,1, 3, 1,
        0.0000000         ,1, 2, 2,
        0.0000000         ,1, 1, 3,
        0.0000000         ,1, 0, 4,

        // Y-coordinate
        0.4681316         ,2, 0, 0,
        1145.850          ,2, 1, 0,
        -42535.76         ,2, 0, 1,
        1138404.          ,2, 2, 0,
        -292670.0         ,2, 1, 1,

        1791996.          ,2, 0, 2,
        0.0000000         ,2, 3, 0,
        0.0000000         ,2, 2, 1,
        0.0000000         ,2, 1, 2,
        0.0000000         ,2, 0, 3,

        0.0000000         ,2, 4, 0,
        0.0000000         ,2, 3, 1,
        0.0000000         ,2, 2, 2,
        0.0000000         ,2, 1, 3,
        0.0000000         ,2, 0, 4
      };

	  polymap = astPolyMap( 2, 2, 20, fts2poly_f, 20, fts2poly_i, " " );
	  cache->map[ subnum ] = (AstMapping *) astCmpMap( cache->map[ subnum ], polymap, 1, " " );
	}
	// Port 2
	else if (subnum == S8C || subnum == S4B)
	{
      // Forward coeffs -- real to ideal
      const double fts2poly_f[] =
      {
        // x-coordinate
        0.0009870064      ,1, 0, 0,
        -2.419827E-005    ,1, 1, 0,
        -1.290069E-006    ,1, 0, 1,
        5.506552E-010     ,1, 2, 0,
        1.367807E-007     ,1, 1, 1,

        -3.108237E-008    ,1, 0, 2,
        -1.426547E-010    ,1, 3, 0,
        -1.148542E-010    ,1, 2, 1,
        2.126468E-009     ,1, 1, 2,
        3.847782E-010     ,1, 0, 3,

        1.944890E-011     ,1, 4, 0,
        -2.692005E-011    ,1, 3, 1,
        -1.598600E-011    ,1, 2, 2,
        -1.856232E-011    ,1, 1, 3,
        -2.788737E-013    ,1, 0, 4,

        // y-coordinate
        2.857362E-005     ,2, 0, 0,
        -1.047448E-006    ,2, 1, 0,
        -2.122682E-005    ,2, 0, 1,
        -2.463341E-009    ,2, 2, 0,
        3.917507E-009     ,2, 1, 1,

        1.343425E-007     ,2, 0, 2,
        1.218829E-009     ,2, 3, 0,
        -8.140413E-010    ,2, 2, 1,
        1.238019E-009     ,2, 1, 2,
        1.465366E-009     ,2, 0, 3,

        -1.104270E-011    ,2, 4, 0,
        1.951404E-011     ,2, 3, 1,
        -3.638774E-011    ,2, 2, 2,
        -1.280725E-012    ,2, 1, 3,
        -1.008556E-011    ,2, 0, 4
      };

      // Inverse coeffs -- ideal to real
      const double fts2poly_i[] =
      {
        // X-coordinate
        42.04569          ,1, 0, 0,
        -46294.58         ,1, 1, 0,
        -2434.026         ,1, 0, 1,
        5049770.          ,1, 2, 0,
        1046491.          ,1, 1, 1,

        343590.4          ,1, 0, 2,
        0.0000000         ,1, 3, 0,
        0.0000000         ,1, 2, 1,
        0.0000000         ,1, 1, 2,
        0.0000000         ,1, 0, 3,

        0.0000000         ,1, 4, 0,
        0.0000000         ,1, 3, 1,
        0.0000000         ,1, 2, 2,
        0.0000000         ,1, 1, 3,
        0.0000000         ,1, 0, 4,

        // Y-coordinate
        0.5840508         ,2, 0, 0,
        -1353.268         ,2, 1, 0,
        -42666.27         ,2, 0, 1,
        1182390.          ,2, 2, 0,
        463245.7          ,2, 1, 1,

        1803338.          ,2, 0, 2,
        0.0000000         ,2, 3, 0,
        0.0000000         ,2, 2, 1,
        0.0000000         ,2, 1, 2,
        0.0000000         ,2, 0, 3,

        0.0000000         ,2, 4, 0,
        0.0000000         ,2, 3, 1,
        0.0000000         ,2, 2, 2,
        0.0000000         ,2, 1, 3,
        0.0000000         ,2, 0, 4
      };

	  polymap = astPolyMap( 2, 2, 20, fts2poly_f, 20, fts2poly_i, " " );
	  cache->map[ subnum ] = (AstMapping *) astCmpMap( cache->map[ subnum ], polymap, 1, " " );
	}

    // Converting from mm to radians is not needed (included in the polynomials).

    /* Apply focal plane offsets - if supplied. Note the effective values in
     * the cache so that we can spot if they are changed.
     */
    if (instap)
    {
      double totinstap[2];
      cache->instap_x[ subnum ] = instap[ 0 ];
      cache->instap_y[ subnum ] = instap[ 1 ];

      /* Should be XML X & Y and might be different for 850 and 450 focal plane.
       * The offset of the instrument center from center of rotation.
       */
      totinstap[ 0 ] = instap[0] - 0.0;
      totinstap[ 1 ] = instap[1] - 0.0;
      instapmap = astShiftMap( 2, totinstap, " " );
      cache->map[ subnum ] = (AstMapping *) astCmpMap( cache->map[ subnum ], instapmap, 1, " " );
    }
    else
    {
      cache->instap_x[ subnum ] = 0.0;
      cache->instap_y[ subnum ] = 0.0;
    }

    // Simplify the Cached Mapping.
    cache->map[ subnum ] = astSimplify( cache->map[ subnum ] );

    /* Exempt the cached AST objects from AST context handling.
     * This means that the pointers will not be annulled as a result of calling astEnd.
     * Therefore the objects need to be annulled explicitly when no longer needed.
     * This is done by calling this function with "subnum" set to SC2AST__NULLSUB.
     */
    astExempt( cache->map[ subnum ] );
    astExempt( cache->frameset[ subnum ] );
  }


  // If state is NULL we are simply returning the focal plane coordinate frames.
  if (!state)
  {
    AstFrame * fp_pos_frame;
    *fset = astClone( cache->frameset[ subnum ] );
    astRemoveFrame( *fset, AST__CURRENT );
    fp_pos_frame = astFrame( 2,
			      "Unit(1)=rad,Unit(2)=rad,Domain=FPLANE"
			      ",label(1)=FplaneX,label(2)=FplaneY" );
    astSetActiveUnit( fp_pos_frame, 1 );
    astAddFrame( *fset, AST__BASE, cache->map[ subnum ], fp_pos_frame );
    astSet( *fset, "unit(1)=arcsec,unit(2)=arcsec" );

    /* Return early rather than embed the subsequent code in a big else.
     * A bit dangerous if more is added at the end.
     */
    astExport( *fset );
    astEnd;
    return result;
  }

  /* Create a MatrixMap that rotates the focal plane so that the second
     pixel axis is parallel to the elevation axis. The rotation angle is
     just equal to the elevation of the boresight. */
  r = state->tcs_az_ac2;
  rot[ 0 ] =  cos( r );
  rot[ 1 ] = -sin( r );
  rot[ 2 ] =  sin( r );
  rot[ 3 ] =  cos( r );
  rotmap = (AstMapping *) astMatrixMap( 2, 2, 0, rot, " " );

  /* Create a ShiftMap that describes the SMU position correction. These
     corrections refer to AZEL offsets so should be applied after the above
     de-rotation MatrixMap. Replace rotmap with a suitable CmpMap combining
     the original rotmap and the ShiftMap. */

  if( state->smu_az_jig_x != VAL__BADD && state->smu_az_jig_y != VAL__BADD &&
      state->smu_az_chop_x != VAL__BADD && state->smu_az_chop_y != VAL__BADD &&
      ( state->smu_az_jig_x != 0.0 ||  state->smu_az_jig_y != 0.0 ||
        state->smu_az_chop_x != 0.0 ||  state->smu_az_chop_y != 0.0 ) ) {
     shifts[ 0 ] = (state->smu_az_jig_x + state->smu_az_chop_x) * AST__DD2R / 3600.0;
     shifts[ 1 ] = (state->smu_az_jig_y + state->smu_az_chop_y) * AST__DD2R / 3600.0;
     jigglemap = astShiftMap( 2, shifts, " " );
     rotmap = (AstMapping *) astCmpMap( rotmap, jigglemap, 1, " " );
  }

  /* Create a Mapping from these de-rotated, SMU corrected, Cartesian Nasmyth coords
     (in rads) to spherical AzEl coords (in rads). */
  azelmap = sc2ast_maketanmap( state->tcs_az_ac1, state->tcs_az_ac2,
                               cache->azel, status );

  /* Combine these with the cached Mapping (from GRID coords for subarray
     to Tanplane Nasmyth coords in rads), to get total Mapping from GRID
     coords to spherical AzEl in rads. */
  mapping = (AstMapping *) astCmpMap( cache->map[ subnum ],
                                      astCmpMap( rotmap, azelmap, 1, " " ),
                                      1, " " );

  /* If not already created, create a SkyFrame describing (Az,El).
   * Hard-wire the geodetic longitude and latitude of JCMT into this Frame.
   * Note, the Epoch value should be TDB, but we supply TT (=TAI+32.184 sec) instead
   * since the difference is only 1-2 milliseconds. We cache the created
   * SkyFrame to avoid the overhead of constantly re-creating it. The Epoch
   * is set every time though since this will vary from call to call. Also,
   * the SkyRef is set every time even though it only changes between
   * observations. We have to do this because the cache does not (yet) get cleared
   * between observations, especially in the DA. Benchmarking indicates that
   * there is no penalty in calling this every time for non-moving objects.
   */
  if( !cache->skyframe )
  {
    cache->skyframe = astSkyFrame ( "system=AzEl" );

    // Ast assumes longitude increases eastward, so change sign to be consistent with smf_calc_telpos here
    astSetD( cache->skyframe, "ObsLon", -telpos[0] );
    astSetD( cache->skyframe, "ObsLat", telpos[1] );

    astExempt( cache->skyframe );
  }

  /* Update the epoch, dut1 and sky reference.
   * Call this every time for skyref and dut1 since we can not ensure that
   * we will always have cleared the cache when a new observation starts
   */
  astSet( cache->skyframe,
           "Epoch=MJD %.*g,SkyRef(1)=%.*g, SkyRef(2)=%.*g,dut1=%.*g",
           DBL_DIG, state->tcs_tai + 32.184 / SPD,
           DBL_DIG, state->tcs_az_bc1,
           DBL_DIG, state->tcs_az_bc2,
           DBL_DIG, dut1 );

  /* Now modify the cached FrameSet to use the new Mapping and SkyFrame.
   * First remove the existing current Frame and then add in the new one.
   * Note, we add a copy of the SkyFrame rather than the cached SkyFrame
   * itself since the SkyFrame contained in the FrameSet will be modified
   * by later functions.
   */
   astRemoveFrame( cache->frameset[ subnum ], AST__CURRENT );
   astAddFrame( cache->frameset[ subnum ], AST__BASE, mapping, astClone( cache->skyframe ) );

  // Return the final FrameSet
  *fset = astClone( cache->frameset[ subnum ] );

  /* Export the returned FrameSet pointer, and then end the AST context.
   * This will annul all AST objects created since the matching call to astBegin,
   * except for those which have been exported using astExport or exempted
   * using astExempt.
   * The use of AST contexts requires that the function does not exit prematurely
   * before reaching the astEnd call. Therefore there should usually no "return"
   * statements within the body of the AST context.
   */
  astExport( *fset );
  astEnd;

  // Return a pointer to the cache.
  return result;
}
