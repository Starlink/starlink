/*
*+
*  Name:
*     smf_create_scuba2wcs.c

*  Purpose:
*     Create frameset representing SCUBA2 coordinate transformations

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_createwcs( int subnum, JCMTState *state, double fplane_off[2], 
*                    AstFrameSet **fset, int *status )

*  Arguments:
*     subnum = int (Given)
*        0--7 SCUBA2 subarray number. -1 to reset cache.
*     state = JCMTState* (Given)
*        Current JCMT state (time, pointing etc.)
*     fplane_off = double[2] (Given)
*        Additional focal plane offsets that may be applied.
*     fset = AstFrameSet** (Returned)
*        Constructed frameset.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Build an AST frameset containing mappings from SCUBA2 pixels to
*     to celestial coordinates. This function allocates static
*     resources (AST object pointers) which should be freed when no
*     longer needed by calling this function with "subnum" set to
*     -1. When this is done, the cached resources are freed and this
*     function returns without further action, returning a NULL
*     pointer in "*fset".

*  Authors:
*     Edward Chapin (UBC)
*     B.D.Kelly (bdk@roe.ac.uk)
*     Tim Jenness (timj@jach.hawaii.edu)
*     D.S. Berry (dsb@ast.man.ac.uk)
*     {enter_new_authors_here}

*  History:
*     2006-07-11 (EC):
*        Initial version duplicated from sc2ast_createwcs
*     2006-08-02 (EC):
*        - Renamed to smf_create_scuba2wcs
*        - Change API to take JCMTState
*        - Add SMU chop offsets + fplane_off
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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


#include <stdio.h>
#include <math.h>
#include <string.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"

/* Data Acquisition Includes */
#include "jcmt/state.h"

/* SMURF includes */
#include "smf.h"

#define MM2RAD 2.4945e-5               /* scale at array in radians */
#define PIBY2 1.57079632679
#define PIX2MM 1.135                   /* pixel interval in mm */
#define SPD 86400.0                    /* Seconds per day */
#define JCMT_LON "W155:28:37.20"       /* Longitude of JCMT */
#define JCMT_LAT "N19:49:22.11"        /* Geodetic latitude of JCMT */
const double RAD2DEG = 90.0 / PIBY2;   /* Convert Radians to degrees */

static char errmess[132];              /* For DRAMA error messages */


void smf_create_scuba2wcs( int subnum, JCMTState *state, double fplane_off[2], 
                           AstFrameSet **fset, int *status ) {

  /* Local Variables */
  AstMapping *azelmap;            /* tangent plane to spherical azel mapping */
  AstMapping *mapping;            /* total pixel -> azel mapping */
  double shifts[ 2 ];             /* size of shifts for jigglemap */ 
  AstShiftMap *jigglemap;         /* account for offsets in tangent plane */
  double a;                       /* rotation angle */
  double fplanerot[4];            /* Elements of fplane rotation matrix */
  AstMatrixMap *fplanerotmap;     /* Rotate fplane to align with AzEl*/
  AstShiftMap *fplane_offmap;     /* Mapping for focal plane shift */

  /* Required only for SCUBA2 */
  AstMatrixMap *flipmap;         /* Component of theoretical SCUBA2 model */
  AstMatrixMap *revmap;          /*               "                       */
  AstMatrixMap *rotmap;          /*               "                       */
  AstPolyMap *polymap;           /*               "                       */
  AstShiftMap *shiftmap;         /*               "                       */
  AstZoomMap *radmap;            /*               "                       */
  AstZoomMap *zoommap;           /*               "                       */
  AstShiftMap *zshiftmap;        /*               "                       */  
  const double rotangle[8] =     /* rotation of each subarray */
    { 0.0, PIBY2, 2*PIBY2, 3*PIBY2, 0.0, PIBY2, 2*PIBY2, 3*PIBY2 };

  double rot[4];
  const double reverse[8] =
    { -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0 };
  double rev[4];
  double shift[2];
  double zshift[2];
  
  const double xoff[8] =         /* x-distance in pixel units from tracking 
                                    centre to the [0][0] pixel in subarray */
    { -41.5,  33.5,  41.5, -33.5, -41.5,  33.5,  41.5, -33.5 };

  const double yoff[8] =         /* y-distance in pixel units from tracking 
                                    centre to the [0][0] pixel in subarray */
    {  33.5,  41.5, -33.5, -41.5,  33.5,  41.5, -33.5, -41.5 };
  
  const double flip[8] =         /* Flip each subarray */
    { 1.0, 1.0, 1.0, 1.0, -1.0, -1.0, -1.0, -1.0 };
  
  /* Distortion mappings. X and Y are in the distorted image, x and y are
     undistorted (Nasmyth). All units are mm.
     
     Least-squares fit to ray tracing provided by Tully gives
     
     x = 0.30037 + 0.99216 * X - 1.4428e-4 * X^2 - 3.612e-6 * X^3
         + 9.6e-4 * Y - 8.628e-6 * Y^2 - 2.986e-7 * Y^3
     
     y = 0.6518 + 1.0488e-3 * X - 6.58e-5 * X^2 -1.63e-7 * X^3
         + 0.98645 * Y - 2.218e-4 * Y^2 - 1.082e-6 * Y^3
     
     X = -0.30461 + 1.0079 * x + 1.502e-4 * x^2 + 3.757e-6 * x^3
         - 9.73e-4 * y + 9.79e-6 * y^2 + 3.135e-7 * y^3
     
     Y = -0.662328 - 1.082e-3 * x + 6.956e-5 * x^2 + 1.676e-7 * x^3
         + 1.0135 * y + 2.321e-4 * y^2 + 1.221e-6 * y^3     
  */

  const double coeff_f[] =       /* Forward coeffs from FRAME850 to Nas */
    { 
      /* x-coordinate */
      0.30037,     1.0, 0.0, 0.0,
      0.99216,     1.0, 1.0, 0.0,
      -1.4428e-4,  1.0, 2.0, 0.0,
      -3.612e-6,   1.0, 3.0, 0.0,
      9.6e-4,      1.0, 0.0, 1.0,
      -8.628e-6,   1.0, 0.0, 2.0,
      -2.986e-7,   1.0, 0.0, 3.0,
      /* y-coordinate */
      0.6518,      2.0, 0.0, 0.0,
      1.0488e-3,   2.0, 1.0, 0.0,
      -6.58e-5,    2.0, 2.0, 0.0,
      -1.63e-7,    2.0, 3.0, 0.0,
      0.98645,     2.0, 0.0, 1.0,
      -2.218e-4,   2.0, 0.0, 2.0,
      -1.082e-6,   2.0, 0.0, 3.0
    };
  
  const double coeff_i[] =       /* Inverse coeffs from Nas to FRAME850 */
    { 
      /* X-coordinate */
      -0.30461,    1.0, 0.0, 0.0,
      1.0079,      1.0, 1.0, 0.0,
      1.502e-4,    1.0, 2.0, 0.0,
      3.757e-6,    1.0, 3.0, 0.0,
      -9.73e-4,    1.0, 0.0, 1.0,
      9.79e-6,     1.0, 0.0, 2.0,
      3.135e-7,    1.0, 0.0, 3.0,
      /* Y-coordinate */
      -0.662328,   2.0, 0.0, 0.0,
      -1.082e-3,   2.0, 1.0, 0.0,
      6.956e-5,    2.0, 2.0, 0.0,
      1.676e-7,    2.0, 3.0, 0.0,
      1.0135,      2.0, 0.0, 1.0,
      2.321e-4,    2.0, 0.0, 2.0,
      1.221e-6,    2.0, 0.0, 3.0
    };
  
  
  /* A cache containing a FrameSet and a Mapping. The 
     FrameSet will contain a single Frame representing BOLO # in the 
     array. The result of applying the Mapping to this Frame will be 
     Cartesian (i.e. in the tangent plane) AzEl coords in rads. The 
     AST pointers in this cache are exempted from AST context handling, and
     so need to be released explicitly using astAnnul. This is done by 
     calling this function with the sub-frame number set to -1. 
  */


  static AstMapping *map_cache[ 8 ] = { NULL, NULL, NULL, NULL, NULL, NULL, 
					NULL, NULL };
  static AstFrameSet *frameset_cache[ 8 ] = { NULL, NULL, NULL, NULL, NULL, 
					      NULL, NULL, NULL };

  /* Cache the SkyFrame used to represent final spherical (Az,El) coords */
  static AstSkyFrame *skyframe = NULL;

  /* Cache used to hold Mappings needed in the tangent plane to celestial
     longitude,latitude Mapping. */
  static AstMapping *azel_cache[ 2 ] = { NULL, NULL };




  /* Main routine */

  /* Check the sub-array number. If it is -1, free the cached AST objects and 
     return. Otherwise, report an error if the value is illegal. We do
     this before checking the inherited status so that the memory is freed
     even if an error has occurred. */

  if( subnum == -1 ) {
    
    for( subnum = 0; subnum < 8; subnum++ ) {
      if( map_cache[ subnum ] ) {
	map_cache[ subnum ] = astAnnul( map_cache[ subnum ] );
      }
      if( frameset_cache[ subnum ] ) {
	frameset_cache[ subnum ] = astAnnul( frameset_cache[ subnum ] );
      }
    }
    
    if( azel_cache[ 0 ] ) azel_cache[ 0 ] = astAnnul( azel_cache[ 0 ] );
    if( azel_cache[ 1 ] ) azel_cache[ 1 ] = astAnnul( azel_cache[ 1 ] );
    if( skyframe ) skyframe = astAnnul( skyframe );
    
    return;
    
  } else if ( subnum < 0 || subnum > 7 ) {
    *status = SAI__ERROR;
    sprintf( errmess, "Sub array number '%d' out of range\n", subnum );
    ErsRep( 0, status, errmess );
    return; 
  }

  /* Now initialise the returned pointer and check the inherited status */
  *fset = AST__NULL;
  if ( *status != SAI__OK ) return;

  /* Start an AST context. This means we do not need to worry about
     annulling AST objects. Note, there should be no "return" statements
     before the matching call to astEnd. */
  astBegin;
  
  /* The Mapping from pixel number to AzEl coords can be thought of as
     divided into two parts; the early part which goes from pixel # to
     boresight focal plane offsets, and the later part which goes from
     focal plane boresight offsets to spherical AzEl coords. The
     nature of the early part is fixed for the instrument and does not
     depend on the JCMTState. Therefore we can create the early part
     once and cache them for later use. The later part depends on the
     JCMTState parameters and so cannot be cached. Create the early
     part of the required Mapping if it has not already been
     created. The cached Mapping transforms positions within the Frame
     encapsulated within the cached FrameSet into Tanplane focal
     plane offsets in radians. */

  if( !map_cache[ subnum ] ) {

    /* Create an AST frame describing GRID coordinates within the instrument
       and put it into the cached FrameSet for this subarray. The centre of
       the first pixel has coords (1.0,1.0) in the GRID Frame. */
    frameset_cache[ subnum ] = astFrameSet( astFrame ( 2, "Domain=GRID" ), 
					    "" );   
    
    /* We add a dummy 2D Frame to the FrameSet so that there is a Frame to
       remove on the first call to astRemoveFrame below. */
    astAddFrame( frameset_cache[ subnum ], AST__BASE, 
		 astUnitMap( 2, "" ), astFrame( 2, "" ) );
    

    /* Start SCUBA2-specific code */
      
    /* The GRID domain locates the [0][0] pixel at coordinates (1,1). Shift
       these so that the [0][0] pixel is at the origin of a coord. system */
    zshift[0] = -1.0;
    zshift[1] = -1.0;
    zshiftmap = astShiftMap ( 2, zshift, "" );
    map_cache[ subnum ] = (AstMapping *) zshiftmap;

    /* The mapping from pixel numbers to millimetres is a simple scaling,
       because the pixel separation is the same in both coordinates and is
       accurately constant. A ZoomMap can be used for this. */
    zoommap = astZoomMap ( 2, PIX2MM, "" );
    map_cache[ subnum ] = (AstMapping *) astCmpMap( map_cache[ subnum ], 
                                                    zoommap, 1, "" );

    /* The mm coords now have to be rotated through an angle approximating
       a multiple of 90 degrees */
    a = rotangle[ subnum ];
    rot[ 0 ] = cos( a );
    rot[ 1 ] = -sin( a );
    rot[ 2 ] = sin( a );
    rot[ 3 ] = cos( a );
    rotmap = astMatrixMap ( 2, 2, 0, rot, "" );
    map_cache[ subnum ] = (AstMapping *) astCmpMap( map_cache[ subnum ], 
                                                    rotmap, 1, "" );

    /* The Y coordinate now has to be reversed */
    rev[ 0 ] = 1;
    rev[ 1 ] = 0;
    rev[ 2 ] = 0;
    rev[ 3 ] = reverse[ subnum ];
    revmap = astMatrixMap ( 2, 2, 0, rev, "" );
    map_cache[ subnum ] = (AstMapping *) astCmpMap( map_cache[ subnum ], 
                                                    revmap, 1, "" );

    /* For each 450/850 subarray, the next Mapping creates FRAME450/FRAME850
       coordinates, which are coordinates in millimetres with origin at the
       optical axis. For a 450 subarray the axes are chosen such that the
       first axis maps onto Frame850 North and the second onto the inverted
       Focus850 UP once the dichroic reflection is taken into account. */
    shift[ 0 ] = xoff[ subnum ] * PIX2MM;
    shift[ 1 ] = yoff[ subnum ] * PIX2MM;
    shiftmap = astShiftMap( 2, shift, "" );
    map_cache[ subnum ] = (AstMapping *) astCmpMap( map_cache[ subnum ], 
                                                    shiftmap, 1, "" );

    /* The final step into Frame850 coordinates is only needed for the 450
       subarrays. */
    rev[ 0 ] = 1;
    rev[ 1 ] = 0;
    rev[ 2 ] = 0;
    rev[ 3 ] = flip[ subnum ];
    flipmap = astMatrixMap( 2, 2, 0, rev, "" );
    map_cache[ subnum ] = (AstMapping *) astCmpMap( map_cache[ subnum ], 
                                                    flipmap, 1, "" );

    /* Correct for polynomial distortion */      
    polymap = astPolyMap( 2, 2, 14, coeff_f, 14, coeff_i, "" );
    map_cache[ subnum ] = (AstMapping *) astCmpMap( map_cache[ subnum ], 
                                                    polymap, 1, "" );
      
    /* Convert from mm to radians (but these coords are still tanplane (x,y)
       (i.e. measured in the tangent plane) rather than spherical (lon,lat)
       measured on the sky. */
    radmap = astZoomMap ( 2, MM2RAD, "" );
    map_cache[ subnum ] = (AstMapping *) astCmpMap( map_cache[ subnum ], 
                                                    radmap, 1, "" );

    /* End SCUBA2-specific code */
    
    /* Apply focal plane offsets */
    fplane_offmap = astShiftMap( 2, fplane_off, "" );
    map_cache[ subnum ] = (AstMapping *) astCmpMap( map_cache[ subnum ], 
						    fplane_offmap, 1, "" );

    /* Simplify the Cached Mapping. */
    map_cache[ subnum ] = astSimplify( map_cache[ subnum ] );
    
    /* Exempt the cached AST objects from AST context handling. This means
       that the pointers will not be annulled as a result of calling astEnd. 
       Therefore the objects need to be annulled explicitly when no longer
       needed. this is done by calling this function with "subnum" set to 
       -1.*/
    astExempt( map_cache[ subnum ] );
    astExempt( frameset_cache[ subnum ] );
  }

  /* Apply the rotation from cached boresight focal plane coordinates to AzEl
     Tanplane coordinates */
  
  a = state->tcs_az_ang;
  fplanerot[ 0 ] = cos( a );
  fplanerot[ 1 ] = -sin( a );
  fplanerot[ 2 ] = sin( a );
  fplanerot[ 3 ] = cos( a );
  fplanerotmap = astMatrixMap ( 2, 2, 0, fplanerot, "" );
  mapping = (AstMapping *) astCmpMap( map_cache[ subnum ], 
				      fplanerotmap, 1, "" );
    
  /* Create a Mapping from tanplane AzEl coords (in rads) to spherical
     AzEl coords (in rads). */
  azelmap = smf_maketanmap( state->tcs_az_ac1, state->tcs_az_ac2, 
			    azel_cache, status );
  
  /* Calculate final mapping with SMU position correction only if needed */
  if( (!state->smu_az_jig_x)  && (!state->smu_az_jig_y) &&
      (!state->smu_az_chop_x) && (!state->smu_az_chop_y) ) {
    
    /* Combine these with the cached Mapping (from GRID coords for subarray 
       to Tanplane Nasmyth coords in rads), to get total Mapping from GRID 
       coords to spherical AzEl in rads. */

    mapping = (AstMapping *) astCmpMap( mapping, azelmap, 1, "" );    

  } else {
    /* Create a ShiftMap which moves the origin of projection plane (X,Y)
       coords to take account of the small offsets of SMU jiggle pattern. */
    shifts[ 0 ] = state->smu_az_jig_x + state->smu_az_chop_x;
    shifts[ 1 ] = state->smu_az_jig_y + state->smu_az_chop_y;
    jigglemap = astShiftMap( 2, shifts, "" );
    
    mapping = (AstMapping *) astCmpMap( mapping, 
					astCmpMap( jigglemap, azelmap, 1, 
						   "" ), 1, "" );
  }
  
  /* If not already created, create a SkyFrame describing (Az,El). Hard-wire 
     the geodetic longitude and latitude of JCMT into this Frame. Note, the 
     Epoch value should be TDB, but we supply TT (=TAI+32.184 sec) instead 
     since the difference is only 1-2 milliseconds. We cache the created 
     SkyFrame to avoid the overhead of constantly re-creating it. The Epoch 
     is set every time though since this will vary from call to call. */
  if( !skyframe ) {
    skyframe = astSkyFrame ( "system=AzEl" );
    astSetC( skyframe, "ObsLon", JCMT_LON );
    astSetC( skyframe, "ObsLat", JCMT_LAT );   
    astExempt( skyframe );
  }
  astSet( skyframe, "Epoch=MJD %.*g", DBL_DIG, state->rts_end + 32.184/SPD );

  /* Now modify the cached FrameSet to use the new Mapping and SkyFrame.
     First remove the existing current Frame and then add in the new one.
     Note, we add a copy of the SkyFrame rather than the cached SkyFrame 
     itself since the SkyFrame contained in the FrameSet will be modified 
     by later functions.  */
  astRemoveFrame( frameset_cache[ subnum ], AST__CURRENT );
  astAddFrame( frameset_cache[ subnum ], AST__BASE, mapping, 
	       astCopy( skyframe ) );

  /* Return the final FrameSet. */
  *fset = astClone( frameset_cache[ subnum ] );

  /* Export the returned FrameSet pointer, and then end the AST context. This 
     will annul all AST objects created since the matching call to astBegin,
     except for those which have been exported using astExport or exempted
     using astExempt. The use of AST contexts requires that the function
     does not exit prematurely before reaching the astEnd call. Therefore 
     there should usually no "return" statements within the body of the AST
     context. */
  astExport( *fset );
  astEnd;
}
