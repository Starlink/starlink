#include <stdio.h>
#include <math.h>
#include <string.h>

#include "star/slalib.h"
#include "sae_par.h"
#include "ast.h"
#include "ndf.h"
#include "Ers.h"

#include "sc2ast.h"


static char errmess[132];              /* For DRAMA error messages */

#define MM2RAD 2.4945e-5               /* scale at array in radians */
#define PIBY2 1.57079632679
#define PI 2*PIBY2 
#define PIX2MM 1.135                   /* pixel interval in mm */
const double RAD2DEG = 90.0 / PIBY2;   /* Convert Radians to degrees */

/*+ sc2ast_createwcs - create WCS description using a static cache */

void sc2ast_createwcs
(
int subnum,             /* subarray number, 0-7 (given). If -1 is
                           supplied the cached AST objects will be freed. */
const JCMTState *state, /* Current telescope state (time, pointing etc.) */
const double instap[2], /* Offset of subarray in the focal plane */ 
const double telpos[3], /* Geodetic W Lon/Lat/Alt of telescope (deg/deg/ign.)*/
AstFrameSet **fset,     /* constructed frameset (returned) */
int *status             /* global status (given and returned) */
)
/* Method :
     Build an AST frameset containing mappings from the original GRID
     coordinates of the bolometers to celestial coordinates. This
     includes the rotations and reflections relevant to each subarray and
     the distortion imposed by the SCUBA-2 optics.
 
     This function allocates static resources (AST object pointers) which
     should be freed when no longer needed by calling this function with
     "subnum" set to -1. In this is done, the cached resources are freed
     and this function returns without further action, returning a NULL 
     pointer in "*fset".

     If telescope state is NULL, a focal plane frameset will be returned.

   Notes :
     - Because of its use of a static cache, this function should be used 
     only when thread-safety is not an issue. For threaded applications,
     use sc2ast_createwcs2 instead.

     - After the first call the cached part of the frameset contains information
     specific to the SCUBA-2 instrument, observatory location etc. If you
     wish to subsequently calculate framesets for a different instrument
     or observatory location ensure that you first clear the cache by
     setting subnum=-1.

   Authors :
     B.D.Kelly (bdk@roe.ac.uk)
     Tim Jenness (timj@jach.hawaii.edu)
     D.S. Berry (dsb@ast.man.ac.uk)
     E.Chapin (echapin@phas.ubc.ca)

   History :
     01Apr2005 : original (bdk)
     21Apr2005 : update comments and add labels to some of the frames
                 (bdk)
     25Apr2005 : revise polynomials and plate scale (bdk)
     27Apr2005 : put in subarray orientations (bdk)
     10May2005 : remember xoff and yoff are in pixel units (bdk)
     10May2005 : fix syntax error in xoff and add more frame names (bdk)
     18May2005 : Check subnam range (timj)
     20Jun2005 : add zpixelframe (bdk)
     23Aug2005 : De-projection (Az,El) rather than (RA,Dec), and leave
                 final SkyFrame describing (Az,El). Also, replace supplied 
                 (ra,dec) parameters by (az,el,tai). (dsb)
     07Nov2005 : Remove duplicate elevation argument (timj)
                 Remove unused parallactic angle argument (timj)
     08Nov2005 : Use RAD2DEG global rather than 90*PIBY2 (timj)
     27Jan2006 : Use ErsRep rather than printf (timj)
     28Jan2006 : Annul all the mappings/frames to fix terrible leak (timj)
     07Feb2006 : Cache Mappings which do not depend on "az", "el" or "tai".
                 Do not create extra Frames unless specifically requested.
                 Use AST contexts for handling the annulling of AST objects.
                 Avoid use of FitsChans for extra speed. (dsb)
     17Feb2006 : Combine Nasmyth->Cassegrain rotation with the rotations
                    performed after the tangent plane->spherical projection.
                 Cache the SkyFrame used to represent AzEl coords.
                 Cache the FrameSets returned for each subnum
                 Do away with the facility for including intermediate
                    Frames (dsb)
     1Mar2006  : Check for (subnum==-1) before checking inherited status (dsb)
     2Mar2006  : const constant arrays (timj)
     12Apr2006 : added jig_az_x/y, remove extra_frames from interface (ec)
     18Apr2006 : Use a ShiftMap to apply jig_az_x/y (dsb)
     19Apr2006 : Only use shiftmap if non-zero jig_az_x/y (ec)
     7Aug2006  : Correct rotation matrix in sc2ast_maketanmap (dsb)
     7Aug2006  : Removed sc2ast_createwcs_compat + related kludges (ec)
     10Aug2006 : More corrections to the rotation matrix in sc2ast_maketanmap (dsb)
     10Aug2006 : Added "rot" parameter to sc2ast_maketanmap (dsb)
     07Sep2006 : Added "telpos" and "instap" arguments 
                 JCMTState now used for time/pointing (EC)
     19Dec2006 : If "state" is NULL return the focal plane frameset (TJ)
     20Feb2007 : Clear the cache if instap is changed.
     23Jul2008 : Set SkyRef (TJ)
     10Sep2008 : Renamed as sc2ast_createwcs2. This function is now just
                 a wrapper for sc2ast_createwcs2. (dsb)
*/
{
   static sc2astCache *cache = NULL;
   cache = sc2ast_createwcs2( subnum, state, instap, telpos, fset,     
                              cache, status );
}



/*+ sc2ast_createwcs2 - create WCS description using a supplied cache. */

sc2astCache *sc2ast_createwcs2
(
int subnum,             /* subarray number, 0-7 (given). If -1 is
                           supplied the cached AST objects will be freed. */
const JCMTState *state, /* Current telescope state (time, pointing etc.) */
const double instap[2], /* Offset of subarray in the focal plane */ 
const double telpos[3], /* Geodetic W Lon/Lat/Alt of telescope (deg/deg/ign.)*/
AstFrameSet **fset,     /* constructed frameset (returned) */
sc2astCache *cache,     /* A pointer to a structure holding cached info */
int *status             /* global status (given and returned) */
)
/* Method :
     Build an AST frameset containing mappings from the original GRID
     coordinates of the bolometers to celestial coordinates. This
     includes the rotations and reflections relevant to each subarray and
     the distortion imposed by the SCUBA-2 optics.
 
     This function uses a supplied structure to hold a cache of information 
     that does not change between invocations. If "cache" is supplied as
     NULL, then memory for a new structure is allocated and initialised, and 
     a pointer to it is returned. The contents of the returned structure
     should not be changed outside this function.

     The cache should be freed by calling this function with "subnum" set  
     to -1. In this is done, the cached resources are freed and this 
     function returns without further action, returning a NULL pointer in 
     "*fset", and a NULL pointer for the function value.

     If telescope state is NULL, a focal plane frameset will be returned.

   Notes :
     After the first call the cached part of the frameset contains information
     specific to the SCUBA-2 instrument, observatory location etc. If you
     wish to subsequently calculate framesets for a different instrument
     or observatory location ensure that you first clear the cache by
     setting subnum=-1.

   Authors :
     B.D.Kelly (bdk@roe.ac.uk)
     Tim Jenness (timj@jach.hawaii.edu)
     D.S. Berry (dsb@ast.man.ac.uk)
     E.Chapin (echapin@phas.ubc.ca)

   History :
     01Apr2005 : original (bdk)
     21Apr2005 : update comments and add labels to some of the frames
                 (bdk)
     25Apr2005 : revise polynomials and plate scale (bdk)
     27Apr2005 : put in subarray orientations (bdk)
     10May2005 : remember xoff and yoff are in pixel units (bdk)
     10May2005 : fix syntax error in xoff and add more frame names (bdk)
     18May2005 : Check subnam range (timj)
     20Jun2005 : add zpixelframe (bdk)
     23Aug2005 : De-projection (Az,El) rather than (RA,Dec), and leave
                 final SkyFrame describing (Az,El). Also, replace supplied 
                 (ra,dec) parameters by (az,el,tai). (dsb)
     07Nov2005 : Remove duplicate elevation argument (timj)
                 Remove unused parallactic angle argument (timj)
     08Nov2005 : Use RAD2DEG global rather than 90*PIBY2 (timj)
     27Jan2006 : Use ErsRep rather than printf (timj)
     28Jan2006 : Annul all the mappings/frames to fix terrible leak (timj)
     07Feb2006 : Cache Mappings which do not depend on "az", "el" or "tai".
                 Do not create extra Frames unless specifically requested.
                 Use AST contexts for handling the annulling of AST objects.
                 Avoid use of FitsChans for extra speed. (dsb)
     17Feb2006 : Combine Nasmyth->Cassegrain rotation with the rotations
                    performed after the tangent plane->spherical projection.
                 Cache the SkyFrame used to represent AzEl coords.
                 Cache the FrameSets returned for each subnum
                 Do away with the facility for including intermediate
                    Frames (dsb)
     1Mar2006  : Check for (subnum==-1) before checking inherited status (dsb)
     2Mar2006  : const constant arrays (timj)
     12Apr2006 : added jig_az_x/y, remove extra_frames from interface (ec)
     18Apr2006 : Use a ShiftMap to apply jig_az_x/y (dsb)
     19Apr2006 : Only use shiftmap if non-zero jig_az_x/y (ec)
     7Aug2006  : Correct rotation matrix in sc2ast_maketanmap (dsb)
     7Aug2006  : Removed sc2ast_createwcs_compat + related kludges (ec)
     10Aug2006 : More corrections to the rotation matrix in sc2ast_maketanmap (dsb)
     10Aug2006 : Added "rot" parameter to sc2ast_maketanmap (dsb)
     07Sep2006 : Added "telpos" and "instap" arguments 
                 JCMTState now used for time/pointing (EC)
     19Dec2006 : If "state" is NULL return the focal plane frameset (TJ)
     20Feb2007 : Clear the cache if instap is changed.
     23Jul2008 : Set SkyRef (TJ)
     10Sep2008 : Original created by renaming sc2ast_createwcs (dsb)
*/

{
   AstMapping *azelmap;
   AstMapping *mapping;
   AstMatrixMap *flipmap;
   AstMatrixMap *revmap;
   AstMatrixMap *rotmap;
   AstPolyMap *polymap;
   AstShiftMap *shiftmap;
   AstZoomMap *radmap;
   AstZoomMap *zoommap;
   AstShiftMap *zshiftmap;
   double shifts[ 2 ];
   AstShiftMap *instapmap;     
   AstShiftMap *jigglemap;
   sc2astCache *result;
   int isub;

   double a;                       /* subarray angle */
   const double rotangle[8] =
      { 0.0, PIBY2, 2*PIBY2, 3*PIBY2, 0.0, PIBY2, 2*PIBY2, 3*PIBY2 };
   double rot[4];
   const double reverse[8] =
      { -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0 };
   double rev[4];
   double shift[2];
   double zshift[2];

/* xoff and yoff are the distance in pixel units from the tracking centre
   to the [0][0] pixel in a subarray */

   const double xoff[8] =
      { -41.5,  33.5,  41.5, -33.5, -41.5,  33.5,  41.5, -33.5 };
   const double yoff[8] =
      {  33.5,  41.5, -33.5, -41.5,  33.5,  41.5, -33.5, -41.5 };

   const double flip[8] =
    /*{ 1.0, 1.0, 1.0, 1.0, -1.0, -1.0, -1.0, -1.0 };*/
      { -1.0, -1.0, -1.0, -1.0, 1.0, 1.0, 1.0, 1.0 };


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

/* Forward coefficients are from FRAME850 to Nasmyth */
   const double coeff_f[] = 
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

/* Inverse coefficients are from Nasmyth to FRAME850 */

   const double coeff_i[] = 
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

/* Check the sub-array number. If it is -1, free the cached AST objects and 
   the cache structure itself, and then return. Otherwise, report an error 
   if the value is illegal. We do this before checking the inherited status 
   so that the memory is freed even if an error has occurred. */
   if( subnum == -1 ) {
      if( cache ) {
         for( subnum = 0; subnum < 8; subnum++ ) {
            if( cache->map[ subnum ] ) {
               cache->map[ subnum ] = astAnnul( cache->map[ subnum ] );
            }
            if( cache->frameset[ subnum ] ) {
               cache->frameset[ subnum ] = astAnnul( cache->frameset[ subnum ] );
            }
         }
   
         if( cache->azel[ 0 ] ) cache->azel[ 0 ] = astAnnul( cache->azel[ 0 ] );
         if( cache->azel[ 1 ] ) cache->azel[ 1 ] = astAnnul( cache->azel[ 1 ] );
         if( cache->skyframe ) cache->skyframe = astAnnul( cache->skyframe );

         cache = astFree( cache );
      } 


      return NULL;

   } else if ( subnum < 0 || subnum > 7 ) {
     *status = SAI__ERROR;
     sprintf( errmess, "Sub array number '%d' out of range\n", subnum );
     ErsRep( 0, status, errmess );
     return cache;

   }

/* Now initialise the returned pointers and check the inherited status */
   *fset = AST__NULL;
   result = cache;
   if ( *status != SAI__OK ) return result;

/* Start an AST context. This means we do not need to worry about
   annulling AST objects. Note, there should be no "return" statements
   before the matching call to astEnd. */
   astBegin;

/* If no cache was supplied, allocate memory for a new cache and
   initialise it. */
   if( ! cache ) {
      cache = astMalloc( sizeof( sc2astCache ) );
      if( !cache ) return result;
      for( isub = 0; isub < 8; isub++ ) {
         cache->map[ isub ] = NULL;
         cache->frameset[ isub ] = NULL;
         cache->instap_x[ isub ] = AST__BAD;   
         cache->instap_y[ isub ] = AST__BAD;   
      }
      cache->azel[ 0 ] = NULL;
      cache->azel[ 1 ] = NULL;
      cache->skyframe = NULL;

/* When this function exits, return a pointer to the newly allocated cache 
   structure. */
      result = cache;
   }

/* See if either of the instap values has changed. If so, clear the cache
   for the requested subarray since the cached Mapping will have the old
   instap values hard-wired into it. */
   if( ( instap && ( instap[ 0 ] != cache->instap_x[ subnum ] ||
                     instap[ 1 ] != cache->instap_y[ subnum ] ) ) ||
       ( !instap && ( 0.0 != cache->instap_x[ subnum ] || 
                      0.0 != cache->instap_y[ subnum ] ) ) ){

      if( cache->map[ subnum ] ) {
         cache->map[ subnum ] = astAnnul( cache->map[ subnum ] );
      }
      if( cache->frameset[ subnum ] ) {
         cache->frameset[ subnum ] = astAnnul( cache->frameset[ subnum ] );
      }
   }

/* The Mapping from GRID coords to AzEl coords can be thought of as divided 
   into two parts; the early part which goes from GRID to Cartesian Nasmyth 
   coords, and the later part which goes from Cartesian Nasmyth to spherical 
   AzEl coords. The nature of the early part is fixed for each subarray and 
   does not depend on any of the supplied parameters (other than sub-array 
   number). Therefore we can create the early part once for each sub-array 
   and cache them for later use. The later part depends on the "az", "el" 
   and "tai" parameters and so cannot be cached. Create the early part of 
   the required Mapping for the requested sub-array if it has not already 
   been created. The cached Mapping transforms positions within the Frame
   encapsulated within the cached FrameSet into Cartesian Nasmyth coords in
   radians. */
   if( !cache->map[ subnum ] ) {

/* Create an AST frame describing GRID coordinates within the subarray
   and put it into the cached FrameSet for this subarray. The centre of
   the first pixel has coords (1.0,1.0) in the GRID Frame. */
      cache->frameset[ subnum ] = astFrameSet( astFrame ( 2, "Domain=GRID" ), 
                                              "" );   

/* We add a dummy 2D Frame to the FrameSet so that there is a Frame to
   remove on the first call to astRemoveFrame below. */
      astAddFrame( cache->frameset[ subnum ], AST__BASE, 
                   astUnitMap( 2, "" ), astFrame( 2, "" ) );

/* The GRID domain locates the [0][0] pixel at coordinates (1,1). Shift
   these so that the [0][0] pixel is at the origin of a coordinate system */

      zshift[0] = -1.0;
      zshift[1] = -1.0;
      zshiftmap = astShiftMap ( 2, zshift, "" );
      cache->map[ subnum ] = (AstMapping *) zshiftmap;

/* The mapping from pixel numbers to millimetres is a simple scaling,
   because the pixel separation is the same in both coordinates and is
   accurately constant. A ZoomMap can be used for this. */
      zoommap = astZoomMap ( 2, PIX2MM, "" );

      cache->map[ subnum ] = (AstMapping *) astCmpMap( cache->map[ subnum ], 
						      zoommap, 1, "" );

/* The mm coords now have to be rotated through an angle approximating
   a multiple of 90 degrees */
      a = rotangle[ subnum ];
      rot[ 0 ] = cos( a );
      rot[ 1 ] = -sin( a );
      rot[ 2 ] = sin( a );
      rot[ 3 ] = cos( a );
      rotmap = astMatrixMap ( 2, 2, 0, rot, "" );
      cache->map[ subnum ] = (AstMapping *) astCmpMap( cache->map[ subnum ], 
                                                      rotmap, 1, "" );

/* The Y coordinate now has to be reversed */
      rev[ 0 ] = 1;
      rev[ 1 ] = 0;
      rev[ 2 ] = 0;
      rev[ 3 ] = reverse[ subnum ];
      revmap = astMatrixMap ( 2, 2, 0, rev, "" );
      cache->map[ subnum ] = (AstMapping *) astCmpMap( cache->map[ subnum ], 
                                                      revmap, 1, "" );

/* For each 450/850 subarray, the next Mapping creates FRAME450/FRAME850
   coordinates, which are coordinates in millimetres with origin at the
   optical axis. For a 450 subarray the axes are chosen such that the
   first axis maps onto Frame850 North and the second onto the inverted
   Focus850 UP once the dichroic reflection is taken into account. */
      shift[ 0 ] = xoff[ subnum ] * PIX2MM;
      shift[ 1 ] = yoff[ subnum ] * PIX2MM;
      shiftmap = astShiftMap( 2, shift, "" );
      cache->map[ subnum ] = (AstMapping *) astCmpMap( cache->map[ subnum ], 
                                                      shiftmap, 1, "" );

/* The final step into Frame850 coordinates is only needed for the 450
   subarrays. */
      rev[ 0 ] = 1;
      rev[ 1 ] = 0;
      rev[ 2 ] = 0;
      rev[ 3 ] = flip[ subnum ];
      flipmap = astMatrixMap( 2, 2, 0, rev, "" );
      cache->map[ subnum ] = (AstMapping *) astCmpMap( cache->map[ subnum ], 
                                                      flipmap, 1, "" );

/* Correct for polynomial distortion */      

      polymap = astPolyMap( 2, 2, 14, coeff_f, 14, coeff_i, "" );
      cache->map[ subnum ] = (AstMapping *) astCmpMap( cache->map[ subnum ], 
						      polymap, 1, "" );
      
/* Convert from mm to radians (but these coords are still cartesian (x,y)
   (i.e. measured in the tangent plane) rather than spherical (lon,lat)
   measured on the sky. */
      radmap = astZoomMap ( 2, MM2RAD, "" );
      cache->map[ subnum ] = (AstMapping *) astCmpMap( cache->map[ subnum ], 
                                                      radmap, 1, "" );

/* Apply focal plane offsets - if supplied. Note the effective values in
   the cache so that we can spot if they are changed. */
      if (instap) {
	instapmap = astShiftMap( 2, instap, "" );
	cache->map[ subnum ] = (AstMapping *) astCmpMap( cache->map[ subnum ], 
							instapmap, 1, "" );
        cache->instap_x[ subnum ] = instap[ 0 ];
        cache->instap_y[ subnum ] = instap[ 1 ];
      } else {
        cache->instap_x[ subnum ] = 0.0;
        cache->instap_y[ subnum ] = 0.0;
      }

/* Simplify the Cached Mapping. */
      cache->map[ subnum ] = astSimplify( cache->map[ subnum ] );

/* Exempt the cached AST objects from AST context handling. This means
   that the pointers will not be annulled as a result of calling astEnd. 
   Therefore the objects need to be annulled explicitly when no longer
   needed. This is done by calling this function with "subnum" set to -1. */
      astExempt( cache->map[ subnum ] );
      astExempt( cache->frameset[ subnum ] );
   }


   /* If state is NULL we are simply returning the focal plane coordinate
      frames */
   if (!state) {
     AstFrame * fp_pos_frame;
     *fset = astClone( cache->frameset[ subnum ] );
     astRemoveFrame( *fset, AST__CURRENT );
     fp_pos_frame = astFrame( 2, 
			      "Unit(1)=rad,Unit(2)=rad,Domain=FPLANE"
			      ",label(1)=FplaneX,label(2)=FplaneY" );
     astSetActiveUnit( fp_pos_frame, 1 );
     astAddFrame( *fset, AST__BASE, cache->map[ subnum ],
		  fp_pos_frame );
     astSet( *fset, "unit(1)=arcsec,unit(2)=arcsec" );
     /* Return early rather than embed the subsequent code in a big else.
	A bit dangerous if more is added at the end */
     astExport( *fset );
     astEnd;
     return result;
   }


/* Create a Mapping from these Cartesian Nasmyth coords (in rads) to spherical 
   AzEl coords (in rads). */

   azelmap = sc2ast_maketanmap( state->tcs_az_ac1, state->tcs_az_ac2,
				cache->azel, state->tcs_az_ac2, status );

/* Calculate final mapping with SMU position correction only if needed */
   if( (!state->smu_az_jig_x)  && (!state->smu_az_jig_y) &&
       (!state->smu_az_chop_x) && (!state->smu_az_chop_y) ) {
    
/* Combine these with the cached Mapping (from GRID coords for subarray 
   to Tanplane Nasmyth coords in rads), to get total Mapping from GRID 
   coords to spherical AzEl in rads. */

      mapping = (AstMapping *) astCmpMap( cache->map[ subnum ], azelmap, 1, 
					  "" );

   } else {
/* Create a ShiftMap which moves the origin of projection plane (X,Y)
   coords to take account of the small offsets of SMU jiggle pattern. */
      shifts[ 0 ] = state->smu_az_jig_x + state->smu_az_chop_x;
      shifts[ 1 ] = state->smu_az_jig_y + state->smu_az_chop_y;
      jigglemap = astShiftMap( 2, shifts, "" );


      mapping = (AstMapping *) astCmpMap( cache->map[ subnum ], 
					  astCmpMap( jigglemap, azelmap, 1, 
						     "" ), 1, "" );
   }

/* If not already created, create a SkyFrame describing (Az,El). Hard-wire 
   the geodetic longitude and latitude of JCMT into this Frame. Note, the 
   Epoch value should be TDB, but we supply TT (=TAI+32.184 sec) instead 
   since the difference is only 1-2 milliseconds. We cache the created 
   SkyFrame to avoid the overhead of constantly re-creating it. The Epoch 
   is set every time though since this will vary from call to call. Also,
   the SkyRef is set every time even though it only changes between
   observations. We have to do this because the cache does not (yet) get cleared
   between observations, especially in the DA. Benchmarking indicates that
   there is no penalty in calling this every time for non-moving objects.
*/
   if( !cache->skyframe ) {
      cache->skyframe = astSkyFrame ( "system=AzEl" );
      
      /* Ast assumes longitude increases eastward, so change sign to
	 be consistent with smf_calc_telpos here */
      astSetD( cache->skyframe, "ObsLon", -telpos[0] );
      astSetD( cache->skyframe, "ObsLat", telpos[1] );

      astExempt( cache->skyframe );
   }

   astSet( cache->skyframe, "Epoch=MJD %.*g", DBL_DIG, state->tcs_tai + 32.184/SC2AST_SPD );

/* Call this every time since we can not ensure that we will always
   have cleared the cache when a new observation starts */
   astSetD( cache->skyframe, "SkyRef(1)", state->tcs_az_bc1 );
   astSetD( cache->skyframe, "SkyRef(2)", state->tcs_az_bc2 );

/* Now modify the cached FrameSet to use the new Mapping and SkyFrame.
   First remove the existing current Frame and then add in the new one.
   Note, we add a copy of the SkyFrame rather than the cached SkyFrame 
   itself since the SkyFrame contained in the FrameSet will be modified 
   by later functions.  */
   astRemoveFrame( cache->frameset[ subnum ], AST__CURRENT );
   astAddFrame( cache->frameset[ subnum ], AST__BASE, mapping, 
                astCopy( cache->skyframe ) );

/* Return the final FrameSet. */
   *fset = astClone( cache->frameset[ subnum ] );

/* Export the returned FrameSet pointer, and then end the AST context. This 
   will annul all AST objects created since the matching call to astBegin,
   except for those which have been exported using astExport or exempted
   using astExempt. The use of AST contexts requires that the function
   does not exit prematurely before reaching the astEnd call. Therefore 
   there should usually no "return" statements within the body of the AST
   context. */
   astExport( *fset );
   astEnd;

/* Return a pointer to the cache. */
   return result;
}


/*+ sc2ast_getdomain - select a domain within a frameset */

void sc2ast_getdomain
(
const char *name,         /* AST domain name (given) */
AstFrameSet *fset,        /* modified frameset (given and returned) */
int *status               /* global status (given and returned) */
)
/* Method :
    Find the last frame within the frameset matching the given domain
    name, and make it the current frame.
   Authors :
     B.D.Kelly (bdk@roe.ac.uk)
   History :
     10May2005 : original (bdk)
*/

{
   AstFrame *frame;    /* individual frame */
   int frameno;        /* number of frame */
   int nframes;        /* number of frames in the frameset */
   int j;              /* loop counter */

   if ( *status != SAI__OK ) return;

   astBegin;

/* Search for the domain name */

   nframes = astGetI ( fset, "NFRAME" );

   frameno = 0;
   for ( j=nframes; j>0; j-- )
   {
      frame = astGetFrame ( fset, j );
      if ( strcmp ( name, astGetC ( frame, "DOMAIN" )) == 0 )
      {
         frameno = j;
         break;
      }
      frame = astAnnul ( frame );
   }

   astEnd;

   if ( frameno != 0  )
   {
      astSetI ( fset, "CURRENT", frameno );      
   }
}


/*+ sc2ast_makefitschan - create a set of FITS headers in a FitsChan */

void sc2ast_makefitschan
(
double crpix1,            /* index of reference point (given) */
double crpix2,            /* index of reference point (given) */
double cd1_1,             /* data increment (given) */
double cd2_2,             /* data increment (given) */
double crval1,            /* reference coordinate (given) */
double crval2,            /* reference coordinate (given) */
const char *ctype1,       /* coordinate mapping type (given) */
const char *ctype2,       /* coordinate mapping type (given) */
AstFitsChan *fitschan,    /* FitsChan to be filled (given and returned) */
int *status               /* global status (given and returned) */
)
/* Method :
     Contruct a set of FITS headers and put them into an AST FitsChan.
   Authors :
     B.D.Kelly (bdk@roe.ac.uk)
     T.Jenness (timj@jach)
   History :
     13Apr2005 : original (bdk)
     08Nov2005 : Use astSetFits rather than intermediate strings (timj)
*/

{

   if ( *status != SAI__OK ) return;

   astSetFitsF ( fitschan, "CRPIX1", crpix1, " ", 0 );
   astSetFitsF ( fitschan, "CRPIX2", crpix2, " ", 0 );
   astSetFitsF ( fitschan, "CD1_1", cd1_1, " ", 0 );
   astSetFitsF ( fitschan, "CD2_2", cd2_2, " ", 0 );
   astSetFitsF ( fitschan, "CRVAL1", crval1, " ", 0 );
   astSetFitsF ( fitschan, "CRVAL2", crval2, " ", 0 );
   astSetFitsS ( fitschan, "CTYPE1", ctype1, " ", 0 );
   astSetFitsS ( fitschan, "CTYPE2", ctype2, " ", 0 );

   return;
}



/*+ sc2ast_moveframe - move the base frame within a frameset */

void sc2ast_moveframe
(
double x,                 /* X coordinate offset in pixels (given) */
double y,                 /* Y coordinate offset in pixels (given) */
AstFrameSet *fset,        /* modified frameset (given and returned) */
int *status               /* global status (given and returned) */
)
/* Method :
    Use astRemapFrame to shift the base frame of the frameset.
   Authors :
     B.D.Kelly (bdk@roe.ac.uk)
   History :
     26May2005 : original (bdk)
     07Jun2005 : apply shift in pixels (bdk)
*/

{
   AstShiftMap *shiftmap;
   double shift[2];

   if ( *status != SAI__OK ) return;

   astBegin;

/* Set up a shift mapping */

   shift[0] = x;
   shift[1] = y;
   shiftmap = astShiftMap ( 2, shift, "" );

/* Remap the base frame */

   astRemapFrame ( fset, AST__BASE, shiftmap );

   astEnd;

}


/*+ sc2ast_name2num - convert subarray name to id number */

void sc2ast_name2num
(
const char *name,             /* subarray name s8a-d, s4a-d (given) */
int *subnum,            /* subarray number, 0-7 (returned) */
int *status             /* global status (given and returned) */
)
/* Method :
    Convert the SCUBA-2 standard name of a subarray into its
    corresponding index number.
   Authors :
     B.D.Kelly (bdk@roe.ac.uk)
     Tim Jenness (timj@jach.hawaii.edu)
   History :
     27Apr2005 : original (bdk)
     18May2005 : force initialisation of returned value (timj)
     27Jan2006 : use strncmp rather than strcmp (timj)
                 Associate error message with error condition (timj)
     20Mar2007 : const "name" argument (timj)
*/

{

   *subnum = 0; /* force initialisation */
 
   if ( *status != SAI__OK ) return;


   if ( strncmp ( name, "s8a", 3 ) == 0 )
   {
      *subnum = 0;
   }
   else if ( strncmp ( name, "s8b", 3 ) == 0 )
   {
      *subnum = 1;
   }
   else if ( strncmp ( name, "s8c", 3 ) == 0 )
   {
      *subnum = 2;
   }
   else if ( strncmp ( name, "s8d", 3 ) == 0 )
   {
      *subnum = 3;
   }
   else if ( strncmp ( name, "s4a", 3 ) == 0 )
   {
      *subnum = 4;
   }
   else if ( strncmp ( name, "s4b", 3 ) == 0 )
   {
      *subnum = 5;
   }
   else if ( strncmp ( name, "s4c", 3 ) == 0 )
   {
      *subnum = 6;
   }
   else if ( strncmp ( name, "s4d", 3 ) == 0 )
   {
      *subnum = 7;
   }
   else
   {
      *status = SAI__ERROR;
      sprintf( errmess,
	       "Error converting subarray name from '%s' to number",
	       name);
      ErsRep( 0, status, errmess);
   }
}



/*+ sc2ast_polframest - create a frameset for polarimetry */

void sc2ast_polframeset
(
AstFrameSet *frameset,  /* 2-D frameset (given) */
AstFrameSet **fset,     /* constructed 3-D frameset (returned) */
int *status             /* global status (given and returned) */
)
/* Method :
    Add a unit mapping onto an existing 2-D frameset to create a 3-D
    frameset.
   Authors :
     B.D.Kelly (bdk@roe.ac.uk)
   History :
     14Jun2005 : original (bdk)
*/

{
   AstCmpMap *cmpmap;
   AstFrame *extframe;
   AstFrame *gridframe;
   AstCmpFrame *polframe;
   AstSkyFrame *skyframe;
   AstUnitMap *unitmap;

 
   if ( *status != SAI__OK ) return;


/* The first step creates an AST frame corresponding to the subarray */

   gridframe = astFrame ( 3, "Domain=GRID" );
   extframe = astFrame ( 1, "Domain=GRID" );

/* An AST FrameSet is initialised and will be built up as the various frames
   and the mappings between them are created */

   *fset = astFrameSet ( gridframe, "" );

   unitmap = astUnitMap ( 1, " " );
   cmpmap = astCmpMap ( frameset, unitmap, 0, " " );
   skyframe = astSkyFrame ( " " );
   polframe = astCmpFrame ( skyframe, extframe, " " );
   astAddFrame ( *fset, AST__CURRENT, cmpmap, polframe );

}

/*+ sc2ast_maketanmap - create a Mapping representing a tangent plane 
                        projection */

AstMapping *sc2ast_maketanmap
(
double lon,              /* Celestial longitude at ref point (rads) */
double lat,              /* Celestial latitude at ref point (rads) */
AstMapping *cache[ 2 ],  /* Cached Mappings (supply as NULL on 1st call) */
double rot,              /* Rotation needed to align input Y axis with North */
int *status              /* global status (given and returned) */
)
/* Method :
    The forward transformation of the returned Mapping transforms 
    cartesian tangent plane offsets in radians, into celestial longitude
    and latitude values, in radians. The reference point of the tangent
    plane is put at the supplied longitude and latitude position. 

    The "cache" array should be filled with NULL values before the first
    call to this function. It will be returned holding AST pointers to
    Mappings which will be needed on subsequent calls (these pointers are
    exempted from AST context handling).

    If the input Cartesian coordinates represent Nasmyth (X,Y) coords,
    then the "rot" angle should be the elevation of the boresight.

   Authors :
     D.S.Berry (dsb@ast.man.ac.uk)

   History :
     10Feb2006 : original (dsb)
     1Sep2008  : Set the SphMap attribute "UnitRadius" so that SphMaps 
                 can be simplified. (dsb)
*/

{
   AstMapping *result;    
   AstMatrixMap *matmap;
   AstCmpMap *m1;
   AstWcsMap *wcsmap;
   double mat[ 3 ][ 3 ];

/* SLA does not use const so we have to copy from const to a temporary */
   char eul[4];

/* Check the inherited status. */
   if ( *status != SAI__OK ) return NULL;

/* If required, create a SphMap for converting spherical cartesian
   (x,y,z) positions to (lon,lat) positions. */
   if( !cache[ 0 ] ) {
      cache[ 0 ] = (AstMapping *) astSphMap( "UnitRadius=1" );
      astExempt( cache[ 0 ] );
   }

/* If required, create a CmpMap holding a WcsMap followed by an inverted
   SphMap. */
   if( !cache[ 1 ] ) {
      wcsmap = astWcsMap( 2, AST__TAN, 1, 2, "" );
      astInvert( wcsmap );
      astInvert( cache[ 0 ] );
      cache[ 1 ] = (AstMapping *) astCmpMap( wcsmap, cache[ 0 ], 1, "" );
      astInvert( cache[ 0 ] );
      wcsmap = astAnnul( wcsmap );
      astExempt( cache[ 1 ] );
   }

/* The required Mapping consists of the "cache[ 1 ]" Mapping, followed by
   a suitable MatrixMap which rotates the tangent point to the requested
   spherical (az,el) coordinates, and rotates the Caresian Y axis to 
   celestial north (elevation). followed by the "cache[ 0 ]" Mapping. The
   logic follows that of FITS-WCS paper II (which is what the WcsMap
   class assumes). The reference point of the TAN projection is at the 
   north pole of the "native" spherical coordinate system. The matrix map
   first rotates about the Z axis by -rot (to rotate the Cartesian Y axis to
   north), then rotates about the Y axis by -(pi/2-lat) (to rotate the
   reference point from the native north pole to the celestial north pole), 
   then rotates about the Z axis by -lon (to rotate the prime native meridian 
   to the prime celestial meridian). Here the Z axis points to the north
   pole, the X axis points to (lon,lat)=(0,0) and the Y axis points to 
   (lon,lat) = (90 degs,0) (the slalib convention). */

   strcpy(eul, "ZYZ" );
   slaDeuler( eul, -rot, -(PIBY2-lat), -lon, mat ); 
   matmap = astMatrixMap( 3, 3, 0, (double *) mat, "" );

/* Create the required compound Mapping. */
   m1 = astCmpMap( cache[ 1 ], matmap, 1, "" );
   result = (AstMapping *) astCmpMap( m1, cache[ 0 ], 1, "" );
   matmap = astAnnul( matmap );
   m1 = astAnnul( m1 );

/* Return the required Mapping.*/
   return result;

}

/*+ sc2ast_set_output_system - set the output SYSTEM to match the
                              equivalent JCMT tracking system
 */

void sc2ast_set_output_system
(
 const char *trsys,      /* JCMT tracking system (given) */
 AstFrameSet *fset,      /* Frameset to update (updated) */
 int * status            /* inherited status (given & returned ) */
)

/*
 *  Purpose:
 *    Sets the SYSTEM attribute based on JCMT naming convention.

 *  Description:
 *    Sets the AST output system and also sets the SkyRefIs and
 *    AlignOffset attributes if this system is associated with
 *    a moving frame.

 *  Authors:
 *     TIMJ: Tim Jenness (JAC, Hawaii)

 *  History:
 *     23-JUL-2008 (TIMJ):
 *        Original version.
 *     11-AUG-2008 (TIMJ):
 *        Set AlignOffset and SkyRefIs if appropriate.

 */
{
  const char * astsys = NULL;

  if (*status != SAI__OK) return;

  astsys = sc2ast_convert_system( trsys, status );
  if ( *status == SAI__OK) {
    astSetC( fset, "SYSTEM", astsys );

    astsys = astGetC( fset, "SYSTEM");
    if (strcmp(astsys,"AZEL") == 0 || strcmp(astsys, "GAPPT") == 0 ) {
      astSet( fset, "SkyRefIs=Origin,AlignOffset=1" );
    }
  }

}



/*+ sc2ast_convert_system - convert JCMT coordinate system to AST coordinate
                           system
 */
const char * sc2ast_convert_system 
(
 const char *label,   /* Input JCMT coordinate system (given) */
 int * status         /* Inherited status (given & returned) */
)

/*
*  Purpose:
*     Convert a JCMT label for a celestial coordinate system into the
*     corresponding AST SkyFrame System value.

*  Returned Value:
*     A point to a static string holding the equivalent AST value, or
*     "" if there is no equivalent AST value.

*  Description:
*     This function converts a JCMT label for a celestial coordinate system 
*     into the corresponding AST SkyFrame System value. It reports an error
*     if the system is not supported by AST.

*  Authors:
*     DSB: David S. Berry (JAC, UCLan)
*     EC: Ed Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     25-SEP-2006 (DSB):
*        Original version.
*     13-DEC-2007 (EC):
*        Use strncmp instead of strcmp
*     23-JUL-2008 (TIMJ):
*        Copy from smf_convert_system (now deleted) so that the
*        DA can do the conversion
*/

{

/* Local Variables */
   const char *result = "";   /* Returned pointer */

/* Check inherited status */
   if (*status != SAI__OK) return result;

/* Compare the supplied labelwith each known type. */
   if( !strncmp( label, "AZEL", 4 ) ) {
      result = "AZEL";

   } else if( !strncmp( label, "APP", 3 ) ) {
      result = "GAPPT";

   } else if( !strncmp( label, "GAL", 3 ) ) {
      result = "GALACTIC";

   } else if( !strncmp( label, "ICRS", 4 ) ||
              !strncmp( label, "ICRF", 4 ) ) {
      result = "ICRS";

   } else if( !strncmp( label, "B1950", 5 ) ) {
      result = "FK4";

   } else if( !strncmp( label, "J2000", 5 ) ) {
      result = "FK5";

   } else {
      *status = SAI__ERROR;
      sprintf( errmess,
               "The JCMT coordinate system %s does not "
               "have an equivalent AST System value.", label );
      ErsRep( 0, status, errmess);
   }

   return result;
}

