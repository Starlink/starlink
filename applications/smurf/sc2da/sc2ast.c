#include <stdio.h>
#include <math.h>
#include <string.h>

#include "sae_par.h"
#include "ast.h"
#include "ndf.h"
#include "sc2ast.h"


#define MM2RAD 2.4945e-5               /* scale at array in radians */
#define MM2DEG 0.001429                /* scale at array in degrees */
#define PIBY2 1.57079632679
#define PIX2MM 1.135                   /* pixel interval in mm */
#define SPD 86400.0                    /* Seconds per day */
#define JCMT_LON "W155:28:37.20"       /* Longitude of JCMT */
#define JCMT_LAT "N19:49:22.11"        /* Geodetic latitude of JCMT */
const double RAD2DEG = 90.0 / PIBY2;   /* Convert Radians to degrees */

/*+ sc2ast_createwcs - create WCS description */

void sc2ast_createwcs
(
int subnum,             /* subarray number, 0-7 (given) */
double az,              /* Boresight azimuth in radians (given) */
double el,              /* Boresight elevation in radians (given) */
double tai,             /* TAI (supplied as a Modified Julian Date) */
AstFrameSet **fset,     /* constructed frameset (returned) */
int *status             /* global status (given and returned) */
)
/* Method :
     Build an AST frameset containing mappings from the original GRID
     coordinates of the bolometers to celestial coordinates. This
     includes the rotations and reflections relevant to each subarray and
     the distortion imposed by the SCUBA-2 optics.
   Authors :
     B.D.Kelly (bdk@roe.ac.uk)
     Tim Jenness (timj@jach.hawaii.edu)
     D.S. Berry (dsb@ast.man.ac.uk)
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
*/

{
   AstFitsChan *fitschan;
   AstFrame *cassframe;
   AstFrame *cassdegframe;
   AstFrame *focusframe;
   AstFrame *frame850;
   AstFrame *gridframe;
   AstFrame *mmframe;
   AstFrame *nasmythframe;
   AstFrame *revframe;
   AstFrame *rotframe;
   AstFrame *zpixelframe;
   AstFrameSet *frameset;
   AstFrameSet *fitsframeset;
   AstSkyFrame *skyframe;
   AstMapping *azelmap;
   AstMatrixMap *cassmap;
   AstMatrixMap *flipmap;
   AstMatrixMap *revmap;
   AstMatrixMap *rotmap;
   AstPolyMap *polymap;
   AstShiftMap *shiftmap;
   AstZoomMap *radmap;
   AstZoomMap *zoommap;
   AstShiftMap *zshiftmap;

   double a;                       /* subarray angle */
   double rotangle[8] =
      { 0.0, PIBY2, 2*PIBY2, 3*PIBY2, 0.0, PIBY2, 2*PIBY2, 3*PIBY2 };
   double rot[4];
   double reverse[8] =
      { -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0 };
   double rev[4];
   double shift[2];
   double zshift[2];

/* xoff and yoff are the distance in pixel units from the tracking centre
   to the [0][0] pixel in a subarray */

   double xoff[8] =
      { -41.5,  33.5,  41.5, -33.5, -41.5,  33.5,  41.5, -33.5 };
   double yoff[8] =
      {  33.5,  41.5, -33.5, -41.5,  33.5,  41.5, -33.5, -41.5 };

   double flip[8] =
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

/* Forward coefficients are from FRAME850 to Nasmyth */
   double coeff_f[] = 
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

   double coeff_i[] = 
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


   static double cassrot[4];


   if ( *status != SAI__OK ) return;

   /* Check subnum range */
   if ( subnum < 0 || subnum > 7 ) {
     printf("Sub array number '%d' out of range\n", subnum);
     *status = SAI__ERROR;
     return;
   }


/* The first step creates an AST frame corresponding to the subarray */

   gridframe = astFrame ( 2, "Domain=GRID" );

/* An AST FrameSet is initialised and will be built up as the various frames
   and the mappings between them are created */

   frameset = astFrameSet ( gridframe, "" );

/* The GRID domain locates the [0][0] pixel at coordinates (1,1). SHift
   these so that the [0][0] pixel is at the origin of a coordinate system */

   zshift[0] = -1.0;
   zshift[1] = -1.0;
   zshiftmap = astShiftMap ( 2, zshift, "" );
   zpixelframe = astFrame ( 2, "Domain=ZPIXEL" );
   astAddFrame ( frameset, AST__CURRENT, zshiftmap, zpixelframe );

/* The mapping from pixel numbers to millimetres is a simple scaling,
   because the pixel separation is the same in both coordinates and is
   accurately constant. A ZoomMap can be used for this. */

   zoommap = astZoomMap ( 2, PIX2MM, "" );
   mmframe = astFrame ( 2, "Domain=ARRAYMM" );
   astAddFrame ( frameset, AST__CURRENT, zoommap, mmframe );

/* The mmframe now has to be rotated through an angle approximating
   a multiple of 90 degrees */

   a = rotangle[subnum];
   rot[0] = cos(a);
   rot[1] = -sin(a);
   rot[2] = sin(a);
   rot[3] = cos(a);
   rotmap = astMatrixMap ( 2, 2, 0, rot, "" );
   rotframe = astFrame ( 2, "Domain=ARRAYROT" );
   astAddFrame ( frameset, AST__CURRENT, rotmap, rotframe );

/* The Y coordinate now has to be reversed */

   rev[0] = 1;
   rev[1] = 0;
   rev[2] = 0;
   rev[3] = reverse[subnum];
   revmap = astMatrixMap ( 2, 2, 0, rev, "" );
   revframe = astFrame ( 2, "Domain=ARRAYREV" );
   astAddFrame ( frameset, AST__CURRENT, revmap, revframe );

/* For each 450/850 subarray, a frame is created in FRAME450/FRAME850
   coordinates, which are coordinates in millimetres with origin at the
   optical axis. For a 450 subarray the axes are chosen such that the
   first axis maps onto Frame850 North and the second onto the inverted
   Focus850 UP once the dichroic reflection is taken into account. */

   shift[0] = xoff[subnum] * PIX2MM;
   shift[1] = yoff[subnum] * PIX2MM;
   shiftmap = astShiftMap ( 2, shift, "" );
   focusframe = astFrame ( 2, "Domain=ARRAYFOCUS" );
   astAddFrame ( frameset, AST__CURRENT, shiftmap, focusframe );

/* The final step into Frame850 coordinates is only needed for the 450
   subarrays. */

   rev[0] = 1;
   rev[1] = 0;
   rev[2] = 0;
   rev[3] = flip[subnum];
   flipmap = astMatrixMap ( 2, 2, 0, rev, "" );
   frame850 = astFrame ( 2, "Domain=FRAME850" );
   astSet ( frame850,
     "Title=FRAME850,Label(1)=NORTH,Unit(1)=mm,Label(2)=UP,Unit(2)=mm" );
   astAddFrame ( frameset, AST__CURRENT, flipmap, frame850 );

/* Correct for polynomial distortion */

   polymap = astPolyMap ( 2, 2, 14, coeff_f, 14, coeff_i, "" );
   nasmythframe = astFrame ( 2, "Domain=NASMYTH" );
   astSet ( nasmythframe,
     "Title=NASMYTH,Label(1)=NORTH,Unit(1)=mm,Label(2)=UP,Unit(2)=mm" );
   astAddFrame ( frameset, AST__CURRENT, polymap, nasmythframe );

/* Rotate into Cassegrain coordinates, "el" is telescope elevation. */

   cassrot[0] = cos(el);
   cassrot[1] = -sin(el);
   cassrot[2] = sin(el);
   cassrot[3] = cos(el);
   cassmap = astMatrixMap ( 2, 2, 0, cassrot, "" );
   cassframe = astFrame ( 2, 
     "Domain=CASS,Label(1)=Az,Unit(1)=mm,Label(2)=El,Unit(2)=mm" );
   astAddFrame ( frameset, AST__CURRENT, cassmap, cassframe );

/* Convert units from mm to degrees, MM2DEG is the effective plate scale. */

   radmap = astZoomMap ( 2, MM2DEG, "" );
   cassdegframe = astFrame ( 2, "Domain=CASSDEG" );
   astAddFrame ( frameset, AST__CURRENT, radmap, cassdegframe );

/* Create a celestial to tangent plane mapping via a FITS description. First 
   create the FitsChan, then store the required FITS WCS header cards in
   it, then rewind the FitsChan, then read a FrameSet from the FitsChan. */

   fitschan = astFitsChan ( NULL, NULL, "" );

   sc2ast_makefitschan ( 0.0, 0.0, 1.0, 1.0, az*RAD2DEG,
     el*RAD2DEG, "CLON-TAN", "CLAT-TAN", fitschan, status );

   astClear ( fitschan, "Card" );

   fitsframeset = astRead ( fitschan );

/* Extract the mapping going from tangent plane to spherical (Az,El) from
   the FrameSet returned by the above call to astRead. */

   azelmap = astGetMapping ( fitsframeset, AST__BASE, AST__CURRENT );

/* Create a SkyFrame describing (Az,El) and add it into the FrameSet
   using the above Mapping to connect it to the Cartesian Cassegrain
   coordinates Frame. Hard-wire the geodetic longitude and latitude of
   JCMT into this Frame. Note, the Epoch value should be TDB, but we
   supply TT (=TAI+32.184 sec) instead since the difference is only 1-2
   milliseconds. */
   skyframe = astSkyFrame ( "system=AzEl" );
   astSetC( skyframe, "ObsLon", JCMT_LON );
   astSetC( skyframe, "ObsLat", JCMT_LAT );   
   astSet( skyframe, "Epoch=MJD %.*g", DBL_DIG, tai + 32.184/SPD );
   astAddFrame ( frameset, AST__CURRENT, azelmap, skyframe );

   *fset = frameset;
}





/*+ sc2ast_getdomain - select a domain within a frameset */

void sc2ast_getdomain
(
char *name,               /* AST domain name (given) */
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
      astAnnul ( frame );
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
char *ctype1,             /* coordinate mapping type (given) */
char *ctype2,             /* coordinate mapping type (given) */
AstFitsChan *fitschan,   /* FitsChan to be filled (given and returned) */
int *status               /* global status (given and returned) */
)
/* Method :
     Contruct a set of FITS headers and put them into an AST FitsChan.
   Authors :
     B.D.Kelly (bdk@roe.ac.uk)
   History :
     13Apr2005 : original (bdk)
*/

{
   char fbuff[80];       /* store for a FITS record */

   if ( *status != SAI__OK ) return;
/*
   astSetFitsF ( fitschan, "CRPIX1", 0.0, " ", 0 );
   astSetFitsF ( fitschan, "CRPIX2", 0.0, " ", 0 );
   astSetFitsF ( fitschan, "CD1_1", 1.0, " ", 0 );
   astSetFitsF ( fitschan, "CD2_2", 1.0, " ", 0 );
   astSetFitsF ( fitschan, "CRVAL1", ra*90.0/PIBY2, " ", 0 );
   astSetFitsF ( fitschan, "CRVAL2", dec*90.0/PIBY2, " ", 0 );
   astSetFitsS ( fitschan, "CTYPE1", "RA---TAN", " ", 0 );
   astSetFitsS ( fitschan, "CTYPE2", "DEC--TAN", " ", 0 );
*/

   sprintf ( fbuff, "CRPIX1  = %e", crpix1 );
   astPutFits ( fitschan, fbuff, 0 );
   sprintf ( fbuff, "CRPIX2  = %e", crpix2 );
   astPutFits ( fitschan, fbuff, 0 );
   sprintf ( fbuff, "CD1_1   = %e", cd1_1 );
   astPutFits ( fitschan, fbuff, 0 );
   sprintf ( fbuff, "CD2_2   = %e", cd2_2 );
   astPutFits ( fitschan, fbuff, 0 );
   sprintf ( fbuff, "CRVAL1  = %e", crval1 );
   astPutFits ( fitschan, fbuff, 0 );
   sprintf ( fbuff, "CRVAL2  = %e", crval2 );
   astPutFits ( fitschan, fbuff, 0 );
   sprintf ( fbuff, "CTYPE1  = '%s'", ctype1 );
   astPutFits ( fitschan, fbuff, 0 );
   sprintf ( fbuff, "CTYPE2  = '%s'", ctype2 );
   astPutFits ( fitschan, fbuff, 0 );

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
char *name,             /* subarray name s8a-d, s4a-d (given) */
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
*/

{

   *subnum = 0; /* force initialisation */
 
   if ( *status != SAI__OK ) return;


   if ( strcmp ( name, "s8a" ) == 0 )
   {
      *subnum = 0;
   }
   else if ( strcmp ( name, "s8b" ) == 0 )
   {
      *subnum = 1;
   }
   else if ( strcmp ( name, "s8c" ) == 0 )
   {
      *subnum = 2;
   }
   else if ( strcmp ( name, "s8d" ) == 0 )
   {
      *subnum = 3;
   }
   else if ( strcmp ( name, "s4a" ) == 0 )
   {
      *subnum = 4;
   }
   else if ( strcmp ( name, "s4b" ) == 0 )
   {
      *subnum = 5;
   }
   else if ( strcmp ( name, "s4c" ) == 0 )
   {
      *subnum = 6;
   }
   else if ( strcmp ( name, "s4d" ) == 0 )
   {
      *subnum = 7;
   }
   else
   {
      *status = SAI__ERROR;
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
