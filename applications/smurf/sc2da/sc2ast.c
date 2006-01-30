#include <stdio.h>
#include <math.h>
#include <string.h>

#include "sae_par.h"
#include "ast.h"
#include "ndf.h"
#include "sc2ast.h"
#include "Ers.h"

/* -----------------------------------------------------------------------*/
/* Temporary kludge required for sc2ast_createwcs_compat   EC 05Dec2005   */
/*                       and for sc2ast_kludgemodjuldate                  */
#ifndef PI
#define PI 3.14159265358979            /* math constant */
#endif

#ifndef dmod
#define dmod(A,B) ((B)!=0.0?((A)*(B)>0.0?(A)-(B)*floor((A)/(B))\
                                        :(A)+(B)*floor(-(A)/(B))):(A))
#endif

#ifndef DS2R
#define DS2R 7.2722052166430399038487115353692196393452995355905e-5
#endif
/* -----------------------------------------------------------------------*/

static char errmess[132];              /* For DRAMA error messages */

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
     27Jan2006 : Use ErsRep rather than printf (timj)
     28Jan2006 : Annul all the mappings/frames to fix terrible leak (timj)
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

   *fset = AST__NULL;
   if ( *status != SAI__OK ) return;

   /* Check subnum range */
   if ( subnum < 0 || subnum > 7 ) {
     *status = SAI__ERROR;
     sprintf(errmess, "Sub array number '%d' out of range\n", subnum);
     ErsRep(0, status, errmess);
     return;
   }


/* The first step creates an AST frame corresponding to the subarray */

   gridframe = astFrame ( 2, "Domain=GRID" );

/* An AST FrameSet is initialised and will be built up as the various frames
   and the mappings between them are created */

   frameset = astFrameSet ( gridframe, "" );
   astAnnul(gridframe);

/* The GRID domain locates the [0][0] pixel at coordinates (1,1). SHift
   these so that the [0][0] pixel is at the origin of a coordinate system */

   zshift[0] = -1.0;
   zshift[1] = -1.0;
   zshiftmap = astShiftMap ( 2, zshift, "" );
   zpixelframe = astFrame ( 2, "Domain=ZPIXEL" );
   astAddFrame ( frameset, AST__CURRENT, zshiftmap, zpixelframe );
   astAnnul(zshiftmap);
   astAnnul(zpixelframe);

/* The mapping from pixel numbers to millimetres is a simple scaling,
   because the pixel separation is the same in both coordinates and is
   accurately constant. A ZoomMap can be used for this. */

   zoommap = astZoomMap ( 2, PIX2MM, "" );
   mmframe = astFrame ( 2, "Domain=ARRAYMM" );
   astAddFrame ( frameset, AST__CURRENT, zoommap, mmframe );
   astAnnul(zoommap);
   astAnnul(mmframe);

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
   astAnnul(rotmap);
   astAnnul(rotframe);

/* The Y coordinate now has to be reversed */

   rev[0] = 1;
   rev[1] = 0;
   rev[2] = 0;
   rev[3] = reverse[subnum];
   revmap = astMatrixMap ( 2, 2, 0, rev, "" );
   revframe = astFrame ( 2, "Domain=ARRAYREV" );
   astAddFrame ( frameset, AST__CURRENT, revmap, revframe );
   astAnnul(revmap);
   astAnnul(revframe);

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
   astAnnul(focusframe);
   astAnnul(shiftmap);

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
   astAnnul(flipmap);
   astAnnul(frame850);

/* Correct for polynomial distortion */

   polymap = astPolyMap ( 2, 2, 14, coeff_f, 14, coeff_i, "" );
   nasmythframe = astFrame ( 2, "Domain=NASMYTH" );
   astSet ( nasmythframe,
     "Title=NASMYTH,Label(1)=NORTH,Unit(1)=mm,Label(2)=UP,Unit(2)=mm" );
   astAddFrame ( frameset, AST__CURRENT, polymap, nasmythframe );
   astAnnul(polymap);
   astAnnul(nasmythframe);

/* Rotate into Cassegrain coordinates, "el" is telescope elevation. */

   cassrot[0] = cos(el);
   cassrot[1] = -sin(el);
   cassrot[2] = sin(el);
   cassrot[3] = cos(el);
   cassmap = astMatrixMap ( 2, 2, 0, cassrot, "" );
   cassframe = astFrame ( 2, 
     "Domain=CASS,Label(1)=Az,Unit(1)=mm,Label(2)=El,Unit(2)=mm" );
   astAddFrame ( frameset, AST__CURRENT, cassmap, cassframe );
   astAnnul(cassmap);
   astAnnul(cassframe);

/* Convert units from mm to degrees, MM2DEG is the effective plate scale. */

   radmap = astZoomMap ( 2, MM2DEG, "" );
   cassdegframe = astFrame ( 2, "Domain=CASSDEG" );
   astAddFrame ( frameset, AST__CURRENT, radmap, cassdegframe );
   astAnnul(radmap);
   astAnnul(cassdegframe);

/* Create a celestial to tangent plane mapping via a FITS description. First 
   create the FitsChan, then store the required FITS WCS header cards in
   it, then rewind the FitsChan, then read a FrameSet from the FitsChan. */

   fitschan = astFitsChan ( NULL, NULL, "" );

   sc2ast_makefitschan ( 0.0, 0.0, 1.0, 1.0, az*RAD2DEG,
     el*RAD2DEG, "CLON-TAN", "CLAT-TAN", fitschan, status );

   astClear ( fitschan, "Card" );

   fitsframeset = astRead ( fitschan );
   astAnnul( fitschan );

/* Extract the mapping going from tangent plane to spherical (Az,El) from
   the FrameSet returned by the above call to astRead. */

   azelmap = astGetMapping ( fitsframeset, AST__BASE, AST__CURRENT );
   astAnnul(fitsframeset);

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
   astAnnul(skyframe);
   astAnnul(azelmap);

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
     27Jan2006 : use strncmp rather than strcmp (timj)
                 Associate error message with error condition (timj)
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

/*+ sc2ast_kludgemodjuldate - kludge for the modified julian date */

double sc2ast_kludgemodjuldate
( 
double ra,           /* Right Ascension in radians (given) */
int *status          /* global status (given and returned) */
)

/* Description :
    Until the simulator runs on a single clock, this kludge will calculate
    a modified julian date that is consistent with the astronomical source
    being on the meridian (i.e. lst=ra as it is assumed everywhere in the
    simulator). This routine is needed to calculate the time fields
    for TAI/RTS.

   Authors :
    Ed Chapin (echapin@phas.ubc.ca)

   History :
    05Dec2005 : Original version (echapin)
*/

{
  double date;          /* ref. modified julian date */
  double delta_st;      /* How much does reflst have to change? radians */
  double lat;           /* JCMT latitude in radians */
  double lon;           /* JCMT west longitude in radians */
  double obsdate;       /* The modified julian date of the observation */
  double refmjdate;     /* date + ut */
  double refgst;        /* ref. gst in radians */
  double reflst;        /* ref. lst in radians */
  double t;             /* Julian centuries since J2000 */
  double ut;            /* ref. universal time as day fraction */


  /*if ( !StatusOkP(status) ) return; */

  /* JCMT coordinates in radians */

  /*lat = ( 19.0 + (49.0/60.0) + (33.0/3600.0) ) / RAD2DEG;*/
  lon = ( 155.0 + (28.0/60.0) + (0.0/3600.0) ) / RAD2DEG;

  /* Define an arbitrary modified julian date + UT time */

  date = 51544;        /* Jan 1, 2000 */
  ut = 0.5;            /* noon */

  /* Use slalib slaGmsta algorithm to calculate greenwich LST at this date */

  t = ( ut + ( date - 51544.5 ) ) / 36525.0;

  refgst = DS2R * ( 24110.54841
                           + ( 8640184.812866
                           + ( 0.093104
                             - 6.2e-6 * t ) * t ) * t
                             + 86400.0 * ( dmod ( ut, 1.0 ) +
                                           dmod ( date, 1.0 ) ) ); 

  /* Calculate the reference LST from the GST using the telescope longitude */

  reflst = refgst - lon;

  /* If we want ra = lst, how far do we have to move in ST?
     i.e. what is the hour angle for the reference lst */
 
  delta_st = ra - reflst;

  /* Calculate the obsdate. Note that delta_st is multiplied by
     the ratio of a civil time interval to a sidereal time interval */

  obsdate = date + ut + delta_st/(2.*PI*1.002737909);

  return obsdate;
}

/*+ sc2ast_telpos - get telescope position and orientation */

void sc2ast_telpos
( 
double ra,           /* Right Ascension in radians (given) */
double dec,          /* Declination in radians (given) */
double lst,          /* local sidereal time in radians (given) */
double *az,          /* Azimuth in radians (returned) */
double *el,          /* Elevation in radians (returned) */
double *p            /* Parallactic angle in radians (returned) */
)

/* Description :
    Copy of dsim_telpos to avoid a libdsim dependency
    Use slalib algorithms to get from equatorial to horizontal coordinates.

   Authors :
    Ed Chapin (echapin@phas.ubc.ca)
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    12May2005 : Original version (bdk)
    05Dec2005 : Copied into sc2ast.c (EC)
*/

{
   double cosp;      /* intermediate result */
   double phi;       /* latitude of telescope in radians */
   double ha;        /* hour angle in radians */
   double r;         /* intermediate result */
   double sinp;      /* intermediate result */
   double x;         /* cartesian coordinates */
   double y;         /* cartesian coordinates */
   double z;         /* cartesian coordinates */

/* JCMT is 19:49:33 N */

   phi = ( 19.0 + (49.0/60.0) + (33.0/3600.0) ) / RAD2DEG;
   ha = lst - ra;

/*
   Equivalent to slaDe2h ( ha, dec, phi, az, el );
*/

/* Az,El as x,y,z */

   x = - cos(ha) * cos(dec) * sin(phi) + sin(dec) * cos(phi);
   y = - sin(ha) * cos(dec);
   z = cos(ha) * cos(dec) * cos(phi) + sin(dec) * sin(phi);

/* To spherical */

   r = sqrt ( x*x + y*y );

   if ( r < 1.0e-20 )
   {
      *az = 0.0;
   }
   else
   {
      *az = atan2 ( y, x );
   }

   if ( *az < 0.0 )
   {
      *az += 2.0 * PI;
   }

   *el = atan2 ( z, r );

/*
   *p = slaPa ( ha, dec, phi ); 
*/
   sinp = cos ( phi ) * sin ( ha );
   cosp = sin ( phi ) * cos ( dec) - cos ( phi ) * sin ( dec) * cos ( ha );

   if ( sinp != 0.0 || cosp != 0.0 )
   {
      *p = atan2 ( sinp, cosp );
   }
   else
   {
      *p = 0.0;
   }

}

/*+ sc2ast_createwcs_compat - create WCS descriptionusing old parameters */

void sc2ast_createwcs_compat
(
int subnum,             /* subarray number, 0-7 (given) */
double ra,              /* Right Ascension of the telescope boresight */
double dec,             /* Declination of the telescope boresight */
double el,              /* Boresight elevation in radians (given) */
double p,               /* No longer used (pass any dummy value) */
AstFrameSet **fset,     /* constructed frameset (returned) */
int *status             /* global status (given and returned) */
)
/* Description : 
     This is a temporary routine meant to replace calls to
     sc2ast_createwcs that were written before TimJ changed the
     API. It is simply a wrapper for sc2ast_createwcs which generates
     an appropriate Modified Julian date so that LST=RA for the
     calculation of TAI using the old parameters for sc2ast_createwcs
     Authors : 
      Ed Chapin (echapin@phas.ubc.ca) 
     History : 
      05Dec2005 : original (EC)
*/

{
  double tai;
  double temp_az, temp_el, temp_lst, temp_p;

  temp_lst = ra;
  sc2ast_telpos ( ra, dec, temp_lst, &temp_az, &temp_el, &temp_p );

  tai = sc2ast_kludgemodjuldate( ra, status );
  
  sc2ast_createwcs( subnum, temp_az, temp_el, tai, fset, status );
}


