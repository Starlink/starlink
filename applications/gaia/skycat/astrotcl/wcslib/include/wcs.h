/* libwcs/wcs.h
   September 21, 1998
   By Doug Mink, Harvard-Smithsonian Center for Astrophysics */

#ifndef _wcs_h_
#define _wcs_h_

#include "wcslib.h"
#include "fitshead.h"

struct WorldCoor {
  double	xref;		/* X reference coordinate value (deg) */
  double	yref;		/* Y reference coordinate value (deg) */
  double	xrefpix;	/* X reference pixel */
  double	yrefpix;	/* Y reference pixel */
  double	xinc;		/* X coordinate increment (deg) */
  double	yinc;		/* Y coordinate increment (deg) */
  double	rot;		/* rotation around axis (deg) (N through E) */
  double	cd[4];		/* rotation matrix */
  double	dc[4];		/* inverse rotation matrix */
  double	equinox;	/* Equinox of coordinates default to 1950.0 */
  double	epoch;		/* Epoch of coordinates default to equinox */
  double	nxpix;		/* Number of pixels in X-dimension of image */
  double	nypix;		/* Number of pixels in Y-dimension of image */
  double	plate_ra;	/* Right ascension of plate center */
  double	plate_dec;	/* Declination of plate center */
  double	plate_scale;	/* Plate scale in arcsec/mm */
  double	x_pixel_offset;	/* X pixel offset of image lower right */
  double	y_pixel_offset;	/* Y pixel offset of image lower right */
  double	x_pixel_size;	/* X pixel_size */
  double	y_pixel_size;	/* Y pixel_size */
  double	ppo_coeff[6];	/* pixel to plate coefficients for DSS */
  double	x_coeff[20];	/* X coefficients for plate model */
  double	y_coeff[20];	/* Y coefficients for plate model */
  double	xpix;		/* X (RA) coordinate (pixels) */
  double	ypix;		/* Y (dec) coordinate (pixels) */
  double	zpix;		/* Z (face) coordinate (pixels) */
  double	xpos;		/* X (RA) coordinate (deg) */
  double	ypos;		/* Y (dec) coordinate (deg) */
  double	crpix[4];	/* Values of CRPIXn keywords */
  double	crval[4];	/* Values of CRVALn keywords */
  double	cdelt[4];	/* Values of CDELTn keywords */
  double	pc[16];		/* Values of PCiiijjj keywords */
  double	projp[10];	/* Constants for various projections */
  double	longpole;	/* Longitude of North Pole in degrees */
  double	latpole;	/* Latitude of North Pole in degrees */
  double	rodeg;		/* Radius of the projection generating sphere */
  double	imrot;		/* Rotation angle of north pole */
  double	pa_north;	/* Position angle of north (0=horizontal) */
  double	pa_east;	/* Position angle of east (0=horizontal) */
  int		imflip;		/* If not 0, image is reflected around axis */
  int		prjcode;	/* projection code (-1-32) */
  int		latbase;	/* Latitude base 90 (NPA), 0 (LAT), -90 (SPA) */
  int		ncoeff1;	/* Number of x-axis plate fit coefficients */
  int		ncoeff2;	/* Number of y-axis plate fit coefficients */
  int		changesys;	/* 1 for FK4->FK5, 2 for FK5->FK4 */
  				/* 3 for FK4->galactic, 4 for FK5->galactic */
  int		printsys;	/* 1 to print coordinate system, else 0 */
  int		ndec;		/* Number of decimal places in PIX2WCST */
  int		degout;		/* 1 to always print degrees in PIX2WCST */
  int		tabsys;		/* 1 to put tab between RA & Dec, else 0 */
  int		rotmat;		/* 0 if CDELT, CROTA; 1 if CD */
  int		coorflip;	/* 0 if x=RA, y=Dec; 1 if x=Dec, y=RA */
  int		offscl;		/* 0 if OK, 1 if offscale */
  int		wcson;		/* 1 if WCS is set, else 0 */
  int		naxes;		/* Number of axes in image */
  int		oldwcs;		/* 1 to use worldpos() and worldpix() instead
				   of Mark Calabretta's WCSLIB subroutines */
  int		linmode;	/* 0=system only, 1=units, 2=system+units */
  int		detector;	/* Instrument detector number */
  char		instrument[32];	/* Instrument name */
  char		ctype[4][9];	/* Values of CTYPEn keywords */
  char		c1type[8];	/*  1st coordinate type code:
					RA--, GLON, ELON */
  char		c2type[8];	/*  2nd coordinate type code:
					DEC-, GLAT, ELAT */
  char		ptype[8];	/*  projection type code:
				    SIN, TAN, ARC, NCP, GLS, MER, AIT, etc */
  char		units[4][32];	/* Units if LINEAR */
  char		radecsys[32];	/* Reference frame: FK4, FK4-NO-E, FK5, GAPPT*/
  char		radecout[32];	/* Output reference frame: FK4,FK5,GAL,ECL */
  char		radecin[32];	/* Input reference frame: FK4,FK5,GAL,ECL */
  double	eqin;		/* Input equinox (match sysin if 0.0) */
  double	eqout;		/* Output equinox (match sysout if 0.0) */
  int		sysin;		/* Input coordinate system code */
  int		syswcs;		/* WCS coordinate system code */
  int		sysout;		/* Output coordinate system code */
				/* WCS_B1950, WCS_J2000, WCS_GALACTIC,
				 * WCS_ECLIPTIC, WCS_LINEAR, WCS_ALTAZ  */
  char		center[32];	/* Center coordinates (with frame) */
  struct wcsprm wcsl;		/* WCSLIB main projection parameters */
  struct linprm lin;		/* WCSLIB image/pixel conversion parameters */
  struct celprm cel;		/* WCSLIB projection type */
  struct prjprm prj;		/* WCSLIB projection parameters */
  struct IRAFsurface *lngcor;	/* RA/longitude correction structure */
  struct IRAFsurface *latcor;	/* Dec/latitude correction structure */
  char *command_format[10];	/* WCS command formats */
				/* where %s is replaced by WCS coordinates */
				/* where %f is replaced by the image filename */
				/* where %x is replaced by image coordinates */
};

/* Projections (1-26 are WCSLIB) */
#define WCS_PIX -1	/* Pixel WCS */
#define WCS_LIN  0	/* Linear projection */
#define WCS_AZP  1	/* Zenithal/Azimuthal Perspective */
#define WCS_TAN  2	/* Gnomonic = Tangent Plane */
#define WCS_SIN  3	/* Orthographic/synthesis */
#define WCS_STG  4	/* Stereographic */
#define WCS_ARC  5	/* Zenithal/azimuthal equidistant */
#define WCS_ZPN  6	/* Zenithal/azimuthal PolyNomial */
#define WCS_ZEA  7	/* Zenithal/azimuthal Equal Area */
#define WCS_AIR  8	/* Airy */
#define WCS_CYP  9	/* CYlindrical Perspective */
#define WCS_CAR 10	/* Cartesian */
#define WCS_MER 11	/* Mercator */
#define WCS_CEA 12	/* Cylindrical Equal Area */
#define WCS_CPS 13	/* Conic PerSpective (COP) */
#define WCS_COD 14	/* COnic equiDistant */
#define WCS_COE 15	/* COnic Equal area */
#define WCS_COO 16	/* COnic Orthomorphic */
#define WCS_BON 17	/* Bonne */
#define WCS_PCO 18	/* Polyconic */
#define WCS_GLS 19	/* Sanson-Flamsteed (GLobal Sinusoidal) */
#define WCS_PAR 20	/* Parabolic */
#define WCS_AIT 21	/* Hammer-Aitoff */
#define WCS_MOL 22	/* Mollweide */
#define WCS_CSC 23	/* COBE quadrilateralized Spherical Cube */
#define WCS_QSC 24	/* Quadrilateralized Spherical Cube */
#define WCS_TSC 25	/* Tangential Spherical Cube */
#define WCS_NCP 26	/* Special case of SIN */
#define WCS_DSS 27	/* Digitized Sky Survey plate solution */
#define WCS_PLT 28	/* Plate fit polynomials (SAO) */
#define WCS_TNX 29	/* Gnomonic = Tangent Plane (NOAO with corrections) */

/* Coordinate systems */
#define WCS_J2000	1	/* J2000(FK5) right ascension and declination */
#define WCS_B1950	2	/* B1950(FK4) right ascension and declination */
#define WCS_GALACTIC	3	/* Galactic longitude and latitude */
#define WCS_ECLIPTIC	4	/* Ecliptic longitude and latitude */
#define WCS_ALTAZ	5	/* Azimuth and altitude/elevation */
#define WCS_LINEAR	6	/* Linear with optional units */
#define WCS_NPOLE	7	/* Longitude and north polar angle */
#define WCS_SPA		8	/* Longitude and south polar angle */

#ifndef PI
#define PI	3.141592653589793238462643
#endif

/* Conversions among hours of RA, degrees and radians. */
#define degrad(x)	((x)*PI/180.)
#define raddeg(x)	((x)*180./PI)
#define hrdeg(x)	((x)*15.)
#define deghr(x)	((x)/15.)
#define hrrad(x)	degrad(hrdeg(x))
#define radhr(x)	deghr(raddeg(x))

/* TNX surface fitting structure and flags */
struct IRAFsurface {
  double xrange;	/* 2. / (xmax - xmin), polynomials */
  double xmaxmin;	/* - (xmax + xmin) / 2., polynomials */
  double yrange;	/* 2. / (ymax - ymin), polynomials */
  double ymaxmin;	/* - (ymax + ymin) / 2., polynomials */
  int	 type;		/* type of curve to be fitted */
  int    xorder;	/* order of the fit in x */
  int    yorder;	/* order of the fit in y */
  int    xterms;	/* cross terms for polynomials */
  int    ncoeff;	/* total number of coefficients */
  double *coeff;	/* pointer to coefficient vector */
  double *xbasis;	/* pointer to basis functions (all x) */
  double *ybasis;	/* pointer to basis functions (all y) */
};

/* TNX permitted types of surfaces */
#define  TNX_CHEBYSHEV    1
#define  TNX_LEGENDRE     2
#define  TNX_POLYNOMIAL   3

/* TNX cross-terms flags */
#define	TNX_XNONE	0	/* no x-terms (old no) */
#define	TNX_XFULL	1	/* full x-terms (new yes) */
#define	TNX_XHALF	2	/* half x-terms (new) */

#ifdef __cplusplus /* allan: 28.4.98: added C++ prototypes */
extern "C" {

    /* WCS subroutines in wcs.c */
    struct WorldCoor *wcsinit (const char* hstring);
    struct WorldCoor *wcsninit (const char* hstring, int len);

    int iswcs(			/* Returns 1 if wcs structure set, else 0 */
	WorldCoor *wcs);	/* World coordinate system structure */
    int nowcs(			/* Returns 0 if wcs structure set, else 1 */
	WorldCoor *wcs);	/* World coordinate system structure */

    int pix2wcst (
        struct WorldCoor *wcs,  /* World coordinate system structure */
        double xpix, 
        double ypix,            /* Image coordinates in pixels */
        char   *wcstring,       /* World coordinate string (returned) */
        int    lstr             /* Length of world coordinate string (returned) */
        );

    int pix2wcs (
        struct WorldCoor *wcs,  /* World coordinate system structure */
        double xpix, 
        double ypix,            /* Image coordinates in pixels */
        double *xpos, 
        double *ypos            /* RA and Dec in degrees (returned) */
        );

    void wcsc2pix (
        struct WorldCoor *wcs,  /* World coordinate system structure */
        double xpos,
        double ypos,            /* World coordinates in degrees */
	char *coorsys,		/* Coordinate system (B1950, J2000, etc) */
        double *xpix,
        double *ypix,           /* Image coordinates in pixels */
        int     *offscl);

    void wcs2pix (
        struct WorldCoor *wcs,  /* World coordinate system structure */
        double xpos,
        double ypos,            /* World coordinates in degrees */
        double *xpix,
        double *ypix,           /* Image coordinates in pixels */
        int     *offscl);

    double wcsdist(		/* Compute angular distance between 2 sky positions */
	double ra0,		/* World coordinates in degrees */
	double dec0,
	double ra1,		/* World coordinates in degrees */
	double dec1);

    struct WorldCoor* wcsxinit(
        double  cra,    /* Center right ascension in degrees */
        double  cdec,   /* Center declination in degrees */
        double  secpix, /* Number of arcseconds per pixel */
        double  xrpix,  /* Reference pixel X coordinate */
        double  yrpix,  /* Reference pixel X coordinate */
        int     nxpix,  /* Number of pixels along x-axis */
        int     nypix,  /* Number of pixels along y-axis */
        double  rotate, /* Rotation angle (clockwise positive) in degrees */
        int     equinox, /* Equinox of coordinates, 1950 and 2000 supported */
        double  epoch,  /* Epoch of coordinates, used for FK4/FK5 conversion
                         * no effect if 0 */
        char    *proj); /* Projection */

    struct WorldCoor* wcskinit( /* set up WCS structure from keyword values */
	int     naxis1,		/* Number of pixels along x-axis */
	int     naxis2,		/* Number of pixels along y-axis */
	char    *ctype1,	/* FITS WCS projection for axis 1 */
	char    *ctype2,	/* FITS WCS projection for axis 2 */
	double  crpix1,		/* Reference pixel coordinates */
	double  crpix2,		/* Reference pixel coordinates */
	double  crval1,		/* Coordinate at reference pixel in degrees */
	double  crval2,		/* Coordinate at reference pixel in degrees */
	double  *cd,            /* Rotation matrix, used if not NULL */
	double  cdelt1,		/* scale in degrees/pixel, if cd is NULL */
	double  cdelt2,		/* scale in degrees/pixel, if cd is NULL */
	double  crota,          /* Rotation angle in degrees, if cd is NULL */
	int     equinox, /* Equinox of coordinates, 1950 and 2000 supported */
	double  epoch);  /* Epoch of coordinates, for FK4/FK5 conversion */

    void wcsshift(		/* Change center of WCS */
        struct WorldCoor *wcs,  /* World coordinate system structure */
        double  cra,            /* New center right ascension in degrees */
        double  cdec,           /* New center declination in degrees */
        char    *coorsys);      /* FK4 or FK5 coordinates (1950 or 2000) */

    void wcsfull(
        struct WorldCoor *wcs,  /* World coordinate system structure */
        double  *cra,           /* Right ascension of image center (deg) (returned) */
        double  *cdec,          /* Declination of image center (deg) (returned) */
        double  *width,         /* Width in degrees (returned) */
        double  *height);       /* Height in degrees (returned) */

    void setwcserr(		/* Set WCS error message for later printing */
	char *errmsg);		/* Error mesage < 80 char */
    void wcserr();		/* Print WCS error message to stderr */

    void setdefwcs(		/* Set flag to use AIPS WCS instead of WCSLIB */
	int oldwcs);		/* 1 for AIPS WCS subroutines, else WCSLIB */
    int getdefwcs();		/* Return flag for AIPS WCS set by setdefwcs */

    char *getradecsys(		/* Return name of image coordinate system */
        struct WorldCoor *wcs);	/* World coordinate system structure */
	
    void wcsoutinit(		/* Set output coordinate system for pix2wcs */
        struct WorldCoor *wcs,	/* World coordinate system structure */
	char *coorsys);		/* Coordinate system (B1950, J2000, etc) */

    char *getwcsout(		/* Return current output coordinate system */
        struct WorldCoor *wcs);	/* World coordinate system structure */

    void wcsininit(		/* Set input coordinate system for wcs2pix */
        struct WorldCoor *wcs,	/* World coordinate system structure */
	char *coorsys);		/* Coordinate system (B1950, J2000, etc) */

    char *getwcsin(		/* Return current input coordinate system */
        struct WorldCoor *wcs);	/* World coordinate system structure */

    int setwcsdeg(		/* Set WCS coordinate output format */
        struct WorldCoor *wcs,	/* World coordinate system structure */
	int degout);		/* 1= degrees, 0= hh:mm:ss dd:mm:ss */

    int wcsndec(		/* Set or get number of output decimal places */
        struct WorldCoor *wcs,	/* World coordinate system structure */
	int ndec);		/* Number of decimal places in output string
				   if < 0, return current ndec unchanged */

    void setwcslin(		/* Set pix2wcst() mode for LINEAR coordinates */
        struct WorldCoor *wcs,	/* World coordinate system structure */
	int mode);		/* 0: x y linear, 1: x units x units
				   2: x y linear units */

    int wcszin(
	int izpix);		/* Set coordinate in third dimension (face) */

    int wcszout (		/* Return coordinate in third dimension */
        struct WorldCoor *wcs);	/* World coordinate system structure */

    void savewcscoor(		/* Save output coordinate system */
	char *wcscoor);		/* coordinate system (J2000, B1950, galactic) */
    char *getwcscoor();		/* Return output coordinate system */
    void savewcscom(		/* Save WCS shell command */
	char *wcscom);		/* Shell command using output WCS string */
    char *getwcscom();		/* Return WCS shell command */


    /* Coordinate conversion subroutines in wcscon.c */
    void wcsconp(	/* Convert between coordinate systems and equinoxes */
	int sys1,	/* Input coordinate system (J2000, B1950, ECLIPTIC, GALACTIC */
	int sys2,	/* Output coordinate system (J2000, B1950, ECLIPTIC, G ALACTIC */
	double eq1,	/* Input equinox (default of sys1 if 0.0) */
	double eq2,	/* Output equinox (default of sys2 if 0.0) */
	double ep1,	/* Input Besselian epoch in years */
	double ep2,	/* Output Besselian epoch in years */
	double *dtheta,	/* Longitude or right ascension in degrees
			   Input in sys1, returned in sys2 */
	double *dphi,	/* Latitude or declination in degrees
			   Input in sys1, returned in sys2 */
	double *ptheta,	/* Longitude or right ascension proper motion in degrees/year
			   Input in sys1, returned in sys2 */
	double *pphi);	/* Latitude or declination proper motion in degrees/year
			   Input in sys1, returned in sys2 */
    void wcscon(	/* Convert between coordinate systems and equinoxes */
	int sys1,	/* Input coordinate system (J2000, B1950, ECLIPTIC, GALACTIC */
	int sys2,	/* Output coordinate system (J2000, B1950, ECLIPTIC, G ALACTIC */
	double eq1,	/* Input equinox (default of sys1 if 0.0) */
	double eq2,	/* Output equinox (default of sys2 if 0.0) */
	double *dtheta,	/* Longitude or right ascension in degrees
			   Input in sys1, returned in sys2 */
	double *dphi,	/* Latitude or declination in degrees
			   Input in sys1, returned in sys2 */
	double epoch);	/* Besselian epoch in years */

    int wcscsys(	/* Return code for coordinate system in string */
	char *coorsys);	 /* Coordinate system (B1950, J2000, etc) */

    double wcsceq (	/* Set equinox from string (return 0.0 if not obvious) */
	char *wcstring);  /* Coordinate system (B1950, J2000, etc) */

    void wcscstr (	/* Set coordinate system type string from system and equinox */
	char   *cstr,	 /* Coordinate system string (returned) */
	int    syswcs,	/* Coordinate system code */
	double equinox,	/* Equinox of coordinate system */
	double epoch);	/* Epoch of coordinate system */


};
#else /* __cplusplus */

/* WCS subroutines in wcs.c */
struct WorldCoor *wcsinit(); /* set up a WCS structure from a FITS image header */
struct WorldCoor *wcsninit(); /* set up a WCS structure from a FITS image header */
struct WorldCoor *wcsxinit(); /* set up a WCS structure from arguments */
struct WorldCoor *wcskinit(); /* set up a WCS structure from keyword values */
int wcstype();		/* Set projection type from header CTYPEs */
void wcscdset();	/* Set scaling and rotation from CD matrix */
void wcsdeltset();	/* set scaling and rotation from CDELTs and CROTA2 */
void wcspcset();	/* set scaling and rotation from CDELTs and PC matrix */
int iswcs();		/* Return 1 if WCS structure is filled, else 0 */
int nowcs();		/* Return 0 if WCS structure is filled, else 1 */
void wcsshift();	/* Reset the center of a WCS structure */
void wcscent();		/* Print the image center and size in WCS units */
void wcssize();		/* Return RA and Dec of image center, size in RA and Dec */
void wcsfull();		/* Return RA and Dec of image center, size in degrees */
double wcsdist();	/* Distance in degrees between two sky coordinates */
void wcscominit();	/* Initialize catalog search command set by -wcscom */
void wcscom();		/* Execute catalog search command set by -wcscom */
char *getradecsys();	/* Return current value of coordinate system */
void wcsoutinit();	/* Initialize WCS output coordinate system for use by pix2wcs */
char *getwcsout();	/* Return current value of WCS output coordinate system */
void wcsininit();	/* Initialize WCS input coordinate system for use by wcs2pix */
char *getwcsin();	/* Return current value of WCS input coordinate system */
int setwcsdeg();	/* Set WCS output in degrees (1) or hh:mm:ss dd:mm:ss (0) */
int wcsndec();		/* Set or get number of output decimal places */
int wcsreset();		/* Change WCS using arguments */
void wcseqset();	/* Change equinox of reference pixel coordinates in WCS */
void wcscstr();		/* Set coordinate system string from system and equinox */
void setwcslin();	/* Set output string mode for LINEAR coordinates */
int pix2wcst();		/* Convert pixel coordinates to World Coordinate string */
void pix2wcs();		/* Convert pixel coordinates to World Coordinates */
void wcsc2pix();	/* Convert World Coordinates to pixel coordinates */
void wcs2pix();		/* Convert World Coordinates to pixel coordinates */
void setdefwcs();	/* Call to use AIPS classic WCS (also not PLT or TNX */
int getdefwcs();	/* Call to get flag for AIPS classic WCS */
int wcszin();		/* Set coordinate in third dimension (face) */
int wcszout();		/* Return coordinate in third dimension */
void wcserr();		/* Print WCS error message to stderr */
void setwcserr();	/* Set WCS error message for later printing */
void savewcscoor();	/* Save output coordinate system */
char *getwcscoor();	/* Return output coordinate system */
void savewcscom();	/* Save WCS shell command */
char *getwcscom();	/* Return WCS shell command */
void setwcscom();	/* Set WCS shell commands from stored values */
void freewcscom();	/* Free memory used to store WCS shell commands */

/* Coordinate conversion subroutines in wcscon.c */
void wcscon();		/* Convert between coordinate systems and equinoxes */
void wcsconp();		/* Convert between coordinate systems and equinoxes */
int wcscsys();		/* Set coordinate system from string */
double wcsceq();		/* Set equinox from string (return 0.0 if not obvious) */
void wcscstr();		/* Return system string from system code, equinox, epoch */
#endif
#endif

/* Oct 26 1994	New file
 * Dec 21 1994	Add rotation matrix
 * Dec 22 1994	Add flag for coordinate reversal

 * Mar  6 1995	Add parameters for Digital Sky Survey plate fit
 * Jun  8 1995	Add parameters for coordinate system change
 * Jun 21 1995	Add parameter for plate scale
 * Jul  6 1995	Add parameter to note whether WCS is set
 * Aug  8 1995	Add parameter to note whether to print coordinate system
 * Oct 16 1995	Add parameters to save image dimensions and center coordinates

 * Feb 15 1996	Add coordinate conversion functions
 * Feb 20 1996	Add flag for tab tables
 * Apr 26 1996	Add epoch of positions (actual date of image)
 * Jul  5 1996	Add subroutine declarations
 * Jul 19 1996	Add WCSFULL declaration
 * Aug  5 1996	Add WCSNINIT to initialize WCS for non-terminated header
 * Oct 31 1996	Add DCnn inverse rotation matrix
 * Nov  1 1996	Add NDEC number of decimal places in output
 *
 * May 22 1997	Change range of pcode from 1-8 to -1-8 for linear transform
 * Sep 12 1997	Add chip rotation MROT, XMPIX, YMPIX
 *
 * Jan  7 1998	Add INSTRUME and DETECTOR for HST metric correction
 * Jan 16 1998	Add Mark Calabretta's WCSLIB data structures
 * Jan 16 1998	Add LONGPOLE, LATPOLE, and PROJP constants for Calabretta
 * Jan 22 1998	Add ctype[], crpix[], crval[], and cdelt[] for Calabretta
 * Jan 23 1998	Change wcsset() to wcsxinit() and pcode to prjcode
 * Jan 23 1998	Define projection type flags
 * Jan 26 1998	Remove chip rotation
 * Jan 26 1998	Add chip correction polynomial
 * Feb  3 1998	Add number of coefficients for residual fit
 * Feb  5 1998	Make cd and dc matrices vectors, not individual elements
 * Feb 19 1998	Add projection names
 * Feb 23 1998	Add TNX projection from NOAO
 * Mar  3 1998	Add NOAO plate fit and residual fit
 * Mar 12 1998	Add variables for TNX correction surface
 * Mar 23 1998	Add PLT plate fit polynomial projection; reassign DSS
 * Mar 23 1998	Drop plate_fit flag from structure
 * Mar 25 1998	Add npcoeff to wcs structure for new plate fit WCS
 * Apr  7 1998	Change amd_i_coeff to i_coeff
 * Apr  8 1998	Add wcseqset() and wcsreset() subroutine declarations
 * Apr 10 1998	Rearrange order of nonstandard WCS types
 * Apr 13 1998	Add setdefwcs() subroutine declaration
 * Apr 14 1998	Add coordinate systems and wcscoor()
 * Apr 24 1998	Add units
 * Apr 28 1998	Change coordinate system flags to WCS_*
 * Apr 28 1998	Change projection flags to WCS_*
 * Apr 28 1998	Add wcsc2pix()
 * May  7 1998	Add C++ declarations
 * May 13 1998	Add eqin and eqout for conversions to and from equinoxes
 * May 14 1998	Add declarations for coordinate conversion subroutines
 * May 27 1998	Add blsearch()
 * May 27 1998	Change linear projection back to WCS_LIN from WCS_LPR
 * May 27 1998	Move hget.c and hput.c C++ declarations to fitshead.h
 * May 27 1998	Include fitshead.h
 * May 29 1998	Add wcskinit()
 * Jun  1 1998	Add wcserr()
 * Jun 11 1998	Add initialization support subroutines
 * Jun 18 1998	Add wcspcset()
 * Jun 25 1998	Add wcsndec()
 * Jul  6 1998	Add wcszin() and wcszout() to use third dimension of images
 * Jul  7 1998	Change setdegout() to setwcsdeg(); setlinmode() to setwcslin()
 * Jul 17 1998	Add savewcscoor(), getwcscoor(), savewcscom(), and getwcscom()
 * Aug 14 1998	Add freewcscom(), setwcscom(), and multiple WCS commands
 * Sep  3 1998	Add pa_north, pa_east, imrot and imflip to wcs structure
 * Sep 14 1998	Add latbase for AXAF North Polar angle (NPOL not LAT-)
 * Sep 16 1998	Make WCS_system start at 1; add NPOLE
 * Sep 17 1998	Add wcscstr()
 * Sep 21 1998	Add wcsconp() to convert proper motions, too.
 */
