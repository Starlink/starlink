/*
*+
*  Name:
*     sc2sim.h

*  Purpose:
*     Prototypes for the libsc2sim library

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Header File

*  Invocation:
*     #include "sc2sim.h"

*  Description:
*     Prototypes used by the libsc2sim functions.

*  Authors:
*     J.Balfour (UBC)
*     A.G. Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-07-21 (JB):
*        Original
*     2006-08-19 (AGG):
*        Updated APIs to sc2sim_ndfwrheat, sc2sim_ndfwrdata
*     2006-09-01 (EC):
*        Removed sc2sim_hor2eq / sc2sim_telpos / extraneous slalib #defines
*     2006-09-06 (EC)
*        Modified interface to ndfwrdata to take INSTRUME as parameter
*     2006-09-08 (EC):
*        Modified sc2sim_calctime to take Longitude as a parameter
*     2006-10-03 (JB):
*        Modified sc2sim_getpong to use width/height and removed 
*        sc2sim_getpongends
*     2006-10-17 (JB):
*        Removed sc2sim_getpong, replaced with sc2sim_getpongvert,
*        sc2sim_getpongends, sc2sim_getstraightpong, and
*        sc2sim_getcurvepong
*     2006-11-16 (JB):
*        Pass accel to sc2sim_getcurvepong
*     2006-11-21 (JB):
*        Added sc2sim_getliss
*     2006-11-22 (JB):
*        Added nmaps to getcurvepong, getstraightpong, and getliss.

*     {enter_further_changes_here}

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

#include "ast.h"
#include "sc2sim_par.h"
#include "sc2da/Dits_Err.h"
#include "sc2da/Ers.h"
#include "jcmt/state.h"

#include "libsc2sim/sc2sim_struct.h"
#include "libsc2sim/sc2sim_par.h"

#ifndef SC2SIM_DEFINED
#define SC2SIM_DEFINED

#define C 299792458.0                  /* speed of light in metres/sec */
#define H 6.626e-34                    /* Planck's constant in joule.sec */
#define COUNTTOSEC 6.28                /* arcsec per pixel */
#define RIZERO 40.0                    /* distortion pattern centre */
#define RJZERO -10.0                   /* distortion pattern centre */
#define PIBY2 (AST__DPI/2.0)           /* Math constant */
#define DIAMETER 15.0                  /* Diameter JCMT in metres */
#define MM2SEC 5.144                   /* plate scale at Nasmyth */

#define BOLCOL 32                      /* number of columns in a subarray */
#define BOLROW 40                      /* number of rows in a subarray */

void sc2sim_addpnoise 
(
double lambda,       /* wavelength in metres (given) */
double bandGHz,      /* bandwidth in GHZ (given) */
double aomega,       /* geometrical optical factor (given) */
double integ_time,   /* Effective integration time in sec (given) */
double *flux,        /* Flux value in pW (given and returned) */
int *status          /* global status (given and returned) */
);

void sc2sim_atmsky 
(
double lambda,       /* wavelength in metres (given) */
double trans,        /* % atmospheric transmission (given) */
double *flux,        /* flux per bolometer in pW (returned) */
int *status          /* global status (given and returned) */ 
); 

void sc2sim_atmtrans
(
double lambda,       /* wavelength in metres (given) */
double flux,         /* flux per bolometer in pW (given) */
double *trans,       /* % atmospheric transmission (returned) */
int *status          /* global status (given and returned) */
);

void sc2sim_bolcoords 
(
char *subname,       /* subarray name, s8a-s4d (given) */
double ra,           /* RA of observation in radians (given) */
double dec,          /* Dec of observation in radians (given) */
double elevation,    /* telescope elevation in radians (given) */
double p,            /* parallactic angle in radians (given) */
char *domain,        /* AST domain name to be used (given) */
int *bol,            /* bolometer counter (returned) */
double xbc[],        /* projected X coords of bolometers (returned) */
double ybc[],        /* projected Y coords of bolometers (returned) */
int *status          /* global status (given and returned) */
);

void sc2sim_calctime
( 
double lon,          /* Geodetic W Lon (radians) */
double mjdaystart,   /* start time as modified juldate */
double samptime,     /* length of a sample in seconds */
int nsamp,           /* number of samples */
double *ut,          /* returned UT at each sample (mod. juldate) */
double *lst,         /* returned LST at each sample (radians) */
int *status          /* global status (given and returned) */
);

void sc2sim_calctrans
(
double lambda,        /* wavelength in metres (given) */
double *trans,        /* % atmospheric transmission (returned) */
double tauCSO,        /* CSO optical depth (given) */
int *status           /* global status (given and returned) */
);

void sc2sim_digitise 
(
int nvals,            /* number of values (given) */
double current[],     /* signal values in amps (given) */
double digmean,       /* mean digitised level (given) */
double digscale,      /* digitisation scale factor (given) */
double digcurrent,    /* current in amps at digmean (given) */
int digits[],         /* digitised currents (returned) */
int *status           /* global status (given and returned) */
);

double sc2sim_drand
(
double sigma          /* sigma of distribution (given) */
);

void sc2sim_fft2d 
(
int direction,       /* transform specification +1 or -1 (given) */
int size,            /* square dimension of complex image array (given) */
double *array,       /* image array size*2 by size (given and returned) */
int *status          /* global status (given and returned) */
);

void sc2sim_fitheat 
(
int nboll,             /* number of bolometers (given) */
int nframes,           /* number of frames in scan (given) */
double *heat,          /* heater values (given) */
double *inptr,         /* measurement values (given) */
double *coptr,         /* coefficients of fit (returned) */
int *status            /* global status (given and returned) */
);

void sc2sim_four1 
( 
int isign,         /* direction of transform (given) */
int nn,            /* number of complex values (given) */
double data[]      /* complex signal transformed in-place - even indices 
                      real values, odd imaginary (given and returned) */
);

void sc2sim_getast_wcs 
( 
int nboll,                   /* total number of bolometers (given) */
double *xbolo,               /* x-bolometer coordinates for array (given) */
double *ybolo,               /* y-bolometer coordinates for array (given) */
AstCmpMap *bolo2map,         /* mapping bolo->sky image coordinates (given ) */
double *astsim,              /* astronomical image (given) */
int astnaxes[2],             /* dimensions of simulated image (given) */
double *dbuf,                /* pointer to bolo output (returned) */
int *status                  /* global status (given and returned) */
);

void sc2sim_getbilinear 
( 
double xpos,         /* X-coordinate of sample point in arcsec (given) */
double ypos,         /* Y-coordinate of sample point in arcsec (given) */
double scale,        /* scale of image in arcsec per pixel (given) */
int size,            /* size of image (given) */
double *image,       /* astronomical image (given) */
double *value,       /* value sampled from image (returned) */
int *status          /* global status (given and returned) */
);

void sc2sim_getbous
(
double angle,        /* angle of pattern relative to telescope
                        axes in radians anticlockwise (given) */
double bouswidth,    /* width of bous pattern (arcsec) (given) */
double bousheight,   /* height of bous pattern (arcsec) (given) */
double spacing,      /* distance between scans (arcsec) (given) */
double accel[2],     /* telescope accelerations (arcsec) (given) */
double vmax[2],      /* telescope maximum velocities (arcsec) (given) */
double samptime,     /* sample interval in sec (given) */
int *bouscount,      /* number of positions in pattern (returned) */
double **posptr,     /* list of positions (returned) */
int *status          /* global status (given and returned) */
);

mapCoordframe sc2sim_getcoordframe
( 
char *name,         /* string containing name of coordinate frame */
int *status         /* global status (given and returned) */
);

void sc2sim_getcurvepong
(
double angle,        /* angle of pattern relative to telescope
                        axes in radians anticlockwise (given) */
double width,        /* minimum width of PONG pattern in arcsec (given) */
double height,       /* minimum height of PONG pattern in arcsec (given) */
double spacing,      /* grid spacing in arcsec (given) */
double accel[2],     /* telescope accelerations (arcsec) (given) */
double vmax[2],      /* telescope maximum velocities (arcsec/sec) (given) */
double samptime,     /* sample interval in sec (given) */
int nmaps,           /* number of cycles of the pattern */
int *pongcount,      /* number of positions in pattern (returned) */
double **posptr,     /* list of positions (returned) */
int *status          /* global status (given and returned) */
);

void sc2sim_getinvf 
( 
double sigma,        /* dispersion of broad-band noise (given) */ 
double corner,       /* corner frequency, where 1/f dispersion=sigma (given)*/
double samptime,     /* time per data sample (given) */
double nterms,       /* number of frequencies calculated (given) */
double *noisecoeffs, /* 1/f spectrum (returned) */
int *status          /* global status (given and returned) */
);

void sc2sim_getliss
(
double angle,        /* angle of pattern relative to telescope
                        axes in radians anticlockwise (given) */
double width,        /* minimum width of Liss pattern in arcsec (given) */
double height,       /* minimum height of Liss pattern in arcsec (given) */
double spacing,      /* grid spacing in arcsec (given) */
double accel[2],     /* telescope accelerations (arcsec) (given) */
double vmax[2],      /* telescope maximum velocities (arcsec/sec) (given) */
double samptime,     /* sample interval in sec (given) */
int nmaps,           /* number of cycles of the pattern */
int *lisscount,      /* number of positions in pattern (returned) */
double **posptr,     /* list of positions (returned) */
int *status          /* global status (given and returned) */
);

obsMode sc2sim_getobsmode
( 
char *name,         /* string containing name of observing mode */
int *status         /* global status (given and returned) */
);

void sc2sim_getobspar 
( 
AstKeyMap *keymap,   /* Keymap containing obs parameters (given) */
struct  sc2sim_obs_struct *inx, /* Structure for values from obs
				   keymap file (given and returned */
int *status          /* global status (given and returned) */
);

void sc2sim_getpat
(
int nvert,            /* Number of vertices per pattern (given) */
int smu_samples,      /* number of samples between vertices (given) */
double sample_t,      /* time between data samples in msec (given) */
double smu_offset,    /* smu timing offset in msec (given) */
int conv_shape,       /* choice of convolution function (given) */
double conv_sig,      /* convolution parameter (given) */
int move_code,        /* SMU waveform choice (given) */
double jig_stepx,     /* X interval in arcsec (given) */
double jig_stepy,     /* Y interval in arcsec (given) */
int jig_vert[][2],    /* Array with relative jiggle coordinates in units of
                         jiggle steps in case jiggle positions are 
                         visited (given) */

int *cycle_samples,   /* The number of samples per cycle (returned) */

double pattern[][2],  /* The array to hold the coordinates of the jiggle 
                         offsets in arcsec. There are cycle_samples entries 
                         filled. (returned) */

int *status           /* global status (given and returned) */
);

void sc2sim_getpongends
( 
double width,          /* minimum width of scan (arcsec) */
double height,         /* minimum height of scan (arcsec) */
double spacing,        /* spacing of grid pattern (arcsec) */
double grid[][2],     /* array of vertex coordinates */
int *numvertices,      /* total number of vertices */
int *status            /* pointer to global status */
);

void sc2sim_getpongvert
( 
double width,          /* minimum width of scan (arcsec) */
double height,         /* minimum height of scan (arcsec) */
double spacing,        /* spacing of grid pattern (arcsec) */
double *vert_spacing,  /* spacing of vertices along axes (arcsec) */
int *x_numvert,        /* number of vertices in x direction */
int *y_numvert,         /* number of vertices in y direction */
int *status            /* pointer to global status */
);

void sc2sim_getscaling
( 
int ncoeffs,          /* number of coefficients describing response curve
                         (given) */
double coeffs[],      /* array to hold response curve coefficients (given) */
double targetpow,     /* target power level in pW (given) */
double photonsigma,   /* photon noise level (given) */
double *digmean,      /* mean digitised level (returned) */
double *digscale,     /* digitisation scale factor (returned) */
double *digcurrent,   /* current in amps at digmean (returned) */
int *status           /* global status (given and returned) */
);

void sc2sim_getscanseg
( 
double samptime,     /* sample time in sec (given) */
double cstart[2],    /* starting coordinates in arcsec (given) */
double cend[2],      /* ending coordinates in arcsec (given) */
double accel[2],     /* telescope accelerations (arcsec) (given) */
double vmax[2],      /* telescope maximum velocities (arcsec) (given) */
int maxoff,          /* maximum total of offsets in pattern (given) */
int *curroff,        /* current offset in pattern (given and returned) */
double *pattern,     /* pointing coordinates in arcsec (given and returned) */
int *status          /* global status (given and returned) */
);

void sc2sim_getscansegsize
( 
double samptime,     /* sample time in sec (given) */
double cstart[2],    /* starting coordinates in arcsec (given) */
double cend[2],      /* ending coordinates in arcsec (given) */
double accel[2],     /* telescope accelerations (arcsec) (given) */
double vmax[2],      /* telescope maximum velocities (arcsec) (given) */
int *size,           /* number of samples in pattern (returned) */
int *status          /* global status (given and returned) */
);

void sc2sim_getsigma
( 
double lambda,         /* wavelength in metres (given) */
double bandGHz,        /* bandwidth in GHz (given) */
double aomega,         /* geometrical factor (given) */
double flux,           /* sky power per pixel in pW (given) */
double *sigma,         /* photon noise in pW (returned) */
int *status            /* global status (given and returned) */
);

void sc2sim_getsimpar 
( 
AstKeyMap *keymap,   /* Keymap containing sim parameters (given) */
struct  sc2sim_sim_struct *inx, /* Structure for values from sim
				   keymap file (given and returned */
int *status          /* global status (given and returned) */
);

void sc2sim_getsinglescan
(
double angle,        /* angle of pattern relative to telescope
                        axes in radians anticlockwise (given) */
double pathlength,   /* length of scanpath (arcsec) (given) */
double accel[2],     /* telescope accelerations (arcsec) (given) */
double vmax[2],      /* telescope maximum velocities (arcsec) (given) */
double samptime,     /* sample interval in sec (given) */
int *scancount,      /* number of positions in pattern (returned) */
double **posptr,     /* list of positions (returned) */
int *status          /* global status (given and returned) */
);

void sc2sim_getspread
( 
int numbols,             /* Number of bolometers (given) */
double pzero[],          /* Array to hold response curve offsets 
                            of bolometers in pW (returned) */
double heater[],         /* Array to hold heater factors of bolometers
                            (returned) */
int *status              /* global status (given and returned) */
);

void sc2sim_getstraightpong 
( 
double angle,         /* angle of pattern relative to the telescope
			 axes in radians (given) */
double width,         /* minimum width of PONG pattern in arcsec (given) */
double height,        /* minimum height of PONG pattern in arcsec (given) */
double spacing,       /* grid spacing in arcsec (given) */
double accel[2],      /* telescope accelerations in arcsec/sec (given) */
double vmax[2],       /* telescope maximum velocities in arcsec/sec (given) */
double samptime,      /* sample interval in sec (given) */ 
int nmaps,           /* number of cycles of the pattern */
int *pongcount,       /* number of positions in pattern (returned) */
double **posptr,      /* list of positions (returned) */
int *status           /* pointer to global status (given and returned) */
);

void sc2sim_getweights
( 
double decay,      /* time constant in millisec (given) */
double samptime,   /* sampling time in millisec (given) */
int nweights,      /* number of values to be returned (given) */
double weights[],  /* array to hold returned values (returned) */
int *status        /* global status (given and returned) */
);

void sc2sim_heatrun ( struct sc2sim_obs_struct *inx, 
                      struct sc2sim_sim_struct *sinx, 
                      double coeffs[], double digcurrent, double digmean, 
                      double digscale, char filter[], double *heater, int nbol,
                      double *pzero, double samptime, int *status );

void sc2sim_instrinit
( 
struct sc2sim_obs_struct *inx, /* structure for values from XML file (returned) */
struct sc2sim_sim_struct *sinx, /* structure for values from XML file(returned) */
AstKeyMap *obskeymap,    /* keymap for obs parameters */
AstKeyMap *simkeymap,    /* keymap for sim parameters */
double coeffs[SC2SIM__NCOEFFS],  /* bolometer response coeffs (returned) */
double *digcurrent,      /* digitisation mean current (returned) */
double *digmean,         /* digitisation mean value (returned) */
double *digscale,        /* digitisation scale factore (returned) */
double *elevation,       /* telescope elevation (radians) (returned) */
double weights[],        /* impulse response (returned) */
double **heater,         /* bolometer heater ratios (returned) */
double **pzero,          /* bolometer power offsets (returned) */
double **xbc,            /* X offsets of bolometers in arcsec (returned) */
double **ybc,            /* Y offsets of bolometers in arcsec (returned) */
double **xbolo,          /* Native bolo x-offsets */
double **ybolo,          /* Native bolo x-offsets */
int *status              /* global status (given and returned) */
);

void sc2sim_invf
( 
double sigma,     /* white noise level (given) */
double corner,    /* corner frequency (given) */
double samptime,  /* time in sec between samples (given) */
int nsamples,     /* number of positions in sequence (given) */
double *fnoise,   /* array to hold noise sequence (returned) */
int *status       /* global status (given and returned) * */
);

void sc2sim_invf2d 
( 
double sigma,     /* white noise level (given) */
double corner,    /* corner frequency in per arcsec (given) */
double p,         /* power law to be used (given) */
double pixsize,   /* pixel size in arcsec (given) */
int size,         /* size of square image array (given) */
double *fnoise,   /* array to hold noise image (returned) */
double *spectrum, /* array to hold complex 2-D spectrum (returned) */
int *status       /* global status (given and returned) */
);

void sc2sim_ndfwrdata
( 
struct sc2sim_obs_struct *inx,      /* structure for values from XML (given) */
struct sc2sim_sim_struct *sinx, /* structure for sim values from XML (given)*/
double meanwvm,   /* 225 GHz tau */
char file_name[], /* output file name (given) */
int numsamples,   /* number of samples (given) */
int nflat,        /* number of flat coeffs per bol (given) */
char *flatname,   /* name of flatfield algorithm (given) */
JCMTState *head,  /* header data for each frame (given) */
int *dbuf,        /* simulated data (given) */
int *dksquid,     /* dark SQUID time stream data (given) */
double *fcal,     /* flatfield calibration (given) */
double *fpar,     /* flat-field parameters (given) */
char instrume[],  /* String representing instrument (e.g. "SCUBA-2") (given) */
char filter[],    /* String representing filter (e.g. "850") (given) */
char *dateobs,    /* DateObs string for FITS header */
double *posptr,   /* Pointing offsets from map centre */
int jigsamples,   /* Number of jiggle samples (given) */
double jigptr[][2], /* Array of X, Y jiggle positions (given) */
int *status       /* global status (given and returned) */
);

void sc2sim_ndfwrheat
( 
struct sc2sim_obs_struct *inx,      /* structure for values from XML (given) */
struct sc2sim_sim_struct *sinx, /* structure for sim values from XML (given)*/
char file_name[],  /* output file name (given) */
int numsamples,    /* number of samples (given) */
int nflat,         /* number of flat coeffs per bol (given) */
char *flatname,    /* name of flatfield algorithm (given) */
JCMTState *head,   /* header data for each frame (given) */
int *dbuf,         /* time stream data (given) */
int *dksquid,      /* dark SQUID time stream data (given) */
double *fcal,      /* flat-field calibration (given) */
double *fpar,      /* flat-field parameters (given) */
char filter[],     /* String representing filter (e.g. "850") (given) */
int *status        /* global status (given and returned) */
);

void sc2sim_parcheck
( 
struct sc2sim_obs_struct *inx,      /* structure for values from XML (given) */
struct sc2sim_sim_struct *sinx, /* structure for sim values from XML (given)*/
int *status        /* global status (given and returned) */
);

void sc2sim_ptoi
( 
double flux,       /* input flux in pW (given) */
int ncoeffs,       /* number of coefficients describing response curve
                      (given) */
double coeffs[],   /* array to hold response curve coefficients (given) */
double pzero,      /* calibration offset in pW (given) */
double *current,   /* signal from bolometer in amps (returned) */
int *status        /* global status (given and returned) */
);

void sc2sim_response
( 
double lambda,     /* wavelength in metres (given) */
int ncoeffs,       /* number of coefficients to be returned (given) */
double coeffs[],   /* array to hold returned coefficients (returned) */
int *status        /* global status (given and returned) */
);

void sc2sim_sex2double ( char *string, double *value, int *status );

void sc2sim_simframe
( 
struct sc2sim_obs_struct inx,      /* structure for values from XML (given) */
struct sc2sim_sim_struct sinx, /* structure for sim values from XML (given)*/
int astnaxes[2],             /* dimensions of simulated image (given) */
double astscale,             /* pixel size in simulated image (given) */
double *astsim,              /* astronomical sky (given) */
int atmnaxes[2],             /* dimensions of simulated atm background
                                (given) */
double atmscale,             /* pixel size in simulated atm background
                                (given) */
double *atmsim,              /* atmospheric emission (given) */
double coeffs[],             /* bolometer response coeffs (given) */
AstFrameSet *fset,           /* World Coordinate transformations */
double heater[],             /* bolometer heater ratios (given) */
int nboll,                   /* total number of bolometers (given) */
int frame,                   /* number of current frame (given) */
int nterms,                  /* number of 1/f noise coeffs (given) */
double *noisecoeffs,         /* 1/f noise coeffs (given) */
double *pzero,               /* bolometer power offsets (given) */
double samptime,             /* sample time in sec (given) */
double start_time,           /* time at start of scan in sec  (given) */
double telemission,          /* power from telescope emission (given) */
double *weights,             /* impulse response (given) */
AstMapping *sky2map,         /* Mapping celestial->map coordinates */
double *xbolo,               /* native X offsets of bolometers */
double *ybolo,               /* native Y offsets of bolometers */
double *xbc,                 /* nasmyth X offsets of bolometers */
double *ybc,                 /* nasmyth Y offsets of bolometers */
double *position,            /* nasmyth positions of bolometers */
double *dbuf,                /* generated frame (returned) */
int *status                  /* global status (given and returned) */
);

void sc2sim_simhits ( struct sc2sim_obs_struct *inx, 
                      struct sc2sim_sim_struct *sinx, 
                      double digcurrent, double digmean, double digscale, 
                      char filter[], int maxwrite, obsMode mode, 
		      mapCoordframe coordframe, int nbol, 
                      int rseed, double samptime, int *status);

void sc2sim_simplescan ( struct sc2sim_obs_struct *inx, 
                         struct sc2sim_sim_struct *sinx, 
                         double digcurrent, double digmean, 
                         double digscale, char filter[], 
                         int maxwrite, obsMode mode, int nbol,  
                         double pathlength, double *pzero, int rseed, 
		         double samptime, double scanangle, 
                         double weights[], double *xbc, double *xbolo, 
                         double *ybc, double *ybolo,int *status );

void sc2sim_simulate ( struct sc2sim_obs_struct *inx, 
                       struct sc2sim_sim_struct *sinx, 
		       double coeffs[], double digcurrent, double digmean, 
		       double digscale, char filter[], double *heater, 
		       int maxwrite, obsMode mode, mapCoordframe coordframe,
		       int nbol, double *pzero, int rseed, double samptime, 
		       double weights[], double *xbc, double *xbolo, 
		       double *ybc, double *ybolo, int *status );

void sc2sim_smupath ( int nvert, double vertex_t, int jig_ver[][2],
                      double jig_stepx, double jig_stepy, int movecode,
                      int nppp, double sample_t, double smu_offset,
                      int pathsz, double jigpath[][2], int *status );

void sc2sim_smupos ( double t, double vertex_t, int movecode, 
                     int nvert, double vertxy[][2], double *x, 
                     double *y, int *status );


void sc2sim_dateobs ( double mjdaystart, int maxwrite, double sample_t, 
		      int outscan, char *dateobs, int *status );

#endif /* SC2SIM_DEFINED */
