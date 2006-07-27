#ifndef HEADGEN___src_dsim_dsim_h
#define HEADGEN___src_dsim_dsim_h 
 
 
/*+ dsim_addinvf - add 1/f noise to a series of values */

void dsim_addinvf 
( 
int bol,                 /* bolometer number (given) */
double start_time,       /* time at first sample in seconds (given) */
double samptime,         /* time per sample in seconds (given) */
int nframes,             /* number of samples to calculate (given) */
int nterms,              /* number of noise terms per bolometer (given) */
double *noisecoeffs,     /* bolometer noise coefficients (given) */
double *output,          /* bolometer signal (given and returned) */
int *status              /* global status (given and returned) */
);

/*+ dsim_addpnoise - add photon noise to a flux */

void dsim_addpnoise 
(
double lambda,       /* wavelength in metres (given) */
double bandGHz,      /* bandwidth in GHZ (given) */
double aomega,       /* geometrical optical factor (given) */
double integ_time,   /* Effective integration time in sec (given) */
double *flux,        /* Flux value in pW (given and returned) */
int *status          /* global status (given and returned) */
);

/*+ dsim_atmatrans - calculate sky transmission at given airmass */

void dsim_atmatrans
(
double trans0,       /* % Zenith transmission (given) */
double airmass,      /* airmass (given) */
double *atrans,      /* % atmospheric transmission (returned) */
int *status          /* global status (given and returned) */
);

/*+ dsim_atmtrans - calculate sky transmission from flux */

void dsim_atmtrans
(
double lambda,       /* wavelength in metres (given) */
double flux,         /* flux per bolometer in pW (given) */
double *trans,       /* % atmospheric transmission (returned) */
int *status          /* global status (given and returned) */
);

/*+ dsim_atmsky - calculate sky flux given sky transmission */

void dsim_atmsky
(
double lambda,       /* wavelength in metres (given) */
double trans,        /* % atmospheric transmission (given) */
double *flux,        /* flux per bolometer in pW (returned) */
int *status          /* global status (given and returned) */
);

/*+ dsim_bolnatcoords - get bolometer native coordinates */

void dsim_bolnatcoords
(
double *xbolo,        /* projected X coords of bolometers (returned) */
double *ybolo,        /* projected Y coords of bolometers (returned) */
int *bol,             /* bolometer counter (returned) */
int *status           /* global status (given and returned) */
);

/*+ dsim_bolcoords - get bolometer Nasmyth coordinates */

void dsim_bolcoords
(
char *subname,       /* subarray name, s8a-s4d (given) */
double ra,           /* RA of observation in radians (given) */
double dec,          /* Dec of observation in radians (given) */
double elevation,    /* telescope elevation in radians (given) */
double p,            /* parallactic angle in radians (given) */
char *domain,        /* AST domain name to be used (given) */
AstFrameSet **fset,  /* World coordinate transformations (returned) */
int *bol,            /* bolometer counter (returned) */
double xbc[],        /* projected X coords of bolometers (returned) */
double ybc[],        /* projected Y coords of bolometers (returned) */
int *status          /* global status (given and returned) */
);

/*+ dsim_calctau - calculate sky zenith optical depth */

void dsim_calctau
(
double lambda,       /* wavelength in metres (given) */
double trans,        /* % atmospheric transmission (given) */
double airmass,      /* airmass (given) */
double *tauCSO,      /* CSO optical depth (returned) */
double *tau850,      /* 850 optical depth (returned) */
double *tau450,      /* 450 optical depth (returned) */
int *status          /* global status (given and returned) */
);

/*+ dsim_calctrans - calculate sky zenith % atmospheric transmission */

void dsim_calctrans
(
double lambda,        /* wavelength in metres (given) */
double *trans,        /* % atmospheric transmission (returned) */
double tauCSO,        /* CSO optical depth (given) */
int *status           /* global status (given and returned) */
);

/*+ dsim_calctime - calculte UT + LST arrays given a start time */

void dsim_calctime
( 
double mjdaystart,   /* start time as modified juldate */
double samptime,     /* length of a sample in seconds */
int nsamp,           /* number of samples */
double *ut,          /* returned UT at each sample (mod. juldate) */
double *lst,         /* returned LST at each sample (radians) */
int *status          /* global status (given and returned) */
);

/*+ dsim_checkCommon - check common elements of dxml_struct 
    & dxml_sim_struct */

void dsim_checkCommon
(
struct dxml_struct *inx,
struct dxml_sim_struct *sinx,
int *status
);

/*+ dsim_checkDream - check DREAM-specific elements of dxml_struct 
    & dxml_sim_struct */

void dsim_checkDream
(
struct dxml_struct *inx,
struct dxml_sim_struct *sinx,
int *status
);

/*+ dsim_checkHeat - check HEATRUN-specific elements of dxml_struct 
    & dxml_sim_struct */

void dsim_checkHeat
(
struct dxml_struct *inx,
struct dxml_sim_struct *sinx,
int *status
);

/*+ dsim_checkPong - check PONG-specific elements of dxml_struct 
    & dxml_sim_struct */

void dsim_checkPong
(
struct dxml_struct *inx,
int *status
);

/*+ dsim_checkStare - check STARE-specific elements of dxml_struct 
    & dxml_sim_struct */

void dsim_checkStare
(
struct dxml_struct *inx,
int *status
);

/*+ dsim_crepoints - simulate image of identical gaussian sources */

void dsim_crepoints 
(
double fluxJy,       /* Flux of each point source (given) */
double diam,         /* diameter of telescope aperture in metres (given) */
double fwhm,         /* fwhm of telescope PSF (given) */
double spacingx,     /* spacing in arcsec between sources in X (given) */
double spacingy,     /* spacing in arcsec between sources in Y (given) */
double transmission, /* transmission of optics as a fraction (given) */
double bandGHz,      /* optical bandwidth in GHz (given) */
double pixsize,      /* pixel grid size in arcsec (given) */
int nx,              /* size of image in X (given) */
int ny,              /* size of image in Y (given) */
double *astsim,      /* array to hold returned image (returned) */
int *status          /* global status (given and returned) */
);

/*+ dsim_crepoint - simulate image of a single gaussian source */

void dsim_crepoint 
(
double fluxJy,       /* Flux of each point source (given) */
double diam,         /* diameter of telescope aperture in metres (given) */
double fwhm,         /* fwhm of telescope PSF (given) */
double transmission, /* transmission of optics as a fraction (given) */
double bandGHz,      /* optical bandwidth in GHz (given) */
double pixsize,      /* pixel grid size in arcsec (given) */
int nx,              /* size of image in X (given) */
int ny,              /* size of image in Y (given) */
double *astsim,      /* array to hold returned image (returned) */
int *status          /* global status (given and returned) */
);

/*+ dsim_creslope - simulate image of sloping source */

void dsim_creslope
(
double fluxJy,       /* Flux of example point source (given) */
double diam,         /* diameter of telescope aperture in metres (given) */
double fwhm,         /* fwhm of telescope PSF (given) */
double transmission, /* transmission of optics as a fraction (given) */
double bandGHz,      /* optical bandwidth in GHz (given) */
double pixsize,      /* pixel grid size in arcsec (given) */
int nx,              /* size of image in X (given) */
int ny,              /* size of image in Y (given) */
double *astsim,      /* array to hold returned image (returned) */
int *status          /* global status (given and returned) */
);

/*+ dsim_crotate - rotate a 2-D complex array through 90 degrees */

void dsim_crotate
(
int direction,       /* +1 clockwise, -1 anticlockwise (given) */
int size,            /* square dimension of complex image array (given) */
double *array,       /* image array size*2 by size (given and returned) */
int *status          /* global status (given and returned) */
);

/*+ dsim_digitise - Convert array of currents to integers */

void dsim_digitise
(
int nvals,            /* number of values (given) */
double current[],     /* signal values in amps (given) */
double digmean,       /* mean digitised level (given) */
double digscale,      /* digitisation scale factor (given) */
double digcurrent,    /* current in amps at digmean (given) */
int digits[],         /* digitised currents (returned) */
int *status           /* global status (given and returned) */
);

/*+ dsim_doscan - Simulate a single scan from a scanmap */

void dsim_doscan
(
struct dxml_struct inx,         /* structure for values from XML (given) */
struct dxml_sim_struct sinx,    /* structure for sim values from XML (given)*/
long astnaxes[2],               /* dimensions of simulated image (given) */
double astscale,                /* pixel size in simulated image (given) */
double *astsim,                 /* astronomical sky (given) */
long atmnaxes[2],               /* dimensions of simulated atm background
                                   (given) */
double atmscale,                /* pixel size in simulated atm background
                                   (given) */
double *atmsim,                 /* atmospheric emission (given) */
double coeffs[NCOEFFS],         /* bolometer response coeffs (given) */
double heater[DREAM__MXBOL],    /* bolometer heater ratios (given) */
int nboll,                      /* total number of bolometers (given) */
int nframes,                    /* number of frames in scan (given) */
int nterms,                     /* number of 1/f noise coeffs (given) */
double *noisecoeffs,            /* 1/f noise coeffs (given) */
double *pzero,                  /* bolometer power offsets (given) */
double samptime,                /* sample time in sec (given) */
double start_time,              /* time at start of scan in sec  (given) */
double *tau_bol,                /* Bol. time constants (given) */
double telemission,             /* power from telescope emission (given) */
double *weights,                /* impulse response (given) */
double *xbc,                    /* X offsets of bolometers in arcsec */
double *ybc,                    /* Y offsets of bolometers in arcsec */
double xstart,                  /* Xcoord at start of scan (given) */
double xvel,                    /* X velocity along scan (given) */
double ystart,                  /* Ycoord at start of scan (given) */
double yvel,                    /* Y velocity along scan (given) */
double output[DREAM__MXSIM],    /* series of output values (returned) */
double dbuf[DREAM__MXSIM*DREAM__MXBOL], /* Data for whole scan (returned) */
int *status                     /* global status */
);

/*+ dsim_drand - return a random number with zero mean */

double dsim_drand
(
double sigma        /* sigma of distribution (given) */
);

/*+ dsim_fft2d - 2-D FFT for double, square arrays */

void dsim_fft2d
(
int direction,       /* transform specification +1 or -1 (given) */
int size,            /* square dimension of complex image array (given) */
double *array,       /* image array size*2 by size (given and returned) */
int *status          /* global status (given and returned) */
);

/*+ dsim_fitheat - fit a cubic polynomial to each bolometer */

void dsim_fitheat
(
int nboll,          /* number of bolometers (given) */
int nframes,        /* number of frames in scan (given) */
double *heat,       /* heater values (given) */
double *inptr,      /* measurement values (given) */
double *coptr,      /* coefficients of fit (returned) */
int *status         /* global status (given and returned) */
);

/*+ dsim_four1 - Cooley-Tukey fft by Brenner */

void dsim_four1 
( 
int isign,         /* direction of transform (given) */
int nn,            /* number of complex values (given) */
double data[]      /* complex signal transformed in-place - even indices 
                      real values, odd imaginary (given and returned) */
);

/*+ dsim_getast - sample simulated astronomical image */

void dsim_getast 
( 
double xpos,         /* X-Nas coordinate of sample point in arcsec (given) */
double ypos,         /* Y-Nas coordinate of sample point in arcsec (given) */
double bolscale,     /* bolometer scale in arcsec per pixel (given) */
double astscale,     /* scale of image in arcsec per pixel (given) */
int astsize,         /* size of image (given) */
double *astsim,      /* astronomical image (given) (RA/Dec) */
double *astvalue,    /* value sampled from image (returned) */
int *status          /* global status (given and returned) */
);

/*+ dsim_getast_wcs - sample simulated astronomical image using wcs info */

void dsim_getast_wcs
( 
int nboll,              /* total number of bolometers (given) */
double *xbolo,          /* x-bolometer coordinates for array (given) */
double *ybolo,          /* y-bolometer coordinates for array (given) */
AstCmpMap *bolo2map,    /* mapping bolo->sky image coordinates (given ) */
double *astsim,         /* astronomical image (given) */
int astnaxes[2],       /* dimensions of simulated image (given) */
double *dbuf,           /* pointer to bolo output (returned) */
int *status             /* global status (given and returned) */
);

/*+ dsim_getbilinear - bilinear interpolation on an image */

void dsim_getbilinear 
( 
double xpos,         /* X-coordinate of sample point in arcsec (given) */
double ypos,         /* Y-coordinate of sample point in arcsec (given) */
double scale,        /* scale of image in arcsec per pixel (given) */
int size,            /* size of image (given) */
double *image,       /* astronomical image (given) */
double *value,       /* value sampled from image (returned) */
int *status          /* global status (given and returned) */
);

/*+ dsim_getbols - return characteristics of bolometers */

void dsim_getbols 
(
int numbols,             /* Number of bolometers (given) */
struct bolpix list[],    /* Pixel locations of bolometers (given) */
double distfac,          /* Distortion factor (given) */
double x[],              /* Array to hold X-coordinates of bolometers 
                            in arcsec (returned) */
double y[],              /* Array to hold Y-coordinates of bolometers 
                            in arcsec (returned) */
double pzero[],          /* Array to hold response curve offsets 
                            of bolometers in pW (returned) */
double heater[],         /* Array to hold heater factors of bolometers
                            (returned) */
int *status              /* global status (given and returned) */
);

/*+ dsim_getinvf - return a compressed form of 1/f noise */

void dsim_getinvf 
( 
double sigma,        /* dispersion of broad-band noise (given) */ 
double corner,       /* corner frequency, where 1/f dispersion=sigma (given)*/
double samptime,     /* time per data sample (given) */
double nterms,       /* number of frequencies calculated (given) */
double *noisecoeffs, /* 1/f spectrum (returned) */
int *status          /* global status (given and returned) */
);

/*+ dsim_getobsmode - calculate obs enumerated type from string */

obsMode dsim_getobsmode
( 
char *name,         /* string containing name of observing mode */
int *status         /* global status (given and returned) */
);

/*+ dsim_getpar - Get parameters from arguments */

void dsim_getpar 
(
int argc,                /* argument count (given) */
char **argv,             /* argument list (given) */
struct dxml_struct *inx, /* structure for values from XML file (returned) */
struct dxml_sim_struct *sinx, /* structure for values from XML file (returned) */
int *rseed,              /* seed for random number generator (returned)*/
int *savebols,           /* flag for writing bolometer details (returned) */
int *status              /* global status (given and returned) */
);

/*+ dsim_getpat - return jiggle pattern */

void dsim_getpat 
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

/*+ dsim_getpatpar - Get parameters for pattern calculation */

void dsim_getpatpar 
(
int argc,                /* argument count (given) */
char **argv,             /* argument list (given) */
struct dxml_struct *inx, /* structure for values from XML file (returned) */
char *outfile,           /* name of output text file (returned) */
int *status              /* global status (given and returned) */
);

/*+ dsim_getpointpar - Get parameters for point simulator */

void dsim_getpointpar 
(
int argc,                /* argument count (given) */
char **argv,             /* argument list (given) */
struct dxml_struct *inx, /* structure for values from XML file (returned) */
struct dxml_sim_struct *sinx, /* structure for values from XML file (returned) */
int *rseed,              /* seed for random number generator (returned)*/
double *fluxJy,          /* source flux in Jansky (returned) */
int *mapwidth,           /* width of created map in pixels (returned) */
double *pixsize,         /* pixel size in arcsec (returned) */
int *spacing,            /* point source spacing in arcsec (returned) */
int *status              /* global status (given and returned) */
);

/*+ dsim_getpong - Get coordinates of full PONG pattern */

void dsim_getpong
(
double angle,        /* angle of pattern relative to telescope
                        axes in radians anticlockwise (given) */
int gridcount,       /* number of grid lines (odd) (given) */
double spacing,      /* grid spacing in arcsec (given) */
double accel[2],     /* telescope accelerations (arcsec) (given) */
double vmax[2],      /* telescope maximum velocities (arcsec) (given) */
double samptime,     /* sample interval in sec (given) */
double grid[][2],    /* pong grid coordinates (returned) */
int *pongcount,      /* number of positions in pattern (returned) */
double **posptr,     /* list of positions (returned) */
int *status          /* global status (given and returned) */
);

/*+ dsim_getpongends - Get coordinates of PONG vertices */

void dsim_getpongends
(
int gridcount,           /* number of grid lines (odd) (given) */
double spacing,          /* grid spacing in arcsec (given) */
double grid[][2],        /* coordinates of pong vertices (returned) */
int *status              /* global status (given and returned) */
);

/*+ dsim_getscaling - Get parameters for scaling data to integers */

void dsim_getscaling 
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

/*+ dsim_getscanpat - return scan pattern */

void dsim_getscanpat 
(
long astnaxes[2],    /* dimensions of simulated image (given) */
double astscale,     /* pixel size in simulated image (given) */
int nboll,           /* total number of bolometers (given) */
double *xbc,         /* projected X offsets of bolometers in arcsec (given) */
double *ybc,         /* projected Y offsets of bolometers in arcsec (given) */
int *ncoords,        /* The number of coordinates in pattern (returned) */
double pattern[][2], /* The array to hold the coordinates of the ends of all
                        the scans relative to the bottom-left of the mapped
			area in arcsec (returned) */
int *status          /* global status (given and returned) */
);

/*+ dsim_getscanseg - return scan segment */

void dsim_getscanseg
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

/*+ dsim_getscansegsize - return size of scan segment */

void dsim_getscansegsize
(
double samptime,     /* sample time in sec (given) */
double cstart[2],    /* starting coordinates in arcsec (given) */
double cend[2],      /* ending coordinates in arcsec (given) */
double accel[2],     /* telescope accelerations (arcsec) (given) */
double vmax[2],      /* telescope maximum velocities (arcsec) (given) */
int *size,           /* number of samples in pattern (returned) */
int *status          /* global status (given and returned) */
);

/*+ dsim_getsigma - Calculate photon noise */

void dsim_getsigma
(
double lambda,         /* wavelength in metres (given) */
double bandGHz,        /* bandwidth in GHz (given) */
double aomega,         /* geometrical factor (given) */
double flux,           /* sky power per pixel in pW (given) */
double *sigma,         /* photon noise in pW (returned) */
int *status            /* global status (given and returned) */
);

/*+ dsim_getskypar - Get parameters for point simulator */

void dsim_getskypar 
(
int argc,                /* argument count (given) */
char **argv,             /* argument list (given) */
struct dxml_struct *inx, /* structure for values from XML file (returned) */
struct dxml_sim_struct *sinx, /* structure for values from XML file (returned) */
int *rseed,              /* seed for random number generator (returned)*/
double *exponent,        /* exponent of power spectrum (returned) */
int *status              /* global status (given and returned) */
);

/*+ dsim_getslopepar - Get parameters for slope simulator */

void dsim_getslopepar 
(
int argc,                /* argument count (given) */
char **argv,             /* argument list (given) */
struct dxml_struct *inx, /* structure for values from XML file (returned) */
struct dxml_sim_struct *sinx, /* structure for values from XML file (returned) */
int *rseed,              /* seed for random number generator (returned)*/
double *fluxJy,          /* source flux in Jansky (returned) */
int *mapwidth,           /* width of created map in pixels (returned) */
double *pixsize,         /* pixel size in arcsec (returned) */
char *file_name,         /* name of output file (returned) */
int *status              /* global status (given and returned) */
);

/*+ dsim_getspread - return scattered characteristics of bolometers */

void dsim_getspread
(
int numbols,             /* Number of bolometers (given) */
double pzero[],          /* Array to hold response curve offsets 
                            of bolometers in pW (returned) */
double heater[],         /* Array to hold heater factors of bolometers
                            (returned) */
int *status              /* global status (given and returned) */
);

/*+ dsim_getweights - return weights for smoothing by impulse response */

void dsim_getweights 
( 
double decay,      /* time constant in millisec (given) */
double samptime,   /* sampling time in millisec (given) */
int nweights,      /* number of values to be returned (given) */
double weights[],  /* array to hold returned values (returned) */
int *status        /* global status (given and returned) */
);

/*+ dsim_hor2eq - get telescope position and orientation */

void dsim_hor2eq
( 
double az,          /* Azimuth in radians (given) */
double el,          /* Elevation in radians (given) */
double lst,         /* local sidereal time in radians (given) */
double *ra,         /* Right Ascension in radians (returned) */
double *dec,        /* Declination in radians (returned) */
int *status         /* global status (given and returned) */
);

/*+ dsim_initast - return simulated astronomical image */

void dsim_initast 
(
char *filename,        /* name of input file (given) */
double *pixsize,       /* pixel size in arcsec (returned) */
int naxes[2],         /* dimensions of image (returned) */ 
double **astsim,       /* array to hold returned image (returned) */
AstFrameSet **fitswcs, /* frameset containing WCS of FITS image */
int *status            /* global status (given and returned) */
);

/*+ dsim_initatm - return simulated atmospheric emission */

void dsim_initatm
(
char *filename,    /* name of input file (given) */
double *pixsize,   /* pixel size in arcsec (returned) */
int naxes[2],     /* dimensions of image (returned) */ 
double **atmsim,   /* array to hold returned image (returned) */
int *status        /* global status (given and returned) */
);

/*+ dsim_instrinit - initialise instrument parameters */

void dsim_instrinit
(
int argc,                /* argument count (given) */
char **argv,             /* argument list (given) */
struct dxml_struct *inx, /* structure for values from XML file (returned) */
struct dxml_sim_struct *sinx, /* structure for values from XML file(returned) */
int *rseed,              /* seed for random number generator (returned)*/
double coeffs[NCOEFFS],  /* bolometer response coeffs (returned) */
double *digcurrent,      /* digitisation mean current (returned) */
double *digmean,         /* digitisation mean value (returned) */
double *digscale,        /* digitisation scale factore (returned) */
double *elevation,       /* telescope elevation (radians) (returned) */
AstFrameSet **fset,      /* World coordinate transformations (returned) */
double weights[],        /* impulse response (returned) */
double **heater,         /* bolometer heater ratios (returned) */
double **pzero,          /* bolometer power offsets (returned) */
double **xbc,            /* X offsets of bolometers in arcsec (returned) */
double **ybc,            /* Y offsets of bolometers in arcsec (returned) */
double **xbolo,          /* Native bolo x-offsets */
double **ybolo,          /* Native bolo x-offsets */
int *status              /* global status (given and returned) */
);

/*+ dsim_invf - generate a 1/f plus white noise sequence */

void dsim_invf 
( 
double sigma,     /* white noise level (given) */
double corner,    /* corner frequency (given) */
double samptime,  /* time in sec between samples (given) */
int nsamples,     /* number of positions in sequence (given) */
double *fnoise,   /* array to hold noise sequence (returned) */
int *status       /* global status (given and returned) */
);

/*+ dsim_invf2d - generate a 2-D image of 1/f noise  */

void dsim_invf2d 
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

/*+ dsim_kludgemodjuldate - kludge for the modified julian date */

double dsim_kludgemodjuldate
( 
double ra,           /* Right Ascension in radians (given) */
int *status          /* global status (given and returned) */
);

/*+ dsim_ndfwrdata - generic digitise/compress and store SC2 data as NDF */

void dsim_ndfwrdata
(
double ra,        /* RA of observation in radians (given) */
double dec,       /* Dec of observation in radians (given) */
int add_atm,      /* flag for adding atmospheric emission (given) */
int add_fnoise,   /* flag for adding 1/f noise (given) */
int add_pns,      /* flag for adding photon noise (given) */
int flux2cur,     /* flag for converting flux to current (given) */
double amstart,   /* Airmass at beginning (given) */
double amend,     /* Airmass at end (given) */
double meanwvm,   /* 225 GHz tau */
double obslam,    /* Wavelength */
char file_name[], /* output file name (given) */
int ncol,         /* number of bolometers in column (given) */
int nrow,         /* number of bolometers in row (given) */
double sample_t,  /* sample interval in msec (given) */
char subarray[],  /* name of the subarray */
int numsamples,   /* number of samples (given) */
int nflat,        /* number of flat coeffs per bol (given) */
char *flatname,   /* name of flatfield algorithm (given) */
JCMTState head[], /* header data for each frame (given) */
int *dbuf,        /* simulated data (given) */
int *dksquid,     /* dark SQUID time stream data (given) */
double *fcal,     /* flatfield calibration (given) */
double *fpar,     /* flat-field parameters (given) */
char filter[],    /* String representing filter (e.g. "850") (given) */
double atstart,   /* Ambient temperature at start (Celsius) (given) */
double atend,     /* Ambient temperature at end (Celsius) (given) */
double *posptr,   /* Pointing offsets from map centre */
char *obsmode,    /* Observing mode */
int *status       /* global status (given and returned) */
);

/*+ dsim_ndfwrdream - digitise and compress the DREAM simulation and store
    as NDF */

void dsim_ndfwrdream
(
double ra,         /* RA of observation in radians (given) */
double dec,        /* Dec of observation in radians (given) */
int jig_vert[][2], /* Array with relative jiggle coordinates in units 
                      of pixel distance in case jiggle positions are 
                      visited (given) */
int add_atm,       /* flag for adding atmospheric emission (given) */
int add_fnoise,    /* flag for adding 1/f noise (given) */
int add_pns,       /* flag for adding photon noise (given) */
int flux2cur,      /* flag for converting flux to current (given) */
double amstart,    /* Airmass at beginning (given) */
double amend,      /* Airmass at end (given) */
double meanwvm,    /* 225 GHz tau */
double obslam,     /* Wavelength */
double astflux,    /* Point source flux as specified when ast.fits generated */
int smu_samples,   /* number of samples between jiggle vertices (given) */
double distfac,    /* distortion factor (0=no distortion) (given) */
int conv_shape,    /* convolution function (Gaussian=0) (given) */
double conv_sig,   /* convolution function parameter (given) */
char file_name[],  /* output file name (given) */
int ncol,          /* number of bolometers in column (given) */
int nrow,          /* number of bolometers in row (given) */
double sample_t,   /* sample interval in msec (given) */
char subarray[],   /* Subarray name (given) */ 
int nvert,         /* Nr of vertices in the Jiggle pattern (given) */
int move_code,     /* Code for the SMU move algorithm (given) */
double jig_stepx,  /* The Jiggle step value in -X-direction on the sky 
                      in arcsec (given)*/
double jig_stepy,  /* The Jiggle step value in -Y-direction on the sky 
                      in arcsec (given) */
int ncycle,        /* number of cycles (given) */
int numsamples,    /* number of samples (given) */
int nflat,         /* number of flat coeffs per bol (given) */
char *flatname,    /* name of flatfield algorithm (given) */
JCMTState head[],  /* header data for each frame (given) */
int *dbuf,         /* simulated data (given) */
int *dksquid,      /* dark SQUID time stream data (given) */
double *fcal,      /* flat-field calibration (given) */
double *fpar,      /* flat-field parameters (given) */
char filter[],     /* String representing filter (e.g. "850") (given) */
double atstart,    /* Ambient temperature at start (Celsius) (given) */
double atend,      /* Ambient temperature at end (Celsius) (given) */
char *obsmode,     /* Observing mode */
int *status        /* global status (given and returned) */
);

/*+ dsim_ndfwrheat - digitise and compress the heater simulation and store
    as NDF */

void dsim_ndfwrheat
(
int add_atm,       /* flag for adding atmospheric emission (given) */
int add_fnoise,    /* flag for adding 1/f noise (given) */
int add_pns,       /* flag for adding photon noise (given) */
double heatstart,  /* initial heater setting in pW (given) */
double heatstep,   /* increment of heater setting in pW (given) */
char file_name[],  /* output file name (given) */
int ncol,          /* number of bolometers in column (given) */
int nrow,          /* number of bolometers in row (given) */
double sample_t,   /* sample interval in msec (given) */
char subarray[],   /* name of the subarray */
int numsamples,    /* number of samples (given) */
int nflat,         /* number of flat coeffs per bol (given) */
char *flatname,    /* name of flatfield algorithm (given) */
JCMTState head[],  /* header data for each frame (given) */
int *dbuf,         /* time stream data (given) */
int *dksquid,      /* dark SQUID time stream data (given) */
double *fcal,      /* flat-field calibration (given) */
double *fpar,      /* flat-field parameters (given) */
char filter[],     /* String representing filter (e.g. "850") (given) */
double atstart,    /* Ambient temperature at start (Celsius) (given) */
double atend,      /* Ambient temperature at end (Celsius) (given) */
int *status        /* global status (given and returned) */
);

/*+ dsim_ndfwrpol - digitise and compress the polarimeter simulation and store
    as NDF */

void dsim_ndfwrpol
(
int add_atm,       /* flag for adding atmospheric emission (given) */
int add_fnoise,    /* flag for adding 1/f noise (given) */
int add_pns,       /* flag for adding photon noise (given) */
int flux2cur,      /* flag for converting flux to current (given) */
double distfac,    /* distortion factor (0=no distortion) (given) */
double heatval,    /* heater setting in pW (given) */
char file_name[],  /* output file name (given) */
int ncol,          /* number of bolometers in column (given) */
int nrow,          /* number of bolometers in row (given) */
double sample_t,   /* sample interval in msec (given) */
char subarray[],   /* name of the subarray */
int ncycle,        /* number of cycles (given) */
int numsamples,    /* number of samples (given) */
int nflat,         /* number of flat coeffs per bol (given) */
char *flatname,    /* name of flatfield algorithm (given) */
JCMTState head[],  /* header data for each frame (given) */
int *dbuf,         /* simulated data (given) */
int *dksquid,      /* dark SQUID time stream data (given) */
double *fcal,      /* flatfield calibration (given) */
double *fpar,      /* flat-field parameters (given) */
char filter[],     /* String representing filter (e.g. "850") (given) */
double atstart,    /* Ambient temperature at start (Celsius) (given) */
double atend,      /* Ambient temperature at end (Celsius) (given) */
int *status        /* global status (given and returned) */
);

/*+ dsim_ndfwrpong - digitise and compress a PONG simulation and store
    as NDF */

void dsim_ndfwrpong
(
double ra,        /* RA of observation in radians (given) */
double dec,       /* Dec of observation in radians (given) */
int add_atm,      /* flag for adding atmospheric emission (given) */
int add_fnoise,   /* flag for adding 1/f noise (given) */
int add_pns,      /* flag for adding photon noise (given) */
int flux2cur,     /* flag for converting flux to current (given) */
double amstart,   /* Airmass at beginning (given) */
double amend,     /* Airmass at end (given) */
double meanwvm,   /* 225 GHz tau */
double obslam,    /* Wavelength */
char file_name[], /* output file name (given) */
int ncol,         /* number of bolometers in column (given) */
int nrow,         /* number of bolometers in row (given) */
double sample_t,  /* sample interval in msec (given) */
char subarray[],  /* name of the subarray */
int numsamples,   /* number of samples (given) */
int nflat,        /* number of flat coeffs per bol (given) */
char *flatname,   /* name of flatfield algorithm (given) */
JCMTState head[], /* header data for each frame (given) */
int *dbuf,        /* simulated data (given) */
int *dksquid,     /* dark SQUID time stream data (given) */
double *fcal,     /* flatfield calibration (given) */
double *fpar,     /* flat-field parameters (given) */
char filter[],    /* String representing filter (e.g. "850") (given) */
double atstart,   /* Ambient temperature at start (Celsius) (given) */
double atend,     /* Ambient temperature at end (Celsius) (given) */
double gridcount, /* Number of grid points (given) */
double spacing,   /* Spacing in arcsec (given) */
double angle,     /* Rotation angle of PONG pattern in radians (given) */
char *obsmode,    /* Observing mode */
int *status       /* global status (given and returned) */
);

/*+ dsim_ndfwrscan - digitise and compress the SCAN simulation and store
    as NDF */

void dsim_ndfwrscan
(
int add_atm,      /* flag for adding atmospheric emission (given) */
int add_fnoise,   /* flag for adding 1/f noise (given) */
int add_pns,      /* flag for adding photon noise (given) */
int flux2cur,     /* flag for converting flux to current (given) */
double distfac,   /* distortion factor (0=no distortion) (given) */
char file_name[], /* output file name (given) */
double xstart,    /* X position at start of scan (given) */
double ystart,    /* Y position at start of scan (given) */
double xvel,      /* scan velocity in X (given) */
double yvel,      /* scan velocity in Y (given) */
int ncol,         /* number of bolometers in column (given) */
int nrow,         /* number of bolometers in row (given) */
double sample_t,  /* sample interval in msec (given) */
char subarray[],  /* name of the subarray */
int numsamples,   /* number of samples (given) */
int nflat,        /* number of flat coeffs per bol (given) */
char *flatname,   /* name of flatfield algorithm (given) */
JCMTState head[], /* header data for each frame (given) */
int *dbuf,        /* simulated data (given) */
int *dksquid,     /* dark SQUID time stream data (given) */
double *fcal,     /* flatfield calibration (given) */
double *fpar,     /* flat-field parameters (given) */
char filter[],    /* String representing filter (e.g. "850") (given) */
double atstart,   /* Ambient temperature at start (Celsius) (given) */
double atend,     /* Ambient temperature at end (Celsius) (given) */
int *status       /* global status (given and returned) */
);

/*+ dsim_ndfwrstare - digitise and compress a STARE simulation and store
    as NDF */

void dsim_ndfwrstare
(
AstFrameSet *fset, /* World coordinate transformations (given) */
double ra,         /* RA of observation in radians (given) */
double dec,        /* Dec of observation in radians (given) */
int add_atm,       /* flag for adding atmospheric emission (given) */
int add_fnoise,    /* flag for adding 1/f noise (given) */
int add_pns,       /* flag for adding photon noise (given) */
int flux2cur,      /* flag for converting flux to current (given) */
double distfac,    /* distortion factor (0=no distortion) (given) */
double heatval,    /* heater setting in pW (given) */
char file_name[],  /* output file name (given) */
int ncol,          /* number of bolometers in column (given) */
int nrow,          /* number of bolometers in row (given) */
double sample_t,   /* sample interval in msec (given) */
char subarray[],   /* name of the subarray */
int numsamples,    /* number of samples (given) */
int nflat,         /* number of flat coeffs per bol (given) */
char *flatname,    /* name of flatfield algorithm (given) */
JCMTState head[],  /* header data for each frame (given) */
int *dbuf,         /* simulated data (given) */
int *dksquid,      /* dark SQUID values (given) */
double *fcal,      /* flatfield calibration (given) */
double *fpar,      /* flatfield parameters (given) */
char filter[],     /* String representing filter (e.g. "850") (given) */
double atstart,    /* Ambient temperature at start (Celsius) (given) */
double atend,      /* Ambient temperature at end (Celsius) (given) */
char *obsmode,     /* Observing mode */
int *status        /* global status (given and returned) */
);

/*+ dsim_pongframe - Simulate a single frame from a PONG map */

void dsim_pongframe
(
struct dxml_struct inx,      /* structure for values from XML (given) */
struct dxml_sim_struct sinx, /* structure for sim values from XML (given)*/
int astnaxes[2],            /* dimensions of simulated image (given) */
double astscale,             /* pixel size in simulated image (given) */
double *astsim,              /* astronomical sky (given) */
long atmnaxes[2],            /* dimensions of simulated atm background
                                (given) */
double atmscale,             /* pixel size in simulated atm background
                                (given) */
double *atmsim,              /* atmospheric emission (given) */
double coeffs[],             /* bolometer response coeffs (given) */
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
AstCmpMap *bolo2map,         /* mapping bolo->sky image coordinates */
double *xbolo,               /* native X offsets of bolometers */
double *ybolo,               /* native Y offsets of bolometers */
double *xbc,                 /* nasmyth X offsets of bolometers */
double *ybc,                 /* nasmyth Y offsets of bolometers */
double *position,            /* nasmyth positions of bolometers */
double *dbuf,                /* generated frame (returned) */
int *status                  /* global status (given and returned) */
);

/*+ dsim_ptoi - convert input pW to bolometer current */

void dsim_ptoi 
( 
double flux,       /* input flux in pW (given) */
int ncoeffs,       /* number of coefficients describing response curve
                      (given) */
double coeffs[],   /* array to hold response curve coefficients (given) */
double pzero,      /* calibration offset in pW (given) */
double *current,   /* signal from bolometer in amps (returned) */
int *status        /* global status (given and returned) */
);

/*+ dsim_response - return coefficients of average bolometer response */

void dsim_response 
( 
double lambda,     /* wavelength in metres (given) */
int ncoeffs,       /* number of coefficients to be returned (given) */
double coeffs[],   /* array to hold returned coefficients (returned) */
int *status        /* global status (given and returned) */
);

/*+ dsim_saveast - store a simulated astronomical image in a FITS file */

void dsim_saveast 
(
char file_name[], /* output file name (given) */
double fluxJy,       /* Flux of each point source (given) */
double diam,         /* diameter of telescope aperture in metres (given) */
double fwhm,         /* fwhm of telescope PSF (given) */
double spacingx,     /* spacing in arcsec between sources in X (given) */
double spacingy,     /* spacing in arcsec between sources in Y (given) */
double transmission, /* transmission of optics as a fraction (given) */
double bandGHz,      /* optical bandwidth in GHz (given) */
double pixsize,   /* distance between samples in arcsec (given) */
int nbolx,        /* number of bolometers in X (given) */
int nboly,        /* number of bolometers in Y (given) */
double *dbuf,     /* simulated data (given) */
int *status       /* global status (given and returned) */
);

/*+ dsim_savesky - store a simulated sky image in a FITS file */

void dsim_savesky 
(
char file_name[], /* output file name (given) */
double sigma,     /* dispersion at corner frequency  (given) */
double corner,    /* corner frequency of the noise spectrum (given) */
double p,         /* power-law exponent (given) */
double pixsize,   /* distance between samples in arcsec (given) */
int nx,           /* size in X (given) */
int ny,           /* size in Y (given) */
double *dbuf,     /* simulated data (given) */
int *status       /* global status (given and returned) */
);

/*+ dsim_simframe - Simulate a single frame of bolometer data */

void dsim_simframe
(
struct dxml_struct inx,      /* structure for values from XML (given) */
struct dxml_sim_struct sinx, /* structure for sim values from XML (given)*/
int astnaxes[2],            /* dimensions of simulated image (given) */
double astscale,             /* pixel size in simulated image (given) */
double *astsim,              /* astronomical sky (given) */
int atmnaxes[2],            /* dimensions of simulated atm background
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

/*+ dsim_smooth - apply smoothing kernel */

void dsim_smooth 
( 
int nweights,      /* number of values in kernel (given) */
double weights[],  /* smoothing kernel (given) */
int numvals,       /* number of values in dataset (given) */
double output[],   /* dataset to be smoothed (given and returned) */
int *status        /* global status (given and returned) */
);

/*+ dsim_starerange - Determine size of stare array to cover DREAM pattern */

void dsim_starerange 
( 
int nbolx,        /* Number of DREAM bolometers in X (given) */
int nboly,        /* Number of DREAM bolometers in Y (given) */
int ngrid,        /* number of grid coordinates within jiggle area (given) */
int gridpts[][2], /* relative grid coordinates within jiggle area (given) */
int *starebolx,   /* Number of stare bolometers needed in X (returned) */ 
int *stareboly,   /* Number of stare bolometers needed in X (returned) */
int *nextxpl,     /* Number of extra bolometers at the left (returned) */
int *nextypb,     /* Number of extra bolometers at the bottom (returned) */
int *status       /* global status (given and returned) */
);

/*+ dsim_telpos - get telescope position and orientation */

void dsim_telpos
( 
double ra,           /* Right Ascension in radians (given) */
double dec,          /* Declination in radians (given) */
double lst,          /* local sidereal time in radians (given) */
double *az,          /* Azimuth in radians (returned) */
double *el,          /* Elevation in radians (returned) */
double *p,           /* Parallactic angle in radians (returned) */
int *status          /* global status (given and returned) */
);

 
 
#endif
