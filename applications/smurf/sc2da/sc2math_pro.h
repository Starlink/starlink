#ifndef HEADGEN____sc2math_pro_h
#define HEADGEN____sc2math_pro_h

#include "../smurf_typ.h"

/*+ sc2math_calcmapwt - Make weight matrix for DREAM solution*/

void sc2math_calcmapwt
(
char *subname,          /* subarray name (given) */
int nbolx,              /* first dimension of subarray (given) */
int nboly,              /* second dimension of subarray (given) */
int *qual,              /* quality array, 0=>good, 1=>bad (given) */
int conv_shape,         /* flag for type of interpolation (given) */
double conv_sig,        /* interpolation parameter (given) */
double gridstep,        /* size of reconstruction grid step in arcsec (given) */
int nvert,              /* number of vertices in SMU path (given) */
int leg_len,            /* number of millisec between vertices (given) */
double sample_t,        /* time between samples in millisec (given) */
int jig_vert[][2],      /* Table with relative vertex coordinates in time
                           (given) */
double jig_stepx,       /* The step size in -X- direction between Jiggle
                           positions in arcsec (given) */
double jig_stepy,       /* The step size in -Y- direction between Jiggle
                           positions in arcsec (given) */
int smu_move,           /* The code for the SMU waveform that determines the
                           SMU motion (given) */
double smu_offset,      /* smu timing offset in msec (given) */
int ngrid,              /* number of grid coordinates (given) */
int gridpts[][2],       /* relative grid coordinates (given) */
dim_t gridwtsdim[],     /* dimensions of gridwts array (returned) */
double **gridwts,       /* Pointer to array of sky grid weights (returned) */
int *invmatdim,         /* dimension of inverted matrix (returned) */
double **invmat,        /* pointer to inverted matrix (returned) */
int *status             /* global status (given and returned) */
);

/*+ sc2math_calcmean - calculate the mean and dispersion of numbers */

void sc2math_calcmean
(
int num,                /* number of values (given)*/
double *values,         /* set of numbers (given) */
double *meanvalue,      /* mean of dataset (returned) */
double *sigma,          /* dispersion of dataset (returned) */
int *status             /* global status (given and returned) */
);

/*+ sc2math_choles - Factorize symmetric positive definite matrix */

void sc2math_choles
(
int n,         /* Dimension of the matrix (given) */
double a[],    /* Square matrix of NxN values.
                  Only the lower-left triangle is given and returned.
                  (given and returned) */
int *loc,      /* Diagonal index in A giving the problem or the lowest value.
                  (returned) */
double *dmin,  /* Lowest value or problem value (returned) */
int *ierr      /* Error code
                   0 : Normal return.
                   1 : There is loss of significance.
                  -1 : Matrix not positive definite.
                  (returned) */
);

/*+ sc2math_cholesky - Invert matrix according to the Cholesky method */

void sc2math_cholesky
(
int nunkno,     /* The number of unknowns (given) */
double lmat[],  /* The lower left triangle of the equation matrix.
                   This is a 1 dimensional array of dimension
                   NUNKNO*(NUNKNO+1)/2 which must be filled before the
                   call of this routine, and which is changed into the
                   inverse matrix (given and returned) */
int *loc,       /* Index nr in LMAT giving the lowest value.
                   This must be a diagonal element (returned) */
double *dmin,   /* Lowest value or problem value of the diagonal element
                   with index nr LOC (returned) */
int *err        /* Possible error code in factorizing the matrix (returned) */
);

/*+ sc2math_clipmean - calculate a sigma-clipped mean of data */

void sc2math_clipmean
(
double sigfac,          /* rejection number of times sigma (given) */
int num,                /* number of values (given)*/
double *values,         /* data values (given) */
double *mean,           /* clipped mean of data values (returned) */
int *status             /* global status (given and returned) */
);

/*+ sc2math_conval - Calculate a value of the convolution function */

double sc2math_conval
(
int conv_shape,     /* Code for convolution function (given) */
double conv_sig,    /* Convolution function parameter (given) */
double dx,          /* Distance from the centre in X (given) */
double dy,          /* Distance from the centre in Y (given) */
int *status         /* global status (given and returned) */
);

/*+  sc2math_convolve - convolve a dataset with a filter */

void sc2math_convolve
(
int filtlen,      /* number of elements in filter (given) */
int filtmid,      /* index of filter centre (given) */
double *filter,     /* convolving filter (given) */
int datalen,      /* length of dataset (given) */
double *input,      /* input dataset (given) */
double *output,     /* convolved dataset (returned) */
int *status       /* global status (given and returned) */
);

/*+ sc2math_cubfit - fit a set of values with a cubic */

void sc2math_cubfit
(
dim_t npts,               /* number of data points (given) */
const double *x,          /* observed values (given) */
const double *y,          /* observed values (given) */
double *coeffs,           /* coefficients of fit (returned) */
double *variances,        /* variances of fit (returned) */
int *status               /* global status (given and returned) */
);

/*+ sc2math_eq0  - Make equation matrix */

void sc2math_eq0
(
int nunkno,        /* The number of unknowns (given) */
double lcoef[],    /* The known values (given) */
double lmat[]      /* equation matrix (given and returned) */
);

/*+ sc2math_fitsky - fit a sky baseline for each bolometer */

void sc2math_fitsky
(
int cliptype,          /* type of sigma clipping (given) */
dim_t nboll,          /* number of bolometers (given) */
dim_t nframes,        /* number of frames in scan (given) */
dim_t ncoeff,         /* number of coefficients (given) */
const double *inptr,   /* measurement values (given) */
double *coptr,         /* coefficients of fit (returned) */
int *status            /* global status (given and returned) */
);

/*+ sc2math_fitskyi - fit a sky baseline to integer data for each bolometer */

void sc2math_fitskyi
(
int cliptype,          /* type of sigma clipping (given) */
dim_t nboll,          /* number of bolometers (given) */
dim_t nframes,        /* number of frames in scan (given) */
dim_t ncoeff,         /* number of coefficients (given) */
const int *inptr,      /* measurement values (given) */
double *coptr,         /* coefficients of fit (returned) */
int *status            /* global status (given and returned) */
);

/*+ sc2math_flat2mask - produce dead pixel mask from flatfield */

void sc2math_flat2mask
(
dim_t nboll,              /* number of bolometers (given) */
const char *flatname,     /* name of flatfield algorithm (given) */
int nflat,                /* number of flatfield parameters (given) */
const double *fcal,       /* calibration coefficients (given) */
const double *fpar,       /* calibration parameters (given) */
int *mask,                /* pixel mask (returned) */
int *status               /* global status (given and returned) */
);

/*+ sc2math_flatten - apply flat field correction to set of frames */

dim_t sc2math_flatten
(
dim_t nboll,              /* number of bolometers (given) */
dim_t nframes,            /* number of frames in scan (given) */
const char *flatname,     /* name of flatfield algorithm (given) */
int nflat,                /* number of flatfield parameters (given) */
const double *fcal,       /* calibration coefficients (given) */
const double *fpar,       /* calibration parameters (given) */
double *inptr,            /* measurement values (given and returned) */
int *status               /* global status (given and returned) */
);

/*+ sc2math_get_cycle - Return data for a single measurement cycle */

void sc2math_get_cycle
(
int cur_cycle,    /* current cycle number (given) */
int nsam_cycle,   /* number of samples per cycle (given) */
int ncycle,       /* total number of cycles (given) */
int r_nbol,       /* number of bolometers (given) */
double drdata[],  /* full raw data set (given) */
double sbuf[],    /* data for the current cycle (returned) */
int *status       /* global status (given and returned) */
);

/*+ sc2math_gridext - Calculate the extent of the sky grid */

void sc2math_gridext
(
int ngrid,           /* Number of grid positions within the jiggle area
                        (given) */
int gridpts[][2],    /* Table with relative grid offsets for a single
                        bolometer (given) */
int *xmin,           /* Grid limit in X-dir (returned) */
int *xmax,           /* Grid limit in X-dir (returned) */
int *ymin,           /* Grid limit in Y-dir (returned) */
int *ymax,           /* Grid limit in Y-dir (returned) */
int *status          /* global status (given and returned) */
);

/*+ sc2math_interpwt - Calculate the weight matrix for spatial interpolation */

void sc2math_interpwt
(
int npath,           /* Nr of rows (given) */
int ngrid,           /* Nr of columns (given) */
int conv_shape,      /* Code for convolution function (given) */
double conv_sig,     /* Convolution function parameter (given) */
double sample_t,     /* Time per path point in millisec (given) */
double tbol,         /* Bolometer time constant in millisec (given) */
double jigpath[][2], /* Table with jiggle path pos (given) */
int jigpts[][2],     /* Table with grid positions (given) */
double b[],          /* the weight factors (returned) */
int *status          /* (given and returned) */
);

/*+ sc2math_invpdm - Invert positive definite matrix */

void sc2math_invpdm
(
int n,       /* Dimension of the matrix (given) */
double a[]   /* Square Matrix with NxN points.
                Only the lower-left triangle is given.
                (given and returned) */
);

/*+ sc2math_jig2grid - convert DREAM jiggle coordinates to grid coordinates */

void sc2math_jig2grid
(
char *subname,          /* subarray name (given) */
double grid_step,       /* size of reconstruction grid step in arcsec (given) */
int npath,              /* number of steps in path (given) */
double jigpath[][2],    /* jiggle path coordinates in arcsec (given) */
double jiggrid[][2],    /* jiggle path in grid coordinates (returned) */
int *status             /* global status (given and returned) */
);

/*+ sc2math_linfit - straight line fit */

void sc2math_linfit
(
dim_t np,            /* number of points (given) */
const double x[],     /* X data (given) */
const double y[],     /* Y data (given) */
const double wt[],    /* weights (given) */
double *grad,         /* slope (returned) */
double *cons,         /* offset (returned) */
int *status           /* global status (given and returned) */
);

/*+ sc2math_mapsolve - Reconstruct SCUBA-2 DREAM data in a single step */

void sc2math_mapsolve
(
int nframes,              /* no of data frames (given) */
int nbolx,                /* number of bolometers in X (given) */
int nboly,                /* number of bolometers in Y (given) */
int gridext[],            /* Table of grid extents for a single
                             bolometer (given) */
double gridsize,          /* size in arcsec of grid step (given) */
int jigext[],             /* Table of SMU pattern extents for a single
                             bolometer (given) */
double jigsize,           /* size in arcsec of SMU step (given) */
double *interpwt,         /* interpolation weights (given) */
double *invmat,           /* inverted matrix (given) */
int *qual,                /* bolometer quality array (given) */
double *psbuf,            /* flatfielded data set [nbolx.nboly.nframes](given) */
int maxmap,               /* maximum size of reconstructed map (given) */
dim_t dims[],             /* actual dimensions of map (returned) */
double *map,              /* Solved intensities (returned) */
double *pbolzero,         /* bolometer zero points (returned) */
int *status
);

/*+  sc2math_martin - spike removal from chop-scan data */

void sc2math_martin
(
double period,       /* chop period in samples (given) */
int maxlen,        /* length of signal (given) */
double *signal,      /* signal to be cleaned (given and returned) */
char *badscan,      /* record of spikes removed (returned) */
int *nbad,         /* number of points removed (returned) */
int *status        /* global status (given and returned) */
);

/*+  sc2math_matinv - invert a symmetric matrix */

void sc2math_matinv
(
int norder,            /* degree of matrix (given) */
double array[10][10],  /* given matrix, its inverse is returned
                         (given and returned) */
double *det,           /* determinant of ARRAY (returned) */
int *status            /* global status (given and returned) */
);

/*+ sc2math_msv - Multiply matrix with column vector */

void sc2math_msv
(
int m,        /* Number of unknowns (given) */
double s[],   /* Lower left triangle of a symmetric matrix of MxM (given) */
double v[],   /* Vector with dimension M (given) */
double x[]    /* Vector with dimension M with the result (returned) */
);

/*+ sc2math_pathwts - Pixel weights for SMU path */

void sc2math_pathwts
(
int conv_shape,       /* Code for convolution function (given) */
double conv_sig,      /* Convolution function parameter (given) */
int npath,            /* Nr of rows in wtpix (given) */
double jigpath[][2],  /* Table with jiggle path pos. (given) */
int ncol,             /* Nr of columns in wtpix (given) */
int gridpts[][2],     /* Table with grid positions (given) */
double wtpix[],       /* Matrix with pixel weights (returned) */
int *status           /* global status (given and returned) */
);

/*+ sc2math_recurfit - fit a set of values with outlier rejection */

void sc2math_recurfit
(
int despike,              /* flag for spike removal (given) */
dim_t npts,               /* number of data points (given) */
int nterms,               /* number of combined waveforms (given) */
double *standard_waves,   /* values of standard waveforms (given) */
double *standard_weights, /* if volts[j] is not to be used,
                            standard_weights[j] = 0.0, otherwise
                            standard_weights[j] = 1.0 (given) */
const double *volts,      /* observed values (given) */
double *used_weights,     /* if volts[j] was not used,
                            used_weights[j] = 0.0, otherwise
                            used_weights[j] = 1.0 (returned) */
double *fitted_volts,     /* combined waveform computed from the fit
                            (returned) */
double *coeffs,           /* coefficients of fit (returned) */
double *variances,        /* variances of fit (returned) */
dim_t *nbad,              /* number of points rejected as suspected
                             "spikes" (returned) */
int *status               /* status must be 0 on entry.
                            If no valid fit was found, SAI__ERROR is
                            returned (given and returned) */
);

/*+  sc2math_regres - multiple linear regression fit */

void sc2math_regres
(
dim_t npts,      /* number of data points (given) */
int nterms,      /* number of combined waveforms (given) */
const double *x,       /* values of standard waveforms (given) */
const double *y,       /* observed values (given) */
const double *weight,  /* weight for each observed value (given) */
double *yfit,    /* values of Y computed from the fit (returned) */
double *a0,      /* constant term in fit (returned) */
double *a,       /* coefficients of fit (returned) */
double *sigma0,  /* standard deviation of A0 (returned) */
double *sigmaa,  /* array of standard deviations for coefficients
                   (returned) */
double *r,       /* array of linear correlation coefficients
                   (returned) */
double *rmul,    /* multiple linear correlation coefficient (returned) */
double *chisqr,  /* reduced chi square for fit (returned) */
double *ftest,   /* value of F for test of fit (returned) */
double *perr,    /* probable error in deviation of a single point from
                   the fit (returned) */
int *status    /* status must be OK on entry
                   on exit, STATUS = OK => fit ok
                   STATUS = DITS__APP_ERROR => exact fit (no noise)
                   (given and returned) */
);

/*+  sc2math_remsine - remove sine wave from scan data */

void sc2math_remsine
(
int phase,        /* position in scan corresponding to zero phase
                      of the superimposed sine (given) */
double period,      /* period in samples of the sine wave (given) */
int scanlen,      /* length of scan (given) */
double *scan,       /* the time series of measurements (given and
                      returned) */
double *amplitude,  /* amplitude of the sine wave (returned) */
int *status       /* global status (given and returned) */
);

/*+ sc2math_response - Calculate the response value */

void sc2math_response
(
double f[],   /* Input function (given) */
double w[],   /* Impulse response function (given) */
int wdim,     /* Dimension of W (given) */
int ix,       /* Index corresponding to response (given) */
double *r     /* Response value (returned) */
);

/*+ sc2math_setcal - set flatfield calibration for a bolometer */

void sc2math_setcal
(
dim_t nboll,             /* total number of bolometers (given) */
dim_t bol,               /* number of current bolometer (given) */
dim_t numsamples,        /* number of data samples (given) */
const double values[],   /* measurements by bolometer (given) */
int ncal,                /* number of calibration measurements (given) */
const double heat[],     /* calibration heater settings (given) */
const double calval[],   /* calibration measurements for all bolometers (given) */
double lincal[2],        /* calibration parameters (returned) */
int *status              /* global status (given and returned) */
);

/*+ sc2math_setcaldec - set decreasing flatfield calibration */

void sc2math_setcaldec
(
dim_t nboll,             /* total number of bolometers (given) */
dim_t bol,               /* number of current bolometer (given) */
const double dvalue,     /* representative data number (given) */
int ncal,                /* number of calibration measurements (given) */
const double heat[],     /* calibration heater settings (given) */
const double calval[],   /* calibration measurements for all bolometers (given) */
double lincal[2],        /* calibration parameters (returned) */
int *status              /* global status (given and returned) */
);

/*+ sc2math_setcalinc - set increasing flatfield calibration */

void sc2math_setcalinc
(
dim_t nboll,             /* total number of bolometers (given) */
dim_t bol,               /* number of current bolometer (given) */
const double dvalue,     /* representative data number (given) */
int ncal,                /* number of calibration measurements (given) */
const double heat[],     /* calibration heater settings (given) */
const double calval[],   /* calibration measurements for all bolometers (given) */
double lincal[2],        /* calibration parameters (returned) */
int *status              /* global status (given and returned) */
);

/*+ sc2math_sigmaclip - do sigma clipping on a straight-line fit */

void sc2math_sigmaclip
(
int type,             /* 0 for double sided clip,
                        >0 positive clip,
                        <0 negative clip (given) */
dim_t np,            /* number of points (given) */
const double x[],     /* X data (given) */
const double y[],     /* Y data (given) */
double wt[],          /* weights (returned) */
double *grad,         /* slope (returned) */
double *cons,         /* offset (returned) */
int *status           /* global status (given and returned) */
);

/*+  sc2math_sinedemod - sine wave demodulate a signal */

void sc2math_sinedemod
(
int length,          /* length of the signal array (given) */
double *sine,          /* sine wave (given) */
double *cosine,        /* cosine wave (given) */
double *signal,        /* signal (given) */
double *amplitude,     /* modulation amplitude (returned) */
double *phase,         /* phase of signal (returned) */
int *status          /* global status (given and returned) */
);

/*+ sc2math_smooth - apply smoothing kernel */

void sc2math_smooth
(
int nweights,      /* number of values in kernel (given) */
double weights[],  /* smoothing kernel (given) */
int numvals,       /* number of values in dataset (given) */
double output[],   /* dataset to be smoothed (given and returned) */
int *status        /* global status (given and returned) */
);

/*+ sc2math_smupath - Calculate the path positions of the SMU */

void sc2math_smupath
(
int nvert,           /* number of vertices in the jiggle pattern,
                        implemented are :
                        =1 : No visit of points.
                        At the moment a circle but that does not work !
                        =4 : Visit 4 points on a square.
                        =5 : Visit 5 points on a '+'
                        =8 : Visit 8 points on a star. (This is the best)
                        (given) */
double vertex_t,     /* Time for movement between vertices in msec (given) */
int jig_vert[][2],   /* Table with relative vertex coordinates in time
                        (given) */
double jig_stepx,    /* The step size in -X- direction between Jiggle
                        positions in arcsec (given) */
double jig_stepy,    /* The step size in -Y- direction between Jiggle
                        positions in arcsec (given) */
int movecode,        /* The code for the SMU waveform that determines the
                        SMU motion (given) */
int nppp,            /* The number of calculated coordinates in the path
                        between 2 successive vertices (given) */
double sample_t,     /* time between samples in msec (given) */
double smu_offset,   /* smu timing offset in msec (given) */
int pathsz,          /* maximum no of path points (given) */
double jigpath[][2], /* Buffer containing the X and Y coordinates of each
                        point in the path of the SMU during the Jiggle in
                        units of arcsec (returned) */
int *status          /* global status (given and returned) */
);

/*+ sc2math_smupos - Calculate the SMU position at an instant */

void sc2math_smupos
(
double t,           /* Time from start of pattern in msec (given) */
double vertex_t,    /* Time for movement between vertices in msec (given) */
int movecode,       /* The code for the SMU waveform that determines the
                       SMU motion (given) */
int nvert,          /* number of vertices in the DREAM pattern (given) */
double vertxy[][2], /* Table of vertex offsets in arcsec (given) */
double *x,          /* calculated X-position (returned) */
double *y,          /* calculated Y-position (returned) */
int *status         /* global status (given and returned) */
);

/*+ sc2math_sol - Solution of least square fit */

void sc2math_sol
(
int nunkno,      /* The number of unknowns (given) */
int nequ,        /* The number of observed points (given) */
double lmat[],   /* lower left triangle of inverted equation matrix
                    (given) */
double lvec[],   /* array containing the known vector (given) */
double lssum,    /* The sum of the square of known terms (given) */
double *lme,     /* Quality measure of the Least Square Solution (returned) */
double lmex[],   /* rms errors of the vector of solutions (returned) */
double lsol[]    /* solved parameters of the Least Square Solution
                    (returned) */
);

/*+ sc2math_trace - provide a flag for debugging level */

int sc2math_trace
(
int value       /* trace level (given) */
);

/*+ sc2math_vec - Make known vector */

void sc2math_vec
(
int nunkno,        /* The number of unknowns (given) */
double lcoef[],    /* The known values (given) */
double lknow,      /* The measured point, or observed value (given) */
double lvec[],     /* array to contain the known vector
                      (given and returned) */
double *lssum      /* sum of the square of known terms (returned) */
);

/*+  sc2math_wavegen - generate sine and cosine signals */

void sc2math_wavegen
(
double period,                 /* period in array elements (given) */
int length,                  /* length of arrays (given) */
double *sine,                  /* generated sine wave (returned) */
double *cosine,                /* generated cosine wave (returned) */
int *status                  /* global status (given and returned) */
);



#endif
