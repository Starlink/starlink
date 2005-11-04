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
int npts,                 /* number of data points (given) */
double *x,                /* observed values (given) */
double *y,                /* observed values (given) */
double *coeffs,           /* coefficients of fit (returned) */
double *variances,        /* variances of fit (returned) */
int *status               /* global status (given and returned) */
);

/*+ sc2math_flatten - apply flat field correction to set of frames */

void sc2math_flatten
(
int nboll,          /* number of bolometers (given) */
int nframes,        /* number of frames in scan (given) */
char *flatname,     /* name of flatfield algorithm (given) */
int nflat,          /* number of flatfield parameters (given) */
double *fcal,       /* calibration coefficients (given) */
double *fpar,       /* calibration parameters (given) */
double *inptr,      /* measurement values (given and returned) */
int *status         /* global status (given and returned) */
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
int norder,          /* degree of matrix (given) */
double array[10][10],  /* given matrix, its inverse is returned 
                         (given and returned) */
double *det,           /* determinant of ARRAY (returned) */
int *status          /* global status (given and returned) */
);

/*+ sc2math_recurfit - fit a set of values with outlier rejection */

void sc2math_recurfit 
( 
int despike,            /* flag for spike removal (given) */
int npts,               /* number of data points (given) */
int nterms,             /* number of combined waveforms (given) */
double *standard_waves,   /* values of standard waveforms (given) */
double *standard_weights, /* if volts[j] is not to be used, 
                            standard_weights[j] = 0.0, otherwise
                            standard_weights[j] = 1.0 (given) */
double *volts,            /* observed values (given) */
double *used_weights,     /* if volts[j] was not used, 
                            used_weights[j] = 0.0, otherwise 
                            used_weights[j] = 1.0 (returned) */
double *fitted_volts,     /* combined waveform computed from the fit 
                            (returned) */
double *coeffs,           /* coefficients of fit (returned) */
double *variances,        /* variances of fit (returned) */
int *nbad,              /* number of points rejected as suspected
                            "spikes" (returned) */
int *status             /* status must be 0 on entry. 
                            If no valid fit was found, SAI__ERROR is
                            returned (given and returned) */
);

/*+  sc2math_regres - multiple linear regression fit */

void sc2math_regres 
( 
int npts,      /* number of data points (given) */
int nterms,    /* number of combined waveforms (given) */
double *x,       /* values of standard waveforms (given) */
double *y,       /* observed values (given) */
double *weight,  /* weight for each observed value (given) */
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

/*+ sc2math_setcal - set flatfield calibration for a bolometer */

void sc2math_setcal
( 
int nboll,         /* total number of bolometers (given) */
int bol,           /* number of current bolometer (given) */
int numsamples,    /* number of data samples (given) */
double values[],   /* measurements by bolometer (given) */
int ncal,          /* number of calibration measurements (given) */
double heat[],     /* calibration heater settings (given) */
double calval[],   /* calibration measurements for all bolometers (given) */
double lincal[2],  /* calibration parameters (returned) */
int *status        /* global status (given and returned) */
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

/*+  sc2math_wavegen - generate sine and cosine signals */

void sc2math_wavegen 
( 
double period,                 /* period in array elements (given) */
int length,                  /* length of arrays (given) */
double *sine,                  /* generated sine wave (returned) */
double *cosine,                /* generated cosine wave (returned) */
int *status                  /* global status (given and returned) */
);

