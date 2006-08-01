#ifndef HEADGEN___src_dream_dream_h
#define HEADGEN___src_dream_dream_h 
 
 
/*+ dream_2store - Store 2-dim data file to disk */

void dream_2store 
( 
char *foutput,     /* output file name (given) */
int nxsize,        /* image dimension (given) */
int nysize,        /* image dimension (given) */
double *buf,       /* pixel values (given) */
int *status        /* global status (given and returned) */
);

/*+ dream_bolinfo - Return information per bolometer */

void dream_bolinfo 
(
int boln,           /* bolometer number within subarray (given) */
double *tau_bol,    /* Bolometer time constant in msec (returned) */ 
double *bol_calib,  /* calibration factor (returned) */
char *bol_name,     /* name of bolometer (returned) */
int *sample_ord     /* The group in which the data sampling moment for
                       boln takes place (returned) */
);

/*+ dream_bolinit - initialise bolometer data */

void dream_bolinit
(
int sarray,        /* subarray number 1-4 (given) */
int nx,            /* number of bolometers in X (given) */
int ny,            /* number of bolometers in Y (given) */
int *status        /* global status (given and returned) */
);

/*+ dream_boljiginx - Find bolometer and jiggle indices */

void dream_boljiginx
(
int igrid,       /* Index in grid image (given) */
int nbolx,       /* Number of bolometers in X (given) */
int nboly,       /* Number of bolometers in Y (given) */
int nxpsol,      /* Nr of points in X-dir (given) */
int nextxpl,     /* Nr of extra cols left side (given) */
int nextypb,     /* Nr of extra rows bottom side (given) */
int *ibol,       /* Index in Bolometer table (returned) */
int *colj,       /* Jiggle offset in X-dir (returned) */
int *rowj        /* Jiggle offset in Y-dir (returned) */
);

/*+ dream_convdist - Calculate the distance for a given PSF response */

double dream_convdist 
(
int conv_shape,   /* Code for the convolution function. 
                     0 - Gaussian (given) */
double conv_sig,  /* Convolution function parameter.
                     If the convolution function is gaussian :
                       CONVAL(r) = exp[-r^2/(2.s^2)]
                       r - Distance in units of HPBW from the centre.
                       s - The specified value of Conv_sig. (given */
double v,         /* The value of the convolution function (<= 1) (given) */
int *status       /* global status (given and returned) */
);

/*+ dream_getbool - prompt for a boolean value */

void dream_getbool
(
char *prompt,   /* prompt string (given) */
int vdefault,   /* default value (given) */
int *value,     /* value obtained (returned) */
int *status     /* global status (given and returned) */
);

/*+ dream_getdouble - prompt for a double value */

void dream_getdouble
(
char *prompt,    /* prompt string (given) */
double vdefault, /* default value (given) */
double vmin,     /* minimum acceptable value (given) */
double vmax,     /* maximum acceptable value (given) */
double *value,   /* value obtained (returned) */
int *status      /* global status (given and returned) */
);

/*+ dream_getint - prompt for an int value */

void dream_getint
(
char *prompt,   /* prompt string (given) */
int vdefault,   /* default value (given) */
int vmin,       /* minimum acceptable value (given) */
int vmax,       /* maximum acceptable value (given) */
int *value,     /* value obtained (returned) */
int *status     /* global status (given and returned) */
);

/*+ dream_getstring - prompt for string input */

void dream_getstring 
( 
char *prompt,       /* prompt string (given) */
char *cdef,         /* default string (given) */
char *ans,          /* response (returned) */
int *status         /* global status (given and returned) */
);

/*+ dream_grid_index - Calculate the grid indices for all bolometers */

void dream_grid_index
(
int ngrid,           /* Number of grid positions within the jiggle area
                        (given) */
int nbolx,           /* Number of bolometers in X (given) */
int nboly,           /* Number of bolometers in Y (given) */
int jigpts[][2],     /* Table with relative grid coordinates determined
                        within the Jiggle area (given) */
int *nunkno,         /* number of unknowns for solution (returned) */
int gridindex[],     /* bolometer index for each grid position (returned) */
int *status          /* global status (given and returned) */
);

/*+ dream_grid_pos - Calculate the grid positions in the image */

void dream_grid_pos 
(
int nxpsol,           /* Nr of columns (X) in the grid image (given) */
int nypsol,           /* Nr of rows (Y) in the grid image (given) */
int nextxpl,          /* Nr of extra columns at the left side (given) */
int nextypb,          /* Nr of extra rows at the bottom side (given) */
int nbolx,            /* Number of bolometers in X (given) */
int nboly,            /* Number of bolometers in Y (given) */
double bol_xy[][2],   /* The (distorted) bolometer coordinates in X and Y
                         direction in arcsec (given) */
double bol_distx,     /* average bolometer distance in X (given) */
double bol_disty,     /* average bolometer distance in Y (given) */
double grid_xy[][2],  /* The (distorted) grid coordinates in X and Y in
                         arcsec (returned) */
int *status           /* global status (given and returned) */
);

/*+ dream_jigpts - Calculate the grid points in the Jiggle */

void dream_jigpts 
(
int npath,           /* Number of positions in jigpath (given) */
double jigpath[][2], /* Buffer containing the X and Y coordinates of each
                        point in the path of the SMU during the Jiggle,
                        in units of the HPBW (given) */
int conv_shape,      /* Code for the shape of the convolution function.
                        0 - Gaussian (given) */

double conv_sig,     /* The meaning depends on the code for the
                        convolution function.
                        If Gaussian : convolution(r) = exp[-r^2/(2.s^2)]
                        r - Distance in units of HPBW (=6.18 arcsec for
                            the long wave array)
                        s - The specified value of Conv_sig (given) */
int *ngrid,          /* Nr of Jiggle positions in JigPts.
                        This is also equal to the number of coefficients
                        per line in the equation matrix (returned) */

int jigpts[][2],     /* The relative Jiggle positions for which an
                        intensity must be calculated in units of the
                        average bolometer distance (returned) */
int *status          /* global status (given and returned) */
);

/*+ dream_smupath - Calculate the path positions of the SMU */

void dream_smupath 
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

/*+ dream_smupos - Calculate the SMU position at an instant */

void dream_smupos
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

/*+ dream_smuwave - Calculate the SMU wave form */

void dream_smuwave 
(
int movecode,     /* The code for the SMU waveform that determines the
                     SMU motion (given) */
int npts,         /* The number of points in buffer A (given) */
double a[], /* Buffer containing the wave form (returned) */
int *status       /* global status (given and returned) */
);

/*+ dream_timenow - get current time and date as strings */

void dream_timenow
( 
int dlength,       /* length of date string (given) */
int tlength,       /* length of time string (given) */
int ilength,       /* length of iso date string (given) */
char cur_day[],    /* String with the date. E.g. "17-May-2001" 
                      (returned) */
char cur_time[],   /* String with the time. E.g. "21:59:26" (returned) */
char iso_time[],   /* ISO date in YYYY-MM-DDThh:mm:ss.s format (returned but can be null) */
char ymd[],        /* Contraction of ISO date in YYYYMMDD format (returned) */
int *status        /* global status (given and returned) */
);

/*+ dream_trace - provide a flag for debugging level */

int dream_trace
( 
int value       /* trace level (given) */
);

/*+ dream_traceinit - initialise dream_trace facility */

void dream_traceinit
(
void
);

/*+ dream_traceout - output trace information */

void dream_traceout 
( 
char *fmt,        /* printf-style format string (given) */
...               /* variable argument list (given) */
);

 
 
#endif
