#ifndef HEADGEN____sc2store_pro_h
#define HEADGEN____sc2store_pro_h 
 
 
/*+ sc2store_compress - compress frame of integers to unsigned short */

void sc2store_compress 
( 
size_t nval,            /* number of values in frame (given) */
const int stackz[],     /* stackzero frame to be subtracted (given) */
int digits[],           /* integer values (given and returned) */
int *bzero,             /* zero offset for compressed values (returned) */
unsigned short data[],  /* compressed values (returned) */
size_t *npix,           /* number of incompressible values (returned) */
int pixnum[],           /* indices of incompressible values (returned) */
int pixval[],           /* incompressible values (returned) */
int *status             /* global status (given and returned) */
);

/*+ sc2store_credream - create DREAM extension in output file */

void sc2store_credream
(
size_t nvert,            /* Number of vertices in DREAM pattern (given)  */
int **jigvert,           /* Pointer to stored jiggle vertices (returned) */
size_t npath,            /* Number of points along SMU path in DREAM pattern 
			    (given) */
double **jigpath,        /* Pointer to stored jiggle path (returned) */
int *status              /* Global status (given and returned) */
);

/*+ sc2store_creimages - create structure to store images */

void sc2store_creimages
(
int *status              /* global status (given and returned) */
);

/*+ sc2store_cremap - create HDS container file and map data arrays */

void sc2store_cremap
(
const char *filename,    /* name of HDS container file (given) */
size_t colsize,          /* number of pixels in a column (given) */
size_t rowsize,          /* number of pixels in a row (given) */
size_t nframes,          /* number of frames (given) */
size_t nflat,            /* number of flat coeffs per bol (given) */
const char *flatname,    /* name of flatfield algorithm (given) */
int **bzero,             /* pointer to subtracted offset values (returned) */
unsigned short **data,   /* pointer to data array (returned) */
int **dksquid,           /* pointer to dark SQUID values (returned) */
int **stackz,            /* pointer to subtracted frame (returned) */
double **flatcal,        /* pointer to flat calibration (returned) */
double **flatpar,        /* pointer to flat parameters (returned) */
int *status              /* global status (given and returned) */
);

/*+ sc2store_decompress - decompress frame of unsigned short to integers */

void sc2store_decompress 
( 
size_t nval,                  /* number of values in frame (given) */
const int stackz[],           /* stackzero frame to be added (given) */
int bzero,                    /* zero offset for compressed values (given) */
const unsigned short data[],  /* compressed values (given) */
size_t npix,                  /* number of incompressible values (given) */
const int pixnum[],           /* indices of incompressible values (given) */
const int pixval[],           /* incompressible values (given) */
int digits[],                 /* integer values (returned) */
int *status                   /* global status (given and returned) */
);

/*+ sc2store_errconv - convert error message from Starlink to DRAMA */

void sc2store_errconv
(
int *status
);

/*+ sc2store_free - unmap and close all references to output file */

void sc2store_free
(
int *status          /* global status (given and returned) */
);

/*+ sc2store_getincomp - get details of incompressible pixels */

void sc2store_getincomp
(
int frame,         /* frame index (given) */
size_t *npix,      /* number of incompressible pixels (returned) */
int pixnum[],      /* indices of incompressible pixels (returned) */
int pixval[],      /* values of incompressible pixels (returned) */
int *status        /* global status (given and returned) */
);

/*+ sc2store_headget - get values from the header arrays */

void sc2store_headget
(
int frame,                    /* frame index (given) */
JCMTState *head,              /* header data for the frame (returned) */
int *status                   /* global status (given and returned) */
);

/*+ sc2store_headcremap - create and map the header arrays */

void sc2store_headcremap
(
const HDSLoc *headloc,           /* HDS locator (given) */
size_t nframes,                  /* number of frames to be created (given) */
inst_t instrument,               /* instrument code (given) */
int *status                      /* global status (given and returned) */
);

/*+ sc2store_headput - put values into the header arrays */

void sc2store_headput
(
int frame,                    /* frame index (given) */
JCMTState head,               /* header data for the frame (given) */
int *status                   /* global status (given and returned) */
);

/*+ sc2store_headrmap - map the header arrays for read access */

void sc2store_headrmap
(
const HDSLoc *headloc,        /* HDS locator (given) */
size_t nframes,               /* number of frames expected (given) */
inst_t instrument,            /* instrument code (given) */
int *status                   /* global status (given and returned) */
);

/*+ sc2store_headunmap - unmap the header arrays */

void sc2store_headunmap
(
int *status                   /* global status (given and returned) */
);

/*+ sc2store_ndfreadscan - read a single scan from an NDF file */

void sc2store_ndfreadscan
(
const char *filename,  /* name of input map file (given) */
const char *access,    /* "READ" or "UPDATE" access to data file (given) */
size_t flatlen,        /* length of string for flatname (given) */
size_t *nframes,       /* number of frames in scan (returned) */
double **xz,           /* X centre for each frame (returned) */
double **yz,           /* Y centre for each frame (returned) */
double **inptr,        /* measurement values (returned) */
size_t *nflat,         /* number of flatfield coeffs per pixel (returned) */
char *flatname,        /* name of flatfield algorithm (returned) */
double **flatcal,      /* flatfield calibration (returned) */
double **flatpar,      /* flatfield parameters (returned) */
int **jigvert,         /* pointer to DREAM jiggle vertices (returned) */
size_t *nvert,         /* Number of vertices in jiggle pattern (returned) */
double **jigpath,      /* pointer to path of SMU over jiggle pattern (returned) */
size_t *npath,         /* Number of points in SMU path (returned) */
int *status            /* global status (given and returned) */
);

/*+ sc2store_putimage - store constructed image */

void sc2store_putimage
(
int frame,               /* frame index (given) */
const AstFrameSet *fset, /* World coordinate transformations (given) */
int ndim,                /* dimensionality of image (given) */
const int dims[],        /* dimensions of image (given) */
int seqstart,            /* first sequence number used in image (given) */
int seqend,              /* last sequence number used in image (given) */
size_t nbolx,            /* number of bolometers in X (given) */
size_t nboly,            /* number of bolometers in Y (given) */
const double *image,     /* constructed image (given) */
const double *zero,      /* bolometer zero values (given) */
const char *fitshd,      /* string of concatenated FITS header records to
                            write (given) */
size_t nrec,             /* Number of FITS records */
int *status              /* global status (given and returned) */
);

/*+ sc2store_putincomp - store details of incompressible pixels */

void sc2store_putincomp
(
int frame,            /* frame index (given) */
size_t npix,          /* number of incompressible pixels (given) */
const int pixnum[],   /* indices of incompressible pixels (given) */
const int pixval[],   /* values of incompressible pixels (given) */
int *status           /* global status (given and returned) */
);

/*+ sc2store_putscanfit - store scan fit coefficients */

void sc2store_putscanfit
(
size_t nbolx,         /* number of bolometers in X (given) */
size_t nboly,         /* number of bolometers in Y (given) */
size_t ncoeff,        /* number of coefficients (given) */
const double *coptr,  /* coefficients (given) */
int *status           /* global status (given and returned) */
);

/*+ sc2store_rdfitshead - read the FITS headers */

void sc2store_rdfitshead
(
size_t maxfits,          /* maximum number of header items (given) */
size_t *nrec,            /* number of header records (returned) */
char *headers,           /* buffer to hold FITS headers (returned) */
int *status              /* global status (given and returned) */
);

/*+ sc2store_rdflatcal - read SCUBA-2 flatfield calibration */

void sc2store_rdflatcal
(
const char *filename,    /* name of HDS container file (given) */
size_t flatlen,             /* length of space for flatfield name (given) */
size_t *colsize,         /* number of pixels in column (returned) */
size_t *rowsize,         /* number of pixels in row (returned) */
size_t *nflat,           /* number of flat coeffs per bol (returned) */
char *flatname,          /* name of flatfield algorithm (returned) */
double **flatcal,        /* pointer to flatfield calibration (returned) */
double **flatpar,        /* pointer to flatfield parameters (returned) */
int *status              /* global status (given and returned) */
);

/*+ sc2store_rdmap - open an existing HDS container file and map data arrays */

void sc2store_rdmap
(
const char *filename,    /* name of HDS container file (given) */
const char *access,      /* "READ" or "UPDATE" access (given) */
size_t flatlen,          /* length of space for flatfield name (given) */
size_t *colsize,         /* number of pixels in column (returned) */
size_t *rowsize,         /* number of pixels in row (returned) */
size_t *nframes,         /* number of frames (returned) */
size_t *nflat,           /* number of flat coeffs per bol (returned) */
char *flatname,          /* name of flatfield algorithm (returned) */
int **bzero,             /* pointer to subtracted offset values (returned) */
unsigned short **data,   /* pointer to data array (returned) */
int **dksquid,           /* pointer to dark SQUID values (returned) */
int **stackz,            /* pointer to subtracted frame (returned) */
double **flatcal,        /* pointer to flatfield calibration (returned) */
double **flatpar,        /* pointer to flatfield parameters (returned) */
int **jigvert,           /* pointer to DREAM jiggle vertices (returned) */
size_t *nvert,           /* Number of vertices in jiggle pattern (returned) */
double **jigpath,        /* pointer to path of SMU over jiggle pattern (returned) */
size_t *npath,           /* Number of points in SMU path (returned) */
int *status              /* global status (given and returned) */
);

/*+ sc2store_rdtstream - read SCUBA-2 time stream data from an NDF */

void sc2store_rdtstream
(
const char *filename,    /* name of HDS container file (given) */
const char *access,      /* "READ" or "UPDATE" access (given) */
size_t flatlen,          /* length of space for flatfield name (given) */
size_t maxfits,          /* max number of FITS headers (given) */
size_t *nrec,            /* actual number of FITS records (returned) */
char *fitshead,          /* up to maxfits FITS header records (returned) */
size_t *colsize,         /* number of pixels in column (returned) */
size_t *rowsize,         /* number of pixels in row (returned) */
size_t *nframes,         /* number of frames (returned) */
size_t *nflat,           /* number of flat coeffs per bol (returned) */
char *flatname,          /* name of flatfield algorithm (returned) */
JCMTState *frhead[],     /* header data for each frame (returned) */
int **outdata,           /* pointer to data array (returned) */
int **dksquid,           /* pointer to dark SQUID values (returned) */
double **flatcal,        /* pointer to flatfield calibration (returned) */
double **flatpar,        /* pointer to flatfield parameters (returned) */
int **jigvert,           /* pointer to DREAM jiggle vertices (returned) */
size_t *nvert,           /* Number of vertices in jiggle pattern (returned) */
double **jigpath,        /* pointer to path of SMU over jiggle pattern (returned) */
size_t *npath,           /* Number of points in SMU path (returned) */
int *status              /* global status (given and returned) */
);

/*+ sc2store_wrconfigxml - Store the CONFIGURE XML */

void sc2store_wrconfigxml
(
const char *xmlfile,  /* name of CONFIGURE XML file (given) */
int *status           /* global status (given and returned) */
);

/*+ sc2store_wrfitshead - write the FITS headers */

void sc2store_wrfitshead
(
int id_ndf,           /* identifier of ndf (given) */
size_t nrec,          /* number of header records (given) */
const char *headers,  /* string of contiguous 80-byte FITS headers (given) */
int *status           /* global status (given and returned) */
);

/*+ sc2store_wrmcehead - Store the MCE headers for each frame */

void sc2store_wrmcehead
(
size_t numsamples,          /* number of samples (given) */
size_t mceheadsz,           /* number of values per MCE header (given) */
const int *mcehead,         /* MCE header for each sample (given) */
int *status                 /* global status (given and returned) */
);

/*+ sc2store_wrtstream - store SCUBA-2 time stream data as NDF */

void sc2store_wrtstream
(
const char file_name[],     /* output file name (given) */
int subnum,                 /* Sub-array number (given) */
size_t nrec,                /* number of FITS header records (given) */
const char *fitsrec,        /* contiguous 80-byte FITS records (given) */
size_t colsize,             /* number of bolometers in column (given) */
size_t rowsize,             /* number of bolometers in row (given) */
size_t numsamples,          /* number of samples (given) */
size_t nflat,               /* number of flat coeffs per bol (given) */
const char *flatname,       /* name of flatfield algorithm (given) */
const JCMTState head[],     /* header data for each frame (given) */
const int *dbuf,            /* time stream data (given) */
const int *darksquid,       /* dark SQUID time stream data (given) */
const double *fcal,         /* flat-field calibration (given) */
const double *fpar,         /* flat-field parameters (given) */
const char *obsmode,        /* Observing mode (given) */
const int *mcehead,         /* MCE header for each sample (given) */
size_t mceheadsz,           /* number of values per MCE header (given) */
int jig_vert[][2],          /* Array of jiggle vertices (given) */
size_t nvert,               /* Number of jiggle vertices (given) */
double jig_path[][2],       /* Path of SMU during jiggle cycle (given) */
size_t npath,               /* Number of positions in jiggle path (given) */
const char *xmlfile,        /* name of CONFIGURE XML file (given) */ 
int *status                 /* global status (given and returned) */
);

 
 
#endif
