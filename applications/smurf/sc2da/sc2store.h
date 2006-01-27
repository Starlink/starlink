/*+ sc2store_compress - compress frame of integers to unsigned short */

void sc2store_compress 
( 
int nval,               /* number of values in frame (given) */
int stackz[],           /* stackzero frame to be subtracted (given) */
int digits[],           /* integer values (given and returned) */
int *bzero,             /* zero offset for compressed values (returned) */
unsigned short data[],  /* compressed values (returned) */
int *npix,              /* number of incompressible values (returned) */
int pixnum[],           /* indices of incompressible values (returned) */
int pixval[],           /* incompressible values (returned) */
int *status             /* global status (given and returned) */
);

/*+ sc2store_creimages - create structure to store images */

void sc2store_creimages
(
int *status              /* global status (given and returned) */
);

/*+ sc2store_cremap - create HDS container file and map data arrays */

void sc2store_cremap
(
char *filename,          /* name of HDS container file (given) */
int colsize,             /* number of pixels in a column (given) */
int rowsize,             /* number of pixels in a row (given) */
int nframes,             /* number of frames (given) */
int nflat,               /* number of flat coeffs per bol (given) */
char *flatname,          /* name of flatfield algorithm (given) */
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
int nval,               /* number of values in frame (given) */
int stackz[],           /* stackzero frame to be added (given) */
int bzero,              /* zero offset for compressed values (given) */
unsigned short data[],  /* compressed values (given) */
int npix,               /* number of incompressible values (given) */
int pixnum[],           /* indices of incompressible values (given) */
int pixval[],           /* incompressible values (given) */
int digits[],           /* integer values (returned) */
int *status             /* global status (given and returned) */
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
int *npix,         /* number of incompressible pixels (returned) */
int pixnum[],      /* indices of incompressible pixels (returned) */
int pixval[],      /* values of incompressible pixels (returned) */
int *status        /* global status (given and returned) */
);

/*+ sc2store_headget - get values from the header arrays */

void sc2store_headget
(
int frame,                    /* frame index (given) */
struct sc2head *head,         /* header data for the frame (returned) */
int *status                   /* global status (given and returned) */
);

/*+ sc2store_headcremap - create and map the header arrays */

void sc2store_headcremap
(
HDSLoc *headloc,     /* HDS locator (given) */
int nframes,                  /* number of frames to be created (given) */
int *status                   /* global status (given and returned) */
);

/*+ sc2store_headput - put values into the header arrays */

void sc2store_headput
(
int frame,                    /* frame index (given) */
struct sc2head head,          /* header data for the frame (given) */
int *status                   /* global status (given and returned) */
);

/*+ sc2store_headrmap - map the header arrays for read access */

void sc2store_headrmap
(
HDSLoc *headloc,     /* HDS locator (given) */
int nframes,                  /* number of frames expected (given) */
int *status                   /* global status (given and returned) */
);

/*+ sc2store_headunmap - unmap the header arrays */

void sc2store_headunmap
(
int *status                   /* global status (given and returned) */
);

/*+ sc2store_putimage - store constructed image */

void sc2store_putimage
(
int frame,         /* frame index (given) */
AstFrameSet *fset, /* World coordinate transformations (given) */
int ndim,          /* dimensionality of image (given) */
int dims[],        /* dimensions of image (given) */
int seqstart,      /* first sequence number used in image (given) */
int seqend,        /* last sequence number used in image (given) */
int nbolx,         /* number of bolometers in X (given) */
int nboly,         /* number of bolometers in Y (given) */
double *image,     /* constructed image (given) */
double *zero,      /* bolometer zero values (given) */
char fitshd[41][81], /* string array of FITS header keywords to write (given) */
int nfits,         /* Number of FITS headers */
int *status        /* global status (given and returned) */
);

/*+ sc2store_putincomp - store details of incompressible pixels */

void sc2store_putincomp
(
int frame,         /* frame index (given) */
int npix,          /* number of incompressible pixels (given) */
int pixnum[],      /* indices of incompressible pixels (given) */
int pixval[],      /* values of incompressible pixels (given) */
int *status        /* global status (given and returned) */
);

/*+ sc2store_putscanfit - store scan fit coefficients */

void sc2store_putscanfit
(
int nbolx,         /* number of bolometers in X (given) */
int nboly,         /* number of bolometers in Y (given) */
int ncoeff,        /* number of coefficients (given) */
double *coptr,     /* coefficients (given) */
int *status        /* global status (given and returned) */
);

/*+ sc2store_rdfitshead - read the FITS headers */

void sc2store_rdfitshead
(
int maxlen,           /* maximum length of FITS header (given) */
int maxfits,          /* maximum number of header items (given) */
int *nfits,           /* number of header items (returned) */
char headers[][81],   /* array of FITS headers (returned) */
int *status           /* global status (given and returned) */
);

/*+ sc2store_rdflatcal - read SCUBA-2 flatfield calibration */

void sc2store_rdflatcal
(
char *filename,          /* name of HDS container file (given) */
int flatlen,             /* length of space for flatfield name (given) */
int *colsize,            /* number of pixels in column (returned) */
int *rowsize,            /* number of pixels in row (returned) */
int *nflat,              /* number of flat coeffs per bol (returned) */
char *flatname,          /* name of flatfield algorithm (returned) */
double **flatcal,        /* pointer to flatfield calibration (returned) */
double **flatpar,        /* pointer to flatfield parameters (returned) */
int *status              /* global status (given and returned) */
);

/*+ sc2store_rdmap - open an existing HDS container file and map data arrays */

void sc2store_rdmap
(
char *filename,          /* name of HDS container file (given) */
char *access,            /* "READ" or "UPDATE" access (given) */
int flatlen,             /* length of space for flatfield name (given) */
int *colsize,            /* number of pixels in column (returned) */
int *rowsize,            /* number of pixels in row (returned) */
int *nframes,            /* number of frames (returned) */
int *nflat,              /* number of flat coeffs per bol (returned) */
char *flatname,          /* name of flatfield algorithm (returned) */
int **bzero,             /* pointer to subtracted offset values (returned) */
unsigned short **data,   /* pointer to data array (returned) */
int **dksquid,           /* pointer to dark SQUID values (returned) */
int **stackz,            /* pointer to subtracted frame (returned) */
double **flatcal,        /* pointer to flatfield calibration (returned) */
double **flatpar,        /* pointer to flatfield parameters (returned) */
int *status              /* global status (given and returned) */
);

/*+ sc2store_rdtstream - read SCUBA-2 time stream data from an NDF */

void sc2store_rdtstream
(
char *filename,          /* name of HDS container file (given) */
char *access,            /* "READ" or "UPDATE" access (given) */
int flatlen,             /* length of space for flatfield name (given) */
int maxlen,              /* max length of FITS header (given) */
int maxfits,             /* max number of FITS headers (given) */
int *nfits,              /* acual number of FITS headers (returned) */
char fitshead[][81],     /* FITS header records (returned) */
int *colsize,            /* number of pixels in column (returned) */
int *rowsize,            /* number of pixels in row (returned) */
int *nframes,            /* number of frames (returned) */
int *nflat,              /* number of flat coeffs per bol (returned) */
char *flatname,          /* name of flatfield algorithm (returned) */
struct sc2head **frhead, /* header data for each frame (returned) */
int **outdata,           /* pointer to data array (returned) */
int **dksquid,           /* pointer to dark SQUID values (returned) */
double **flatcal,        /* pointer to flatfield calibration (returned) */
double **flatpar,        /* pointer to flatfield parameters (returned) */
int *status              /* global status (given and returned) */
);

/*+ sc2store_wrfitshead - write the FITS headers */

void sc2store_wrfitshead
(
int nfits,            /* number of header items (given) */
char headers[][81],   /* array of FITS headers (given) */
int *status           /* global status (given and returned) */
);

/*+ sc2store_wrtstream - store SCUBA-2 time stream data as NDF */

void sc2store_wrtstream
(
char file_name[],  /* output file name (given) */
int nrec,          /* number of FITS header records (given) */
char fitsrec[][81],/* FITS records (given) */
int colsize,       /* number of bolometers in column (given) */
int rowsize,       /* number of bolometers in row (given) */
int numsamples,    /* number of samples (given) */
int nflat,         /* number of flat coeffs per bol (given) */
char *flatname,    /* name of flatfield algorithm (given) */
struct sc2head head[], /* header data for each frame (given) */
int *dbuf,         /* time stream data (given) */
int *darksquid,    /* dark SQUID time stream data (given) */
double *fcal,      /* flat-field calibration (given) */
double *fpar,      /* flat-field parameters (given) */
int *status        /* global status (given and returned) */
);

