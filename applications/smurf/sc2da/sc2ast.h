/*+ sc2ast_createwcs - create WCS description */

void sc2ast_createwcs
(
int subnum,             /* subarray number, 0-7 (given) */
double az,              /* Azimuth in radians (given) */
double el,              /* Elevation in radians (given) */
double tai,             /* TAI (supplied as an MJD) */
int extra_frames,       /* Add intermediate Frames to returned FrameSet? */
AstFrameSet **fset,     /* constructed frameset (returned) */
int *status             /* global status (given and returned) */
);

/*+ sc2ast_getdomain - select a domain within a frameset */

void sc2ast_getdomain
(
char *name,               /* AST domain name (given) */
AstFrameSet *fset,        /* modified frameset (given and returned) */
int *status               /* global status (given and returned) */
);

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
);

/*+ sc2ast_moveframe - move the base frame within a frameset */

void sc2ast_moveframe
(
double x,                 /* X coordinate offset in pixels (given) */
double y,                 /* Y coordinate offset in pixels (given) */
AstFrameSet *fset,        /* modified frameset (given and returned) */
int *status               /* global status (given and returned) */
);

/*+ sc2ast_name2num - convert subarray name to id number */

void sc2ast_name2num
(
char *name,             /* subarray name s8a-d, s4a-d (given) */
int *subnum,            /* subarray number, 0-7 (returned) */
int *status             /* global status (given and returned) */
);

/*+ sc2ast_polframest - create a frameset for polarimetry */

void sc2ast_polframeset
(
AstFrameSet *frameset,  /* 2-D frameset (given) */
AstFrameSet **fset,     /* constructed 3-D frameset (returned) */
int *status             /* global status (given and returned) */
);


/*+ sc2ast_maketanmap - create a Mapping representing a tangent plane 
                        projection */

AstMapping *sc2ast_maketanmap
(
double lon,               /* Celestial longitude at ref point (rads) */
double lat,               /* Celestial latitude at ref point (rads) */
AstMapping *cache[ 2 ],   /* Cached Mappings (supply as NULL on 1st call) */
int *status               /* global status (given and returned) */
);


