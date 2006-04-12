#ifndef HEADGEN___src_sc2ast_sc2ast_h
#define HEADGEN___src_sc2ast_sc2ast_h 
 
 
/*+ sc2ast_createwcs - create WCS description */

void sc2ast_createwcs
(
int subnum,             /* subarray number, 0-7 (given). If -1 is
                           supplied the cached AST objects will be freed. */
double az,              /* Boresight azimuth in radians (given) */
double el,              /* Boresight elevation in radians (given) */
double az_jig_x,        /* SMU azimuth jiggle offset radians (given) */ 
double az_jig_y,        /* SMU elevation jiggle offset radians (given) */ 
double tai,             /* TAI (supplied as a Modified Julian Date) */
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

/*+ sc2ast_kludgemodjuldate - kludge for the modified julian date */

double sc2ast_kludgemodjuldate
( 
double ra,           /* Right Ascension in radians (given) */
int *status          /* global status (given and returned) */
);

/*+ sc2ast_telpos - get telescope position and orientation */

void sc2ast_telpos
( 
double ra,           /* Right Ascension in radians (given) */
double dec,          /* Declination in radians (given) */
double lst,          /* local sidereal time in radians (given) */
double *az,          /* Azimuth in radians (returned) */
double *el,          /* Elevation in radians (returned) */
double *p            /* Parallactic angle in radians (returned) */
);

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

 
 
#endif
