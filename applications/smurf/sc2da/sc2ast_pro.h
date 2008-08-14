#ifndef HEADGEN____sc2ast_pro_h
#define HEADGEN____sc2ast_pro_h 
 
 
/*+ sc2ast_createwcs - create WCS description */

void sc2ast_createwcs
(
int subnum,             /* subarray number, 0-7 (given). If -1 is
                           supplied the cached AST objects will be freed. */
const JCMTState *state, /* Current telescope state (time, pointing etc.) */
const double instap[2], /* Offset of subarray in the focal plane */ 
const double telpos[3], /* Geodetic W Lon/Lat/Alt of telescope (deg/deg/ign.)*/
AstFrameSet **fset,     /* constructed frameset (returned) */
int *status             /* global status (given and returned) */
);

/*+ sc2ast_getdomain - select a domain within a frameset */

void sc2ast_getdomain
(
const char *name,         /* AST domain name (given) */
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
const char *ctype1,       /* coordinate mapping type (given) */
const char *ctype2,       /* coordinate mapping type (given) */
AstFitsChan *fitschan,    /* FitsChan to be filled (given and returned) */
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
const char *name,             /* subarray name s8a-d, s4a-d (given) */
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
double lon,              /* Celestial longitude at ref point (rads) */
double lat,              /* Celestial latitude at ref point (rads) */
AstMapping *cache[ 2 ],  /* Cached Mappings (supply as NULL on 1st call) */
double rot,              /* Rotation needed to align input Y axis with North */
int *status              /* global status (given and returned) */
);

/*+ sc2ast_convert_system - convert JCMT coordinate system to AST coordinate
                           system
 */
const char * sc2ast_convert_system 
(
 const char *label,   /* Input JCMT coordinate system (given) */
 int * status         /* Inherited status (given & returned) */
);

 
 
#endif
