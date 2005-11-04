/*+ sc2dat_annul - annul an HDS locator */

void sc2dat_annul
( 
char loc[DAT__SZLOC],   /* object locator, returned as DAT__NOLOC 
                           (given and returned) */
int *status             /* global status (given and returned) */
);

/*+ sc2dat_cell - obtain a locator to an array element */

void sc2dat_cell
( 
char loc1[DAT__SZLOC],    /* locator to an array (given) */
int ndim,                 /* number of dimensions (given) */
const int dim[],          /* subscript values (given) */
char loc2[DAT__SZLOC],    /* locator to array element (returned) */
int *status               /* global status (given and returned) */
);

/*+ sc2dat_find - find named component */

void sc2dat_find
( 
char loc1[DAT__SZLOC],    /* known structure locator (given) */
char name[DAT__SZNAM],    /* name of component to be found (given) */
char loc2[DAT__SZLOC],    /* new locator (returned) */
int *status               /* global status (given and returned) */
);

/*+ sc2dat_get0c - get a character string from an HDS scalar primitive */

void sc2dat_get0c
( 
char loc[DAT__SZLOC],  /* locator to character string (given) */
int value_length,      /* length of array provided (given) */
char *value,           /* character string (returned) */
int *status            /* global status (given and returned) */
);

/*+ sc2dat_get0d - get a double from an HDS scalar primitive */

void sc2dat_get0d
( 
char loc[DAT__SZLOC],    /* locator to scalar primitive (given) */
double *value,           /* object value (returned) */
int *status              /* global status (given and returned) */
);

/*+ sc2dat_get0i - get an integer from an HDS scalar primitive */

void sc2dat_get0i
( 
char loc[DAT__SZLOC],    /* locator to scalar primitive (given) */
int *value,              /* object value (returned) */
int *status              /* global status (given and returned) */ 
);

/*+ sc2dat_get0r - get a real from an HDS scalar primitive */

void sc2dat_get0r
( 
char loc[DAT__SZLOC],    /* locator to scalar primitive (given) */
float *value,            /* object value (returned) */
int *status              /* global status (given and returned) */  
);

/*+ sc2dat_map - map primitive */

void sc2dat_map
( 
const char loc[DAT__SZLOC],    /* locator to primitive (given) */
const char type[DAT__SZTYP],   /* data type: one of
                                  "_INTEGER"
                                  "_REAL"
                                  "_DOUBLE"
                                  "_LOGICAL"
                                  "_CHAR<*n>" (eg "_CHAR*16" )
                                  "_WORD"
                                  "_UWORD"
                                  "_BYTE"
                                  "_UBYTE" 
                                  (given) */
const char mode[DAT__SZMOD],   /* access mode "READ", "UPDATE" or "WRITE" 
                                  (given) */
int ndim,                      /* number of dimensions (given) */
const int dim[],               /* object dimensions (given) */
void **pntr,                   /* pointer to mapped value (returned) */
int *status                    /* global status (given and returned) */
);

/*+ sc2dat_new - create component */

void sc2dat_new
(
const char loc[ DAT__SZLOC ],  /* locator to existing structure (given) */
const char *name,              /* name of component to be created (given) */
const char *type,              /* type of component to be created (given) */
int ndim,                      /* number of dimensions (given) */
const int dim[],               /* component dimensions (given) */
int *status                    /* global status (given and returned) */
);

/*+ sc2dat_put0c - put a character string into an HDS scalar primitive */

void sc2dat_put0c
( 
char loc[DAT__SZLOC],   /* locator to primitive (given) */
char *value,            /* null terminated character string (given) */
int *status             /* global status (given and returned) */
);

/*+ sc2dat_put0d - put a double into an HDS scalar primitive */

void sc2dat_put0d
( 
char loc[DAT__SZLOC],   /* locator to primitive (given) */
double value,           /* value to be put (given) */
int *status             /* global status (given and returned) */ 
);

/*+ sc2dat_put0i - put an integer into an HDS scalar primitive */

void sc2dat_put0i
( 
char loc[DAT__SZLOC],   /* locator to primitive (given) */
int value,              /* value to be put (given) */
int *status             /* global status (given and returned) */  
);

/*+ sc2dat_put0r - put a real into an HDS scalar primitive */

void sc2dat_put0r
( 
char loc[DAT__SZLOC],   /* locator to primitive (given) */
float value,            /* value to be put (given) */
int *status             /* global status (given and returned) */   
);

/*+ sc2dat_shape - enquire object shape */

void sc2dat_shape
(
const char loc[ DAT__SZLOC ],  /* locator to existing structure (given) */
int ndimx,                     /* size of dim (given) */
int dim[],                     /* object dimensions (returned) */
int *ndim,                     /* number of dimensions (returned) */
int *status                    /* global status (given and returned) */
);

/*+ sc2dat_there - enquire if a component of a structure exists */

void sc2dat_there
( 
char loc1[DAT__SZLOC],    /* known structure locator (given) */
char name[DAT__SZNAM],    /* name of component to be found (given) */
int *reply,               /* TRUE if exists, otherwise FALSE (returned) */
int *status               /* global status (given and returned) */
);

/*+ sc2dat_unmap - unmap an object previously mapped */

void sc2dat_unmap
( 
char loc[DAT__SZLOC],   /* primitive locator (given) */
int *status             /* global status (given and returned) */
);

