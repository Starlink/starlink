#ifndef HEADGEN___src_dxml_dxml_h
#define HEADGEN___src_dxml_dxml_h 
 
 
/*+ dxml_cvtdouble - convert a parameter value to double */

void dxml_cvtdouble
(
const char *name,          /* name of parameter (given) */
const char *value,         /* value string of parameter (given) */
double *x,                 /* converted number (returned) */
int *status                /* global status (given and returned) */
);

/*+ dxml_cvtsexdouble - convert a sexagesimal parameter value to double */

void dxml_cvtsexdouble
(
const char *name,          /* name of parameter (given) */
const char *value,         /* value string of parameter (given) */
double *x,                 /* converted number (returned) */
int *status                /* global status (given and returned) */
);

/*+ dxml_cvterr - report value conversion error */

void dxml_cvterr
(
const char *name,          /* name of parameter (given) */
const char *value,         /* value string of parameter (given) */
int *status                /* global status (given and returned) */
);

/*+ dxml_cvtint - convert a parameter value to int */

void dxml_cvtint
(
const char *name,          /* name of parameter (given) */
const char *value,         /* value string of parameter (given) */
int *x,                    /* converted number (returned) */
int *status                /* global status (given and returned) */
);

/*+ dxml_endsimXML - callback for XML parser */

void dxml_endsimXML
(
void *userData,                 /* unused (given) */
const char *name                /* name of item (given) */
);

/*+ dxml_endXML - callback for XML parser */

void dxml_endXML
(
void *userData,                 /* unused (given) */
const char *name                /* name of item (given) */
);

/*+ dxml_initpars - initialise parameters */

void dxml_initpars
( 
int *status                /* global status (given and returned) */
);

/*+ dxml_makeupper - convert a string to uppercase */

char * dxml_makeupper 
(
char *lower,              /* string to be converted */
int *status               /* global status (given and returned) */
);

/*+ dxml_readsimXML - read simulation details from a file */

void dxml_readsimXML
( 
char *filename,            /* name of file (given) */
int *status                /* global status (given and returned) */
);

/*+ dxml_readXML - read details from a file */

void dxml_readXML
( 
char *filename,            /* name of file (given) */
int *status                /* global status (given and returned) */
);

/*+ dxml_report - report values from XML parse */

void dxml_report
(
char *rpt_name,           /* name of output file (given) */
struct dxml_struct inx,   /* structure for setup values (given) */
int *status                /* global status (given and returned) */
);

/*+ dxml_returnsimXML - return values from XML parse */

void dxml_returnsimXML
(
struct dxml_sim_struct *inx,   /* structure for returning values (returned) */
int *status                    /* global status (given and retuned) */
);

/*+ dxml_returnXML - return values from XML parse */

void dxml_returnXML
(
struct dxml_struct *inx,   /* structure for returning values (returned) */
int *status                /* global status (given and retuned) */
);

/*+ dxml_sextod - convert a sexagesimal string into a double */

double dxml_sextod
(
const char *string,        /* string (given) */
int *status                /* global status (given and returned) */
);

/*+ dxml_startsimXML - callback for XML parser when reading simulator file */

void dxml_startsimXML
(
void *userData,                /* unused (given and returned)*/
const char *name,              /* name of item (given) */
const char **atts              /* array of name-value pairs (given) */
);

/*+ dxml_startXML - callback for XML parser when reading file */

void dxml_startXML
(
void *userData,                /* unused (given and returned)*/
const char *name,              /* name of item (given) */
const char **atts              /* array of name-value pairs (given) */
);

/*+ dxml_strtod - convert a string into a double */

double dxml_strtod
(
const char *string,        /* string (given) */
int *status                /* global status (given and returned) */
);

/*+ dxml_strtol - convert a string into an integer */

int dxml_strtol
(
const char *string,        /* string (given) */
int *status                /* global status (given and returned) */
);

 
 
#endif
