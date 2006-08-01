#ifndef HEADGEN___src_fhead_fhead_h
#define HEADGEN___src_fhead_fhead_h 
 
 
/*+ fhead_getfits - get the list of FITS records */

void fhead_getfits
( 
int *nrec,                /* number of header records (returned) */
char records[][81],       /* header records (returned) */
int  *status              /* global status (given and returned) */
);

/*+ fhead_init - initialise the list of FITS records */

void fhead_init
( 
int  *status        /* global status (given and returned */
);

/*+ fhead_make - make a FITS header card */

void fhead_make
( 
int  datatype,      /* I - datatype of the value    */
char *keyname,      /* I - name of keyword to write */
void *value,        /* I - keyword value            */
char *comm,         /* I - keyword comment          */
char *card,         /* O - constructed card         */
int  *status        /* IO - error status            */
);

/*+ fhead_pkyc - put complex float */

void fhead_pkyc
(
float *value,        /* I - keyword value (real, imaginary)     */
int   decim,         /* I - number of decimal places to display */
char *valstring,     /* O - value as string                     */
int   *status        /* IO - error status                       */
);

/*+ fhead_pkym - put complex double */

void fhead_pkym
(
double *value,       /* I - keyword value (real, imaginary)     */
int   decim,         /* I - number of decimal places to display */
char *valstring,     /* O - value as string                     */
int   *status        /* IO - error status                       */
);

/*+ fhead_putfits - add a fits record to a list of them */

void fhead_putfits
( 
int  datatype,      /* I - datatype of the value    */
char *keyname,      /* I - name of keyword to write */
void *value,        /* I - keyword value            */
char *comm,         /* I - keyword comment          */
int  *status        /* IO - error status            */
);

/*+ fhead_putfitscom - add a fits comment record to a list of them */

void fhead_putfitscom
( 
char *comm,         /* I - keyword comment          */
int  *status        /* IO - error status            */
);

 
 
#endif
