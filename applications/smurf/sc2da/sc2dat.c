/*  sc2dat - provide C-callable wrapper to Starlink DAT_ routines

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    29Jul2004: original (bdk)
*/

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "dat_par.h"
#include "f77.h"
#include "ndf.h"


#include "sc2dat.h"



F77_SUBROUTINE(dat_annul)( CHARACTER(loc),
                          INTEGER(status)
                          TRAIL(loc) );

/*+ sc2dat_annul - annul an HDS locator */

void sc2dat_annul
( 
char loc[DAT__SZLOC],   /* object locator, returned as DAT__NOLOC 
                           (given and returned) */
int *status             /* global status (given and returned) */
) 
{

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc, floc );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(dat_annul)(
                       CHARACTER_ARG(floc),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc) );

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}


F77_SUBROUTINE(dat_cell)( CHARACTER(loc1),
                          INTEGER(ndim),
                          INTEGER_ARRAY(dim),
                          CHARACTER(loc2),
                          INTEGER(status)
                          TRAIL(loc1)
                          TRAIL(loc2) );

/*+ sc2dat_cell - obtain a locator to an array element */

void sc2dat_cell
( 
char loc1[DAT__SZLOC],    /* locator to an array (given) */
int ndim,                 /* number of dimensions (given) */
const int dim[],          /* subscript values (given) */
char loc2[DAT__SZLOC],    /* locator to array element (returned) */
int *status               /* global status (given and returned) */
) 
{

DECLARE_CHARACTER(floc1,DAT__SZLOC);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fdim);
DECLARE_CHARACTER(floc2,DAT__SZLOC);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc1, floc1 );
   F77_EXPORT_INTEGER( ndim, fndim );
   F77_CREATE_INTEGER_ARRAY( fdim, ndim );
   F77_EXPORT_INTEGER_ARRAY( dim, fdim, ndim );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(dat_cell)(
                       CHARACTER_ARG(floc1),
                       INTEGER_ARG(&fndim),
                       INTEGER_ARRAY_ARG(fdim),
                       CHARACTER_ARG(floc2),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc1)
                       TRAIL_ARG(floc2) );

   F77_FREE_INTEGER( fdim );
   F77_IMPORT_LOCATOR( floc2, loc2 );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}


F77_SUBROUTINE(dat_find)( CHARACTER(loc1),
                          CHARACTER(name),
                          CHARACTER(loc2),
                          INTEGER(status)
                          TRAIL(loc1)
                          TRAIL(name)
                          TRAIL(loc2) );

/*+ sc2dat_find - find named component */

void sc2dat_find
( 
char loc1[DAT__SZLOC],    /* known structure locator (given) */
char name[DAT__SZNAM],    /* name of component to be found (given) */
char loc2[DAT__SZLOC],    /* new locator (returned) */
int *status               /* global status (given and returned) */
) 
{

DECLARE_CHARACTER(floc1,DAT__SZLOC);
DECLARE_CHARACTER(fname,DAT__SZNAM);
DECLARE_CHARACTER(floc2,DAT__SZLOC);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc1, floc1 );
   F77_EXPORT_CHARACTER( name, fname, DAT__SZNAM );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(dat_find)(
                       CHARACTER_ARG(floc1),
                       CHARACTER_ARG(fname),
                       CHARACTER_ARG(floc2),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc1)
                       TRAIL_ARG(fname)
                       TRAIL_ARG(floc2) );

   F77_IMPORT_LOCATOR( floc2, loc2 );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}



F77_SUBROUTINE(dat_get0c)( CHARACTER(loc),
                           CHARACTER(value),
                           INTEGER(status)
                           TRAIL(loc)
                           TRAIL(value) );


/*+ sc2dat_get0c - get a character string from an HDS scalar primitive */

void sc2dat_get0c
( 
char loc[DAT__SZLOC],  /* locator to character string (given) */
int value_length,      /* length of array provided (given) */
char *value,           /* character string (returned) */
int *status            /* global status (given and returned) */
) 



{

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc, floc );
   F77_CREATE_CHARACTER( fvalue, value_length-1 );
   F77_EXPORT_CHARACTER( value, fvalue, fvalue_length );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(dat_get0c)( CHARACTER_ARG(floc),
                        CHARACTER_ARG(fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc)
                        TRAIL_ARG(fvalue) );

   F77_IMPORT_CHARACTER( fvalue, fvalue_length, value );
   F77_FREE_CHARACTER( fvalue );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}


F77_SUBROUTINE(dat_get0d)( CHARACTER(loc),
                           DOUBLE(value),
                           INTEGER(status)
                           TRAIL(loc) );


/*+ sc2dat_get0d - get a double from an HDS scalar primitive */

void sc2dat_get0d
( 
char loc[DAT__SZLOC],    /* locator to scalar primitive (given) */
double *value,           /* object value (returned) */
int *status              /* global status (given and returned) */
) 

{

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_DOUBLE(fvalue);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc, floc );
   F77_EXPORT_DOUBLE( *value, fvalue );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(dat_get0d)( CHARACTER_ARG(floc),
                        DOUBLE_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc) );

   F77_IMPORT_DOUBLE( fvalue, *value );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}


F77_SUBROUTINE(dat_get0i)( CHARACTER(loc),
                           INTEGER(value),
                           INTEGER(status)
                           TRAIL(loc) );


/*+ sc2dat_get0i - get an integer from an HDS scalar primitive */

void sc2dat_get0i
( 
char loc[DAT__SZLOC],    /* locator to scalar primitive (given) */
int *value,              /* object value (returned) */
int *status              /* global status (given and returned) */ 
) 

{

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fvalue);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc, floc );
   F77_EXPORT_INTEGER( *value, fvalue );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(dat_get0i)( CHARACTER_ARG(floc),
                        INTEGER_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc) );

   F77_IMPORT_INTEGER( fvalue, *value );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}


F77_SUBROUTINE(dat_get0r)( CHARACTER(loc),
                           REAL(value),
                           INTEGER(status)
                           TRAIL(loc) );


/*+ sc2dat_get0r - get a real from an HDS scalar primitive */

void sc2dat_get0r
( 
char loc[DAT__SZLOC],    /* locator to scalar primitive (given) */
float *value,            /* object value (returned) */
int *status              /* global status (given and returned) */  
) 

{

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_REAL(fvalue);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc, floc );
   F77_EXPORT_REAL( *value, fvalue );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(dat_get0r)( CHARACTER_ARG(floc),
                        REAL_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc) );

   F77_IMPORT_REAL( fvalue, *value );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}




F77_SUBROUTINE(dat_map)( CHARACTER(loc),
                         CHARACTER(type),
                         CHARACTER(mode),
                         INTEGER(ndim),
                         INTEGER_ARRAY(dim),
                         POINTER(pntr),
                         INTEGER(status)
                         TRAIL(loc)
                         TRAIL(type)
                         TRAIL(mode) );


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
) 

{

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_CHARACTER(ftype,DAT__SZTYP);
DECLARE_CHARACTER(fmode,DAT__SZMOD);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fdim);
DECLARE_POINTER(fpntr);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc, floc );
   F77_EXPORT_CHARACTER( type, ftype, DAT__SZTYP );
   F77_EXPORT_CHARACTER( mode, fmode, DAT__SZMOD );
   F77_EXPORT_INTEGER( ndim, fndim );
   F77_EXPORT_INTEGER_ARRAY( dim, fdim, ndim );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(dat_map)( CHARACTER_ARG(floc),
                      CHARACTER_ARG(ftype),
                      CHARACTER_ARG(fmode),
                      INTEGER_ARG(&fndim),
                      INTEGER_ARRAY_ARG(fdim),
                      POINTER_ARG(&fpntr),
                      INTEGER_ARG(&fstatus)
                      TRAIL_ARG(floc)
                      TRAIL_ARG(ftype)
                      TRAIL_ARG(fmode) );

   F77_IMPORT_POINTER( fpntr, *pntr );
   F77_FREE_INTEGER( fdim );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}





F77_SUBROUTINE(dat_new)(  CHARACTER(loc),
                          CHARACTER(name),
                          CHARACTER(type),
                          INTEGER(ndim),
                          INTEGER_ARRAY(dim),
                          INTEGER(status)
                          TRAIL(loc) 
                          TRAIL(xname)
                          TRAIL(type) );

/*+ sc2dat_new - create component */

void sc2dat_new
(
const char loc[ DAT__SZLOC ],  /* locator to existing structure (given) */
const char *name,              /* name of component to be created (given) */
const char *type,              /* type of component to be created (given) */
int ndim,                      /* number of dimensions (given) */
const int dim[],               /* component dimensions (given) */
int *status                    /* global status (given and returned) */
) 

{

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_CHARACTER_DYN(fname);
DECLARE_CHARACTER_DYN(ftype);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fdim);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc, floc );
   F77_CREATE_CHARACTER( fname, strlen( name ) );
   F77_EXPORT_CHARACTER( name, fname, fname_length );
   F77_CREATE_CHARACTER( ftype, strlen( type ) );
   F77_EXPORT_CHARACTER( type, ftype, ftype_length );
   F77_EXPORT_INTEGER( ndim, fndim );
   F77_CREATE_INTEGER_ARRAY( fdim, ndim );
   F77_EXPORT_INTEGER_ARRAY( dim, fdim, ndim );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(dat_new)(  CHARACTER_ARG(floc),
                       CHARACTER_ARG(fname),
                       CHARACTER_ARG(ftype),
                       INTEGER_ARG(&fndim),
                       INTEGER_ARRAY_ARG(fdim),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc) 
                       TRAIL_ARG(fname)
                       TRAIL_ARG(ftype) );

   F77_FREE_CHARACTER( fname );
   F77_FREE_CHARACTER( ftype );
   F77_FREE_INTEGER( fdim );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}


F77_SUBROUTINE(dat_put0c)( CHARACTER(loc),
                           CHARACTER(value),
                           INTEGER(status)
                           TRAIL(loc)
                           TRAIL(value) );


/*+ sc2dat_put0c - put a character string into an HDS scalar primitive */

void sc2dat_put0c
( 
char loc[DAT__SZLOC],   /* locator to primitive (given) */
char *value,            /* null terminated character string (given) */
int *status             /* global status (given and returned) */
) 



{

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc, floc );
   F77_CREATE_CHARACTER( fvalue, strlen( value ) );
   F77_EXPORT_CHARACTER( value, fvalue, fvalue_length );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(dat_put0c)( CHARACTER_ARG(floc),
                        CHARACTER_ARG(fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc)
                        TRAIL_ARG(fvalue) );

   F77_FREE_CHARACTER( fvalue );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}


F77_SUBROUTINE(dat_put0d)( CHARACTER(loc),
                           DOUBLE(value),
                           INTEGER(status)
                           TRAIL(loc) );


/*+ sc2dat_put0d - put a double into an HDS scalar primitive */

void sc2dat_put0d
( 
char loc[DAT__SZLOC],   /* locator to primitive (given) */
double value,           /* value to be put (given) */
int *status             /* global status (given and returned) */ 
) 

{

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_DOUBLE(fvalue);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc, floc );
   F77_EXPORT_DOUBLE( value, fvalue );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(dat_put0d)( CHARACTER_ARG(floc),
                        DOUBLE_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc) );

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}


F77_SUBROUTINE(dat_put0i)( CHARACTER(loc),
                           INTEGER(value),
                           INTEGER(status)
                           TRAIL(loc) );


/*+ sc2dat_put0i - put an integer into an HDS scalar primitive */

void sc2dat_put0i
( 
char loc[DAT__SZLOC],   /* locator to primitive (given) */
int value,              /* value to be put (given) */
int *status             /* global status (given and returned) */  
) 

{

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fvalue);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc, floc );
   F77_EXPORT_INTEGER( value, fvalue );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(dat_put0i)( CHARACTER_ARG(floc),
                        INTEGER_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc) );

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}


F77_SUBROUTINE(dat_put0r)( CHARACTER(loc),
                           REAL(value),
                           INTEGER(status)
                           TRAIL(loc) );


/*+ sc2dat_put0r - put a real into an HDS scalar primitive */

void sc2dat_put0r
( 
char loc[DAT__SZLOC],   /* locator to primitive (given) */
float value,            /* value to be put (given) */
int *status             /* global status (given and returned) */   
) 

{

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_REAL(fvalue);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc, floc );
   F77_EXPORT_REAL( value, fvalue );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(dat_put0r)( CHARACTER_ARG(floc),
                        REAL_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc) );

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}



F77_SUBROUTINE(dat_shape)(  CHARACTER(loc),
                          INTEGER(ndimx),
                          INTEGER_ARRAY(dim),
                          INTEGER(ndim),
                          INTEGER(status)
                          TRAIL(loc) );

/*+ sc2dat_shape - enquire object shape */

void sc2dat_shape
(
const char loc[ DAT__SZLOC ],  /* locator to existing structure (given) */
int ndimx,                     /* size of dim (given) */
int dim[],                     /* object dimensions (returned) */
int *ndim,                     /* number of dimensions (returned) */
int *status                    /* global status (given and returned) */
) 

{

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fndimx);
DECLARE_INTEGER_ARRAY_DYN(fdim);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc, floc );
   F77_EXPORT_INTEGER( ndimx, fndimx );
   F77_CREATE_INTEGER_ARRAY( fdim, ndimx );
   F77_EXPORT_INTEGER_ARRAY( dim, fdim, ndimx );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(dat_shape)( CHARACTER_ARG(floc),
                       INTEGER_ARG(&fndimx),
                       INTEGER_ARRAY_ARG(fdim),
                       INTEGER_ARG(&fndim),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc) );

   F77_IMPORT_INTEGER( fndim, *ndim );
   F77_IMPORT_INTEGER_ARRAY( fdim, dim, fndim );
   F77_FREE_INTEGER( fdim );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}




F77_SUBROUTINE(dat_there)( CHARACTER(loc1),
                          CHARACTER(name),
                          LOGICAL(reply),
                          INTEGER(status)
                          TRAIL(loc1)
                          TRAIL(name) );

/*+ sc2dat_there - enquire if a component of a structure exists */

void sc2dat_there
( 
char loc1[DAT__SZLOC],    /* known structure locator (given) */
char name[DAT__SZNAM],    /* name of component to be found (given) */
int *reply,               /* TRUE if exists, otherwise FALSE (returned) */
int *status               /* global status (given and returned) */
) 
{

DECLARE_CHARACTER(floc1,DAT__SZLOC);
DECLARE_CHARACTER(fname,DAT__SZNAM);
DECLARE_LOGICAL(freply);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc1, floc1 );
   F77_EXPORT_CHARACTER( name, fname, DAT__SZNAM );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(dat_there)(
                       CHARACTER_ARG(floc1),
                       CHARACTER_ARG(fname),
                       LOGICAL_ARG(&freply),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc1)
                       TRAIL_ARG(fname) );

   F77_IMPORT_LOGICAL( freply, *reply );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}



F77_SUBROUTINE(dat_unmap)( CHARACTER(loc),
                           INTEGER(status)
                           TRAIL(loc) );


/*+ sc2dat_unmap - unmap an object previously mapped */

void sc2dat_unmap
( 
char loc[DAT__SZLOC],   /* primitive locator (given) */
int *status             /* global status (given and returned) */
) 

{

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc, floc );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(dat_unmap)( CHARACTER_ARG(floc),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc) );

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}


