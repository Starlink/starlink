/*
*  Name:
*     grp.c

*  Purpose:
*     Implement the C interface to the GRP library.

*  Description:
*     This module implements C-callable wrappers for the public
*     routines in the GRP library. The interface to these wrappers
*     is defined in grp.h.

*  Notes:
*     - Given the size of the GRP library, providing a complete C
*     interface is probably not worth the effort. Instead, I suggest that 
*     people who want to use GRP from C extend this file (and
*     grp.h) to include any functions which they need but which are
*     not already included.

*  Authors:
*     DSB: David S Berry
*     {enter_new_authors_here}

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}
*/

/* Header files. */
/* ============= */
#include "f77.h"                 
#include "grp.h"
#include "grp_err.h"
#include "sae_par.h"
#include "merswrap.h"
#include <string.h>

/* Wrapper function implementations. */
/* ================================= */

/* We need a function to allocate the memory for the struct
   and intialise it. It will be freed using grpDelet.
   Alternatively use grpFree.
   Probably should put this
   in its own file with documentation. */

Grp * grpInit( int *status ) {
  Grp * newgrp;

  if ( *status != SAI__OK ) return NULL;
  newgrp = malloc( sizeof(Grp) );
  if (newgrp != NULL) {
    newgrp->igrp = GRP__NOID;
  } else {
    *status = GRP__NOMEM;
    errRep( "GRP_INIT_ERR", "Unable to allocate memory for Grp object",
	    status );
  }
  return newgrp;
}

void grpFree ( Grp ** igrp, int * status ) {
  if ( *igrp != NULL ) free( *igrp );
}

/* We need private (yet public) functions that are available to
   other libraries that are wrapping fortran access to GRP, that 
   allows them to store the fortran Grp identifier without having
   to look inside the struct. This function is not part of the
   public API for anything except wrapping fortran (eg kaplibs
   and NDG) */

/* Note that it takes a fortran integer as arg */
void grp1Setid ( Grp *igrp, F77_INTEGER_TYPE IGRP, int * status ) {
  if ( *status != SAI__OK ) return;
  if ( igrp == NULL ) {
    *status = GRP__INTER;
    errRep( "GRP1_SETID_ERR", "Grp struct pointer was null in setid",
	    status);
  }
  igrp->igrp = IGRP;
}

/* Note that it takes a fortran integer as arg */
F77_INTEGER_TYPE grp1Getid ( Grp *igrp, int * status ) {
  if ( *status != SAI__OK ) return (F77_INTEGER_TYPE) GRP__NOID;
  if ( igrp == NULL ) {
    *status = GRP__INTER;
    errRep( "GRP1_GETID_ERR", "Grp struct pointer was null in getid",
	    status);
  }
  return igrp->igrp;
}



/* Now start wrapping fortran routines */

F77_SUBROUTINE(grp_grpsz)( INTEGER(IGRP), INTEGER(SIZE), INTEGER(STATUS) );

void grpGrpsz( Grp *igrp, int *size, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(SIZE);
   DECLARE_INTEGER(STATUS);

   IGRP = grp1Getid( igrp, status );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_CALL(grp_grpsz)( INTEGER_ARG(&IGRP),
                        INTEGER_ARG(&SIZE),
                        INTEGER_ARG(&STATUS) );

   F77_IMPORT_INTEGER( SIZE, *size );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}


F77_SUBROUTINE(grp_delet)( INTEGER(IGRP), INTEGER(STATUS) );

void grpDelet( Grp **igrp, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(STATUS);

   IGRP = grp1Getid( *igrp, status );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_CALL(grp_delet)( INTEGER_ARG(&IGRP),
                        INTEGER_ARG(&STATUS) );

   F77_IMPORT_INTEGER( STATUS, *status );

   /* Just free the memory associated with the struct */
   grpFree( igrp, status );

   return;
}



F77_SUBROUTINE(grp_get)( INTEGER(IGRP), INTEGER(INDEX), INTEGER(SIZE), 
                         CHARACTER_ARRAY(NAMES), INTEGER(STATUS)
                         TRAIL(NAMES) );

/* Note the addition of a "len" parameter following the "names" array.
   This should be supplied equal to the allocated length of the shortest 
   string for which a pointer has been supplied in "names". This length
   should include room for the trailing null. */

void grpGet( Grp *igrp, int index, int size, char *const *names, int len, 
             int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(INDEX);
   DECLARE_INTEGER(SIZE);
   DECLARE_CHARACTER_ARRAY_DYN(NAMES);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( index, INDEX );
   F77_EXPORT_INTEGER( size, SIZE );
   F77_CREATE_CHARACTER_ARRAY(NAMES,len-1,size);
   IGRP = grp1Getid( igrp, status );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_CALL(grp_get)( INTEGER_ARG(&IGRP),
                      INTEGER_ARG(&INDEX),
                      INTEGER_ARG(&SIZE),
                      CHARACTER_ARRAY_ARG(NAMES),
                      INTEGER_ARG(&STATUS) 
                      TRAIL_ARG(NAMES) );

   F77_IMPORT_CHARACTER_ARRAY_P(NAMES,NAMES_length,names,len,size);
   F77_FREE_CHARACTER(NAMES);
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}




F77_SUBROUTINE(grp_valid)( INTEGER(IGRP), 
                           LOGICAL(VALID),
                           INTEGER(STATUS) );

void grpValid( Grp *igrp, int *valid, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_LOGICAL(VALID);
   DECLARE_INTEGER(STATUS);

   IGRP = grp1Getid( igrp, status );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_CALL(grp_valid)( INTEGER_ARG(&IGRP),
                        LOGICAL_ARG(&VALID),
                        INTEGER_ARG(&STATUS) );

   F77_IMPORT_LOGICAL( VALID, *valid );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}



F77_SUBROUTINE(grp_new)( CHARACTER(TYPE), 
                         INTEGER(IGRP), 
                         INTEGER(STATUS)
                         TRAIL(TYPE) );


Grp *grpNew( const char *type, int *status ){
   DECLARE_CHARACTER_DYN(TYPE);
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(STATUS);
   Grp *ret;

   F77_CREATE_CHARACTER( TYPE, strlen( type ) );
   F77_EXPORT_CHARACTER( type, TYPE, TYPE_length );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_CALL(grp_new)( CHARACTER_ARG(TYPE),
                      INTEGER_ARG(&IGRP),
                      INTEGER_ARG(&STATUS) 
                      TRAIL_ARG(TYPE) );

   F77_FREE_CHARACTER( TYPE );
   F77_IMPORT_INTEGER( STATUS, *status );

   ret = grpInit( status );
   grp1Setid( ret, IGRP, status );

   return ret;
}



F77_SUBROUTINE(grp_put1)( INTEGER(IGRP), 
                          CHARACTER(NAME), 
                          INTEGER(INDEX), 
                          INTEGER(STATUS)
                          TRAIL(NAME) );


void grpPut1( Grp *grp, const char *name, int index, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_CHARACTER_DYN(NAME);
   DECLARE_INTEGER(INDEX);
   DECLARE_INTEGER(STATUS);

   IGRP = grp1Getid( grp, status );

   F77_CREATE_CHARACTER( NAME, strlen( name ) );
   F77_EXPORT_CHARACTER( name, NAME, NAME_length );
   F77_EXPORT_INTEGER( index, INDEX );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_CALL(grp_put1)( INTEGER_ARG(&IGRP),
                       CHARACTER_ARG(NAME),
                       INTEGER_ARG(&INDEX),
                       INTEGER_ARG(&STATUS) 
                       TRAIL_ARG(NAME) );

   F77_FREE_CHARACTER( NAME );
   F77_IMPORT_INTEGER( STATUS, *status );
}




