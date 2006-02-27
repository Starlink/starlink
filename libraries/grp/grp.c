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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     03-NOV-2005 (TIMJ):
*        Use Grp* rather than plain integer
*     09-NOV-2005 (DSB):
*        Add grpValid, grpNew and grpPut1
*     09-NOV-2005 (TIMJ):
*        Fix return value of grp1Getid if input Grp* is NULL
*     24-JAN-2006 (TIMJ):
*        Add grpInfoi
*     25-JAN-2006 (TIMJ):
*        GRP identifier should be obtained for grpDelet even if status is bad
*        Rename grpInfoi to grpInfoI
*     26-JAN-2006 (TIMJ):
*        grpInfoI back to grpInfoi after populist revolt. (and to be
*        consistent with other Starlink wrappers).
*     24-FEB-2006 (TIMJ):
*        Add grpInfoc
*     26-FEB-2006 (TIMJ):
*        Add grpGrpex
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

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
    errRep( "GRP1_SETID_ERR", "Grp struct pointer was NULL in setid",
	    status);
  }
  igrp->igrp = IGRP;
}

/* Note that it takes a fortran integer as arg */
/* Not sure why this can't return GRP__NOID if given a NULL pointer
   without setting status */
F77_INTEGER_TYPE grp1Getid ( const Grp *igrp, int * status ) {
  if ( *status != SAI__OK ) return (F77_INTEGER_TYPE) GRP__NOID;
  if ( igrp == NULL ) {
    *status = GRP__INTER;
    errRep( "GRP1_GETID_ERR", "Grp struct pointer was NULL in getid",
	    status);
    return (F77_INTEGER_TYPE) GRP__NOID;
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
   int lstat = SAI__OK;

   /* Need to get hold of the Id regardless of status */
   errMark();
   IGRP = grp1Getid( *igrp, &lstat );
   if (lstat != SAI__OK) errAnnul( &lstat );
   errRlse();

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

   if( igrp ) {

      IGRP = grp1Getid( igrp, status );
      F77_EXPORT_INTEGER( *status, STATUS );
   
      F77_CALL(grp_valid)( INTEGER_ARG(&IGRP),
                           LOGICAL_ARG(&VALID),
                           INTEGER_ARG(&STATUS) );
   
      F77_IMPORT_LOGICAL( VALID, *valid );
      F77_IMPORT_INTEGER( STATUS, *status );

   } else {
      *valid = 0;
   }      

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


F77_SUBROUTINE(grp_infoi)(INTEGER(IGRP),
			  INTEGER(INDEX),
			  CHARACTER(ITEM),
			  INTEGER(VALUE),
			  INTEGER(STATUS)
			  TRAIL(ITEM));

void grpInfoi( Grp *grp, int index, const char * item, int * value, 
	       int *status) {
  DECLARE_INTEGER(IGRP);
  DECLARE_INTEGER(INDEX);
  DECLARE_CHARACTER_DYN(ITEM);
  DECLARE_INTEGER(STATUS);
  DECLARE_INTEGER(VALUE);

  if (grp == NULL ) {
    IGRP = (F77_INTEGER_TYPE)GRP__NOID;
  } else {
    IGRP = grp1Getid( grp, status );
  }

  F77_CREATE_CHARACTER( ITEM, strlen(item) );
  F77_EXPORT_CHARACTER( item, ITEM, ITEM_length );
  F77_EXPORT_INTEGER( index, INDEX );
  F77_EXPORT_INTEGER( *status, STATUS );

  F77_CALL(grp_infoi)( INTEGER_ARG( &IGRP ),
		       INTEGER_ARG( &INDEX ),
		       CHARACTER_ARG(ITEM),
		       INTEGER_ARG(&VALUE),
		       INTEGER_ARG(&STATUS)
		       TRAIL_ARG(ITEM) );

  F77_FREE_CHARACTER( ITEM );
  F77_IMPORT_INTEGER( STATUS, *status );
  F77_IMPORT_INTEGER( VALUE, *value );

}

F77_SUBROUTINE(grp_infoc)( INTEGER(igrp), INTEGER(index),
                                  CHARACTER(item), CHARACTER(value),
                                  INTEGER(status) TRAIL(item) TRAIL(value) );

void grpInfoc( Grp *grp, int index, const char * item, char * value, 
	       size_t value_len, int *status) {
  DECLARE_INTEGER(IGRP);
  DECLARE_INTEGER(INDEX);
  DECLARE_CHARACTER_DYN(ITEM);
  DECLARE_INTEGER(STATUS);
  DECLARE_CHARACTER_DYN(VALUE);

  if (grp == NULL ) {
    IGRP = (F77_INTEGER_TYPE)GRP__NOID;
  } else {
    IGRP = grp1Getid( grp, status );
  }

  F77_CREATE_CHARACTER( ITEM, strlen(item) );
  F77_EXPORT_CHARACTER( item, ITEM, ITEM_length );
  F77_EXPORT_INTEGER( index, INDEX );
  F77_EXPORT_INTEGER( *status, STATUS );

  /* Create the fortran string one smaller than
     the C string so that we can take into account the nul */
  F77_CREATE_CHARACTER( VALUE, value_len - 1 );

  F77_CALL(grp_infoc)( INTEGER_ARG( &IGRP ),
		       INTEGER_ARG( &INDEX ),
		       CHARACTER_ARG(ITEM),
		       CHARACTER_ARG(VALUE),
		       INTEGER_ARG(&STATUS)
		       TRAIL_ARG(ITEM) TRAIL_ARG(VALUE) );

  F77_FREE_CHARACTER( ITEM );
  F77_IMPORT_CHARACTER( VALUE, VALUE_length, value );
  F77_FREE_CHARACTER( VALUE );
  F77_IMPORT_INTEGER( STATUS, *status );

  if (*status != SAI__OK) {
    /* terminate on bad status */
    value[0] = '\0';
  }
}

F77_SUBROUTINE(grp_grpex)( CHARACTER(grpexp), INTEGER(igrp1),
                                  INTEGER(igrp2), INTEGER(size),
                                  INTEGER(added), LOGICAL(flag),
                                  INTEGER(status) TRAIL(grpexp) );

void grpGrpex( const char * grpexp, const Grp * igrp1, Grp * igrp2,
               int* size, int *added, int * flag, int * status ) {

  DECLARE_INTEGER(IGRP1);
  DECLARE_INTEGER(IGRP2);
  DECLARE_CHARACTER_DYN(GRPEXP);
  DECLARE_INTEGER(SIZE);
  DECLARE_INTEGER(ADDED);
  DECLARE_LOGICAL(FLAG);
  DECLARE_INTEGER(STATUS);

  if (igrp1 == NULL ) {
    IGRP1 = (F77_INTEGER_TYPE)GRP__NOID;
  } else {
    IGRP1 = grp1Getid( igrp1, status );
  }
  if (igrp2 == NULL ) {
    IGRP2 = (F77_INTEGER_TYPE)GRP__NOID;
  } else {
    IGRP2 = grp1Getid( igrp2, status );
  }

  F77_CREATE_CHARACTER( GRPEXP, strlen(grpexp) );
  F77_EXPORT_CHARACTER( grpexp, GRPEXP, GRPEXP_length );
  F77_EXPORT_INTEGER( *status, STATUS );

  
  F77_CALL(grp_grpex)( CHARACTER_ARG(GRPEXP), INTEGER_ARG(&IGRP1),
                       INTEGER_ARG(&IGRP2), INTEGER_ARG(&SIZE),
                       INTEGER_ARG(&ADDED), LOGICAL_ARG(&FLAG),
                       INTEGER_ARG(&STATUS) TRAIL_ARG(GRPEXP) );

  F77_FREE_CHARACTER( GRPEXP );
  F77_IMPORT_INTEGER( STATUS, *status );
  F77_IMPORT_INTEGER( SIZE, *size );
  F77_IMPORT_INTEGER( ADDED, *added );
  F77_IMPORT_LOGICAL( FLAG, *flag );

}
