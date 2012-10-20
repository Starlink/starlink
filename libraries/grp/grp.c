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
*     28-FEB-2006 (DSB):
*        Rationalised conversion between C pointers and Fortran integer
*        identifiers. All Grp structures are now allocated dynamically
*        within this module (i.e. addresses to Grp structures allocated
*        statically within application code are no longer allowed). Also,
*        NULL pointers are used consistently in C to represent null
*        groups (i.e. the F77 GRP__NOID value).
*     25-JUN-2006 (TIMJ):
*        Add grpCopy.
*     8-AUG-2006 (DSB):
*        Add grpIndex
*     20-NOV-2006 (DSB):
*        Add grpSetcs
*     6-JUL-2007 (DSB):
*        Use "const Grp *" pointers for group parameters that are not
*        changed by the changed by the called function.
*     25-OCT-2007 (DSB):
*        Add grpSetsz
*     6-NOV-2007 (TIMJ):
*        Fix compiler error from confusion of C GRP__NOID with Fortran
*        GRP__NOID.
*     15-JUL-2008 (TIMJ):
*        - use size_t for index
*        - return result for grpValid, grpIndex and grpGrpsz rather than
*          passing in a pointer. More flexible that way.
*        - grpGrpex now passes in a size_t*
*     6-JUL-201 (DSB):
*        - grpValid should return zero without error if the supplied Grp
*        pointer is invalid.
*        - grpF2C was not modifing the contents of the returned Grp
*        structure if a previously allocated structure was re-used.
*     29-MAY-2012 (DSB):
*        Add grpGetcc
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2007, 2008, 2011-2012 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*/

/* Header files. */
/* ============= */
#include "f77.h"
#include "grp1.h"
#include "grp_err.h"
#include "sae_par.h"
#include "merswrap.h"
#include "star/mem.h"
#include <string.h>

/* Prototypes for local static functions */
/* ===================================== */
int grpSlot( int, int * );


/* Wrapper function implementations. */
/* ================================= */

/* An array which holds a pointer to a Grp structure for each possible
   group. The elements in this array correspond to the common arrays
   used to store group information within the fortran implementation
   (see GRP_COM). */
Grp *Grp_Pointers[ GRP__MAXG ];

/* A flag indicating if the Grp_Pointers array has been initialised. */
static int Grp_Init = 0;



/* All the following functions attempt to execute even if status is set
   on entry. They use a NULL pointer to represent the Fortran GRP__NOID
   value. They are not declared static so that they can be used within
   wrapper implementations for other libraries which use GRP. */



/* Free the Grp structure used to hold the Fortran ID of a group, and
   return NULL. */
/* ------------------------------------------------------------------- */
Grp *grpFree ( Grp *grp, int *status ) {
   int slot;
   if( grp ) {
      slot = grpSlot( grp->igrp, status );
      if( slot >= 0 ) Grp_Pointers[ slot ] = NULL;
      memset( grp, 0, sizeof( Grp ) );
      starFree( grp );
   }
   return NULL;
}



/* Return a pointer to a Grp structure holding a given Fortran group ID,
   creating a new structure if necessary. Note that it takes a fortran
   integer as arg */
/* ------------------------------------------------------------------- */
Grp *grpF2C( F77_INTEGER_TYPE IGRP, int * status ) {
   Grp *ret;
   int slot;

   ret = NULL;
   if( IGRP != GRP__FNOID ) {
      slot = grpSlot( IGRP, status );
      if( slot >= 0 ) {
         ret = Grp_Pointers[ slot ];
         if( !ret ) {
            ret = starMalloc( sizeof( Grp ) );
            if( ret ) {
               Grp_Pointers[ slot ] = ret;
            } else {
               *status = GRP__NOMEM;
               errRep( "GRP_F2C_ERR", "Unable to allocate memory for Grp object",
                       status );
            }
         }
         if( ret ) {
            ret->igrp = IGRP;
            ret->slot = slot;
         }
      }
   }
   return ret;
}



/* Return the Fortran ID value for a given Grp structure. */
/* ------------------------------------------------------------------- */
F77_INTEGER_TYPE grpC2F( const Grp *grp, int *status ){
   F77_INTEGER_TYPE ret;
   int slot;

   if( grp ){
      ret = grp->igrp;
      slot = grpSlot( ret, status );
      if( slot < 0 || slot != grp->slot ) ret = GRP__FNOID;

   } else {
      ret = GRP__FNOID;
   }

   return ret;
}



/* Return the slot number corresponding to a given Fortran ID value. Also
   initialises the static array of pointers if needed. */
/* ------------------------------------------------------------------- */
F77_SUBROUTINE(grp1_id2sl)( INTEGER(IGRP), INTEGER(SLOT) );

int grpSlot( int igrp, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(SLOT);
   int ret;

   if( !Grp_Init ) {
      int i;
      for( i = 0; i < GRP__MAXG; i++ ) Grp_Pointers[ i ] = NULL;
      Grp_Init = 1;
   }

   F77_EXPORT_INTEGER( igrp, IGRP );
   F77_LOCK( F77_CALL(grp1_id2sl)( INTEGER_ARG(&IGRP),
                         INTEGER_ARG(&SLOT) ); )
   F77_IMPORT_INTEGER( SLOT, ret );

   ret--;   /* Convert from 1-based to zero-based index */
   if( ret < 0 && *status == SAI__OK ) {
      *status = GRP__INVID;
      errRep( "GRP_SLOT_ERR", "Supplied Grp structure refers to an "
              "invalid GRP group (has the group already been deleted?)",
              status );
   }

   return ret;
}




/* Now start wrapping fortran routines */

F77_SUBROUTINE(grp_grpsz)( INTEGER(IGRP), INTEGER(SIZE), INTEGER(STATUS) );

size_t grpGrpsz( const Grp *grp, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(SIZE);
   DECLARE_INTEGER(STATUS);
   size_t size;

   IGRP = grpC2F( grp, status );

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(grp_grpsz)( INTEGER_ARG(&IGRP),
                        INTEGER_ARG(&SIZE),
                        INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( SIZE, size );
   F77_IMPORT_INTEGER( STATUS, *status );

   return size;
}

F77_SUBROUTINE(grp_setsz)( INTEGER(IGRP), INTEGER(SIZE), INTEGER(STATUS) );

void grpSetsz( Grp *grp, size_t size, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(SIZE);
   DECLARE_INTEGER(STATUS);

   IGRP = grpC2F( grp, status );

   F77_EXPORT_INTEGER( size, SIZE );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(grp_setsz)( INTEGER_ARG(&IGRP),
                        INTEGER_ARG(&SIZE),
                        INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}


F77_SUBROUTINE(grp_delet)( INTEGER(IGRP), INTEGER(STATUS) );

void grpDelet( Grp **grp, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(STATUS);

   IGRP = grpC2F( *grp, status );
   *grp = grpFree( *grp, status );

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(grp_delet)( INTEGER_ARG(&IGRP),
                        INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );


   return;
}



F77_SUBROUTINE(grp_get)( INTEGER(IGRP), INTEGER(INDEX), INTEGER(SIZE),
                         CHARACTER_ARRAY(NAMES), INTEGER(STATUS)
                         TRAIL(NAMES) );

/* Note the addition of a "len" parameter following the "names" array.
   This should be supplied equal to the allocated length of the shortest
   string for which a pointer has been supplied in "names". This length
   should include room for the trailing null. */

void grpGet( const Grp *grp, size_t index, size_t size, char *const *names,
             size_t len, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(INDEX);
   DECLARE_INTEGER(SIZE);
   DECLARE_CHARACTER_ARRAY_DYN(NAMES);
   DECLARE_INTEGER(STATUS);

   IGRP = grpC2F( grp, status );

   F77_EXPORT_INTEGER( index, INDEX );
   F77_EXPORT_INTEGER( size, SIZE );
   F77_CREATE_CHARACTER_ARRAY(NAMES,len-1,size);
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(grp_get)( INTEGER_ARG(&IGRP),
                      INTEGER_ARG(&INDEX),
                      INTEGER_ARG(&SIZE),
                      CHARACTER_ARRAY_ARG(NAMES),
                      INTEGER_ARG(&STATUS)
                      TRAIL_ARG(NAMES) ); )

   F77_IMPORT_CHARACTER_ARRAY_P(NAMES,NAMES_length,names,len,size);
   F77_FREE_CHARACTER(NAMES);
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}




F77_SUBROUTINE(grp_valid)( INTEGER(IGRP),
                           LOGICAL(VALID),
                           INTEGER(STATUS) );

int grpValid( const Grp *grp, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_LOGICAL(VALID);
   DECLARE_INTEGER(STATUS);
   int valid;

   if( *status != SAI__OK ) return 0;

   IGRP = grpC2F( grp, status );

   if( *status != SAI__OK ) {
      errAnnul( status );
      valid = 0;

   } else {

      F77_EXPORT_INTEGER( *status, STATUS );

      F77_LOCK( F77_CALL(grp_valid)( INTEGER_ARG(&IGRP),
                           LOGICAL_ARG(&VALID),
                           INTEGER_ARG(&STATUS) ); )

      F77_IMPORT_LOGICAL( VALID, valid );
      F77_IMPORT_INTEGER( STATUS, *status );
   }

   return valid;
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

   F77_LOCK( F77_CALL(grp_new)( CHARACTER_ARG(TYPE),
                      INTEGER_ARG(&IGRP),
                      INTEGER_ARG(&STATUS)
                      TRAIL_ARG(TYPE) ); )

   F77_FREE_CHARACTER( TYPE );
   F77_IMPORT_INTEGER( STATUS, *status );

   ret = grpF2C( IGRP, status );

   return ret;
}



F77_SUBROUTINE(grp_put1)( INTEGER(IGRP),
                          CHARACTER(NAME),
                          INTEGER(INDEX),
                          INTEGER(STATUS)
                          TRAIL(NAME) );


void grpPut1( Grp *grp, const char *name, size_t index, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_CHARACTER_DYN(NAME);
   DECLARE_INTEGER(INDEX);
   DECLARE_INTEGER(STATUS);

   IGRP = grpC2F( grp, status );

   F77_CREATE_CHARACTER( NAME, strlen( name ) );
   F77_EXPORT_CHARACTER( name, NAME, NAME_length );
   F77_EXPORT_INTEGER( index, INDEX );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(grp_put1)( INTEGER_ARG(&IGRP),
                       CHARACTER_ARG(NAME),
                       INTEGER_ARG(&INDEX),
                       INTEGER_ARG(&STATUS)
                       TRAIL_ARG(NAME) ); )

   F77_FREE_CHARACTER( NAME );
   F77_IMPORT_INTEGER( STATUS, *status );
}


F77_SUBROUTINE(grp_infoi)(INTEGER(IGRP),
			  INTEGER(INDEX),
			  CHARACTER(ITEM),
			  INTEGER(VALUE),
			  INTEGER(STATUS)
			  TRAIL(ITEM));

void grpInfoi( const Grp *grp, size_t index, const char *item, int *value,
	       int *status) {
  DECLARE_INTEGER(IGRP);
  DECLARE_INTEGER(INDEX);
  DECLARE_CHARACTER_DYN(ITEM);
  DECLARE_INTEGER(STATUS);
  DECLARE_INTEGER(VALUE);

  IGRP = grpC2F( grp, status );

  F77_CREATE_CHARACTER( ITEM, strlen(item) );
  F77_EXPORT_CHARACTER( item, ITEM, ITEM_length );
  F77_EXPORT_INTEGER( index, INDEX );
  F77_EXPORT_INTEGER( *status, STATUS );

  F77_LOCK( F77_CALL(grp_infoi)( INTEGER_ARG( &IGRP ),
		       INTEGER_ARG( &INDEX ),
		       CHARACTER_ARG(ITEM),
		       INTEGER_ARG(&VALUE),
		       INTEGER_ARG(&STATUS)
		       TRAIL_ARG(ITEM) ); )

  F77_FREE_CHARACTER( ITEM );
  F77_IMPORT_INTEGER( STATUS, *status );
  F77_IMPORT_INTEGER( VALUE, *value );

}

F77_SUBROUTINE(grp_infoc)( INTEGER(igrp), INTEGER(index),
                           CHARACTER(item), CHARACTER(value),
                           INTEGER(status) TRAIL(item) TRAIL(value) );

void grpInfoc( const Grp *grp, size_t index, const char *item, char *value,
	       size_t value_len, int *status) {
  DECLARE_INTEGER(IGRP);
  DECLARE_INTEGER(INDEX);
  DECLARE_CHARACTER_DYN(ITEM);
  DECLARE_INTEGER(STATUS);
  DECLARE_CHARACTER_DYN(VALUE);

  IGRP = grpC2F( grp, status );

  F77_CREATE_CHARACTER( ITEM, strlen(item) );
  F77_EXPORT_CHARACTER( item, ITEM, ITEM_length );
  F77_EXPORT_INTEGER( index, INDEX );
  F77_EXPORT_INTEGER( *status, STATUS );

  /* Create the fortran string one smaller than
     the C string so that we can take into account the nul */
  F77_CREATE_CHARACTER( VALUE, value_len - 1 );

  F77_LOCK( F77_CALL(grp_infoc)( INTEGER_ARG( &IGRP ),
		       INTEGER_ARG( &INDEX ),
		       CHARACTER_ARG(ITEM),
		       CHARACTER_ARG(VALUE),
		       INTEGER_ARG(&STATUS)
		       TRAIL_ARG(ITEM) TRAIL_ARG(VALUE) ); )

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

void grpGrpex( const char *grpexp, const Grp *grp1, Grp *grp2,
               size_t *size, int *added, int *flag, int *status ) {

  DECLARE_INTEGER(IGRP1);
  DECLARE_INTEGER(IGRP2);
  DECLARE_CHARACTER_DYN(GRPEXP);
  DECLARE_INTEGER(SIZE);
  DECLARE_INTEGER(ADDED);
  DECLARE_LOGICAL(FLAG);
  DECLARE_INTEGER(STATUS);

  IGRP1 = grpC2F( (Grp *) grp1, status );
  IGRP2 = grpC2F( grp2, status );

  F77_CREATE_CHARACTER( GRPEXP, strlen(grpexp) );
  F77_EXPORT_CHARACTER( grpexp, GRPEXP, GRPEXP_length );
  F77_EXPORT_INTEGER( *status, STATUS );


  F77_LOCK( F77_CALL(grp_grpex)( CHARACTER_ARG(GRPEXP), INTEGER_ARG(&IGRP1),
                       INTEGER_ARG(&IGRP2), INTEGER_ARG(&SIZE),
                       INTEGER_ARG(&ADDED), LOGICAL_ARG(&FLAG),
                       INTEGER_ARG(&STATUS) TRAIL_ARG(GRPEXP) ); )

  F77_FREE_CHARACTER( GRPEXP );
  F77_IMPORT_INTEGER( STATUS, *status );
  F77_IMPORT_INTEGER( SIZE, *size );
  F77_IMPORT_INTEGER( ADDED, *added );
  F77_IMPORT_LOGICAL( FLAG, *flag );

}


F77_SUBROUTINE(grp_index)( CHARACTER(NAME), INTEGER(IGRP),
                           INTEGER(START), INTEGER(INDEX),
                           INTEGER(STATUS) TRAIL(NAME) );

size_t grpIndex( const char *name, const Grp *grp, size_t start,
               int *status ){
  DECLARE_CHARACTER_DYN(NAME);
  DECLARE_INTEGER(IGRP);
  DECLARE_INTEGER(START);
  DECLARE_INTEGER(INDEX);
  DECLARE_INTEGER(STATUS);
  size_t index;

  IGRP = grpC2F( (Grp *) grp, status );

  F77_CREATE_CHARACTER( NAME, strlen(name) );
  F77_EXPORT_CHARACTER( name, NAME, NAME_length );
  F77_EXPORT_INTEGER( start, START );
  F77_EXPORT_INTEGER( *status, STATUS );

  F77_LOCK( F77_CALL(grp_index)( CHARACTER_ARG(NAME), INTEGER_ARG(&IGRP),
                       INTEGER_ARG(&START), INTEGER_ARG(&INDEX),
                       INTEGER_ARG(&STATUS) TRAIL_ARG(NAME) ); )

  F77_FREE_CHARACTER( NAME );
  F77_IMPORT_INTEGER( STATUS, *status );
  F77_IMPORT_INTEGER( INDEX, index );
  return index;
}


F77_SUBROUTINE(grp_copy)( INTEGER(IGRP1), INTEGER(INDXLO),
			  INTEGER(INDXHI), LOGICAL(REJECT),
			  INTEGER(IGRP2), INTEGER(STATUS));

Grp * grpCopy( const Grp* grp1, size_t indxlo, size_t indxhi, int reject,
	       int * status ) {
  DECLARE_INTEGER(IGRP1);
  DECLARE_INTEGER(IGRP2);
  DECLARE_INTEGER(INDXLO);
  DECLARE_INTEGER(INDXHI);
  DECLARE_LOGICAL(REJECT);
  DECLARE_INTEGER(STATUS);

  Grp * ret = NULL;

  IGRP1 = grpC2F( grp1, status );
  F77_EXPORT_LOGICAL( reject, REJECT );
  F77_EXPORT_INTEGER( indxlo, INDXLO );
  F77_EXPORT_INTEGER( indxhi, INDXHI );
  F77_EXPORT_INTEGER( *status, STATUS );

  F77_LOCK( F77_CALL(grp_copy)( INTEGER_ARG(&IGRP1), INTEGER_ARG(&INDXLO),
		      INTEGER_ARG(&INDXHI), LOGICAL_ARG(&REJECT),
		      INTEGER_ARG(&IGRP2), INTEGER_ARG(&STATUS)); )

  F77_IMPORT_INTEGER( STATUS, *status );

  ret = grpF2C( IGRP2, status );
  return ret;
}


F77_SUBROUTINE(grp_setcs)( INTEGER(IGRP), LOGICAL(SENSIT), INTEGER(STATUS) );

void grpSetcs( Grp *grp, int sensit, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_LOGICAL(SENSIT);
   DECLARE_INTEGER(STATUS);

   IGRP = grpC2F( grp, status );

   F77_EXPORT_INTEGER( *status, STATUS );
   F77_EXPORT_LOGICAL( sensit, SENSIT );

   F77_LOCK( F77_CALL(grp_setcs)( INTEGER_ARG(&IGRP),
                        LOGICAL_ARG(&SENSIT),
                        INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

F77_SUBROUTINE(grp_remov)( INTEGER(IGRP1), CHARACTER(NAME),
                           INTEGER(IGRP2), INTEGER(STATUS) TRAIL(NAME) );

Grp *grpRemov( const Grp *grp, const char *name, int *status ) {

   DECLARE_INTEGER(IGRP1);
   DECLARE_CHARACTER_DYN(NAME);
   DECLARE_INTEGER(IGRP2);
   DECLARE_INTEGER(STATUS);
   Grp *ret;

   IGRP1 = grpC2F( (Grp *) grp, status );

   F77_CREATE_CHARACTER( NAME, strlen(name) );
   F77_EXPORT_CHARACTER( name, NAME, NAME_length );
   F77_EXPORT_INTEGER( *status, STATUS );


   F77_LOCK( F77_CALL(grp_remov)( INTEGER_ARG(&IGRP1), CHARACTER_ARG(NAME),
                        INTEGER_ARG(&IGRP2), INTEGER_ARG(&STATUS)
                        TRAIL_ARG(NAME) ); )

   F77_FREE_CHARACTER( NAME );
   F77_IMPORT_INTEGER( STATUS, *status );

   ret = grpF2C( IGRP2, status );

   return ret;
}


F77_SUBROUTINE(grp_msg)( CHARACTER(TOKEN),
                         INTEGER(IGRP),
                         INTEGER(INDEX)
                         TRAIL(TOKEN) );

void grpMsg( const char *token, const Grp *grp, int index ){
   DECLARE_CHARACTER_DYN(TOKEN);
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(INDEX);
   int status = SAI__OK;

   if( grp ) {
      errMark();
      IGRP = grpC2F( (Grp *) grp, &status );
      if( status == SAI__OK ) {
         errRlse();

         F77_CREATE_CHARACTER( TOKEN, strlen( token ) );
         F77_EXPORT_CHARACTER( token, TOKEN, TOKEN_length );
         F77_EXPORT_INTEGER( index, INDEX );

         F77_LOCK( F77_CALL(grp_msg)( CHARACTER_ARG(TOKEN),
                            INTEGER_ARG(&IGRP),
                            INTEGER_ARG(&INDEX)
                            TRAIL_ARG(TOKEN) ); )

         F77_FREE_CHARACTER( TOKEN );

      } else {
         errAnnul( &status );
         errRlse();
         msgSetc( token, " " );
      }

   } else {
      msgSetc( token, " " );
   }
}

F77_SUBROUTINE(grp_own)( INTEGER(IGRP1), INTEGER(IGRP2), INTEGER(STATUS) );

Grp *grpOwn( const Grp *grp, int *status ){
   DECLARE_INTEGER(IGRP1);
   DECLARE_INTEGER(IGRP2);
   DECLARE_INTEGER(STATUS);
   Grp *ret;

   IGRP1 = grpC2F( (Grp *) grp, status );

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(grp_own)( INTEGER_ARG(&IGRP1),
                                INTEGER_ARG(&IGRP2),
                                INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );

   ret = grpF2C( IGRP2, status );
   return ret;
}


F77_SUBROUTINE(grp_slave)( INTEGER(IGRP1), INTEGER(IGRP2), INTEGER(STATUS) );

Grp *grpSlave( const Grp *grp, int *status ){
   DECLARE_INTEGER(IGRP1);
   DECLARE_INTEGER(IGRP2);
   DECLARE_INTEGER(STATUS);
   Grp *ret;

   IGRP1 = grpC2F( (Grp *) grp, status );

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(grp_slave)( INTEGER_ARG(&IGRP1),
                                  INTEGER_ARG(&IGRP2),
                                  INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );

   ret = grpF2C( IGRP2, status );
   return ret;
}


F77_SUBROUTINE(grp_show)( INTEGER(IGRP), INTEGER(SLAVES), INTEGER(STATUS) );

void grpShow( const Grp *grp, int slaves, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(SLAVES);
   DECLARE_INTEGER(STATUS);

   IGRP = grpC2F( grp, status );

   F77_EXPORT_INTEGER( *status, STATUS );
   F77_EXPORT_INTEGER( slaves, SLAVES );

   F77_LOCK( F77_CALL(grp_show)( INTEGER_ARG(&IGRP),
                                 INTEGER_ARG(&SLAVES),
                                 INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

F77_SUBROUTINE(grp_same)( INTEGER(IGRP1), INTEGER(IGRP2), LOGICAL(SAME), INTEGER(STATUS) );

void grpSame( const Grp *grp1, const Grp *grp2, int *same, int *status ){
   DECLARE_INTEGER(IGRP1);
   DECLARE_INTEGER(IGRP2);
   DECLARE_LOGICAL(SAME);
   DECLARE_INTEGER(STATUS);

   IGRP1 = grpC2F( grp1, status );
   IGRP2 = grpC2F( grp2, status );

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(grp_same)( INTEGER_ARG(&IGRP1),
                                 INTEGER_ARG(&IGRP2),
                                 LOGICAL_ARG(&SAME),
                                 INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_LOGICAL( SAME, *same );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

F77_SUBROUTINE(grp_sown)( INTEGER(IGRP1), INTEGER(IGRP2), INTEGER(STATUS) );

void grpSown( Grp *grp1, Grp *grp2, int *status ){
   DECLARE_INTEGER(IGRP1);
   DECLARE_INTEGER(IGRP2);
   DECLARE_INTEGER(STATUS);

   IGRP1 = grpC2F( grp1, status );
   IGRP2 = grpC2F( grp2, status );

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(grp_sown)( INTEGER_ARG(&IGRP1),
                                 INTEGER_ARG(&IGRP2),
                                 INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

F77_SUBROUTINE(grp_head)( INTEGER(IGRP1), INTEGER(IGRP2), INTEGER(STATUS) );

Grp *grpHead( const Grp *grp, int *status ){
   DECLARE_INTEGER(IGRP1);
   DECLARE_INTEGER(IGRP2);
   DECLARE_INTEGER(STATUS);
   Grp *ret;

   IGRP1 = grpC2F( (Grp *) grp, status );

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(grp_head)( INTEGER_ARG(&IGRP1),
                                 INTEGER_ARG(&IGRP2),
                                 INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );

   ret = grpF2C( IGRP2, status );
   return ret;
}



F77_SUBROUTINE(grp_getcc)( INTEGER(igrp), CHARACTER(cclist), CHARACTER(cc),
                           INTEGER(status) TRAIL(cclist) TRAIL(cc) );

void grpGetcc( const Grp *grp, const char *cclist, char *cc, size_t cc_len,
               int *status) {
  DECLARE_INTEGER(IGRP);
  DECLARE_CHARACTER_DYN(CCLIST);
  DECLARE_INTEGER(STATUS);
  DECLARE_CHARACTER_DYN(CC);

  IGRP = grpC2F( grp, status );

  F77_CREATE_CHARACTER( CCLIST, strlen(cclist) );
  F77_EXPORT_CHARACTER( cclist, CCLIST, CCLIST_length );
  F77_EXPORT_INTEGER( *status, STATUS );

  /* Create the fortran string one smaller than
     the C string so that we can take into account the nul */
  F77_CREATE_CHARACTER( CC, cc_len - 1 );

  F77_LOCK( F77_CALL(grp_getcc)( INTEGER_ARG( &IGRP ),
		       CHARACTER_ARG(CCLIST),
		       CHARACTER_ARG(CC),
		       INTEGER_ARG(&STATUS)
		       TRAIL_ARG(CCLIST) TRAIL_ARG(CC) ); )

  F77_FREE_CHARACTER( CCLIST );
  F77_IMPORT_CHARACTER( CC, CC_length, cc );
  F77_FREE_CHARACTER( CC );
  F77_IMPORT_INTEGER( STATUS, *status );

  if (*status != SAI__OK) {
    /* terminate on bad status */
    cc[0] = '\0';
  }
}

F77_SUBROUTINE(grp_watch)( INTEGER(IGRP), INTEGER(STATUS) );
void grpWatch( int igrp, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(STATUS);
   F77_EXPORT_INTEGER( igrp, IGRP );
   F77_EXPORT_INTEGER( *status, STATUS );
   F77_LOCK( F77_CALL(grp_watch)( INTEGER_ARG(&IGRP),
                                  INTEGER_ARG(&STATUS) ); )
   F77_IMPORT_INTEGER( STATUS, *status );
}

