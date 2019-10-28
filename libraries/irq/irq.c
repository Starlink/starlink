/*
*+
*  Name:
*     irq.c

*  Purpose:
*     Implement the C interface to the standalone routines in the KAPLIBS
*     library.

*  Description:
*     This module implements C-callable wrappers for the public non-ADAM
*     routines in the IRQ library. The interface to these wrappers
*     is defined in irq.h.

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S Berry
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     29-SEP-2005 (DSB):
*        Original version.
*     10-JUL-2008 (TIMJ):
*        Use starmem.
*     2009 August 24 (MJC):
*        Separate IRQ.
*     2010-06-17 (TIMJ):
*        Add irqNxtqn and irqNumqn
*     {enter_further_changes_here}

*-
*/

/* Header files. */
/* ============= */
#include "f77.h"
#include "sae_par.h"
#include "mers.h"
#include "star/mem.h"
#include "star/hds_fortran.h"
#include "irq.h"
#include <string.h>

/* Wrapper function implementations. */
/* ================================= */

F77_SUBROUTINE(irq_delet)( INTEGER(INDF),
                           INTEGER(STATUS) );

void irqDelet( int indf, int *status ){
   DECLARE_INTEGER(INDF);
   DECLARE_INTEGER(STATUS);
   F77_EXPORT_INTEGER( indf, INDF );
   F77_EXPORT_INTEGER( *status, STATUS );
   F77_LOCK( F77_CALL(irq_delet)( INTEGER_ARG(&INDF),
                        INTEGER_ARG(&STATUS)  ); )
   F77_IMPORT_INTEGER( STATUS, *status );
}

/* ------------------------------- */

F77_SUBROUTINE(irq_rlse)( CHARACTER_ARRAY(LOCS),
                          INTEGER(STATUS)
                          TRAIL(LOCS) );

void irqRlse( IRQLocs **locs, int *status ){
   DECLARE_CHARACTER_ARRAY(LOCS,DAT__SZLOC,5);
   DECLARE_INTEGER(STATUS);

   if( !locs || !*locs ) return;

/* Convert the C HDSLocs to DAT__SZLOC character locators, freeing the
   memory used to store the HDSLocs. */
   datExportFloc( &( (*locs)->loc[0] ), 1, DAT__SZLOC, LOCS[0], status );
   datExportFloc( &( (*locs)->loc[1] ), 1, DAT__SZLOC, LOCS[1], status );
   datExportFloc( &( (*locs)->loc[2] ), 1, DAT__SZLOC, LOCS[2], status );
   datExportFloc( &( (*locs)->loc[3] ), 1, DAT__SZLOC, LOCS[3], status );
   datExportFloc( &( (*locs)->loc[4] ), 1, DAT__SZLOC, LOCS[4], status );
   starFree( *locs );
   *locs = NULL;

/* Free the DAT__SZLOC character locators, etc. */
   F77_EXPORT_INTEGER( *status, STATUS );
   F77_LOCK( F77_CALL(irq_rlse)( CHARACTER_ARRAY_ARG(LOCS),
                       INTEGER_ARG(&STATUS)
                       TRAIL_ARG(LOCS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
}

/* ------------------------------------- */

F77_SUBROUTINE(irq_new)( INTEGER(INDF),
                         CHARACTER(XNAME),
                         CHARACTER_ARRAY(LOCS),
                         INTEGER(STATUS)
                         TRAIL(XNAME)
                         TRAIL(LOCS) );

void irqNew( int indf, const char *xname, IRQLocs **locs, int *status ){

   DECLARE_INTEGER(INDF);
   DECLARE_CHARACTER_DYN(XNAME);
   DECLARE_CHARACTER_ARRAY(LOCS,DAT__SZLOC,5);
   DECLARE_INTEGER(STATUS);
   int j;

   if( !locs ) return;
   *locs= NULL;

   F77_EXPORT_INTEGER( indf, INDF );
   F77_CREATE_EXPORT_CHARACTER( xname, XNAME );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(irq_new)( INTEGER_ARG(&INDF),
                      CHARACTER_ARG(XNAME),
                      CHARACTER_ARRAY_ARG(LOCS),
                      INTEGER_ARG(&STATUS)
                      TRAIL_ARG(XNAME)
                      TRAIL_ARG(LOCS) ); )

   F77_FREE_CHARACTER( XNAME );
   F77_IMPORT_INTEGER( STATUS, *status );

   if( *status == SAI__OK ) {
      *locs = starMalloc( sizeof( IRQLocs ) );
      if( *locs ) {
         for( j = 0; j < 5; j++ ) (*locs)->loc[j] = NULL;
         for( j = 0; j < 5; j++ ) {
            HDS_IMPORT_FLOCATOR( LOCS[j], &((*locs)->loc[j]), status );
         }
      } else {
         F77_LOCK( F77_CALL(irq_rlse)( CHARACTER_ARRAY_ARG(LOCS),
                             INTEGER_ARG(&STATUS)
                             TRAIL_ARG(LOCS) ); )

         if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep( "IRQNEW_ERR", "Cannot allocate memory for a new "
                    "IRQLocs structure.", status );
         }
      }
   }
}

/* ------------------------------------- */

F77_SUBROUTINE(irq_find)( INTEGER(INDF),
                          CHARACTER_ARRAY(LOCS),
                          CHARACTER(XNAME),
                          INTEGER(STATUS)
                          TRAIL(LOCS)
                          TRAIL(XNAME) );

void irqFind( int indf, IRQLocs **locs, char xname[DAT__SZNAM + 1], int *status ){

   DECLARE_INTEGER(INDF);
   DECLARE_CHARACTER_ARRAY(LOCS,DAT__SZLOC,5);
   DECLARE_CHARACTER_DYN(XNAME);
   DECLARE_INTEGER(STATUS);
   int j;

   if( !locs ) return;
   *locs= NULL;

   F77_EXPORT_INTEGER( indf, INDF );
   F77_CREATE_CHARACTER( XNAME, DAT__SZNAM );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(irq_find)( INTEGER_ARG(&INDF),
                       CHARACTER_ARRAY_ARG(LOCS),
                       CHARACTER_ARG(XNAME),
                       INTEGER_ARG(&STATUS)
                       TRAIL_ARG(LOCS)
                       TRAIL_ARG(XNAME) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_CHARACTER( XNAME, XNAME_length, xname );

   if( *status == SAI__OK ) {
      *locs = starMalloc( sizeof( IRQLocs ) );
      if( *locs ) {
         for( j = 0; j < 5; j++ ) (*locs)->loc[j] = NULL;
         for( j = 0; j < 5; j++ ) {
            HDS_IMPORT_FLOCATOR( LOCS[j], &((*locs)->loc[j]), status );
         }
      } else {
         F77_LOCK( F77_CALL(irq_rlse)( CHARACTER_ARRAY_ARG(LOCS),
                             INTEGER_ARG(&STATUS)
                             TRAIL_ARG(LOCS) ); )

         if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep( "IRQFIND_ERR", "Cannot allocate memory for a new "
                    "IRQLocs structure.", status );
         }
      }
   }

   F77_FREE_CHARACTER( XNAME );

}

/* ------------------------------- */

F77_SUBROUTINE(irq_addqn)( CHARACTER_ARRAY(LOCS),
                           CHARACTER(QNAME),
                           LOGICAL(DEFLT),
                           CHARACTER(COMMNT),
                           INTEGER(STATUS)
                           TRAIL(LOCS)
                           TRAIL(QNAME)
                           TRAIL(COMMNT) );

void irqAddqn( const IRQLocs *locs, const char *qname, int deflt,
               const char *commnt, int *status ){
   DECLARE_CHARACTER_ARRAY(LOCS,DAT__SZLOC,5);
   DECLARE_CHARACTER_DYN(QNAME);
   DECLARE_LOGICAL(DEFLT);
   DECLARE_CHARACTER_DYN(COMMNT);
   DECLARE_INTEGER(STATUS);

   HDS_EXPORT_CLOCATOR( locs->loc[0], LOCS[0], status );
   HDS_EXPORT_CLOCATOR( locs->loc[1], LOCS[1], status );
   HDS_EXPORT_CLOCATOR( locs->loc[2], LOCS[2], status );
   HDS_EXPORT_CLOCATOR( locs->loc[3], LOCS[3], status );
   HDS_EXPORT_CLOCATOR( locs->loc[4], LOCS[4], status );

   F77_CREATE_EXPORT_CHARACTER( qname, QNAME );
   F77_EXPORT_LOGICAL( deflt, DEFLT );
   F77_CREATE_EXPORT_CHARACTER( commnt, COMMNT );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(irq_addqn)( CHARACTER_ARRAY_ARG(LOCS),
                        CHARACTER_ARG(QNAME),
                        LOGICAL_ARG(&DEFLT),
                        CHARACTER_ARG(COMMNT),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(LOCS)
                        TRAIL_ARG(QNAME)
                        TRAIL_ARG(COMMNT) ); )

   F77_FREE_CHARACTER( QNAME );
   F77_FREE_CHARACTER( COMMNT );
   F77_IMPORT_INTEGER( STATUS, *status );
}

/* ------------------------------- */

F77_SUBROUTINE(irq_setqm)( CHARACTER_ARRAY(LOCS),
                           LOGICAL(BAD),
                           CHARACTER(QNAME),
                           INTEGER(SIZE),
                           REAL_ARRAY(MASK),
                           INTEGER(SET),
                           INTEGER(STATUS)
                           TRAIL(LOCS)
                           TRAIL(QNAME) );

void irqSetqm( const IRQLocs *locs, int bad, const char *qname, int size,
               float *mask, int *set, int *status ){

   DECLARE_CHARACTER_ARRAY(LOCS,DAT__SZLOC,5);
   DECLARE_LOGICAL(BAD);
   DECLARE_CHARACTER_DYN(QNAME);
   DECLARE_INTEGER(SIZE);
   DECLARE_REAL_ARRAY_DYN(MASK);
   DECLARE_INTEGER(SET);
   DECLARE_INTEGER(STATUS);

   HDS_EXPORT_CLOCATOR( locs->loc[0], LOCS[0], status );
   HDS_EXPORT_CLOCATOR( locs->loc[1], LOCS[1], status );
   HDS_EXPORT_CLOCATOR( locs->loc[2], LOCS[2], status );
   HDS_EXPORT_CLOCATOR( locs->loc[3], LOCS[3], status );
   HDS_EXPORT_CLOCATOR( locs->loc[4], LOCS[4], status );

   F77_EXPORT_LOGICAL( bad, BAD );
   F77_CREATE_EXPORT_CHARACTER( qname, QNAME );
   F77_EXPORT_INTEGER( size, SIZE );
   F77_CREATE_REAL_ARRAY( MASK, size );
   F77_EXPORT_REAL_ARRAY( mask, MASK, size );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(irq_setqm)( CHARACTER_ARRAY_ARG(LOCS),
                        LOGICAL_ARG(&BAD),
                        CHARACTER_ARG(QNAME),
                        INTEGER_ARG(&SIZE),
                        REAL_ARRAY_ARG(MASK),
                        INTEGER_ARG(&SET),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(LOCS)
                        TRAIL_ARG(QNAME) ); )

   F77_FREE_CHARACTER( QNAME );
   F77_IMPORT_INTEGER( SET, *set );
   F77_IMPORT_INTEGER( STATUS, *status );
   F77_FREE_REAL( MASK );
}

/* ------------------------------- */

F77_SUBROUTINE(irq_setqm8)( CHARACTER_ARRAY(LOCS),
                            LOGICAL(BAD),
                            CHARACTER(QNAME),
                            INTEGER8(SIZE),
                            REAL_ARRAY(MASK),
                            INTEGER8(SET),
                            INTEGER(STATUS)
                            TRAIL(LOCS)
                            TRAIL(QNAME) );

void irqSetqm8( const IRQLocs *locs, int bad, const char *qname, int64_t size,
                float *mask, int64_t *set, int *status ){

   DECLARE_CHARACTER_ARRAY(LOCS,DAT__SZLOC,5);
   DECLARE_LOGICAL(BAD);
   DECLARE_CHARACTER_DYN(QNAME);
   DECLARE_INTEGER8(SIZE);
   DECLARE_REAL_ARRAY_DYN(MASK);
   DECLARE_INTEGER8(SET);
   DECLARE_INTEGER(STATUS);

   HDS_EXPORT_CLOCATOR( locs->loc[0], LOCS[0], status );
   HDS_EXPORT_CLOCATOR( locs->loc[1], LOCS[1], status );
   HDS_EXPORT_CLOCATOR( locs->loc[2], LOCS[2], status );
   HDS_EXPORT_CLOCATOR( locs->loc[3], LOCS[3], status );
   HDS_EXPORT_CLOCATOR( locs->loc[4], LOCS[4], status );

   F77_EXPORT_LOGICAL( bad, BAD );
   F77_CREATE_EXPORT_CHARACTER( qname, QNAME );
   F77_EXPORT_INTEGER8( size, SIZE );
   F77_CREATE_REAL_ARRAY( MASK, size );
   F77_EXPORT_REAL_ARRAY( mask, MASK, size );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(irq_setqm8)( CHARACTER_ARRAY_ARG(LOCS),
                        LOGICAL_ARG(&BAD),
                        CHARACTER_ARG(QNAME),
                        INTEGER8_ARG(&SIZE),
                        REAL_ARRAY_ARG(MASK),
                        INTEGER8_ARG(&SET),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(LOCS)
                        TRAIL_ARG(QNAME) ); )

   F77_FREE_CHARACTER( QNAME );
   F77_IMPORT_INTEGER8( SET, *set );
   F77_IMPORT_INTEGER( STATUS, *status );
   F77_FREE_REAL( MASK );
}


/* ------------------------------- */

F77_SUBROUTINE(irq_rwqn)( CHARACTER_ARRAY(LOCS),
                          CHARACTER(QNAME),
                          LOGICAL(SET),
                          LOGICAL(NEWVAL),
                          LOGICAL(OLDVAL),
                          INTEGER(STATUS)
                          TRAIL(LOCS)
                          TRAIL(QNAME) );

void irqRwqn( const IRQLocs *locs, const char *qname, int set, int newval,
              int *oldval, int *status ){

   DECLARE_CHARACTER_ARRAY(LOCS,DAT__SZLOC,5);
   DECLARE_CHARACTER_DYN(QNAME);
   DECLARE_LOGICAL(SET);
   DECLARE_LOGICAL(NEWVAL);
   DECLARE_LOGICAL(OLDVAL);
   DECLARE_INTEGER(STATUS);

   HDS_EXPORT_CLOCATOR( locs->loc[0], LOCS[0], status );
   HDS_EXPORT_CLOCATOR( locs->loc[1], LOCS[1], status );
   HDS_EXPORT_CLOCATOR( locs->loc[2], LOCS[2], status );
   HDS_EXPORT_CLOCATOR( locs->loc[3], LOCS[3], status );
   HDS_EXPORT_CLOCATOR( locs->loc[4], LOCS[4], status );

   F77_CREATE_EXPORT_CHARACTER( qname, QNAME );

   F77_EXPORT_LOGICAL( set, SET );
   F77_EXPORT_LOGICAL( newval, NEWVAL );

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(irq_rwqn)( CHARACTER_ARRAY_ARG(LOCS),
                       CHARACTER_ARG(QNAME),
                       LOGICAL_ARG(&SET),
                       LOGICAL_ARG(&NEWVAL),
                       LOGICAL_ARG(&OLDVAL),
                       INTEGER_ARG(&STATUS)
                       TRAIL_ARG(LOCS)
                       TRAIL_ARG(QNAME) ); )

   F77_FREE_CHARACTER( QNAME );
   F77_IMPORT_LOGICAL( OLDVAL, *oldval );
   F77_IMPORT_INTEGER( STATUS, *status );
}


/* ------------------------------- */

F77_SUBROUTINE(irq_fxbit)( CHARACTER_ARRAY(LOCS),
                           CHARACTER(QNAME),
                           INTEGER(BIT),
                           LOGICAL(FIXBIT),
                           INTEGER(STATUS)
                           TRAIL(LOCS)
                           TRAIL(QNAME) );

void irqFxbit( const IRQLocs *locs, const char *qname, int bit, int *fixbit,
               int *status ){

   DECLARE_CHARACTER_ARRAY(LOCS,DAT__SZLOC,5);
   DECLARE_CHARACTER_DYN(QNAME);
   DECLARE_INTEGER(BIT);
   DECLARE_LOGICAL(FIXBIT);
   DECLARE_INTEGER(STATUS);

   HDS_EXPORT_CLOCATOR( locs->loc[0], LOCS[0], status );
   HDS_EXPORT_CLOCATOR( locs->loc[1], LOCS[1], status );
   HDS_EXPORT_CLOCATOR( locs->loc[2], LOCS[2], status );
   HDS_EXPORT_CLOCATOR( locs->loc[3], LOCS[3], status );
   HDS_EXPORT_CLOCATOR( locs->loc[4], LOCS[4], status );

   F77_CREATE_EXPORT_CHARACTER( qname, QNAME );
   F77_EXPORT_INTEGER( bit, BIT );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(irq_fxbit)( CHARACTER_ARRAY_ARG(LOCS),
                        CHARACTER_ARG(QNAME),
                        INTEGER_ARG(&BIT),
                        LOGICAL_ARG(&FIXBIT),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(LOCS)
                        TRAIL_ARG(QNAME) ); )

   F77_FREE_CHARACTER( QNAME );
   F77_IMPORT_LOGICAL( FIXBIT, *fixbit );
   F77_IMPORT_INTEGER( STATUS, *status );
}


/* ------------------------------- */

F77_SUBROUTINE(irq_getqn)( CHARACTER_ARRAY(LOCS),
                           CHARACTER(QNAME),
                           LOGICAL(FIXED),
                           LOGICAL(VALUE),
                           INTEGER(BIT),
                           CHARACTER(COMMNT),
                           INTEGER(STATUS)
                           TRAIL(LOCS)
                           TRAIL(QNAME)
                           TRAIL(COMMNT) );

void irqGetqn( const IRQLocs *locs, const char *qname, int *fixed, int *value,
                int *bit, char *commnt, int commnt_len, int *status ){

   DECLARE_CHARACTER_ARRAY(LOCS,DAT__SZLOC,5);
   DECLARE_CHARACTER_DYN(QNAME);
   DECLARE_LOGICAL(FIXED);
   DECLARE_LOGICAL(VALUE);
   DECLARE_INTEGER(BIT);
   DECLARE_CHARACTER_DYN(COMMNT);
   DECLARE_INTEGER(STATUS);

   HDS_EXPORT_CLOCATOR( locs->loc[0], LOCS[0], status );
   HDS_EXPORT_CLOCATOR( locs->loc[1], LOCS[1], status );
   HDS_EXPORT_CLOCATOR( locs->loc[2], LOCS[2], status );
   HDS_EXPORT_CLOCATOR( locs->loc[3], LOCS[3], status );
   HDS_EXPORT_CLOCATOR( locs->loc[4], LOCS[4], status );

   F77_CREATE_EXPORT_CHARACTER( qname, QNAME );
   F77_CREATE_CHARACTER( COMMNT, commnt_len-1 );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(irq_getqn)( CHARACTER_ARRAY_ARG(LOCS),
                        CHARACTER_ARG(QNAME),
                        LOGICAL_ARG(&FIXED),
                        LOGICAL_ARG(&VALUE),
                        INTEGER_ARG(&BIT),
                        CHARACTER_ARG(COMMNT),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(LOCS)
                        TRAIL_ARG(QNAME)
                        TRAIL_ARG(COMMNT) ); )

   F77_IMPORT_LOGICAL( FIXED, *fixed );
   F77_IMPORT_LOGICAL( VALUE, *value );
   F77_IMPORT_INTEGER( BIT, *bit );
   F77_IMPORT_CHARACTER( COMMNT, COMMNT_length, commnt );
   F77_IMPORT_INTEGER( STATUS, *status );

   F77_FREE_CHARACTER( QNAME );
   F77_FREE_CHARACTER( COMMNT );
}


/* ------------------------------- */

F77_SUBROUTINE(irq_rbit)( CHARACTER_ARRAY(LOCS),
                          CHARACTER(QNAME),
                          INTEGER(BIT),
                          INTEGER(STATUS)
                          TRAIL(LOCS)
                          TRAIL(QNAME) );

void irqRbit( const IRQLocs *locs, const char *qname, int *bit, int *status ){

   DECLARE_CHARACTER_ARRAY(LOCS,DAT__SZLOC,5);
   DECLARE_CHARACTER_DYN(QNAME);
   DECLARE_INTEGER(BIT);
   DECLARE_INTEGER(STATUS);

   HDS_EXPORT_CLOCATOR( locs->loc[0], LOCS[0], status );
   HDS_EXPORT_CLOCATOR( locs->loc[1], LOCS[1], status );
   HDS_EXPORT_CLOCATOR( locs->loc[2], LOCS[2], status );
   HDS_EXPORT_CLOCATOR( locs->loc[3], LOCS[3], status );
   HDS_EXPORT_CLOCATOR( locs->loc[4], LOCS[4], status );

   F77_CREATE_EXPORT_CHARACTER( qname, QNAME );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(irq_rbit)( CHARACTER_ARRAY_ARG(LOCS),
                       CHARACTER_ARG(QNAME),
                       INTEGER_ARG(&BIT),
                       INTEGER_ARG(&STATUS)
                       TRAIL_ARG(LOCS)
                       TRAIL_ARG(QNAME) ); )

   F77_IMPORT_INTEGER( BIT, *bit );
   F77_IMPORT_INTEGER( STATUS, *status );

   F77_FREE_CHARACTER( QNAME );
}

/* ------------------------------- */

F77_SUBROUTINE(irq_nxtqn)( CHARACTER_ARRAY(LOCS),
                           INTEGER(CONTXT),
                           CHARACTER(QNAME),
                           LOGICAL(FIXED),
                           LOGICAL(VALUE),
                           INTEGER(BIT),
                           CHARACTER(COMMNT),
                           LOGICAL(DONE),
                           INTEGER(STATUS)
                           TRAIL(LOCS)
                           TRAIL(QNAME)
                           TRAIL(COMMNT) );

void irqNxtqn( const IRQLocs *locs, IRQcntxt *contxt, char *qname, int *fixed, int *value,
               int *bit, char *commnt, int commnt_len, int * done, int *status ){

   DECLARE_CHARACTER_ARRAY(LOCS,DAT__SZLOC,5);
   DECLARE_CHARACTER_DYN(QNAME);
   DECLARE_INTEGER(CONTXT);
   DECLARE_LOGICAL(FIXED);
   DECLARE_LOGICAL(VALUE);
   DECLARE_INTEGER(BIT);
   DECLARE_CHARACTER_DYN(COMMNT);
   DECLARE_LOGICAL(DONE);
   DECLARE_INTEGER(STATUS);

   HDS_EXPORT_CLOCATOR( locs->loc[0], LOCS[0], status );
   HDS_EXPORT_CLOCATOR( locs->loc[1], LOCS[1], status );
   HDS_EXPORT_CLOCATOR( locs->loc[2], LOCS[2], status );
   HDS_EXPORT_CLOCATOR( locs->loc[3], LOCS[3], status );
   HDS_EXPORT_CLOCATOR( locs->loc[4], LOCS[4], status );

   F77_CREATE_CHARACTER( QNAME, IRQ__SZQNM );
   F77_CREATE_CHARACTER( COMMNT, commnt_len-1 );
   F77_EXPORT_INTEGER( *status, STATUS );
   F77_EXPORT_INTEGER( *contxt, CONTXT );

   F77_LOCK( F77_CALL(irq_nxtqn)( CHARACTER_ARRAY_ARG(LOCS),
                        INTEGER_ARG(&CONTXT),
                        CHARACTER_ARG(QNAME),
                        LOGICAL_ARG(&FIXED),
                        LOGICAL_ARG(&VALUE),
                        INTEGER_ARG(&BIT),
                        CHARACTER_ARG(COMMNT),
                        LOGICAL_ARG(&DONE),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(LOCS)
                        TRAIL_ARG(QNAME)
                        TRAIL_ARG(COMMNT) ); )

   F77_IMPORT_LOGICAL( FIXED, *fixed );
   F77_IMPORT_LOGICAL( VALUE, *value );
   F77_IMPORT_INTEGER( BIT, *bit );
   F77_IMPORT_CHARACTER( QNAME, QNAME_length, qname );
   F77_IMPORT_CHARACTER( COMMNT, COMMNT_length, commnt );
   F77_IMPORT_INTEGER( CONTXT, *contxt );
   F77_IMPORT_LOGICAL( DONE, *done );
   F77_IMPORT_INTEGER( STATUS, *status );

   F77_FREE_CHARACTER( QNAME );
   F77_FREE_CHARACTER( COMMNT );
}

/* ------------------------------- */
F77_SUBROUTINE(irq_numqn)( CHARACTER_ARRAY(LOCS),
                           INTEGER(NAMES),
                           INTEGER(STATUS)
                           TRAIL(LOCS) );

int irqNumqn( const IRQLocs *locs, int *status ) {
  int names;

  DECLARE_CHARACTER_ARRAY(LOCS,DAT__SZLOC,5);
  DECLARE_INTEGER(NAMES);
  DECLARE_INTEGER(STATUS);

  HDS_EXPORT_CLOCATOR( locs->loc[0], LOCS[0], status );
  HDS_EXPORT_CLOCATOR( locs->loc[1], LOCS[1], status );
  HDS_EXPORT_CLOCATOR( locs->loc[2], LOCS[2], status );
  HDS_EXPORT_CLOCATOR( locs->loc[3], LOCS[3], status );
  HDS_EXPORT_CLOCATOR( locs->loc[4], LOCS[4], status );

  F77_EXPORT_INTEGER( *status, STATUS );

  F77_LOCK( F77_CALL(irq_numqn)( CHARACTER_ARRAY_ARG(LOCS),
                       INTEGER_ARG(&NAMES),
                       INTEGER_ARG(&STATUS)
                       TRAIL_ARG(LOCS) ); )

  F77_IMPORT_INTEGER( NAMES, names );
  F77_IMPORT_INTEGER( STATUS, *status );
  return names;
}
