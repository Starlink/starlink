/*
*  Name:
*     ndg.c

*  Purpose:
*     Implement the C interface to the NDG library.

*  Description:
*     This module implements C-callable wrappers for the public non-ADAM
*     routines in the NDG library. The interface to these wrappers
*     is defined in ndg.h.

*  Notes:
*     - Given the size of the NDG library, providing a complete C
*     interface is probably not worth the effort. Instead, I suggest that
*     people who want to use NDG from C extend this file (and
*     ndg.h) to include any functions which they need but which are
*     not already included.

*  Authors:
*     DSB: David S Berry (JAC, UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     02-NOV-2005 (TIMJ):
*        Port from GRP
*     20-DEC-2005 (TIMJ):
*        Add ndgAsexp
*     28-FEB-2006 (DSB):
*        Use grpC2F and grpF2C.
*     12-JUL-2006 (TIMJ):
*        Add ndgNdfcr
*     8-AUG-2006 (DSB):
*        Added ndgGtsup
*     2-NOV-2007 (DSB):
*        Added ndgPtprv, ndgBegpv and ndgEndpv.
*     23-NOV-2007 (DSB):
*        Removed ndgPtprv (now in ndg_provenance.c) and modified ndgEndpv.
*     15-JUL-2008 (TIMJ):
*        Use size_t for index to match new Grp interface.
*     13-AUG-2008 (DSB):
*        Added ndgCopy.
*     2010-05-05 (TIMJ):
*        Add ndgAddgh
*     6-MAY-2010 (DSB):
*        Move ndgAddgh to ndg_adam.c
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2007-2010 Science & Technology Facilities Council.
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
#include "sae_par.h"
#include "par_par.h"
#include "star/grp.h"
#include "star/hds_types.h"
#include "star/hds_fortran.h"
#include "ndf.h"
#include "ndg.h"

/* Wrapper function implementations. */
/* ================================= */

F77_SUBROUTINE(ndg_ndfas)( INTEGER(IGRP), INTEGER(INDEX), CHARACTER(MODE), INTEGER(INDF),
INTEGER(STATUS) TRAIL(MODE) );

void ndgNdfas( const Grp *igrp, size_t index, const char mode[], int * indf, int * status ) {
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(INDEX);
   DECLARE_CHARACTER(MODE, 10);
   DECLARE_INTEGER(INDF);
   DECLARE_INTEGER(STATUS);

   IGRP = grpC2F( igrp, status );
   F77_EXPORT_INTEGER(index, INDEX );
   F77_EXPORT_CHARACTER( mode, MODE, 10 );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(ndg_ndfas)( INTEGER_ARG(&IGRP), INTEGER_ARG(&INDEX),
                        CHARACTER_ARG(MODE), INTEGER_ARG(&INDF),
                        INTEGER_ARG(&STATUS) TRAIL_ARG(MODE) ); )

   F77_IMPORT_INTEGER( INDF, *indf );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

F77_SUBROUTINE(ndg_ndfcr)( INTEGER(IGRP), INTEGER(INDEX), CHARACTER(FTYPE), INTEGER(NDIM),
			   INTEGER(LBND), INTEGER(UBND), INTEGER(INDF), INTEGER(STATUS)
			   TRAIL(FTYPE) );

void ndgNdfcr( const Grp* igrp, size_t index, const char ftype[], int ndim,
	       const int lbnd[], const int ubnd[], int * indf, int * status ) {

  DECLARE_INTEGER(IGRP);
  DECLARE_INTEGER(INDEX);
  DECLARE_CHARACTER(FTYPE, NDF__SZFTP);
  DECLARE_INTEGER(NDIM);
  DECLARE_INTEGER_ARRAY_DYN(LBND);
  DECLARE_INTEGER_ARRAY_DYN(UBND);
  DECLARE_INTEGER(INDF);
  DECLARE_INTEGER(STATUS);

  F77_CREATE_INTEGER_ARRAY( LBND, ndim );
  F77_EXPORT_INTEGER_ARRAY( lbnd, LBND, ndim );
  F77_CREATE_INTEGER_ARRAY( UBND, ndim );
  F77_EXPORT_INTEGER_ARRAY( ubnd, UBND, ndim );
  IGRP = grpC2F( igrp, status );
  F77_EXPORT_INTEGER( index, INDEX );
  F77_EXPORT_CHARACTER( ftype, FTYPE, NDF__SZFTP );
  F77_EXPORT_INTEGER( ndim, NDIM );
  F77_EXPORT_INTEGER(*status, STATUS );

  F77_LOCK( F77_CALL(ndg_ndfcr)( INTEGER_ARG(&IGRP), INTEGER_ARG(&INDEX), CHARACTER_ARG(FTYPE),
		       INTEGER_ARG(&NDIM), INTEGER_ARRAY_ARG(LBND), INTEGER_ARRAY_ARG(UBND),
		       INTEGER_ARG(&INDF), INTEGER_ARG(&STATUS)
		       TRAIL_ARG(FTYPE) ); )

  F77_IMPORT_INTEGER( STATUS, *status );
  F77_IMPORT_INTEGER( INDF, *indf );

}


F77_SUBROUTINE(ndg_ndfpr)( INTEGER(INDF1), CHARACTER(CLIST), INTEGER(IGRP), INTEGER(INDEX), INTEGER(INDF2), INTEGER(STATUS) TRAIL(CLIST));

void ndgNdfpr( int indf1, const char clist[], const Grp *igrp, size_t index, int * indf2, int * status) {

  DECLARE_INTEGER(INDF1);
  DECLARE_CHARACTER(CLIST, 128);
  DECLARE_INTEGER(IGRP);
  DECLARE_INTEGER(INDEX);
  DECLARE_INTEGER(INDF2);
  DECLARE_INTEGER(STATUS);

  F77_EXPORT_INTEGER(indf1, INDF1);
  IGRP = grpC2F( igrp, status );
  F77_EXPORT_CHARACTER( clist, CLIST, 128);
  F77_EXPORT_INTEGER(index, INDEX);
  F77_EXPORT_INTEGER(*status, STATUS);

  F77_LOCK( F77_CALL(ndg_ndfpr)( INTEGER_ARG(&INDF1), CHARACTER_ARG(CLIST),
		       INTEGER_ARG(&IGRP), INTEGER_ARG(&INDEX),
		       INTEGER_ARG(&INDF2), INTEGER_ARG(&STATUS)
		       TRAIL_ARG(CLIST) ); )

  F77_IMPORT_INTEGER( STATUS, *status);
  F77_IMPORT_INTEGER( INDF2, *indf2);

}

F77_SUBROUTINE(ndg_ndfco)( INTEGER(INDF1), INTEGER(IGRP), INTEGER(INDEX), INTEGER(INDF2), INTEGER(STATUS));

void ndgNdfco( int indf1, const Grp *igrp, size_t index, int * indf2, int * status) {

  DECLARE_INTEGER(INDF1);
  DECLARE_INTEGER(IGRP);
  DECLARE_INTEGER(INDEX);
  DECLARE_INTEGER(INDF2);
  DECLARE_INTEGER(STATUS);

  F77_EXPORT_INTEGER(indf1, INDF1);
  IGRP = grpC2F( igrp, status );
  F77_EXPORT_INTEGER(index, INDEX);
  F77_EXPORT_INTEGER(*status, STATUS);

  F77_LOCK( F77_CALL(ndg_ndfco)( INTEGER_ARG(&INDF1),
		       INTEGER_ARG(&IGRP), INTEGER_ARG(&INDEX),
		       INTEGER_ARG(&INDF2), INTEGER_ARG(&STATUS) ); )

  F77_IMPORT_INTEGER( STATUS, *status);
  F77_IMPORT_INTEGER( INDF2, *indf2);

}

F77_SUBROUTINE(ndg_asexp)( CHARACTER(GRPEXP), LOGICAL(VERB), INTEGER(IGRP1), INTEGER(IGRP2),
			   INTEGER(SIZE), LOGICAL(FLAG), INTEGER(STATUS) TRAIL(GRPEXP) );

void ndgAsexp( const char grpexp[], int verb, const Grp *igrp1, Grp ** igrp2, size_t *size, int * flag,
	       int *status ){
   DECLARE_INTEGER(IGRP1);
   DECLARE_INTEGER(IGRP2);
   DECLARE_INTEGER(SIZE);
   DECLARE_INTEGER(STATUS);
   DECLARE_LOGICAL(VERB);
   DECLARE_LOGICAL(FLAG);
   DECLARE_CHARACTER(GRPEXP, GRP__SZNAM);

   IGRP2 = grpC2F( *igrp2, status );
   IGRP1 = grpC2F( igrp1, status );
   if ( *status != SAI__OK ) return;

   F77_EXPORT_LOGICAL( verb, VERB );
   F77_EXPORT_CHARACTER( grpexp, GRPEXP, GRP__SZNAM );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(ndg_asexp)( CHARACTER_ARG(GRPEXP), LOGICAL_ARG(&VERB), INTEGER_ARG(&IGRP1),
                        INTEGER_ARG(&IGRP2),
                        INTEGER_ARG(&SIZE), LOGICAL_ARG(&FLAG),
                        INTEGER_ARG(&STATUS) TRAIL_ARG(GRPEXP) ); )

   F77_IMPORT_INTEGER( SIZE, *size );
   F77_IMPORT_LOGICAL( FLAG, *flag );

   F77_IMPORT_INTEGER( STATUS, *status );
   *igrp2 = (Grp *) grpF2C( IGRP2, status );

   return;
}



F77_SUBROUTINE(ndg_gtsup)( INTEGER(IGRP), INTEGER(I), CHARACTER_ARRAY(FIELDS),
                           INTEGER(STATUS) TRAIL(FIELDS) );

/* Note the addition of a "len" parameter following the "fields" array.
   This should be supplied equal to the allocated length of the shortest
   string for which a pointer has been supplied in "fields". This length
   should include room for the trailing null. */

void ndgGtsup( const Grp *grp, size_t i, char *const fields[6], size_t len, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(I);
   DECLARE_CHARACTER_ARRAY_DYN(FIELDS);
   DECLARE_INTEGER(STATUS);

   IGRP = grpC2F( grp, status );

   F77_EXPORT_INTEGER( i, I );
   F77_CREATE_CHARACTER_ARRAY(FIELDS,len-1,6);
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(ndg_gtsup)( INTEGER_ARG(&IGRP),
                        INTEGER_ARG(&I),
                        CHARACTER_ARRAY_ARG(FIELDS),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(FIELDS) ); )

   F77_IMPORT_CHARACTER_ARRAY_P(FIELDS,FIELDS_length,fields,
                                len,6);
   F77_FREE_CHARACTER(FIELDS);
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

F77_SUBROUTINE(ndg_cpsup)( INTEGER(IGRP1), INTEGER(I), INTEGER(IGRP2),
                           INTEGER(STATUS) );

void ndgCpsup( const Grp *igrp1, size_t i, Grp *igrp2, int * status ) {
   DECLARE_INTEGER(IGRP1);
   DECLARE_INTEGER(I);
   DECLARE_INTEGER(IGRP2);
   DECLARE_INTEGER(STATUS);

   IGRP1 = grpC2F( igrp1, status );
   F77_EXPORT_INTEGER(i, I );
   IGRP2 = grpC2F( igrp2, status );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(ndg_cpsup)( INTEGER_ARG(&IGRP1), INTEGER_ARG(&I),
                        INTEGER_ARG(&IGRP2), INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

F77_SUBROUTINE(ndg_endpv)( CHARACTER(CREATR), INTEGER(STATUS) TRAIL(CREATR) );

void ndgEndpv( const char *creator, int * status ) {
  DECLARE_CHARACTER_DYN(CREATR);
  DECLARE_INTEGER(STATUS);
  F77_EXPORT_INTEGER(*status, STATUS );
  F77_CREATE_EXPORT_CHARACTER( creator, CREATR );
  F77_LOCK( F77_CALL(ndg_endpv)( CHARACTER_ARG(CREATR), INTEGER_ARG(&STATUS)
                       TRAIL_ARG(CREATR) ); )
  F77_IMPORT_INTEGER( STATUS, *status );
  F77_FREE_CHARACTER( CREATR );
}

F77_SUBROUTINE(ndg_begpv)( INTEGER(STATUS) );

void ndgBegpv( int * status ) {
  DECLARE_INTEGER(STATUS);
  F77_EXPORT_INTEGER(*status, STATUS );
  F77_LOCK( F77_CALL(ndg_begpv)( INTEGER_ARG(&STATUS) ); )
  F77_IMPORT_INTEGER( STATUS, *status );
}


F77_SUBROUTINE(ndg_copy)( INTEGER(IGRP1), INTEGER(INDXLO),
			  INTEGER(INDXHI), LOGICAL(REJECT),
			  INTEGER(IGRP2), INTEGER(STATUS));

Grp * ndgCopy( const Grp* grp1, size_t indxlo, size_t indxhi, int reject,
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

  F77_LOCK( F77_CALL(ndg_copy)( INTEGER_ARG(&IGRP1), INTEGER_ARG(&INDXLO),
		      INTEGER_ARG(&INDXHI), LOGICAL_ARG(&REJECT),
		      INTEGER_ARG(&IGRP2), INTEGER_ARG(&STATUS)); )

  F77_IMPORT_INTEGER( STATUS, *status );

  ret = grpF2C( IGRP2, status );
  return ret;
}

F77_SUBROUTINE(ndg_crexp)( CHARACTER(GRPEXP), INTEGER(IGRP0), INTEGER(IGRP),
			   INTEGER(SIZE), LOGICAL(FLAG), INTEGER(STATUS) TRAIL(GRPEXP) );

void ndgCrexp( const char grpexp[], const Grp *igrp0, Grp ** igrp, size_t *size, int * flag,
	       int *status ){
   DECLARE_INTEGER(IGRP0);
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(SIZE);
   DECLARE_INTEGER(STATUS);
   DECLARE_LOGICAL(FLAG);
   DECLARE_CHARACTER(GRPEXP, GRP__SZNAM);

   IGRP = grpC2F( *igrp, status );
   IGRP0 = grpC2F( igrp0, status );
   if ( *status != SAI__OK ) return;

   F77_EXPORT_CHARACTER( grpexp, GRPEXP, GRP__SZNAM );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(ndg_crexp)( CHARACTER_ARG(GRPEXP), INTEGER_ARG(&IGRP0), INTEGER_ARG(&IGRP),
                        INTEGER_ARG(&SIZE), LOGICAL_ARG(&FLAG),
                        INTEGER_ARG(&STATUS) TRAIL_ARG(GRPEXP) ); )

   F77_IMPORT_INTEGER( SIZE, *size );
   F77_IMPORT_LOGICAL( FLAG, *flag );

   F77_IMPORT_INTEGER( STATUS, *status );
   *igrp = (Grp *) grpF2C( IGRP, status );

   return;
}

F77_SUBROUTINE(ndg_beggh)( INTEGER(STATUS) );

void ndgBeggh( int * status ) {
  DECLARE_INTEGER(STATUS);
  F77_EXPORT_INTEGER(*status, STATUS );
  F77_LOCK( F77_CALL(ndg_beggh)( INTEGER_ARG(&STATUS) ); )
  F77_IMPORT_INTEGER( STATUS, *status );
}


F77_SUBROUTINE(ndg_endgh)( INTEGER(STATUS) );

void ndgEndgh( int * status ) {
  DECLARE_INTEGER(STATUS);
  F77_EXPORT_INTEGER(*status, STATUS );
  F77_LOCK( F77_CALL(ndg_endgh)( INTEGER_ARG(&STATUS) ); )
  F77_IMPORT_INTEGER( STATUS, *status );
}

F77_SUBROUTINE(ndg_hwrgh)( INTEGER(INDF), INTEGER(STATUS) );

void ndgHwrgh( int indf, int *status ) {
  DECLARE_INTEGER(INDF);
  DECLARE_INTEGER(STATUS);
  F77_EXPORT_INTEGER( indf, INDF );
  F77_EXPORT_INTEGER( *status, STATUS );
  F77_LOCK( F77_CALL(ndg_hwrgh)( INTEGER_ARG(&INDF), INTEGER_ARG(&STATUS) ); )
  F77_IMPORT_INTEGER( STATUS, *status );
}

F77_SUBROUTINE(ndg_hltgh)( LOGICAL(NEW), LOGICAL(OLD), INTEGER(STATUS) );

void ndgHltgh( int new, int *old, int *status ){
   DECLARE_LOGICAL(NEW);
   DECLARE_LOGICAL(OLD);
   DECLARE_INTEGER(STATUS);

   if ( *status != SAI__OK ) return;

   F77_EXPORT_LOGICAL( new, NEW );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(ndg_hltgh)( LOGICAL_ARG(&NEW), LOGICAL_ARG(&OLD),
                        INTEGER_ARG(&STATUS) ); )

   if( old ) F77_IMPORT_LOGICAL( OLD, *old );
   F77_IMPORT_INTEGER( STATUS, *status );
}

F77_SUBROUTINE(ndg_hltpv)( LOGICAL(NEW), LOGICAL(OLD), INTEGER(STATUS) );

void ndgHltpv( int new, int *old, int *status ){
   DECLARE_LOGICAL(NEW);
   DECLARE_LOGICAL(OLD);
   DECLARE_INTEGER(STATUS);

   if ( *status != SAI__OK ) return;

   F77_EXPORT_LOGICAL( new, NEW );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(ndg_hltpv)( LOGICAL_ARG(&NEW), LOGICAL_ARG(&OLD),
                        INTEGER_ARG(&STATUS) ); )

   if( old ) F77_IMPORT_LOGICAL( OLD, *old );
   F77_IMPORT_INTEGER( STATUS, *status );
}

F77_SUBROUTINE(ndg_moreg)( INTEGER(INDF), INTEGER(IGRP), INTEGER(SIZE),
                           INTEGER(STATUS) );

void ndgMoreg( int indf, Grp **igrp, size_t *size, int *status ){
   DECLARE_INTEGER(INDF);
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(SIZE);
   DECLARE_INTEGER(STATUS);

   IGRP = grpC2F( *igrp, status );
   if ( *status != SAI__OK ) return;

   F77_EXPORT_INTEGER( indf, INDF );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(ndg_moreg)( INTEGER_ARG(&INDF), INTEGER_ARG(&IGRP),
                                  INTEGER_ARG(&SIZE), INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( SIZE, *size );
   F77_IMPORT_INTEGER( STATUS, *status );
   *igrp = (Grp *) grpF2C( IGRP, status );

   return;
}

