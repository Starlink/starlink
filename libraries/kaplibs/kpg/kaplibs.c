/*
*+
*  Name:
*     kaplibs.c

*  Purpose:
*     Implement the C interface to the standalone routines in the KAPLIBS
*     library.

*  Description:
*     This module implements C-callable wrappers for the public non-ADAM
*     routines in the KAPLIBS library. The interface to these wrappers
*     is defined in kaplibs.h.

*  Notes:
*     - Given the size of the KAPLIBS library, providing a complete C
*     interface is probably not worth the effort. Instead, I suggest that
*     people who want to use KAPLIBS from C extend this file (and
*     kaplibs.h) to include any functions which they need but which are
*     not already included.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2005-2007 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2008-2010 Science & Technology Facilities Council.
*     Copyright (C) 2009 University of British Columbia.
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
*     EC: Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     29-SEP-2005 (DSB):
*        Original version.
*     03-NOV-2005 (TIMJ):
*        Update GRP interface
*     30-JUN-2006 (TIMJ):
*        Add kpg1_statd
*     10-JUL-2006 (DSB):
*        Add kpg1_wwrt and kpg1_wread.
*     14-AUG-2006 (DSB):
*        Added kpg1_mxmnd, kpg1_mxmnr and kpg1_mxmni.
*     5-FEB-2007 (DSB):
*        Added kpg1_gtwcs.
*     7-FEB-2007 (DSB):
*        Added kpg1_medur.
*     10-JUL-2008 (TIMJ):
*        Use starmem
*     15-JUL-2008 (TIMJ):
*        const and size_t to match Grp
*     24-JUL-2008 (TIMJ):
*        Use more robust F77_CREATE_EXPORT_CHARACTER
*     21-AUG-2008 (TIMJ):
*        Add kpgStati
*     2009 August 25 (MJC/EC):
*        Removed IRQ interfaces.
*     5-NOV-2009 (DSB):
*        Add kpg1Loctd.
*     13-NOV-2009 (TIMJ):
*        Add kpg1_pixsc
*     2010 August 6 (MJC):
*        Add kpg1_filli.
*     2011-08-22 (TIMJ):
*        kpg1GhstX has a new API
*     {enter_further_changes_here}

*-
*/

/* Header files. */
/* ============= */
#include "f77.h"
#include "sae_par.h"
#include "mers.h"
#include "star/grp.h"
#include "star/mem.h"
#include "star/hds_fortran.h"
#include "kaplibs.h"
#include "kaplibs_private.h"
#include <string.h>

/* Wrapper function implementations. */
/* ================================= */

F77_SUBROUTINE(kpg1_filli)( INTEGER(VALUE),
                            INTEGER(EL),
                            INTEGER_ARRAY(ARRAY),
                            INTEGER(STATUS) );

void kpg1Filli( int value, int el, int *array, int *status ){
   DECLARE_INTEGER(VALUE);
   DECLARE_INTEGER(EL);
   DECLARE_INTEGER_ARRAY_DYN(ARRAY);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_INTEGER_ARRAY( ARRAY, el );

   F77_EXPORT_INTEGER( value, VALUE );
   F77_EXPORT_INTEGER( el, EL );
   F77_ASSOC_INTEGER_ARRAY( ARRAY, array );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_filli)( INTEGER_ARG(&VALUE),
                         INTEGER_ARG(&EL),
                         INTEGER_ARRAY_ARG(ARRAY),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_INTEGER_ARRAY( ARRAY, array, el );
   F77_FREE_INTEGER( ARRAY );
}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_fillr)( REAL(VALUE),
                            INTEGER(EL),
                            REAL_ARRAY(ARRAY),
                            INTEGER(STATUS) );

void kpg1Fillr( float value, int el, float *array, int *status ){
   DECLARE_REAL(VALUE);
   DECLARE_INTEGER(EL);
   DECLARE_REAL_ARRAY_DYN(ARRAY);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_REAL_ARRAY( ARRAY, el );

   F77_EXPORT_REAL( value, VALUE );
   F77_EXPORT_INTEGER( el, EL );
   F77_ASSOC_REAL_ARRAY( ARRAY, array );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_fillr)( REAL_ARG(&VALUE),
                         INTEGER_ARG(&EL),
                         REAL_ARRAY_ARG(ARRAY),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_REAL_ARRAY( ARRAY, array, el );
   F77_FREE_REAL( ARRAY );
}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_gausr)( REAL(SIGMA),
                            INTEGER(IBOX),
                            LOGICAL(SAMBAD),
                            REAL(WLIM),
                            INTEGER(NX),
                            INTEGER(NY),
                            LOGICAL(BAD),
                            LOGICAL(VAR),
                            REAL_ARRAY(A),
                            REAL_ARRAY(B),
                            LOGICAL(BADOUT),
                            REAL_ARRAY(WEIGHT),
                            REAL_ARRAY(AMAR),
                            REAL_ARRAY(WMAR),
                            INTEGER(STATUS) );

void kpg1Gausr( float sigma, int ibox, int sambad, float wlim, int nx,
                int ny, int bad, int var, float *a, float *b, int *badout,
                float *weight, float *amar, float *wmar, int *status ){

   DECLARE_REAL(SIGMA);
   DECLARE_INTEGER(IBOX);
   DECLARE_LOGICAL(SAMBAD);
   DECLARE_REAL(WLIM);
   DECLARE_INTEGER(NX);
   DECLARE_INTEGER(NY);
   DECLARE_LOGICAL(BAD);
   DECLARE_LOGICAL(VAR);
   DECLARE_REAL_ARRAY_DYN(A);
   DECLARE_REAL_ARRAY_DYN(B);
   DECLARE_LOGICAL(BADOUT);
   DECLARE_REAL_ARRAY_DYN(WEIGHT);
   DECLARE_REAL_ARRAY_DYN(AMAR);
   DECLARE_REAL_ARRAY_DYN(WMAR);
   DECLARE_INTEGER(STATUS);

   int nxy, nw;
   nxy = nx*ny;
   nw = 2*ibox + 1;

   F77_CREATE_REAL_ARRAY( A, nxy );
   F77_CREATE_REAL_ARRAY( B, nxy );
   F77_CREATE_REAL_ARRAY( WEIGHT, nw );
   F77_CREATE_REAL_ARRAY( AMAR, nx );
   F77_CREATE_REAL_ARRAY( WMAR, nx );

   F77_EXPORT_REAL( sigma, SIGMA );
   F77_EXPORT_INTEGER( ibox, IBOX );
   F77_EXPORT_LOGICAL( sambad, SAMBAD );
   F77_EXPORT_REAL( wlim, WLIM );
   F77_EXPORT_INTEGER( nx, NX );
   F77_EXPORT_INTEGER( ny, NY );
   F77_EXPORT_LOGICAL( bad, BAD );
   F77_EXPORT_LOGICAL( var, VAR );
   F77_EXPORT_REAL_ARRAY( a, A, nxy );
   F77_ASSOC_REAL_ARRAY( B, b );
   F77_ASSOC_REAL_ARRAY( WEIGHT, weight );
   F77_ASSOC_REAL_ARRAY( AMAR, amar );
   F77_ASSOC_REAL_ARRAY( WMAR, wmar );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_gausr)( REAL_ARG(&SIGMA),
                         INTEGER_ARG(&IBOX),
                         LOGICAL_ARG(&SAMBAD),
                         REAL_ARG(&WLIM),
                         INTEGER_ARG(&NX),
                         INTEGER_ARG(&NY),
                         LOGICAL_ARG(&BAD),
                         LOGICAL_ARG(&VAR),
                         REAL_ARRAY_ARG(A),
                         REAL_ARRAY_ARG(B),
                         LOGICAL_ARG(&BADOUT),
                         REAL_ARRAY_ARG(WEIGHT),
                         REAL_ARRAY_ARG(AMAR),
                         REAL_ARRAY_ARG(WMAR),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_LOGICAL( BADOUT, *badout );
   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_REAL_ARRAY( B, b, nxy );

   F77_FREE_REAL( A );
   F77_FREE_REAL( B );
   F77_FREE_REAL( WEIGHT );
   F77_FREE_REAL( AMAR );
   F77_FREE_REAL( BMAR );
}

/* ------------------------------- */

void kpg1Kygrp( AstKeyMap *keymap, Grp **igrp, int *status ){
  kpg1Kygp1( keymap, igrp, NULL, status );
}

/* ------------------------------- */

void kpg1Kymap( const Grp *igrp, AstKeyMap **keymap, int *status ){
  kpg1Kymp1( igrp, keymap, status );
}

/* ------------------------------- */

/* NB The supplied axis indices should be one based, not zero based. */

F77_SUBROUTINE(kpg1_manir)( INTEGER(NDIMI),
                            INTEGER_ARRAY(DIMI),
                            REAL_ARRAY(IN),
                            INTEGER(NDIMO),
                            INTEGER_ARRAY(DIMO),
                            INTEGER_ARRAY(AXES),
                            INTEGER_ARRAY(COLOFF),
                            INTEGER_ARRAY(EXPOFF),
                            REAL_ARRAY(OUT),
                            INTEGER(STATUS) );

void kpg1Manir( int ndimi, int *dimi, float *in, int ndimo, int *dimo,
                int *axes, int *coloff, int *expoff, float *out, int *status ){

   DECLARE_INTEGER(NDIMI);
   DECLARE_INTEGER_ARRAY_DYN(DIMI);
   DECLARE_REAL_ARRAY_DYN(IN);
   DECLARE_INTEGER(NDIMO);
   DECLARE_INTEGER_ARRAY_DYN(DIMO);
   DECLARE_INTEGER_ARRAY_DYN(AXES);
   DECLARE_INTEGER_ARRAY_DYN(COLOFF);
   DECLARE_INTEGER_ARRAY_DYN(EXPOFF);
   DECLARE_REAL_ARRAY_DYN(OUT);
   DECLARE_INTEGER(STATUS);

   int ncoloff, nexpoff, i, j, use, nin, nout;

   nin = 1;
   ncoloff = 1;
   for( i = 0; i < ndimi; i++ ) {
      use = 1;
      for( j = 0; j < ndimo; j++ ) {
         if( axes[ j ] == i ) {
            use = 0;
            break;
         }
      }
      if( use ) ncoloff *= dimi[ i ];
      nin *= dimi[ i ];
   }

   nout = 1;
   nexpoff = 1;
   for( j = 0; j < ndimo; j++ ) {
      if( axes[ j ] == 0 ) nexpoff *= dimo[ j ];
      nout *= dimo[ j ];
   }

   F77_CREATE_INTEGER_ARRAY( DIMI, ndimi );
   F77_CREATE_REAL_ARRAY( IN, nin );
   F77_CREATE_INTEGER_ARRAY( DIMO, ndimo );
   F77_CREATE_INTEGER_ARRAY( AXES, ndimo );
   F77_CREATE_INTEGER_ARRAY( COLOFF, ncoloff );
   F77_CREATE_INTEGER_ARRAY( EXPOFF, nexpoff );
   F77_CREATE_REAL_ARRAY( OUT, nout );

   F77_EXPORT_INTEGER( ndimi, NDIMI );
   F77_EXPORT_INTEGER_ARRAY( dimi, DIMI, ndimi );
   F77_EXPORT_REAL_ARRAY( in, IN, nin );
   F77_EXPORT_INTEGER( ndimo, NDIMO );
   F77_EXPORT_INTEGER_ARRAY( dimo, DIMO, ndimo );
   F77_EXPORT_INTEGER_ARRAY( axes, AXES, ndimo );
   F77_ASSOC_INTEGER_ARRAY( COLOFF, coloff );
   F77_ASSOC_INTEGER_ARRAY( EXPOFF, expoff );
   F77_ASSOC_REAL_ARRAY( OUT, out );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_manir)( INTEGER_ARG(&NDIMI),
                         INTEGER_ARRAY_ARG(DIMI),
                         REAL_ARRAY_ARG(IN),
                         INTEGER_ARG(&NDIMO),
                         INTEGER_ARRAY_ARG(DIMO),
                         INTEGER_ARRAY_ARG(AXES),
                         INTEGER_ARRAY_ARG(COLOFF),
                         INTEGER_ARRAY_ARG(EXPOFF),
                         REAL_ARRAY_ARG(OUT),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_REAL_ARRAY( OUT, out, nout );

   F77_FREE_INTEGER( DIMI );
   F77_FREE_REAL( IN );
   F77_FREE_INTEGER( DIMO );
   F77_FREE_INTEGER( AXES );
   F77_FREE_INTEGER( COLOFF );
   F77_FREE_INTEGER( EXPOFF );
   F77_FREE_REAL( OUT );

}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_pseed)( INTEGER(STATUS) );

void kpg1Pseed( int *status ){
   DECLARE_INTEGER(STATUS);
   F77_EXPORT_INTEGER( *status, STATUS );
   F77_LOCK( F77_CALL(kpg1_pseed)( INTEGER_ARG(&STATUS) ); )
   F77_IMPORT_INTEGER( STATUS, *status );
}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_statd)( LOGICAL(BAD), INTEGER(EL), DOUBLE_ARRAY(DATA),
			    INTEGER(NCLIP), REAL_ARRAY(CLIP), INTEGER(NGOOD),
			    INTEGER(IMIN), DOUBLE(DMIN), INTEGER(IMAX),
			    DOUBLE(DMAX), DOUBLE(SUM), DOUBLE(MEAN), DOUBLE(STDEV),
			    INTEGER(NGOODC), INTEGER(IMINC), DOUBLE(DMINC),
			    INTEGER(IMAXC), DOUBLE(DMAXC), DOUBLE(SUMC), DOUBLE(MEANC),
			    DOUBLE(STDEVC), INTEGER(STATUS) );

void kpgStatd( int bad, int el, const double data[], int nclip, const float clip[],
	       int * ngood, int *imin, double * dmin, int * imax,
	       double * dmax, double * sum, double * mean, double * stdev,
	       int * ngoodc, int * iminc, double * dminc, int * imaxc, double * dmaxc,
	       double * sumc, double * meanc, double * stdevc, int * status ) {

  DECLARE_LOGICAL(BAD);
  DECLARE_INTEGER(EL);
  DECLARE_DOUBLE_ARRAY_DYN(DATA);
  DECLARE_INTEGER(NCLIP);
  DECLARE_REAL_ARRAY_DYN(CLIP);
  DECLARE_INTEGER(NGOOD);
  DECLARE_INTEGER(IMIN);
  DECLARE_DOUBLE(DMIN);
  DECLARE_INTEGER(IMAX);
  DECLARE_DOUBLE(DMAX);
  DECLARE_DOUBLE(SUM);
  DECLARE_DOUBLE(MEAN);
  DECLARE_DOUBLE(STDEV);
  DECLARE_INTEGER(NGOODC);
  DECLARE_INTEGER(IMINC);
  DECLARE_DOUBLE(DMINC);
  DECLARE_INTEGER(IMAXC);
  DECLARE_DOUBLE(DMAXC);
  DECLARE_DOUBLE(SUMC);
  DECLARE_DOUBLE(MEANC);
  DECLARE_DOUBLE(STDEVC);
  DECLARE_INTEGER(STATUS);

  F77_CREATE_DOUBLE_ARRAY( DATA, el );
  F77_CREATE_REAL_ARRAY( CLIP, nclip );

  F77_EXPORT_LOGICAL( bad, BAD );
  F77_EXPORT_INTEGER( el, EL );
  F77_EXPORT_DOUBLE_ARRAY( data, DATA, el );
  F77_EXPORT_INTEGER( nclip, NCLIP );
  F77_EXPORT_REAL_ARRAY( clip, CLIP, nclip );
  F77_EXPORT_INTEGER( *status, STATUS );

  F77_LOCK( F77_CALL(kpg1_statd)( LOGICAL_ARG(&BAD),
			INTEGER_ARG(&EL),
			DOUBLE_ARRAY_ARG(DATA),
			INTEGER_ARG(&NCLIP),
			REAL_ARRAY_ARG(CLIP),
			INTEGER_ARG(&NGOOD),
			INTEGER_ARG(&IMIN),
			DOUBLE_ARG(&DMIN),
			INTEGER_ARG(&IMAX),
			DOUBLE_ARG(&DMAX),
			DOUBLE_ARG(&SUM),
			DOUBLE_ARG(&MEAN),
			DOUBLE_ARG(&STDEV),
			INTEGER_ARG(&NGOODC),
			INTEGER_ARG(&IMINC),
			DOUBLE_ARG(&DMINC),
			INTEGER_ARG(&IMAXC),
			DOUBLE_ARG(&DMAXC),
			DOUBLE_ARG(&SUMC),
			DOUBLE_ARG(&MEANC),
			DOUBLE_ARG(&STDEVC),
			INTEGER_ARG(&STATUS) ); )

  F77_IMPORT_INTEGER( STATUS, *status );
  F77_IMPORT_INTEGER( NGOOD, *ngood );
  F77_IMPORT_INTEGER( IMIN, *imin );
  F77_IMPORT_DOUBLE( DMIN, *dmin );
  F77_IMPORT_INTEGER( IMAX, *imax );
  F77_IMPORT_DOUBLE( DMAX, *dmax );
  F77_IMPORT_DOUBLE( SUM, *sum );
  F77_IMPORT_DOUBLE( MEAN, *mean );
  F77_IMPORT_DOUBLE( STDEV, *stdev );
  F77_IMPORT_INTEGER( NGOODC, *ngoodc );
  F77_IMPORT_INTEGER( IMINC, *iminc );
  F77_IMPORT_DOUBLE( DMINC, *dminc );
  F77_IMPORT_INTEGER( IMAXC, *imaxc );
  F77_IMPORT_DOUBLE( DMAXC, *dmaxc );
  F77_IMPORT_DOUBLE( SUMC, *sumc );
  F77_IMPORT_DOUBLE( MEANC, *meanc );
  F77_IMPORT_DOUBLE( STDEVC, *stdevc );

  F77_FREE_DOUBLE( DATA );
  F77_FREE_REAL( CLIP );

}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_stati)( LOGICAL(BAD), INTEGER(EL), INTEGER_ARRAY(DATA),
			    INTEGER(NCLIP), REAL_ARRAY(CLIP), INTEGER(NGOOD),
			    INTEGER(IMIN), DOUBLE(DMIN), INTEGER(IMAX),
			    DOUBLE(DMAX), DOUBLE(SUM), DOUBLE(MEAN), DOUBLE(STDEV),
			    INTEGER(NGOODC), INTEGER(IMINC), DOUBLE(DMINC),
			    INTEGER(IMAXC), DOUBLE(DMAXC), DOUBLE(SUMC), DOUBLE(MEANC),
			    DOUBLE(STDEVC), INTEGER(STATUS) );

void kpgStati( int bad, int el, const int data[], int nclip, const float clip[],
	       int * ngood, int *imin, double * dmin, int * imax,
	       double * dmax, double * sum, double * mean, double * stdev,
	       int * ngoodc, int * iminc, double * dminc, int * imaxc, double * dmaxc,
	       double * sumc, double * meanc, double * stdevc, int * status ) {

  DECLARE_LOGICAL(BAD);
  DECLARE_INTEGER(EL);
  DECLARE_INTEGER_ARRAY_DYN(DATA);
  DECLARE_INTEGER(NCLIP);
  DECLARE_REAL_ARRAY_DYN(CLIP);
  DECLARE_INTEGER(NGOOD);
  DECLARE_INTEGER(IMIN);
  DECLARE_DOUBLE(DMIN);
  DECLARE_INTEGER(IMAX);
  DECLARE_DOUBLE(DMAX);
  DECLARE_DOUBLE(SUM);
  DECLARE_DOUBLE(MEAN);
  DECLARE_DOUBLE(STDEV);
  DECLARE_INTEGER(NGOODC);
  DECLARE_INTEGER(IMINC);
  DECLARE_DOUBLE(DMINC);
  DECLARE_INTEGER(IMAXC);
  DECLARE_DOUBLE(DMAXC);
  DECLARE_DOUBLE(SUMC);
  DECLARE_DOUBLE(MEANC);
  DECLARE_DOUBLE(STDEVC);
  DECLARE_INTEGER(STATUS);

  F77_CREATE_INTEGER_ARRAY( DATA, el );
  F77_CREATE_REAL_ARRAY( CLIP, nclip );

  F77_EXPORT_LOGICAL( bad, BAD );
  F77_EXPORT_INTEGER( el, EL );
  F77_EXPORT_INTEGER_ARRAY( data, DATA, el );
  F77_EXPORT_INTEGER( nclip, NCLIP );
  F77_EXPORT_REAL_ARRAY( clip, CLIP, nclip );
  F77_EXPORT_INTEGER( *status, STATUS );

  F77_LOCK( F77_CALL(kpg1_stati)( LOGICAL_ARG(&BAD),
			INTEGER_ARG(&EL),
			INTEGER_ARRAY_ARG(DATA),
			INTEGER_ARG(&NCLIP),
			REAL_ARRAY_ARG(CLIP),
			INTEGER_ARG(&NGOOD),
			INTEGER_ARG(&IMIN),
			DOUBLE_ARG(&DMIN),
			INTEGER_ARG(&IMAX),
			DOUBLE_ARG(&DMAX),
			DOUBLE_ARG(&SUM),
			DOUBLE_ARG(&MEAN),
			DOUBLE_ARG(&STDEV),
			INTEGER_ARG(&NGOODC),
			INTEGER_ARG(&IMINC),
			DOUBLE_ARG(&DMINC),
			INTEGER_ARG(&IMAXC),
			DOUBLE_ARG(&DMAXC),
			DOUBLE_ARG(&SUMC),
			DOUBLE_ARG(&MEANC),
			DOUBLE_ARG(&STDEVC),
			INTEGER_ARG(&STATUS) ); )

  F77_IMPORT_INTEGER( STATUS, *status );
  F77_IMPORT_INTEGER( NGOOD, *ngood );
  F77_IMPORT_INTEGER( IMIN, *imin );
  F77_IMPORT_DOUBLE( DMIN, *dmin );
  F77_IMPORT_INTEGER( IMAX, *imax );
  F77_IMPORT_DOUBLE( DMAX, *dmax );
  F77_IMPORT_DOUBLE( SUM, *sum );
  F77_IMPORT_DOUBLE( MEAN, *mean );
  F77_IMPORT_DOUBLE( STDEV, *stdev );
  F77_IMPORT_INTEGER( NGOODC, *ngoodc );
  F77_IMPORT_INTEGER( IMINC, *iminc );
  F77_IMPORT_DOUBLE( DMINC, *dminc );
  F77_IMPORT_INTEGER( IMAXC, *imaxc );
  F77_IMPORT_DOUBLE( DMAXC, *dmaxc );
  F77_IMPORT_DOUBLE( SUMC, *sumc );
  F77_IMPORT_DOUBLE( MEANC, *meanc );
  F77_IMPORT_DOUBLE( STDEVC, *stdevc );

  F77_FREE_DOUBLE( DATA );
  F77_FREE_REAL( CLIP );

}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_stat8d)( LOGICAL(BAD), INTEGER8(EL), DOUBLE_ARRAY(DATA),
 			    INTEGER(NCLIP), REAL_ARRAY(CLIP), INTEGER8(NGOOD),
 			    INTEGER8(IMIN), DOUBLE(DMIN), INTEGER8(IMAX),
 			    DOUBLE(DMAX), DOUBLE(SUM), DOUBLE(MEAN), DOUBLE(STDEV),
 			    INTEGER8(NGOODC), INTEGER8(IMINC), DOUBLE(DMINC),
			    INTEGER8(IMAXC), DOUBLE(DMAXC), DOUBLE(SUMC), DOUBLE(MEANC),
			    DOUBLE(STDEVC), INTEGER(STATUS) );

void kpgStat8d( int bad, hdsdim el, const double data[], int nclip, const float clip[],
 	        hdsdim *ngood, hdsdim *imin, double *dmin, hdsdim *imax,
 	        double *dmax, double *sum, double *mean, double *stdev,
 	        hdsdim *ngoodc, hdsdim *iminc, double *dminc, hdsdim *imaxc,
                double *dmaxc, double *sumc, double *meanc, double *stdevc,
                int *status ) {

  DECLARE_LOGICAL(BAD);
  DECLARE_INTEGER8(EL);
  DECLARE_DOUBLE_ARRAY_DYN(DATA);
  DECLARE_INTEGER(NCLIP);
  DECLARE_REAL_ARRAY_DYN(CLIP);
  DECLARE_INTEGER8(NGOOD);
  DECLARE_INTEGER8(IMIN);
  DECLARE_DOUBLE(DMIN);
  DECLARE_INTEGER8(IMAX);
  DECLARE_DOUBLE(DMAX);
  DECLARE_DOUBLE(SUM);
  DECLARE_DOUBLE(MEAN);
  DECLARE_DOUBLE(STDEV);
  DECLARE_INTEGER8(NGOODC);
  DECLARE_INTEGER8(IMINC);
  DECLARE_DOUBLE(DMINC);
  DECLARE_INTEGER8(IMAXC);
  DECLARE_DOUBLE(DMAXC);
  DECLARE_DOUBLE(SUMC);
  DECLARE_DOUBLE(MEANC);
  DECLARE_DOUBLE(STDEVC);
  DECLARE_INTEGER(STATUS);

  F77_CREATE_DOUBLE_ARRAY( DATA, el );
  F77_CREATE_REAL_ARRAY( CLIP, nclip );

  F77_EXPORT_LOGICAL( bad, BAD );
  F77_EXPORT_INTEGER8( el, EL );
  F77_EXPORT_DOUBLE_ARRAY( data, DATA, el );
  F77_EXPORT_INTEGER( nclip, NCLIP );
  F77_EXPORT_REAL_ARRAY( clip, CLIP, nclip );
  F77_EXPORT_INTEGER( *status, STATUS );

  F77_LOCK( F77_CALL(kpg1_stat8d)( LOGICAL_ARG(&BAD),
			INTEGER8_ARG(&EL),
			DOUBLE_ARRAY_ARG(DATA),
			INTEGER_ARG(&NCLIP),
			REAL_ARRAY_ARG(CLIP),
			INTEGER8_ARG(&NGOOD),
			INTEGER8_ARG(&IMIN),
			DOUBLE_ARG(&DMIN),
			INTEGER8_ARG(&IMAX),
			DOUBLE_ARG(&DMAX),
			DOUBLE_ARG(&SUM),
			DOUBLE_ARG(&MEAN),
			DOUBLE_ARG(&STDEV),
			INTEGER8_ARG(&NGOODC),
			INTEGER8_ARG(&IMINC),
			DOUBLE_ARG(&DMINC),
			INTEGER8_ARG(&IMAXC),
			DOUBLE_ARG(&DMAXC),
			DOUBLE_ARG(&SUMC),
			DOUBLE_ARG(&MEANC),
			DOUBLE_ARG(&STDEVC),
			INTEGER_ARG(&STATUS) ); )

  F77_IMPORT_INTEGER( STATUS, *status );
  F77_IMPORT_INTEGER8( NGOOD, *ngood );
  F77_IMPORT_INTEGER8( IMIN, *imin );
  F77_IMPORT_DOUBLE( DMIN, *dmin );
  F77_IMPORT_INTEGER8( IMAX, *imax );
  F77_IMPORT_DOUBLE( DMAX, *dmax );
  F77_IMPORT_DOUBLE( SUM, *sum );
  F77_IMPORT_DOUBLE( MEAN, *mean );
  F77_IMPORT_DOUBLE( STDEV, *stdev );
  F77_IMPORT_INTEGER8( NGOODC, *ngoodc );
  F77_IMPORT_INTEGER8( IMINC, *iminc );
  F77_IMPORT_DOUBLE( DMINC, *dminc );
  F77_IMPORT_INTEGER8( IMAXC, *imaxc );
  F77_IMPORT_DOUBLE( DMAXC, *dmaxc );
  F77_IMPORT_DOUBLE( SUMC, *sumc );
  F77_IMPORT_DOUBLE( MEANC, *meanc );
  F77_IMPORT_DOUBLE( STDEVC, *stdevc );

  F77_FREE_DOUBLE( DATA );
  F77_FREE_REAL( CLIP );

}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_stat8i)( LOGICAL(BAD), INTEGER8(EL), INTEGER_ARRAY(DATA),
			    INTEGER(NCLIP), REAL_ARRAY(CLIP), INTEGER8(NGOOD),
			    INTEGER8(IMIN), DOUBLE(DMIN), INTEGER8(IMAX),
			    DOUBLE(DMAX), DOUBLE(SUM), DOUBLE(MEAN), DOUBLE(STDEV),
			    INTEGER8(NGOODC), INTEGER8(IMINC), DOUBLE(DMINC),
			    INTEGER8(IMAXC), DOUBLE(DMAXC), DOUBLE(SUMC), DOUBLE(MEANC),
			    DOUBLE(STDEVC), INTEGER(STATUS) );

void kpgStat8i( int bad, hdsdim el, const int data[], int nclip, const float clip[],
	       hdsdim *ngood, hdsdim *imin, double *dmin, hdsdim *imax,
	       double *dmax, double *sum, double *mean, double *stdev,
	       hdsdim *ngoodc, hdsdim *iminc, double *dminc, hdsdim *imaxc, double *dmaxc,
	       double *sumc, double *meanc, double *stdevc, int *status ) {

  DECLARE_LOGICAL(BAD);
  DECLARE_INTEGER8(EL);
  DECLARE_INTEGER_ARRAY_DYN(DATA);
  DECLARE_INTEGER(NCLIP);
  DECLARE_REAL_ARRAY_DYN(CLIP);
  DECLARE_INTEGER8(NGOOD);
  DECLARE_INTEGER8(IMIN);
  DECLARE_DOUBLE(DMIN);
  DECLARE_INTEGER8(IMAX);
  DECLARE_DOUBLE(DMAX);
  DECLARE_DOUBLE(SUM);
  DECLARE_DOUBLE(MEAN);
  DECLARE_DOUBLE(STDEV);
  DECLARE_INTEGER8(NGOODC);
  DECLARE_INTEGER8(IMINC);
  DECLARE_DOUBLE(DMINC);
  DECLARE_INTEGER8(IMAXC);
  DECLARE_DOUBLE(DMAXC);
  DECLARE_DOUBLE(SUMC);
  DECLARE_DOUBLE(MEANC);
  DECLARE_DOUBLE(STDEVC);
  DECLARE_INTEGER(STATUS);

  F77_CREATE_INTEGER_ARRAY( DATA, el );
  F77_CREATE_REAL_ARRAY( CLIP, nclip );

  F77_EXPORT_LOGICAL( bad, BAD );
  F77_EXPORT_INTEGER8( el, EL );
  F77_EXPORT_INTEGER_ARRAY( data, DATA, el );
  F77_EXPORT_INTEGER( nclip, NCLIP );
  F77_EXPORT_REAL_ARRAY( clip, CLIP, nclip );
  F77_EXPORT_INTEGER( *status, STATUS );

  F77_LOCK( F77_CALL(kpg1_stat8i)( LOGICAL_ARG(&BAD),
			INTEGER8_ARG(&EL),
			INTEGER_ARRAY_ARG(DATA),
			INTEGER_ARG(&NCLIP),
			REAL_ARRAY_ARG(CLIP),
			INTEGER8_ARG(&NGOOD),
			INTEGER8_ARG(&IMIN),
			DOUBLE_ARG(&DMIN),
			INTEGER8_ARG(&IMAX),
			DOUBLE_ARG(&DMAX),
			DOUBLE_ARG(&SUM),
			DOUBLE_ARG(&MEAN),
			DOUBLE_ARG(&STDEV),
			INTEGER8_ARG(&NGOODC),
			INTEGER8_ARG(&IMINC),
			DOUBLE_ARG(&DMINC),
			INTEGER8_ARG(&IMAXC),
			DOUBLE_ARG(&DMAXC),
			DOUBLE_ARG(&SUMC),
			DOUBLE_ARG(&MEANC),
			DOUBLE_ARG(&STDEVC),
			INTEGER_ARG(&STATUS) ); )

  F77_IMPORT_INTEGER( STATUS, *status );
  F77_IMPORT_INTEGER8( NGOOD, *ngood );
  F77_IMPORT_INTEGER8( IMIN, *imin );
  F77_IMPORT_DOUBLE( DMIN, *dmin );
  F77_IMPORT_INTEGER8( IMAX, *imax );
  F77_IMPORT_DOUBLE( DMAX, *dmax );
  F77_IMPORT_DOUBLE( SUM, *sum );
  F77_IMPORT_DOUBLE( MEAN, *mean );
  F77_IMPORT_DOUBLE( STDEV, *stdev );
  F77_IMPORT_INTEGER8( NGOODC, *ngoodc );
  F77_IMPORT_INTEGER8( IMINC, *iminc );
  F77_IMPORT_DOUBLE( DMINC, *dminc );
  F77_IMPORT_INTEGER8( IMAXC, *imaxc );
  F77_IMPORT_DOUBLE( DMAXC, *dmaxc );
  F77_IMPORT_DOUBLE( SUMC, *sumc );
  F77_IMPORT_DOUBLE( MEANC, *meanc );
  F77_IMPORT_DOUBLE( STDEVC, *stdevc );

  F77_FREE_DOUBLE( DATA );
  F77_FREE_REAL( CLIP );

}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_wwrt)( INTEGER(IAST),
                           CHARACTER(NAME),
                           CHARACTER(LOC),
                           INTEGER(STATUS)
                           TRAIL(NAME)
                           TRAIL(LOC) );

void kpg1Wwrt( AstObject *obj, const char *name, const HDSLoc *loc,
               int *status ){
   DECLARE_INTEGER(IAST);
   DECLARE_CHARACTER_DYN(NAME);
   DECLARE_CHARACTER(LOC,DAT__SZLOC);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( astP2I( obj ), IAST );

   F77_CREATE_EXPORT_CHARACTER( name, NAME );

   if ( loc == NULL ) {
      F77_EXPORT_LOCATOR( DAT__ROOT, LOC );
   } else {
      HDS_EXPORT_CLOCATOR( loc, LOC, status );
   }

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_wwrt)( INTEGER_ARG(&IAST),
                        CHARACTER_ARG(NAME),
                        CHARACTER_ARG(LOC),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(NAME)
                        TRAIL_ARG(LOC) ); )

   F77_FREE_CHARACTER( NAME );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_wread)( CHARACTER(LOC),
                            CHARACTER(NAME),
                            INTEGER(IAST),
                            INTEGER(STATUS)
                            TRAIL(LOC)
                            TRAIL(NAME) );

void kpg1Wread( const HDSLoc *loc, const char *name, AstObject **obj,
                int *status ){
   DECLARE_CHARACTER(LOC,DAT__SZLOC);
   DECLARE_CHARACTER_DYN(NAME);
   DECLARE_INTEGER(IAST);
   DECLARE_INTEGER(STATUS);

   if ( loc == NULL ) {
      F77_EXPORT_LOCATOR( DAT__ROOT, LOC );
   } else {
      HDS_EXPORT_CLOCATOR( loc, LOC, status );
   }

   F77_CREATE_EXPORT_CHARACTER( name, NAME );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_wread)( CHARACTER_ARG(LOC),
                         CHARACTER_ARG(NAME),
                         INTEGER_ARG(&IAST),
                         INTEGER_ARG(&STATUS)
                         TRAIL_ARG(LOC)
                         TRAIL_ARG(NAME) ); )
   {
      int tmp;
      F77_IMPORT_INTEGER( IAST, tmp );
      *obj = astI2P( tmp );
   }

   F77_FREE_CHARACTER( NAME );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_mxmnr)( LOGICAL(BAD),
                            INTEGER(EL),
                            REAL_ARRAY(ARRAY),
                            INTEGER(NINVAL),
                            REAL(MAXMUM),
                            REAL(MINMUM),
                            INTEGER(MAXPOS),
                            INTEGER(MINPOS),
                            INTEGER(STATUS) );

void kpg1Mxmnr( int bad, int el, float *array, int *ninval, float *maxmum,
                float *minmum, int *maxpos, int *minpos, int *status ){

   DECLARE_LOGICAL(BAD);
   DECLARE_INTEGER(EL);
   DECLARE_REAL_ARRAY_DYN(ARRAY);
   DECLARE_INTEGER(NINVAL);
   DECLARE_REAL(MAXMUM);
   DECLARE_REAL(MINMUM);
   DECLARE_INTEGER(MAXPOS);
   DECLARE_INTEGER(MINPOS);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_LOGICAL( bad, BAD );
   F77_EXPORT_INTEGER( el, EL );
   F77_CREATE_REAL_ARRAY( ARRAY, el );
   F77_ASSOC_REAL_ARRAY( ARRAY, array );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_mxmnr)( LOGICAL_ARG(&BAD),
                         INTEGER_ARG(&EL),
                         REAL_ARRAY_ARG(ARRAY),
                         INTEGER_ARG(&NINVAL),
                         REAL_ARG(&MAXMUM),
                         REAL_ARG(&MINMUM),
                         INTEGER_ARG(&MAXPOS),
                         INTEGER_ARG(&MINPOS),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_REAL_ARRAY( ARRAY, array, el );
   F77_IMPORT_INTEGER( NINVAL, *ninval );
   F77_IMPORT_REAL( MAXMUM, *maxmum );
   F77_IMPORT_REAL( MINMUM, *minmum );
   F77_IMPORT_INTEGER( MAXPOS, *maxpos );
   F77_IMPORT_INTEGER( MINPOS, *minpos );

   F77_FREE_REAL( ARRAY );

}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_mxmni)( LOGICAL(BAD),
                            INTEGER(EL),
                            INTEGER_ARRAY(ARRAY),
                            INTEGER(NINVAL),
                            INTEGER(MAXMUM),
                            INTEGER(MINMUM),
                            INTEGER(MAXPOS),
                            INTEGER(MINPOS),
                            INTEGER(STATUS) );

void kpg1Mxmni( int bad, int el, int *array, int *ninval, int *maxmum,
                int *minmum, int *maxpos, int *minpos, int *status ){

   DECLARE_LOGICAL(BAD);
   DECLARE_INTEGER(EL);
   DECLARE_INTEGER_ARRAY_DYN(ARRAY);
   DECLARE_INTEGER(NINVAL);
   DECLARE_INTEGER(MAXMUM);
   DECLARE_INTEGER(MINMUM);
   DECLARE_INTEGER(MAXPOS);
   DECLARE_INTEGER(MINPOS);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_LOGICAL( bad, BAD );
   F77_EXPORT_INTEGER( el, EL );
   F77_CREATE_INTEGER_ARRAY( ARRAY, el );
   F77_ASSOC_INTEGER_ARRAY( ARRAY, array );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_mxmni)( LOGICAL_ARG(&BAD),
                         INTEGER_ARG(&EL),
                         INTEGER_ARRAY_ARG(ARRAY),
                         INTEGER_ARG(&NINVAL),
                         INTEGER_ARG(&MAXMUM),
                         INTEGER_ARG(&MINMUM),
                         INTEGER_ARG(&MAXPOS),
                         INTEGER_ARG(&MINPOS),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_INTEGER_ARRAY( ARRAY, array, el );
   F77_IMPORT_INTEGER( NINVAL, *ninval );
   F77_IMPORT_INTEGER( MAXMUM, *maxmum );
   F77_IMPORT_INTEGER( MINMUM, *minmum );
   F77_IMPORT_INTEGER( MAXPOS, *maxpos );
   F77_IMPORT_INTEGER( MINPOS, *minpos );

   F77_FREE_INTEGER( ARRAY );

}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_mxmnd)( LOGICAL(BAD),
                            INTEGER(EL),
                            DOUBLE_ARRAY(ARRAY),
                            INTEGER(NINVAL),
                            DOUBLE(MAXMUM),
                            DOUBLE(MINMUM),
                            INTEGER(MAXPOS),
                            INTEGER(MINPOS),
                            INTEGER(STATUS) );

void kpg1Mxmnd( int bad, int el, double *array, int *ninval, double *maxmum,
                double *minmum, int *maxpos, int *minpos, int *status ){

   DECLARE_LOGICAL(BAD);
   DECLARE_INTEGER(EL);
   DECLARE_DOUBLE_ARRAY_DYN(ARRAY);
   DECLARE_INTEGER(NINVAL);
   DECLARE_DOUBLE(MAXMUM);
   DECLARE_DOUBLE(MINMUM);
   DECLARE_INTEGER(MAXPOS);
   DECLARE_INTEGER(MINPOS);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_LOGICAL( bad, BAD );
   F77_EXPORT_INTEGER( el, EL );
   F77_CREATE_DOUBLE_ARRAY( ARRAY, el );
   F77_ASSOC_DOUBLE_ARRAY( ARRAY, array );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_mxmnd)( LOGICAL_ARG(&BAD),
                         INTEGER_ARG(&EL),
                         DOUBLE_ARRAY_ARG(ARRAY),
                         INTEGER_ARG(&NINVAL),
                         DOUBLE_ARG(&MAXMUM),
                         DOUBLE_ARG(&MINMUM),
                         INTEGER_ARG(&MAXPOS),
                         INTEGER_ARG(&MINPOS),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_DOUBLE_ARRAY( ARRAY, array, el );
   F77_IMPORT_INTEGER( NINVAL, *ninval );
   F77_IMPORT_DOUBLE( MAXMUM, *maxmum );
   F77_IMPORT_DOUBLE( MINMUM, *minmum );
   F77_IMPORT_INTEGER( MAXPOS, *maxpos );
   F77_IMPORT_INTEGER( MINPOS, *minpos );

   F77_FREE_DOUBLE( ARRAY );

}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_medud)( LOGICAL(BAD),
                            INTEGER(EL),
                            DOUBLE_ARRAY(ARRAY),
                            DOUBLE(MEDIAN),
                            INTEGER(NELUSE),
                            INTEGER(STATUS) );

void kpg1Medud( int bad, int el, double *array, double *median,
                int *neluse, int *status ){
   DECLARE_LOGICAL(BAD);
   DECLARE_INTEGER(EL);
   DECLARE_DOUBLE_ARRAY_DYN(ARRAY);
   DECLARE_DOUBLE(MEDIAN);
   DECLARE_INTEGER(NELUSE);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_DOUBLE_ARRAY( ARRAY, el );

   F77_EXPORT_LOGICAL( bad, BAD );
   F77_EXPORT_INTEGER( el, EL );
   F77_ASSOC_DOUBLE_ARRAY( ARRAY, array );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_medud)( LOGICAL_ARG(&BAD),
                         INTEGER_ARG(&EL),
                         DOUBLE_ARRAY_ARG(ARRAY),
                         DOUBLE_ARG(&MEDIAN),
                         INTEGER_ARG(&NELUSE),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_DOUBLE( MEDIAN, *median );
   F77_IMPORT_INTEGER( NELUSE, *neluse );

   F77_FREE_REAL( ARRAY );
}

F77_SUBROUTINE(kpg1_medur)( LOGICAL(BAD),
                            INTEGER(EL),
                            REAL_ARRAY(ARRAY),
                            REAL(MEDIAN),
                            INTEGER(NELUSE),
                            INTEGER(STATUS) );

void kpg1Medur( int bad, int el, float *array, float *median,
                int *neluse, int *status ){
   DECLARE_LOGICAL(BAD);
   DECLARE_INTEGER(EL);
   DECLARE_REAL_ARRAY_DYN(ARRAY);
   DECLARE_REAL(MEDIAN);
   DECLARE_INTEGER(NELUSE);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_REAL_ARRAY( ARRAY, el );

   F77_EXPORT_LOGICAL( bad, BAD );
   F77_EXPORT_INTEGER( el, EL );
   F77_ASSOC_REAL_ARRAY( ARRAY, array );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_medur)( LOGICAL_ARG(&BAD),
                         INTEGER_ARG(&EL),
                         REAL_ARRAY_ARG(ARRAY),
                         REAL_ARG(&MEDIAN),
                         INTEGER_ARG(&NELUSE),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_REAL( MEDIAN, *median );
   F77_IMPORT_INTEGER( NELUSE, *neluse );

   F77_FREE_REAL( ARRAY );
}

F77_SUBROUTINE(kpg1_opgrd)( INTEGER(NPOS),
                            DOUBLE_ARRAY(POS),
                            LOGICAL(WEST),
                            DOUBLE_ARRAY(PAR),
                            DOUBLE(RDIAM),
                            INTEGER(STATUS) );


void kpg1Opgrd( int npos, const double pos[], int west, double *par, double *rdiam, int *status ){
   DECLARE_INTEGER(NPOS);
   DECLARE_DOUBLE_ARRAY_DYN(POS);
   DECLARE_LOGICAL(WEST);
   DECLARE_DOUBLE_ARRAY_DYN(PAR);
   DECLARE_DOUBLE(RDIAM);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_DOUBLE_ARRAY( POS, npos );
   F77_CREATE_DOUBLE_ARRAY( PAR, 7 );

   F77_EXPORT_INTEGER( npos, NPOS );
   F77_EXPORT_DOUBLE_ARRAY( pos, POS, npos );
   F77_EXPORT_LOGICAL( west, WEST );
   F77_ASSOC_DOUBLE_ARRAY( PAR, par );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_opgrd)( INTEGER_ARG(&NPOS),
                         DOUBLE_ARRAY_ARG(POS),
                         LOGICAL_ARG(&WEST),
                         DOUBLE_ARRAY_ARG(PAR),
                         DOUBLE_ARG(&RDIAM),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_DOUBLE_ARRAY( PAR, par, 7 );
   F77_IMPORT_DOUBLE( RDIAM, *rdiam );

   F77_FREE_DOUBLE( POS );
   F77_FREE_DOUBLE( PAR );
}


F77_SUBROUTINE(kpg1_gtwcs)( INTEGER(INDF), INTEGER(IWCS), INTEGER(STATUS) );

void kpg1Gtwcs( int indf, AstFrameSet **iwcs, int *status ){
   DECLARE_INTEGER(INDF);
   DECLARE_INTEGER(IWCS);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( indf, INDF );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_gtwcs)( INTEGER_ARG(&INDF),
                         INTEGER_ARG(&IWCS),
                         INTEGER_ARG(&STATUS) ); )

   {
      int tmp;
      F77_IMPORT_INTEGER( IWCS, tmp );
      *iwcs = astI2P( tmp );
   }

   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_asffr)( INTEGER(TARGET),
                            CHARACTER(DOMAIN),
                            INTEGER(IFRM),
                            INTEGER(STATUS)
                            TRAIL(DOMAIN) );

void kpg1Asffr( AstFrameSet *target, const char *domain, int *ifrm, int *status ){
   DECLARE_INTEGER(TARGET);
   DECLARE_CHARACTER_DYN(DOMAIN);
   DECLARE_INTEGER(IFRM);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( astP2I( target ), TARGET );
   F77_CREATE_EXPORT_CHARACTER( domain, DOMAIN );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_asffr)( INTEGER_ARG(&TARGET),
                         CHARACTER_ARG(DOMAIN),
                         INTEGER_ARG(&IFRM),
                         INTEGER_ARG(&STATUS)
                         TRAIL_ARG(DOMAIN) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_INTEGER( IFRM, *ifrm );
   F77_FREE_CHARACTER( DOMAIN );
}



/* ------------------------------- */

F77_SUBROUTINE(kpg1_asmrg)( INTEGER(IWCS1),
                            INTEGER(IWCS2),
                            CHARACTER(DOMAIN),
                            LOGICAL(QUIET),
                            INTEGER(IND),
                            INTEGER(STATUS)
                            TRAIL(DOMAIN) );

void kpg1Asmrg( AstFrameSet *iwcs1, AstFrameSet *iwcs2, const char *domain,
                int quiet, int ind, int *status ){
   DECLARE_INTEGER(IWCS1);
   DECLARE_INTEGER(IWCS2);
   DECLARE_CHARACTER_DYN(DOMAIN);
   DECLARE_LOGICAL(QUIET);
   DECLARE_INTEGER(IND);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( astP2I( iwcs1 ), IWCS1 );
   F77_EXPORT_INTEGER( astP2I( iwcs2 ), IWCS2 );
   F77_CREATE_EXPORT_CHARACTER( domain, DOMAIN );
   F77_EXPORT_LOGICAL( quiet, QUIET );
   F77_EXPORT_INTEGER( ind, IND );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_asmrg)( INTEGER_ARG(&IWCS1),
                                   INTEGER_ARG(&IWCS2),
                                   CHARACTER_ARG(DOMAIN),
                                   LOGICAL_ARG(&QUIET),
                                   INTEGER_ARG(&IND),
                                   INTEGER_ARG(&STATUS)
                                   TRAIL_ARG(DOMAIN) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_FREE_CHARACTER( DOMAIN );
}



/* ------------------------------- */

F77_SUBROUTINE(kpg1_datcp)( CHARACTER(LOC1),
                            CHARACTER(LOC2),
                            CHARACTER(NAME),
                            INTEGER(STATUS)
                            TRAIL(LOC1)
                            TRAIL(LOC2)
                            TRAIL(NAME) );

void kpg1Datcp( const HDSLoc *loc1, HDSLoc *loc2, const char *name, int *status ){
   DECLARE_CHARACTER(LOC1,DAT__SZLOC);
   DECLARE_CHARACTER(LOC2,DAT__SZLOC);
   DECLARE_CHARACTER_DYN(NAME);
   DECLARE_INTEGER(STATUS);

   if ( loc1 == NULL ) {
      F77_EXPORT_LOCATOR( DAT__ROOT, LOC1 );
   } else {
      HDS_EXPORT_CLOCATOR( loc1, LOC1, status );
   }

   if ( loc2 == NULL ) {
      F77_EXPORT_LOCATOR( DAT__ROOT, LOC2 );
   } else {
      HDS_EXPORT_CLOCATOR( loc2, LOC2, status );
   }

   F77_CREATE_EXPORT_CHARACTER( name, NAME );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_datcp)( CHARACTER_ARG(LOC1),
                         CHARACTER_ARG(LOC2),
                         CHARACTER_ARG(NAME),
                         INTEGER_ARG(&STATUS)
                         TRAIL_ARG(LOC1)
                         TRAIL_ARG(LOC2)
                         TRAIL_ARG(NAME) ); )

   F77_FREE_CHARACTER( NAME );
   F77_IMPORT_INTEGER( STATUS, *status );
}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_ghstd)( LOGICAL(BAD),
                            INTEGER(DIM),
                            DOUBLE_ARRAY(ARRAY),
                            DOUBLE_ARRAY(WGTS),
                            DOUBLE(WEIGHT),
                            INTEGER(NUMBIN),
                            LOGICAL(CUMUL),
                            DOUBLE(VALMAX),
                            DOUBLE(VALMIN),
                            INTEGER_ARRAY(HIST),
                            INTEGER(STATUS) );

void kpg1Ghstd( int bad, int dim, const double *array, const double *wgts,
                double weight, int numbin, int cumul,
                double *valmax, double *valmin, int *hist, int *status ){
   DECLARE_LOGICAL(BAD);
   DECLARE_INTEGER(DIM);
   DECLARE_DOUBLE_ARRAY_DYN(ARRAY);
   DECLARE_DOUBLE_ARRAY_DYN(WGTS);
   DECLARE_DOUBLE(WEIGHT);
   DECLARE_INTEGER(NUMBIN);
   DECLARE_LOGICAL(CUMUL);
   DECLARE_DOUBLE(VALMAX);
   DECLARE_DOUBLE(VALMIN);
   DECLARE_INTEGER_ARRAY_DYN(HIST);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_LOGICAL( bad, BAD );
   F77_EXPORT_INTEGER( dim, DIM );
   F77_CREATE_DOUBLE_ARRAY( ARRAY, dim );
   F77_EXPORT_DOUBLE_ARRAY( array, ARRAY, dim );
   F77_CREATE_DOUBLE_ARRAY( WGTS, dim );
   F77_EXPORT_DOUBLE_ARRAY( wgts, WGTS, dim );
   F77_EXPORT_DOUBLE( weight, WEIGHT );
   F77_EXPORT_INTEGER( numbin, NUMBIN );
   F77_EXPORT_LOGICAL( cumul, CUMUL );
   F77_EXPORT_DOUBLE( *valmax, VALMAX );
   F77_EXPORT_DOUBLE( *valmin, VALMIN );
   F77_CREATE_INTEGER_ARRAY( HIST, numbin );
   F77_ASSOC_INTEGER_ARRAY( HIST, hist );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_ghstd)( LOGICAL_ARG(&BAD),
                         INTEGER_ARG(&DIM),
                         DOUBLE_ARRAY_ARG(ARRAY),
                         DOUBLE_ARRAY_ARG(WGTS),
                         DOUBLE_ARG(&WEIGHT),
                         INTEGER_ARG(&NUMBIN),
                         LOGICAL_ARG(&CUMUL),
                         DOUBLE_ARG(&VALMAX),
                         DOUBLE_ARG(&VALMIN),
                         INTEGER_ARRAY_ARG(HIST),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_DOUBLE( VALMAX, *valmax );
   F77_IMPORT_DOUBLE( VALMIN, *valmin );
   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_INTEGER_ARRAY( HIST, hist, numbin );
   F77_FREE_INTEGER( ARRAY );
   F77_FREE_INTEGER( HIST );
}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_ghstr)( LOGICAL(BAD),
                            INTEGER(DIM),
                            REAL_ARRAY(ARRAY),
                            DOUBLE_ARRAY(WGTS),
                            DOUBLE(WEIGHT),
                            INTEGER(NUMBIN),
                            LOGICAL(CUMUL),
                            REAL(VALMAX),
                            REAL(VALMIN),
                            INTEGER_ARRAY(HIST),
                            INTEGER(STATUS) );

void kpg1Ghstr( int bad, int dim, const float *array,  const double *wgts,
                double weight, int numbin, int cumul,
                float *valmax, float *valmin, int *hist, int *status ){
   DECLARE_LOGICAL(BAD);
   DECLARE_INTEGER(DIM);
   DECLARE_REAL_ARRAY_DYN(ARRAY);
   DECLARE_DOUBLE_ARRAY_DYN(WGTS);
   DECLARE_DOUBLE(WEIGHT);
   DECLARE_INTEGER(NUMBIN);
   DECLARE_LOGICAL(CUMUL);
   DECLARE_REAL(VALMAX);
   DECLARE_REAL(VALMIN);
   DECLARE_INTEGER_ARRAY_DYN(HIST);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_LOGICAL( bad, BAD );
   F77_EXPORT_INTEGER( dim, DIM );
   F77_CREATE_REAL_ARRAY( ARRAY, dim );
   F77_EXPORT_REAL_ARRAY( array, ARRAY, dim );
   F77_CREATE_DOUBLE_ARRAY( WGTS, dim );
   F77_EXPORT_DOUBLE_ARRAY( wgts, WGTS, dim );
   F77_EXPORT_DOUBLE( weight, WEIGHT );
   F77_EXPORT_INTEGER( numbin, NUMBIN );
   F77_EXPORT_LOGICAL( cumul, CUMUL );
   F77_EXPORT_REAL( *valmax, VALMAX );
   F77_EXPORT_REAL( *valmin, VALMIN );
   F77_CREATE_INTEGER_ARRAY( HIST, numbin );
   F77_ASSOC_INTEGER_ARRAY( HIST, hist );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_ghstr)( LOGICAL_ARG(&BAD),
                         INTEGER_ARG(&DIM),
                         REAL_ARRAY_ARG(ARRAY),
                         DOUBLE_ARRAY_ARG(WGTS),
                         DOUBLE_ARG(&WEIGHT),
                         INTEGER_ARG(&NUMBIN),
                         LOGICAL_ARG(&CUMUL),
                         REAL_ARG(&VALMAX),
                         REAL_ARG(&VALMIN),
                         INTEGER_ARRAY_ARG(HIST),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_REAL( VALMAX, *valmax );
   F77_IMPORT_REAL( VALMIN, *valmin );
   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_INTEGER_ARRAY( HIST, hist, numbin );
   F77_FREE_INTEGER( ARRAY );
   F77_FREE_INTEGER( HIST );
}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_ghst8d)( LOGICAL(BAD),
                             INTEGER8(DIM),
                             DOUBLE_ARRAY(ARRAY),
                             DOUBLE_ARRAY(WGTS),
                             DOUBLE(WEIGHT),
                             INTEGER(NUMBIN),
                             LOGICAL(CUMUL),
                             DOUBLE(VALMAX),
                             DOUBLE(VALMIN),
                             INTEGER8_ARRAY(HIST),
                             INTEGER(STATUS) );

void kpg1Ghst8d( int bad, hdsdim dim, const double *array, const double *wgts,
                 double weight, int numbin, int cumul,
                 double *valmax, double *valmin, hdsdim *hist, int *status ){
   DECLARE_LOGICAL(BAD);
   DECLARE_INTEGER8(DIM);
   DECLARE_DOUBLE_ARRAY_DYN(ARRAY);
   DECLARE_DOUBLE_ARRAY_DYN(WGTS);
   DECLARE_DOUBLE(WEIGHT);
   DECLARE_INTEGER(NUMBIN);
   DECLARE_LOGICAL(CUMUL);
   DECLARE_DOUBLE(VALMAX);
   DECLARE_DOUBLE(VALMIN);
   DECLARE_INTEGER8_ARRAY_DYN(HIST);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_LOGICAL( bad, BAD );
   F77_EXPORT_INTEGER8( dim, DIM );
   F77_CREATE_DOUBLE_ARRAY( ARRAY, dim );
   F77_EXPORT_DOUBLE_ARRAY( array, ARRAY, dim );
   F77_CREATE_DOUBLE_ARRAY( WGTS, dim );
   F77_EXPORT_DOUBLE_ARRAY( wgts, WGTS, dim );
   F77_EXPORT_DOUBLE( weight, WEIGHT );
   F77_EXPORT_INTEGER( numbin, NUMBIN );
   F77_EXPORT_LOGICAL( cumul, CUMUL );
   F77_EXPORT_DOUBLE( *valmax, VALMAX );
   F77_EXPORT_DOUBLE( *valmin, VALMIN );
   F77_CREATE_INTEGER8_ARRAY( HIST, numbin );
   F77_ASSOC_INTEGER8_ARRAY( HIST, hist );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_ghst8d)( LOGICAL_ARG(&BAD),
                         INTEGER8_ARG(&DIM),
                         DOUBLE_ARRAY_ARG(ARRAY),
                         DOUBLE_ARRAY_ARG(WGTS),
                         DOUBLE_ARG(&WEIGHT),
                         INTEGER_ARG(&NUMBIN),
                         LOGICAL_ARG(&CUMUL),
                         DOUBLE_ARG(&VALMAX),
                         DOUBLE_ARG(&VALMIN),
                         INTEGER8_ARRAY_ARG(HIST),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_DOUBLE( VALMAX, *valmax );
   F77_IMPORT_DOUBLE( VALMIN, *valmin );
   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_INTEGER8_ARRAY( HIST, hist, numbin );
   F77_FREE_INTEGER( ARRAY );
   F77_FREE_INTEGER8( HIST );
}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_ghst8r)( LOGICAL(BAD),
                             INTEGER8(DIM),
                             REAL_ARRAY(ARRAY),
                             DOUBLE_ARRAY(WGTS),
                             DOUBLE(WEIGHT),
                             INTEGER(NUMBIN),
                             LOGICAL(CUMUL),
                             REAL(VALMAX),
                             REAL(VALMIN),
                             INTEGER8_ARRAY(HIST),
                             INTEGER(STATUS) );

void kpg1Ghst8r( int bad, hdsdim dim, const float *array,  const double *wgts,
                double weight, int numbin, int cumul,
                float *valmax, float *valmin, hdsdim *hist, int *status ){
   DECLARE_LOGICAL(BAD);
   DECLARE_INTEGER8(DIM);
   DECLARE_REAL_ARRAY_DYN(ARRAY);
   DECLARE_DOUBLE_ARRAY_DYN(WGTS);
   DECLARE_DOUBLE(WEIGHT);
   DECLARE_INTEGER(NUMBIN);
   DECLARE_LOGICAL(CUMUL);
   DECLARE_REAL(VALMAX);
   DECLARE_REAL(VALMIN);
   DECLARE_INTEGER8_ARRAY_DYN(HIST);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_LOGICAL( bad, BAD );
   F77_EXPORT_INTEGER8( dim, DIM );
   F77_CREATE_REAL_ARRAY( ARRAY, dim );
   F77_EXPORT_REAL_ARRAY( array, ARRAY, dim );
   F77_CREATE_DOUBLE_ARRAY( WGTS, dim );
   F77_EXPORT_DOUBLE_ARRAY( wgts, WGTS, dim );
   F77_EXPORT_DOUBLE( weight, WEIGHT );
   F77_EXPORT_INTEGER( numbin, NUMBIN );
   F77_EXPORT_LOGICAL( cumul, CUMUL );
   F77_EXPORT_REAL( *valmax, VALMAX );
   F77_EXPORT_REAL( *valmin, VALMIN );
   F77_CREATE_INTEGER8_ARRAY( HIST, numbin );
   F77_ASSOC_INTEGER8_ARRAY( HIST, hist );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_ghst8r)( LOGICAL_ARG(&BAD),
                         INTEGER8_ARG(&DIM),
                         REAL_ARRAY_ARG(ARRAY),
                         DOUBLE_ARRAY_ARG(WGTS),
                         DOUBLE_ARG(&WEIGHT),
                         INTEGER_ARG(&NUMBIN),
                         LOGICAL_ARG(&CUMUL),
                         REAL_ARG(&VALMAX),
                         REAL_ARG(&VALMIN),
                         INTEGER8_ARRAY_ARG(HIST),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_REAL( VALMAX, *valmax );
   F77_IMPORT_REAL( VALMIN, *valmin );
   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_INTEGER8_ARRAY( HIST, hist, numbin );
   F77_FREE_INTEGER( ARRAY );
   F77_FREE_INTEGER8( HIST );
}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_hsstp)( INTEGER(NUMBIN),
                            INTEGER_ARRAY(HIST),
                            DOUBLE(VALMAX),
                            DOUBLE(VALMIN),
                            DOUBLE(SUM),
                            DOUBLE(MEAN),
                            DOUBLE(MEDIAN),
                            DOUBLE(MODE),
                            INTEGER(STATUS) );

void kpg1Hsstp( int numbin, const int *hist, double valmax, double valmin,
                double *sum, double *mean, double *median, double *mode,
                int *status ){
   DECLARE_INTEGER(NUMBIN);
   DECLARE_INTEGER_ARRAY_DYN(HIST);
   DECLARE_DOUBLE(VALMAX);
   DECLARE_DOUBLE(VALMIN);
   DECLARE_DOUBLE(SUM);
   DECLARE_DOUBLE(MEAN);
   DECLARE_DOUBLE(MEDIAN);
   DECLARE_DOUBLE(MODE);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( numbin, NUMBIN );
   F77_CREATE_INTEGER_ARRAY( HIST, numbin );
   F77_EXPORT_INTEGER_ARRAY( hist, HIST, numbin );
   F77_EXPORT_DOUBLE( valmax, VALMAX );
   F77_EXPORT_DOUBLE( valmin, VALMIN );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_hsstp)( INTEGER_ARG(&NUMBIN),
                         INTEGER_ARRAY_ARG(HIST),
                         DOUBLE_ARG(&VALMAX),
                         DOUBLE_ARG(&VALMIN),
                         DOUBLE_ARG(&SUM),
                         DOUBLE_ARG(&MEAN),
                         DOUBLE_ARG(&MEDIAN),
                         DOUBLE_ARG(&MODE),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_DOUBLE( SUM, *sum );
   F77_IMPORT_DOUBLE( MEAN, *mean );
   F77_IMPORT_DOUBLE( MEDIAN, *median );
   F77_IMPORT_DOUBLE( MODE, *mode );
   F77_IMPORT_INTEGER( STATUS, *status );
   F77_FREE_INTEGER( HIST );
}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_hsstp8)( INTEGER(NUMBIN),
                             INTEGER8_ARRAY(HIST),
                             DOUBLE(VALMAX),
                             DOUBLE(VALMIN),
                             DOUBLE(SUM),
                             DOUBLE(MEAN),
                             DOUBLE(MEDIAN),
                             DOUBLE(MODE),
                             INTEGER(STATUS) );

void kpg1Hsstp8( int numbin, const hdsdim *hist, double valmax, double valmin,
                 double *sum, double *mean, double *median, double *mode,
                 int *status ){
   DECLARE_INTEGER(NUMBIN);
   DECLARE_INTEGER8_ARRAY_DYN(HIST);
   DECLARE_DOUBLE(VALMAX);
   DECLARE_DOUBLE(VALMIN);
   DECLARE_DOUBLE(SUM);
   DECLARE_DOUBLE(MEAN);
   DECLARE_DOUBLE(MEDIAN);
   DECLARE_DOUBLE(MODE);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( numbin, NUMBIN );
   F77_CREATE_INTEGER8_ARRAY( HIST, numbin );
   F77_EXPORT_INTEGER8_ARRAY( hist, HIST, numbin );
   F77_EXPORT_DOUBLE( valmax, VALMAX );
   F77_EXPORT_DOUBLE( valmin, VALMIN );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_hsstp8)( INTEGER_ARG(&NUMBIN),
                         INTEGER8_ARRAY_ARG(HIST),
                         DOUBLE_ARG(&VALMAX),
                         DOUBLE_ARG(&VALMIN),
                         DOUBLE_ARG(&SUM),
                         DOUBLE_ARG(&MEAN),
                         DOUBLE_ARG(&MEDIAN),
                         DOUBLE_ARG(&MODE),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_DOUBLE( SUM, *sum );
   F77_IMPORT_DOUBLE( MEAN, *mean );
   F77_IMPORT_DOUBLE( MEDIAN, *median );
   F77_IMPORT_DOUBLE( MODE, *mode );
   F77_IMPORT_INTEGER( STATUS, *status );
   F77_FREE_INTEGER8( HIST );
}

/* ------------------------------- */
F77_SUBROUTINE(fts1_astwn)( INTEGER(FC),
                            INTEGER(INDF),
                            INTEGER(STATUS) );

void fts1Astwn( AstFitsChan *fc, int indf, int *status ){
   DECLARE_INTEGER(FC);
   DECLARE_INTEGER(INDF);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( astP2I( fc ), FC );
   F77_EXPORT_INTEGER( indf, INDF );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(fts1_astwn)( INTEGER_ARG(&FC),
                         INTEGER_ARG(&INDF),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
}

F77_SUBROUTINE(kpg1_elgau)( REAL_ARRAY(SIG),
                            REAL(SIG0),
                            REAL(AXIS),
                            REAL(THETA),
                            INTEGER(STATUS) );

void kpg1Elgau( float *sig, float *sig0, float *axis, float *theta,
                int *status ) {

   DECLARE_REAL_ARRAY_DYN(SIG);
   DECLARE_REAL(SIG0);
   DECLARE_REAL(AXIS);
   DECLARE_REAL(THETA);
   DECLARE_INTEGER(STATUS);
   F77_CREATE_REAL_ARRAY( SIG, 4 );
   F77_EXPORT_REAL_ARRAY( sig, SIG, 4 );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_elgau)( REAL_ARRAY_ARG(SIG),
                         REAL_ARG(&SIG0),
                         REAL_ARG(&AXIS),
                         REAL_ARG(&THETA),
                         INTEGER_ARG(&STATUS) ); )
   F77_IMPORT_REAL( SIG0, *sig0 );
   F77_IMPORT_REAL( AXIS, *axis );
   F77_IMPORT_REAL( THETA, *theta );
   F77_IMPORT_INTEGER( STATUS, *status );

}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_loctd)( INTEGER(NDIM),
                            INTEGER_ARRAY(LBND),
                            INTEGER_ARRAY(UBND),
                            DOUBLE_ARRAY(ARRAY),
                            REAL_ARRAY(INIT),
                            INTEGER_ARRAY(SEARCH),
                            LOGICAL(POSTIV),
                            REAL_ARRAY(MXSHFT),
                            INTEGER(MAXITE),
                            REAL(TOLER),
                            INTEGER(SEL),
                            REAL_ARRAY(FINAL),
                            REAL_ARRAY(WORK1),
                            INTEGER(STATUS) );

void kpg1Loctd( int ndim, const int *lbnd, const int *ubnd,
                const double *array, const float *init, const int
		*search, int postiv, const float *mxshft, int maxite,
		float toler, int sel, float *final, float *work1, int
		*status ){

   DECLARE_INTEGER(NDIM);
   DECLARE_INTEGER_ARRAY_DYN(LBND);
   DECLARE_INTEGER_ARRAY_DYN(UBND);
   DECLARE_DOUBLE_ARRAY_DYN(ARRAY);
   DECLARE_REAL_ARRAY_DYN(INIT);
   DECLARE_INTEGER_ARRAY_DYN(SEARCH);
   DECLARE_LOGICAL(POSTIV);
   DECLARE_REAL_ARRAY_DYN(MXSHFT);
   DECLARE_INTEGER(MAXITE);
   DECLARE_REAL(TOLER);
   DECLARE_INTEGER(SEL);
   DECLARE_REAL_ARRAY_DYN(FINAL);
   DECLARE_REAL_ARRAY_DYN(WORK1);
   DECLARE_INTEGER(STATUS);
   int i, nel, nw;

   nel = 1;
   for( i = 0; i < ndim; i++ ) nel *= ( ubnd[ i ] - lbnd [ i ] + 1 );
   nw = 51*sel*ndim;

   F77_CREATE_INTEGER_ARRAY( LBND, ndim );
   F77_CREATE_INTEGER_ARRAY( UBND, ndim );
   F77_CREATE_DOUBLE_ARRAY( ARRAY, nel );
   F77_CREATE_REAL_ARRAY( INIT, ndim );
   F77_CREATE_INTEGER_ARRAY( SEARCH, ndim );
   F77_CREATE_REAL_ARRAY( MXSHFT, ndim );
   F77_CREATE_REAL_ARRAY( FINAL, ndim );
   F77_CREATE_REAL_ARRAY( WORK1, nw );

   F77_EXPORT_INTEGER( ndim, NDIM );
   F77_EXPORT_INTEGER_ARRAY( lbnd, LBND, ndim );
   F77_EXPORT_INTEGER_ARRAY( ubnd, UBND, ndim );
   F77_EXPORT_DOUBLE_ARRAY( array, ARRAY, nel );
   F77_EXPORT_REAL_ARRAY( init, INIT, ndim );
   F77_EXPORT_INTEGER_ARRAY( search, SEARCH, ndim );
   F77_EXPORT_LOGICAL( postiv, POSTIV );
   F77_EXPORT_REAL_ARRAY( mxshft, MXSHFT, ndim );
   F77_EXPORT_INTEGER( maxite, MAXITE );
   F77_EXPORT_REAL( toler, TOLER );
   F77_EXPORT_INTEGER( sel, SEL );
   F77_EXPORT_REAL_ARRAY( final, FINAL, ndim );
   F77_EXPORT_REAL_ARRAY( work1, WORK1, nw );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_loctd)( INTEGER_ARG(&NDIM),
                         INTEGER_ARRAY_ARG(LBND),
                         INTEGER_ARRAY_ARG(UBND),
                         DOUBLE_ARRAY_ARG(ARRAY),
                         REAL_ARRAY_ARG(INIT),
                         INTEGER_ARRAY_ARG(SEARCH),
                         LOGICAL_ARG(&POSTIV),
                         REAL_ARRAY_ARG(MXSHFT),
                         INTEGER_ARG(&MAXITE),
                         REAL_ARG(&TOLER),
                         INTEGER_ARG(&SEL),
                         REAL_ARRAY_ARG(FINAL),
                         REAL_ARRAY_ARG(WORK1),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_REAL_ARRAY( FINAL, final, nbnd );
   F77_IMPORT_REAL_ARRAY( WORK1, work1, nbnd );

   F77_FREE_INTEGER( LBND );
   F77_FREE_INTEGER( UBND );
   F77_FREE_DOUBLE( ARRAY );
   F77_FREE_REAL( INIT );
   F77_FREE_INTEGER( SEARCH );
   F77_FREE_REAL( MXSHFT );
   F77_FREE_REAL( FINAL );
   F77_FREE_REAL( WORK1 );
}


F77_SUBROUTINE(kpg1_pixsc)( INTEGER(IWCS),
                            DOUBLE_ARRAY(AT),
                            DOUBLE_ARRAY(PIXSC),
                            CHARACTER_ARRAY(VALUE),
                            CHARACTER_ARRAY(UNIT),
                            INTEGER(STATUS) TRAIL(VALUE) TRAIL(UNIT));

void kpgPixsc( AstFrameSet * iwcs,
               const double at[],
               double pixsc[],
               char *const *value,
               char *const *unit,
               int chrarr_length,
               int *status ) {
  int nwcs;
  int nin;
  int strlength;

  DECLARE_INTEGER( IWCS );
  DECLARE_DOUBLE_ARRAY_DYN( AT );
  DECLARE_DOUBLE_ARRAY_DYN( PIXSC );
  DECLARE_CHARACTER_ARRAY_DYN( VALUE );
  DECLARE_CHARACTER_ARRAY_DYN( UNIT );
  DECLARE_INTEGER( STATUS );

  if (*status != SAI__OK) return;
  if (! iwcs) return;

  /* We need to know how much space to allocate for the return arrays
     in the fortran side */
  nwcs = astGetI( iwcs, "Nout" );
  nin = astGetI( iwcs, "Nin" );

  /* if the returned char array pointers are null we set a length
     that will work */
  if ( (value || unit) && chrarr_length > 0) {
    strlength = chrarr_length;
  } else {
    strlength = 40;
  }

  F77_CREATE_DOUBLE_ARRAY( PIXSC, nwcs );
  F77_CREATE_DOUBLE_ARRAY( AT, nwcs );
  F77_CREATE_CHARACTER_ARRAY( VALUE, strlength-1, nwcs );
  F77_CREATE_CHARACTER_ARRAY( UNIT, strlength-1, nwcs );
  F77_EXPORT_DOUBLE_ARRAY( at, AT, nin );
  F77_ASSOC_DOUBLE_ARRAY( PIXSC, pixsc );
  F77_EXPORT_INTEGER( astP2I( iwcs ), IWCS );
  F77_EXPORT_INTEGER( *status, STATUS );

  F77_LOCK( F77_CALL(kpg1_pixsc)( INTEGER_ARG(&IWCS),
                        DOUBLE_ARRAY_ARG(AT),
                        DOUBLE_ARRAY_ARG(PIXSC),
                        CHARACTER_ARRAY_ARG(VALUE),
                        CHARACTER_ARRAY_ARG(UNIT),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(VALUE)
                        TRAIL_ARG(UNIT) ); )

  F77_IMPORT_INTEGER( STATUS, *status );
  if (pixsc) {
    F77_IMPORT_DOUBLE_ARRAY( PIXSC, pixsc, nwcs );
  }
  if (value && chrarr_length > 0) {
    F77_IMPORT_CHARACTER_ARRAY_P( VALUE, VALUE_length, value, strlength,
                                  nwcs );
  }
  if (unit && chrarr_length > 0) {
    F77_IMPORT_CHARACTER_ARRAY_P( UNIT, UNIT_length, unit, strlength,
                                  nwcs );
  }
  F77_FREE_CHARACTER(VALUE);
  F77_FREE_CHARACTER(UNIT);

  return;
}

F77_SUBROUTINE(kpg1_badbx)( INTEGER(INDF1),
                            INTEGER(OPER),
                            INTEGER(INDF2),
                            INTEGER(NGOOD),
                            INTEGER(STATUS) );

void kpg1Badbx( int indf1, int oper, int *indf2, int *ngood, int *status ){
   DECLARE_INTEGER(INDF1);
   DECLARE_INTEGER(OPER);
   DECLARE_INTEGER(INDF2);
   DECLARE_INTEGER(NGOOD);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( indf1, INDF1 );
   F77_EXPORT_INTEGER( oper, OPER );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_badbx)( INTEGER_ARG(&INDF1),
                         INTEGER_ARG(&OPER),
                         INTEGER_ARG(&INDF2),
                         INTEGER_ARG(&NGOOD),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( INDF2, *indf2 );
   F77_IMPORT_INTEGER( NGOOD, *ngood );
   F77_IMPORT_INTEGER( STATUS, *status );
}

F77_SUBROUTINE(kpg1_pxscl)( INTEGER(IWCS),
                            DOUBLE_ARRAY(AT),
                            DOUBLE_ARRAY(PIXSC),
                            INTEGER(STATUS));

void kpg1Pxscl( AstFrameSet *iwcs,
                const double *at,
                double *pixsc,
                int *status ) {
   DECLARE_INTEGER( IWCS );
   DECLARE_DOUBLE_ARRAY_DYN( AT );
   DECLARE_DOUBLE_ARRAY_DYN( PIXSC );
   DECLARE_INTEGER( STATUS );

   int nwcs;
   int nin;

   if( *status != SAI__OK ) return;

   nwcs = astGetI( iwcs, "Nout" );
   nin = astGetI( iwcs, "Nin" );

   F77_CREATE_DOUBLE_ARRAY( PIXSC, nwcs );
   F77_ASSOC_DOUBLE_ARRAY( PIXSC, pixsc );
   F77_CREATE_DOUBLE_ARRAY( AT, nwcs );
   F77_EXPORT_DOUBLE_ARRAY( at, AT, nin );
   F77_EXPORT_INTEGER( astP2I( iwcs ), IWCS );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_pxscl)( INTEGER_ARG(&IWCS),
                         DOUBLE_ARRAY_ARG(AT),
                         DOUBLE_ARRAY_ARG(PIXSC),
                         INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_DOUBLE_ARRAY( PIXSC, pixsc, nwcs );

   return;
}


/* ------------------------------- */

F77_SUBROUTINE(kpg1_hsect)( CHARACTER(LOC1),
                            INTEGER(NDIM),
                            INTEGER_ARRAY(LBND),
                            INTEGER_ARRAY(UBND),
                            CHARACTER(LOC2),
                            CHARACTER(NAME),
                            INTEGER(STATUS)
                            TRAIL(LOC1)
                            TRAIL(LOC2)
                            TRAIL(NAME) );

void kpg1Hsect( const HDSLoc *loc1, int ndim, int *lbnd, int *ubnd, HDSLoc *loc2,
                const char *name, int *status ){
   DECLARE_CHARACTER(LOC1,DAT__SZLOC);
   DECLARE_INTEGER(NDIM);
   DECLARE_INTEGER_ARRAY_DYN(LBND);
   DECLARE_INTEGER_ARRAY_DYN(UBND);
   DECLARE_CHARACTER(LOC2,DAT__SZLOC);
   DECLARE_CHARACTER_DYN(NAME);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_INTEGER_ARRAY( LBND, ndim );
   F77_CREATE_INTEGER_ARRAY( UBND, ndim );

   if ( loc1 == NULL ) {
      F77_EXPORT_LOCATOR( DAT__ROOT, LOC1 );
   } else {
      HDS_EXPORT_CLOCATOR( loc1, LOC1, status );
   }

   if ( loc2 == NULL ) {
      F77_EXPORT_LOCATOR( DAT__ROOT, LOC2 );
   } else {
      HDS_EXPORT_CLOCATOR( loc2, LOC2, status );
   }

   F77_CREATE_EXPORT_CHARACTER( name, NAME );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_EXPORT_INTEGER( ndim, NDIM );
   F77_EXPORT_INTEGER_ARRAY( lbnd, LBND, ndim );
   F77_EXPORT_INTEGER_ARRAY( ubnd, UBND, ndim );



   F77_LOCK( F77_CALL(kpg1_hsect)( CHARACTER_ARG(LOC1),
                         INTEGER_ARG(&NDIM),
                         INTEGER_ARRAY_ARG(LBND),
                         INTEGER_ARRAY_ARG(UBND),
                         CHARACTER_ARG(LOC2),
                         CHARACTER_ARG(NAME),
                         INTEGER_ARG(&STATUS)
                         TRAIL_ARG(LOC1)
                         TRAIL_ARG(LOC2)
                         TRAIL_ARG(NAME) ); )

   F77_FREE_CHARACTER( NAME );
   F77_IMPORT_INTEGER( STATUS, *status );
   F77_FREE_INTEGER( LBND );
   F77_FREE_INTEGER( UBND );
}


/* ------------------------------- */

F77_SUBROUTINE(kpg1_cputm)( INTEGER_ARRAY(CONTXT),
                            DOUBLE(CPUTIM) );

void kpg1Cputm( int contxt[4], double *cputim ){
   F77_LOCK( F77_CALL(kpg1_cputm)( contxt, cputim ); )
}



/* ------------------------------- */

F77_SUBROUTINE(kpg_typsz)( CHARACTER(TYPE),
                           INTEGER(NBYTES),
                           INTEGER(STATUS)
                           TRAIL(TYPE) );

size_t kpg1Typsz( const char *type, int *status ){
   DECLARE_CHARACTER_DYN(TYPE);
   DECLARE_INTEGER(STATUS);
   DECLARE_INTEGER(NBYTES);

   F77_CREATE_EXPORT_CHARACTER( type, TYPE );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg_typsz)( CHARACTER_ARG(TYPE),
                                  INTEGER_ARG(&NBYTES),
                                  INTEGER_ARG(&STATUS)
                                  TRAIL_ARG(TYPE) ); )

   F77_FREE_CHARACTER( TYPE );
   F77_IMPORT_INTEGER( STATUS, *status );

   return NBYTES;
}


