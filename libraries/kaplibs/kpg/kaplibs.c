/*
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

*  Authors:
*     DSB: David S Berry
*     {enter_new_authors_here}

*  History:
*     29-SEP-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}
*/

/* Header files. */
/* ============= */
#include "f77.h"                 
#include "kaplibs.h"
#include "kaplibs_private.h"


/* Wrapper function implementations. */
/* ================================= */

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

   F77_CALL(kpg1_fillr)( REAL_ARG(&VALUE),
                         INTEGER_ARG(&EL),
                         REAL_ARRAY_ARG(ARRAY),
                         INTEGER_ARG(&STATUS) );

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_REAL_ARRAY( ARRAY, array,el );
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

   F77_CALL(kpg1_gausr)( REAL_ARG(&SIGMA), 
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
                         INTEGER_ARG(&STATUS) );

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

void kpg1Kymap( int igrp, AstKeyMap **keymap, int *status ){
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

   F77_CALL(kpg1_manir)( INTEGER_ARG(&NDIMI), 
                         INTEGER_ARRAY_ARG(DIMI),
                         REAL_ARRAY_ARG(IN),
                         INTEGER_ARG(&NDIMO), 
                         INTEGER_ARRAY_ARG(DIMO),
                         INTEGER_ARRAY_ARG(AXES),
                         INTEGER_ARRAY_ARG(COLOFF),
                         INTEGER_ARRAY_ARG(EXPOFF),
                         REAL_ARRAY_ARG(OUT),
                         INTEGER_ARG(&STATUS) );

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_REAL_ARRAY( OUT, out, nout );

   F77_FREE_REAL( DIMI );
   F77_FREE_REAL( IN );
   F77_FREE_REAL( DIMO );
   F77_FREE_REAL( AXES );
   F77_FREE_REAL( COLOFF );
   F77_FREE_REAL( EXPOFF );
   F77_FREE_REAL( OUT );

}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_pseed)( INTEGER(STATUS) );

void kpg1Pseed( int *status ){
   DECLARE_INTEGER(STATUS);
   F77_EXPORT_INTEGER( *status, STATUS );
   F77_CALL(kpg1_pseed)( INTEGER_ARG(&STATUS) );
   F77_IMPORT_INTEGER( STATUS, *status );
}
