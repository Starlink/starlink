/*
*+
*  Name:
*     pda.c

*  Purpose:
*     Implement the C interface to the PDA library.

*  Language:
*     Starlink C

*  Description:
*     This module implements C-callable wrappers for the public
*     routines in the PDA library. The interface to these wrappers
*     is defined in pda.h.

*  Notes:
*     - Given the size of the PDA library, providing a complete C
*     interface is probably not worth the effort. Instead, I suggest that
*     people who want to use PDA from C extend this file (and
*     pda.h) to include any functions which they need but which are
*     not already included.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2007-2009 Science and Technology Facilities Council.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S Berry
*     PWD: Peter W. Draper
*     {enter_new_authors_here}

*  History:
*     12-OCT-2005 (DSB):
*        Original version.
*     31-OCT-2005 (DSB):
*        Added pdaRand and pdaRnnor.
*     20-APR-2007 (PWD):
*        Added pda_d1mach.
*     21-OCT-2009 (DSB):
*        Added pdaRnpoi.
*     {enter_further_changes_here}

*-
*/

/* Header files. */
/* ============= */
#include "f77.h"
#include "pda.h"


/* Module variables: */
/* ================= */
static void (*this_calcf)( int, double *, int *, double * );
static void (*this_calcg)( int, double *, int *, double * );

/* Local functions for interfacing C and F77 service routines */
/* ========================================================== */
F77_SUBROUTINE(CALCF)( INTEGER(N), DOUBLE_ARRAY(X), INTEGER(NF),
                       DOUBLE(F), INTEGER_ARRAY(UIPARM), DOUBLE_ARRAY(URPARM),
                       SUBROUTINE(UFPARM) ){
   GENPTR_INTEGER(N)
   GENPTR_DOUBLE_ARRAY(X)
   GENPTR_INTEGER(NF)
   GENPTR_DOUBLE(F)
   int n;
   int nf;
   double f;

   F77_IMPORT_INTEGER(*N,n);
   F77_IMPORT_INTEGER(*NF,nf);

   (*this_calcf)( n, X, &nf, &f );

   F77_EXPORT_INTEGER(nf,*NF);
   F77_EXPORT_DOUBLE(f,*F);

}

F77_SUBROUTINE(CALCG)( INTEGER(N), DOUBLE_ARRAY(X), INTEGER(NF),
                       DOUBLE_ARRAY(G), INTEGER_ARRAY(UIPARM), DOUBLE_ARRAY(URPARM),
                       SUBROUTINE(UFPARM) ){
   GENPTR_INTEGER(N)
   GENPTR_DOUBLE_ARRAY(X)
   GENPTR_INTEGER(NF)
   GENPTR_DOUBLE_ARRAY(G)
   int n;
   int nf;

   F77_IMPORT_INTEGER(*N,n);
   F77_IMPORT_INTEGER(*NF,nf);

   (*this_calcg)( n, X, &nf, G );

   F77_EXPORT_INTEGER(nf,*NF);

}



/* Wrapper function implementations. */
/* ================================= */

F77_SUBROUTINE(pda_sumsl)( INTEGER(N),
                           DOUBLE_ARRAY(D),
                           DOUBLE_ARRAY(X),
                           SUBROUTINE(CALCF),
                           SUBROUTINE(CALCG),
                           INTEGER_ARRAY(IV),
                           INTEGER(LIV),
                           INTEGER(LV),
                           DOUBLE_ARRAY(V),
                           INTEGER_ARRAY(UIPARM),
                           DOUBLE_ARRAY(URPARM),
                           SUBROUTINE(UFPARM) );

/* The C interface for PDA_SUMSL omits the UIPARM, URPARM and UFPARM
   parameters of PDA_SUMSL. Communication between the calling program and
   the "calcf" and "calcg" functions should instead be done using
   external variables. Likewise the supplied "calcf" and "calcg" functions
   should not include "uiparm", "urparm" or "ufparm" parameters. */

void pdaSumsl( int n, double *d, double *x,
               void (*calcf)( int, double *, int *, double * ),
               void (*calcg)( int, double *, int *, double * ),
               int *iv, int liv, int lv, double *v ){

   DECLARE_INTEGER(N);
   DECLARE_DOUBLE_ARRAY_DYN(D);
   DECLARE_DOUBLE_ARRAY_DYN(X);
   DECLARE_INTEGER(LIV);
   DECLARE_INTEGER_ARRAY_DYN(IV);
   DECLARE_INTEGER(LV);
   DECLARE_DOUBLE_ARRAY_DYN(V);
   DECLARE_INTEGER_ARRAY_DYN(UIPARM);
   DECLARE_DOUBLE_ARRAY_DYN(URPARM);

   F77_EXPORT_INTEGER( n, N );

   F77_CREATE_DOUBLE_ARRAY( D, n );
   F77_EXPORT_DOUBLE_ARRAY( d, D, n );

   F77_CREATE_DOUBLE_ARRAY( X, n );
   F77_EXPORT_DOUBLE_ARRAY( x, X, n );

   this_calcf = calcf;
   this_calcg = calcg;

   F77_EXPORT_INTEGER( liv, LIV );
   F77_CREATE_INTEGER_ARRAY( IV, liv );
   F77_EXPORT_INTEGER_ARRAY( iv, IV, liv );

   F77_EXPORT_INTEGER( lv, LV );
   F77_CREATE_DOUBLE_ARRAY( V, lv );
   F77_EXPORT_DOUBLE_ARRAY( v, V, lv );

   F77_CREATE_INTEGER_ARRAY( UIPARM, 1 );
   F77_CREATE_DOUBLE_ARRAY( URPARM, 1 );

   F77_LOCK( F77_CALL(pda_sumsl)( INTEGER_ARG(&N),
                        DOUBLE_ARRAY_ARG(D),
                        DOUBLE_ARRAY_ARG(X),
                        F77_EXTERNAL_NAME(CALCF),
                        F77_EXTERNAL_NAME(CALCG),
                        INTEGER_ARRAY_ARG(IV),
                        INTEGER_ARG(&LIV),
                        INTEGER_ARG(&LV),
                        DOUBLE_ARRAY_ARG(V),
                        INTEGER_ARRAY_ARG(UIPARM),
                        DOUBLE_ARRAY_ARG(URPARM),
                        F77_EXTERNAL_NAME(CALCF) ); )

   F77_IMPORT_DOUBLE_ARRAY( D, d, n );
   F77_FREE_DOUBLE( D );

   F77_IMPORT_DOUBLE_ARRAY( X, x, n );
   F77_FREE_DOUBLE( X );

   F77_IMPORT_INTEGER_ARRAY( IV, iv, liv );
   F77_FREE_INTEGER( IV );

   F77_IMPORT_DOUBLE_ARRAY( V, v, lv );
   F77_FREE_DOUBLE( V );

   F77_FREE_INTEGER( UIPARM );
   F77_FREE_DOUBLE( URPARM );

   return;
}




F77_REAL_FUNCTION(pda_rand)( REAL(X) );

float pdaRand(){
   DECLARE_REAL(X);
   float result;
   X = 0;
   F77_LOCK( result = F77_CALL(pda_rand)( REAL_ARG(&X) ); )
   return result;
}



F77_REAL_FUNCTION(pda_rnnor)( REAL(MEAN), REAL(SIGMA) );

float pdaRnnor( float mean, float sigma ){
   DECLARE_REAL(MEAN);
   DECLARE_REAL(SIGMA);
   float result;
   MEAN = mean;
   SIGMA = sigma;
   F77_LOCK( result = F77_CALL(pda_rnnor)( REAL_ARG(&MEAN), REAL_ARG(&SIGMA) ); )
   return result;
}


F77_INTEGER_FUNCTION(pda_rnpoi)( REAL(MEAN) );

int pdaRnpoi( float mean ){
   DECLARE_REAL(MEAN);
   float result;
   MEAN = mean;
   F77_LOCK( result = F77_CALL(pda_rnpoi)( REAL_ARG(&MEAN) ); )
   return result;
}


F77_SUBROUTINE(pda_deflt)( INTEGER(ALG),
                           INTEGER_ARRAY(IV),
                           INTEGER(LIV),
                           INTEGER(LV),
                           DOUBLE_ARRAY(V));

void pdaDeflt( int alg, int *iv, int liv, int lv, double *v){
   DECLARE_INTEGER(ALG);
   DECLARE_INTEGER_ARRAY_DYN(IV);
   DECLARE_INTEGER(LIV);
   DECLARE_INTEGER(LV);
   DECLARE_DOUBLE_ARRAY_DYN(V);

   F77_EXPORT_INTEGER( alg, ALG );

   F77_EXPORT_INTEGER( liv, LIV );
   F77_CREATE_INTEGER_ARRAY( IV, liv );
   F77_EXPORT_INTEGER_ARRAY( iv, IV, liv );

   F77_EXPORT_INTEGER( lv, LV );
   F77_CREATE_DOUBLE_ARRAY( V, lv );
   F77_EXPORT_DOUBLE_ARRAY( v, V, lv );

   F77_LOCK( F77_CALL(pda_deflt)( INTEGER_ARG(&ALG),
                        INTEGER_ARRAY_ARG(IV),
                        INTEGER_ARG(&LIV),
                        INTEGER_ARG(&LV),
                        DOUBLE_ARRAY_ARG(V) ); )

   F77_IMPORT_INTEGER_ARRAY( IV, iv, liv );
   F77_FREE_INTEGER( IV );

   F77_IMPORT_DOUBLE_ARRAY( V, v, lv );
   F77_FREE_DOUBLE( V );

   return;
}

/* Fortran access to C routines */
/* ============================ */

F77_DOUBLE_FUNCTION(pda_d1mach)( INTEGER(i) ) {
    GENPTR_INTEGER(i)
    F77_DOUBLE_TYPE(result);
    result = pdaD1mach( *i );
    return result;
}


