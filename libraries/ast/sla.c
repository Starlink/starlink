/*
*  Name:
*     sla.c

*  Purpose:
*     Implement a C interface to the Fortran SLALIB library.

*  Description:
*     This file implements a C interface to the Fortran version of the
*     SLALIB library. It permits the AST library to function using either
*     version of SLALIB, so long as it is appropriately linked.

*  Notes:
*     This interface only supports the functions actually used in the AST
*     library.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)

*  History:
*     12-NOV-1996 (RFWS):
*        Original version.
*     28-APR-1997 (RFWS):
*        Added SLA_DJCAL.
*     26-SEP-1997 (DSB):
*        Added SLA_DD2TF, SLA_DJCL.
*/

/* Macros */
/* ====== */
#define astCLASS                 /* Make protected functions available */

/* Header files. */
/* ============= */
#include "memory.h"              /* Memory allocation facilities */
#include "error.h"               /* Error reporting facilities */
#include "f77.h"                 /* FORTRAN <-> C interface macros (SUN/209) */
#include "slalib.h"              /* Prototypes for C SLALIB functions */

/* Function implementations. */
/* ========================= */
/* Fortran routine prototype. */
F77_SUBROUTINE(sla_addet)( DOUBLE(RM),
                           DOUBLE(DM),
                           DOUBLE(EQ),
                           DOUBLE(RC),
                           DOUBLE(DC) );

/* C interface implementation. */
void slaAddet ( double rm, double dm, double eq, double *rc, double *dc ) {
   DECLARE_DOUBLE(RM);
   DECLARE_DOUBLE(DM);
   DECLARE_DOUBLE(EQ);
   DECLARE_DOUBLE(RC);
   DECLARE_DOUBLE(DC);
   RM = rm;
   DM = dm;
   EQ = eq;
   F77_CALL(sla_addet)( DOUBLE_ARG(&RM),
                        DOUBLE_ARG(&DM),
                        DOUBLE_ARG(&EQ),
                        DOUBLE_ARG(&RC),
                        DOUBLE_ARG(&DC) );
   *rc = RC;
   *dc = DC;
}

/* etc... */
F77_SUBROUTINE(sla_ampqk)( DOUBLE(RA),
                           DOUBLE(DA),
                           DOUBLE_ARRAY(AMPRMS),
                           DOUBLE(RM),
                           DOUBLE(DM) );

void slaAmpqk ( double ra, double da, double amprms[21],
                double *rm, double *dm ) {
   DECLARE_DOUBLE(RA);
   DECLARE_DOUBLE(DA);
   DECLARE_DOUBLE_ARRAY(AMPRMS,21);
   DECLARE_DOUBLE(RM);
   DECLARE_DOUBLE(DM);
   int i;
   RA = ra;
   DA = da;
   for ( i = 0; i < 21; i++ ) AMPRMS[ i ] = amprms[ i ];
   F77_CALL(sla_ampqk)( DOUBLE_ARG(&RA),
                        DOUBLE_ARG(&DA),
                        DOUBLE_ARRAY_ARG(AMPRMS),
                        DOUBLE_ARG(&RM),
                        DOUBLE_ARG(&DM) );
   *rm = RM;
   *dm = DM;
}

F77_SUBROUTINE(sla_caldj)( INTEGER(IY),
                           INTEGER(IM),
                           INTEGER(ID),
                           DOUBLE(DJM),
                           INTEGER(J) );

void slaCaldj ( int iy, int im, int id, double *djm, int *j ) {
   DECLARE_INTEGER(IY);
   DECLARE_INTEGER(IM);
   DECLARE_INTEGER(ID);
   DECLARE_DOUBLE(DJM);
   DECLARE_INTEGER(J);
   IY = iy;
   IM = im;
   ID = id;
   F77_CALL(sla_caldj)( INTEGER_ARG(&IY),
                        INTEGER_ARG(&IM),
                        INTEGER_ARG(&ID),
                        DOUBLE_ARG(&DJM),
                        INTEGER_ARG(&J) );
   *djm = DJM;
   *j = J;
}

F77_SUBROUTINE(sla_daf2r)( INTEGER(IDEG),
                           INTEGER(IAMIN),
                           DOUBLE(ASEC),
                           DOUBLE(RAD),
                           INTEGER(J) );

void slaDaf2r ( int ideg, int iamin, double asec, double *rad, int *j ) {
   DECLARE_INTEGER(IDEG);
   DECLARE_INTEGER(IAMIN);
   DECLARE_DOUBLE(ASEC);
   DECLARE_DOUBLE(RAD);
   DECLARE_INTEGER(J);
   IDEG = ideg;
   IAMIN = iamin;
   ASEC = asec;
   F77_CALL(sla_daf2r)( INTEGER_ARG(&IDEG),
                        INTEGER_ARG(&IAMIN),
                        DOUBLE_ARG(&ASEC),
                        DOUBLE_ARG(&RAD),
                        INTEGER_ARG(&J) );
   *rad = RAD;
   *j = J;
}

F77_SUBROUTINE(sla_dav2m)( DOUBLE_ARRAY(AXVEC),
                           DOUBLE_ARRAY(RMAT) );

void slaDav2m ( double axvec[3], double rmat[3][3] ) {
   DECLARE_DOUBLE_ARRAY(AXVEC,3);
   DECLARE_DOUBLE_ARRAY(RMAT,9);
   int i;
   int j;
   for ( i = 0; i < 3; i++ ) AXVEC[ i ] = axvec[ i ];
   F77_CALL(sla_dav2m)( DOUBLE_ARRAY_ARG(AXVEC),
                        DOUBLE_ARRAY_ARG(RMAT) );
   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 3; j++ ) rmat[ i ][ j ] = RMAT[ i + 3 * j ];
   }
}

F77_SUBROUTINE(sla_dcc2s)( DOUBLE_ARRAY(V),
                           DOUBLE(A),
                           DOUBLE(B) );

void slaDcc2s ( double v[3], double *a, double *b ) {
   DECLARE_DOUBLE_ARRAY(V,3);
   DECLARE_DOUBLE(A);
   DECLARE_DOUBLE(B);
   int i;
   for ( i = 0; i < 3; i++ ) V[ i ] = v[ i ];
   F77_CALL(sla_dcc2s)( DOUBLE_ARRAY_ARG(V),
                        DOUBLE_ARG(&A),
                        DOUBLE_ARG(&B) );
   *a = A;
   *b = B;
}

F77_SUBROUTINE(sla_dcs2c)( DOUBLE(A),
                           DOUBLE(B),
                           DOUBLE_ARRAY(V) );

void slaDcs2c ( double a, double b, double v[3] ) {
   DECLARE_DOUBLE(A);
   DECLARE_DOUBLE(B);
   DECLARE_DOUBLE_ARRAY(V,3);
   int i;
   A = a;
   B = b;
   F77_CALL(sla_dcs2c)( DOUBLE_ARG(&A),
                        DOUBLE_ARG(&B),
                        DOUBLE_ARRAY_ARG(V) );
   for ( i = 0; i < 3; i++ ) v[ i ] = V[ i ];
}

F77_SUBROUTINE(sla_dd2tf)( INTEGER(NDP),
                           DOUBLE(DAYS),
                           CHARACTER(SIGN),
                           INTEGER_ARRAY(IHMSF)
                           TRAIL(SIGN) );

void slaDd2tf ( int ndp, double days, char *sign, int ihmsf[4] ) {
   DECLARE_INTEGER(NDP);
   DECLARE_DOUBLE(DAYS);
   DECLARE_CHARACTER(SIGN,2);
   DECLARE_INTEGER_ARRAY(IHMSF,4);
   int i;

   NDP = ndp;   
   DAYS = days;
   F77_CALL(sla_dd2tf)( INTEGER_ARG(&NDP),
                        DOUBLE_ARG(&DAYS),
                        CHARACTER_ARG(SIGN),
                        INTEGER_ARRAY_ARG(IHMSF) 
                        TRAIL_ARG(SIGN) );
   sign[0] = SIGN[0];
   sign[1] = 0;
   for ( i = 0; i < 4; i++ ) ihmsf[ i ] = IHMSF[ i ];
}

F77_SUBROUTINE(sla_dimxv)( DOUBLE_ARRAY(DM),
                           DOUBLE_ARRAY(VA),
                           DOUBLE_ARRAY(VB) );

void slaDimxv ( double dm[3][3], double va[3], double vb[3] ) {
   DECLARE_DOUBLE_ARRAY(DM,9);
   DECLARE_DOUBLE_ARRAY(VA,3);
   DECLARE_DOUBLE_ARRAY(VB,3);
   int i;
   int j;
   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 3; j++ ) DM[ i + j * 3 ] = dm[ i ][ j ];
      VA[ i ] = va[ i ];
   }
   F77_CALL(sla_dimxv)( DOUBLE_ARRAY_ARG(DM),
                        DOUBLE_ARRAY_ARG(VA),
                        DOUBLE_ARRAY_ARG(VB) );
   for ( i = 0; i < 3; i++ ) vb[ i ] = VB[ i ];
}

F77_SUBROUTINE(sla_djcal)( INTEGER(NDP),
                           DOUBLE(DJM),
                           INTEGER_ARRAY(IYMDF),
                           INTEGER(J) );

void slaDjcal ( int ndp, double djm, int iymdf[ 4 ], int *j ) {
   DECLARE_INTEGER(NDP);
   DECLARE_DOUBLE(DJM);
   DECLARE_INTEGER_ARRAY(IYMDF,4);
   DECLARE_INTEGER(J);
   int i;

   NDP = ndp;
   DJM = djm;
   F77_CALL(sla_djcal)( INTEGER_ARG(&NDP),
                        DOUBLE_ARG(&DJM),
                        INTEGER_ARRAY_ARG(IYMDF),
                        INTEGER_ARG(&J) );
   for ( i = 0; i < 4; i++ ) iymdf[ i ] = IYMDF[ i ];
   *j = J;
}

F77_SUBROUTINE(sla_djcl)( DOUBLE(DJM),
                          INTEGER(IY),
                          INTEGER(IM),
                          INTEGER(ID),
                          DOUBLE(FD),
                          INTEGER(J) );

void slaDjcl ( double djm, int *iy, int *im, int *id, double *fd, int *j ) {
   DECLARE_DOUBLE(DJM);
   DECLARE_INTEGER(IY);
   DECLARE_INTEGER(IM);
   DECLARE_INTEGER(ID);
   DECLARE_DOUBLE(FD);
   DECLARE_INTEGER(J);

   DJM = djm;
   F77_CALL(sla_djcl)( DOUBLE_ARG(&DJM),
                       INTEGER_ARG(&IY),
                       INTEGER_ARG(&IM),
                       INTEGER_ARG(&ID),
                       DOUBLE_ARG(&FD),
                       INTEGER_ARG(&J) );
   *iy = IY;
   *im = IM;
   *id = ID;
   *fd = FD;
   *j = J;
}

F77_SUBROUTINE(sla_dmat)( INTEGER(N),
                          DOUBLE_ARRAY(A),
                          DOUBLE_ARRAY(Y),
                          DOUBLE(D),
                          INTEGER(JF),
                          INTEGER_ARRAY(IW) );

void slaDmat ( int n, double *a, double *y, double *d, int *jf, int *iw ) {
   DECLARE_INTEGER(N);
   F77_DOUBLE_TYPE *A;
   F77_DOUBLE_TYPE *Y;
   DECLARE_DOUBLE(D);
   DECLARE_INTEGER(JF);
   F77_INTEGER_TYPE *IW;
   int i;
   int j;
   A = astMalloc( sizeof( F77_DOUBLE_TYPE ) * (size_t) ( n * n ) );
   Y = astMalloc( sizeof( F77_DOUBLE_TYPE ) * (size_t) n );
   if ( sizeof( F77_INTEGER_TYPE ) > sizeof( int ) ) {
      IW = astMalloc( sizeof( F77_INTEGER_TYPE ) * (size_t) n );
   } else {
      IW = (F77_INTEGER_TYPE *) iw;
   }
   if ( astOK ) {
      N = n;
      for ( i = 0; i < n; i++ ) {
         for ( j = 0; j < n; j++ ) A[ i + n * j ] = a[ n * i + j ];
         Y[ i ] = y[ i ];
      }
      F77_CALL(sla_dmat)( INTEGER_ARG(&N), DOUBLE_ARRAY_ARG(A),
                          DOUBLE_ARRAY_ARG(Y), DOUBLE_ARG(&D),
                          INTEGER_ARG(&JF), INTEGER_ARG(IW) );
      for ( i = 0; i < n; i++ ) {
         for ( j = 0; j < n; j++ ) a[ n * i + j ] = A[ i + n * j ];
         y[ i ] = Y[ i ];
      }
      *d = D;
      *jf = JF;
   }
   A = astFree( A );
   Y = astFree( Y );
   if ( sizeof( F77_INTEGER_TYPE ) > sizeof( int ) ) IW = astFree( IW );
}

F77_SUBROUTINE(sla_dmxm)( DOUBLE_ARRAY(A),
                          DOUBLE_ARRAY(B),
                          DOUBLE_ARRAY(C) );

void slaDmxm ( double a[3][3], double b[3][3], double c[3][3] ) {
   DECLARE_DOUBLE_ARRAY(A,9);
   DECLARE_DOUBLE_ARRAY(B,9);
   DECLARE_DOUBLE_ARRAY(C,9);
   int i;
   int j;
   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 3; j++ ) {
         A[ i + 3 * j ] = a[ i ][ j ];
         B[ i + 3 * j ] = b[ i ][ j ];
      }
   }
   F77_CALL(sla_dmxm)( DOUBLE_ARRAY_ARG(A),
                       DOUBLE_ARRAY_ARG(B),
                       DOUBLE_ARRAY_ARG(C) );
   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 3; j++ ) c[ i ][ j ] = C[ i + 3 * j ];
   }
}

F77_SUBROUTINE(sla_dmxv)( DOUBLE_ARRAY(DM),
                          DOUBLE_ARRAY(VA),
                          DOUBLE_ARRAY(VB) );

void slaDmxv ( double dm[3][3], double va[3], double vb[3] ) {
   DECLARE_DOUBLE_ARRAY(DM,9);
   DECLARE_DOUBLE_ARRAY(VA,3);
   DECLARE_DOUBLE_ARRAY(VB,3);
   int i;
   int j;
   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 3; j++ ) DM[ i + 3 * j ] = dm[ i ][ j ];
      VA[ i ] = va[ i ];
   }
   F77_CALL(sla_dmxv)( DOUBLE_ARRAY_ARG(DM),
                       DOUBLE_ARRAY_ARG(VA),
                       DOUBLE_ARRAY_ARG(VB) );
   for ( i = 0; i < 3; i++ ) vb[ i ] = VB[ i ];
}

F77_DOUBLE_FUNCTION(sla_drange)( DOUBLE(ANGLE) );

double slaDrange ( double angle ) {
   DECLARE_DOUBLE(ANGLE);
   double result;
   ANGLE = angle;
   result = F77_CALL(sla_drange)( DOUBLE_ARG(&ANGLE) );
   return result;
}

F77_DOUBLE_FUNCTION(sla_dranrm)( DOUBLE(ANGLE) );

double slaDranrm ( double angle ) {
   DECLARE_DOUBLE(ANGLE);
   double result;
   ANGLE = angle;
   result = F77_CALL(sla_dranrm)( DOUBLE_ARG(&ANGLE) );
   return result;
}

F77_DOUBLE_FUNCTION(sla_dsep)( DOUBLE(A1),
                               DOUBLE(B1),
                               DOUBLE(A2),
                               DOUBLE(B2) );

double slaDsep ( double a1, double b1, double a2, double b2 ) {
   DECLARE_DOUBLE(A1);
   DECLARE_DOUBLE(B1);
   DECLARE_DOUBLE(A2);
   DECLARE_DOUBLE(B2);
   double result;
   A1 = a1;
   B1 = b1;
   A2 = a2;
   B2 = b2;
   result = F77_CALL(sla_dsep)( DOUBLE_ARG(&A1),
                                DOUBLE_ARG(&B1),
                                DOUBLE_ARG(&A2),
                                DOUBLE_ARG(&B2) );
   return result;
}

F77_SUBROUTINE(sla_dtf2d)( INTEGER(IHOUR),
                           INTEGER(IMIN),
                           DOUBLE(SEC),
                           DOUBLE(DAYS),
                           INTEGER(J) );

void slaDtf2d ( int ihour, int imin, double sec, double *days, int *j ) {
   DECLARE_INTEGER(IHOUR);
   DECLARE_INTEGER(IMIN);
   DECLARE_DOUBLE(SEC);
   DECLARE_DOUBLE(DAYS);
   DECLARE_INTEGER(J);
   IHOUR = ihour;
   IMIN = imin;
   SEC = sec;
   F77_CALL(sla_dtf2d)( INTEGER_ARG(&IHOUR),
                        INTEGER_ARG(&IMIN),
                        DOUBLE_ARG(&SEC),
                        DOUBLE_ARG(&DAYS),
                        INTEGER_ARG(&J) );
   *days = DAYS;
   *j = J;
}

F77_SUBROUTINE(sla_dtf2r)( INTEGER(IHOUR),
                           INTEGER(IMIN),
                           DOUBLE(SEC),
                           DOUBLE(RAD),
                           INTEGER(J) );

void slaDtf2r ( int ihour, int imin, double sec, double *rad, int *j ) {
   DECLARE_INTEGER(IHOUR);
   DECLARE_INTEGER(IMIN);
   DECLARE_DOUBLE(SEC);
   DECLARE_DOUBLE(RAD);
   DECLARE_INTEGER(J);
   IHOUR = ihour;
   IMIN = imin;
   SEC = sec;
   F77_CALL(sla_dtf2r)( INTEGER_ARG(&IHOUR),
                        INTEGER_ARG(&IMIN),
                        DOUBLE_ARG(&SEC),
                        DOUBLE_ARG(&RAD),
                        INTEGER_ARG(&J) );
   *rad = RAD;
   *j = J;
}

F77_SUBROUTINE(sla_dvn)( DOUBLE_ARRAY(V),
                         DOUBLE_ARRAY(UV),
                         DOUBLE(VM) );

void slaDvn ( double v[3], double uv[3], double *vm ) {
   DECLARE_DOUBLE_ARRAY(V,3);
   DECLARE_DOUBLE_ARRAY(UV,3);
   DECLARE_DOUBLE(VM);
   int i;
   for ( i = 0; i < 3; i++ ) V[ i ] = v[ i ];
   F77_CALL(sla_dvn)( DOUBLE_ARRAY_ARG(V),
                      DOUBLE_ARRAY_ARG(UV),
                      DOUBLE_ARG(&VM) );
   for ( i = 0; i < 3; i++ ) uv[ i ] = UV[ i ];
   *vm = VM;
}

F77_SUBROUTINE(sla_dvxv)( DOUBLE_ARRAY(VA),
                          DOUBLE_ARRAY(VB),
                          DOUBLE_ARRAY(VC) );

void slaDvxv ( double va[3], double vb[3], double vc[3] ) {
   DECLARE_DOUBLE_ARRAY(VA,3);
   DECLARE_DOUBLE_ARRAY(VB,3);
   DECLARE_DOUBLE_ARRAY(VC,3);
   int i;
   for ( i = 0; i < 3; i++ ) {
      VA[ i ] = va[ i ];
      VB[ i ] = vb[ i ];
   }
   F77_CALL(sla_dvxv)( DOUBLE_ARRAY_ARG(VA),
                       DOUBLE_ARRAY_ARG(VB),
                       DOUBLE_ARRAY_ARG(VC) );
   for ( i = 0; i < 3; i++ ) vc[ i ] = VC[ i ];
}

F77_SUBROUTINE(sla_ecmat)( DOUBLE(DATE),
                           DOUBLE_ARRAY(RMAT) );

void slaEcmat ( double date, double rmat[3][3] ) {
   DECLARE_DOUBLE(DATE);
   DECLARE_DOUBLE_ARRAY(RMAT,9);
   int i;
   int j;
   DATE = date;
   F77_CALL(sla_ecmat)( DOUBLE_ARG(&DATE),
                        DOUBLE_ARRAY_ARG(RMAT) );
   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 3; j++ ) rmat[ i ][ j ] = RMAT[ i + 3 * j ];
   }
}

F77_DOUBLE_FUNCTION(sla_epb)( DOUBLE(DATE) );

double slaEpb ( double date ) {
   DECLARE_DOUBLE(DATE);
   double result;
   DATE = date;
   result = F77_CALL(sla_epb)( DOUBLE_ARG(&DATE) );
   return result;
}

F77_DOUBLE_FUNCTION(sla_epb2d)( DOUBLE(EPB) );

double slaEpb2d ( double epb ) {
   DECLARE_DOUBLE(EPB);
   double result;
   EPB = epb;
   result = F77_CALL(sla_epb2d)( DOUBLE_ARG(&EPB) );
   return result;
}

F77_DOUBLE_FUNCTION(sla_epj)( DOUBLE(DATE) );

double slaEpj ( double date ) {
   DECLARE_DOUBLE(DATE);
   double result;
   DATE = date;
   result = F77_CALL(sla_epj)( DOUBLE_ARG(&DATE) );
   return result;
}

F77_DOUBLE_FUNCTION(sla_epj2d)( DOUBLE(EPJ) );

double slaEpj2d ( double epj ) {
   DECLARE_DOUBLE(EPJ);
   double result;
   EPJ = epj;
   result = F77_CALL(sla_epj2d)( DOUBLE_ARG(&EPJ) );
   return result;
}

F77_SUBROUTINE(sla_eqgal)( DOUBLE(DR),
                           DOUBLE(DD),
                           DOUBLE(DL),
                           DOUBLE(DB) );

void slaEqgal ( double dr, double dd, double *dl, double *db ) {
   DECLARE_DOUBLE(DR);
   DECLARE_DOUBLE(DD);
   DECLARE_DOUBLE(DL);
   DECLARE_DOUBLE(DB);
   DR = dr;
   DD = dd;
   F77_CALL(sla_eqgal)( DOUBLE_ARG(&DR),
                        DOUBLE_ARG(&DD),
                        DOUBLE_ARG(&DL),
                        DOUBLE_ARG(&DB) );
   *dl = DL;
   *db = DB;
}

F77_SUBROUTINE(sla_fk45z)( DOUBLE(R1950),
                           DOUBLE(D1950),
                           DOUBLE(BEPOCH),
                           DOUBLE(R2000),
                           DOUBLE(D2000) );

void slaFk45z ( double r1950, double d1950, double bepoch,
                double *r2000, double *d2000 ) {
   DECLARE_DOUBLE(R1950);
   DECLARE_DOUBLE(D1950);
   DECLARE_DOUBLE(BEPOCH);
   DECLARE_DOUBLE(R2000);
   DECLARE_DOUBLE(D2000);
   R1950 = r1950;
   D1950 = d1950;
   BEPOCH = bepoch;
   F77_CALL(sla_fk45z)( DOUBLE_ARG(&R1950),
                        DOUBLE_ARG(&D1950),
                        DOUBLE_ARG(&BEPOCH),
                        DOUBLE_ARG(&R2000),
                        DOUBLE_ARG(&D2000) );
   *r2000 = R2000;
   *d2000 = D2000;
}

F77_SUBROUTINE(sla_fk54z)( DOUBLE(R2000),
                           DOUBLE(D2000),
                           DOUBLE(BEPOCH),
                           DOUBLE(R1950),
                           DOUBLE(D1950),
                           DOUBLE(DR1950),
                           DOUBLE(DD1950) );

void slaFk54z ( double r2000, double d2000, double bepoch,
                double *r1950, double *d1950,
                double *dr1950, double *dd1950 ) {
   DECLARE_DOUBLE(R2000);
   DECLARE_DOUBLE(D2000);
   DECLARE_DOUBLE(BEPOCH);
   DECLARE_DOUBLE(R1950);
   DECLARE_DOUBLE(D1950);
   DECLARE_DOUBLE(DR1950);
   DECLARE_DOUBLE(DD1950);
   R2000 = r2000;
   D2000 = d2000;
   BEPOCH = bepoch;
   F77_CALL(sla_fk54z)( DOUBLE_ARG(&R2000),
                        DOUBLE_ARG(&D2000),
                        DOUBLE_ARG(&BEPOCH),
                        DOUBLE_ARG(&R1950),
                        DOUBLE_ARG(&D1950),
                        DOUBLE_ARG(&DR1950),
                        DOUBLE_ARG(&DD1950) );
   *r1950 = R1950;
   *d1950 = D1950;
   *dr1950 = DR1950;
   *dd1950 = DD1950;
}

F77_SUBROUTINE(sla_galeq)( DOUBLE(DL),
                           DOUBLE(DB),
                           DOUBLE(DR),
                           DOUBLE(DD) );

void slaGaleq ( double dl, double db, double *dr, double *dd ) {
   DECLARE_DOUBLE(DL);
   DECLARE_DOUBLE(DB);
   DECLARE_DOUBLE(DR);
   DECLARE_DOUBLE(DD);
   DL = dl;
   DB = db;
   F77_CALL(sla_galeq)( DOUBLE_ARG(&DL),
                        DOUBLE_ARG(&DB),
                        DOUBLE_ARG(&DR),
                        DOUBLE_ARG(&DD) );
   *dr = DR;
   *dd = DD;
}

F77_SUBROUTINE(sla_galsup)( DOUBLE(DL),
                            DOUBLE(DB),
                            DOUBLE(DSL),
                            DOUBLE(DSB) );

void slaGalsup ( double dl, double db, double *dsl, double *dsb ) {
   DECLARE_DOUBLE(DL);
   DECLARE_DOUBLE(DB);
   DECLARE_DOUBLE(DSL);
   DECLARE_DOUBLE(DSB);
   DL = dl;
   DB = db;
   F77_CALL(sla_galsup)( DOUBLE_ARG(&DL),
                         DOUBLE_ARG(&DB),
                         DOUBLE_ARG(&DSL),
                         DOUBLE_ARG(&DSB) );
   *dsl = DSL;
   *dsb = DSB;
}

F77_SUBROUTINE(sla_mappa)( DOUBLE(EQ),
                           DOUBLE(DATE),
                           DOUBLE_ARRAY(AMPRMS) );

void slaMappa ( double eq, double date, double amprms[21] ) {
   DECLARE_DOUBLE(EQ);
   DECLARE_DOUBLE(DATE);
   DECLARE_DOUBLE_ARRAY(AMPRMS,21);
   int i;
   EQ = eq;
   DATE = date;
   F77_CALL(sla_mappa)( DOUBLE_ARG(&EQ),
                        DOUBLE_ARG(&DATE),
                        DOUBLE_ARRAY_ARG(AMPRMS) );
   for ( i = 0; i < 21; i++ ) amprms[ i ] = AMPRMS[ i ];
}

F77_SUBROUTINE(sla_mapqkz)( DOUBLE(RM),
                            DOUBLE(DM),
                            DOUBLE_ARRAY(AMPRMS),
                            DOUBLE(RA),
                            DOUBLE(DA) );

void slaMapqkz ( double rm, double dm, double amprms[21],
                 double *ra, double *da ) {
   DECLARE_DOUBLE(RM);
   DECLARE_DOUBLE(DM);
   DECLARE_DOUBLE_ARRAY(AMPRMS,21);
   DECLARE_DOUBLE(RA);
   DECLARE_DOUBLE(DA);
   int i;
   RM = rm;
   DM = dm;
   for ( i = 0; i < 21; i++ ) AMPRMS[ i ] = amprms[ i ];
   F77_CALL(sla_mapqkz)( DOUBLE_ARG(&RM),
                         DOUBLE_ARG(&DM),
                         DOUBLE_ARRAY_ARG(AMPRMS),
                         DOUBLE_ARG(&RA),
                         DOUBLE_ARG(&DA) );
   *ra = RA;
   *da = DA;
}

F77_SUBROUTINE(sla_prebn)( DOUBLE(BEP0),
                           DOUBLE(BEP1),
                           DOUBLE_ARRAY(RMATP) );

void slaPrebn ( double bep0, double bep1, double rmatp[3][3] ) {
   DECLARE_DOUBLE(BEP0);
   DECLARE_DOUBLE(BEP1);
   DECLARE_DOUBLE_ARRAY(RMATP,9);
   int i;
   int j;
   BEP0 = bep0;
   BEP1 = bep1;
   F77_CALL(sla_prebn)( DOUBLE_ARG(&BEP0),
                        DOUBLE_ARG(&BEP1),
                        DOUBLE_ARRAY_ARG(RMATP) );
   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 3; j++ ) rmatp[ i ][ j ] = RMATP[ i + 3 * j ];
   }
}

F77_SUBROUTINE(sla_prec)( DOUBLE(EP0),
                          DOUBLE(EP1),
                          DOUBLE_ARRAY(RMATP) );

void slaPrec ( double ep0, double ep1, double rmatp[3][3] ) {
   DECLARE_DOUBLE(EP0);
   DECLARE_DOUBLE(EP1);
   DECLARE_DOUBLE_ARRAY(RMATP,9);
   int i;
   int j;
   EP0 = ep0;
   EP1 = ep1;
   F77_CALL(sla_prec)( DOUBLE_ARG(&EP0),
                       DOUBLE_ARG(&EP1),
                       DOUBLE_ARRAY_ARG(RMATP) );
   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 3; j++ ) rmatp[ i ][ j ] = RMATP[ i + 3 * j ];
   }
}

F77_SUBROUTINE(sla_subet)( DOUBLE(RC),
                           DOUBLE(DC),
                           DOUBLE(EQ),
                           DOUBLE(RM),
                           DOUBLE(DM) );

void slaSubet ( double rc, double dc, double eq, double *rm, double *dm ) {
   DECLARE_DOUBLE(RC);
   DECLARE_DOUBLE(DC);
   DECLARE_DOUBLE(EQ);
   DECLARE_DOUBLE(RM);
   DECLARE_DOUBLE(DM);
   RC = rc;
   DC = dc;
   EQ = eq;
   F77_CALL(sla_subet)( DOUBLE_ARG(&RC),
                        DOUBLE_ARG(&DC),
                        DOUBLE_ARG(&EQ),
                        DOUBLE_ARG(&RM),
                        DOUBLE_ARG(&DM) );
   *rm = RM;
   *dm = DM;
}

F77_SUBROUTINE(sla_supgal)( DOUBLE(DSL),
                            DOUBLE(DSB),
                            DOUBLE(DL),
                            DOUBLE(DB) );

void slaSupgal ( double dsl, double dsb, double *dl, double *db ) {
   DECLARE_DOUBLE(DSL);
   DECLARE_DOUBLE(DSB);
   DECLARE_DOUBLE(DL);
   DECLARE_DOUBLE(DB);
   DSL = dsl;
   DSB = dsb;
   F77_CALL(sla_supgal)( DOUBLE_ARG(&DSL),
                         DOUBLE_ARG(&DSB),
                         DOUBLE_ARG(&DL),
                         DOUBLE_ARG(&DB) );
   *dl = DL;
   *db = DB;
}
