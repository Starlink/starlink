#if defined(USE_PTHREADS)
/*
*  Name:
*     sla_r.c

*  Purpose:
*     Implement a thread-safe C interface to the Fortran SLA library.

*  Description:
*     This file implements a thread-safe C interface to the Fortran 
*     SLA library. 

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*     
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry ()

*  History:
*     18-JUL-2008 (DSB):
*        Original version.
*-
*/

/* Header files. */
/* ============= */
#include "sla.h"                
#include "sla_r.h"                
#include <pthread.h>


void slaAddet_r( double rm, double dm, double eq, double *rc, double *dc ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaAddet( rm, dm, eq, rc, dc );
   pthread_mutex_unlock( &mutex );
}

double slaAirmas_r( double zd ){
   double result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaAirmas(  zd);
   pthread_mutex_unlock( &mutex );
   return result;
}

void slaAmp_r( double ra, double da, double date, double eq,
              double *rm, double *dm ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaAmp( ra, da, date, eq, rm, dm );
   pthread_mutex_unlock( &mutex );
}

void slaAmpqk_r( double ra, double da, double amprms[21],
                double *rm, double *dm ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaAmpqk( ra, da, amprms, rm, dm );
   pthread_mutex_unlock( &mutex );
}

void slaCaldj_r( int iy, int im, int id, double *djm, int *j ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaCaldj( iy, im, id, djm, j );
   pthread_mutex_unlock( &mutex );
}

void slaCldj_r( int iy, int im, int id, double *djm, int *j ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaCldj( iy, im, id, djm, j );
   pthread_mutex_unlock( &mutex );
}

void slaDaf2r_r( int ideg, int iamin, double asec, double *rad, int *j ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDaf2r( ideg, iamin, asec, rad, j );
   pthread_mutex_unlock( &mutex );
}

void slaDafin_r( const char *string, int *iptr, double *a, int *j ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDafin( string, iptr, a, j );
   pthread_mutex_unlock( &mutex );
}

double slaDat_r( double dju ){
   double result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaDat(  dju);
   pthread_mutex_unlock( &mutex );
   return result;
}

void slaDav2m_r( double axvec[3], double rmat[3][3] ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDav2m( axvec, rmat );
   pthread_mutex_unlock( &mutex );
}

double slaDbear_r( double a1, double b1, double a2, double b2 ){
   double result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaDbear(  a1, b1, a2, b2);
   pthread_mutex_unlock( &mutex );
   return result;
}

void slaDcc2s_r( double v[3], double *a, double *b ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDcc2s( v, a, b );
   pthread_mutex_unlock( &mutex );
}

void slaDcs2c_r( double a, double b, double v[3] ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDcs2c( a, b, v );
   pthread_mutex_unlock( &mutex );
}

void slaDd2tf_r( int ndp, double days, char *sign, int ihmsf[4] ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDd2tf( ndp, days, sign, ihmsf );
   pthread_mutex_unlock( &mutex );
}

void slaDe2h_r( double ha, double dec, double phi,
               double *az, double *el ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDe2h( ha, dec, phi, az, el );
   pthread_mutex_unlock( &mutex );
}

void slaDeuler_r( const char *order, double phi, double theta, double psi,
                 double rmat[3][3] ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDeuler( order, phi, theta, psi, rmat );
   pthread_mutex_unlock( &mutex );
}

void slaDh2e_r( double az, double el, double phi, double *ha, double *dec){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDh2e( az, el, phi, ha, dec );
   pthread_mutex_unlock( &mutex );
}

void slaDimxv_r( double dm[3][3], double va[3], double vb[3] ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDimxv( dm, va, vb );
   pthread_mutex_unlock( &mutex );
}

void slaDjcal_r( int ndp, double djm, int iymdf[4], int *j ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDjcal( ndp, djm, iymdf, j );
   pthread_mutex_unlock( &mutex );
}

void slaDjcl_r( double djm, int *iy, int *im, int *id, double *fd, int *j ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDjcl( djm, iy, im, id, fd, j );
   pthread_mutex_unlock( &mutex );
}

void slaDmat_r( int n, double *a, double *y, double *d, int *jf, int *iw ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDmat( n, a, y, d, jf, iw );
   pthread_mutex_unlock( &mutex );
}

void slaDmxm_r( double a[3][3], double b[3][3], double c[3][3] ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDmxm( a, b, c );
   pthread_mutex_unlock( &mutex );
}

void slaDmxv_r( double dm[3][3], double va[3], double vb[3] ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDmxv( dm, va, vb );
   pthread_mutex_unlock( &mutex );
}

void slaDr2af_r( int ndp, double angle, char *sign, int idmsf[4] ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDr2af( ndp, angle, sign, idmsf );
   pthread_mutex_unlock( &mutex );
}

void slaDr2tf_r( int ndp, double angle, char *sign, int ihmsf[4] ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDr2tf( ndp, angle, sign, ihmsf );
   pthread_mutex_unlock( &mutex );
}

double slaDrange_r( double angle ){
   double result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaDrange(  angle);
   pthread_mutex_unlock( &mutex );
   return result;
}

double slaDranrm_r( double angle ){
   double result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaDranrm(  angle);
   pthread_mutex_unlock( &mutex );
   return result;
}

double slaDsep_r( double a1, double b1, double a2, double b2 ){
   double result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaDsep(  a1, b1, a2, b2);
   pthread_mutex_unlock( &mutex );
   return result;
}

double slaDt_r( double epoch ){
   double result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaDt(  epoch);
   pthread_mutex_unlock( &mutex );
   return result;
}

void slaDtf2d_r( int ihour, int imin, double sec, double *days, int *j ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDtf2d( ihour, imin, sec, days, j );
   pthread_mutex_unlock( &mutex );
}

void slaDtf2r_r( int ihour, int imin, double sec, double *rad, int *j ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDtf2r( ihour, imin, sec, rad, j );
   pthread_mutex_unlock( &mutex );
}

double slaDtt_r( double dju ){
   double result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaDtt(  dju);
   pthread_mutex_unlock( &mutex );
   return result;
}

double slaDvdv_r( double va[3], double vb[3] ){
   double result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaDvdv(  va, vb);
   pthread_mutex_unlock( &mutex );
   return result;
}

void slaDvn_r( double v[3], double uv[3], double *vm ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDvn( v, uv, vm );
   pthread_mutex_unlock( &mutex );
}

void slaDvxv_r( double va[3], double vb[3], double vc[3] ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaDvxv( va, vb, vc );
   pthread_mutex_unlock( &mutex );
}

void slaEcmat_r( double date, double rmat[3][3] ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaEcmat( date, rmat );
   pthread_mutex_unlock( &mutex );
}

double slaEpb_r( double date ){
   double result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaEpb(  date);
   pthread_mutex_unlock( &mutex );
   return result;
}

double slaEpb2d_r( double epb ){
   double result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaEpb2d(  epb);
   pthread_mutex_unlock( &mutex );
   return result;
}

double slaEpj_r( double date ){
   double result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaEpj(  date);
   pthread_mutex_unlock( &mutex );
   return result;
}

double slaEpj2d_r( double epj ){
   double result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaEpj2d(  epj);
   pthread_mutex_unlock( &mutex );
   return result;
}

double slaEqeqx_r( double date ){
   double result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaEqeqx(  date);
   pthread_mutex_unlock( &mutex );
   return result;
}

void slaEqgal_r( double dr, double dd, double *dl, double *db ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaEqgal( dr, dd, dl, db );
   pthread_mutex_unlock( &mutex );
}

void slaEvp_r( double date, double deqx,
              double dvb[3], double dpb[3],
              double dvh[3], double dph[3] ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaEvp( date, deqx, dvb, dpb, dvh, dph );
   pthread_mutex_unlock( &mutex );
}

void slaFk45z_r( double r1950, double d1950, double bepoch,
                double *r2000, double *d2000 ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaFk45z( r1950, d1950, bepoch, r2000, d2000 );
   pthread_mutex_unlock( &mutex );
}

void slaFk54z_r( double r2000, double d2000, double bepoch,
                double *r1950, double *d1950,
                double *dr1950, double *dd1950 ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaFk54z( r2000, d2000, bepoch, r1950, d1950, dr1950, dd1950 );
   pthread_mutex_unlock( &mutex );
}

void slaFk5hz_r( double r5, double d5, double epoch,
                double *rh, double *dh ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaFk5hz( r5, d5, epoch, rh, dh );
   pthread_mutex_unlock( &mutex );
}

void slaGaleq_r( double dl, double db, double *dr, double *dd ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaGaleq( dl, db, dr, dd );
   pthread_mutex_unlock( &mutex );
}

void slaGalsup_r( double dl, double db, double *dsl, double *dsb ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaGalsup( dl, db, dsl, dsb );
   pthread_mutex_unlock( &mutex );
}

void slaGeoc_r( double p, double h, double *r, double *z ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaGeoc( p, h, r, z );
   pthread_mutex_unlock( &mutex );
}

double slaGmst_r( double ut1 ){
   double result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaGmst(  ut1);
   pthread_mutex_unlock( &mutex );
   return result;
}

void slaHfk5z_r( double rh, double dh, double epoch,
                double *r5, double *d5, double *dr5, double *dd5 ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaHfk5z( rh, dh, epoch, r5, d5, dr5, dd5 );
   pthread_mutex_unlock( &mutex );
}

void slaMap_r( double rm, double dm, double pr, double pd,
              double px, double rv, double eq, double date,
              double *ra, double *da ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaMap( rm, dm, pr, pd, px, rv, eq, date, ra, da );
   pthread_mutex_unlock( &mutex );
}

void slaMappa_r( double eq, double date, double amprms[21] ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaMappa( eq, date, amprms );
   pthread_mutex_unlock( &mutex );
}

void slaMapqkz_r( double rm, double dm, double amprms[21],
                 double *ra, double *da ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaMapqkz( rm, dm, amprms, ra, da );
   pthread_mutex_unlock( &mutex );
}

void slaOap_r( const char *type, double ob1, double ob2, double date,
              double dut, double elongm, double phim, double hm,
              double xp, double yp, double tdk, double pmb,
              double rh, double wl, double tlr,
              double *rap, double *dap ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaOap( type, ob1, ob2, date, dut, elongm, phim, hm, xp, yp, tdk, pmb, rh, wl, tlr, rap, dap );
   pthread_mutex_unlock( &mutex );
}

void slaObs_r( int n, char *c, char *name, double *w, double *p, double *h ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaObs( n, c, name, w, p, h );
   pthread_mutex_unlock( &mutex );
}

double slaPa_r( double ha, double dec, double phi ){
   double result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaPa(  ha, dec, phi);
   pthread_mutex_unlock( &mutex );
   return result;
}

void slaPertel_r(int jform, double date0, double date1,
                double epoch0, double orbi0, double anode0,
                double perih0, double aorq0, double e0, double am0,
                double *epoch1, double *orbi1, double *anode1,
                double *perih1, double *aorq1, double *e1, double *am1,
                int *jstat ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaPertel( jform, date0, date1, epoch0, orbi0, anode0, perih0, aorq0, e0, am0, epoch1, orbi1, anode1, perih1, aorq1, e1, am1, jstat );
   pthread_mutex_unlock( &mutex );
}

void slaPlante_r( double date, double elong, double phi, int jform,
                 double epoch, double orbinc, double anode, double perih,
                 double aorq, double e, double aorl, double dm,
                 double *ra, double *dec, double *r, int *jstat ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaPlante( date, elong, phi, jform, epoch, orbinc, anode, perih, aorq, e, aorl, dm, ra, dec, r, jstat );
   pthread_mutex_unlock( &mutex );
}

void slaPrebn_r( double bep0, double bep1, double rmatp[3][3] ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaPrebn( bep0, bep1, rmatp );
   pthread_mutex_unlock( &mutex );
}

void slaPrec_r( double ep0, double ep1, double rmatp[3][3] ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaPrec( ep0, ep1, rmatp );
   pthread_mutex_unlock( &mutex );
}

void slaRdplan_r( double date, int np, double elong, double phi,
                 double *ra, double *dec, double *diam ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaRdplan( date, np, elong, phi, ra, dec, diam );
   pthread_mutex_unlock( &mutex );
}

float slaRverot_r( float phi, float ra, float da, float st ){
   float result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaRverot(  phi, ra, da, st);
   pthread_mutex_unlock( &mutex );
   return result;
}

float slaRvgalc_r( float r2000, float d2000 ){
   float result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaRvgalc(  r2000, d2000);
   pthread_mutex_unlock( &mutex );
   return result;
}

float slaRvlg_r( float r2000, float d2000 ){
   float result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaRvlg(  r2000, d2000);
   pthread_mutex_unlock( &mutex );
   return result;
}

float slaRvlsrd_r( float r2000, float d2000 ){
   float result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaRvlsrd(  r2000, d2000);
   pthread_mutex_unlock( &mutex );
   return result;
}

float slaRvlsrk_r( float r2000, float d2000 ){
   float result;
   pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   result = slaRvlsrk(  r2000, d2000);
   pthread_mutex_unlock( &mutex );
   return result;
}

void slaSubet_r( double rc, double dc, double eq,
                double *rm, double *dm ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaSubet( rc, dc, eq, rm, dm );
   pthread_mutex_unlock( &mutex );
}

void slaSupgal_r( double dsl, double dsb, double *dl, double *db ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaSupgal( dsl, dsb, dl, db );
   pthread_mutex_unlock( &mutex );
}

void slaSvd_r( int m, int n, int mp, int np,
              double *a, double *w, double *v, double *work,
              int *jstat ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaSvd( m, n, mp, np, a, w, v, work, jstat );
   pthread_mutex_unlock( &mutex );
}

void slaSvdsol_r( int m, int n, int mp, int np,
                 double *b, double *u, double *w, double *v,
                 double *work, double *x ){
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
   pthread_mutex_lock( &mutex );
   slaSvdsol( m, n, mp, np, b, u, w, v, work, x );
   pthread_mutex_unlock( &mutex );
}
#endif
