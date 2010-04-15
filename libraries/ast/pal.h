#ifndef PALHDEF
#define PALHDEF
/*
**  Author:
**    Patrick Wallace  (ptw@tpsoft.demon.co.uk)
**
**  License:
**    This program is free software; you can redistribute it and/or modify
**    it under the terms of the GNU General Public License as published by
**    the Free Software Foundation; either version 2 of the License, or
**    (at your option) any later version.
**
**    This program is distributed in the hope that it will be useful,
**    but WITHOUT ANY WARRANTY; without even the implied warranty of
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**    GNU General Public License for more details.
**
**    You should have received a copy of the GNU General Public License
**    along with this program; if not, write to the Free Software
**    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
**    USA.
**
**  Last revision:   10 December 2002
**
*/

#ifdef __cplusplus
extern "C" {
#endif

#include <math.h>

void palSlaAddet ( double rm, double dm, double eq, double *rc, double *dc );

void palSlaAfin ( char *string, int *iptr, float *a, int *j );

double palSlaAirmas ( double zd );

void palSlaAltaz ( double ha, double dec, double phi,
                double *az, double *azd, double *azdd,
                double *el, double *eld, double *eldd,
                double *pa, double *pad, double *padd );

void palSlaAmp ( double ra, double da, double date, double eq,
              double *rm, double *dm );

void palSlaAmpqk ( double ra, double da, double amprms[21],
                double *rm, double *dm );

void palSlaAop ( double rap, double dap, double date, double dut,
              double elongm, double phim, double hm, double xp,
              double yp, double tdk, double pmb, double rh,
              double wl, double tlr,
              double *aob, double *zob, double *hob,
              double *dob, double *rob );

void palSlaAoppa ( double date, double dut, double elongm, double phim,
                double hm, double xp, double yp, double tdk, double pmb,
                double rh, double wl, double tlr, double aoprms[14] );

void palSlaAoppat ( double date, double aoprms[14] );

void palSlaAopqk ( double rap, double dap, double aoprms[14],
                double *aob, double *zob, double *hob,
                double *dob, double *rob );

void palSlaAtmdsp ( double tdk, double pmb, double rh, double wl1,
                 double a1, double b1, double wl2, double *a2, double *b2 );

void palSlaAv2m ( float axvec[3], float rmat[3][3] );

float palSlaBear ( float a1, float b1, float a2, float b2 );

void palSlaCaf2r ( int ideg, int iamin, float asec, float *rad, int *j );

void palSlaCaldj ( int iy, int im, int id, double *djm, int *j );

void palSlaCalyd ( int iy, int im, int id, int *ny, int *nd, int *j );

void palSlaCc2s ( float v[3], float *a, float *b );

void palSlaCc62s ( float v[6], float *a, float *b, float *r,
                float *ad, float *bd, float *rd );

void palSlaCd2tf ( int ndp, float days, char *sign, int ihmsf[4] );

void palSlaCldj ( int iy, int im, int id, double *djm, int *j );

void palSlaClyd ( int iy, int im, int id, int *ny, int *nd, int *jstat );

void palSlaCombn ( int nsel, int ncand, int list[], int *j );

void palSlaCr2af ( int ndp, float angle, char *sign, int idmsf[4] );

void palSlaCr2tf ( int ndp, float angle, char *sign, int ihmsf[4] );

void palSlaCs2c ( float a, float b, float v[3] );

void palSlaCs2c6 ( float a, float b, float r, float ad,
                float bd, float rd, float v[6] );

void palSlaCtf2d ( int ihour, int imin, float sec, float *days, int *j );

void palSlaCtf2r ( int ihour, int imin, float sec, float *rad, int *j );

void palSlaDaf2r ( int ideg, int iamin, double asec, double *rad, int *j );

void palSlaDafin ( char *string, int *iptr, double *a, int *j );

double palSlaDat ( double dju );

void palSlaDav2m ( double axvec[3], double rmat[3][3] );

double palSlaDbear ( double a1, double b1, double a2, double b2 );

void palSlaDbjin ( char *string, int *nstrt,
                double *dreslt, int *jf1, int *jf2 );

void palSlaDc62s ( double v[6], double *a, double *b, double *r,
                double *ad, double *bd, double *rd );

void palSlaDcc2s ( double v[3], double *a, double *b );

void palSlaDcmpf ( double coeffs[6], double *xz, double *yz, double *xs,
                double *ys, double *perp, double *orient );

void palSlaDcs2c ( double a, double b, double v[3] );

void palSlaDd2tf ( int ndp, double days, char *sign, int ihmsf[4] );

void palSlaDe2h ( double ha, double dec, double phi, double diurab,
               double *az, double *el );

void palSlaDeuler ( char *order, double phi, double theta, double psi,
                 double rmat[3][3] );

void palSlaDfltin ( char *string, int *nstrt, double *dreslt, int *jflag );

void palSlaDh2e( double az, double el, double phi, double diurab, double *ha,
                 double *dec);

void palSlaDimxv ( double dm[3][3], double va[3], double vb[3] );

void palSlaDjcal ( int ndp, double djm, int iymdf[4], int *j );

void palSlaDjcl ( double djm, int *iy, int *im, int *id, double *fd, int *j );

void palSlaDm2av ( double rmat[3][3], double axvec[3] );

void palSlaDmat ( int n, double *a, double *y, double *d, int *jf, int *iw );

void palSlaDmoon ( double date, double pv[6] );

void palSlaDmxm ( double a[3][3], double b[3][3], double c[3][3] );

void palSlaDmxv ( double dm[3][3], double va[3], double vb[3] );

double palSlaDpav ( double v1[3], double v2[3] );

void palSlaDr2af ( int ndp, double angle, char *sign, int idmsf[4] );

void palSlaDr2tf ( int ndp, double angle, char *sign, int ihmsf[4] );

double palSlaDrange ( double angle );

double palSlaDranrm ( double angle );

void palSlaDs2c6 ( double a, double b, double r, double ad, double bd,
                double rd, double v[6] );

void palSlaDs2tp ( double ra, double dec, double raz, double decz,
                double *xi, double *eta, int *j );

double palSlaDsep ( double a1, double b1, double a2, double b2 );

double palSlaDsepv ( double v1[3], double v2[3] );

double palSlaDt ( double epoch );

void palSlaDtf2d ( int ihour, int imin, double sec, double *days, int *j );

void palSlaDtf2r ( int ihour, int imin, double sec, double *rad, int *j );

void palSlaDtp2s ( double xi, double eta, double raz, double decz,
                double *ra, double *dec );

void palSlaDtp2v ( double xi, double eta, double v0[3], double v[3] );

void palSlaDtps2c ( double xi, double eta, double ra, double dec,
                 double *raz1, double *decz1,
                 double *raz2, double *decz2, int *n );

void palSlaDtpv2c ( double xi, double eta, double v[3],
                 double v01[3], double v02[3], int *n );

double palSlaDtt ( double dju );

void palSlaDv2tp ( double v[3], double v0[3], double *xi, double *eta, int *j );

double palSlaDvdv ( double va[3], double vb[3] );

void palSlaDvn ( double v[3], double uv[3], double *vm );

void palSlaDvxv ( double va[3], double vb[3], double vc[3] );

void palSlaE2h ( float ha, float dec, float phi, float *az, float *el );

void palSlaEarth ( int iy, int id, float fd, float posvel[6] );

void palSlaEcleq ( double dl, double db, double date, double *dr, double *dd );

void palSlaEcmat ( double date, double rmat[3][3] );

void palSlaEcor ( float rm, float dm, int iy, int id, float fd,
               float *rv, float *tl );

void palSlaEg50 ( double dr, double dd, double *dl, double *db );

void palSlaEl2ue ( double date, int jform, double epoch, double orbinc,
                double anode, double perih, double aorq, double e,
                double aorl, double dm, double u[], int *jstat );

double palSlaEpb ( double date );

double palSlaEpb2d ( double epb );

double palSlaEpco ( char k0, char k, double e );

double palSlaEpj ( double date );

double palSlaEpj2d ( double epj );

void palSlaEqecl ( double dr, double dd, double date, double *dl, double *db );

double palSlaEqeqx ( double date );

void palSlaEqgal ( double dr, double dd, double *dl, double *db );

void palSlaEtrms ( double ep, double ev[3] );

void palSlaEuler ( char *order, float phi, float theta, float psi,
                float rmat[3][3] );

void palSlaEvp ( double date, double deqx,
              double dvb[3], double dpb[3],
              double dvh[3], double dph[3] );

void palSlaFitxy ( int itype, int np, double xye[][2], double xym[][2],
                double coeffs[6], int *j );

void palSlaFk425 ( double r1950, double d1950, double dr1950,
                double dd1950, double p1950, double v1950,
                double *r2000, double *d2000, double *dr2000,
                double *dd2000, double *p2000, double *v2000 );

void palSlaFk45z ( double r1950, double d1950, double bepoch,
                double *r2000, double *d2000 );

void palSlaFk524 ( double r2000, double d2000, double dr2000,
                double dd2000, double p2000, double v2000,
                double *r1950, double *d1950, double *dr1950,
                double *dd1950, double *p1950, double *v1950 );

void palSlaFk52h ( double r5, double d5, double dr5, double dd5,
                double *dr, double *dh, double *drh, double *ddh );

void palSlaFk54z ( double r2000, double d2000, double bepoch,
                double *r1950, double *d1950,
                double *dr1950, double *dd1950 );

void palSlaFk5hz ( double r5, double d5, double epoch,
                double *rh, double *dh );

void palSlaFlotin ( char *string, int *nstrt, float *reslt, int *jflag );

void palSlaGaleq ( double dl, double db, double *dr, double *dd );

void palSlaGalsup ( double dl, double db, double *dsl, double *dsb );

void palSlaGe50 ( double dl, double db, double *dr, double *dd );

void palSlaGeoc ( double p, double h, double *r, double *z );

double palSlaGmst ( double ut1 );

double palSlaGmsta ( double date, double ut1 );

void palSlaH2e ( float az, float el, float phi, float *ha, float *dec );

void palSlaH2fk5 ( double dr, double dh, double drh, double ddh,
                double *r5, double *d5, double *dr5, double *dd5 );

void palSlaHfk5z ( double rh, double dh, double epoch,
                double *r5, double *d5, double *dr5, double *dd5 );

void palSlaImxv ( float rm[3][3], float va[3], float vb[3] );

void palSlaInt2in ( char *string, int *nstrt, int *ireslt, int *jflag );

void palSlaIntin ( char *string, int *nstrt, long *ireslt, int *jflag );

void palSlaInvf ( double fwds[6], double bkwds[6], int *j );

void palSlaKbj ( int jb, double e, char *k, int *j );

void palSlaM2av ( float rmat[3][3], float axvec[3] );

void palSlaMap ( double rm, double dm, double pr, double pd,
              double px, double rv, double eq, double date,
              double *ra, double *da );

void palSlaMappa ( double eq, double date, double amprms[21] );

void palSlaMapqk ( double rm, double dm, double pr, double pd,
                double px, double rv, double amprms[21],
                double *ra, double *da );

void palSlaMapqkz ( double rm, double dm, double amprms[21],
                 double *ra, double *da );

void palSlaMoon ( int iy, int id, float fd, float posvel[6] );

void palSlaMxm ( float a[3][3], float b[3][3], float c[3][3] );

void palSlaMxv ( float rm[3][3], float va[3], float vb[3] );

void palSlaNut ( double date, double rmatn[3][3] );

void palSlaNutc ( double date, double *dpsi, double *deps, double *eps0 );

void palSlaNutc80 ( double date, double *dpsi, double *deps, double *eps0 );

void palSlaOap ( char *type, double ob1, double ob2, double date,
              double dut, double elongm, double phim, double hm,
              double xp, double yp, double tdk, double pmb,
              double rh, double wl, double tlr,
              double *rap, double *dap );

void palSlaOapqk ( char *type, double ob1, double ob2, double aoprms[14],
                double *rap, double *dap );

void palSlaObs ( int n, char *c, char *name, double *w, double *p, double *h );

double palSlaPa ( double ha, double dec, double phi );

double palSlaPav ( float v1[3], float v2[3] );

void palSlaPcd ( double disco, double *x, double *y );

void palSlaPda2h ( double p, double d, double a,
                double *h1, int *j1, double *h2, int *j2 );

void palSlaPdq2h ( double p, double d, double q,
                double *h1, int *j1, double *h2, int *j2 );

void palSlaPermut ( int n, int istate[], int iorder[], int *j );

void palSlaPertel (int jform, double date0, double date1,
                double epoch0, double orbi0, double anode0,
                double perih0, double aorq0, double e0, double am0,
                double *epoch1, double *orbi1, double *anode1,
                double *perih1, double *aorq1, double *e1, double *am1,
                int *jstat );

void palSlaPertue ( double date, double u[], int *jstat );

void palSlaPlanel ( double date, int jform, double epoch, double orbinc,
                 double anode, double perih, double aorq,  double e,
                 double aorl, double dm, double pv[6], int *jstat );

void palSlaPlanet ( double date, int np, double pv[6], int *j );

void palSlaPlante ( double date, double elong, double phi, int jform,
                 double epoch, double orbinc, double anode, double perih,
                 double aorq, double e, double aorl, double dm,
                 double *ra, double *dec, double *r, int *jstat );

void palSlaPlantu ( double date, double elong, double phi, double u[],
                 double *ra, double *dec, double *r, int *jstat );

void palSlaPm ( double r0, double d0, double pr, double pd,
             double px, double rv, double ep0, double ep1,
             double *r1, double *d1 );

void palSlaPolmo ( double elongm, double phim, double xp, double yp,
                double *elong, double *phi, double *daz );

void palSlaPrebn ( double bep0, double bep1, double rmatp[3][3] );

void palSlaPrec ( double ep0, double ep1, double rmatp[3][3] );

void palSlaPrecl ( double ep0, double ep1, double rmatp[3][3] );

void palSlaPreces ( char sys[3], double ep0, double ep1,
                 double *ra, double *dc );

void palSlaPrenut ( double epoch, double date, double rmatpn[3][3] );

void palSlaPv2el ( double pv[], double date, double pmass, int jformr,
                int *jform, double *epoch, double *orbinc,
                double *anode, double *perih, double *aorq, double *e,
                double *aorl, double *dm, int *jstat );

void palSlaPv2ue ( double pv[], double date, double pmass,
                double u[], int *jstat );

void palSlaPvobs ( double p, double h, double stl, double pv[6] );

void palSlaPxy ( int np, double xye[][2], double xym[][2],
              double coeffs[6],
              double xyp[][2], double *xrms, double *yrms, double *rrms );

float palSlaRange ( float angle );

float palSlaRanorm ( float angle );

double palSlaRcc ( double tdb, double ut1, double wl, double u, double v );

void palSlaRdplan ( double date, int np, double elong, double phi,
                 double *ra, double *dec, double *diam );

void palSlaRefco ( double hm, double tdk, double pmb, double rh,
                double wl, double phi, double tlr, double eps,
                double *refa, double *refb );

void palSlaRefcoq ( double tdk, double pmb, double rh, double wl,
                double *refa, double *refb );

void palSlaRefro ( double zobs, double hm, double tdk, double pmb,
                double rh, double wl, double phi, double tlr, double eps,
                double *ref );

void palSlaRefv ( double vu[3], double refa, double refb, double vr[3] );

void palSlaRefz ( double zu, double refa, double refb, double *zr );

float palSlaRverot ( float phi, float ra, float da, float st );

float palSlaRvgalc ( float r2000, float d2000 );

float palSlaRvlg ( float r2000, float d2000 );

float palSlaRvlsrd ( float r2000, float d2000 );

float palSlaRvlsrk ( float r2000, float d2000 );

void palSlaS2tp ( float ra, float dec, float raz, float decz,
               float *xi, float *eta, int *j );

float palSlaSep ( float a1, float b1, float a2, float b2 );

float palSlaSepv ( float v1[3], float v2[3] );

void palSlaSmat ( int n, float *a, float *y, float *d, int *jf, int *iw );

void palSlaSubet ( double rc, double dc, double eq,
                double *rm, double *dm );

void palSlaSupgal ( double dsl, double dsb, double *dl, double *db );

void palSlaSvd ( int m, int n, int mp, int np,
              double *a, double *w, double *v, double *work,
              int *jstat );

void palSlaSvdcov ( int n, int np, int nc,
                 double *w, double *v, double *work, double *cvm );

void palSlaSvdsol ( int m, int n, int mp, int np,
                 double *b, double *u, double *w, double *v,
                 double *work, double *x );

void palSlaTp2s ( float xi, float eta, float raz, float decz,
               float *ra, float *dec );

void palSlaTp2v ( float xi, float eta, float v0[3], float v[3] );

void palSlaTps2c ( float xi, float eta, float ra, float dec,
                float *raz1, float *decz1,
                float *raz2, float *decz2, int *n );

void palSlaTpv2c ( float xi, float eta, float v[3],
                float v01[3], float v02[3], int *n );

void palSlaUe2el ( double u[], int jformr,
                int *jform, double *epoch, double *orbinc,
                double *anode, double *perih, double *aorq, double *e,
                double *aorl, double *dm, int *jstat );

void palSlaUe2pv ( double date, double u[], double pv[], int *jstat );

void palSlaUnpcd ( double disco, double *x, double *y );

void palSlaV2tp ( float v[3], float v0[3], float *xi, float *eta, int *j );

float palSlaVdv ( float va[3], float vb[3] );

void palSlaVn ( float v[3], float uv[3], float *vm );

void palSlaVxv ( float va[3], float vb[3], float vc[3] );

void palSlaXy2xy ( double x1, double y1, double coeffs[6],
                double *x2, double *y2 );

double palSlaZd ( double ha, double dec, double phi );

#ifdef __cplusplus
}
#endif

#endif
