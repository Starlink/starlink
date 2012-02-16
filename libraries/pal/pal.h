#ifndef PALHDEF
#define PALHDEF

/*
*+
*  Name:
*     pal.h

*  Purpose:
*     Function prototypes for PAL routines.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Include file

*  Description:
*     Function prototypes for PAL routines.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*

*  History:
*     2012-02-08 (TIMJ):
*        Initial version. Define all SLA prototypes in PAL form even
*        though none are implemented.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program; if not, write to the Free Software
*    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
*    USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#ifdef __cplusplus
extern "C" {
#endif

#include <math.h>

void palAddet ( double rm, double dm, double eq, double *rc, double *dc );

void palAfin ( const char *string, int *iptr, float *a, int *j );

double palAirmas ( double zd );

void palAltaz ( double ha, double dec, double phi,
                double *az, double *azd, double *azdd,
                double *el, double *eld, double *eldd,
                double *pa, double *pad, double *padd );

void palAmp ( double ra, double da, double date, double eq,
              double *rm, double *dm );

void palAmpqk ( double ra, double da, double amprms[21],
                double *rm, double *dm );

void palAop ( double rap, double dap, double date, double dut,
              double elongm, double phim, double hm, double xp,
              double yp, double tdk, double pmb, double rh,
              double wl, double tlr,
              double *aob, double *zob, double *hob,
              double *dob, double *rob );

void palAoppa ( double date, double dut, double elongm, double phim,
                double hm, double xp, double yp, double tdk, double pmb,
                double rh, double wl, double tlr, double aoprms[14] );

void palAoppat ( double date, double aoprms[14] );

void palAopqk ( double rap, double dap, double aoprms[14],
                double *aob, double *zob, double *hob,
                double *dob, double *rob );

void palAtmdsp ( double tdk, double pmb, double rh, double wl1,
                 double a1, double b1, double wl2, double *a2, double *b2 );

void palAv2m ( float axvec[3], float rmat[3][3] );

float palBear ( float a1, float b1, float a2, float b2 );

void palCaf2r ( int ideg, int iamin, float asec, float *rad, int *j );

void palCaldj ( int iy, int im, int id, double *djm, int *j );

void palCalyd ( int iy, int im, int id, int *ny, int *nd, int *j );

void palCc2s ( float v[3], float *a, float *b );

void palCc62s ( float v[6], float *a, float *b, float *r,
                float *ad, float *bd, float *rd );

void palCd2tf ( int ndp, float days, char *sign, int ihmsf[4] );

void palCldj ( int iy, int im, int id, double *djm, int *j );

void palClyd ( int iy, int im, int id, int *ny, int *nd, int *jstat );

void palCombn ( int nsel, int ncand, int list[], int *j );

void palCr2af ( int ndp, float angle, char *sign, int idmsf[4] );

void palCr2tf ( int ndp, float angle, char *sign, int ihmsf[4] );

void palCs2c ( float a, float b, float v[3] );

void palCs2c6 ( float a, float b, float r, float ad,
                float bd, float rd, float v[6] );

void palCtf2d ( int ihour, int imin, float sec, float *days, int *j );

void palCtf2r ( int ihour, int imin, float sec, float *rad, int *j );

void palDaf2r ( int ideg, int iamin, double asec, double *rad, int *j );

void palDafin ( const char *string, int *iptr, double *a, int *j );

double palDat ( double dju );

void palDav2m ( double axvec[3], double rmat[3][3] );

double palDbear ( double a1, double b1, double a2, double b2 );

void palDbjin ( const char *string, int *nstrt,
                double *dreslt, int *jf1, int *jf2 );

void palDc62s ( double v[6], double *a, double *b, double *r,
                double *ad, double *bd, double *rd );

void palDcc2s ( double v[3], double *a, double *b );

void palDcmpf ( double coeffs[6], double *xz, double *yz, double *xs,
                double *ys, double *perp, double *orient );

void palDcs2c ( double a, double b, double v[3] );

void palDd2tf ( int ndp, double days, char *sign, int ihmsf[4] );

void palDe2h ( double ha, double dec, double phi,
               double *az, double *el );

void palDeuler ( const char *order, double phi, double theta, double psi,
                 double rmat[3][3] );

void palDfltin ( const char *string, int *nstrt, double *dreslt, int *jflag );

void palDh2e ( double az, double el, double phi, double *ha, double *dec);

void palDimxv ( double dm[3][3], double va[3], double vb[3] );

void palDjcal ( int ndp, double djm, int iymdf[4], int *j );

void palDjcl ( double djm, int *iy, int *im, int *id, double *fd, int *j );

void palDm2av ( double rmat[3][3], double axvec[3] );

void palDmat ( int n, double *a, double *y, double *d, int *jf, int *iw );

void palDmoon ( double date, double pv[6] );

void palDmxm ( double a[3][3], double b[3][3], double c[3][3] );

void palDmxv ( double dm[3][3], double va[3], double vb[3] );

double palDpav ( double v1[3], double v2[3] );

void palDr2af ( int ndp, double angle, char *sign, int idmsf[4] );

void palDr2tf ( int ndp, double angle, char *sign, int ihmsf[4] );

double palDrange ( double angle );

double palDranrm ( double angle );

void palDs2c6 ( double a, double b, double r, double ad, double bd,
                double rd, double v[6] );

void palDs2tp ( double ra, double dec, double raz, double decz,
                double *xi, double *eta, int *j );

double palDsep ( double a1, double b1, double a2, double b2 );

double palDsepv ( double v1[3], double v2[3] );

double palDt ( double epoch );

void palDtf2d ( int ihour, int imin, double sec, double *days, int *j );

void palDtf2r ( int ihour, int imin, double sec, double *rad, int *j );

void palDtp2s ( double xi, double eta, double raz, double decz,
                double *ra, double *dec );

void palDtp2v ( double xi, double eta, double v0[3], double v[3] );

void palDtps2c ( double xi, double eta, double ra, double dec,
                 double *raz1, double *decz1,
                 double *raz2, double *decz2, int *n );

void palDtpv2c ( double xi, double eta, double v[3],
                 double v01[3], double v02[3], int *n );

double palDtt ( double dju );

void palDv2tp ( double v[3], double v0[3], double *xi, double *eta, int *j );

double palDvdv ( double va[3], double vb[3] );

void palDvn ( double v[3], double uv[3], double *vm );

void palDvxv ( double va[3], double vb[3], double vc[3] );

void palE2h ( float ha, float dec, float phi, float *az, float *el );

void palEarth ( int iy, int id, float fd, float posvel[6] );

void palEcleq ( double dl, double db, double date, double *dr, double *dd );

void palEcmat ( double date, double rmat[3][3] );

void palEcor ( float rm, float dm, int iy, int id, float fd,
               float *rv, float *tl );

void palEg50 ( double dr, double dd, double *dl, double *db );

void palEl2ue ( double date, int jform, double epoch, double orbinc,
                double anode, double perih, double aorq, double e,
                double aorl, double dm, double u[], int *jstat );

double palEpb ( double date );

double palEpb2d ( double epb );

double palEpco ( char k0, char k, double e );

double palEpj ( double date );

double palEpj2d ( double epj );

void palEqecl ( double dr, double dd, double date, double *dl, double *db );

double palEqeqx ( double date );

void palEqgal ( double dr, double dd, double *dl, double *db );

void palEtrms ( double ep, double ev[3] );

void palEuler ( const char *order, float phi, float theta, float psi,
                float rmat[3][3] );

void palEvp ( double date, double deqx,
              double dvb[3], double dpb[3],
              double dvh[3], double dph[3] );

void palFitxy ( int itype, int np, double xye[][2], double xym[][2],
                double coeffs[6], int *j );

void palFk425 ( double r1950, double d1950, double dr1950,
                double dd1950, double p1950, double v1950,
                double *r2000, double *d2000, double *dr2000,
                double *dd2000, double *p2000, double *v2000 );

void palFk45z ( double r1950, double d1950, double bepoch,
                double *r2000, double *d2000 );

void palFk524 ( double r2000, double d2000, double dr2000,
                double dd2000, double p2000, double v2000,
                double *r1950, double *d1950, double *dr1950,
                double *dd1950, double *p1950, double *v1950 );

void palFk52h ( double r5, double d5, double dr5, double dd5,
                double *dr, double *dh, double *drh, double *ddh );

void palFk54z ( double r2000, double d2000, double bepoch,
                double *r1950, double *d1950,
                double *dr1950, double *dd1950 );

void palFk5hz ( double r5, double d5, double epoch,
                double *rh, double *dh );

void palFlotin ( const char *string, int *nstrt, float *reslt, int *jflag );

void palGaleq ( double dl, double db, double *dr, double *dd );

void palGalsup ( double dl, double db, double *dsl, double *dsb );

void palGe50 ( double dl, double db, double *dr, double *dd );

void palGeoc ( double p, double h, double *r, double *z );

double palGmst ( double ut1 );

double palGmsta ( double date, double ut1 );

void palH2e ( float az, float el, float phi, float *ha, float *dec );

void palH2fk5 ( double dr, double dh, double drh, double ddh,
                double *r5, double *d5, double *dr5, double *dd5 );

void palHfk5z ( double rh, double dh, double epoch,
                double *r5, double *d5, double *dr5, double *dd5 );

void palImxv ( float rm[3][3], float va[3], float vb[3] );

void palInt2in ( const char *string, int *nstrt, int *ireslt, int *jflag );

void palIntin ( const char *string, int *nstrt, long *ireslt, int *jflag );

void palInvf ( double fwds[6], double bkwds[6], int *j );

void palKbj ( int jb, double e, char *k, int *j );

void palM2av ( float rmat[3][3], float axvec[3] );

void palMap ( double rm, double dm, double pr, double pd,
              double px, double rv, double eq, double date,
              double *ra, double *da );

void palMappa ( double eq, double date, double amprms[21] );

void palMapqk ( double rm, double dm, double pr, double pd,
                double px, double rv, double amprms[21],
                double *ra, double *da );

void palMapqkz ( double rm, double dm, double amprms[21],
                 double *ra, double *da );

void palMoon ( int iy, int id, float fd, float posvel[6] );

void palMxm ( float a[3][3], float b[3][3], float c[3][3] );

void palMxv ( float rm[3][3], float va[3], float vb[3] );

void palNut ( double date, double rmatn[3][3] );

void palNutc ( double date, double *dpsi, double *deps, double *eps0 );

void palNutc80 ( double date, double *dpsi, double *deps, double *eps0 );

void palOap ( const char *type, double ob1, double ob2, double date,
              double dut, double elongm, double phim, double hm,
              double xp, double yp, double tdk, double pmb,
              double rh, double wl, double tlr,
              double *rap, double *dap );

void palOapqk ( const char *type, double ob1, double ob2, double aoprms[14],
                double *rap, double *dap );

void palObs ( int n, char *c, char *name, double *w, double *p, double *h );

double palPa ( double ha, double dec, double phi );

double palPav ( float v1[3], float v2[3] );

void palPcd ( double disco, double *x, double *y );

void palPda2h ( double p, double d, double a,
                double *h1, int *j1, double *h2, int *j2 );

void palPdq2h ( double p, double d, double q,
                double *h1, int *j1, double *h2, int *j2 );

void palPermut ( int n, int istate[], int iorder[], int *j );

void palPertel (int jform, double date0, double date1,
                double epoch0, double orbi0, double anode0,
                double perih0, double aorq0, double e0, double am0,
                double *epoch1, double *orbi1, double *anode1,
                double *perih1, double *aorq1, double *e1, double *am1,
                int *jstat );

void palPertue ( double date, double u[], int *jstat );

void palPlanel ( double date, int jform, double epoch, double orbinc,
                 double anode, double perih, double aorq,  double e,
                 double aorl, double dm, double pv[6], int *jstat );

void palPlanet ( double date, int np, double pv[6], int *j );

void palPlante ( double date, double elong, double phi, int jform,
                 double epoch, double orbinc, double anode, double perih,
                 double aorq, double e, double aorl, double dm,
                 double *ra, double *dec, double *r, int *jstat );

void palPlantu ( double date, double elong, double phi, double u[],
                 double *ra, double *dec, double *r, int *jstat );

void palPm ( double r0, double d0, double pr, double pd,
             double px, double rv, double ep0, double ep1,
             double *r1, double *d1 );

void palPolmo ( double elongm, double phim, double xp, double yp,
                double *elong, double *phi, double *daz );

void palPrebn ( double bep0, double bep1, double rmatp[3][3] );

void palPrec ( double ep0, double ep1, double rmatp[3][3] );

void palPrecl ( double ep0, double ep1, double rmatp[3][3] );

void palPreces ( const char sys[3], double ep0, double ep1,
                 double *ra, double *dc );

void palPrenut ( double epoch, double date, double rmatpn[3][3] );

void palPv2el ( double pv[], double date, double pmass, int jformr,
                int *jform, double *epoch, double *orbinc,
                double *anode, double *perih, double *aorq, double *e,
                double *aorl, double *dm, int *jstat );

void palPv2ue ( double pv[], double date, double pmass,
                double u[], int *jstat );

void palPvobs ( double p, double h, double stl, double pv[6] );

void palPxy ( int np, double xye[][2], double xym[][2],
              double coeffs[6],
              double xyp[][2], double *xrms, double *yrms, double *rrms );

float palRange ( float angle );

float palRanorm ( float angle );

double palRcc ( double tdb, double ut1, double wl, double u, double v );

void palRdplan ( double date, int np, double elong, double phi,
                 double *ra, double *dec, double *diam );

void palRefco ( double hm, double tdk, double pmb, double rh,
                double wl, double phi, double tlr, double eps,
                double *refa, double *refb );

void palRefcoq ( double tdk, double pmb, double rh, double wl,
                double *refa, double *refb );

void palRefro ( double zobs, double hm, double tdk, double pmb,
                double rh, double wl, double phi, double tlr, double eps,
                double *ref );

void palRefv ( double vu[3], double refa, double refb, double vr[3] );

void palRefz ( double zu, double refa, double refb, double *zr );

float palRverot ( float phi, float ra, float da, float st );

double palRvgalc ( double r2000, double d2000 );

double palRvlg ( double r2000, double d2000 );

float palRvlsrd ( float r2000, float d2000 );

float palRvlsrk ( float r2000, float d2000 );

void palS2tp ( float ra, float dec, float raz, float decz,
               float *xi, float *eta, int *j );

float palSep ( float a1, float b1, float a2, float b2 );

float palSepv ( float v1[3], float v2[3] );

void palSmat ( int n, float *a, float *y, float *d, int *jf, int *iw );

void palSubet ( double rc, double dc, double eq,
                double *rm, double *dm );

void palSupgal ( double dsl, double dsb, double *dl, double *db );

void palSvd ( int m, int n, int mp, int np,
              double *a, double *w, double *v, double *work,
              int *jstat );

void palSvdcov ( int n, int np, int nc,
                 double *w, double *v, double *work, double *cvm );

void palSvdsol ( int m, int n, int mp, int np,
                 double *b, double *u, double *w, double *v,
                 double *work, double *x );

void palTp2s ( float xi, float eta, float raz, float decz,
               float *ra, float *dec );

void palTp2v ( float xi, float eta, float v0[3], float v[3] );

void palTps2c ( float xi, float eta, float ra, float dec,
                float *raz1, float *decz1,
                float *raz2, float *decz2, int *n );

void palTpv2c ( float xi, float eta, float v[3],
                float v01[3], float v02[3], int *n );

void palUe2el ( double u[], int jformr,
                int *jform, double *epoch, double *orbinc,
                double *anode, double *perih, double *aorq, double *e,
                double *aorl, double *dm, int *jstat );

void palUe2pv ( double date, double u[], double pv[], int *jstat );

void palUnpcd ( double disco, double *x, double *y );

void palV2tp ( float v[3], float v0[3], float *xi, float *eta, int *j );

float palVdv ( float va[3], float vb[3] );

void palVn ( float v[3], float uv[3], float *vm );

void palVxv ( float va[3], float vb[3], float vc[3] );

void palXy2xy ( double x1, double y1, double coeffs[6],
                double *x2, double *y2 );

double palZd ( double ha, double dec, double phi );

#ifdef __cplusplus
}
#endif

#endif
