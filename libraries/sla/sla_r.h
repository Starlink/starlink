#if !defined( SLA_R_INCLUDED ) /* Include this file only once */
#define SLA_R_INCLUDED
/*
*  Name:
*     sla_r.h

*  Purpose:
*     Define the thread-safe C interface to the Fortran SLA library.

*  Description:
*     This module defines the thread-safe C interface to the functions of 
*     the SLA library. The file sla_r.c contains thread-safe C wrappers 
*     for the Fortran routines.

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

#if HAVE_CONFIG_H
#include "config.h"
#endif

#if USE_PTHREADS

void slaAddet_r( double rm, double dm, double eq, double *rc, double *dc );
double slaAirmas_r( double zd );
void slaAmp_r( double ra, double da, double date, double eq,
              double *rm, double *dm );
void slaAmpqk_r( double ra, double da, double amprms[21],
                double *rm, double *dm );
void slaCaldj_r( int iy, int im, int id, double *djm, int *j );
void slaCldj_r( int iy, int im, int id, double *djm, int *j );
void slaDaf2r_r( int ideg, int iamin, double asec, double *rad, int *j );
void slaDafin_r( const char *string, int *iptr, double *a, int *j );
double slaDat_r( double dju );
void slaDav2m_r( double axvec[3], double rmat[3][3] );
double slaDbear_r( double a1, double b1, double a2, double b2 );
void slaDcc2s_r( double v[3], double *a, double *b );
void slaDcs2c_r( double a, double b, double v[3] );
void slaDd2tf_r( int ndp, double days, char *sign, int ihmsf[4] );
void slaDe2h_r( double ha, double dec, double phi,
               double *az, double *el );
void slaDeuler_r( const char *order, double phi, double theta, double psi,
                 double rmat[3][3] );
void slaDh2e_r( double az, double el, double phi, double *ha, double *dec);
void slaDimxv_r( double dm[3][3], double va[3], double vb[3] );
void slaDjcal_r( int ndp, double djm, int iymdf[4], int *j );
void slaDjcl_r( double djm, int *iy, int *im, int *id, double *fd, int *j );
void slaDmat_r( int n, double *a, double *y, double *d, int *jf, int *iw );
void slaDmxm_r( double a[3][3], double b[3][3], double c[3][3] );
void slaDmxv_r( double dm[3][3], double va[3], double vb[3] );
void slaDr2af_r( int ndp, double angle, char *sign, int idmsf[4] );
void slaDr2tf_r( int ndp, double angle, char *sign, int ihmsf[4] );
double slaDrange_r( double angle );
double slaDranrm_r( double angle );
double slaDsep_r( double a1, double b1, double a2, double b2 );
double slaDt_r( double epoch );
void slaDtf2d_r( int ihour, int imin, double sec, double *days, int *j );
void slaDtf2r_r( int ihour, int imin, double sec, double *rad, int *j );
double slaDtt_r( double dju );
double slaDvdv_r( double va[3], double vb[3] );
void slaDvn_r( double v[3], double uv[3], double *vm );
void slaDvxv_r( double va[3], double vb[3], double vc[3] );
void slaEcmat_r( double date, double rmat[3][3] );
double slaEpb_r( double date );
double slaEpb2d_r( double epb );
double slaEpj_r( double date );
double slaEpj2d_r( double epj );
double slaEqeqx_r( double date );
void slaEqgal_r( double dr, double dd, double *dl, double *db );
void slaEvp_r( double date, double deqx,
              double dvb[3], double dpb[3],
              double dvh[3], double dph[3] );
void slaFk45z_r( double r1950, double d1950, double bepoch,
                double *r2000, double *d2000 );
void slaFk54z_r( double r2000, double d2000, double bepoch,
                double *r1950, double *d1950,
                double *dr1950, double *dd1950 );
void slaFk5hz_r( double r5, double d5, double epoch,
                double *rh, double *dh );
void slaGaleq_r( double dl, double db, double *dr, double *dd );
void slaGalsup_r( double dl, double db, double *dsl, double *dsb );
void slaGeoc_r( double p, double h, double *r, double *z );
double slaGmst_r( double ut1 );
void slaHfk5z_r( double rh, double dh, double epoch,
                double *r5, double *d5, double *dr5, double *dd5 );
void slaMap_r( double rm, double dm, double pr, double pd,
              double px, double rv, double eq, double date,
              double *ra, double *da );
void slaMappa_r( double eq, double date, double amprms[21] );
void slaMapqkz_r( double rm, double dm, double amprms[21],
                 double *ra, double *da );
void slaOap_r( const char *type, double ob1, double ob2, double date,
              double dut, double elongm, double phim, double hm,
              double xp, double yp, double tdk, double pmb,
              double rh, double wl, double tlr,
              double *rap, double *dap );
void slaObs_r( int n, char *c, char *name, double *w, double *p, double *h );
double slaPa_r( double ha, double dec, double phi );
void slaPertel_r(int jform, double date0, double date1,
                double epoch0, double orbi0, double anode0,
                double perih0, double aorq0, double e0, double am0,
                double *epoch1, double *orbi1, double *anode1,
                double *perih1, double *aorq1, double *e1, double *am1,
                int *jstat );
void slaPlante_r( double date, double elong, double phi, int jform,
                 double epoch, double orbinc, double anode, double perih,
                 double aorq, double e, double aorl, double dm,
                 double *ra, double *dec, double *r, int *jstat );
void slaPrebn_r( double bep0, double bep1, double rmatp[3][3] );
void slaPrec_r( double ep0, double ep1, double rmatp[3][3] );
void slaRdplan_r( double date, int np, double elong, double phi,
                 double *ra, double *dec, double *diam );
float slaRverot_r( float phi, float ra, float da, float st );
float slaRvgalc_r( float r2000, float d2000 );
float slaRvlg_r( float r2000, float d2000 );
float slaRvlsrd_r( float r2000, float d2000 );
float slaRvlsrk_r( float r2000, float d2000 );
void slaSubet_r( double rc, double dc, double eq,
                double *rm, double *dm );
void slaSupgal_r( double dsl, double dsb, double *dl, double *db );
void slaSvd_r( int m, int n, int mp, int np,
              double *a, double *w, double *v, double *work,
              int *jstat );
void slaSvdsol_r( int m, int n, int mp, int np,
                 double *b, double *u, double *w, double *v,
                 double *work, double *x );
#endif
#endif
