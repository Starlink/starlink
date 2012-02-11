/*
*+
*  Name:
*     palTest

*  Purpose:
*     Test the PAL library

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Application

*  Description:
*     Test the PAL library is functioning correctly. Uses some of the SLA test code.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-02-08 (TIMJ):
*        Initial version
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

#include <stdlib.h>
#include <stdio.h>

#include "pal.h"
#include "palmac.h"

static int verbose = 1;

/* Support functions to allow to test results. viv and vvd match the SOFA implementations */

static void viv(int ival, int ivalok, const char *func, const char *test,
                int *status)
/*
**  - - - -
**   v i v
**  - - - -
**
**  Validate an integer result.
**
**  Internal function used by t_sofa_c program.
**
**  Given:
**     ival     int          value computed by function under test
**     ivalok   int          correct value
**     func     char[]       name of function under test
**     test     char[]       name of individual test
**
**  Given and returned:
**     status   int          set to FALSE if test fails
**
**  This revision:  2009 November 4
*/
{
   if (ival != ivalok) {
      *status = 1;
      printf("%s failed: %s want %d got %d\n",
             func, test, ivalok, ival);
   } else if (verbose) {
      printf("%s passed: %s want %d got %d\n",
                    func, test, ivalok, ival);
   }
   return;
}

static void vvd(double val, double valok, double dval,
                const char *func, const char *test, int *status)
/*
**  - - - -
**   v v d
**  - - - -
**
**  Validate a double result.
**
**  Internal function used by t_sofa_c program.
**
**  Given:
**     val      double       value computed by function under test
**     valok    double       expected value
**     dval     double       maximum allowable error
**     func     char[]       name of function under test
**     test     char[]       name of individual test
**
**  Given and returned:
**     status   int          set to FALSE if test fails
**
**  This revision:  2008 June 8
*/
{
   double a, f;   /* absolute and fractional error */


   a = val - valok;
   if (fabs(a) > dval) {
      f = fabs(valok / a);
      *status = 1;
      printf("%s failed: %s want %.20g got %.20g (1/%.3g)\n",
             func, test, valok, val, f);
   } else if (verbose) {
      printf("%s passed: %s want %.20g got %.20g\n",
             func, test, valok, val);
   }
   return;
}

/* Verify the 3x3 rmat matrix */
static void
vrmat( double rmat[3][3], double expected[3][3], const char * func,
       int * status ) {
  int i;
  char buf[10];
  for( i = 0; i < 3; i++ ) {
    int j;
    for( j = 0; j < 3; j++ ) {
      sprintf( buf, "%d,%d", i, j );
      vvd( rmat[i][j], expected[i][j], 1e-12, func, buf, status );
    }
  }
}

/******************************************************************/
/*          TEST FUNCTIONS          */

/* palDaf2r */

static void t_caf2r( int * status ) {
  int j;
  double dr;

  palDaf2r ( 76, 54, 32.1, &dr, &j );
  vvd ( dr, 1.342313819975276, 1e-12, "palDaf2r",
        "r", status );
  viv ( j, 0, "palDaf2r", "j", status );
}

/* Test palDcc2s routines */

static void t_cc2s( int * status ) {
  double dv[3] = { 100., -50., 25. };
  double da, db;

  palDcc2s ( dv, &da, &db );
  vvd ( da, -0.4636476090008061, 1e-12, "palDcc2s",
        "A", status );
  vvd ( db, 0.2199879773954594, 1e-12, "palDcc2s",
        "B", status );
}

/* palDd2tf */

static void t_cd2tf( int *status ) {
  int ihmsf[4];
  char s;

  palDd2tf ( 4, -0.987654321, &s, ihmsf );
  viv ( s, '-', "palDd2tf", "S", status );
  viv ( ihmsf[0], 23, "palDd2tf", "(1)", status );
  viv ( ihmsf[1], 42, "palDd2tf", "(2)", status );
  viv ( ihmsf[2], 13, "palDd2tf", "(3)", status );
  viv ( ihmsf[3], 3333, "palDd2tf", "(4)", status );
}

/* palDtf2d */

static void t_ctf2d( int *status ) {
  double dd;
  int j;

  palDtf2d (23, 56, 59.1, &dd, &j);
  vvd ( dd, 0.99790625, 1e-12, "palDtf2d", "D", status );
  viv ( j, 0, "palDtf2d", "J", status );
}

/* palDtf2r */

static void t_ctf2r( int *status ) {
  double dr;
  int j;

  palDtf2r (23, 56, 59.1, &dr, &j);
  vvd ( dr, 6.270029887942679, 1e-12, "palDtf2r",
        "R", status );
  viv ( j, 0, "palDtf2r", "J", status );
}

/* Dates */

static void t_djcal( int *status ) {
  const double djm = 50123.9999;
  int iy, im, id;
  int iydmf[4];
  int j;
  double f;

  /* Later
  palDjcal ( 4, djm, iydmf, &j );
  viv ( iydmf[0], 1996, "palDjcal", "Y", status );
  viv ( iydmf[1], 2, "palDjcal", "M", status );
  viv ( iydmf[2], 10, "palDjcal", "D", status );
  viv ( iydmf[3], 9999, "palDjcal", "F", status );
  viv ( j, 0, "palDjcal", "J", status );
  */

  palDjcl ( djm, &iy, &im, &id, &f, &j );
  viv ( iy, 1996, "palDjcl", "Y", status );
  viv ( im, 2, "palDjcl", "M", status );
  viv ( id, 10, "palDjcl", "D", status );
  vvd ( f, 0.9999, 1e-7, "palDjcl", "F", status );
  viv ( j, 0, "palDjcl", "J", status );

}

/* Test palDe2h and palDh2e routines */

static void t_e2h( int *status ) {
  double dh, dd, dp, da, de;

  dh = -0.3;
  dd = -1.1;
  dp = -0.7;

  palDe2h( dh, dd, dp, &da, &de );
  vvd( da, 2.820087515852369, 1e-12, "palDe2h",
       "AZ", status);
  vvd( de, 1.132711866443304, 1e-12, "palDe2h",
       "EL", status );

  palDh2e( da, de, dp, &dh, &dd );
  vvd( dh, -0.3, 1e-12, "palDh2e", "HA", status);
  vvd( dd, -1.1, 1e-12, "palDh2e", "DEC", status );

}

/* Epochs */

static void t_epb( int *status ) {
  vvd ( palEpb( 45123 ), 1982.419793168669, 1e-8,
        "palEpb", " ", status );
}

static void t_epb2d( int *status ) {
  vvd ( palEpb2d( 1975.5 ), 42595.5995279655, 1e-7,
        "palEpb2d", " ", status );
}

static void t_epj( int *status ) {
  vvd ( palEpj( 42999 ), 1976.603696098563,
        1e-7, "palEpj", " ", status );
}

static void t_epj2d( int *status ) {
  vvd ( palEpj2d( 2010.077 ), 55225.124250,
        1e-6, "palEpj2d", " ", status );
}

/* Equation of the equinoxes */

/* Use SOFA test because of change in precession model */
static void t_eqeqx (int *status ) {
  vvd ( palEqeqx( 53736. ), -0.8834195072043790156e-5,
        1e-15, "palEqeqx", " ", status );
}

/* Geocentric coordinates */

/* This is not from sla_test.f */

static void t_geoc( int *status ) {
  double r;
  double z;
  /* JCMT */
  const double lat = 19.822838905884 * PAL__DD2R;
  const double alt = 4120.0;
  palGeoc( lat, alt, &r, &z );

  /* Note the lower tolerance than normal since the models in SLA
     differ from the more up to date model in SOFA */
  vvd( r, 4.01502667039618e-05, 1e-10, "palGeoc", "R", status );
  vvd( z, 1.43762411970295e-05, 1e-10, "palGeoc", "Z", status );

}

/* GMST */

/* We use the SOFA test values rather than the values from SLA
   because the precession models have changed */

static void t_gmst( int *status ) {
  vvd ( palGmst( 53736. ), 1.754174971870091203,
        1e-12, "palGmst", " ", status );
}

/* FK5 */

static void t_fk52h ( int *status ) {
  double r5, d5, dr5, dd5, rh, dh;

  palFk5hz ( 1.234, -0.987, 1980, &rh, &dh );
  vvd ( rh, 1.234000136713611301, 1e-13, "palFk5hz",
        "R", status );
  vvd ( dh, -0.9869999702020807601, 1e-13, "palFk5hz",
        "D", status );
  palHfk5z ( rh, dh, 1980, &r5, &d5, &dr5, &dd5 );
  vvd ( r5, 1.234, 1e-13, "palHfk5z", "R", status );
  vvd ( d5, -0.987, 1e-13, "palHfk5z", "D", status );
  vvd ( dr5, 0.000000006822074, 1e-13, "palHfk5z",
        "DR", status );
  vvd ( dd5, -0.000000002334012, 1e-13, "palHfk5z",
        "DD", status );

}

/* Range */

static void t_range( int *status ) {
  vvd ( palDrange ( -4 ), 2.283185307179586,
        1e-12, "palDrange", " ", status );
}

static void t_ranorm( int *status ) {
  vvd ( palDranrm ( -0.1 ), 6.183185307179587,
        1e-12, "palDranrm", "2", status );
}

/* Separation routines */

static void t_sep( int *status ) {
  double d1[3] = { 1.0, 0.1, 0.2 };
  double d2[3] = { -3.0, 1e-3, 0.2 };
  double ad1, bd1, ad2, bd2;

  palDcc2s( d1, &ad1, &bd1 );
  palDcc2s( d2, &ad2, &bd2 );

  vvd ( palDsep ( ad1, bd1, ad2, bd2 ),
        2.8603919190246608, 1e-7, "palDsep", " ", status );
  vvd ( palDsepv ( d1, d2 ),
        2.8603919190246608, 1e-7, "palDsepv", " ", status );

}

/* Test spherical tangent-plane-projection routines */
static void t_tp( int *status ) {

  int j;
  double dr0, dd0, dr1, dd1, dx, dy, dr2, dd2, dr01,
    dd01, dr02, dd02;

  dr0 = 3.1;
  dd0 = -0.9;
  dr1 = dr0 + 0.2;
  dd1 = dd0 - 0.1;
  palDs2tp( dr1, dd1, dr0, dd0, &dx, &dy, &j );
  vvd( dx, 0.1086112301590404, 1e-12, "palDs2tp",
       "x", status );
  vvd( dy, -0.1095506200711452, 1e-12, "palDs2tp",
       "y", status );
  viv( j, 0, "palDs2tp", "j", status );

  palDtp2s( dx, dy, dr0, dd0, &dr2, &dd2 );
  vvd( dr2 - dr1, 0., 1e-12, "palDtp2s", "r", status );
  vvd( dd2 - dd1, 0., 1e-12, "palDtp2s", "d", status );

  palDtps2c( dx, dy, dr2, dd2, &dr01, &dd01, &dr02, &dd02, &j );
  vvd( dr01, 3.1, 1e-12, "palDtps2c", "r1", status);
  vvd( dd01, -0.9, 1e-12, "palDtps2c", "d1", status);
  vvd( dr02, 0.3584073464102072, 1e-12, "palDtps2c",
       "r2", status);
  vvd( dd02, -2.023361658234722, 1e-12, "palDtps2c",
       "d2", status );
  viv( j, 1, "palDtps2c", "n", status );

}

/* Test all the 3-vector and 3x3 matrix routines. */

static void t_vecmat( int * status ) {
  int i;

  /* palDav2m */
  double drm1[3][3];
  double dav[3] = { -0.123, 0.0987, 0.0654 };
  double dav2m_expected[3][3] = {
    {  0.9930075842721269,  0.05902743090199868, -0.1022335560329612 },
    { -0.07113807138648245, 0.9903204657727545,  -0.1191836812279541 },
    {  0.09420887631983825, 0.1256229973879967,   0.9875948309655174 },
  };

  /* palDeuler */
  double drm2[3][3];
  double deuler_expected[3][3] = {
    { -0.1681574770810878,  0.1981362273264315,  0.9656423242187410 },
    { -0.2285369373983370,  0.9450659587140423, -0.2337117924378156 },
    { -0.9589024617479674, -0.2599853247796050, -0.1136384607117296 } };

  /* palDmxm */
  double drm[3][3];
  double dmxm_expected[3][3] = {
    { -0.09010460088585805,  0.3075993402463796,  0.9472400998581048 },
    { -0.3161868071070688,   0.8930686362478707, -0.3200848543149236 },
    { -0.9444083141897035,  -0.3283459407855694,  0.01678926022795169 },
  };

  /* palDcs2c et al */
  double dv1[3];
  double dv2[3];
  double dv3[3];
  double dv4[3];
  double dv5[3];
  double dv6[3];
  double dv7[3];
  double dvm;

  /* palDav2m */
  palDav2m( dav, drm1 );
  vrmat( drm1, dav2m_expected, "palDav2m", status );

  /* Test palDeuler */
  palDeuler( "YZY", 2.345, -0.333, 2.222, drm2 );
  vrmat( drm2, deuler_expected, "palDeuler", status );

  /* palDmxm */
  palDmxm( drm2, drm1, drm );
  vrmat( drm, dmxm_expected, "palDmxm", status );

  /* palDcs2c */
  palDcs2c( 3.0123, -0.999, dv1 );
  vvd ( dv1[0], -0.5366267667260525, 1e-12,
        "palDcs2c", "x", status );
  vvd ( dv1[1], 0.06977111097651444, 1e-12,
        "palDcs2c", "y", status );
  vvd ( dv1[2], -0.8409302618566215, 1e-12,
        "palDcs2c", "z", status );

  /* palDmxv */
  palDmxv( drm1, dv1, dv2 );
  palDmxv( drm2, dv2, dv3 );
  vvd ( dv3[0], -0.7267487768696160, 1e-12,
        "palDmxv", "x", status );
  vvd ( dv3[1], 0.5011537352639822, 1e-12,
        "palDmxv", "y", status );
  vvd ( dv3[2], 0.4697671220397141, 1e-12,
        "palDmxv", "z", status );

  /* palDimxv */
  palDimxv( drm, dv3, dv4 );
  vvd ( dv4[0], -0.5366267667260526, 1e-12,
        "palDimxv", "X", status );
  vvd ( dv4[1], 0.06977111097651445, 1e-12,
        "palDimxv", "Y", status );
  vvd ( dv4[2], -0.8409302618566215, 1e-12,
        "palDimxv", "Z", status );

  /* palDm2av */
  palDm2av( drm, dv5 );
  vvd ( dv5[0], 0.006889040510209034, 1e-12,
        "palDm2av", "X", status );
  vvd ( dv5[1], -1.577473205461961, 1e-12,
        "palDm2av", "Y", status );
  vvd ( dv5[2], 0.5201843672856759, 1e-12,
        "palDm2av", "Z", status );

  for (i=0; i<3; i++) {
    dv5[i] *= 1000.0;
  }

  /* palDvn */
  palDvn( dv5, dv6, &dvm );
  vvd ( dv6[0], 0.004147420704640065, 1e-12,
        "palDvn", "X", status );
  vvd ( dv6[1], -0.9496888606842218, 1e-12,
        "palDvn", "Y", status );
  vvd ( dv6[2], 0.3131674740355448, 1e-12,
        "palDvn", "Z", status );
  vvd ( dvm, 1661.042127339937, 1e-9, "palDvn",
        "M", status );

  vvd ( palDvdv ( dv6, dv1 ), -0.3318384698006295,
        1e-12, "palDvn", " ", status );

  /* palDvxv */
  palDvxv( dv6, dv1, dv7 );
  vvd ( dv7[0], 0.7767720597123304, 1e-12,
        "palDvxv", "X", status );
  vvd ( dv7[1], -0.1645663574562769, 1e-12,
        "palDvxv", "Y", status );
  vvd ( dv7[2], -0.5093390925544726, 1e-12,
        "palDvxv", "Z", status );

}

static void t_ecmat( int *status ) {
   double rmat[3][3];
   double expected[3][3] = {
     { 1.0,                    0.0,                   0.0 },
     { 0.0, 0.91749307789883549624, 0.3977517467060596168 },
     { 0.0, -0.3977517467060596168, 0.91749307789883549624 } };

   palEcmat( 55966.46, rmat );
   vrmat( rmat, expected, "palEcmat", status );
}

static void t_prec( int *status ) {
   double rmat[3][3];
   double expected[3][3] = {
     { 0.9999856154510, -0.0049192906204,    -0.0021376320580 },
     {  0.0049192906805,  0.9999879002027,    -5.2297405698747e-06 },
     { 0.0021376319197, -5.2859681191735e-06, 0.9999977152483 } };

   palPrec( 1990.0, 2012.0, rmat );
   vrmat( rmat, expected, "palPrec", status );
}

/**********************************************************************/

int main (void) {

  /* Use the SLA and SOFA conventions */
  int status = 0; /* Unix and SAE convention */

  t_caf2r(&status);
  t_cc2s(&status);
  t_cd2tf(&status);
  t_ctf2d(&status);
  t_ctf2r(&status);
  t_djcal(&status);
  t_epb(&status);
  t_epb2d(&status);
  t_epj(&status);
  t_epj2d(&status);
  t_eqeqx(&status);
  t_geoc(&status);
  t_gmst(&status);
  t_fk52h(&status);
  t_prec(&status);
  t_ecmat(&status);
  t_e2h(&status);
  t_range(&status);
  t_ranorm(&status);
  t_sep(&status);
  t_tp(&status);
  t_vecmat(&status);
  return status;
}

