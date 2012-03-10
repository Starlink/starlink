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
#include <string.h>

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

/* Verify a string */
static void vcs( const char * val, const char * valok,
                 const char * func, const char * test,
                 int *status ) {

  if (strcmp(val, valok) != 0) {
    *status = 1;
    printf("%s failed: %s want %s got %s\n",
           func, test, valok, val );
  } else if (verbose) {
    printf("%s passed: %s want %s got %s\n",
           func, test, valok, val );
  }
  return;

}

/* Verify the 3x3 rmat matrix */
static void
vrmat( double rmat[3][3], double expected[3][3], const char * func,
       double dval, int * status ) {
  int i;
  char buf[10];
  for( i = 0; i < 3; i++ ) {
    int j;
    for( j = 0; j < 3; j++ ) {
      sprintf( buf, "%d,%d", i, j );
      vvd( rmat[i][j], expected[i][j], dval, func, buf, status );
    }
  }
}

/* Verify a vector */
static void
vvec( int len, double *vec, double *expected, const char *func,
      int *status ) {
  int i;
  char buf[10];
  for( i = 0; i < len; i++ ) {
    sprintf( buf, "%d", i );
    vvd( vec[i], expected[i], 1e-12, func, buf, status );
  }
}

/******************************************************************/
/*          TEST FUNCTIONS          */

/* Adding E-terms */

static void t_addet( int *status ) {
  double r1,d1,r2,d2;
  double rm = 2.;
  double dm = -1.;
  double eq = 1975.;


  palAddet ( rm, dm, eq, &r1, &d1 );
  vvd ( r1 - rm, 2.983864874295250e-6, 1e-12, "palAddet",
	"R", status );
  vvd ( d1 - dm, 2.379650804185118e-7, 1e-12, "palAddet",
	"D", status );

  palSubet ( r1, d1, eq, &r2, &d2 );
  vvd ( r2 - rm, 0, 1e-12, "palSubet", "R", status );
  vvd ( d2 - dm, 0, 1e-12, "palSubet", "D", status );

}

static void t_afin( int * status ) {

  int j;
  int i = 1;
  double d = 0.0;
  const char * s = "12 34 56.7 |";

  palDafin (s, &i, &d, &j);
  viv ( i, 12, "palDafin", "I", status );
  vvd ( d, 0.2196045986911432, 1e-12, "palDafin", "A",
        status );
  viv ( j, 0, "palDafin", "J", status );
}

/* Airmass */

static void t_airmas( int *status ) {
  vvd ( palAirmas ( 1.2354 ), 3.015698990074724,
        1e-12, "palAirmas", " ", status );
}

/* Apparent to mean place */

static void t_amp ( int *status ) {
  double rm, dm;

  /* Original SLA test is not accurate since palMapqk
     differs from slaMapqk */
  palAmp ( 2.345, -1.234, 50100, 1990, &rm, &dm );
  vvd ( rm, 2.344472180027961, 1e-6, "palAmp", "R",
        status );
  vvd ( dm, -1.233573099847705, 1e-7, "palAmp", "D",
        status );

  /* This is the palMapqk test */
  palAmp( 1.234, -0.567, 55927.0, 2010.0, &rm, &dm );
  vvd( rm, 1.2335120411026936349, 1.0E-12, "palAmp", "rm", status );
  vvd( dm, -0.56702908706930343907, 1.0E-12, "palAmp", "dm", status );
}

/* Bearings */

static void t_bear( int *status ) {
  double a1 = 1.234;
  double b1 = -0.123;
  double a2 = 2.345;
  double b2 = 0.789;

  double d1[3];
  double d2[3];

  vvd ( palDbear ( a1, b1, a2, b2 ), 0.7045970341781791,
	1e-12, "palDbear", " ", status );
  palDcs2c ( a1, b1, d1 );
  palDcs2c ( a2, b2, d2 );

  vvd ( palDpav ( d1, d2 ), 0.7045970341781791,
	1e-12, "palDpav", " ", status );

}

/* Calendar to MJD */

static void t_caldj( int *status ) {
  int j;
  double djm;

  palCaldj ( 1999, 12, 31, &djm, &j );
  vvd ( djm, 51543, 0, "palCaldj", " ", status );
  viv ( j, 0, "palCaldj", "J", status );
}

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

/* Calendar to MJD */

static void t_cldj( int *status ) {
  double d;
  int j;

  palCldj ( 1899, 12, 31, &d, &j );
  vvd ( d, 15019, 0, "palCldj", "D", status );
  viv ( j, 0, "palCldj", "J", status );
}

/* palDr2af */

static void t_cr2af( int *status ) {
  char s;
  int idmsf[4];
  palDr2af ( 4, 2.345, &s, idmsf );
  viv ( s, '+', "palDr2af", "S", status );
  viv ( idmsf[0], 134, "palDr2af", "(1)", status );
  viv ( idmsf[1], 21, "palDr2af", "(2)", status );
  viv ( idmsf[2], 30, "palDr2af", "(3)", status );
  viv ( idmsf[3], 9706, "palDr2af", "(4)", status );
}

/* palDr2tf */

static void t_cr2tf( int *status ) {
  char s;
  int ihmsf[4];
  palDr2tf ( 4, -3.01234, &s, ihmsf );
  viv ( s, '-', "palDr2tf", "S", status );
  viv ( ihmsf[0], 11, "palDr2tf", "(1)", status );
  viv ( ihmsf[1], 30, "palDr2tf", "(2)", status );
  viv ( ihmsf[2], 22, "palDr2tf", "(3)", status );
  viv ( ihmsf[3], 6484, "palDr2tf", "(4)", status );
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

static void t_dat ( int *status ) {
  vvd ( palDat ( 43900 ), 18, 0, "palDat",
        " ", status );
  vvd ( palDtt ( 40404 ), 39.709746, 1e-12, "palDtt",
        " ", status );
  vvd ( palDt ( 500 ), 4686.7, 1e-10, "palDt",
        "500", status );
  vvd ( palDt ( 1400 ), 408, 1e-11, "palDt",
        "1400", status );
  vvd ( palDt ( 1950 ), 27.99145626, 1e-12, "palDt",
        "1950", status );
}

/* Dates */

static void t_djcal( int *status ) {
  const double djm = 50123.9999;
  int iy, im, id;
  int iydmf[4];
  int j;
  double f;

  palDjcal ( 4, djm, iydmf, &j );
  viv ( iydmf[0], 1996, "palDjcal", "Y", status );
  viv ( iydmf[1], 2, "palDjcal", "M", status );
  viv ( iydmf[2], 10, "palDjcal", "D", status );
  viv ( iydmf[3], 9999, "palDjcal", "F", status );
  viv ( j, 0, "palDjcal", "J", status );

  palDjcl ( djm, &iy, &im, &id, &f, &j );
  viv ( iy, 1996, "palDjcl", "Y", status );
  viv ( im, 2, "palDjcl", "M", status );
  viv ( id, 10, "palDjcl", "D", status );
  vvd ( f, 0.9999, 1e-7, "palDjcl", "F", status );
  viv ( j, 0, "palDjcl", "J", status );

}

/* Matrix inversion */

static void t_dmat( int *status ) {
  int j;
  int iw[3];
  double dd;
  double da[9] = {
    2.22,     1.6578,     1.380522,
    1.6578,   1.380522,   1.22548578,
    1.380522, 1.22548578, 1.1356276122
  };
  double dv[3] = {
    2.28625, 1.7128825, 1.429432225
  };

  palDmat( 3, da, dv, &dd, &j, iw );
  vvd ( da[0], 18.02550629769198,
	1e-10, "palDmat", "a[0]", status );
  vvd ( da[1], -52.16386644917280607,
	1e-10, "palDmat", "a[1]", status );
  vvd ( da[2], 34.37875949717850495,
	1e-10, "palDmat", "a[2]", status );
  vvd ( da[3], -52.16386644917280607,
	1e-10, "palDmat", "a[3]", status );
  vvd ( da[4], 168.1778099099805627,
	1e-10, "palDmat", "a[4]", status );
  vvd ( da[5], -118.0722869694232670,
	1e-10, "palDmat", "a[5]", status );
  vvd ( da[6], 34.37875949717850495,
	1e-10, "palDmat", "a[6]", status );
  vvd ( da[7], -118.0722869694232670,
	1e-10, "palDmat", "a[7]", status );
  vvd ( da[8], 86.50307003740151262,
	1e-10, "palDmat", "a[8]", status );
  vvd ( dv[0], 1.002346480763383,
	1e-12, "palDmat", "v[0]", status );
  vvd ( dv[1], 0.03285594016974583489,
	1e-12, "palDmat", "v[1]", status );
  vvd ( dv[2], 0.004760688414885247309,
	1e-12, "palDmat", "v[2]", status );
  vvd ( dd, 0.003658344147359863,
	1e-12, "palDmat", "D", status );
  viv ( j, 0, "palDmat", "J", status );

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

static void t_epco( int *status ) {
  vvd ( palEpco ( 'B', 'J', 2000 ), 2000.001277513665,
        1e-7, "palEpco", "BJ", status );
  vvd ( palEpco ( 'J', 'B', 1950 ), 1949.999790442300,
        1e-7, "palEpco", "JB", status );
  vvd ( palEpco ( 'J', 'j', 2000 ), 2000,
        1e-7, "palEpco", "JJ", status );
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

/* E-terms */

static void t_etrms( int * status ) {
  double ev[3];

  palEtrms ( 1976.9, ev );

  vvd ( ev[0], -1.621617102537041e-6, 1e-18, "palEtrms",
	"X", status );
  vvd ( ev[1], -3.310070088507914e-7, 1e-18, "palEtrms",
	"Y", status );
  vvd ( ev[2], -1.435296627515719e-7, 1e-18, "palEtrms",
	"Z", status );
}

/* J2000 to Galactic */

static void t_eqgal( int *status ) {
  double dl, db;

  palEqgal ( 5.67, -1.23, &dl, &db );

  vvd ( dl, 5.612270780904526, 1e-12, "palEqgal",
	"DL", status );
  vvd ( db, -0.6800521449061520, 1e-12, "palEqgal",
	"DB", status );
}

/* Galactic to J2000 equatorial */

static void t_galeq( int *status ) {
  double dr, dd;

  palGaleq ( 5.67, -1.23, &dr, &dd );

  vvd ( dr, 0.04729270418071426, 1e-12, "palGaleq",
	"DR", status );
  vvd ( dd, -0.7834003666745548, 1e-12, "palGaleq",
	"DD", status );
}

/* Galactic to supergalactic */
static void t_galsup(int *status ) {
  double dsl, dsb;

  palGalsup ( 6.1, -1.4, &dsl, &dsb );

  vvd ( dsl, 4.567933268859171, 1e-12, "palGalsup",
	"DSL", status );
  vvd ( dsb, -0.01862369899731829, 1e-12, "palGalsup",
	"DSB", status );
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

/* Moon */

static void t_moon ( int *status ) {
  double pv[6];

  double expected1[] = {
    0.00229161514616454,
    0.000973912029208393,
    0.000669931538978146,
    -3.44709700068209e-09,
    5.44477533462392e-09,
    2.11785724844417e-09
  };

  /* SLA test only include slaMoon so we use the
     example from SUN/67 */
  palDmoon( 48634.4687174074, pv );
  vvec( 6, pv, expected1, "palDmoon", status );
}

/* Nutation */

static void t_nut( int *status ) {
  double dpsi, deps, eps0;

  double expected[3][3] = {
    {  9.999999969492166e-1, 7.166577986249302e-5,  3.107382973077677e-5 },
    { -7.166503970900504e-5, 9.999999971483732e-1, -2.381965032461830e-5 },
    { -3.107553669598237e-5, 2.381742334472628e-5,  9.999999992335206818e-1 }
  };

  double rmatn[3][3];

  /* SLA tests with low precision */
  palNut( 46012.32, rmatn );
  vrmat( rmatn, expected, "palNut", 1.0e-3, status );

  /* Use the SOFA tests */
  palNutc( 54388.0, &dpsi, &deps, &eps0 );
  vvd( eps0, 0.4090749229387258204, 1e-14,
      "palNutc", "eps0", status);

  palNutc( 53736.0, &dpsi, &deps, &eps0 );
   vvd(dpsi, -0.9630912025820308797e-5, 1e-13,
       "palNutc", "dpsi", status);
   vvd(deps,  0.4063238496887249798e-4, 1e-13,
       "palNutc", "deps", status);
}

/* palPrebn */

static void t_prebn( int *status ) {
  double rmatp[3][3];
  double prebn_expected[3][3] = {
    { 9.999257613786738e-1, -1.117444640880939e-2, -4.858341150654265e-3 },
    { 1.117444639746558e-2,  9.999375635561940e-1, -2.714797892626396e-5 },
    { 4.858341176745641e-3, -2.714330927085065e-5,  9.999881978224798e-1 },
  };

  palPrebn ( 1925., 1975., rmatp );
  vrmat( rmatp, prebn_expected, "palPrebn", 1.0e-12, status );
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

/* Supergalactic */

static void t_supgal( int *status ) {
  double dl, db;

  palSupgal ( 6.1, -1.4, &dl, &db );

  vvd ( dl, 3.798775860769474, 1e-12, "palSupgal",
	"DL", status );
  vvd ( db, -0.1397070490669407, 1e-12, "palSupgal",
	"DB", status );

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
  vrmat( drm1, dav2m_expected, "palDav2m", 1.0e-12, status );

  /* Test palDeuler */
  palDeuler( "YZY", 2.345, -0.333, 2.222, drm2 );
  vrmat( drm2, deuler_expected, "palDeuler", 1.0e-12, status );

  /* palDmxm */
  palDmxm( drm2, drm1, drm );
  vrmat( drm, dmxm_expected, "palDmxm", 1.0e-12, status );

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
   vrmat( rmat, expected, "palEcmat", 1.0e-12, status );
}

static void t_eqecl ( int *status ) {
  double dl, db;
  palEqecl ( 0.789, -0.123, 46555, &dl, &db );

  /* Slight changes from SLA for 2006 precession/nutation */
  vvd ( dl, 0.7036566430349022, 1e-6, "palEqecl",
        "L", status );
  vvd ( db, -0.4036047164116848, 1e-6, "palEqecl",
        "B", status );
}

static void t_prec( int *status ) {
   double rmat[3][3];
   double expected[3][3] = {
     { 0.9999856154510, -0.0049192906204,    -0.0021376320580 },
     {  0.0049192906805,  0.9999879002027,    -5.2297405698747e-06 },
     { 0.0021376319197, -5.2859681191735e-06, 0.9999977152483 } };

   palPrec( 1990.0, 2012.0, rmat );
   vrmat( rmat, expected, "palPrec", 1.0e-12, status );
}

static void t_preces( int *status ) {
  double ra;
  double dc;
  ra = 6.28;
  dc = -1.123;
  palPreces ( "FK4", 1925, 1950, &ra, &dc );
  vvd ( ra,  0.002403604864728447, 1e-12, "palPreces",
        "R", status );
  vvd ( dc, -1.120570643322045, 1e-12, "palPreces",
        "D", status );

  /* This is the SLA test but PAL now uses the IAU 2006
     precession model so we need to loosen the comparison */
  ra = 0.0123;
  dc = 1.0987;
  palPreces ( "FK5", 2050, 1990, &ra, &dc );
  vvd ( ra, 6.282003602708382, 1e-6, "palPreces",
        "R", status );
  vvd ( dc, 1.092870326188383, 1e-6, "palPreces",
        "D", status );

}

static void t_evp( int *status ) {
   double dvb[3],dpb[3],dvh[3],dph[3];
   double vbex[3] = { 1.6957348127008098514e-07,
                     -9.1093446116039685966e-08,
                     -3.9528532243991863036e-08 };
   double pbex[3] = {-0.49771075259730546136,
                     -0.80273812396332311359,
                     -0.34851593942866060383  };
   double vhex[3] = { 1.6964379181455713805e-07,
                     -9.1147224045727438391e-08,
                     -3.9553158272334222497e-08 };
   double phex[3] = { -0.50169124421419830639,
                      -0.80650980174901798492,
                      -0.34997162028527262212 };

   palEvp( 2010.0, 2012.0, dvb, dpb, dvh, dph );

   vvec( 3, dvb, vbex, "palEvp", status );
   vvec( 3, dpb, pbex, "palEvp", status );
   vvec( 3, dvh, vhex, "palEvp", status );
   vvec( 3, dph, phex, "palEvp", status );
}

static void t_map( int *status ) {
  double ra, da;
  palMap ( 6.123, -0.999, 1.23e-5, -0.987e-5,
           0.123, 32.1, 1999, 43210.9, &ra, &da );

  /* These are the SLA tests but and they agree to 0.1 arcsec
     with PAL/SOFA. We expect a slight difference from the change
     to nutation models. */
  vvd ( ra, 6.117130429775647, 1e-6, "palMap",
          "RA", status );
  vvd ( da, -1.000880769038632, 1e-8, "palMap",
        "DA", status );
}

static void t_mappa( int *status ) {
   double amprms[21];
   double expected[21] = {1.9986310746064646082,
                          -0.1728200754134739392,
                          0.88745394651412767839,
                          0.38472374350184274094,
                          -0.17245634725219796679,
                          0.90374808622520386159,
                          0.3917884696321610738,
                          2.0075929387510784968e-08,
                          -9.9464149073251757597e-05,
                          -1.6125306981057062306e-05,
                          -6.9897255793245634435e-06,
                          0.99999999489900059935,
                          0.99999983777998024959,
                          -0.00052248206600935195865,
                          -0.00022683144398381763045,
                          0.00052248547063364874764,
                          0.99999986339269864022,
                          1.4950491424992534218e-05,
                          0.00022682360163333854623,
                          -1.5069005133483779417e-05,
                          0.99999997416198904698};

   palMappa( 2010.0, 55927.0, amprms );
   vvec( 21, amprms, expected, "palMappa", status );
}

static void t_mapqkz( int *status ) {
   double amprms[21],  ra, da;
   palMappa( 2010.0, 55927.0, amprms );
   palMapqkz( 1.234, -0.567, amprms, &ra, &da );
   vvd( ra, 1.2344879748414849807, 1.0E-12, "palMapqkz", "ra", status );
   vvd( da, -0.56697099554368701746, 1.0E-12, "palMapqkz", "da", status );

   /* Try the same with palMapqk and zero parallax and proper motion */
   palMapqk( 1.234, -0.567, 0., 0., 0., 0., amprms, &ra, &da );
   vvd( ra, 1.2344879748414849807, 1.0E-7, "palMapqkz", "ra", status );
   vvd( da, -0.56697099554368701746, 1.0E-7, "palMapqkz", "da", status );

}

static void t_ampqk( int *status ) {
   double amprms[21],  rm, dm;
   palMappa( 2010.0, 55927.0, amprms );
   palAmpqk( 1.234, -0.567, amprms, &rm, &dm );
   vvd( rm, 1.2335120411026936349, 1.0E-12, "palAmpqk", "rm", status );
   vvd( dm, -0.56702908706930343907, 1.0E-12, "palAmpqk", "dm", status );
}

static void t_fk45z( int *status ) {
   double r2000, d2000;
   palFk45z( 1.2, -0.3, 1960.0, &r2000, &d2000 );
   vvd( r2000, 1.2097812228966762227, 1.0E-12, "palFk45z", "r2000", status );
   vvd( d2000, -0.29826111711331398935, 1.0E-12, "palFk45z", "d2000", status );
}

static void t_fk54z( int *status ) {
   double r1950, d1950, dr1950, dd1950;
   palFk54z( 1.2, -0.3, 1960.0, &r1950, &d1950, &dr1950, &dd1950 );
   vvd( r1950, 1.1902221805755279771, 1.0E-12, "palFk54z", "r1950", status );
   vvd( d1950, -0.30178317645793828472, 1.0E-12, "palFk54z", "d1950", status );
   vvd( dr1950, -1.7830874775952945507e-08, 1.0E-12, "palFk54z", "dr1950", status );
   vvd( dd1950, 7.196059425334821089e-09, 1.0E-12, "palFk54z", "dd1950", status );
}

static void t_flotin( int * status ) {

  int j;
  const char * s = "  12.345, , -0 1E3-4 2000  E     ";
  /*                123456789012345678901234567890123 */
  int i = 1;
  double dv = 0.0;

  palDfltin ( s, &i, &dv, &j );
  viv ( i, 10, "palDfltin", "I1", status );
  vvd ( dv, 12.345, 1e-12, "palDfltin", "V1", status );
  viv ( j, 0, "palDfltin", "J1", status );

  palDfltin ( s, &i, &dv, &j );
  viv ( i, 12, "palDfltin", "I2", status );
  vvd ( dv, 12.345, 1e-12, "palDfltin", "V2", status );
  viv ( j, 1, "palDfltin", "J2", status );

  palDfltin ( s, &i, &dv, &j );
  viv ( i, 16, "palDfltin", "I3", status );
  vvd ( dv, 0, 0, "palDfltin", "V3", status );
  viv ( j, -1, "palDfltin", "J3", status );

  palDfltin ( s, &i, &dv, &j );
  viv ( i, 19, "palDfltin", "I4", status );
  vvd ( dv, 1000, 0, "palDfltin", "V4", status );
  viv ( j, 0, "palDfltin", "J4", status );

  palDfltin ( s, &i, &dv, &j );
  viv ( i, 22, "palDfltin", "I5", status );
  vvd ( dv, -4, 0, "palDfltin", "V5", status );
  viv ( j, -1, "palDfltin", "J5", status );

  palDfltin ( s, &i, &dv, &j );
  viv ( i, 28, "palDfltin", "I6", status );
  vvd ( dv, 2000, 0, "palDfltin", "V6", status );
  viv ( j, 0, "palDfltin", "J6", status );

  palDfltin ( s, &i, &dv, &j );
  viv ( i, 34, "palDfltin", "I7", status );
  vvd ( dv, 2000, 0, "palDfltin", "V7", status );
  viv ( j, 1, "palDfltin", "J7", status ); /* differs from slaDfltin */

  /* Now test overflow and underflow */
  i = 1;
  palDfltin( " 1D600 ", &i, &dv, &j );
  viv ( i, 8, "palDfltin", "I8", status );
  vvd ( dv, HUGE_VAL, 0, "palDfltin", "V8", status );
  viv ( j, 2, "palDfltin", "J8", status );

}

static void t_obs( int * status ) {

  char shortname[11];
  char longname[41];
  double w, p, h;
  int lstat;

  lstat = palObs( 0, "MMT", shortname, sizeof(shortname),
                  longname, sizeof(longname), &w, &p, &h );
  vcs ( shortname, "MMT", "palObs", "1/C", status );
  vcs ( longname, "MMT 6.5m, Mt Hopkins", "palObs", "1/NAME",
        status );
  vvd ( w, 1.935300584055477, 1e-8, "palObs",
        "1/W", status );
  vvd ( p, 0.5530735081550342238, 1e-10, "palObs",
        "1/P", status );
  vvd ( h, 2608, 1e-10, "palObs",
        "1/H", status );
  viv( lstat, 0, "palObs", "retval", status );

  lstat = palObs ( 61, NULL, shortname, sizeof(shortname),
                   longname, sizeof(longname), &w, &p, &h );
  vcs ( shortname, "KECK1", "palObs", "2/C", status );
  vcs ( longname, "Keck 10m Telescope #1", "palObs",
        "2/NAME", status );
  vvd ( w, 2.713545757918895, 1e-8, "palObs",
        "2/W", status );
  vvd ( p, 0.3460280563536619, 1e-8, "palObs",
        "2/P", status );
  vvd ( h, 4160, 1e-10, "palObs",
        "2/H", status );
  viv( lstat, 0, "palObs", "retval", status );

  lstat = palObs ( 83, NULL, shortname, sizeof(shortname),
                   longname, sizeof(longname), &w, &p, &h );
  vcs ( shortname, "MAGELLAN2", "palObs", "3/C", status );
  vcs ( longname, "Magellan 2, 6.5m, Las Campanas",
        "palObs", "3/NAME", status );
  vvd ( w, 1.233819305534497, 1e-8, "palObs",
        "3/W", status );
  vvd ( p, -0.506389344359954, 1e-8, "palObs",
        "3/P", status );
  vvd ( h, 2408, 1e-10, "palObs",
        "3/H", status );
  viv( lstat, 0, "palObs", "retval", status );

  lstat = palObs ( 84, NULL, shortname, sizeof(shortname),
                   longname, sizeof(longname), &w, &p, &h );
  vcs ( longname, "?", "palObs", "4/NAME", status );
  viv( lstat, -1, "palObs", "retval", status );

  lstat = palObs ( 0, "MISSING", shortname, sizeof(shortname),
                   longname, sizeof(longname), &w, &p, &h );
  vcs ( longname, "?", "palObs", "5/NAME", status );
  viv( lstat, -1, "palObs", "retval", status );

  lstat = palObs( 0, "mmt", shortname, sizeof(shortname),
                  longname, sizeof(longname), &w, &p, &h );
  vcs ( shortname, "MMT", "palObs", "6/C", status );
  vcs ( longname, "MMT 6.5m, Mt Hopkins", "palObs", "6/NAME",
        status );
  vvd ( w, 1.935300584055477, 1e-8, "palObs",
        "6/W", status );
  vvd ( p, 0.5530735081550342238, 1e-10, "palObs",
        "6/P", status );
  vvd ( h, 2608, 1e-10, "palObs",
        "6/H", status );
  viv( lstat, 0, "palObs", "retval", status );

}

static void t_pa( int *status ) {
  vvd ( palPa ( -1.567, 1.5123, 0.987 ),
        -1.486288540423851, 1e-12, "palPa", " ", status );
  vvd ( palPa ( 0, 0.789, 0.789 ),
        0, 0, "palPa", "zenith", status );
}

static void t_planet( int * status ) {
  int j;
  double pv[6];
  double u[13];
  double expected1[6] = { 0., 0., 0., 0., 0., 0. };
  double expectedue[13] = {
    1.00006, -4.856142884511782, 50000., 0.3, -0.2,
    0.1,  -0.4520378601821727,  0.4018114312730424,
    -.3515850023639121, 0.3741657386773941,
    -0.2511321445456515, 50000., 0.
  };
  double expectedpv[6] = {
    0.07944764084631667011, -0.04118141077419014775,
    0.002915180702063625400, -0.6890132370721108608e-6,
    0.4326690733487621457e-6, -0.1763249096254134306e-6,
  };
  double ra,dec,diam;

  palPlanet( 1e6, 0, pv, &j );
  vvec( 6, pv, expected1, "palPlanet 1", status );
  viv ( j, -1, "palPlanet", "J 1", status );

  palPlanet( 1e6, 9, pv, &j);
  viv ( j, -1, "palPlanet", "J 2", status );

  palPlanet ( -320000, 3, pv, &j );
  vvd ( pv[0], 0.9308038666827242603, 1e-11, "palPlanet",
        "pv[0] 3", status );
  vvd ( pv[1], 0.3258319040252137618, 1e-11, "palPlanet",
        "pv[1] 3", status );
  vvd ( pv[2], 0.1422794544477122021, 1e-11, "palPlanet",
        "pv[2] 3", status );
  vvd ( pv[3], -7.441503423889371696e-8, 1e-17, "palPlanet",
        "pv[3] 3", status );
  vvd ( pv[4], 1.699734557528650689e-7, 1e-17, "palPlanet",
        "pv[4] 3", status );
  vvd ( pv[5], 7.415505123001430864e-8, 1e-17, "palPlanet",
        "pv[5] 3", status );
  viv ( j, 1, "palPlanet", "J 3", status );

  palPlanet ( 43999.9, 1, pv, &j );
  vvd ( pv[0], 0.2945293959257422246, 1e-11, "palPlanet",
        "pv[0] 4", status );
  vvd ( pv[1], -0.2452204176601052181, 1e-11, "palPlanet",
        "pv[1] 4", status );
  vvd ( pv[2], -0.1615427700571978643, 1e-11, "palPlanet",
        "pv[2] 4", status );
  vvd ( pv[3], 1.636421147459047057e-7, 1e-18, "palPlanet",
        "pv[3] 4", status );
  vvd ( pv[4], 2.252949422574889753e-7, 1e-18, "palPlanet",
        "pv[4] 4", status );
  vvd ( pv[5], 1.033542799062371839e-7, 1e-18, "palPlanet",
        "pv[5] 4", status );
  viv ( j, 0, "palPlanet", "J 4", status );

  /* palPlante test would go here */

  /* palPlantu */

  /* palPv2el */

  pv[0] = 0.3;
  pv[1] = -0.2;
  pv[2] = 0.1;
  pv[3] = -0.9e-7;
  pv[4] = 0.8e-7;
  pv[5] = -0.7e-7;

  /* palPv2ue */
  palPv2ue ( pv, 50000., 0.00006, u, &j );
  vvec( 13, u, expectedue, "palPv2ue", status );
  viv ( j, 0, "palPv2ue", "J", status );

  /* Planets */
  palRdplan ( 40999.9, 0, 0.1, -0.9, &ra, &dec, &diam );
  vvd ( ra, 5.772270359389275837, 1e-6, "palRdplan",
        "ra 0", status );
  vvd ( dec, -0.2089207338795416192, 1e-7, "palRdplan",
        "dec 0", status );
  vvd ( diam, 9.415338935229717875e-3, 1e-10, "palRdplan",
        "diam 0", status );
  palRdplan ( 41999.9, 1, 1.1, -0.9, &ra, &dec, &diam );
  vvd ( ra, 3.866363420052936653, 1e-6, "palRdplan",
        "ra 1", status );
  vvd ( dec, -0.2594430577550113130, 1e-7, "palRdplan",
        "dec 1", status );
  vvd ( diam, 4.638468996795023071e-5, 1e-14, "palRdplan",
        "diam 1", status );
  palRdplan ( 42999.9, 2, 2.1, 0.9, &ra, &dec, &diam );
  vvd ( ra, 2.695383203184077378, 1e-6, "palRdplan",
        "ra 2", status );
  vvd ( dec, 0.2124044506294805126, 1e-7, "palRdplan",
        "dec 2", status );
  vvd ( diam, 4.892222838681000389e-5, 1e-14, "palRdplan",
        "diam 2", status );
  palRdplan ( 43999.9, 3, 3.1, 0.9, &ra, &dec, &diam );
  vvd ( ra, 2.908326678461540165, 1e-7, "palRdplan",
        "ra 3", status );
  vvd ( dec, 0.08729783126905579385, 1e-7, "palRdplan",
        "dec 3", status );
  vvd ( diam, 8.581305866034962476e-3, 1e-7, "palRdplan",
        "diam 3", status );
  palRdplan ( 44999.9, 4, -0.1, 1.1, &ra, &dec, &diam );
  vvd ( ra, 3.429840787472851721, 1e-6, "palRdplan",
        "ra 4", status );
  vvd ( dec, -0.06979851055261161013, 1e-7, "palRdplan",
        "dec 4", status );
  vvd ( diam, 4.540536678439300199e-5, 1e-14, "palRdplan",
        "diam 4", status );
  palRdplan ( 45999.9, 5, -1.1, 0.1, &ra, &dec, &diam );
  vvd ( ra, 4.864669466449422548, 1e-6, "palRdplan",
        "ra 5", status );
  vvd ( dec, -0.4077714497908953354, 1e-7, "palRdplan",
        "dec 5", status );
  vvd ( diam, 1.727945579027815576e-4, 1e-14, "palRdplan",
        "diam 5", status );
  palRdplan ( 46999.9, 6, -2.1, -0.1, &ra, &dec, &diam );
  vvd ( ra, 4.432929829176388766, 1e-6, "palRdplan",
        "ra 6", status );
  vvd ( dec, -0.3682820877854730530, 1e-7, "palRdplan",
        "dec 6", status );
  vvd ( diam, 8.670829016099083311e-5, 1e-14, "palRdplan",
        "diam 6", status );
  palRdplan ( 47999.9, 7, -3.1, -1.1, &ra, &dec, &diam );
  vvd ( ra, 4.894972492286818487, 1e-6, "palRdplan",
        "ra 7", status );
  vvd ( dec, -0.4084068901053653125, 1e-7, "palRdplan",
        "dec 7", status );
  vvd ( diam, 1.793916783975974163e-5, 1e-14, "palRdplan",
        "diam 7", status );
  palRdplan ( 48999.9, 8, 0, 0, &ra, &dec, &diam );
  vvd ( ra, 5.066050284760144000, 1e-6, "palRdplan",
        "ra 8", status );
  vvd ( dec, -0.3744690779683850609, 1e-7, "palRdplan",
        "dec 8", status );
  vvd ( diam, 1.062210086082700563e-5, 1e-14, "palRdplan",
        "diam 8", status );

  /* palUe2el */

  /* palUe2pv */
  palUe2pv( 50010., u, pv, &j );

  /* Update the final two elements of the expecte UE array */
  expectedue[11] = 50010.;
  expectedue[12] = 0.7194308220038886856;

  vvec( 13, u, expectedue, "palUe2pv", status );
  vvec( 6, pv, expectedpv, "palU2pv", status );
  viv ( j, 0, "palUe2pv", "J", status );

}

static void t_pm( int * status ) {
  double ra2, dec2;
  double ra1, dec1, pmr1, pmd1, px1, rv1;

  ra1 = 5.43;
  dec1 = -0.87;
  pmr1 = -0.33e-5;
  pmd1 = 0.77e-5;
  px1 = 0.7;
  rv1 = 50.3*365.2422/365.25;

  palPm ( ra1, dec1, pmr1, pmd1, px1, rv1,
          1899, 1943,
          &ra2, &dec2 );
  vvd ( ra2, 5.429855087793875, 1e-10, "palPm",
        "R", status );
  vvd ( dec2, -0.8696617307805072, 1e-10, "palPm",
        "D", status );

  /* SOFA test */
  ra1 =   0.01686756;
  dec1 = -1.093989828;
  pmr1 = -1.78323516e-5;
  pmd1 =  2.336024047e-6;
  px1 =   0.74723;
  rv1 = -21.6;

  palPm(ra1, dec1, pmr1, pmd1, px1, rv1,
        palEpj(50083.0), palEpj(53736.0),
        &ra2, &dec2);
  vvd(ra2, 0.01668919069414242368, 1e-13,
      "palPm", "ra", status);
  vvd(dec2, -1.093966454217127879, 1e-13,
      "palPm", "dec", status);


}

static void t_pvobs( int *status ) {
   double pv[6];
   double expected[6] = { -4.7683600138836167813e-06,
                           1.0419056712717953176e-05,
                           4.099831053320363277e-05,
                          -7.5976959740661272483e-10,
                          -3.4771429582640930371e-10,
                           0.0};
   palPvobs( 1.3, 10000.0, 2.0, pv );
   vvec( 6, pv, expected, "palPvobs", status );
}

static void t_rv( int *status ) {
  vvd ( palRverot ( -0.777, 5.67, -0.3, 3.19 ),
        -0.1948098355075913, 1e-6,
        "palRverot", " ", status );
  vvd ( palRvgalc ( 1.11E0, -0.99E0 ),
        158.9630759840254, 1e-3, "palRvgalc", " ", status );
  vvd ( palRvlg ( 3.97E0, 1.09E0 ),
        -197.818762175363, 1e-3, "palRvlg", " ", status );
  vvd ( palRvlsrd ( 6.01E0, 0.1E0 ),
        -4.082811335150567, 1e-4, "palRvlsrd", " ", status );
  vvd ( palRvlsrk ( 6.01E0, 0.1E0 ),
        -5.925180579830265, 1e-4, "palRvlsrk", " ", status );
}

static void t_rvgalc( int *status ) {
   double rv;
   rv = palRvgalc( 2.7, -1.0 );
   vvd( rv, 213.98084425751144977, 1.0E-12, "palRvgalc", "rv", status );
}

static void t_rvlg( int *status ) {
   double rv;
   rv = palRvlg( 2.7, -1.0 );
   vvd( rv, 291.79205281252404802, 1.0E-12, "palRvlg", "rv", status );
}

static void t_rvlsrd( int *status ) {
   double rv;
   rv = palRvlsrd( 2.7, -1.0 );
   vvd( rv, 9.620674692097630043, 1.0E-12, "palRvlsrd", "rv", status );
}

static void t_rvlsrk( int *status ) {
   double rv;
   rv = palRvlsrk( 2.7, -1.0 );
   vvd( rv, 12.556356851411955233, 1.0E-12, "palRvlsrk", "rv", status );
}







/**********************************************************************/

int main (void) {

  /* Use the SLA and SOFA conventions */
  int status = 0; /* Unix and SAE convention */

  t_addet(&status);
  t_afin(&status);
  t_ampqk(&status);
  t_airmas(&status);
  t_amp(&status);
  t_bear(&status);
  t_caf2r(&status);
  t_caldj(&status);
  t_cc2s(&status);
  t_cd2tf(&status);
  t_cldj(&status);
  t_cr2af(&status);
  t_cr2tf(&status);
  t_ctf2d(&status);
  t_ctf2r(&status);
  t_dat(&status);
  t_djcal(&status);
  t_dmat(&status);
  t_epb(&status);
  t_epb2d(&status);
  t_epco(&status);
  t_epj(&status);
  t_epj2d(&status);
  t_eqecl(&status);
  t_eqeqx(&status);
  t_etrms(&status);
  t_eqgal(&status);
  t_evp(&status);
  t_fk45z(&status);
  t_fk54z(&status);
  t_flotin(&status);
  t_galeq(&status);
  t_galsup(&status);
  t_geoc(&status);
  t_gmst(&status);
  t_fk52h(&status);
  t_prec(&status);
  t_preces(&status);
  t_ecmat(&status);
  t_e2h(&status);
  t_map(&status);
  t_mappa(&status);
  t_mapqkz(&status);
  t_moon(&status);
  t_nut(&status);
  t_obs(&status);
  t_pa(&status);
  t_planet(&status);
  t_pm(&status);
  t_prebn(&status);
  t_pvobs(&status);
  t_range(&status);
  t_ranorm(&status);
  t_rv(&status);
  t_rvgalc(&status);
  t_rvlg(&status);
  t_rvlsrd(&status);
  t_rvlsrk(&status);
  t_sep(&status);
  t_supgal(&status);
  t_tp(&status);
  t_vecmat(&status);
  return status;
}

