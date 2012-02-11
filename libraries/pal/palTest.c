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

/* Test vector routines.
   Start with a subset */

static void t_vecmat( int * status ) {

  double drm2[3][3];
  double deuler_expected[3][3] = {
    { -0.1681574770810878,  0.1981362273264315,  0.9656423242187410 },
    { -0.2285369373983370,  0.9450659587140423, -0.2337117924378156 },
    { -0.9589024617479674, -0.2599853247796050, -0.1136384607117296 } };

  /* Test palDeuler */
  palDeuler( "YZY", 2.345, -0.333, 2.222, drm2 );
  vrmat( drm2, deuler_expected, "palDeuler", status );
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

  t_prec(&status);
  t_ecmat(&status);
  t_e2h(&status);
  t_tp(&status);
  t_vecmat(&status);
  return status;
}

