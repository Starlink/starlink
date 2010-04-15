#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/*
** Use flags OLD_DCMPF and FORTRAN_DCMPF to control which dcmpf
** routine is actually tested.  The regression test case is
** -DOLD_DCMPF=0 -DFORTRAN_DCMPF=1, and this is the default.
**
** If you're not linking against the SLALIB library (which is the case
** if you're using -DFORTRAN_DCMPF=0, then you need to find a
** slaDrange to link against, probably from drange.c of the SLALIB C
** distribution.
*/

/* If OLD_DCMPF is true, then use the old, broken, C version of
** slaDcmpf, to verify cases that used to fail.
*/
#ifndef OLD_DCMPF
#define OLD_DCMPF 0
#endif

/* If FORTRAN_DCMPF is true, then link this against the Fortran version of
** this routine
*/
#ifndef FORTRAN_DCMPF
#  if OLD_DCMPF
#    define FORTRAN_DCMPF 0
#  else
#    define FORTRAN_DCMPF 1
#  endif
#endif

/* If DEBUG is true, then chatter */
#ifndef DEBUG
#define DEBUG 0
#endif

/* We want to link this against Fortran libraries.  That can cause
   linking errors if those libraries require a Fortran main function.
   autoconf/configure.ac has defined F77_DUMMY_MAIN to the required
   function, if this is necessary. */
#ifdef F77_DUMMY_MAIN
int F77_DUMMY_MAIN() { return 1; }
#endif

#if !FORTRAN_DCMPF

#include "slalib.h"
void slaDcmpf ( double coeffs[6],
                double *xz, double *yz, double *xs,
                double *ys, double *perp, double *orient )
/*
**  - - - - - - - - -
**   s l a D c m p f
**  - - - - - - - - -
**
**  Decompose an [x,y] linear fit into its constituent parameters:
**  zero points, scales, nonperpendicularity and orientation.
**
**  Given:
**     coeffs    double[6]     transformation coefficients (see note)
**
**  Returned:
**     *xz       double        x zero point
**     *yz       double        y zero point
**     *xs       double        x scale
**     *ys       double        y scale
**     *perp     double        nonperpendicularity (radians)
**     *orient   double        orientation (radians)
**
**  Called:  slaDrange
**
**  The model relates two sets of [x,y] coordinates as follows.
**  Naming the elements of coeffs:
**
**     coeffs[0] = a
**     coeffs[1] = b
**     coeffs[2] = c
**     coeffs[3] = d
**     coeffs[4] = e
**     coeffs[5] = f
**
**  The model transforms coordinates [x1,y1] into coordinates
**  [x2,y2] as follows:
**
**     x2 = a + b*x1 + c*y1
**     y2 = d + e*x1 + f*y1
**
**  The transformation can be decomposed into four steps:
**
**     1)  Zero points:
**
**             x' = xz + x1
**             y' = yz + y1
**
**     2)  Scales:
**
**             x'' = xs*x'
**             y'' = ys*y'
**
**     3)  Nonperpendicularity:
**
**             x''' = cos(perp/2)*x'' + sin(perp/2)*y''
**             y''' = sin(perp/2)*x'' + cos(perp/2)*y''
**
**     4)  Orientation:
**
**             x2 = cos(orient)*x''' + sin(orient)*y'''
**             y2 =-sin(orient)*y''' + cos(orient)*y'''
**
**  See also slaFitxy, slaPxy, slaInvf, slaXy2xy
**
**  Last revision:   19 December 2001
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double a, b, c, d, e, f, rb2e2, rc2f2, xsc, ysc, p,
          ws, wc, or, hp, shp, chp, sor, cor, det, x0, y0;


/* Copy the six coefficients. */
   a = coeffs[0];
   b = coeffs[1];
   c = coeffs[2];
   d = coeffs[3];
   e = coeffs[4];
   f = coeffs[5];

/* Scales. */
   rb2e2 = sqrt ( b * b + e * e );
   rc2f2 = sqrt ( c * c + f * f );
   if ( ( b * f - c * e ) >= 0.0 )
      xsc = rb2e2;
   else {
      b = -b;
      e = -e;
      xsc = -rb2e2;
   }
   ysc = rc2f2;

/* Non-perpendicularity. */
   p = slaDrange ( ( ( c != 0.0 || f != 0.0 ) ? atan2 ( c, f ) : 0.0 ) +
                   ( ( e != 0.0 || b != 0.0 ) ? atan2 ( e, b ) : 0.0 ) );

/* Orientation. */
   ws = ( c * rb2e2 ) - ( e * rc2f2 );
   wc = ( b * rc2f2 ) + ( f * rb2e2 );
   or = ( ws != 0.0 || wc != 0.0 ) ? atan2 ( ws, wc ) : 0.0;

/* Zero points. */
   hp = p / 2.0;
   shp = sin ( hp );
   chp = cos ( hp );
   sor = sin ( or );
   cor = cos ( or );
   det = xsc * ysc * ( chp + shp ) * ( chp - shp );
   if ( fabs ( det ) > 0.0 ) {
      x0 = ysc * ( a * ( ( chp * cor ) - ( shp * sor ) )
                 - d * ( ( chp * sor ) + ( shp * cor ) ) ) / det;
      y0 = xsc * ( a * ( ( chp * sor ) - ( shp * cor ) )
                 + d * ( ( chp * cor ) + ( shp * sor ) ) ) / det;
   }
   else {
      x0 = 0.0;
      y0 = 0.0;
   }

/* Results. */
   *xz = x0;
   *yz = y0;
   *xs = xsc;
   *ys = ysc;
   *perp = p;
   *orient = or;
}

/* Old version of slaDcmpf, priort to Pat's fix (here to allow me to
** check test cases that used to fail)
*/
void slaDcmpf_old ( double coeffs[6],
                double *xz, double *yz, double *xs,
                double *ys, double *perp, double *orient )
/*
**  - - - - - - - - -
**   s l a D c m p f
**  - - - - - - - - -
**
**  Decompose an [x,y] linear fit into its constituent parameters:
**  zero points, scales, nonperpendicularity and orientation.
**
**  Given:
**     coeffs    double[6]     transformation coefficients (see note)
**
**  Returned:
**     *xz       double        x zero point
**     *yz       double        y zero point
**     *xs       double        x scale
**     *ys       double        y scale
**     *perp     double        nonperpendicularity (radians)
**     *orient   double        orientation (radians)
**
**  The model relates two sets of [x,y] coordinates as follows.
**  Naming the elements of coeffs:
**
**     coeffs[0] = a
**     coeffs[1] = b
**     coeffs[2] = c
**     coeffs[3] = d
**     coeffs[4] = e
**     coeffs[5] = f
**
**  The model transforms coordinates [x1,y1] into coordinates
**  [x2,y2] as follows:
**
**     x2 = a + b*x1 + c*y1
**     y2 = d + e*x1 + f*y1
**
**  The transformation can be decomposed into four steps:
**
**     1)  Zero points:
**
**             x' = xz + x1
**             y' = yz + y1
**
**     2)  Scales:
**
**             x'' = xs*x'
**             y'' = ys*y'
**
**     3)  Nonperpendicularity:
**
**             x''' = cos(perp/2)*x'' + sin(perp/2)*y''
**             y''' = sin(perp/2)*x'' + cos(perp/2)*y''
**
**     4)  Orientation:
**
**             x2 = cos(orient)*x''' + sin(orient)*y'''
**             y2 =-sin(orient)*y''' + cos(orient)*y'''
**
**  See also slaFitxy, slaPxy, slaInvf, slaXy2xy
**
**  Last revision:   22 September 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double a, b, c, d, e, f, rb2e2, rc2f2, xsc, ysc, p,
          ws, wc, or, hp, shp, chp, sor, cor, det, x0, y0;

/* Copy the six coefficients */
   a = coeffs[0];
   b = coeffs[1];
   c = coeffs[2];
   d = coeffs[3];
   e = coeffs[4];
   f = coeffs[5];

/* Scales */
   rb2e2 = sqrt ( b * b + e * e );
   rc2f2 = sqrt ( c * c + f * f );
   if ( ( b * f - c * e ) >= 0.0 )
     xsc = rb2e2;
   else {
     b = -b;
     c = -c;
     xsc = -rb2e2;
   }
   ysc = rc2f2;

/* Non-perpendicularity */
   p = ( ( c != 0.0 || f != 0.0 ) ? atan2 ( c, f ) : 0.0 ) +
       ( ( e != 0.0 || b != 0.0 ) ? atan2 ( e, b ) : 0.0 );

/* Orientation */
   ws = ( c * rb2e2 ) - ( e * rc2f2 );
   wc = ( b * rc2f2 ) + ( f * rb2e2 );
   or = ( ws != 0.0 || wc != 0.0 ) ? atan2 ( ws, wc ) : 0.0;

/* Zero corrections */
   hp = p / 2.0;
   shp = sin ( hp );
   chp = cos ( hp );
   sor = sin ( or );
   cor = cos ( or );
   det = xsc * ysc * ( chp + shp ) * ( chp - shp );
   if ( fabs ( det ) > 0.0 ) {
     x0 = ysc * ( a * ( ( chp * cor ) - ( shp * sor ) )
                - d * ( ( chp * sor ) + ( shp * cor ) ) ) / det;
     y0 = xsc * ( a * ( ( chp * sor ) - ( shp * cor ) )
                + d * ( ( chp * cor ) + ( shp * sor ) ) ) / det;
   }
   else {
     x0 = 0.0;
     y0 = 0.0;
   }

/* Results */
   *xz = x0;
   *yz = y0;
   *xs = xsc;
   *ys = ysc;
   *perp = p;
   *orient = or;
}

#endif


/*
** Compare two doubles, with tolerance.  Return non-zero if they're DIFFERENT
*/
int doubles_unequal(double d1, double d2)
{
    return fabs(d1-d2) > 1e-7;
}


/* Do the forward transformation that slaDcmpf inverts.
**
** Arg vector is xz, yz, xs, ys, perp, orient.
** Replace with a, b, c, d, e, f.
*/
void make_cpts (double v[])
{
    double xz = v[0];
    double yz = v[1];
    double xs = v[2];
    double ys = v[3];
    double perp = v[4];
    double orient = v[5];
    double r2d, c1, s1, c2, s2, rk11, rk12, rk21, rk22;
    r2d = 57.295779513082320876; /* radians to degrees */
    c1 = cos (orient/r2d);
    s1 = sin (orient/r2d);
    c2 = cos (perp/2.0/r2d);
    s2 = sin (perp/2.0/r2d);
    rk11 = +c1*c2+s1*s2;
    rk12 = +c1*s2+s1*c2;
    rk21 = -s1*c2+c1*s2;
    rk22 = -s1*s2+c1*c2;

    v[0] = rk11*xs*xz + rk12*ys*yz;
    v[1] = rk11*xs;
    v[2] = rk12*ys;
    v[3] = rk21*xs*xz + rk22*ys*yz;
    v[4] = rk21*xs;
    v[5] = rk22*ys;

    return;
}

int main (int argc, char **argv)
{
#if FORTRAN_DCMPF
    void sla_dcmpf_ ( double coeffs[6],
		      double *xz, double *yz, double *xs,
		      double *ys, double *perp, double *orient );
#endif

    /* pars[] is an array of groups of six parameter values, and their
       expected results.  The expected values are the same as the
       input values, except where noted. */
    double pars[][12] = {
	{ 0.00, 0.00,  1.00,  1.00,   0,   60,
          0.00, 0.00,  1.00,  1.00,   0,   60  },
	{ 0.00, 0.00,  1.00,  1.00,   0,  120,
          0.00, 0.00,  1.00,  1.00,   0,  120  },
	{ 0.00, 0.00,  1.00,  1.00,   0, -120,
          0.00, 0.00,  1.00,  1.00,   0, -120  },
	{ 0.00, 0.00, -1.00,  1.00,   0, -120,
          0.00, 0.00, -1.00,  1.00,   0, -120  },
	{ 0.00, 0.00, -1.00,  1.00,   0, +120,
          0.00, 0.00, -1.00,  1.00,   0,  120  },
	{ 0.00, 0.00, -1.00,  1.00,  60,  -90,
          -0.00,-0.00, -1.00,  1.00,  60, -90  },
	{ 0.00, 0.00, -1.00,  1.00, -60,  -90,
          -0.00, 0.00, -1.00,  1.00, -60, -90  },
	{ 0.00, 0.00,  1.00,  1.00,  60,  -90,
          0.00, 0.00,  1.00,  1.00,  60,  -90  },
	{ 0.00, 0.00,  1.00,  1.00, -60,  -90,
          0.00, 0.00,  1.00,  1.00, -60,  -90  },
	{ 0.00, 0.00,  1.00,  1.00,  60,   90,
          0.00, 0.00,  1.00,  1.00,  60,   90  },
	{ 0,    0,    -1,    -1,      0,    0,
          0.00, 0.00,  1.00,  1.00,  -0,  180  }, /* different */
	{ 0,    0,    +1,    +1,   -360,  180,
          0.00, 0.00,  1.00,  1.00,   0,   -0  }, /* different */
	{ 0,    0,    +2,    +2,      0,    0,
          0.00, 0.00,  2.00,  2.00,   0,    0  },
	{ 10,   5,    +1,    +2,      0,   45,
          10.00, 5.00,  1.00,  2.00,   0,  45  },
	{ -5,  -2,    +1,    +1,     45,    0,
          -5.00,-2.00,  1.00,  1.00,  45,   0  },
        { 0.00, 0.00,  1.00,  1.00,  10,   0,
          0.00, 0.00,  1.00,  1.00,  10,   0  },
        { 0.00, 0.00,  1.00,  1.00, -10,   0,
          0.00, 0.00,  1.00,  1.00, -10,   0  },
        { 0.00, 0.00,  1.00, -1.00,  10,   0,
          -0.00, 0.00, -1.00,  1.00,  10, 180  }, /* different */
        { 0.00, 0.00,  1.00, -1.00, -10,   0,
          -0.00, 0.00, -1.00,  1.00, -10, 180  }, /* different */
        { 0.00, 0.00, -1.00, -1.00,  10,   0,
          0.00, 0.00,  1.00,  1.00,  10, 180  }, /* different */
        { 0.00, 0.00, -1.00, -1.00, -10,   0,
          -0.00,-0.00,  1.00,  1.00, -10, 180  }, /* different */
    };
    int npars = sizeof(pars)/(sizeof(pars[0]));
    int i;
    double xz, yz, xs, ys, perp, orient;
    double r2d = 57.295779513082320876;
    int headerdone = 0;
    int numbad = 0;

    for (i=0; i<npars; i++) {
        int badflag;
        int j;
        double mat[6];

	/* Do the forward transformation -- convert these parameters
	   to matrix elements, in place */
        for (j=0; j<6; j++)
            mat[j] = pars[i][j];
	make_cpts (mat);

#if DEBUG
        printf ("     %5s %5s %6s %6s %4s %4s      %5s %5s %5s %5s %5s %5s      %5s %5s %6s %6s %4s %4s \n",
                "xz", "yz", "xs", "ys", "perp", "ori",
                "a", "b", "c", "d", "e", "f",
                "xz", "yz", "xs", "ys", "perp", "ori");
        headerdone = 1;
        printf ("%2d: (%g,%g,%g,%g,%g,%g) -> (%g,%g,%g,%g,%g,%g)\n",
                i,
                pars[i][0], pars[i][1], pars[i][2],
                pars[i][3], pars[i][4], pars[i][5],
                mat[0], mat[1], mat[2], mat[3], mat[4], mat[5]);
#endif

	/* Do the inversion -- transform the matrix elements to
	   parameters, returning the parameters in xz, yz, xs, ys,
	   perp and orient. */
#if FORTRAN_DCMPF
	sla_dcmpf_   (mat, &xz, &yz, &xs, &ys, &perp, &orient);
#elif OLD_DCMPF
	slaDcmpf_old (mat, &xz, &yz, &xs, &ys, &perp, &orient);
#else
	slaDcmpf     (mat, &xz, &yz, &xs, &ys, &perp, &orient);
#endif

        badflag = 0;
        badflag |= doubles_unequal(xz,         pars[i][6]);
        badflag |= doubles_unequal(yz,         pars[i][7]);
        badflag |= doubles_unequal(xs,         pars[i][8]);
        badflag |= doubles_unequal(ys,         pars[i][9]);
        badflag |= doubles_unequal(perp*r2d,   pars[i][10]);
        badflag |= doubles_unequal(orient*r2d, pars[i][11]);

        if (badflag) {
            if (! headerdone) {
                printf ("          %5s %5s %6s %6s %4s %4s      %5s %5s %5s %5s %5s %5s      %5s %5s %6s %6s %4s %4s \n",
                        "xz", "yz", "xs", "ys", "perp", "ori",
                        "a", "b", "c", "d", "e", "f",
                        "xz", "yz", "xs", "ys", "perp", "ori");
                headerdone = 1;
            }

            printf ("BAD: %2d: (%5.2f,%5.2f,%6.2f,%6.2f,%4.0f,%4.0f) -> (%5.2f,%5.2f,%5.2f,%5.2f,%5.2f,%5.2f) -> (%5.2f,%5.2f,%6.2f,%6.2f,%4.0f,%4.0f) not (%5.2f,%5.2f,%6.2f,%6.2f,%4.0f,%4.0f)\n",
                    i,
                    pars[i][0],
                    pars[i][1],
                    pars[i][2],
                    pars[i][3],
                    pars[i][4],
                    pars[i][5],
                    mat[0],
                    mat[1],
                    mat[2],
                    mat[3],
                    mat[4],
                    mat[5],
                    xz, yz, xs, ys, perp*r2d, orient*r2d,
                    pars[i][6],
                    pars[i][7],
                    pars[i][8],
                    pars[i][9],
                    pars[i][10],
                    pars[i][11]
                    );
            numbad++;
        }
    }

    printf ("%s\n", (numbad > 0 ? "BAD" : "ok"));
    exit (0);
}
