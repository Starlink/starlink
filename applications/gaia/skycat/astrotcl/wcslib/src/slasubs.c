/* File slasubs.c
 *** Starlink subroutines by Patrick Wallace used by wcscon.c subroutines
 *** April 13, 1998
 */

#include <math.h>
#include <string.h>
#include "slasubs.h"

/*  slaDcs2c (a, b, v): Spherical coordinates to direction cosines.
 *  slaDcc2s (v, a, b):  Direction cosines to spherical coordinates.
 *  slaDmxv (dm, va, vb): vector vb = matrix dm * vector va
 *  slaImxv (rm, va, vb): vector vb = (inverse of matrix rm) * vector va
 *  slaDranrm (angle):  Normalize angle into range 0-2 pi.
 *  slaDrange (angle):  Normalize angle into range +/- pi.
 *  slaDeuler (order, phi, theta, psi, rmat)
 *	      Form a rotation matrix from the Euler angles - three successive
 *	      rotations about specified Cartesian axes.
 */

void
slaDcs2c (a, b, v)

double a;	/* Right ascension in radians */
double b;	/* Declination in radians */
double *v;	/* x,y,z unit vector (returned) */

/*
**  slaDcs2c: Spherical coordinates to direction cosines.
**
**  The spherical coordinates are longitude (+ve anticlockwise
**  looking from the +ve latitude pole) and latitude.  The
**  Cartesian coordinates are right handed, with the x axis
**  at zero longitude and latitude, and the z axis at the
**  +ve latitude pole.
**
**  P.T.Wallace   Starlink   31 October 1993
*/
{
    double cosb;
 
    cosb = cos ( b );
    v[0] = cos ( a ) * cosb;
    v[1] = sin ( a ) * cosb;
    v[2] = sin ( b );
}


void
slaDcc2s (v, a, b)

double *v;	/* x,y,z vector */
double *a;	/* Right ascension in radians */
double *b;	/* Declination in radians */

/*
**  slaDcc2s:
**  Direction cosines to spherical coordinates.
**
**  Returned:
**     *a,*b  double      spherical coordinates in radians
**
**  The spherical coordinates are longitude (+ve anticlockwise
**  looking from the +ve latitude pole) and latitude.  The
**  Cartesian coordinates are right handed, with the x axis
**  at zero longitude and latitude, and the z axis at the
**  +ve latitude pole.
**
**  If v is null, zero a and b are returned.
**  At either pole, zero a is returned.
**
**  P.T.Wallace   Starlink   31 October 1993
*/
{
    double x, y, z, r;
 
    x = v[0];
    y = v[1];
    z = v[2];
    r = sqrt ( x * x + y * y );
 
    *a = ( r != 0.0 ) ? atan2 ( y, x ) : 0.0;
    *b = ( z != 0.0 ) ? atan2 ( z, r ) : 0.0;
}


void
slaDmxv (dm, va, vb)

double (*dm)[3];	/* 3x3 Matrix */
double *va;		/* Vector */
double *vb;		/* Result vector (returned) */

/*
**  slaDmxv:
**  Performs the 3-d forward unitary transformation:
**     vector vb = matrix dm * vector va
**
**  P.T.Wallace   Starlink   31 October 1993
*/
{
    int i, j;
    double w, vw[3];
 
    /* Matrix dm * vector va -> vector vw */
    for ( j = 0; j < 3; j++ ) {
	w = 0.0;
	for ( i = 0; i < 3; i++ ) {
	    w += dm[j][i] * va[i];
	    }
	vw[j] = w;
	}
 
    /* Vector vw -> vector vb */
    for ( j = 0; j < 3; j++ ) {
	vb[j] = vw[j];
	}
}


void slaDimxv (dm, va, vb)
     double (*dm)[3];
     double *va;
     double *vb;
/*
**  - - - - - - - - -
**   s l a D i m x v
**  - - - - - - - - -
**
**  Performs the 3-d backward unitary transformation:
**
**     vector vb = (inverse of matrix dm) * vector va
**
**  (double precision)
**
**  (n.b.  The matrix must be unitary, as this routine assumes that
**   the inverse and transpose are identical)
**
**
**  Given:
**     dm       double[3][3]   matrix
**     va       double[3]      vector
**
**  Returned:
**     vb       double[3]      result vector
**
**  P.T.Wallace   Starlink   31 October 1993
*/
{
  long i, j;
  double w, vw[3];
 
/* Inverse of matrix dm * vector va -> vector vw */
   for ( j = 0; j < 3; j++ ) {
      w = 0.0;
      for ( i = 0; i < 3; i++ ) {
         w += dm[i][j] * va[i];
      }
      vw[j] = w;
   }
 
/* Vector vw -> vector vb */
   for ( j = 0; j < 3; j++ ) {
     vb[j] = vw[j];
   }
}

 
/* 2pi */
#define D2PI 6.2831853071795864769252867665590057683943387987502

/* pi */
#define DPI 3.1415926535897932384626433832795028841971693993751

double slaDranrm (angle)

double angle;	/* angle in radians */

/*
**  slaDranrm:
**  Normalize angle into range 0-2 pi.
**  The result is angle expressed in the range 0-2 pi (double).
**  Defined in slamac.h:  D2PI
**
**  P.T.Wallace   Starlink   30 October 1993
*/
{
    double w;
 
    w = fmod ( angle, D2PI );
    return ( w >= 0.0 ) ? w : w + D2PI;
}

#ifndef dsign
#define dsign(A,B) ((B)<0.0?-(A):(A))
#endif

double
slaDrange (angle)
     double angle;
/*
**  - - - - - - - - - -
**   s l a D r a n g e
**  - - - - - - - - - -
**
**  Normalize angle into range +/- pi.
**
**  (double precision)
**
**  Given:
**     angle     double      the angle in radians
**
**  The result is angle expressed in the +/- pi (double precision).
**
**  Defined in slamac.h:  DPI, D2PI
**
**  P.T.Wallace   Starlink   31 October 1993
*/
{
  double w;
 
  w = fmod ( angle, D2PI );
  return ( fabs ( w ) < DPI ) ? w : w - dsign ( D2PI, angle );
}


void
slaDeuler (order, phi, theta, psi, rmat)

char *order;		/* specifies about which axes the rotations occur */
double phi;		/* 1st rotation (radians) */
double theta;		/* 2nd rotation (radians) */
double psi;		/* 3rd rotation (radians) */
double (*rmat)[3];	/* 3x3 Rotation matrix (returned) */

/*
**  slaDeuler:
**  Form a rotation matrix from the Euler angles - three successive
**  rotations about specified Cartesian axes.
**
**  A rotation is positive when the reference frame rotates
**  anticlockwise as seen looking towards the origin from the
**  positive region of the specified axis.
**
**  The characters of order define which axes the three successive
**  rotations are about.  A typical value is 'zxz', indicating that
**  rmat is to become the direction cosine matrix corresponding to
**  rotations of the reference frame through phi radians about the
**  old z-axis, followed by theta radians about the resulting x-axis,
**  then psi radians about the resulting z-axis.
**
**  The axis names can be any of the following, in any order or
**  combination:  x, y, z, uppercase or lowercase, 1, 2, 3.  Normal
**  axis labelling/numbering conventions apply;  the xyz (=123)
**  triad is right-handed.  Thus, the 'zxz' example given above
**  could be written 'zxz' or '313' (or even 'zxz' or '3xz').  Order
**  is terminated by length or by the first unrecognised character.
**
**  Fewer than three rotations are acceptable, in which case the later
**  angle arguments are ignored.  Zero rotations produces a unit rmat.
**
**  P.T.Wallace   Starlink   17 November 1993
*/
{
   int j, i, l, n, k;
   double result[3][3], rotn[3][3], angle, s, c , w, wm[3][3];
   char axis;
 
/* Initialize result matrix */
   for ( j = 0; j < 3; j++ ) {
      for ( i = 0; i < 3; i++ ) {
         result[i][j] = ( i == j ) ? 1.0 : 0.0;
      }
   }
 
/* Establish length of axis string */
   l = strlen ( order );
 
/* Look at each character of axis string until finished */
   for ( n = 0; n < 3; n++ ) {
      if ( n <= l ) {
 
      /* Initialize rotation matrix for the current rotation */
         for ( j = 0; j < 3; j++ ) {
            for ( i = 0; i < 3; i++ ) {
               rotn[i][j] = ( i == j ) ? 1.0 : 0.0;
            }
         }
 
      /* Pick up the appropriate Euler angle and take sine & cosine */
         switch ( n ) {
         case 0 :
           angle = phi;
           break;
         case 1 :
           angle = theta;
           break;
         case 2 :
           angle = psi;
           break;
         }
         s = sin ( angle );
         c = cos ( angle );
 
      /* Identify the axis */
         axis =  order[n];
         if ( ( axis == 'X' ) || ( axis == 'x' ) || ( axis == '1' ) ) {
 
         /* Matrix for x-rotation */
            rotn[1][1] = c;
            rotn[1][2] = s;
            rotn[2][1] = -s;
            rotn[2][2] = c;
         }
         else if ( ( axis == 'Y' ) || ( axis == 'y' ) || ( axis == '2' ) ) {
 
         /* Matrix for y-rotation */
            rotn[0][0] = c;
            rotn[0][2] = -s;
            rotn[2][0] = s;
            rotn[2][2] = c;
         }
         else if ( ( axis == 'Z' ) || ( axis == 'z' ) || ( axis == '3' ) ) {
 
         /* Matrix for z-rotation */
            rotn[0][0] = c;
            rotn[0][1] = s;
            rotn[1][0] = -s;
            rotn[1][1] = c;
         } else {
 
         /* Unrecognized character - fake end of string */
            l = 0;
         }
 
      /* Apply the current rotation (matrix rotn x matrix result) */
         for ( i = 0; i < 3; i++ ) {
            for ( j = 0; j < 3; j++ ) {
               w = 0.0;
               for ( k = 0; k < 3; k++ ) {
                  w += rotn[i][k] * result[k][j];
               }
               wm[i][j] = w;
            }
         }
         for ( j = 0; j < 3; j++ ) {
            for ( i= 0; i < 3; i++ ) {
               result[i][j] = wm[i][j];
            }
         }
      }
   }
 
/* Copy the result */
   for ( j = 0; j < 3; j++ ) {
      for ( i = 0; i < 3; i++ ) {
         rmat[i][j] = result[i][j];
      }
   }
}
/*
 * Nov  4 1996	New file
 *
 * Apr 13 1998	Add list of subroutines to start of file
 */
