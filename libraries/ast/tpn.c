#include <math.h>
#include <stdlib.h>
#include "wcsmath.h"
#include "wcstrig.h"
#include "proj.h"

#define icopysign(X, Y) ((Y) < 0.0 ? -abs(X) : abs(X))
#define TPN 999

/*============================================================================
*   TAN: gnomonic projection, with correction terms.
*
*   This projection is no longer part of the FITSWCS standard, but is
*   retained here for use b the AST library as a means of implementing
*   the IRAF TNX projection, and the DSS encoding.
*
*   Given and/or returned:
*      prj->p       Array of latitude coefficients
*      prj->p2      Array of longitude coefficients
*      prj->flag    TPN, or -TPN if prj->flag is given < 0.
*      prj->r0      r0; reset to 180/pi if 0.
*      prj->n       If zero, only do poly part of transformation (i.e. omit
*                   the TAN projection).
*
*   Returned:
*      prj->code    "TPN"
*      prj->phi0     0.0
*      prj->theta0  90.0
*      prj->astPRJfwd  Pointer to astTPNfwd().
*      prj->astPRJrev  Pointer to astTPNrev().
*      prj->w[ 0 ]     Set to 0.0 if a simple tan projection is required
*                      (with no polynomial correction). Otherwise, set to 1.0.
*===========================================================================*/

int astTPNset(prj)

struct AstPrjPrm *prj;

{
   int m;

   prj->flag   = icopysign(TPN, prj->flag);
   prj->phi0   =  0.0;
   prj->theta0 = 90.0;

   if (prj->r0 == 0.0) prj->r0 = R2D;

   prj->astPRJfwd = astTPNfwd;
   prj->astPRJrev = astTPNrev;

/* If all co-efficients have their "unit" values, we do not need to
   use the polynomial correction. */
   prj->w[ 0 ] = 0.0;

   if( prj->p[ 0 ] != 0.0 || prj->p2[ 0 ] != 0.0 ) {
      prj->w[ 0 ] = 1.0;

   } else if( prj->p[ 1 ] != 1.0 || prj->p2[ 1 ] != 1.0 ) {
      prj->w[ 0 ] = 1.0;

   } else {
      for( m = 2; m < WCSLIB_MXPAR; m++ ){
         if( prj->p[ m ] != 0.0 || prj->p2[ m ] != 0.0 ){
            prj->w[ 0 ] = 1.0;
            break;
         }
      }
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int astTPNfwd(phi, theta, prj, xx, yy)

const double phi, theta;
double *xx, *yy;
struct AstPrjPrm *prj;

{
   double r, xi, eta, x2, xy, y2, r2, x3, x2y, xy2, y3, r3, x4, x3y,
          x2y2, xy3, y4, x5, x4y, x3y2, x2y3, xy4, y5, r5, x6, x5y, x4y2,
	  x3y3, x2y4, xy5, y6, x7, x6y, x5y2, x4y3, x3y4, x2y5, xy6, y7,
	  r7, tol, f, g, fx, fy, gx, gy, dx, dy, x, y, denom;
   double *a, *b;
   int i, ok;

   if (abs(prj->flag) != TPN ) {
      if (astTPNset(prj)) return 1;
   }

   if( prj->n ) {
      double s = astSind(theta);
      if (prj->flag > 0 && s < 0.0) {
         return 2;
      }
      r =  prj->r0*astCosd(theta)/s;
      xi =  r*astSind(phi);
      eta = -r*astCosd(phi);
   } else {
      xi = phi;
      eta = theta;
   }

   /* Simple tan */
   if( prj->w[ 0 ] == 0.0 ){
      *xx = xi;
      *yy = eta;

   /* Tan with polynomial corrections: Iterate using Newton's method to
      get the (x,y) corresponding to the above (xi,eta). */
   } else {
      a = prj->p2;
      b = prj->p;

   /* Initial guess: linear solution assuming a3,... and b3,... are zero. */
      denom = a[1]*b[1] - a[2]*b[2];
      if( denom != 0.0 ) {
         x = ( xi*b[1] - eta*a[2] - a[0]*b[1] + b[0]*a[2] )/denom;
         y = -( xi*b[2] - eta*a[1] - a[0]*b[2] + b[0]*a[1] )/denom;

      } else {
         if( a[1] != 0.0 ){
            x = ( xi - a[0] )/a[1];
         } else {
            x = a[0];
         }

         if( b[1] != 0.0 ){
            y = ( eta - b[0] )/b[1];
         } else {
            y = b[0];
         }
      }

/* Iterate up to 50 times, until the required relative accuracy is
   achieved. */
      tol = 1.0E-5;
      ok = 0;
      for (i = 0; i < 50; i++) {

/* Get required products of the current x and y values */
         x2 = x*x;
         xy = x*y;
         y2 = y*y;

         r2 = x2 + y2;
         r = sqrt( r2 );

         x3 = x2*x;
         x2y = x2*y;
         xy2 = x*y2;
         y3 = y*y2;

         r3 = r*r2;

         x4 = x3*x;
         x3y = x3*y;
         x2y2 = x2*y2;
         xy3 = x*y3;
         y4 = y*y3;

         x5 = x4*x;
         x4y = x4*y;
         x3y2 = x3*y2;
         x2y3 = x2*y3;
         xy4 = x*y4;
         y5 = y*y4;

         r5 = r3*r2;

         x6 = x5*x;
         x5y = x5*y;
         x4y2 = x4*y2;
         x3y3 = x3*y3;
         x2y4 = x2*y4;
         xy5 = x*y5;
         y6 = y*y5;

         x7 = x6*x;
         x6y = x6*y;
         x5y2 = x5*y2;
         x4y3 = x4*y3;
         x3y4 = x3*y4;
         x2y5 = x2*y5;
         xy6 = x*y6;
         y7 = y*y6;

         r7 = r5*r2;

/* Get the xi and eta models corresponding to the current x and y values */
         f =   a[0]       + a[1]*x     + a[2]*y     + a[3]*r     + a[4]*x2
              + a[5]*xy    + a[6]*y2    + a[7]*x3    + a[8]*x2y   + a[9]*xy2
              + a[10]*y3   + a[11]*r3   + a[12]*x4   + a[13]*x3y  + a[14]*x2y2
              + a[15]*xy3  + a[16]*y4   + a[17]*x5   + a[18]*x4y  + a[19]*x3y2
              + a[20]*x2y3 + a[21]*xy4  + a[22]*y5   + a[23]*r5   + a[24]*x6
              + a[25]*x5y  + a[26]*x4y2 + a[27]*x3y3 + a[28]*x2y4 + a[29]*xy5
              + a[30]*y6   + a[31]*x7   + a[32]*x6y  + a[33]*x5y2 + a[34]*x4y3
              + a[35]*x3y4 + a[36]*x2y5 + a[37]*xy6  + a[38]*y7   + a[39]*r7;

         g =  b[0]       + b[1]*y     + b[2]*x     + b[3]*r     + b[4]*y2
              + b[5]*xy    + b[6]*x2    + b[7]*y3    + b[8]*xy2   + b[9]*x2y
              + b[10]*x3   + b[11]*r3   + b[12]*y4   + b[13]*xy3  + b[14]*x2y2
              + b[15]*x3y  + b[16]*x4   + b[17]*y5   + b[18]*xy4  + b[19]*x2y3
              + b[20]*x3y2 + b[21]*x4y  + b[22]*x5   + b[23]*r5   + b[24]*y6
              + b[25]*xy5  + b[26]*x2y4 + b[27]*x3y3 + b[28]*x4y2 + b[29]*x5y
              + b[30]*x6   + b[31]*y7   + b[32]*xy6  + b[33]*x2y5 + b[34]*x3y4
              + b[35]*x4y3 + b[36]*x5y2 + b[37]*x6y  + b[38]*x7   + b[39]*r7;

/* Partial derivative of xi wrt x... */
         fx = a[1]         + a[3]*( (r!=0.0)?(x/r):0.0 ) + 2*a[4]*x +
              a[5]*y       + 3*a[7]*x2    + 2*a[8]*xy    + a[9]*y2 +
              3*a[11]*r*x  + 4*a[12]*x3   + 3*a[13]*x2y  + 2*a[14]*xy2  +
	      a[15]*y3     + 5*a[17]*x4   + 4*a[18]*x3y  + 3*a[19]*x2y2 +
	      2*a[20]*xy3  + a[21]*y4     + 5*a[23]*r3*x + 6*a[24]*x5  +
              5*a[25]*x4y  + 4*a[26]*x3y2 + 3*a[27]*x2y3 + 2*a[28]*xy4  +
	      a[29]*y5     + 7*a[31]*x6   + 6*a[32]*x5y  + 5*a[33]*x4y2 +
              4*a[34]*x3y3 + 3*a[35]*x2y4 + 2*a[36]*xy5  + a[37]*y6 +
              7*a[39]*r5*x;

/* Partial derivative of xi wrt y... */
         fy = a[2]         + a[3]*( (r!=0.0)?(y/r):0.0 ) + a[5]*x +
              2*a[6]*y     + a[8]*x2      + 2*a[9]*xy    + 3*a[10]*y2 +
	      3*a[11]*r*y  + a[13]*x3     + 2*a[14]*x2y  + 3*a[15]*xy2 +
	      4*a[16]*y3   + a[18]*x4     + 2*a[19]*x3y  + 3*a[20]*x2y2 +
	      4*a[21]*xy3  + 5*a[22]*y4   + 5*a[23]*r3*y + a[25]*x5 +
	      2*a[26]*x4y  + 3*a[27]*x3y2 + 4*a[28]*x2y3 + 5*a[29]*xy4 +
              6*a[30]*y5   + a[32]*x6     + 2*a[33]*x5y  + 3*a[34]*x4y2 +
              4*a[35]*x3y3 + 5*a[36]*x2y4 + 6*a[37]*xy5  + 7*a[38]*y6 +
              7*a[39]*r5*y;

/* Partial derivative of eta wrt x... */
         gx = b[2]         + b[3]*( (r!=0.0)?(x/r):0.0 ) + b[5]*y +
              2*b[6]*x     + b[8]*y2      + 2*b[9]*xy    + 3*b[10]*x2 +
	      3*b[11]*r*x  + b[13]*y3     + 2*b[14]*xy2  + 3*b[15]*x2y +
	      4*b[16]*x3   + b[18]*y4     + 2*b[19]*xy3  + 3*b[20]*x2y2 +
	      4*b[21]*x3y  + 5*b[22]*x4   + 5*b[23]*r3*x + b[25]*y5 +
	      2*b[26]*xy4  + 3*b[27]*x2y3 + 4*b[28]*x3y2 + 5*b[29]*x4y +
	      6*b[30]*x5   + b[32]*y6     + 2*b[33]*xy5  + 3*b[34]*x2y4 +
	      4*b[35]*x3y3 + 5*b[36]*x4y2 + 6*b[37]*x5y  + 7*b[38]*x6 +
	      7*b[39]*r5*x;

/* Partial derivative of eta wrt y... */
         gy = b[1]         + b[3]*( (r!=0.0)?(y/r):0.0 ) + 2*b[4]*y +
              b[5]*x       + 3*b[7]*y2    + 2*b[8]*xy    + b[9]*x2 +
              3*b[11]*r*y  + 4*b[12]*y3   + 3*b[13]*xy2  + 2*b[14]*x2y  +
	      b[15]*x3     + 5*b[17]*y4   + 4*b[18]*xy3  + 3*b[19]*x2y2 +
	      2*b[20]*x3y  + b[21]*x4     + 5*b[23]*r3*y + 6*b[24]*y5  +
	      5*b[25]*xy4  + 4*b[26]*x2y3 + 3*b[27]*x3y2 + 2*b[28]*x4y  +
	      b[29]*x5     + 7*b[31]*y6   + 6*b[32]*xy5  + 5*b[33]*x2y4 +
	      4*b[34]*x3y3 + 3*b[35]*x4y2 + 2*b[36]*x5y  + b[37]*x6     +
              7*b[39]*r5*y;

/* Calculate new x and y values. */
         f = f - xi;
         g = g - eta;
         dx = ( (-f*gy) + (g*fy) ) / ( (fx*gy) - (fy*gx) );
         dy = ( (-g*fx) + (f*gx) ) / ( (fx*gy) - (fy*gx) );
         x += dx;
         y += dy;

/* Check if convergence has been achieved. */
         if( fabs(dx) <= tol*fabs(x) && fabs(dy) <= tol*fabs(y) ) {
            ok = 1;
            break;
         }
      }

      *xx = x;
      *yy = y;

      if( !ok ) return 2;

   }

   return 0;

}

/*--------------------------------------------------------------------------*/

int astTPNrev(x, y, prj, phi, theta)

const double x, y;
double *phi, *theta;
struct AstPrjPrm *prj;

{
   double r, xi, eta, x2, xy, y2, r2, x3, x2y, xy2, y3, r3, x4, x3y,
          x2y2, xy3, y4, x5, x4y, x3y2, x2y3, xy4, y5, r5, x6, x5y, x4y2,
	  x3y3, x2y4, xy5, y6, x7, x6y, x5y2, x4y3, x3y4, x2y5, xy6, y7,
	  r7;
   double *a, *b;

   if (abs(prj->flag) != TPN ) {
      if (astTPNset(prj)) return 1;
   }

   /* Simple tan */
   if( prj->w[ 0 ] == 0.0 ){
      xi = x;
      eta = y;

   /* Tan with polynomial corrections. */
   } else {
      x2 = x*x;
      xy = x*y;
      y2 = y*y;

      r2 = x2 + y2;
      r = sqrt( r2 );

      x3 = x2*x;
      x2y = x2*y;
      xy2 = x*y2;
      y3 = y*y2;

      r3 = r*r2;

      x4 = x3*x;
      x3y = x3*y;
      x2y2 = x2*y2;
      xy3 = x*y3;
      y4 = y*y3;

      x5 = x4*x;
      x4y = x4*y;
      x3y2 = x3*y2;
      x2y3 = x2*y3;
      xy4 = x*y4;
      y5 = y*y4;

      r5 = r3*r2;

      x6 = x5*x;
      x5y = x5*y;
      x4y2 = x4*y2;
      x3y3 = x3*y3;
      x2y4 = x2*y4;
      xy5 = x*y5;
      y6 = y*y5;

      x7 = x6*x;
      x6y = x6*y;
      x5y2 = x5*y2;
      x4y3 = x4*y3;
      x3y4 = x3*y4;
      x2y5 = x2*y5;
      xy6 = x*y6;
      y7 = y*y6;

      r7 = r5*r2;

      a = prj->p2;
      xi =   a[0]       + a[1]*x     + a[2]*y     + a[3]*r     + a[4]*x2
           + a[5]*xy    + a[6]*y2    + a[7]*x3    + a[8]*x2y   + a[9]*xy2
           + a[10]*y3   + a[11]*r3   + a[12]*x4   + a[13]*x3y  + a[14]*x2y2
           + a[15]*xy3  + a[16]*y4   + a[17]*x5   + a[18]*x4y  + a[19]*x3y2
           + a[20]*x2y3 + a[21]*xy4  + a[22]*y5   + a[23]*r5   + a[24]*x6
           + a[25]*x5y  + a[26]*x4y2 + a[27]*x3y3 + a[28]*x2y4 + a[29]*xy5
           + a[30]*y6   + a[31]*x7   + a[32]*x6y  + a[33]*x5y2 + a[34]*x4y3
           + a[35]*x3y4 + a[36]*x2y5 + a[37]*xy6  + a[38]*y7   + a[39]*r7;

      b = prj->p;
      eta =  b[0]       + b[1]*y     + b[2]*x     + b[3]*r     + b[4]*y2
           + b[5]*xy    + b[6]*x2    + b[7]*y3    + b[8]*xy2   + b[9]*x2y
           + b[10]*x3   + b[11]*r3   + b[12]*y4   + b[13]*xy3  + b[14]*x2y2
           + b[15]*x3y  + b[16]*x4   + b[17]*y5   + b[18]*xy4  + b[19]*x2y3
           + b[20]*x3y2 + b[21]*x4y  + b[22]*x5   + b[23]*r5   + b[24]*y6
           + b[25]*xy5  + b[26]*x2y4 + b[27]*x3y3 + b[28]*x4y2 + b[29]*x5y
           + b[30]*x6   + b[31]*y7   + b[32]*xy6  + b[33]*x2y5 + b[34]*x3y4
           + b[35]*x4y3 + b[36]*x5y2 + b[37]*x6y  + b[38]*x7   + b[39]*r7;

   }

   /* Now do the tan projection */
   if( prj->n ) {
      r = sqrt(xi*xi + eta*eta);
      if (r == 0.0) {
         *phi = 0.0;
      } else {
         *phi = astATan2d(xi, -eta);
      }
      *theta = astATan2d(prj->r0, r);
   } else {
      *phi = xi;
      *theta = eta;
   }

   return 0;
}

