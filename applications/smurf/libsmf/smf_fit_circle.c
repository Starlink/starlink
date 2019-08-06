/*
*+
*  Name:
*     smf_fit_circle

*  Purpose:
*     Fit a circle to a set of points in a flat two-dimensional space.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_fit_circle( size_t n, double *ax, double *ay, double *wx,
*                     double *wy, double *rad, double *xc, double *yc,
*                     double *rms, int *status )

*  Arguments:
*     n = size_t (Given)
*        The number of points supplied in arrays ax and ay.
*     ax = double * (Given)
*        Pointer to an array of "n" X values.
*     ay = double * (Given)
*        Pointer to an array of "n" Y values.
*     wx = double * (Given)
*        Pointer to an array of "n" X weight values. May be NULL.
*     wy = double * (Given)
*        Pointer to an array of "n" Y weight values. May be NULL.
*     rad = double * (Returned)
*        Returned holding the radius of the fitted circle. This will be
*        VAL__BADD if the fitted radius was too small (compared to the
*        returned RMS level) to be de-biased.
*     xc = double * (Returned)
*        Returned holding the X value at the centre of the fitted circle.
*     yc = double * (Returned)
*        Returned holding the Y value at the centre of the fitted circle.
*     rms = double * (Returned)
*        Returned holding the RMS of the fit.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine fits a circle to the supplied (x,y) points. The
*     returned circle parameters (radius and centre) minimise f, the sum
*     of the squared residuals between the fitted circle and the supplied
*     points:
*
*     f = sum( ( radius - sqrt( (x_i - x_centre)**2 + (y_i - y_centre)**2 ) )**2 )
*
*     The intial guess is calculated using an algebraic fit, due to Taubin,
*     based on the journal article:
*
*      G. Taubin, "Estimation Of Planar Curves, Surfaces And Nonplanar
*                  Space Curves Defined By Implicit Equations, With
*                  Applications To Edge And Range Image Segmentation",
*                  IEEE Trans. PAMI, Vol. 13, pages 1115-1138, (1991)
*
*    The returned radius is de-biased to correct for the tendency of the
*    above fitting process to over-estimate the radius if the radius is
*    comparable to, or smaller than, the noise.

*  Authors:
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     30-MAY-2019 (DSB):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory.
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <math.h>
#include "gsl/gsl_multimin.h"

/* Starlink includes */
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "smf.h"

#define IterMAX 99

static void smf1_init_guess( size_t n, double *ax, double *ay, double *wx,
                             double *wy, double *rad, double *xc, double *yc,
                             int *status );
static double smf1_f( const gsl_vector *v, void *pars );
static void smf1_df( const gsl_vector *v, void *params, gsl_vector *df );
static void smf1_fdf( const gsl_vector *v, void *params, double *f, gsl_vector *df );
static double smf1_debias( double rad, double rms );

typedef struct Params {
   double *ax;    /* Array of X values */
   double *ay;    /* Array of Y values */
   double *w;     /* Array of weights */
   double eps;    /* Minimum significant distance */
   double sw;     /* Sum of the weights */
   size_t n;      /* Length of above arrays */
} Params;

/* Length of the following tables */
#define NDEB 29

/* A table of logarithmically spaced ratios of measured radius to measure
   RMS radial residual. */
static double Snr[NDEB] = { 1.99521, 2.02528, 2.08149, 2.15445, 2.24306,
			    2.35422, 2.5053, 2.68862, 2.91903, 3.17717,
			    3.51337, 3.87368, 4.31587, 4.79185, 5.32693,
			    5.98955, 6.68967, 7.532, 8.44527, 9.46276,
			    10.6575, 11.9244, 13.4047, 15.0637, 16.9481,
			    19.0227, 21.4103, 24.0599, 27.1597 };


/* A table of bias factors - the ratio of the measured radius to the true
   radius - at the SNR value held in the corresponding element of the "snr"
   table. */
static double Gain[NDEB] = { 1.54923, 1.44032, 1.35224, 1.27921, 1.22039,
			     1.16938, 1.12938, 1.10235, 1.08208, 1.06209,
			     1.04928, 1.03977, 1.02993, 1.02469, 1.0185,
			     1.01475, 1.01172, 1.00899, 1.00725, 1.00543,
			     1.00463, 1.0042, 1.00277, 1.00239, 1.00178,
			     1.00136, 1.00108, 1.00084, 1.00076 };



void smf_fit_circle( size_t n, double *ax, double *ay, double *wx, double *wy,
                     double *rad, double *xc, double *yc, double *rms, int *status ){

/* Local Variables: */
   Params params;
   double *pwgt;
   double *pwx;
   double *pwy;
   double *px;
   double *py;
   double *wgt;
   double init_rad;
   double init_xc;
   double init_yc;
   double sw;
   gsl_multimin_function_fdf my_func;
   gsl_multimin_fdfminimizer *s;
   gsl_vector *x;
   int gsl_status;
   int iter;
   size_t i;

/* Initialise returned values */
   *rad = VAL__BADD;
   *xc = VAL__BADD;
   *yc = VAL__BADD;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get an initial guess at the centre and radius of the circle, using an
   algebraic minimisation. */
   smf1_init_guess( n, ax, ay, wx, wy, &init_rad, &init_xc, &init_yc, status );

/* Allocate room for the weight for each point. */
   wgt = astMalloc( n*sizeof(*wgt) );

/* Check we have an initial guess. */
   if( init_rad != VAL__BADD && *status == SAI__OK ) {

/* Calculate the weight for each point (the geometric mean of the X and Y
   weights), and the sum of the weights. */
      sw = 0.0;
      px = ax;
      py = ay;
      pwgt = wgt;
      if( wx && wy ) {
         pwx = wx;
         pwy = wy;
         for( i = 0; i < n; i++,px++,py++,pwx++,pwy++,pwgt++ ) {
            if( *px != VAL__BADD && *py != VAL__BADD &&
                *pwx != VAL__BADD && *pwy != VAL__BADD ) {
               *pwgt = sqrt( (*pwx)*(*pwy) );
               sw += *pwgt;
            } else {
               *pwgt = 0.0;
            }
         }
      } else {
         for( i = 0; i < n; i++,px++,py++,pwgt++ ) {
            if( *px != VAL__BADD && *py != VAL__BADD ) {
               *pwgt = 1.0;
               sw += *pwgt;
            } else {
               *pwgt = 0.0;
            }
         }
      }

/* Store the starting point. */
      x = gsl_vector_alloc( 3 );
      gsl_vector_set( x, 0, init_rad );
      gsl_vector_set( x, 1, init_xc );
      gsl_vector_set( x, 2, init_yc );

/* Store details of the service routines that calculate the function to
   be minimised and its derivatives. */
      my_func.n = 3;
      my_func.f = &smf1_f;
      my_func.df = &smf1_df;
      my_func.fdf = &smf1_fdf;
      my_func.params = (void *) &params;

/* Store information to be passed to the above service routines. */
      params.ax = ax;
      params.ay = ay;
      params.w = wgt;
      params.n = n;
      params.sw = sw;
      params.eps = init_rad*1.0E-10;

/* Create a 3D minimiser. */
      s = gsl_multimin_fdfminimizer_alloc( gsl_multimin_fdfminimizer_conjugate_fr,
                                           3 );

/* Store the service routines, step size and tolerance in the minimiser. */
      gsl_multimin_fdfminimizer_set( s, &my_func, x, init_rad, 0.1 );

/* Iterate to a solution. */
      iter = 0;
      gsl_status = GSL_CONTINUE;
      while( gsl_status == GSL_CONTINUE && ++iter < 100 ){
         gsl_status = gsl_multimin_fdfminimizer_iterate( s );
         if( gsl_status ) break;
         gsl_status = gsl_multimin_test_gradient( s->gradient, 1e-4 );
      }

/* Return the circle parameters. */
      *rad = gsl_vector_get( s->x, 0 );
      *xc = gsl_vector_get( s->x, 1 );
      *yc = gsl_vector_get( s->x, 2 );
      *rms = sqrt( s->f );

/* De-bias the radius. */
      *rad = smf1_debias( *rad, *rms );

/* Clean up. */
      gsl_multimin_fdfminimizer_free( s );
      gsl_vector_free( x );

   }
   wgt = astFree( wgt );
}





static void smf1_init_guess( size_t n, double *ax, double *ay, double *wx,
                             double *wy, double *rad, double *xc, double *yc,
                             int *status ){
/*
*     Get an initial guess at the centre and radius of the cirle. This is
*     an algebraic minimisation, due to Taubin, based on the journal article:
*
*      G. Taubin, "Estimation Of Planar Curves, Surfaces And Nonplanar
*                  Space Curves Defined By Implicit Equations, With
*                  Applications To Edge And Range Image Segmentation",
*                  IEEE Trans. PAMI, Vol. 13, pages 1115-1138, (1991)
*/

/* Local Variables: */
   double *pwy;
   double *pwx;
   double *px;
   double *py;
   double A0;
   double A1;
   double A2;
   double A22;
   double A33;
   double A3;
   double Cov_xy;
   double Dy;
   double Mxx;
   double Mxy;
   double Mxz;
   double Myy;
   double Myz;
   double Mz;
   double Mzz;
   double Var_z;
   double Wxx;
   double Wxy;
   double Wxz;
   double Wyy;
   double Wyz;
   double Wzz;
   double Xi;
   double Yi;
   double Zi;
   double det;
   double swy;
   double swx;
   double sx;
   double sy;
   double x;
   double xmean;
   double xnew;
   double xoff;
   double xwgt;
   double y;
   double ymean;
   double ynew;
   double yoff;
   double ywgt;
   double zwgt;
   int iter;
   size_t i;

/* Initialise returned values */
   *rad = VAL__BADD;
   *xc = VAL__BADD;
   *yc = VAL__BADD;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Compute weighted mean of remaining X and Y. */
   sx = 0.0;
   sy = 0.0;
   swx = 0.0;
   swy = 0.0;
   px = ax;
   py = ay;
   pwx = wx;
   pwy = wy;

   for( i = 0; i < n; i++,px++,py++,pwx++,pwy++ ) {
      if( *px != VAL__BADD &&
          *py != VAL__BADD ){
         xwgt = wx ? *pwx : 1.0;
         ywgt = wy ? *pwy : 1.0;
         sx += xwgt*(*px);
         sy += ywgt*(*py);
         swx += xwgt;
         swy += ywgt;
      }
   }

   if( swx != 0.0 && swy != 0.0 ) {
      xmean = sx/swx;
      ymean = sy/swy;

/* Compute the required moments. */
      Mxx = Myy = Mxy = Mxz = Myz = Mzz = 0.0;
      Wxy = Wxx = Wyy = Wxz = Wyz = Wzz = 0.0;
      px = ax;
      py = ay;
      pwx = wx;
      pwy = wy;
      for( i = 0; i < n; i++,px++,py++,pwx++,pwy++ ) {
         if( *px != VAL__BADD &&
             *py != VAL__BADD ){
            xwgt = wx ? *pwx : 1.0;
            ywgt = wy ? *pwy : 1.0;

            Xi = *px - xmean;
            Yi = *py - ymean;
            Zi = Xi*Xi + Yi*Yi;
            zwgt = xwgt*xwgt + ywgt*ywgt;

            Xi *= xwgt;
            Yi *= ywgt;
            Zi *= zwgt;

            Mxy += Xi*Yi;
            Mxx += Xi*Xi;
            Myy += Yi*Yi;
            Mxz += Xi*Zi;
            Myz += Yi*Zi;
            Mzz += Zi*Zi;

            Wxy += xwgt*ywgt;
            Wxx += xwgt*xwgt;
            Wyy += ywgt*ywgt;
            Wxz += xwgt*zwgt;
            Wyz += ywgt*zwgt;
            Wzz += zwgt*zwgt;
         }
      }
      Mxx /= Wxx;
      Myy /= Wyy;
      Mxy /= Wxy;
      Mxz /= Wxz;
      Myz /= Wyz;
      Mzz /= Wzz;

/* Compute the coefficients of the characteristic polynomial. */
      Mz = Mxx + Myy;
      Cov_xy = Mxx*Myy - Mxy*Mxy;
      Var_z = Mzz - Mz*Mz;
      A3 = 4.0*Mz;
      A2 = -3.0*Mz*Mz - Mzz;
      A1 = Var_z*Mz + 4.0*Cov_xy*Mz - Mxz*Mxz - Myz*Myz;
      A0 = Mxz*(Mxz*Myy - Myz*Mxy) + Myz*(Myz*Mxx - Mxz*Mxy) - Var_z*Cov_xy;
      A22 = A2 + A2;
      A33 = A3 + A3 + A3;

/*  Find the root of the characteristic polynomial using Newton's method
    starting at x=0 (it is guaranteed to converge to the right root).
    Usually, 4-6 iterations are enough. */
      x = 0.0;
      y = A0;
      for( iter = 0; iter < IterMAX; iter++ ){
         Dy = A1 + x*(A22 + A33*x);
         xnew = x - y/Dy;
         if( xnew == x || !isfinite(xnew) ) break;
         ynew = A0 + xnew*(A1 + xnew*(A2 + xnew*A3));
         if( abs(ynew) >= abs(y) )  break;
         x = xnew;
         y = ynew;
      }

/* Compute parameters of the fitting circle. */
      det = x*x - x*Mz + Cov_xy;
      xoff = 0.5*(Mxz*(Myy - x) - Myz*Mxy)/det;
      yoff = 0.5*(Myz*(Mxx - x) - Mxz*Mxy)/det;
      *xc = xoff + xmean;
      *yc = yoff + ymean;
      *rad = sqrt( xoff*xoff + yoff*yoff + Mz);
   }
}




static double smf1_f( const gsl_vector *v, void *pars ) {

/*
*  Service routine that returns the value of the function to be minimised,
*  assuming the circle centre and radius stored in "v".
*/

   double rad, xc, yc, f, dx, dy, resid;
   double *px, *py, *pw;
   size_t n, i;
   Params *params = (Params *) pars;

   n = params->n;
   px = params->ax;
   py = params->ay;
   pw = params->w;

   rad = gsl_vector_get( v, 0 );
   xc = gsl_vector_get( v, 1 );
   yc = gsl_vector_get( v, 2 );

   f = 0.0;

   for( i = 0; i < n; i++,px++,py++,pw++) {
      if( *pw > 0.0 ) {
         dx = *px - xc;
         dy = *py - yc;
         resid = rad - sqrt( dx*dx + dy*dy );
         f += (*pw)*resid*resid;
      }
   }

   f /= params->sw;

   return f;
}


static void smf1_df( const gsl_vector *v, void *pars, gsl_vector *df ){
/*
*  Service routine that returns the returns the gradient of the function
*  to be minimised (df/drad, df/dxc, df/dyc), assuming the circle centre
*  and radius stored in "v".
*/

   double rad, xc, yc, dx, dy, resid, wres, s1, s2, s3, this_rad;
   double *px, *py, *pw;
   size_t n, i;
   Params *params = (Params  *) pars;

   n = params->n;
   px = params->ax;
   py = params->ay;
   pw = params->w;

   rad = gsl_vector_get( v, 0 );
   xc = gsl_vector_get( v, 1 );
   yc = gsl_vector_get( v, 2 );

   s1 = 0.0;
   s2 = 0.0;
   s3 = 0.0;

   for( i = 0; i < n; i++,px++,py++,pw++) {
      if( *pw > 0.0 ) {
         dx = *px - xc;
         dy = *py - yc;
         this_rad = sqrt( dx*dx + dy*dy );
         resid = rad - this_rad;
         wres = resid*(*pw);
         s1 += wres;
         if( this_rad  > params->eps ) {
            s2 += wres*dx/this_rad;
            s3 += wres*dy/this_rad;
         } else {
            s2 += wres;
            s3 += wres;
         }
      }
   }

   s1 *= 2/params->sw;
   s2 *= 2/params->sw;
   s3 *= 2/params->sw;

   gsl_vector_set( df, 0, s1 );
   gsl_vector_set( df, 1, s2 );
   gsl_vector_set( df, 2, s3 );
}




static void smf1_fdf( const gsl_vector *v, void *pars, double *f, gsl_vector *df ){
/*
*   Service routine that returns the value and gradient of the function to be
*   minimised (f, df/drad, df/dxc, df/dyc ), assuming the circle centre and
*   radius stored in "v".
*/

   double rad, wres, xc, yc, dx, dy, resid, s1, s2, s3, this_rad;
   double *px, *py, *pw;
   size_t n, i;
   Params *params = (Params *) pars;

   n = params->n;
   px = params->ax;
   py = params->ay;
   pw = params->w;

   rad = gsl_vector_get( v, 0 );
   xc = gsl_vector_get( v, 1 );
   yc = gsl_vector_get( v, 2 );

   *f = 0.0;
   s1 = 0.0;
   s2 = 0.0;
   s3 = 0.0;

   for( i = 0; i < n; i++,px++,py++,pw++) {
      if( *pw > 0.0 ) {
         dx = *px - xc;
         dy = *py - yc;
         this_rad = sqrt( dx*dx + dy*dy );
         resid = rad - this_rad;
         wres = resid*(*pw);
         *f += wres*resid;
         s1 += wres;
         if( this_rad  > params->eps ) {
            s2 += wres*dx/this_rad;
            s3 += wres*dy/this_rad;
         } else {
            s2 += wres;
            s3 += wres;
         }
      }
   }

   *f /= params->sw;
   s1 *= 2/params->sw;
   s2 *= 2/params->sw;
   s3 *= 2/params->sw;

   gsl_vector_set( df, 0, s1 );
   gsl_vector_set( df, 1, s2 );
   gsl_vector_set( df, 2, s3 );
}

static double smf1_debias( double rad, double rms ){
/*
*   Returns the de-biased radius given the original radius and the RMS
*   radial residual. This is based on a table holding the results from an
*   empirical test based on fitting circles to test points with known
*   Gaussian noise over a range of signal-to-noise ratios.
*/
   double f;
   double g;
   double snr;
   int i;

/* Get the measured SNR - the ratio of the measured radius to the
   measured RMS radial residual. */
   snr = rad/rms;

/* If the SNR is too low, we cannot debias it. */
   if( snr <= Snr[ 0 ] ) {
      rad = VAL__BADD;

/* If the SNR is very high, return the radius unchanged. */
   } else if( snr >= Snr[ NDEB - 1 ] ) {
      rad = rad;

/* Otherwise, find the corresponding radius gain using linear
   interpolation  in the debias tables - Snr and Gain. */
   } else {

/* Find the first Snr table value that is greater than the snr. */
      for( i = 1; i < NDEB; i++ ) {
        if( Snr[ i ] > snr ) break;
      }

/* Get the distance of the snr value from the previous table entry, as
   a fraction of the distance from the previous table entry to the next
   table entry. */
      f = ( snr - Snr[ i - 1 ] )/( Snr[ i ] - Snr[ i - 1 ] );

/* Get the corresponding value in the Gain array. */
      g = ( Gain[ i ] - Gain[ i - 1 ] )*f + Gain[ i - 1 ];

/* Calculated the de-biased radius. */
      rad /= g;
   }

/* Return the debiased radius. */
   return rad;
}
