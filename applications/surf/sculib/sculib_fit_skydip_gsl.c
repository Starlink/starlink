/*
*+
*  Name:
*     sculib_fits_skydip_gsl
*
*  Purpose:
*     Fit skydip data using GSL LM algorithm
*
*  Language:
*     Starlink ANSI C (to be called from Fortran)
*
*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL SCULIB_FIT_SKYDIP_GSL(CVAR, N_MEASUREMENTS, AIRMASS, J_MEASURED,
*    :  J_VARIANCE, SUB_WAVELENGTH, SUB_INSTRUMENT, SUB_FILTER, T_TEL,
*    :  T_AMB, ETA_TEL_IN, B_IN, ETA_TEL_FIT, B_FIT, TAUZ_FIT,
*    :  REXISQ, TAU_ERROR, ETA_ERROR, B_ERROR, RESIDUAL, SIGMA, STATUS)

*  Description:
*     Designed to be a drop in replacement for the SCULIB_FIT_SKYDIP
*     routine but using the standard GSL Levenberg-Marquardt fitting algorithm.
*     See SCULIB_FIT_SKYDIP for details of the function that is being fitted.
*     This subroutine is designed to be API compatible with SCULIB_FIT_SKYDIP.

*  Arguments:
*     CVAR                      = LOGICAL (Given)
*              flag to govern whether to use a fixed variance (true)
*              or the actual variance. The fixed variance is the mean
*              of the actual variances.
*     N_MEASUREMENTS            = INTEGER (Given)
*              the number of SKYDIP measurements
*     AIRMASS (N_MEASUREMENTS)  = REAL (Given)
*              the airmasses at which the measurements were made
*     DATA (N_MEASUREMENTS)     = REAL (Given)
*              the measured sky brightness temperatures
*     VARIANCE (N_MEASUREMENTS) = REAL (Given)
*              the variance on DATA
*     SUB_WAVELENGTH            = REAL (Given)
*              the wavelength of the measurements in microns
*     SUB_INSTRUMENT            = CHARACTER*(*) (Given)
*              the name of the sub-instrument used
*     SUB_FILTER                = CHARACTER*(*) (Given)
*              the name of the filter used
*     T_TEL                     = REAL (Given)
*              the telescope temperature (K)
*     T_AMB                     = REAL (Given)
*              the ambient temperature (K)
*     ETA_TEL_IN                = REAL (Given)
*              if >= 0 then this will the ETAtel assumed in the fit.
*              if < 0 then ETAtel will be allowed to vary in the fit.
*     B_IN                      = REAL (Given)
*              if >=0 then this value of b will be assumed in the fit.
*              if < 0 then b will be allowed to vary in the fit.
*     ETA_TEL_FIT               = REAL (Returned)
*              the result for ETAtel
*     B_FIT                     = REAL (Returned)
*              the result for b
*     TAUZ_FIT                  = REAL (Returned)
*              the fitted result for tauz
*     REXISQ                    = REAL (Returned)
*              the reduced chi square of the fit
*     TAU_ERROR                 = REAL (Returned)
*              error in the tau
*     ETA_ERROR                 = REAL (Returned)
*              error in eta_tel
*     B_ERROR                   = REAL (Returned)
*              error in B
*     RESIDUAL                  = DOUBLE (Returned)
*              Absolute difference between the model and the fit.
*     SIGMA                     = DOUBLE (Returned)
*              standard deviation of the difference between the
*              model and the fit.
*     STATUS                    = INTEGER (Given and returned)
*              Global status

*  History:
*     2011-10-26 (TIMJ):
*        First version in C using GSL.

*  Implementation Status:
*     All support routines are included in a single source file
*     and declared static to prevent the API leaking. For many
*     of these routines there are Fortran equivalents that were
*     written for the original NAG fitting routine that was
*     replaced when NAG was removed. See sculib_skyfunc,
*     sculib_skyfunc_1 and sculib_skyfunc_2. C routines were
*     written so that the support struct could be used whereas
*     the Fortran code relied on COMMON.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*-
*/

#include "sae_par.h"
#include "prm_par.h"
#include "ast.h"
#include "mers.h"

#include <stdlib.h>
#include <stdio.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_multifit_nlin.h>

/* Fudge constant */
#define X_GCONST 3.669383
/* Temperature drop / km */
#define H1 -6.5
/* Scale height of absorber */
#define H2 2.0

/* Definition of X_G */
#define X_G(J_AMB, TAU, AIRMASS) 1.0 + ( H1 * H2 / J_AMB ) * \
    exp( -TAU * AIRMASS / X_GCONST )

/* Minimum acceptable error for a temperature reading */
#define MINERR 1.0E-2

/* Data structure to pass into the fitting routine */
struct data {
  size_t n;
  size_t p;
  double * airmass;
  double * j_sky;
  double * j_sky_sigma;
  double j_tel;
  double j_amb;
  double eta_l_fixed;  /* Fixed value of eta_l if fixed */
  double b_fixed;      /* Fixed value of b if fixed */
  int eta_l_is_fixed;  /* indicates that eta_l is fixed */
  int b_is_fixed;      /* indicates b is fixed */
};

/* This function is self contained so that we include the routines
   to calculate the Skydip function and jacobian in here directly.
   This simplifies the calling of the routine when mixing C and Fortran
   in SURF. Note that the E04UPF support routines do exist in sculib
   and are the models on which the C code is based. */

/* Calculate Rayleigh-Jeans corrected brightness temperature
   C implementation of SCULIB_JNU
 */

static double
sculib__jnu( double nu, double  t, int *status ) {
  double h = 6.6252e-27;     /* Planck */
  double k = 1.38046e-16;    /* Boltzmann */
  double jnu = VAL__BADD;
  double kt;
  double x;

  if (*status != SAI__OK) return jnu;

  /* denominator */
  kt = k * t;

  /* if denominator is almost zero then jnu is 0 */
  if (kt < 1e-32 ) return 0.0;

  /* Exponent */
  x = ( h * nu ) / kt;

  if ( fabs(x) < 1.0e-4) {
    jnu = t;
  } else if ( fabs(x) > 20.0 ) {
    jnu = 0.0;
  } else {
    jnu = x * t / ( exp(x) - 1.0 );
  }
  return jnu;
}

/* Helper routine to extract the current etal, b and tau
   values from the two fit structures. covar can be null
   if we are not interested in error information. */
static void
sculib__get_fit_values( const gsl_vector *x, void * data,
                        const gsl_matrix *covar,
                        double *eta_l, double * b, double *tau,
                        double *eta_l_err, double * b_err,
                        double * tau_err
                        ) {
  struct data * d = (struct data *)data;
  int i = 0;

  if (d->eta_l_is_fixed) {
    *eta_l = d->eta_l_fixed;
    if (covar) *eta_l_err = 0.0;
  } else {
    *eta_l = gsl_vector_get( x, i );
    if (covar) *eta_l_err = sqrt( gsl_matrix_get( covar, i, i ));
    i++;
  }
  if (d->b_is_fixed) {
    *b = d->b_fixed;
    if (covar) *b_err = 0.0;
  } else {
    *b = gsl_vector_get( x, i );
    if (covar) *b_err = sqrt( gsl_matrix_get( covar, i, i ));
    i++;
  }

  /* tau is always fitted */
  *tau = gsl_vector_get( x, i );
  if (covar) *tau_err = sqrt( gsl_matrix_get( covar, i, i ));

}


/* Skydip function calculator */
static double
sculib__skydip_func( double airmass, double j_tel, double j_amb, double eta_l,
         double b, double tau ) {

  double j_theory;
  double j_atm;

  if (tau > 0.0 && b > 0.0 && tau*airmass < 20.0) {
    double x_g = X_G( j_amb, tau, airmass);

    j_atm = j_amb * x_g;

    //    printf("%zu j_atm = %g x_g = %g\n",i, j_atm, x_g);

    j_theory = (1.0 - eta_l) * j_tel +
      ( eta_l * j_atm * ( 1.0 - b * exp(-tau * airmass )));

  } else {
    j_atm = j_amb;

    j_theory = (1.0 - eta_l) * j_tel +
      ( eta_l * j_atm );

  }

  return j_theory;
}

/* Function to be fitted is the function - the measured data divided
   by the error in the measured data. */

static int
sculib__skydip_f (const gsl_vector * x, void *data,
        gsl_vector * f)
{
  struct data * d = (struct data *) data;
  size_t n = d->n;
  double *airmass = d->airmass;
  double *j_sky = d->j_sky;
  double *j_sky_sigma = d->j_sky_sigma;
  double j_tel = d->j_tel;
  double j_amb = d->j_amb;

  size_t i;
  double eta_l;
  double b;
  double tau;
  int status = SAI__OK; /* Assume everything is ok inside minimizer */

  const double B_MIN = 0.25;
  const double B_MAX = 1.0;
  const double ETA_MIN = 0.25;
  const double ETA_MAX = 1.0;
  const double TAU_MIN = 0.001;
  const double TAU_MAX = 5.0;

  sculib__get_fit_values( x, data, NULL,
                          &eta_l, &b, &tau,
                          NULL, NULL, NULL );

  /* If any of the parameters are out of range we return a huge number */
  if ( b < B_MIN || b > B_MAX || eta_l < ETA_MIN || eta_l > ETA_MAX ||
       tau < TAU_MIN) {
    for (i=0; i<n; i++) {
      gsl_vector_set (f, i, VAL__BADD);
    }
    return GSL_SUCCESS;
  }

  for (i = 0; i < n; i++)
    {
      double j_theory;
      double sigma;

      j_theory = sculib__skydip_func( airmass[i], j_tel, j_amb, eta_l, b, tau );

      sigma = ( j_sky_sigma[i] > MINERR ? j_sky_sigma[i] : MINERR );

      msgOutiff(MSG__DEBUG, "",
                "iter %zu  theory %g - data %g = %g", &status,
                i, j_theory,j_sky[i], j_theory-j_sky[i]);
      gsl_vector_set (f, i, (j_theory - j_sky[i])/sigma);
    }

  return GSL_SUCCESS;
}

/* Calculate the jacobian */

static int
sculib__skydip_df (const gsl_vector * x, void *data,
         gsl_matrix * J)
{
  struct data * d = (struct data *) data;
  size_t n = d->n;
  double *airmass = d->airmass;
  double *j_sky_sigma = d->j_sky_sigma;
  double j_tel = d->j_tel;
  double j_amb = d->j_amb;

  size_t i;
  double eta_l;
  double b;
  double tau;

  sculib__get_fit_values( x, data, NULL,
                          &eta_l, &b, &tau,
                          NULL, NULL, NULL );

  for (i = 0; i < n; i++)
    {
      /* Jacobian */
      double fjac1 = 0.0;
      double fjac2 = 0.0;
      double fjac3 = 0.0;
      int k = 0;
      double j_atm;
      double sigma = ( j_sky_sigma[i] > MINERR ? j_sky_sigma[i] : MINERR );

      if (tau > 0.0 && b > 0.0 && tau*airmass[i] < 20.0) {
        double dj_atmdt;
        double dx_dgt;
        double x_g = X_G( j_amb, tau, airmass[i]);

        j_atm = j_amb * x_g;

        fjac1 = ( - j_tel + j_atm -
                  b * j_atm * exp( -tau * airmass[i])) / sigma;
        fjac2 = ( - eta_l * j_atm * exp( -tau * airmass[i] )) / sigma;

        dx_dgt = ((-H1 * H2 * airmass[i] ) / ( j_amb * X_GCONST)) *
          exp( -tau * airmass[i] / X_GCONST );
        dj_atmdt = j_amb * dx_dgt;

        fjac3 = ( eta_l * dj_atmdt - b * eta_l *
                  exp(-tau * airmass[i]) * (dj_atmdt - j_atm * airmass[i])) /
          sigma;

      } else {
        j_atm = j_amb;
        fjac1 = (- j_tel + j_atm ) / sigma;
        fjac2 = 0.0;
        fjac3 = 0.0;
      }

      /* Now assign to the relevant matrix position
         depending on what we are actually fitting.
         We have calculated values for all three possible
         parameters. */
      k = 0;
      if (! d->eta_l_is_fixed ) {
        gsl_matrix_set( J, i, k, fjac1 );
        k++;
      }
      if (! d->b_is_fixed ) {
        gsl_matrix_set( J, i, k, fjac2 );
        k++;
      }
      gsl_matrix_set( J, i, k, fjac3 );

    }

  return GSL_SUCCESS;
}

static int
sculib__skydip_fdf (const gsl_vector * x, void *data,
          gsl_vector * f, gsl_matrix * J)
{
  sculib__skydip_f (x, data, f);
  sculib__skydip_df (x, data, J);

  return GSL_SUCCESS;
}

/* Routine to print iteration status */

static void
sculib__print_state( size_t iter, int p, gsl_multifit_fdfsolver * s,
                     int * status ) {
  if (*status != SAI__OK) return;

  if (p == 1) {
    msgOutiff( MSG__VERB, "",
               "iter: %3zu x = %11.8f |f(x)| = %g",
               status,
               iter,
               gsl_vector_get( s->x, 0 ),
               gsl_blas_dnrm2( s->f )
               );
  } else if (p == 2) {
    msgOutiff( MSG__VERB, "",
               "iter: %3zu x = %11.8f %11.8f |f(x)| = %g",
               status,
               iter,
               gsl_vector_get( s->x, 0 ),
               gsl_vector_get( s->x, 1 ),
               gsl_blas_dnrm2( s->f )
               );
  } else {
    msgOutiff( MSG__VERB, "",
               "iter: %3zu x = %11.8f %11.8f %11.8f |f(x)| = %g",
               status,
               iter,
               gsl_vector_get( s->x, 0 ),
               gsl_vector_get( s->x, 1 ),
               gsl_vector_get( s->x, 2 ),
               gsl_blas_dnrm2( s->f )
               );
  }
}


/* C API - keep it private for now */

static void
sculib_fit_skydip_gsl( int cvar, int n_measurements, const float airmass[],
                       const float data[], const float variance[],
                       float sub_wavelength, const char sub_instrument[],
                       const char sub_filter[], float t_tel, float t_amb,
                       float eta_l_in, float b_in,
                       float *eta_l_fit, float *b_fit, float * tauz_fit,
                       float *rexisq, float *tau_error, float * eta_error,
                       float *b_error, double *residual, double * sigma,
                       int * status ) {

  const double C_LIGHT = 299792458.0;   /* Speed of light */
  const double STARTTAU = 0.5;          /* Initial guess of tau */

  double cfac;        /* Correction factor for errors if xisq is too high */
  double chi = 0.0;   /* Chisq */
  struct data d;      /* Data to pass into fitting routine */
  double *dairmass;   /* Double precision version of input airmass */
  double *ddata;      /* Double precision version of input data */
  double *derror;     /* Double precision error from input variance */
  int dof;            /* Number of degrees of freedom */
  size_t i;           /* Counter */
  size_t ii;          /* Counter */
  size_t iter = 0;    /* Number of iterations */
  double nu;          /* Frequency of observation */
  size_t p;           /* Number of free parameters in fit */
  double sum;         /* Sum for mean variance calculation if cvar */
  double x_init[3];   /* Initial guess for fit. Will be eta,b,tau, or eta,tau or b,tau or tau */

  /* Local versions of fit results */
  double eta_l_final;
  double eta_l_err_final = 0.0;
  double b_final;
  double b_err_final = 0.0;
  double tau_final;
  double tau_err_final = 0.0;
  double dresidual = 0.0;
  double dsigma = VAL__BADD;
  double drexisq;

  /* GSL items */
  const gsl_multifit_fdfsolver_type *T = NULL;
  gsl_multifit_fdfsolver *s = NULL;
  gsl_matrix *covar = NULL;
  gsl_multifit_function_fdf f;
  gsl_vector_view x;
  const gsl_rng_type *type = NULL;
  gsl_rng *r = NULL;
  int gslstatus = 0;   /* Status from GSL commands */

  if (*status != SAI__OK) return;

  /* Calculate J_TEL and J_AMB */
  nu = C_LIGHT / ( sub_wavelength * 1.0E-6 );
  d.j_tel = sculib__jnu( nu, t_tel, status );
  d.j_amb = sculib__jnu( nu, t_amb, status );
  if (*status != SAI__OK) return;

  if (d.j_tel == VAL__BADD || d.j_amb == VAL__BADD) {
    *status = SAI__ERROR;
    errRep("", "Error converting a temperature in skydip fitting",
           status );
    return;
  }

  /* Get some memory for the double precision form of the data */
  dairmass = astMalloc( n_measurements * sizeof(*dairmass));
  ddata = astMalloc( n_measurements * sizeof(*ddata));
  derror = astMalloc( n_measurements * sizeof(*derror));

  /* Copy data and strip out bad values. Also calculate the mean
     variance. Temperature data must be positive. */
  ii = 0;
  sum = 0.0;
  for (i=0; i<(size_t)n_measurements; i++) {
    if (airmass[i] != VAL__BADR && data[i] != VAL__BADR
        && variance[i] != VAL__BADR
        && data[i] > 0.0 ) {
      double derr;

      dairmass[ii] = airmass[i];
      ddata[ii] = data[i];
      derr = sqrt(variance[i]);
      derror[ii] = ( derr > MINERR ? derr : MINERR);
      sum += (double)variance[ii];
      ii++;
    }
  }

  if (ii == 0) {
    *status = SAI__ERROR;
    errRep( "", "Zero good data points for skydip fit.", status );
    goto CLEANUP;
  }
  n_measurements = ii;

  /* Calculate the mean variance and force it into each data point
     if requested. */
  if (cvar) {
    double mean_err = sqrt(sum / (double)n_measurements);
    if (mean_err < MINERR) mean_err = MINERR;
    for (i=0; i<(size_t)n_measurements; i++) {
      derror[i] = mean_err;
    }
  }

  /* Assign data to the data struct */
  d.n = n_measurements;
  d.airmass = dairmass;
  d.j_sky = ddata;
  d.j_sky_sigma = derror;

  /* Determine the number of free parameters */
  p = 0;

  if (eta_l_in < 0.0) {
    d.eta_l_is_fixed = 0;
    x_init[p] = -1.0 * eta_l_in;
    p++;
  } else {
    d.eta_l_is_fixed = 1;
    d.eta_l_fixed = eta_l_in;
  }
  if (b_in < 0.0) {
    d.b_is_fixed = 0;
    x_init[p] = -1.0 * b_in;
    p++;
  } else {
    d.b_is_fixed = 1;
    d.b_fixed = b_in;
  }

  /* Tau is always free. Default TAU could be derived from known WVM or CSO data */
  x_init[p] = STARTTAU;
  p++;

  /* Calculate the number of degrees of freedom. We must have more than 5 degrees of
     freedom (and we should generally have at least 7 for a good data set (10-3) */
  dof = n_measurements - p;

  if (dof <= 5) {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRepf( "", "Not attempting to fit skydip with only %d measurements",
              status, n_measurements );
    }
    goto CLEANUP;
  }


  /* Initialise the solvers and setup GSL */
  gsl_rng_env_setup();
  x = gsl_vector_view_array( x_init, p );
  covar = gsl_matrix_alloc( p, p );

  type = gsl_rng_default;
  r = gsl_rng_alloc( type );

  f.f = &sculib__skydip_f;
  f.df = &sculib__skydip_df;
  f.fdf = &sculib__skydip_fdf;
  f.n = n_measurements;
  f.p = p;
  f.params = &d;

  T = gsl_multifit_fdfsolver_lmsder;
  s = gsl_multifit_fdfsolver_alloc (T, n_measurements, p);
  gsl_multifit_fdfsolver_set (s, &f, &x.vector);

  sculib__print_state( iter, p, s, status );
  do {
    iter++;
    gslstatus = gsl_multifit_fdfsolver_iterate(s);
    sculib__print_state( iter, p, s, status );
    if (gslstatus) break;
    gslstatus = gsl_multifit_test_delta( s->dx, s->x,
                                         1e-4, 1e-4);
  }
  while (gslstatus == GSL_CONTINUE && iter < 10000);

  gsl_multifit_covar( s->J, 0.0, covar );

  /* Calculate chisq of fit */
  chi = gsl_blas_dnrm2(s->f);
  drexisq = pow(chi, 2.0) / dof;

  /* Copy the fitted results to the appropriate variables to simplify
     reporting code. */
  sculib__get_fit_values( s->x, &d, covar,
                          &eta_l_final, &b_final, &tau_final,
                          &eta_l_err_final, &b_err_final,
                          &tau_err_final );

  /* Multiply errors by correction factor if our chisq was too high */
  cfac = GSL_MAX_DBL(1, chi / sqrt(dof));
  eta_l_err_final *= cfac;
  b_err_final *= cfac;
  tau_err_final *= cfac;

  if (gslstatus != GSL_SUCCESS) {
    *status = SAI__ERROR;
    dresidual = VAL__BADD;
    errRepf( "", "Skydip fit failed for filter = %s and sub-instrument %s",
             status, sub_filter, sub_instrument );
    errRepf( "", " GSL error status = %s",
             status, gsl_strerror(gslstatus));
    errRepf( "", " - final fit values were: eta = %g b = %g tau = %g  X= %g N = %zu",
             status, eta_l_final, b_final, tau_final, rexisq, iter );

  } else {
    double var = 0.0;

    /* Calculate the absolute residual and its variance as is
       done in SCULIB_SKYDIP_VAR */
    dresidual = 0.0;
    for (i=0; i < n_measurements; i++) {
      double j_theory = sculib__skydip_func( dairmass[i], d.j_tel, d.j_amb,
                                             eta_l_final, b_final, tau_final );
      double res = j_theory - ddata[i];
      if (derror[i] > 0.0) {
        var += (res * res) + (derror[i]*derror[i]);
      }
      dresidual += fabs(res);
    }

    /* divide the variance by the number of degrees of freedom */
    var /= (double)dof;
    dsigma = sqrt(var);

    /* Write out the results */
    msgOutf( "", "Skydip fit for filter = %s and sub-instrument %s",
             status, sub_filter, sub_instrument );
    msgOutf( "", " eta = %.5f +/- %.5f b = %.5f +/- %.5f tau = %.5f +/- %.5f",
             status, eta_l_final, eta_l_err_final, b_final, b_err_final,
             tau_final, tau_err_final );
    msgOutf( "", "Standard Deviation of fit residual = %.2f K (X = %.3f N = %zu)",
             status, sqrt(var), drexisq, iter );
    msgOutf( "", "Residual of fit: %.2f K", status, dresidual );

  }

  /* Copy into output variables */
  if (eta_l_fit) *eta_l_fit = eta_l_final;
  if (b_fit) *b_fit = b_final;
  if (tauz_fit) *tauz_fit = tau_final;
  if (rexisq) *rexisq = drexisq;
  if (tau_error) *tau_error = tau_err_final;
  if (eta_error) *eta_error = eta_l_err_final;
  if (b_error) *b_error = b_err_final;
  if (residual) *residual = dresidual;
  if (sigma) *sigma = dsigma;

 CLEANUP:
  if (ddata) ddata = astFree( ddata );
  if (derror) derror = astFree( derror );
  if (dairmass) dairmass = astFree( dairmass );
  if (s) gsl_multifit_fdfsolver_free(s);
  if (covar) gsl_matrix_free(covar);
  if (r) gsl_rng_free(r);

}

/* Now we need the Fortran interface */
#include "f77.h"

F77_SUBROUTINE(sculib_fit_skydip_gsl)( LOGICAL(CVAR), INTEGER(N_MEASUREMENTS),
                                      REAL_ARRAY(AIRMASS), REAL_ARRAY(DATA),
                                      REAL_ARRAY(VARIANCE), REAL(SUB_WAVELENGTH),
                                      CHARACTER(SUB_INSTRUMENT), CHARACTER(SUB_FILTER),
                                      REAL(T_TEL), REAL(T_AMB), REAL(ETA_L_IN),
                                      REAL(B_IN),
                                      REAL(ETA_L_FIT), REAL(B_FIT), REAL(TAUZ_FIT),
                                      REAL(REXISQ), REAL(TAU_ERROR), REAL(ETA_ERROR),
                                      REAL(B_ERROR), DOUBLE(RESIDUAL), DOUBLE(SIGMA),
                                      INTEGER(STATUS)
                                      TRAIL(SUB_INSTRUMENT)
                                      TRAIL(SUB_FILTER) ) {

  char sub_instrument[32];
  char sub_filter[32];
  int cvar;
  int n_measurements;
  int status;
  float sub_wavelength;
  float t_tel;
  float t_amb;
  float eta_l_in;
  float b_in;
  float eta_l_fit;
  float b_fit;
  float tauz_fit;
  float rexisq;
  float tau_error;
  float eta_error;
  float b_error;
  double residual;
  double sigma;

  F77_IMPORT_INTEGER( *STATUS, status );
  if (status != SAI__OK) return;

  cnfImpn( SUB_INSTRUMENT, SUB_INSTRUMENT_length, sizeof(sub_instrument),
           sub_instrument );
  cnfImpn( SUB_FILTER, SUB_FILTER_length, sizeof(sub_filter),
           sub_filter );

  F77_IMPORT_LOGICAL( *CVAR, cvar );
  F77_IMPORT_INTEGER( *N_MEASUREMENTS, n_measurements );
  F77_IMPORT_REAL( *SUB_WAVELENGTH, sub_wavelength );
  F77_IMPORT_REAL( *T_TEL, t_tel );
  F77_IMPORT_REAL( *T_AMB, t_amb );
  F77_IMPORT_REAL( *ETA_L_IN, eta_l_in );
  F77_IMPORT_REAL( *B_IN, b_in );

  sculib_fit_skydip_gsl( cvar, n_measurements, AIRMASS, DATA,
                         VARIANCE, sub_wavelength, sub_instrument,
                         sub_filter, t_tel, t_amb, eta_l_in,
                         b_in, &eta_l_fit, &b_fit, &tauz_fit,
                         &rexisq, &tau_error, &eta_error, &b_error,
                         &residual, &sigma, &status
                         );

  F77_EXPORT_REAL( eta_l_fit, *ETA_L_FIT );
  F77_EXPORT_REAL( b_fit, *B_FIT );
  F77_EXPORT_REAL( tauz_fit, *TAUZ_FIT );
  F77_EXPORT_REAL( rexisq, *REXISQ );
  F77_EXPORT_REAL( tau_error, *TAU_ERROR );
  F77_EXPORT_REAL( eta_error, *ETA_ERROR );
  F77_EXPORT_REAL( b_error, *B_ERROR );
  F77_EXPORT_DOUBLE( residual, *RESIDUAL );
  F77_EXPORT_DOUBLE( sigma, *SIGMA );
  F77_EXPORT_INTEGER(status, *STATUS );

}
