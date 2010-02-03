/*
*+
*  Name:
*     smf_fit_poly1d

*  Purpose:
*     Fit a polynomial to a 1d data set

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     void smf_fit_poly1d ( size_t order, size_t nelem, const double x[], const double y[],
*                            const double vary[], double coeffs[], double varcoeffs[],
*                            double polydata[], int * status );

*  Arguments:
*     order = size_t (Given)
*        Order of polynomial to use for the fit.
*     nelem = size_t (Given)
*        Number of elements in x, y and dy.
*     x = const double [] (Given)
*        X coordinates. If NULL the array index will be used.
*     y = const double [] (Given)
*        The data to fit. Must be of size nelem.
*     vary = const double [] (Given)
*        Variance of supplied data. Can be NULL for unweighted fit.
*     coeffs = double [] (Given & Returned)
*        Buffer of size order+1 to receive the coefficients of the fit.
*     varcoeffs = double [] (Given & Returned)
*        Buffer of size order+1 to receive the error in coefficients of the fit.
*        Can be NULL.
*     polydata = double [] (Given & Returned)
*        Evaluated polynomial at the x coordinates. Can be NULL. Must have
*        space for nelem points.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Fit a simple polynomial to a 1 dimensional dataset.

*  Notes:
*     o Can handle bad values. A bad variance will result in zero weight.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-01-29 (TIMJ):
*        First version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
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

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "smf_typ.h"
#include "smf.h"
#include "smurf_par.h"

#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"

#include "sc2da/sc2math.h"
#include "gsl/gsl_fit.h"
#include "gsl/gsl_multifit.h"

void smf_fit_poly1d ( size_t order, size_t nelem, const double x[], const double y[],
                      const double vary[], double coeffs[], double varcoeffs[],
                      double polydata[], int *status ) {

  const double * xx = NULL;
  double * xptr = NULL;

  memset(coeffs, 0, sizeof(*coeffs)*(order+1));
  if (varcoeffs) memset(varcoeffs, 0, sizeof(*varcoeffs)*(order+1));

  if (*status != SAI__OK) return;

  if (order == 0) {
    *status = SAI__ERROR;
    errRep( "", "smf_fit_poly1d does not calculate a simple mean", status );
    return;
  }

  if (order >= nelem) {
    *status = SAI__ERROR;
    errRepf( "", "Only %zd data points used for fitting, please use a smaller order than %zd",
             status, nelem, order);
    return;
  }

  /* Handle missing x coordinate */
  if (x) {
    xx = x;
  } else {
    size_t i;
    xptr = smf_malloc( nelem, sizeof(*xptr), 0, status );
    for ( i = 0; i < nelem; i++) {
      xptr[i] = i;
    }
    xx = xptr;
  }

  /* Special case a first order linear regression */
  if (order == 1 ) {
    size_t i;
    double c0, c1, cov00, cov01, cov11, chisq;

    if (vary) {
      /* Space for the weights */
      double * w = smf_malloc( nelem, sizeof(*w), 0, status );

      /* weighted fit */
      for (i = 0; i < nelem; i++) {
        if ( vary[i] == VAL__BADD || y[i] == VAL__BADD || xx[i] == VAL__BADD ||
             vary[i] == 0.0 ) {
          w[i] = 0.0;
        } else {
          w[i] = 1.0 / vary[i];
        }
      }

      gsl_fit_wlinear( xx, 1, w, 1, y, 1, nelem,  &c0, &c1, &cov00, &cov01, &cov11, &chisq );

      w = smf_free( w, status );

    } else {
      /* We need some space to copy the data because we are worried about bad values
         for x and y */
      double * fx = smf_malloc( nelem, sizeof(*fx), 0, status );
      double * fy = smf_malloc( nelem, sizeof(*fy), 0, status );
      size_t nrgood = 0;

      for (i = 0; i < nelem; i++) {
        if ( xx[i] != VAL__BADD && y[i] != VAL__BADD ) {
          fx[nrgood] = xx[i];
          fy[nrgood] = y[i];
          nrgood++;
        }
      }

      gsl_fit_linear( fx, 1, fy, 1, nrgood, &c0, &c1, &cov00, &cov01, &cov11, &chisq );

      fx = smf_free( fx, status );
      fy = smf_free( fy, status );
    }

    /* copy the result */
    coeffs[0] = (isnan(c0) ? VAL__BADD : c0 );
    coeffs[1] = (isnan(c1) ? VAL__BADD : c1 );

    if (varcoeffs) {
      varcoeffs[0] = (isnan(cov00) ? VAL__BADD : cov00);
      varcoeffs[1] = (isnan(cov11) ? VAL__BADD : cov11);
    }

    /* evaluate the polynomial */
    if (polydata) {
      double y_err;
      for (i = 0; i<nelem; i++) {
        double yval;
        gsl_fit_linear_est( xx[i], c0, c1, cov00, cov01, cov11, &(polydata[i]), &y_err);
        yval = xx[i] * c1 + c0;
      }
    }

    /* Report the fit details */
    if (msgFlevok( MSG__DEBUG2, status ) ) {
      msgOutiff( MSG__DEBUG2, "", "Best fit = %g + %g X  Reduced chisq = %f",
                 status, c0, c1, chisq / (nelem - order) );
    }

  } else {
    const int use_sc2math = 0;
    size_t i;

    if ( use_sc2math && order == 3 ) {
      /* unweighted fit using sc2math for a cubic */
      double *var = NULL;
      if (varcoeffs) {
        var = varcoeffs;
      } else {
        var = smf_malloc( order+1, sizeof(*var), 1, status );
      }
      sc2math_cubfit( nelem, (double*)x, (double*)y, coeffs, var, status);
      if (var && !varcoeffs) var = smf_free( var, status );
    } else {
      /* GSL method */
      size_t k;
      gsl_vector * mcoeffs = NULL;
      gsl_matrix * mcov = NULL;
      size_t ncoeff = order + 1;
      gsl_vector * W = NULL;
      gsl_matrix * X = NULL;
      gsl_vector * Y = NULL;
      gsl_multifit_linear_workspace *work = NULL;
      double chisq;

      /* allocate space */
      work = gsl_multifit_linear_alloc( nelem, ncoeff );
      X = gsl_matrix_alloc( nelem, ncoeff );
      Y = gsl_vector_alloc( nelem );
      W = gsl_vector_alloc( nelem );
      mcoeffs = gsl_vector_alloc( ncoeff );
      mcov = gsl_matrix_alloc( ncoeff, ncoeff );

      /* copy data into vectors */
      for (i = 0; i<nelem; i++) {
        double w;

        /* coordinates */
        for ( k = 0; k<ncoeff; k++) {
          double xik = (double) pow(xx[i], k);
          gsl_matrix_set( X, i, k, xik );
        }

        /* data */
        gsl_vector_set( Y, i, y[i] );

        /* weight - this is where we handle bad values */
        if (y[i] == VAL__BADD || xx[i] == VAL__BADD) {
          w = 0.0;
        } else if (vary) {
          if ( vary[i] == VAL__BADD || vary[i] == 0.0 ) {
            w = 0.0;
          } else {
            w = 1.0 / vary[i];
          }
        } else {
          w = 1.0;  /* equal weighting */
        }
        gsl_vector_set( W, i, w );

      }

      /* Carry out fit */
      gsl_multifit_wlinear( X, W, Y, mcoeffs, mcov, &chisq, work );

      /* Report the fit details */
      if (msgFlevok( MSG__DEBUG2, status ) ) {
        msgFmt( "POLY", "%g +", gsl_vector_get(mcoeffs,(0)));
        for (k = 1; k<ncoeff; k++) {
          msgFmt( "POLY", " %g", gsl_vector_get(mcoeffs,(k)));
          if (k>1) {
            msgFmt( "POLY", " X^%zd", k);
          } else {
            msgSetc( "POLY", " X");
          }
          if (k<ncoeff-1) msgSetc( "POLY", " +");
        }
        msgOutiff( MSG__DEBUG2, "", "Best fit = ^POLY  Reduced chisq = %f",
                   status, chisq / (nelem - order) );
      }

      /* Store coefficients */
      for (k=0; k<ncoeff; k++) {
        coeffs[k] = gsl_vector_get( mcoeffs, k );
        if (varcoeffs) {
          varcoeffs[k] = gsl_matrix_get( mcov, k, k );
        }
      }

      /* tidy up */
      gsl_multifit_linear_free( work );
      gsl_matrix_free( X );
      gsl_vector_free( Y );
      gsl_vector_free( W );
      gsl_vector_free( mcoeffs );
      gsl_matrix_free( mcov );
    }


    if (polydata) {
      for (i=0; i<nelem; i++) {
        size_t j;
        polydata[i] = coeffs[0];
        for ( j = 1; j <= order; j++) {
          polydata[i] += coeffs[j] * pow(xx[i], j );
        }
      }
    }

  }

  if (xptr) xptr = smf_free( xptr, status );

}
