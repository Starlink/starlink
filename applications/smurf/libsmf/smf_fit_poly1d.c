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
*     void smf_fit_poly1d ( size_t order, size_t nelem, double clip,
*                           int maxiter,
*                           const double x[], const double y[],
*                           const double vary[], const smf_qual_t qual[],
*                           double coeffs[], double varcoeffs[],
*                           double polydata[], int64_t *nused,
*                           int *status );

*  Arguments:
*     order = size_t (Given)
*        Order of polynomial to use for the fit.
*     nelem = size_t (Given)
*        Number of elements in x, y and dy.
*     clip = double (Given)
*        Sigma clipping level. The fit is calculated and then the
*        standard deviation of the residual is calculated. If there
*        are any points greater than the supplied clip level the
*        points are removed and the polynomial refitted. This
*        continues until no points are removed, or "maxiter" iterations
*        have been performed. A value less than or equal to zero disables
*        clipping.
*     maxiter = int (Given)
*        Max number of clipping iterations. Zero or negative means no limit.
*     x = const double [] (Given)
*        X coordinates. If NULL the array index will be used.
*     y = const double [] (Given)
*        The data to fit. Must be of size nelem.
*     vary = const double [] (Given)
*        Variance of supplied data. Can be NULL for unweighted fit.
*     qual = const smf_qual_t [] (Given)
*        Optional array of quality with nelem elements. Can be NULL.
*     coeffs = double [] (Given & Returned)
*        Buffer of size order+1 to receive the coefficients of the fit.
*     varcoeffs = double [] (Given & Returned)
*        Buffer of size order+1 to receive the error in coefficients of the fit.
*        Can be NULL.
*     polydata = double [] (Given & Returned)
*        Evaluated polynomial at the x coordinates. Can be NULL. Must have
*        space for nelem points.
*     nused = int64_t * (Returned)
*        Number of points used in the fit. Can be smaller than nelem if any
*        of the points are bad or if the points have been removed during
*        sigma clipping.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Fit a simple polynomial to a 1 dimensional dataset. The fit will
*     be weighted if variance is supplied.

*  Notes:
*     o Can handle bad values. A bad variance will result in zero weight.
*     o If qual supplied, will use SMF__Q_FIT as a bitmask.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     EC: Ed Chapin (UBC)
*     MS: Matt Sherwood (ULeth)
*     {enter_new_authors_here}

*  History:
*     2010-01-29 (TIMJ):
*        First version.
*     2010-02-04 (TIMJ):
*        Add better debugging reports for fits.
*        Initialise all returned arrays to bad.
*        Handle NaN before returning.
*     2010-02-05 (TIMJ):
*        Add iterative sigma-clipping.
*     2010-09-15 (EC):
*        -Added special-case code for 0th order polynomial
*        -Added quality to interface
*     2013-07-31 (MS):
*        Expose alternate function that returns chi^2 value, but without clipping
*     2019-03-27 (DSB):
*        Added argument "maxiter".
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
*     Copyright (C) 2010 University of British Columbia.
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

#include <stdint.h>

#include "smf_typ.h"
#include "smf.h"
#include "smurf_par.h"

#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"

#include "sc2da/sc2math.h"
#include "gsl/gsl_fit.h"
#include "gsl/gsl_multifit.h"

void smf__fit_poly1d ( size_t order, size_t nelem, const double x[],
                       const double y[], const double vary[],
                       const smf_qual_t qual[], double coeffs[],
                       double varcoeffs[], double polydata[], int64_t *nused,
                       double * rchisq, int *status );

/* Expose alternate function that returns chi^2 value, but without clipping */
void smf_fit_poly1d_chisq ( size_t order, size_t nelem, const double x[],
                            const double y[], const double vary[],
                            const smf_qual_t qual[], double coeffs[],
                            double varcoeffs[], double polydata[], int64_t *nused,
                            double * rchisq, int *status ) {

    smf__fit_poly1d ( order, nelem, x, y, vary, qual, coeffs, varcoeffs,
                      polydata, nused, rchisq, status);

}

void smf_fit_poly1d ( size_t order, size_t nelem, double clip, int maxiter,
                      const double x[], const double y[], const double vary[],
                      const smf_qual_t qual[], double coeffs[],
                      double varcoeffs[], double polydata[], int64_t * nused,
                      int *status ) {
  int niter;
  size_t i;
  double rchisq;     /* Reduced chisq of fit */

  /* initialise to bad */
  for (i=0; i<order+1; i++) {
    coeffs[i] = VAL__BADD;
    if (varcoeffs) varcoeffs[i] = VAL__BADD;
    if (polydata) polydata[i] = VAL__BADD;
  }

  if (*status != SAI__OK) return;

  if ( clip <= 0.0 ) {

    smf__fit_poly1d ( order, nelem, x, y, vary, qual, coeffs, varcoeffs,
                      polydata, nused, &rchisq, status);

  } else {
    int iterating = 1;
    double *resid = NULL;
    double * pptr = polydata;
    double * polyptr = NULL;
    double * varptr = NULL;

    /* we need to calculate the polynomial expansion regardless */
    if (polydata) {
      pptr = polydata;
    } else {
      polyptr = astCalloc( nelem, sizeof(*polyptr) );
      pptr = polyptr;
    }

    /* the easiest approach is to control the contents of the
       variance array so that we can set elements to bad
       as we clip them out. This is much better than copying
       x and y to new arrays and making sure that polydata
       is expanded properly afterwards. It does mean that
       we will always end up in the weighted branch even if
       there is no supplied variance. */
    varptr = astMalloc( nelem*sizeof(*varptr) );
    if (varptr) {
      if (vary && varptr) {
        memcpy( varptr, vary, sizeof(*varptr) * nelem );
      } else {
        for (i=0; i< nelem; i++) {
          varptr[i] = 1.0; /* equal weighting */
        }
      }
    }

    resid = astMalloc( nelem*sizeof(*resid) );

    /* we are clipping */
    niter = 0;
    while ( ( maxiter <= 0 || niter < maxiter ) && iterating &&
            *status == SAI__OK) {
      niter++;
      double mean;
      double stdev;
      size_t ngood;
      double thresh;
      size_t nclipped;
      size_t maxidx;
      double maxresid;
      double prevchisq = VAL__BADD;

      /* calculate the fit */
      smf__fit_poly1d ( order, nelem, x, y, varptr, qual, coeffs, varcoeffs,
                        pptr, nused, &rchisq, status);

      if (*nused == 0) {
        iterating = 0;
        break;
      }

      /* calculate the residuals */
      ngood = 0;
      maxresid = 0.0;
      maxidx = VAL__BADI;
      for (i=0; i<nelem; i++) {
        if ( y[i] != VAL__BADD && pptr[i] != VAL__BADD && varptr[i] != VAL__BADD
             && varptr[i] != 0.0 && (qual ? !(qual[i]&SMF__Q_FIT) : 1) ) {
          resid[i] = y[i] - pptr[i];
          ngood++;
          if (resid[i] > maxresid) {
            maxresid = resid[i];
            maxidx = i;
          }
        } else {
          resid[i] = VAL__BADD;
        }
      }

      /* if there are now too few points for statistics we either let the fit
         through as is or we mark the fit as bad. For now let it through. */
      if ( ngood < SMF__MINSTATSAMP ) {
        iterating = 0;
        break;
      }

      /* calculate the standard deviation. Don't need quality here because
         resid is already VAL__BADD wherever there was bad quality */
      smf_stats1D( resid, 1, nelem, NULL, 0, 0, &mean, &stdev, NULL, &ngood,
                   status );

      /* see if any points are outside the clip range */
      thresh = clip * stdev;
      nclipped = 0;
      for ( i=0; i<nelem; i++) {
        if (resid[i] != VAL__BADD && fabs(resid[i]) > thresh ) {
          varptr[i] = VAL__BADD;
          nclipped++;
        }
      }

      if ( nclipped == 0 ) iterating = 0;
      prevchisq = rchisq;
    }

    /* clean up resources */
    if (polyptr) polyptr = astFree( polyptr );
    if (resid) resid = astFree( resid );
    if (varptr) varptr = astFree( varptr );

  }

}

/* internal implementation to simplify the iterative clipping routine */

void smf__fit_poly1d ( size_t order, size_t nelem, const double x[],
                       const double y[], const double vary[],
                       const smf_qual_t qual[], double coeffs[],
                       double varcoeffs[], double polydata[], int64_t *nused,
                       double * rchisq, int *status ) {

  const double * xx = NULL;
  double * xptr = NULL;
  size_t i;

  if (rchisq) *rchisq = VAL__BADD;

  /* initialise to bad in smf_fit_poly1d itself*/
  if (*status != SAI__OK) return;

  if (order >= nelem) {
    *status = SAI__ERROR;
    errRepf( "", "Only %zd data points used for fitting, please use a smaller "
             "order than %zd", status, nelem, order);
    return;
  }

  /* Handle missing x coordinate */
  if (x) {
    xx = x;
  } else {
    xptr = astMalloc( nelem*sizeof(*xptr) );
    for ( i = 0; i < nelem; i++) {
      xptr[i] = i;
    }
    xx = xptr;
  }

  if (order == 0 ) {
    /* Special case: a simple mean ********************************************/
    size_t nrgood = 0;
    double res_sq = 0;

    if (vary) {
      double mean=VAL__BADD;
      double sumweight=0;
      double sumweight_valsq=0;
      double sumweight_val=0;
      double variance=VAL__BADD;
      double w=0;

      /* weighted average */
      for (i = 0; i < nelem; i++) {
        if ( vary[i] != VAL__BADD && y[i] != VAL__BADD && vary[i] != 0.0 &&
             (qual ? !(qual[i]&SMF__Q_FIT) : 1) ) {
          w = 1.0 / vary[i];
          sumweight += w;
          sumweight_valsq += w*y[i]*y[i];
          sumweight_val += w*y[i];

          nrgood++;
        }
      }

      if( sumweight != 0 ) {
        mean = sumweight_val / sumweight;
        variance = (sumweight*sumweight_valsq - (sumweight_val*sumweight_val)) /
          ( nrgood * (sumweight*sumweight) );

        /* Calculate sum of residuals^2 */
        for (i = 0; i < nelem; i++) {
          if ( vary[i] != VAL__BADD && y[i] != VAL__BADD && vary[i] != 0.0 &&
               (qual ? !(qual[i]&SMF__Q_FIT) : 1) ) {
            res_sq += (y[i]-mean)*(y[i]-mean)/vary[i];
          }
        }
      }

      coeffs[0] = (isnan(mean) ? VAL__BADD : mean );
      if( varcoeffs) varcoeffs[0] = (isnan(variance) ? VAL__BADD : variance );
    } else {
      double mean;
      double meanvariance;
      double sigma;
      double variance;

      /* simple average */
      smf_stats1D( y, 1, nelem, qual, 1, SMF__Q_FIT, &mean, &sigma, NULL,
                   &nrgood, status );

      if( (*status == SAI__OK) && (nrgood > 0) ) {
        /* Convert population error to variance on the mean */
        variance = sigma*sigma;
        meanvariance = variance/nrgood;

        coeffs[0] = mean;
        if( varcoeffs ) varcoeffs[0] = meanvariance;

        /* Calculate sum of residuals^2 */
        for( i=0; i<nelem; i++ ) {
          if( y[i] != VAL__BADD && (qual ? !(qual[i]&SMF__Q_FIT) : 1) ) {
            res_sq += (y[i]-mean)*(y[i]-mean)/variance;
          }
        }
      } else {
        coeffs[0] = VAL__BADD;
        if( varcoeffs ) varcoeffs[0] = VAL__BADD;
      }
    }

    /* Normalize and return chisq */
    if( rchisq ) *rchisq = res_sq / ( nrgood - order );

    /* Evaluated polynomial is constant */
    if( polydata ) {
      for( i=0; i<nelem; i++ ) {
        polydata[i] = coeffs[0];
      }
    }

    /* Report the fit details */
    if (msgFlevok( MSG__DEBUG2, status ) ) {
      if (nrgood == nelem) {
        msgSeti( "NVAL", nrgood);
      } else {
        msgFmt( "NVAL", "%zd/%zd", nrgood, nelem);
      }
      msgOutiff( MSG__DEBUG2, "",
                 "Fit (%s) = %g Red. chisq = %f (^NVAL pnts)",
                 status, (vary ? "weighted" : "unweighted"), coeffs[0],
                 res_sq / ( nrgood - order ) );
    }


    if( nused ) *nused = nrgood;

  } else if (order == 1 ) {
    /* Special case: a first order linear regression **************************/
    double c0, c1, cov00, cov01, cov11, chisq;
    size_t nrgood = 0;

    if (vary) {
      /* Space for the weights */
      double * w = astMalloc( nelem*sizeof(*w) );

      /* weighted fit */
      for (i = 0; i < nelem; i++) {
        if ( vary[i] == VAL__BADD || y[i] == VAL__BADD || xx[i] == VAL__BADD ||
             vary[i] == 0.0 || (qual ? (qual[i]&SMF__Q_FIT) : 0) ) {
          w[i] = 0.0;
        } else {
          w[i] = 1.0 / vary[i];
          nrgood++;
        }
      }

      gsl_fit_wlinear( xx, 1, w, 1, y, 1, nelem,  &c0, &c1, &cov00, &cov01,
                       &cov11, &chisq );

      w = astFree( w );

    } else {
      /* We need some space to copy the data because we are worried about bad
         values for x and y */
      double * fx = astMalloc( nelem*sizeof(*fx) );
      double * fy = astMalloc( nelem*sizeof(*fy) );

      for (i = 0; i < nelem; i++) {
        if ( xx[i] != VAL__BADD && y[i] != VAL__BADD &&
             (qual ? !(qual[i]&SMF__Q_FIT) : 1) ) {
          fx[nrgood] = xx[i];
          fy[nrgood] = y[i];
          nrgood++;
        }
      }

      gsl_fit_linear( fx, 1, fy, 1, nrgood, &c0, &c1, &cov00, &cov01, &cov11,
                      &chisq );

      fx = astFree( fx );
      fy = astFree( fy );
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
        gsl_fit_linear_est( xx[i], c0, c1, cov00, cov01, cov11, &(polydata[i]),
                            &y_err);
        if (isnan(polydata[i])) polydata[i] = VAL__BADD;
      }
    }

    if( rchisq ) *rchisq = chisq / ( nrgood - order );

    /* Report the fit details */
    if (msgFlevok( MSG__DEBUG2, status ) ) {
      if (nrgood == nelem) {
        msgSeti( "NVAL", nrgood);
      } else {
        msgFmt( "NVAL", "%zd/%zd", nrgood, nelem);
      }
      msgOutiff( MSG__DEBUG2, "",
                 "Fit (%s) = %g + %g X Red. chisq = %f (^NVAL pnts)",
                 status, (vary ? "weighted" : "unweighted"), c0, c1,
                 chisq/(nrgood-order) );
    }

    if (nused) *nused = nrgood;

  } else {
    /* All higher-order fits **************************************************/
    const int use_sc2math = 0;

    if ( use_sc2math && order == 3 ) {
      /* unweighted fit using sc2math for a cubic */
      double *var = NULL;
      if (varcoeffs) {
        var = varcoeffs;
      } else {
        var = astCalloc( order+1, sizeof(*var) );
      }
      sc2math_cubfit( nelem, (double*)x, (double*)y, coeffs, var, status);
      if (var && !varcoeffs) var = astFree( var );

      /* sc2math assumes all the points are good so if we seriously
         want to continue with this option we really need to filter the
         data as for the other techniques. */
      if (nused) *nused = nelem;

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
      size_t nrgood = 0;

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
        if( qual ? (qual[i]&SMF__Q_FIT) : 0 ) {
          w = 0.0;
        } else if (y[i] == VAL__BADD || xx[i] == VAL__BADD) {
          w = 0.0;
        } else if (vary) {
          if ( vary[i] == VAL__BADD || vary[i] == 0.0 ) {
            w = 0.0;
          } else {
            w = 1.0 / vary[i];
            nrgood++;
          }
        } else {
          nrgood++;
          w = 1.0;  /* equal weighting */
        }
        gsl_vector_set( W, i, w );

      }

      /* Carry out fit */
      gsl_multifit_wlinear( X, W, Y, mcoeffs, mcov, &chisq, work );

      if( rchisq ) *rchisq = chisq / ( nrgood - order );

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
        msgOutiff( MSG__DEBUG2, "",
                   "Fit (%s) = ^POLY  Red. chisq = %f (%zd/%zd pnts)",
                   status, (vary ? "weighted" : "unweighted"),
                   chisq/(nrgood-order), nrgood,
                   nelem );
      }

      if (nused) *nused = nrgood;

      /* Store coefficients */
      for (k=0; k<ncoeff; k++) {
        coeffs[k] = gsl_vector_get( mcoeffs, k );
        if(isnan(coeffs[k])) coeffs[k] = VAL__BADD;
        if (varcoeffs) {
          varcoeffs[k] = gsl_matrix_get( mcov, k, k );
          if (isnan(varcoeffs[k])) varcoeffs[k] = VAL__BADD;
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
        EVALPOLY( polydata[i], xx[i], order, coeffs )
      }
    }

  }

  if (xptr) xptr = astFree( xptr );

}
