/**
 *  Name:
 *     XYHistogram

 *  Language:
 *     C++

 *  Purpose:
 *     Defines a class that returns a histogram of the data values
 *     of a region on an image, together with some basic analysis.

 *  Description:
 *     This routine uses a reference to an ImageIO object to access
 *     data values in all the supported RTD formats. It uses this to
 *     create the histogram of the data values for a given region of the
 *     image. The histogram is assumed to be peaked and has a background
 *     estimation performed by fitting a parabola to get a best peak position
 *     and a least squares gaussian to determine the width and a second
 *     estimate of the position.

 *  Authors:
 *     P.W. Draper (PWD)

 *  Copyright:
 *     Copyright (C) 2014 Science and Technology Research Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  History:
 *     09-JAN-2014 (PWD):
 *        Original version.
 *     {enter_changes_here}
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <climits>
#include <cstring>
#include <cstdlib>
#include <cfloat>

/* GSL for fit */
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_multifit_nlin.h>

#include "define.h"
#include "XYHistogram.h"

/*
 *  Constructor, only imio reference is required.
 */
XYHistogram::XYHistogram( const ImageIO imio )
   : imageio_(imio),
     x0_(0),
     y0_(0),
     x1_(0),
     y1_(0),
     swap_(0),
     datalimits_(0),
     low_(-DBL_MAX),
     high_(DBL_MAX)
{
}

/*
 * Destructor
 */
XYHistogram::~XYHistogram()
{
}

/*
 *   Set the limits of the region to be used (pixel indices).
 */
void XYHistogram::setRegion( const int x0, const int y0,
                             const int x1, const int y1 )
{
    x0_ = x0;
    y0_ = y0;
    x1_ = x1;
    y1_ = y1;
}

/*
 *  Get the region that is to be used.
 */
void XYHistogram::getRegion( int& x0, int& y0, int& x1, int& y1 )
{
   x0 = x0_;
   y0 = y0_;
   x1 = x1_;
   y1 = y1_;
}

/*
 *  Create the histogram.
 */
void XYHistogram::extractHistogram( Histogram *histogram )
{

    //  Get image data properties.
    void *image = (void *) imageio_.dataPtr();
    int nx = imageio_.width();
    int ny = imageio_.height();
    int type = imageio_.bitpix();
    double bscale = imageio_.bscale();
    double bzero = imageio_.bzero();

    //  Make sure the part of the image to draw is sane, and change to
    //  array indices from image pixels.
    int x0 = max( 1, min( x0_, x1_ ) ) - 1;
    int y0 = max( 1, min( y0_, y1_ ) ) - 1;
    int x1 = min( nx, max( x1_, x0_ ) ) - 1;
    int y1 = min( ny, max( y1_, y0_ ) ) - 1;

    //  Get data limits, if used. Note these are unscaled.
    double low = -DBL_MAX;
    double high = DBL_MAX;
    if ( datalimits_ ) {
        low = (low_ - bzero) / bscale;
        high = (high_ - bzero) / bscale;
    }

    //  Create the histograms. Call appropriate member for data format.
    switch ( type ) {
    case BYTE_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (unsigned char *) image, nx, ny, bscale, bzero,
                              low, high, x0, y0, x1, y1, histogram );
        } else {
            extractNativeImage( (unsigned char *) image, nx, ny, bscale, bzero,
                                low, high, x0, y0, x1, y1, histogram );
        }
        break;
    case X_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (char *) image, nx, ny, bscale, bzero, 
                              low, high, x0, y0, x1, y1, histogram );
        } else {
            extractNativeImage( (char *) image, nx, ny, bscale, bzero, 
                                low, high, x0, y0, x1, y1, histogram );
        }
        break;
    case USHORT_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (ushort *) image, nx, ny, bscale, bzero,
                              low, high, x0, y0, x1, y1, histogram );
        } else {
            extractNativeImage( (ushort *) image, nx, ny, bscale, bzero, 
                                low, high, x0, y0, x1, y1, histogram );
        }
        break;
    case SHORT_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (short *) image, nx, ny, bscale, bzero, 
                              low, high, x0, y0, x1, y1, histogram );
        } else {
            extractNativeImage( (short *) image, nx, ny, bscale, bzero, 
                                low, high, x0, y0, x1, y1, histogram );
        }
        break;
    case LONG_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (int *) image, nx, ny, bscale, bzero, 
                              low, high, x0, y0, x1, y1, histogram );
        } else {
            extractNativeImage( (int *) image, nx, ny, bscale, bzero, 
                                low, high, x0, y0, x1, y1, histogram );
        }
        break;
    case LONGLONG_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (INT64 *) image, nx, ny, bscale, bzero, 
                              low, high, x0, y0, x1, y1, histogram );
        } else {
            extractNativeImage( (INT64 *) image, nx, ny, bscale, bzero, 
                                low, high, x0, y0, x1, y1, histogram );
        }
        break;
    case FLOAT_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (float *) image, nx, ny, bscale, bzero,
                              low, high, x0, y0, x1, y1, histogram );
        } else {
            extractNativeImage( (float *) image, nx, ny, bscale, bzero, 
                                low, high, x0, y0, x1, y1, histogram );
        }
        break;
    case DOUBLE_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (double *) image, nx, ny, bscale, bzero,
                              low, high, x0, y0, x1, y1, histogram );
        } else {
            extractNativeImage( (double *) image, nx, ny, bscale, bzero, 
                                low, high, x0, y0, x1, y1, histogram );
        }
        break;
    }

    //  Now do the analysis, if we have any chance of success.
    if ( histogram->nbin > MINBIN ) {
        fitHistParabola( histogram );
        fitGauss( histogram );
    }
}

/**
 *  Parabolic fit to a histogram to determine an interpolated
 *  maximum position.
 *
 *  histogram of data to fit, the structure is updated as needed.
 *
 *  On exit updates the histogram struct with the peak position and width.
 */
void XYHistogram::fitHistParabola( Histogram *histogram )
{
    /* Extract X and Y positions excluding the outer 10% of positions to
     * avoid fitting the wings of what might be an asymmetric distribution.
     * Use cumulative count to define the cut. */
    double sum = 0.0;
    for ( int i = 0; i < histogram->nbin; i++ ) {
        sum += histogram->hist[i];
    }
    double lsum = sum * 0.1;
    double usum = sum * 0.9;

    /* Locate outer indices. */
    sum = 0.0;
    int inner;
    for ( int i = 0; i < histogram->nbin; i++ ) {
        sum += histogram->hist[i];
        if ( sum >= lsum ) {
            inner = i;
            break;
        }
    }
    int outer;
    for ( int i = inner + 1; i < histogram->nbin; i++ ) {
        sum += histogram->hist[i];
        if ( sum >= usum ) {
            outer = i;
            break;
        }
    }

    /*  Allocate memory and populate X and Y arrays. */
    double *x = new double[outer - inner + 1];
    double *y = new double[outer - inner + 1];
    int ngood = 0;
    for ( int i = inner; i <= outer; i++ ) {
        if ( histogram->hist[i] > 0 ) {
            x[ngood] = (double) i;
            y[ngood] = histogram->hist[i];
            ngood++;
        }
    }

    /*  Solve the parabola */
    double coeff[3];
    fitParabola( ngood, x, y, coeff );

    /*  Record peak bin. */
    histogram->ppeak = -0.5 * coeff[1] / coeff[2];

    /*  Expected count for that bin. */
    double peak = coeff[0] + coeff[1] * histogram->ppeak +
                  coeff[2] * histogram->ppeak * histogram->ppeak;

    /*  Half peak value for FWHM est. */
    peak *= 0.5;
    double a = coeff[2];
    double b = coeff[1];
    double c = coeff[0];
    c -= peak;
    double sq = sqrt( ( b * b ) - ( 4.0 * a * c ) );
    double xl = ( -b - sq ) / ( 2.0 * a );
    double xh = ( -b + sq ) / ( 2.0 * a );
    histogram->pfwhm = fabs( xl - xh );

    /*  As gaussian sigma. */
    histogram->psd = histogram->pfwhm / 2.35;

    /*  Tidy up. */
    delete[] x;
    delete[] y;
}

/**
 *  Parabolic fit to a set of data.
 *
 *     y = c[0] + c[1]x + c[2]x^2
 *
 *  n number of positions
 *  x x positions
 *  y y position
 *  c[3] coefficients of fit.
 */
void XYHistogram::fitParabola( int n, double *x, double *y, double c[3] )
{
    /*  Note S(1) = 1.0, so all S() need to be divided by n. */
    double a0 = 1.0;  /* S(1)    */
    double a1 = 0.0;  /* S(x)    */
    double a2 = 0.0;  /* S(x^2)  */
    double a3 = 0.0;  /* S(x^3)  */
    double a4 = 0.0;  /* S(x^4)  */
    double b0 = 0.0;  /* S(y)    */
    double b1 = 0.0;  /* S(xy)   */
    double b2 = 0.0;  /* S(x^2y) */

    /*  Normal equation sums.
     *
     *  S(y)    = a S(1)   + b S(x)   + c S(x^2)
     *  S(xy)   = a S(x)   + b S(x^2) + c S(x^3)
     *  S(x^2y) = a S(x^2) + b S(x^3) + c S(x^4)
     *
    */
    double xx;
    double yy;
    for ( int m = 0; m < n; m++ ) {
        xx = x[m];
        yy = y[m];

        a1 += xx;
        xx *= x[m];

        a2 += xx;
        xx *= x[m];

        a3 += xx;
        xx *= x[m];

        a4 += xx;
        xx  = x[m];

        b0 += yy;
        b1 += yy * xx;
        b2 += yy * xx * xx;
    }

    /*  Normalisation for S(1) = 1.0. */
    a1 /= n;
    a2 /= n;
    a3 /= n;
    a4 /= n;

    b0 /= n;
    b1 /= n;
    b2 /= n;

    /*  Solutions */
    double d = a0 * ( a2 * a4 - a3 * a3 ) -
               a1 * ( a1 * a4 - a2 * a3 ) +
               a2 * ( a1 * a3 - a2 * a2 );

    double m_a = b0 * ( a2 * a4 - a3 * a3 ) +
                 b1 * ( a2 * a3 - a1 * a4 ) +
                 b2 * ( a1 * a3 - a2 * a2 );
    double m_b = b0 * ( a2 * a3 - a1 * a4 ) +
                 b1 * ( a0 * a4 - a2 * a2 ) +
                 b2 * ( a1 * a2 - a0 * a3 );
    double m_c = b0 * ( a1 * a3 - a2 * a2 ) +
                 b1 * ( a2 * a1 - a0 * a3 ) +
                 b2 * ( a0 * a2 - a1 * a1 );


    /*  Coefficients for parabola. */
    c[0] = m_a / d;
    c[1] = m_b / d;
    c[2] = m_c / d;
}


/**
 *  Support functions for Gaussian fitting. 
 **/
static int Gaussian_f( const gsl_vector *v, void *data, gsl_vector *f )
{
    size_t n = ((FunctionData *)data)->n;
    double *x = ((FunctionData *)data)->x;
    double *y = ((FunctionData *)data)->y;

    double sigma = gsl_vector_get( v, 2 );
    double x0 = gsl_vector_get( v, 1 );
    double A = gsl_vector_get( v, 0 );

    double sigmasq = sigma * sigma;

    for ( size_t i = 0; i < n; i++ ) {
        double dx = x[i] - x0;
        double dy = A * exp( -0.5 * dx * dx / sigmasq ) - y[i];
        gsl_vector_set( f, i, dy );
    }
    return GSL_SUCCESS;
}

static int Gaussian_df( const gsl_vector *v, void *data, gsl_matrix *J )
{
    size_t n = ((FunctionData *)data)->n;
    double *x = ((FunctionData *)data)->x;

    double sigma = gsl_vector_get( v, 2 );
    double x0 = gsl_vector_get( v, 1 );
    double A = gsl_vector_get( v, 0 );

    for ( size_t i = 0; i < n; i++ ) {
        double rbys = ( x[i] - x0 ) / sigma;
        double yy = exp( -0.5 * rbys * rbys );

        gsl_matrix_set( J, i, 2, A * yy * rbys * rbys / sigma );
        gsl_matrix_set( J, i, 1, A * yy * rbys / sigma );
        gsl_matrix_set( J, i, 0, yy );
    }
    return GSL_SUCCESS;
}

static int Gaussian_fdf( const gsl_vector *v, void *data,
                         gsl_vector *f, gsl_matrix *J )
{
    Gaussian_f( v, data, f );
    Gaussian_df( v, data, J );
    return GSL_SUCCESS;
}

/**
 *  Non-linear least squares fit of a Gaussian to the distribution.
 *  Requires a guess at the peak position and width (from the parabola fit).
 *
 *  On return the gaussian fields of the given histogram structure this
 *  will be updated.
 *
 */
void XYHistogram::fitGauss( Histogram *histogram )
{
    /*  Get the initial guesses. */
    double peak = histogram->ppeak;
    double width = histogram->psd;
    double scale = lookupHist_( histogram, (int)round(histogram->ppeak));

    /*  Trap nan issues, assuming any indicate complete failure. */
    if ( isnan( width ) || isnan( peak ) || isnan( width ) ) {
        width = histogram->nbin * 0.05;
        peak = histogram->mode;
        scale = histogram->hist[histogram->mode];
    }

    /*  Set the range of bins to consider when determining fit and not using
     *  user set limits. Look for the nearest left and right bins with 5% of
     *  the number count of the modal bin, attempt to reduce weight of
     *  interesting parts of image, and any badly masked parts. 
     */
    double limit = 0.05 * peak;
    int start = 0;
    int iend = histogram->nbin - 1;

    if ( ! datalimits_ ) {
        /*  Upper half */
        for ( int i = histogram->mode; i < histogram->nbin; i++ ) {
            if ( histogram->hist[ i ] != 0 ) {
                if ( histogram->hist[i] <= limit ) {
                    iend = i;
                    break;
                }
            }
        }

        /*  Lower half */
        for ( int i = histogram->mode; i >= 0; i-- ) {
            if ( histogram->hist[ i ] != 0 ) {
                if ( histogram->hist[i] <= limit ) {
                    start = i;
                    break;
                }
            }
        }
    }

    /*  Extract data. */
    size_t ncount = iend - start + 1;
    double *y = new double[ncount];
    double *x = new double[ncount];
    ncount = 0;
    for ( int j = start, i = 0; j <= iend; j++, i++ ) {
        if ( histogram->hist[ j ] != 0 ) {
            y[i] = (double) histogram->hist[ j ];
            x[i] = (double) j;
            ncount++;
        }
    }

    /*  Need sufficient points to be worthwhile. */
    if ( ncount < MINBIN ) {
        delete[] x;
        delete[] y;
        return;
    }

    /*  Use non-linear least squares solver from GSL library, lots of
     *  setup required... */
    const gsl_multifit_fdfsolver_type *T;
    const size_t n = ncount;
    const size_t p = 3;
    gsl_multifit_fdfsolver *s;
    int status;
    int iter = 0;

    gsl_matrix *covar = gsl_matrix_alloc( p, p );
    FunctionData data = { n, x, y };
    gsl_multifit_function_fdf f;
    double x_init[3] = { scale, peak, width };
    gsl_vector_view guess = gsl_vector_view_array( x_init, p );

    /*  Register callable functions that evaluate the Gaussian. */
    f.f = &Gaussian_f;
    f.df = &Gaussian_df;
    f.fdf = &Gaussian_fdf;

    f.n = n;
    f.p = p;
    f.params = &data;

    T = gsl_multifit_fdfsolver_lmsder;
    s = gsl_multifit_fdfsolver_alloc( T, n, p );
    gsl_multifit_fdfsolver_set( s, &f, &guess.vector );

    do {
        iter++;
        status = gsl_multifit_fdfsolver_iterate( s );
        if ( status ) {
            break;
        }
        status = gsl_multifit_test_delta( s->dx, s->x, DELTA, DELTA );
    }
    while ( status == GSL_CONTINUE && iter < MAXITER );

    /*  Gather fits and use covariance to estimate errors (weights not
     *  used so these will not be great). */
    gsl_multifit_covar( s->J, 0.0, covar );

    double chi = gsl_blas_dnrm2( s->f );
    double dof = n - p;
    double c = GSL_MAX_DBL( 1, chi / sqrt( dof ) );

    scale = gsl_vector_get( s->x, 0 );
    histogram->gpeak = gsl_vector_get( s->x, 1 );
    histogram->gdpeak = c * sqrt( gsl_matrix_get( covar, 1, 1 ) );
    histogram->gsd = gsl_vector_get( s->x, 2 );
    histogram->gdsd = c * sqrt( gsl_matrix_get( covar, 2, 2 ) );

    gsl_multifit_fdfsolver_free( s );
    gsl_matrix_free( covar );
    delete[] x;
    delete[] y;

    /* Sigma to FWHM */
    histogram->gfwhm = histogram->gsd * 2.35;

    /* Full evaluation for reference. */
    double sigmasq = (histogram->gsd * histogram->gsd);
    for ( int i = 0; i < histogram->nbin; i++ ) {
        double dx = (double) i - histogram->gpeak;
        histogram->ghist[i] = 
            (int) round( scale * exp( -0.5 * dx * dx / sigmasq ) );
    }
}

/*  Define members that are data type dependent. See XYHistogramTemplates.icc
 *  for which ones.
 */
#define DATA_TYPE char
#include "XYHistogramTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE unsigned char
#include "XYHistogramTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE short
#include "XYHistogramTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE unsigned short
#include "XYHistogramTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE int
#include "XYHistogramTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE INT64
#include "XYHistogramTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE float
#include "XYHistogramTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE double
#include "XYHistogramTemplates.icc"
#undef DATA_TYPE
