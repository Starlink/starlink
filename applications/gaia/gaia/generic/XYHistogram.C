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
 *     and a gaussian to determine the width.

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
     swap_(0)
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

    //  Create the histograms. Call appropriate member for data format.
    switch ( type ) {
    case BYTE_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (unsigned char *) image, nx, ny, bscale, bzero,
                              x0, y0, x1, y1, histogram );
        } else {
            extractNativeImage( (unsigned char *) image, nx, ny, bscale, bzero,
                                x0, y0, x1, y1, histogram );
        }
        break;
    case X_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (char *) image, nx, ny, bscale,
                              bzero, x0, y0, x1, y1, histogram );
        } else {
            extractNativeImage( (char *) image, nx, ny,
                                bscale, bzero, x0, y0, x1, y1,
                                histogram );
        }
        break;
    case USHORT_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (ushort *) image, nx, ny, bscale, bzero,
                              x0, y0, x1, y1, histogram );
        } else {
            extractNativeImage( (ushort *) image, nx, ny, bscale,
                                bzero, x0, y0, x1, y1, histogram );
        }
        break;
    case SHORT_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (short *) image, nx, ny, bscale, bzero, x0, y0,
                              x1, y1, histogram );
        } else {
            extractNativeImage( (short *) image, nx, ny, bscale,
                                bzero, x0, y0, x1, y1, histogram );
        }
        break;
    case LONG_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (int *) image, nx, ny, bscale,
                              bzero, x0, y0, x1, y1, histogram );
        } else {
            extractNativeImage( (int *) image, nx, ny, bscale,
                                bzero, x0, y0, x1, y1, histogram );
        }
        break;
    case LONGLONG_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (INT64 *) image, nx, ny, bscale,
                              bzero, x0, y0, x1, y1, histogram );
        } else {
            extractNativeImage( (INT64 *) image, nx, ny, bscale,
                                bzero, x0, y0, x1, y1, histogram );
        }
        break;
    case FLOAT_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (float *) image, nx, ny, bscale, bzero,
                              x0, y0, x1, y1, histogram );
        } else {
            extractNativeImage( (float *) image, nx, ny, bscale,
                                bzero, x0, y0, x1, y1, histogram );
        }
        break;
    case DOUBLE_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (double *) image, nx, ny, bscale, bzero,
                              x0, y0, x1, y1, histogram );
        } else {
            extractNativeImage( (double *) image, nx, ny, bscale,
                                bzero, x0, y0, x1, y1, histogram );
        }
        break;
    }


    //  Now do the analysis.
    fitHistParabola( histogram );
    fitGauss( histogram );
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
    histogram->peak = -0.5 * coeff[1] / coeff[2];

    /*  Expected count for that bin. */
    double peak = coeff[0] + coeff[1] * histogram->peak +
                  coeff[2] * histogram->peak * histogram->peak;

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
 *  Fits a gaussian to histogram data with known peak to determine the
 *  standard deviation.
 *
 *  The routine uses a logarithmic transformation of a histogram of values to
 *  fit a gaussian whose peak position is known. This routine just determines
 *  the standard deviation of the fit.  An iterative refinement of the fit is
 *  performed to reduce the effects of non-gaussian outliers.
 *
 *  The iterative refinement is performed three times. Each time data outside
 *  of 95% of the peak count level are removed from the sums.
 *
 *  The standard deviation of the gaussian is determined using a logarithmic
 *  transformation of the equation:
 *
 *    y = Io * exp( -0.5* ( (x-Xo) /sigma ) **2 )
 *
 *  which breaks down to:
 *
 *    Y = ln( Io ) + (X/sigma)**2
 *
 *  where Y = ln(y) and X=(x-Xo).
 *
 *  A least squares solution to this gives:
 *
 *    2*sigma**2 = S(X*X)/(n*C-S(Y))
 *
 *  where:
 *     S() = sum of
 *     n = number of values
 *     C = ln(Io)
 *
 *  histogram of values to fit the gaussian  on return the sd field of this
 *  will be updated. 
 *
 */
void XYHistogram::fitGauss( Histogram *histogram )
{
    /*  Get the position of the peak count. */
    double peak = histogram->peak;

    /*  Take the natural logarithm. */
    double lnpeak = log( peak );

    /*  Set fraction of SD to convert into 95% of peak intensity. */
    double frac = sqrt( -2.0 * log( 0.05 ) );
    
    /*  Set the range of bins to consider when determining fit. Look for the
     *  nearest left and right bins with 5% of the number count of the modal
     *  bin. */
    double limit = 0.05 * peak;
    int start = 1;
    int iend = histogram->nbin;

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

    /*  Loop determining the gaussian standard deviation until the required
     *  number of iterations has been performed or the standard deviation is
     *  not changed by one bin. */
    double change;
    double denom = 0.0;
    double sdold = 0.0;
    double x = 0.0;
    double xsum = 0.0;
    double y = 0.0;
    double ysum = 0.0;
    int ncount = 0;
    int range = 0;
    for ( int i = 0; i < 3; i++ ) {
        /*  Form the current estimate of the deviation. */
        ncount = 0;
        ysum = 0.0;
        xsum = 0.0;
        for ( int j = start; j <= iend; j++ ) {
            if ( histogram->hist[ j ] != 0 ) {
                y = (double) histogram->hist[ j ];
                ysum = ysum + log( y );
                x = (double)( j - histogram->mode );
                xsum += x * x;
                ncount += 1;
            }
        }

        /*  Form the sigma estimate. */
        denom = (double) ncount * lnpeak - ysum;
        if ( denom != 0.0 ) {
            histogram->gsd = xsum / ( (double) ncount * lnpeak - ysum );
            histogram->gsd = sqrt( fabs( histogram->gsd / 2.0 ) );
        }
        else {
            /*  Force another iteration. */
            histogram->gsd = sdold + 2.0;
        }

        /*  Now look at change from last estimate. */
        if ( i != 1 ) {
            change = histogram->gsd - sdold;
        }
        else {
            change = 2.0;
        }

        if ( fabs( change ) > 1.0 ) {

            /*  Try again. Set the range of bins so that 95% of the current
             *  gaussian is used. */
            range = round( histogram->gsd * frac );
            start = max( 1, min( ( histogram->mode - range ),
                                 histogram->nbin ) );
            iend = max( 1, min( ( histogram->mode + range ),
                                histogram->nbin ) );

            /*  Record the current SD estimate. */
            sdold = histogram->gsd;
        }
        else {
            /*  End the refinement. */
            break;
        }
    }

    /* Sigma to FWHM */
    histogram->gfwhm = histogram->gsd * 2.35;
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
