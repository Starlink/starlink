/**
 *  Name:
 *     XYProfile

 *  Language:
 *     C++

 *  Purpose:
 *     Defines a class that returns the average data values along the
 *     X and Y directions of a region on an image.

 *  Description:
 *     This routine uses a reference to an ImageIO object to access
 *     data values in all the supported RTD formats. It uses this to
 *     create lists of the average data values in the X and Y
 *     directions of a given region of the image. These values are
 *     intended for use in spectrum-like plots that want to average
 *     over many data values.

 *  Authors:
 *     P.W. Draper (PWD)

 *  Copyright:
 *     Copyright (C) 2000 Central Laboratory of the Research Councils
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
 *     10-JUL-2000 (PWD):
 *        Original version.
 *     30-MAY-2001 (PWD):
 *        Added double precision support.
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
#include "XYProfile.h"

/*
 *  Constructor, only imio reference is required.
 */
XYProfile::XYProfile( const ImageIO imio )
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
XYProfile::~XYProfile()
{
}

/*
 *   Set the limits of the region to be profiled (pixel indices).
 */
void XYProfile::setRegion( const int x0, const int y0,
                           const int x1, const int y1 )
{
    x0_ = x0;
    y0_ = y0;
    x1_ = x1;
    y1_ = y1;
}

/*
 *  Get the region that is to be profiled.
 */
void XYProfile::getRegion( int& x0, int& y0, int& x1, int& y1 )
{
   x0 = x0_;
   y0 = y0_;
   x1 = x1_;
   y1 = y1_;
}

/*
 *  Create the profiles.
 *
 *  The results are:
 *
 *     xCoords and yCoords contain the original X and Y pixel indices
 *     of the profile positions (these can be used to refer back to
 *     the image coordinates).
 *
 *     xVector and yVector contain pairs of values. The first value is
 *     the array index (along X or Y) and the second value the mean
 *     data values along that row or column.
 *
 *     numValues[2] contains the number of valid (i.e. non-BAD) values
 *     that are returned in xVector and yVector.
 */
void XYProfile::extractProfiles( double *xCoords, double *xVector,
                                 double *yCoords, double *yVector,
                                 int numValues[2] )
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

    //  Create the profiles. Call appropriate member for data format.
    switch ( type ) {
    case BYTE_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (unsigned char *) image, nx, ny, bscale, bzero,
                              x0, y0, x1, y1, xCoords, xVector,
                              yCoords, yVector, numValues );
        } else {
            extractNativeImage( (unsigned char *) image, nx, ny, bscale, bzero,
                                x0, y0, x1, y1, xCoords, xVector,
                                yCoords, yVector, numValues );
        }
        break;
    case X_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (char *) image, nx, ny, bscale,
                              bzero, x0, y0, x1, y1, xCoords, xVector,
                              yCoords, yVector, numValues );
        } else {
            extractNativeImage( (char *) image, nx, ny,
                                bscale, bzero, x0, y0,
                                x1, y1, xCoords, xVector, yCoords,
                                yVector, numValues );
        }
        break;
    case USHORT_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (ushort *) image, nx, ny, bscale, bzero,
                              x0, y0, x1, y1, xCoords, xVector,
                              yCoords, yVector, numValues );
        } else {
            extractNativeImage( (ushort *) image, nx, ny, bscale,
                                bzero, x0, y0, x1, y1, xCoords, xVector,
                                yCoords, yVector, numValues );
        }
        break;
    case SHORT_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (short *) image, nx, ny, bscale, bzero, x0, y0,
                              x1, y1, xCoords, xVector, yCoords,
                              yVector, numValues );
        } else {
            extractNativeImage( (short *) image, nx, ny, bscale,
                                bzero, x0, y0, x1, y1, xCoords, xVector,
                                yCoords, yVector, numValues );
        }
        break;
    case LONG_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (int *) image, nx, ny, bscale,
                              bzero, x0, y0, x1, y1, xCoords, xVector,
                              yCoords, yVector, numValues );
        } else {
            extractNativeImage( (int *) image, nx, ny, bscale,
                                bzero, x0, y0, x1, y1, xCoords, xVector,
                                yCoords, yVector, numValues );
        }
        break;
    case LONGLONG_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (INT64 *) image, nx, ny, bscale,
                              bzero, x0, y0, x1, y1, xCoords, xVector,
                              yCoords, yVector, numValues );
        } else {
            extractNativeImage( (INT64 *) image, nx, ny, bscale,
                                bzero, x0, y0, x1, y1, xCoords, xVector,
                                yCoords, yVector, numValues );
        }
        break;
    case FLOAT_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (float *) image, nx, ny, bscale, bzero,
                              x0, y0, x1, y1, xCoords, xVector,
                              yCoords, yVector, numValues );
        } else {
            extractNativeImage( (float *) image, nx, ny, bscale,
                                bzero, x0, y0, x1, y1, xCoords, xVector,
                                yCoords, yVector, numValues );
        }
        break;
    case DOUBLE_IMAGE:
        if ( swap_ ) {
            extractSwapImage( (double *) image, nx, ny, bscale, bzero,
                              x0, y0, x1, y1, xCoords, xVector,
                              yCoords, yVector, numValues );
        } else {
            extractNativeImage( (double *) image, nx, ny, bscale,
                                bzero, x0, y0, x1, y1, xCoords, xVector,
                                yCoords, yVector, numValues );
        }
        break;
    default:
        numValues[0] = numValues[1] = 0;
    }
}

/*  Define members that are data type dependent. See XYProfileTemplates.icc for
 *  which ones.
 */
#define DATA_TYPE char
#include "XYProfileTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE unsigned char
#include "XYProfileTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE short
#include "XYProfileTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE unsigned short
#include "XYProfileTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE int
#include "XYProfileTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE INT64
#include "XYProfileTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE float
#include "XYProfileTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE double
#include "XYProfileTemplates.icc"
#undef DATA_TYPE
