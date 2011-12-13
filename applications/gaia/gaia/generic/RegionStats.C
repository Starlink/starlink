/*+
 *  Name:
 *     RegionStats

 *  Language:
 *     C++

 *  Purpose:
 *     Derive statistics for a region of an image.

 *  Description:
 *     This routine uses a reference to an ImageIO object to access
 *     data values in all the supported RTD formats. Statistics, as
 *     needed by the UKIRT Quick Look Facility, are then derived and
 *     made available.

 *  Authors:
 *     P.W. Draper (PWD)
 *
 *  Copyright:
 *     Copyright (C) 2001 Central Laboratory of the Research Councils
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
 *     32-JUL-2001 (PWD):
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
#include <cstring>
#include <cfloat>
#include "define.h"
#include "RegionStats.h"

//
//  Constructor, need imio reference.
//
RegionStats::RegionStats( const ImageIO imio )
: imageio_(imio),
  x0_(0), y0_(0), x1_(1), y1_(1),
  swap_(0),
  pixels_(0), min_(0.0), max_(0.0), mean_(0.0), std_(0.0)
{
    //  Do nothing.
}

//
// Destructor
//
RegionStats::~RegionStats()
{
    //  Do nothing.
}

//
//   Set the limits of the region to be processed (pixel indices).
//
void RegionStats::setRegion( const int x0, const int y0,
                             const int x1, const int y1 )
{
    x0_ = x0;
    y0_ = y0;
    x1_ = x1;
    y1_ = y1;
}

//
//  Get the region that is to be processed.
//
void RegionStats::getRegion( int& x0, int& y0, int& x1, int& y1 )
{
    x0 = x0_;
    y0 = y0_;
    x1 = x1_;
    y1 = y1_;
}

//
//  Calculate the statistics.
//
//  The results are encoded as members that can be directly
//  referenced as values or encoded strings.
//
void RegionStats::calc()
{
    //  Get image data properties.
    void *image = (void *) imageio_.dataPtr();
    int nx = imageio_.width();
    int ny = imageio_.height();
    int type = imageio_.bitpix();
    double bscale = imageio_.bscale();
    double bzero = imageio_.bzero();

    //  Make sure the part of the image to draw is sane, and change to
    //  array indices from image pixels. XXX are QL coordinates array
    //  or pixel?
    int x0 = max( 1, min( x0_, x1_ ) ) - 1;
    int y0 = max( 1, min( y0_, y1_ ) ) - 1;
    int x1 = min( nx, max( x1_, x0_ ) ) - 1;
    int y1 = min( ny, max( y1_, y0_ ) ) - 1;

    //  Create the profiles. Call appropriate member for data format.
    switch ( type ) {
        case BYTE_IMAGE:
            if ( swap_ ) {
                calcSwap( (unsigned char *) image, nx, ny, bscale, bzero,
                          x0, y0, x1, y1 );
            } else {
                calcNative( (unsigned char *) image, nx, ny, bscale, bzero,
                            x0, y0, x1, y1 );
            }
            break;
        case X_IMAGE:
            if ( swap_ ) {
                calcSwap( (char *) image, nx, ny, bscale, bzero,
                          x0, y0, x1, y1 );
            } else {
                calcNative( (char *) image, nx, ny, bscale, bzero,
                            x0, y0, x1, y1 );
            }
            break;
        case USHORT_IMAGE:
            if ( swap_ ) {
                calcSwap( (ushort *) image, nx, ny, bscale, bzero,
                          x0, y0, x1, y1 );
            } else {
                calcNative( (ushort *) image, nx, ny, bscale, bzero,
                            x0, y0, x1, y1 );
            }
            break;
        case SHORT_IMAGE:
            if ( swap_ ) {
                calcSwap( (short *) image, nx, ny, bscale, bzero,
                          x0, y0, x1, y1 );
            } else {
                calcNative( (short *) image, nx, ny, bscale, bzero,
                            x0, y0, x1, y1 );
            }
            break;
        case LONG_IMAGE:
            if ( swap_ ) {
                calcSwap( (FITS_LONG *) image, nx, ny, bscale, bzero,
                          x0, y0, x1, y1 );
            } else {
                calcNative( (FITS_LONG *) image, nx, ny, bscale, bzero,
                            x0, y0, x1, y1 );
            }
            break;
        case FLOAT_IMAGE:
            if ( swap_ ) {
                calcSwap( (float *) image, nx, ny, bscale, bzero, x0, y0,
                          x1, y1 );
            } else {
                calcNative( (float *) image, nx, ny, bscale, bzero,
                            x0, y0, x1, y1 );
            }
            break;
        case DOUBLE_IMAGE:
            if ( swap_ ) {
                calcSwap( (double *) image, nx, ny, bscale, bzero,
                          x0, y0, x1, y1 );
            } else {
                calcNative( (double *) image, nx, ny, bscale, bzero,
                            x0, y0, x1, y1 );
            }
            break;
        default:
            break;
    }
}

//  Define members that are data type dependent. See
//  RegionStatsTemplates.icc for which ones.

#define DATA_TYPE char
#include "RegionStatsTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE unsigned char
#include "RegionStatsTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE short
#include "RegionStatsTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE unsigned short
#include "RegionStatsTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE FITS_LONG
#include "RegionStatsTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE float
#include "RegionStatsTemplates.icc"
#undef DATA_TYPE

#define DATA_TYPE double
#include "RegionStatsTemplates.icc"
#undef DATA_TYPE
