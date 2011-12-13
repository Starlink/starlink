//  Avoid inclusion into files more than once.
#ifndef _RegionStats_h_
#define _RegionStats_h_

/*+
 *  Name:
 *     RegionStats

 *  Purpose:
 *     Include file that defines the RegionStats class.

 *  Copyright:
 *     Copyright (C) 2001-2005 Central Laboratory of the Research Councils.
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

 *  Authors:
 *     P.W. Draper (PWD)

 *  History:
 *     23-JUL-2001 (PWD):
 *        Original version.
 *     {enter_changes_here}
 *-
 */

//  Include files:
#include <sys/types.h>
#include <netinet/in.h>
extern "C" {
#include "img.h"
}
#include "ImageData.h"

// The type "long" may have up to 64 bits on alpha machines. FITS
// defines the long we want to use as 32 bits, so use a macro to
// replace the long data type with plain int when appropriate.
#if SIZEOF_LONG == 8
#define FITS_LONG int
#else
#define FITS_LONG long
#endif

class RegionStats
{
 public:

    //  Constructor.
    RegionStats( const ImageIO imio );

    //  Destructor
    virtual ~RegionStats();

    //  Calculate the statistics.
    void calc();

    //  Set the region of the image.
    void setRegion( const int x0, const int y0, const int x1, const int y1 );

    //  Get the region.
    void getRegion( int& x0, int& y0, int& x1, int& y1 );

    //  Set whether image data is byte swapped
    void setSwap( const int swap ) { swap_ = swap; }

    // Get the results.
    long getArea() { return pixels_; }
    double getMin() { return min_; }
    double getMax() { return max_; }
    double getTotal() { return total_; }
    double getMean() { return mean_; }
    double getStd() { return std_; }

 protected:

    // Calculation results members.
    long pixels_;
    double min_;
    double max_;
    double total_;
    double mean_;
    double std_;

    //  Pointer to imageIO object. This has the image data and its type.
    ImageIO imageio_;

    //  Coordinates of image region.
    int x0_;
    int y0_;
    int x1_;
    int y1_;

    //  Whether the image data is byte swapped (from the machine
    //  native form).
    int swap_;

    //  Get pixel value from 2D array, "span" is second dimension. Use
    //  a macro to define this and expand for all possible data types.
#define GENERATE_ARRAYVAL( T ) \
  inline T arrayVal( const T *arrayPtr, const int& span, \
                     const int &ix, const int& iy ) \
     { return arrayPtr[iy*span + ix]; }
    GENERATE_ARRAYVAL(char);
    GENERATE_ARRAYVAL(unsigned char);
    GENERATE_ARRAYVAL(short);
    GENERATE_ARRAYVAL(unsigned short);
    GENERATE_ARRAYVAL(FITS_LONG);
    GENERATE_ARRAYVAL(float);
    GENERATE_ARRAYVAL(double);

    //  Get byte swapped pixel value from 2D array, "span" is second
    //  dimension. Cannot use a macro as need to be aware of the data
    //  size in bytes.
    inline char swapArrayVal( const char *arrayPtr, const int& span,
                              const int &ix, const int& iy )
        {
            return arrayPtr[iy*span + ix];
        }

    inline unsigned char swapArrayVal( const unsigned char *arrayPtr,
                                       const int& span, const int &ix,
                                       const int& iy )
        {
            return arrayPtr[iy*span + ix];
        }

    inline short swapArrayVal( const short *arrayPtr, const int& span,
                               const int &ix, const int& iy )
        {
            return (short)ntohs((unsigned short)arrayPtr[iy*span + ix]);
        }

    inline unsigned short swapArrayVal( const unsigned short *arrayPtr,
                                        const int& span, const int &ix,
                                        const int& iy )
        {
            return ntohs(arrayPtr[iy*span + ix]);
        }

    inline FITS_LONG swapArrayVal( const FITS_LONG *arrayPtr,
                                   const int& span,
                                   const int &ix,
                                   const int& iy )
        {
            return ntohl(arrayPtr[iy*span + ix]);
        }

    inline float swapArrayVal( const float *arrayPtr, const int& span,
                               const int &ix, const int& iy )
        {
            union { unsigned FITS_LONG raw; float typed; } ret;
            ret.typed = arrayPtr[iy*span + ix];
            ret.raw = ntohl(ret.raw);
            return ret.typed;
        }

    inline double swapArrayVal( const double *arrayPtr, const int& span,
                                const int &ix, const int& iy )
        {
            FITS_LONG tmp;
            union { unsigned FITS_LONG raw[2]; double typed; } ret;
            ret.typed = arrayPtr[iy*span + ix];
            tmp = ret.raw[0];
            ret.raw[0] = ntohl( ret.raw[1] );
            ret.raw[1] = ntohl( tmp );
            return ret.typed;
        }


    //  Test for a BAD pixel. XXX Note NDF specific doesn't check BLANK or NaN.
#define GENERATE_BADPIXA( T, BADVAL ) \
   inline int badpix( const T *image, const int& span, \
                      const int& i, const int& j ) \
      { return ( arrayVal( image, span, i    , j     ) == BADVAL ); }
    GENERATE_BADPIXA(char, VAL__BADB);
    GENERATE_BADPIXA(unsigned char, VAL__BADUB);
    GENERATE_BADPIXA(short, VAL__BADS);
    GENERATE_BADPIXA(unsigned short, VAL__BADUS);
    GENERATE_BADPIXA(FITS_LONG, VAL__BADI);
    GENERATE_BADPIXA(float, VAL__BADF);
    GENERATE_BADPIXA(double, VAL__BADD);

    //  Test for a BAD pixel, swapped version.
#define GENERATE_SWAPBADPIXA( T, BADVAL ) \
   inline int swapBadpix( const T *image, const int& span, \
                      const int& i, const int& j ) \
      { return ( swapArrayVal( image, span, i    , j     ) == BADVAL ); }
    GENERATE_SWAPBADPIXA(char, VAL__BADB);
    GENERATE_SWAPBADPIXA(unsigned char, VAL__BADUB);
    GENERATE_SWAPBADPIXA(short, VAL__BADS);
    GENERATE_SWAPBADPIXA(unsigned short, VAL__BADUS);
    GENERATE_SWAPBADPIXA(FITS_LONG, VAL__BADI);
    GENERATE_SWAPBADPIXA(float, VAL__BADF);
    GENERATE_SWAPBADPIXA(double, VAL__BADD);

    //  Data type dependent definitions, use overloaded members.
#define DATA_TYPE char
#include "RegionStatsTemplate.h"
#undef DATA_TYPE

#define DATA_TYPE unsigned char
#include "RegionStatsTemplate.h"
#undef DATA_TYPE

#define DATA_TYPE short
#include "RegionStatsTemplate.h"
#undef DATA_TYPE

#define DATA_TYPE unsigned short
#include "RegionStatsTemplate.h"
#undef DATA_TYPE

#define DATA_TYPE FITS_LONG
#include "RegionStatsTemplate.h"
#undef DATA_TYPE

#define DATA_TYPE float
#include "RegionStatsTemplate.h"
#undef DATA_TYPE

#define DATA_TYPE double
#include "RegionStatsTemplate.h"
#undef DATA_TYPE

};

#endif /* _RegionStats_ */
