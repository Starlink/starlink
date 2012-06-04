//  Avoid inclusion into files more than once.
#ifndef _Contour_h_
#define _Contour_h_

/*+
 *  Name:
 *     Contour

 *  Purpose:
 *     Include file that defines the Contour class.

 *  Copyright:
 *     Copyright (C) 1999-2005 Central Laboratory of the Research Councils.
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
 *     12-APR-1999 (PWD):
 *        Original version.
 *     {enter_changes_here}
 *-
 */

//  Include files:
#include <sys/types.h>
#include <netinet/in.h>
extern "C" {
#include "ast.h"
#include "img.h"
}
#include "ImageData.h"
#if HAVE_CONFIG_H
#include "config.h"
#endif

// Macro function to join two strings, when the strings may themselves
// be macros requiring further expansion.
#define DEFER_JOIN_STRINGS(string1,string2) string1 ## string2
#define JOIN_STRINGS(string1,string2) DEFER_JOIN_STRINGS(string1,string2)

class Contour {

public:
  //  Constructor.
  Contour( const ImageIO imio, const AstPlot *plot, const double
           levels[] = NULL, const int nlevels = 0,
           const char *prefs[] = NULL, const int nprefs = 0 );

  //  Destructor
  virtual ~Contour();

  //  Draw contours.
  int drawContours();

  //  Set/reset the contour levels.
  void setLevels( const double levels[], const int nlevels );

  //  Get the number of contour levels.
  int getNlevels() { return nlevels_; }

  //  Get a contour level (indexed up to nlevels_).
  double getLevel( const int ilevel );

  //  Set/reset the line preferences.
  void setPrefs( const char *prefs[], const int nprefs );

  //  Get the number of preferences.
  int getNprefs() { return nprefs_; }

  //  Get a copy of a set of preferences.
  char *getPrefs( const int ipref );

  //  Set the region of the image to be contoured.
  void setRegion( const int xlower, const int ylower,
                  const int xsize, const int ysize );

  //  Get the region that is to be contoured.
  void getRegion( int& xlower, int& ylower, int& xsize, int& ysize );

  //  Set the careful contouring plotting flag.
  void setCareful( const int careful ) { careful_ = careful; }

  //  Get the careful contouring plotting flag.
  int getCareful() { return careful_;}

  //  Set whether image data is byte swapped
  void setSwap( const int swap ) { swap_ = swap; }

  //  Set whether image data is from a FITS file and we need to handle
  //  NaN in the floating point.
  void setIsFITS( const int isfits) { isfits_ = isfits; }

 protected:
  //  Pointer to imageIO object. This has the image data and its type.
  ImageIO imageio_;

  //  AstPlot with frames for image grid and graphics coordinates and
  //  the mappings between them.
  AstPlot *plot_;

  //  Number of contour levels.
  int nlevels_;

  //  Contour levels.
  double *levels_;

  //  Number of line preferences (i.e. AST attributes for the curves).
  int nprefs_;

  //  Array of pointers to the preferences strings.
  char **prefs_;

  //  Coordinates of part of image to be contoured. These run from
  //  xlower to xlower+xsize-1 and ylower to ylower+ysize-1.
  int xlower_;
  int ylower_;
  int xsize_;
  int ysize_;

  //  Whether the coordinates are to be drawn quickly, or carefully.
  int careful_;

  //  Whether the image data is byte swapped (from the machine native
  //  form).
  int swap_;

  //  Whether this is a FITS image. Different idea of BAD values, especially
  //  for floats.
  int isfits_;

  //  Release existing contours.
  void freeLevels();

  //  Release existing preferences.
  void freePrefs();

  //  Maximum number of positions in each axis that define the locus of
  //  a contour (note this just limits the length of an individual segment).
  enum {MAXPTS = 10000};

  //  Get pixel value from 2D array, "span" is first dimension. Use a
  //  macro to define this and expand for all possible data types.
#define GENERATE_ARRAYVAL( T ) \
  inline T arrayVal( const T *arrayPtr, const int& span, \
                     const int &ix, const int& iy ) \
     { return arrayPtr[iy*span + ix]; }
  GENERATE_ARRAYVAL(char);
  GENERATE_ARRAYVAL(unsigned char);
  GENERATE_ARRAYVAL(short);
  GENERATE_ARRAYVAL(unsigned short);
  GENERATE_ARRAYVAL(int);
  GENERATE_ARRAYVAL(INT64);
  GENERATE_ARRAYVAL(float);
  GENERATE_ARRAYVAL(double);

  //  Get byte swapped pixel value from 2D array, "span" is first
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

  inline int swapArrayVal( const int *arrayPtr,
                           const int& span,
                           const int &ix,
                           const int& iy )
     {
        return ntohl(arrayPtr[iy*span + ix]);
     }

  inline double swapArrayVal( const INT64 *arrayPtr,
                              const int& span,
                              const int &ix,
                              const int& iy )
     {
         int tmp;
         union { unsigned int raw[2]; INT64 typed; } ret;
         ret.typed = arrayPtr[iy*span + ix];
         tmp = ret.raw[0];
         ret.raw[0] = ntohl( ret.raw[1] );
         ret.raw[1] = ntohl( tmp );
         return ret.typed;
     }

  inline float swapArrayVal( const float *arrayPtr, const int& span,
                             const int &ix, const int& iy )
     {
        union { unsigned int raw; float typed; } ret;
        ret.typed = arrayPtr[iy*span + ix];
        ret.raw = ntohl(ret.raw);
        return ret.typed;
     }

  inline double swapArrayVal( const double *arrayPtr, const int& span,
                              const int &ix, const int& iy )
     {
         int tmp;
         union { unsigned int raw[2]; double typed; } ret;
         ret.typed = arrayPtr[iy*span + ix];
         tmp = ret.raw[0];
         ret.raw[0] = ntohl( ret.raw[1] );
         ret.raw[1] = ntohl( tmp );
         return ret.typed;
     }

  //  Set an element of an array.
#define GENERATE_SETARRAYVAL( T ) \
  inline void setArrayVal( T *arrayPtr, const int& span, \
                           const int &ix, const int& iy, const T& value ) \
     { arrayPtr[iy*span + ix] = value; }
  GENERATE_SETARRAYVAL(char);
  GENERATE_SETARRAYVAL(unsigned char);
  GENERATE_SETARRAYVAL(short);
  GENERATE_SETARRAYVAL(unsigned short);
  GENERATE_SETARRAYVAL(int);
  GENERATE_SETARRAYVAL(INT64);
  GENERATE_SETARRAYVAL(float);
  GENERATE_SETARRAYVAL(double);

  // Maximum of two values of the same type.
#define GENERATE_MAX( T ) \
  inline const T& max( const T& a, const T &b ) \
     { return a > b ? a : b; }
  GENERATE_MAX(char);
  GENERATE_MAX(unsigned char);
  GENERATE_MAX(short);
  GENERATE_MAX(unsigned short);
  GENERATE_MAX(int);
  GENERATE_MAX(INT64);
  GENERATE_MAX(float);
  GENERATE_MAX(double);

  // Minimum of two values of the same type.
#define GENERATE_MIN( T ) \
  inline const T& min( const T& a, const T& b ) \
     { return a < b ? a : b; }
  GENERATE_MIN(char);
  GENERATE_MIN(unsigned char);
  GENERATE_MIN(short);
  GENERATE_MIN(unsigned short);
  GENERATE_MIN(int);
  GENERATE_MIN(INT64);
  GENERATE_MIN(float);
  GENERATE_MIN(double);

  //  Test for a BAD pixel within the current cell. NDF specific form.
#define GENERATE_BADPIXNDF( T, BADVAL ) \
   inline int badpixNDF( const T *image, const int& span, \
                         const int& i, const int& j ) \
      { return ( arrayVal( image, span, i    , j     ) == BADVAL ) || \
               ( arrayVal( image, span, i + 1, j     ) == BADVAL ) || \
               ( arrayVal( image, span, i + 1, j + 1 ) == BADVAL ) || \
               ( arrayVal( image, span, i    , j + 1 ) == BADVAL ); }
  GENERATE_BADPIXNDF(char, VAL__BADB);
  GENERATE_BADPIXNDF(unsigned char, VAL__BADUB);
  GENERATE_BADPIXNDF(short, VAL__BADS);
  GENERATE_BADPIXNDF(unsigned short, VAL__BADUS);
  GENERATE_BADPIXNDF(int, VAL__BADI);
  GENERATE_BADPIXNDF(INT64, VAL__BADK);
  GENERATE_BADPIXNDF(float, VAL__BADF);
  GENERATE_BADPIXNDF(double, VAL__BADD);

  //  Test for a BAD pixel within the current cell, swapped version.
#define GENERATE_SWAPBADPIXNDF( T, BADVAL ) \
   inline int swapBadpixNDF( const T *image, const int& span, \
                             const int& i, const int& j ) \
      { return ( swapArrayVal( image, span, i    , j     ) == BADVAL ) || \
               ( swapArrayVal( image, span, i + 1, j     ) == BADVAL ) || \
               ( swapArrayVal( image, span, i + 1, j + 1 ) == BADVAL ) || \
               ( swapArrayVal( image, span, i    , j + 1 ) == BADVAL ); }
  GENERATE_SWAPBADPIXNDF(char, VAL__BADB);
  GENERATE_SWAPBADPIXNDF(unsigned char, VAL__BADUB);
  GENERATE_SWAPBADPIXNDF(short, VAL__BADS);
  GENERATE_SWAPBADPIXNDF(unsigned short, VAL__BADUS);
  GENERATE_SWAPBADPIXNDF(int, VAL__BADI);
  GENERATE_SWAPBADPIXNDF(INT64, VAL__BADK);
  GENERATE_SWAPBADPIXNDF(float, VAL__BADF);
  GENERATE_SWAPBADPIXNDF(double, VAL__BADD);


  //  Test for a BAD pixel within the current cell. FITS specific form
  //  for floating point data. Note handle BLANK by contouring anyway
  //  but NaNs cause numeric issues and must be handled carefully.
  //  Test for a BAD pixel within the current cell, swapped version.
#define ISNAN(x) ((x) != (x))
#define GENERATE_BADPIXFITS( T ) \
   inline int badpixFITS( const T *image, const int& span, \
                          const int& i, const int& j ) \
       { return ( ISNAN(arrayVal( image, span, i, j         ) ) ) || \
                ( ISNAN(arrayVal( image, span, i + 1, j     ) ) ) || \
                ( ISNAN(arrayVal( image, span, i + 1, j + 1 ) ) ) || \
                ( ISNAN(arrayVal( image, span, i    , j + 1 ) ) ); }
  GENERATE_BADPIXFITS(float);
  GENERATE_BADPIXFITS(double);

  //  Test for a BAD pixel within the current cell, swapped version.
#define ISNAN(x) ((x) != (x))
#define GENERATE_SWAPBADPIXFITS( T ) \
   inline int swapBadpixFITS( const T *image, const int& span, \
                             const int& i, const int& j ) \
       { return ( ISNAN(swapArrayVal( image, span, i, j         ) ) ) || \
                ( ISNAN(swapArrayVal( image, span, i + 1, j     ) ) ) || \
                ( ISNAN(swapArrayVal( image, span, i + 1, j + 1 ) ) ) || \
                ( ISNAN(swapArrayVal( image, span, i    , j + 1 ) ) ); }
  GENERATE_SWAPBADPIXFITS(float);
  GENERATE_SWAPBADPIXFITS(double);

  //  Distance between two points, double and int versions.
  inline double rdist( const double x[], const double y[],
                       const int& i, const int& j )
    { return sqrt( ( x[i] - x[j] ) * ( x[i] - x[j] ) +
                   ( y[i] - y[j] ) * ( y[i] - y[j] ) ); }
  inline int dist( const double x[], const double y[],
                   const int& i, const int& j )
    { return (int) rdist( x, y, i, j ); }

  //  Are we outside the allowed part of image?
  inline int offimg( const int& xsize, const int &ysize,
                     const int& i, const int& j )
    { return ( i < 0 ) ||  ( j < 0 ) ||
             ( i >= xsize - 1 ) || ( j >= ysize - 1 ); }

  //  Plot a contour polyline.
  void contPlot( const AstPlot *lplot, const int npts,
                 const double x[], const double y[] );

  //  Data type dependent definitions, use overloaded members.
#define DATA_TYPE char
#define DATA_FORMAT NDF
#include "ContourTemplates.h"
#undef DATA_TYPE
#undef DATA_FORMAT

#define DATA_TYPE unsigned char
#define DATA_FORMAT NDF
#include "ContourTemplates.h"
#undef DATA_TYPE
#undef DATA_FORMAT

#define DATA_TYPE short
#define DATA_FORMAT NDF
#include "ContourTemplates.h"
#undef DATA_TYPE
#undef DATA_FORMAT

#define DATA_TYPE unsigned short
#define DATA_FORMAT NDF
#include "ContourTemplates.h"
#undef DATA_TYPE
#undef DATA_FORMAT

#define DATA_TYPE int
#define DATA_FORMAT NDF
#include "ContourTemplates.h"
#undef DATA_TYPE
#undef DATA_FORMAT

#define DATA_TYPE INT64
#define DATA_FORMAT NDF
#include "ContourTemplates.h"
#undef DATA_TYPE
#undef DATA_FORMAT

#define DATA_TYPE float
#define DATA_FORMAT NDF
#include "ContourTemplates.h"
#undef DATA_TYPE
#undef DATA_FORMAT

#define DATA_TYPE double
#define DATA_FORMAT NDF
#include "ContourTemplates.h"
#undef DATA_TYPE
#undef DATA_FORMAT

  // FITS NaN handling.
#define DATA_TYPE float
#define DATA_FORMAT FITS
#include "ContourTemplates.h"
#undef DATA_TYPE
#undef DATA_FORMAT

#define DATA_TYPE double
#define DATA_FORMAT FITS
#include "ContourTemplates.h"
#undef DATA_TYPE
#undef DATA_FORMAT

 private:
  //  Pointer used for passing back character strings.
  char *userBuffer_;
};

#endif /* _Contour_ */
