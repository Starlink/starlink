//  Avoid inclusion into files more than once.
#ifndef _XYProfile_h_
#define _XYProfile_h_


/*+
 * Name:
 *    XYProfile

 * Purpose:
 *    Include file that defines the XYProfile class.

 * Authors:
 *    P.W. Draper (PWD)

 * History:
 *    10-JUL-2000 (PWD):
 *       Original version.
 *    {enter_changes_here}
 *-
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

//  Include files:
#include <sys/types.h>
#include <netinet/in.h>
extern "C" {
#include "img.h"
}
#include "ImageData.h"

class XYProfile {

public:

  //  Constructor.
  XYProfile( const ImageIO imio );

  //  Destructor
  virtual ~XYProfile();

  //  Extract and return XYProfiles.
  void extractProfiles( double *xCoords, double *xVector,
                        double *yCoords, double *yVector, int numValues[2] );

  //  Set the region of the image to be profiled.
  void setRegion( const int x0, const int y0, const int x1, const int y1 );

  //  Get the region that is to be XYProfileed.
  void getRegion( int& x0, int& y0, int& x1, int& y1 );

  //  Set whether image data is byte swapped
  void setSwap( const int swap ) { swap_ = swap; }

 protected:

  //  Pointer to imageIO object. This has the image data and its type.
  ImageIO imageio_;

  //  Coordinates of part of image to be profiled.
  int x0_;
  int y0_;
  int x1_;
  int y1_;

  //  Whether the image data is byte swapped (from the machine native
  //  form).
  int swap_;

  //  Get pixel value from 2D array, "span" is second dimension. Use a
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

  inline int swapArrayVal( const int *arrayPtr,
                           const int& span,
                           const int &ix,
                           const int& iy )
     {
         return ntohl(arrayPtr[iy*span + ix]);
     }

  inline INT64 swapArrayVal( const INT64 *arrayPtr,
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


  //  Test for a BAD pixel. XXX Note NDF specific doesn't check BLANK or NaN.
#define GENERATE_BADPIXA( T, BADVAL ) \
   inline int badpix( const T *image, const int& span, \
                      const int& i, const int& j ) \
      { return ( arrayVal( image, span, i    , j     ) == BADVAL ); }
  GENERATE_BADPIXA(char, VAL__BADB);
  GENERATE_BADPIXA(unsigned char, VAL__BADUB);
  GENERATE_BADPIXA(short, VAL__BADS);
  GENERATE_BADPIXA(unsigned short, VAL__BADUS);
  GENERATE_BADPIXA(int, VAL__BADI);
  GENERATE_BADPIXA(INT64, VAL__BADK);
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
  GENERATE_SWAPBADPIXA(int, VAL__BADI);
  GENERATE_SWAPBADPIXA(INT64, VAL__BADK);
  GENERATE_SWAPBADPIXA(float, VAL__BADF);
  GENERATE_SWAPBADPIXA(double, VAL__BADD);

  //  Data type dependent definitions, use overloaded members.
#define DATA_TYPE char
#include "XYProfileTemplates.h"
#undef DATA_TYPE

#define DATA_TYPE unsigned char
#include "XYProfileTemplates.h"
#undef DATA_TYPE

#define DATA_TYPE short
#include "XYProfileTemplates.h"
#undef DATA_TYPE

#define DATA_TYPE unsigned short
#include "XYProfileTemplates.h"
#undef DATA_TYPE

#define DATA_TYPE int
#include "XYProfileTemplates.h"
#undef DATA_TYPE

#define DATA_TYPE INT64
#include "XYProfileTemplates.h"
#undef DATA_TYPE

#define DATA_TYPE float
#include "XYProfileTemplates.h"
#undef DATA_TYPE

#define DATA_TYPE double
#include "XYProfileTemplates.h"
#undef DATA_TYPE

};

#endif /* _XYProfile_ */
