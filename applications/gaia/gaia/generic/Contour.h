//  Avoid inclusion into files more than once.
#ifndef _Contour_h_
#define _Contour_h_

//
//+
// Name:
//    Contour
//
// Purpose:
//    Include file that defines the Contour class.
//
// Authors:
//    P.W. Draper (PWD)
//
// History:
//    12-APR-1999 (PWD):
//       Original version.
//    {enter_changes_here}
//-

//  Include files:
#include <sys/types.h>
extern "C" {
#include "ast.h"
#include "img.h"
}
#include "ImageData.h"
#include "config.h"

// The type "long" may have up to 64 bits on alpha machines. FITS
// defines the long we want to use as 32 bits, so use a macro to
// replace the long data type with plain int when appropriate.
#if SIZEOF_LONG == 8
#define FITS_LONG int
#else
#define FITS_LONG long
#endif

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

  //  Release existing contours.
  void freeLevels();

  //  Release existing preferences.
  void freePrefs();

  //  Maximum number of positions in each axis that define the locus of
  //  a contour (note this just limits the length of an individual segment).
  enum {MAXPTS = 10000};

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
  GENERATE_ARRAYVAL(FITS_LONG);
  GENERATE_ARRAYVAL(float);

  //  Set an element of an array.
#define GENERATE_SETARRAYVAL( T ) \
  inline void setArrayVal( T *arrayPtr, const int& span, \
                           const int &ix, const int& iy, const T& value ) \
     { arrayPtr[iy*span + ix] = value; }
  GENERATE_SETARRAYVAL(char);
  GENERATE_SETARRAYVAL(unsigned char);
  GENERATE_SETARRAYVAL(short);
  GENERATE_SETARRAYVAL(unsigned short);
  GENERATE_SETARRAYVAL(FITS_LONG);
  GENERATE_SETARRAYVAL(float);

  // Maximum of two values of the same type.
#define GENERATE_MAX( T ) \
  inline T& max( const T& a, const T &b ) \
     { return a > b ? a : b; }
  GENERATE_MAX(char);
  GENERATE_MAX(unsigned char);
  GENERATE_MAX(short);
  GENERATE_MAX(unsigned short);
  GENERATE_MAX(FITS_LONG);
  GENERATE_MAX(float);
  GENERATE_MAX(double);

  // Minimum of two values of the same type.
#define GENERATE_MIN( T ) \
  inline T& min( const T& a, const T& b ) \
     { return a < b ? a : b; }
  GENERATE_MIN(char);
  GENERATE_MIN(unsigned char);
  GENERATE_MIN(short);
  GENERATE_MIN(unsigned short);
  GENERATE_MIN(FITS_LONG);
  GENERATE_MIN(float);
  GENERATE_MIN(double);

  //  Test for a BAD pixel within the current cell.
#define GENERATE_BADPIX( T, BADVAL ) \
   inline int badpix( const T *image, const int& span, \
                      const int& i, const int& j ) \
      { return ( arrayVal( image, span, i    , j     ) == BADVAL ) || \
               ( arrayVal( image, span, i + 1, j     ) == BADVAL ) || \
               ( arrayVal( image, span, i + 1, j + 1 ) == BADVAL ) || \
               ( arrayVal( image, span, i    , j + 1 ) == BADVAL ); }
  GENERATE_BADPIX(char, VAL__BADB);
  GENERATE_BADPIX(unsigned char, VAL__BADUB);
  GENERATE_BADPIX(short, VAL__BADS);
  GENERATE_BADPIX(unsigned short, VAL__BADUS);
  GENERATE_BADPIX(FITS_LONG, VAL__BADI);
  GENERATE_BADPIX(float, VAL__BADF);

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
#include "ContourTemplates.h"
#undef DATA_TYPE

#define DATA_TYPE unsigned char
#include "ContourTemplates.h"
#undef DATA_TYPE

#define DATA_TYPE short
#include "ContourTemplates.h"
#undef DATA_TYPE

#define DATA_TYPE unsigned short
#include "ContourTemplates.h"
#undef DATA_TYPE

#define DATA_TYPE FITS_LONG
#include "ContourTemplates.h"
#undef DATA_TYPE

#define DATA_TYPE float
#include "ContourTemplates.h"
#undef DATA_TYPE

 private:
  //  Pointer used for passing back character strings.
  char *userBuffer_;
};

#endif /* _Contour_ */
