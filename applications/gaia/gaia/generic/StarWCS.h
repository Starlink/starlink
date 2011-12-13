//  Avoid inclusion into files more than once.
#ifndef _StarWCS_h_
#define _StarWCS_h_


/*+
 *  Name:
 *     StarWCS

 *  Purpose:
 *     Include file that defines the StarWCS class.

 *  Authors:
 *     P.W. Draper (PWD)
 *     Allan Brighton, ESO (ALLAN)

 *  Copyright:
 *     Copyright (C) 1997-2005 Central Laboratory of the Research Councils.
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
 *     24-JUL-1997 (PWD):
 *        Original version based on WCS for RTD.
 *     16-MAR-1998 (ALLAN)
 *        Renamed the local WCSRep class to StarWCS and removed local WCS class,
 *        to be compatible with the main Rtd/Skycat release.
 *        The original WCSRep class has been renamed to SAOWCS (based on the
 *        saoimage wcslib) and WCSRep is now an abstract base class.
 *        Here we define a subclass of WCSRep that defines the new behavior
 *        and uses the Starlink routines, rather then the sao version, as is
 *        used by skycat.
 *     22-APR-1998 (ALLAN)
 *        Fixed xSecPix() and ySecPix(), added setSecPix() method
 *        (needed for plotting some symbols based on world coordinates).
 *     22-JAN-2003 (PWD):
 *        Added milli-arcsec resolution option.
 *     19-DEC-2003 (PWD):
 *        Added CarLin static member and control method.
 *     10-FEB-2010 (PWD):
 *        Added forceDegrees static member.
 *-
 */

extern "C" {
#include "ast.h"
}
#include "WorldCoords.h"
#include "WCSRep.h"
#include "tcl.h"

//
//  This class is used internally for reference counting.
//  The public interface is through the WCS class.
//
class StarWCS : public WCSRep {

protected:
  //  Frameset with frames for image and world coordinates and
  //  mappings between them.
  AstFrameSet *wcs_;

  //  Equinox as 2000.0, 1950.0, ...
  double equinox_;

  //  Equinox string: "J2000", "B1950", ...
  char equinoxStr_[32];

  //  Projection, if known.
  char projection_[10];

  //  Show extra precision for milli-arcsec resolution.
  int extraPrecision_;

  //  Reference pixels (again if known).
  double xrefpix_;
  double yrefpix_;

  //  Set up object for celestial coordinates.
  void initCelestial();

  //  Set equinox value and string.
  void setEquinox();

  //  Set the xSecPix_ and ySecPix_ values (arcsecs per pixel).
  void setSecPix();

  //  Record if WCS is celestial
  void setCelestial();

  //  Width and height of the image in pixels.
  int nxpix_;
  int nypix_;

  //  Maximum number of dimensions we can process.
  enum {MAXDIM = 12};

  //  All the dimensions of the supplied data.
  int ndims_;
  int dims_[MAXDIM];

  //  Rotation of angle
  double rotate_;

  //  Make WCS 2D from sky to image.
  int make2D();

  //  The indices of the RA and Dec axes.
  int raIndex_;
  int decIndex_;

  //  Number of arcsecs per pixel.
  double xSecPix_, ySecPix_;

  //  Is WCS celestial?
  int issky_;

  //  Construct a single warning from all warning cards.
  void constructWarning( const char *encoding, int failed,
                         AstFitsChan *fitschan );

  //  Pointer to warnings string.
  char *warnings_;

  //  Whether CAR projections are broken linear. Applies to all instances.
  static int carlin_;

  //  Whether to work in degrees. Overrides everything.
  static int forceDegrees_;

public:

  //  Constructor (derived classes call this).
  StarWCS( const char* header, const size_t lheader,
           void *channelData = NULL,
           void (*)( AstFitsChan *, const char *,
                     int, int, int * ) = NULL );

  //  Destructor
  virtual ~StarWCS();

  // return class name as a string
  virtual const char* classname() const {return "StarWCS";}

  //  Return 1 if WCS info is available, else 0
  int isWcs() const {return (wcs_ && astIsAObject( wcs_ ));}

  //  Return 1 if WCS is a celestial coordinate system.
  int isCelestial() {return issky_;}

  //  Return the world coordinates string for the given image coords
  char* pix2wcs( double x, double y, char* buf, int bufsz,
                 int hms_flag = 1 ) const;

  //  Return the world coordinates string for the given image
  //  coords. Choose if offimage coordinates should be converted.
  char* pix2wcs( double x, double y, int notbound, char* buf, int bufsz,
                int hms_flag = 1 ) const;

  //  Return the world coords (in degrees, as 2 doubles) for the ximage coords
  int pix2wcs(double x, double y, double& ra, double& dec) const;

  //  Return the full world coords, can be more than 2 if needed.
  int pix2wcs( double x, double y, double wcs[], int &ndim ) const;

  //  Get the image coordinates for the given world coords
  int wcs2pix(double ra, double dec, double &x, double &y) const;

  //  Get the image coordinates for the given world coords
  int anyWcs2pix(double inx, double iny, int notcelestial,
                 double &outx, double &outy) const;

  //  Get the image coordinates distance for the given world coords distance in deg
  int wcs2pixDist(double ra, double dec, double &x, double &y) const;

  // get the world coords distance in deg for the given image coordinates distance
  int pix2wcsDist(double x, double y, double& ra, double& dec) const;

  //  Get the distance between two points.
  double dist( double ra0, double dec0, double ra1, double dec1 ) const;

  //  Get the distance between two points which are in the correct
  //  units (radians for celestial, returned in radians).
  double plaindist( double x0, double y0, double x1, double y1 ) const;

  //  Set up an WCS system from the given information about the image
  int set( double ra, double dec,
           double secpix,
           double xrefpix, double yrefpix,
           int nxpix, int nypix,
           double rotate,
           int equinox, double epoch,
           const char* proj );

  //  Set rotation and scaling
  int deltset( double cdelt1, double cdelt2, double rotation );

  //  Reset the center of the WCS structure
  int shift( double ra, double dec, double equinox );

  // Return the WCS equinox
  double equinox() const {return equinox_;}
  const char* equinoxStr() const {return equinoxStr_;}

  // Return the WCS epoch
  double epoch() const;

  // Return the rotation angle in degrees
  double rotate() const {return rotate_;}

  // Return the width, height, radius of the image in arc-minutes
  double width() const;
  double height() const;
  double radius() const;

  // Return the number of world coordinate arcseconds per pixel
  double xSecPix() const {return xSecPix_;}
  double ySecPix() const {return ySecPix_;}
  double secPix() const  {return ySecPix_;}

  // Return the world coordinates of the center of the image
  WorldCoords center() const;

  // Return image dimensions
  int pixWidth() const {return nxpix_;}
  int pixHeight() const {return nypix_;}

  // Return the x,y reference pixel values, only works for FITS-like headers,
  // otherwise return central pixel.
  double xRefPix() const { return xrefpix_; }
  double yRefPix() const { return yrefpix_; }

  // Return the projection type, only works for FITS-like headers.
  const char* projection() const { return projection_; }

  // Return a clone of the pointer to the main AstFrameSet.
  AstFrameSet *astWCSClone() const {
    if ( wcs_ && astIsAObject( wcs_ ) )
      return (AstFrameSet *) astClone( wcs_ );
    else
      return (AstFrameSet *) NULL;
  }

  // Return a copy of the pointer to the main AstFrameSet.
  AstFrameSet *astWCSCopy() const {
    if ( wcs_ && astIsAObject( wcs_ ) )
      return (AstFrameSet *) astCopy( wcs_ );
    else
      return (AstFrameSet *) NULL;
  }

  // Replace the main AstFrameSet with another.
  int astWCSReplace( AstFrameSet *newwcs);

  // Return the value of an AST attribute of the main AstFrameSet.
  const char *astGetAttrib( char *attrib );

  //  Return copy ofwarnings string.
  const char *getWarning();

  //  Get a list of the domains available in the frameset.
  char *getDomains( int dimens );

  //  Set an AST attribute.
  int astSetAttrib( const char *what, const char *value );

  //  Set display of extra precision.
  void extraPrecision( int value );

  //  Set whether CAR projections are linear.
  static void setCarLin( int value ) {
      StarWCS::carlin_ = value;
  }

  //  Set whether to force use of degrees.
  static void setForceDegrees( int value ) {
      StarWCS::forceDegrees_ = value;
  }

};


#endif /* _StarWCS_h_ */

