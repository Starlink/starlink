//  Avoid inclusion into files more than once.
#ifndef _StarWCS_h_
#define _StarWCS_h_

//
//+
// Name:
//    StarWCS
//
// Purpose:
//    Include file that defines the StarWCS class.
//
// Authors:
//    P.W. Draper (PWD)
//    Allan Brighton, ESO (ALLAN)
//
// History:
//    24-JUL-1997 (PWD):
//       Original version based on WCS for RTD.
//    16-MAR-1998 (ALLAN)
//       Renamed the local WCSRep class to StarWCS and removed local WCS class, 
//       to be compatible with the main Rtd/Skycat release. 
//       The original WCSRep class has been renamed to SAOWCS (based on the
//       saoimage wcslib) and WCSRep is now an abstract base class.
//       Here we define a subclass of WCSRep that defines the new behavior 
//       and uses the Starlink routines, rather then the sao version, as is
//       used by skycat.
//    22-APR-1998 (ALLAN)
//       Fixed xSecPix() and ySecPix(), added setSecPix() method
//       (needed for plotting some symbols based on world coordinates).
//-

extern "C" {
#include "ast.h"
}
#include "WorldCoords.h"
#include "WCS.h"
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

  //  Set equinox value and string.
  void setEquinox();	     

  //  Set the xSecPix_ and ySecPix_ values (arcsecs per pixel).
  void setSecPix();	     

  //  Width and height of the image in pixels. 
  int nxpix_;
  int nypix_;

  //  Rotation of angle
  double rotate_;

  //  Make WCS 2D from sky to image.
  int make2D();
  
  //  Maximum number of dimensions we can process.
  enum {MAXDIM = 12};

  //  The indices of the RA and Dec axes.
  int raIndex_;
  int decIndex_;

  // number of arcsecs per pixel.
  double xSecPix_, ySecPix_;

public:

  //  Constructor (derived classes call this)
  StarWCS(const char* header);

  //  Destructor
  virtual ~StarWCS();

  // return class name as a string
  virtual const char* classname() const {return "StarWCS";}

  //  Return 1 if WCS info is available, else 0 
  int isWcs() const {return (wcs_ && astIsAObject( wcs_ ));}

  //  Return the world coordinates string for the given ximage coords
  char* pix2wcs(double x, double y, char* buf, int bufsz, int hms_flag = 1) const;

  //  Return the world coords (in degrees, as 2 doubles) for the ximage coords
  int pix2wcs(double x, double y, double& ra, double& dec) const;

  //  Get the image coordinates for the given world coords
  int wcs2pix(double ra, double dec, double &x, double &y) const;

  //  Get the image coordinates distance for the given world coords distance in deg
  int wcs2pixDist(double ra, double dec, double &x, double &y) const;

  // get the world coords distance in deg for the given image coordinates distance
  int pix2wcsDist(double x, double y, double& ra, double& dec) const;

  //  Get the distance between two points.
  double dist( double ra0, double dec0, double ra1, double dec1 ) const;

  //  Set up an WCS system from the given information about the image
  int set(double ra, double dec, 
          double secpix, 
          double xrefpix, double yrefpix,
          int nxpix, int nypix, 
          double rotate, 
          int equinox, double epoch,
          const char* proj);

  //  Reset the center of the WCS structure
  int shift(double ra, double dec, double equinox);

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

  // Return the x,y reference pixel values XXX ?????????
  double xRefPix() const {return 0.0;}
  double yRefPix() const {return 0.0;}

  // Return the projection type  XXX ??????????
  const char* projection() const {return "UNKNOWN";}

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
  const char *astGet( char *attrib );
};
  

#endif /* _StarWCS_h_ */

