// -*-c++-*-
#ifndef _SAOWCS_h_
#define _SAOWCS_h_
/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id: SAOWCS.h,v 1.5 1998/11/16 21:23:02 abrighto Exp $" 
 *
 * SAOWCS.h - declarations for class SAOWCS, an implementation class for
 *            class WCS, which is a reference counted class that manages
 *            a pointer to a class derived from WCSRep.
 *
 *            This class is based on Doug Mink's (saoimage) WCS C library.
 *            Other implementations of the WCS interface may be added by
 *            Defining new subclasses of the abstract class WCSRep.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  30 Sep 96  Created
 *                 17 Mar 98  Renamed from WSCRep, made WCSRep abstract,
 *                            to allow new implementations based on other
 *                            libraries.
 */


#include "WCS.hxx"
#include "WorldCoords.hxx"
#include "wcs.h"

/* 
 * This class defines an interface to Doug Mink's (saoimage) WCS C library.
 * The public interface is through the WCS class. 
 *
 *    Usage: WCS wcs(new SAOWCS(fits_header));
 *
 * The memory for the new object is managed by the WCS class with reference
 * counting (as long as you use normal assignment and not pointers).
 */
class SAOWCS : public WCSRep {
protected:
    WorldCoor* wcs_;		// C object to manage World Coordinates

    // The following are needed here because they are sometimes set to 0
    // in the wcs_ object
    double equinox_;		// equinox as 2000.0, 1950.0, ...
    char equinoxStr_[32];	// equinox string: "J2000", "B1950", ...
    
    double ra_, dec_;		// coordinates of image center in deg
    double width_, height_;     // image width and height in deg
    
    double xSecPix_, ySecPix_;	// number of arcsecs per pixel
    
    void setEquinox();		// set equinox value and string

public:
     // constructor (derived classes call this)
    SAOWCS(const char* header, int headerLength);

    // destructor
    virtual ~SAOWCS();

    // return class name as a string
    virtual const char* classname() const {return "SAOWCS";}

    // Return 1 if WCS info is available, else 0 
    int isWcs() const {return (wcs_ && ::iswcs(wcs_));}

    // return the world coordinates string for the given ximage coords
    char* pix2wcs(double x, double y, char* buf, int bufsz, int hms_flag = 1) const;

    // return the world coords (in degrees, as 2 doubles) for the ximage coords
    int pix2wcs(double x, double y, double& ra, double& dec) const;

    // get the image coordinates for the given world coords
    int wcs2pix(double ra, double dec, double &x, double &y) const;

    // get the image coordinates distance for the given world coords distance in deg
    int wcs2pixDist(double ra, double dec, double &x, double &y) const;

    // get the world coords distance in deg for the given image coordinates distance
    int pix2wcsDist(double x, double y, double& ra, double& dec) const;

    // set up the WCS structure from the given information about the image
    int set(double ra, double dec, 
		double secpix, 
		double xrefpix, double yrefpix,
		int nxpix, int nypix, 
		double rotate, 
		int equinox, double epoch,
		const char* proj);

    // reset the center of the WCS structure
    int shift(double ra, double dec, double equinox);

    // Return the WCS equinox
    double equinox() const {return equinox_;}
    const char* equinoxStr() const {return equinoxStr_;}

    // return the WCS epoch
    double epoch() const {return wcs_->epoch;}
   
    // return the rotation angle in degrees
    double rotate() const {return wcs_->rot;}
  
    // return the width, height, radius of the image in arcmin
    double width() const {return width_*60.;}
    double height() const {return height_*60.;}
    double radius() const;
    
    // return the number of world coordinate arcsecs per pixel
    double secPix() const {return ySecPix_;}
    double xSecPix() const {return xSecPix_;}
    double ySecPix() const {return ySecPix_;}

    // return the world coordinates of the center of the image 
    WorldCoords center() const;
    
    // return image dimensions
    int pixWidth() const {return int(wcs_->nxpix);}
    int pixHeight() const {return int(wcs_->nypix);}

    // Return the WCS distance between the 2 given WCS points in arcsec */
    double dist(double ra0, double dec0, double ra1, double dec1) const {
	return ::wcsdist(ra0, dec0, ra1, dec1) * 60.0 * 60.0;
    }
    
    // return the x,y reference pixel values
    double xRefPix() const {return wcs_->xrefpix;}
    double yRefPix() const {return wcs_->yrefpix;}

    // return the projection type
    const char* projection() const {return wcs_->ptype;}
};



#endif /* _SAOWCS_h_ */

