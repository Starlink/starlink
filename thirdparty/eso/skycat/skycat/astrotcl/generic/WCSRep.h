// -*-c++-*-
#ifndef _WCS_h_
#define _WCS_h_
/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id$" 
 *
 * WCS.h - declarations for class WCS, a reference counted wrapper class
 *         for managing world coordinates for an image. The implementation
 *         class is a subclass of the abstract WCSRep class.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  30 Sep 96  Created 
 *                 17 MAr 98  Made WCSRep an abstract class, to allow
 *                            new WCS implementations. The old WCSRep
 *                            class was renamed to SAOWCS.
 */


#include "WorldCoords.hxx"

/* 
 * This class is used internally for reference counting.
 * The public interface is through the WCS class.
 */
class WCSRep {
friend class WCS;
private:
    int refcnt_;		// reference count
    int status_;		// status after constructor

public:
     // constructor
    WCSRep();

    // destructor
    virtual ~WCSRep();

    // return class name as a string
    virtual const char* classname() const = 0;

    // Return 1 if WCS info is available, else 0 
    virtual int isWcs() const = 0;

    // return the world coordinates as a string for the given ximage coords
    virtual char* pix2wcs(double x, double y, char* buf, int bufsz, int hms_flag = 1) const = 0;

    // return the world coords (in degrees, as 2 doubles) for the ximage coords
    virtual int pix2wcs(double x, double y, double& ra, double& dec) const = 0;

    // get the image coordinates for the given world coords
    virtual int wcs2pix(double ra, double dec, double &x, double &y) const = 0;

    // get the image coordinates distance for the given world coords distance in deg
    virtual int wcs2pixDist(double ra, double dec, double &x, double &y) const = 0;

    // get the world coords distance in deg for the given image coordinates distance
    virtual int pix2wcsDist(double x, double y, double& ra, double& dec) const = 0;

    // set up the WCS structure from the given information about the image
    virtual int set(double ra, double dec, 
		double secpix, 
		double xrefpix, double yrefpix,
		int nxpix, int nypix, 
		double rotate, 
		int equinox, double epoch,
		const char* proj) = 0;

    // reset the center of the WCS structure
    virtual int shift(double ra, double dec, double equinox) = 0;

    // Return the WCS equinox
    virtual double equinox() const = 0;

    // Return the WCS equinox as a string
    virtual const char* equinoxStr() const = 0;

    // return the WCS epoch
    virtual double epoch() const = 0;
   
    // return the rotation angle in degrees
    virtual double rotate() const = 0;
  
    // return the width, height, radius of the image in arcmin
    virtual double width() const = 0;
    virtual double height() const = 0;
    virtual double radius() const = 0;
    
    // return the number of world coordinate arcsecs per pixel
    virtual double secPix() const = 0;
    virtual double xSecPix() const = 0;
    virtual double ySecPix() const = 0;

    // return the world coordinates of the center of the image 
    virtual WorldCoords center() const = 0;
    
    // return image dimensions
    virtual int pixWidth() const = 0;
    virtual int pixHeight() const = 0;

    // Return the WCS distance between the 2 given WCS points in arcsec */
    virtual double dist(double ra0, double dec0, double ra1, double dec1) const = 0;

    // return the x,y reference pixel values
    virtual double xRefPix() const = 0;
    virtual double yRefPix() const = 0;

    // return the projection type
    virtual const char* projection() const = 0;

    // member access
    virtual int status() const {return status_;}
    virtual void status(int s) {status_ = s;}
};


/* 
 * This class defines the public interface. It uses reference
 * counting with the above class to make it easier to share copies
 * of this object.
 */
class WCS {
private:
    WCSRep* rep_;		// internal representation for reference counting

public:
    // default constructor: initialize to null
    WCS() : rep_(NULL) {}

    // copy constructor
    WCS(const WCS&);

    // Constructor, from a pointer to a subclass of WCSRep.
    // The memory is managed by this class after this call.
    WCS(WCSRep* rep) : rep_(rep) {}

    // destructor
    ~WCS();

    // assignment
    WCS& operator=(const WCS&);

    // Return 1 if WCS info is available, else 0 
    // (always check this before calling any other methods)
    int isWcs() const {return rep_ && rep_->isWcs();}

    // return the world coordinates string for the given ximage coords
    // member access
    int pixWidth() const {return rep_->pixWidth();}
    int pixHeight() const {return rep_->pixHeight();}

    // Return the WCS distance between the 2 given WCS points in arcsec */
    double dist(double ra0, double dec0, double ra1, double dec1) const {
	return rep_->dist(ra0, dec0, ra1, dec1);
    }
    
    char* pix2wcs(double x, double y, char* buf, int bufsz, int hms_flag = 1) const {
	return rep_->pix2wcs(x, y, buf, bufsz, hms_flag);
    }

    // return the world coords (in degrees, as 2 doubles) for the ximage coords
    int pix2wcs(double x, double y, double& ra, double& dec) const {
	return rep_->pix2wcs(x, y, ra, dec);
    }

    // get the image coordinates for the given world coords
    int wcs2pix(double ra, double dec, double &x, double &y) const {
	return rep_->wcs2pix(ra, dec, x, y);
    }

    // get the image coordinates distance for the given world coords distance in deg
    int wcs2pixDist(double ra, double dec, double &x, double &y) const {
	return rep_->wcs2pixDist(ra, dec, x, y);
    }

    // get the world coords distance in deg for the given image coordinates distance
    int pix2wcsDist(double x, double y, double& ra, double& dec) const {
	return rep_->pix2wcsDist(x, y, ra, dec);
    }

    // set up the WCS structure from the given information about the image
    int set(double ra, double dec, 
		double secpix, 
		double xrefpix, double yrefpix,
		int nxpix, int nypix, 
		double rotate, 
		int equinox, double epoch,
		const char* proj) {
	return rep_->set(ra, dec, secpix, xrefpix, yrefpix, nxpix, nypix, rotate, 
			 equinox, epoch, proj);
    }

    // reset the center of the WCS structure
    int shift(double ra, double dec, double equinox) {
	return rep_->shift(ra, dec, equinox);
    }

    // Return the WCS equinox
    double equinox() const {return rep_->equinox();}
    const char* equinoxStr() const {return rep_->equinoxStr();}

    // return the WCS epoch
    double epoch() const {return rep_->epoch();}
   
    // return the rotation angle in degrees
    double rotate() const {return rep_->rotate();}

    // return the width, height, radius of the image in arcmin
    double width() const {return rep_->width();}
    double height() const {return rep_->height();}
    double radius() const {return rep_->radius();}
    
    // return the number of world coordinate arcsecs per pixel
    double secPix() const {return rep_->secPix();}
    double xSecPix() const {return rep_->xSecPix();}
    double ySecPix() const {return rep_->ySecPix();}

    // return the x,y reference pixel values
    double xRefPix() const {return rep_->xRefPix();}
    double yRefPix() const {return rep_->yRefPix();}

    // return the projection type
    const char* projection() const {return rep_->projection();}

    // return the world coordinates of the center of the image 
    WorldCoords center() const {return rep_->center();}

    // note: if status is non-zero, the other methods are undefined
    int status() const {return rep_ ? rep_->status() : 1;}

    // return true if the WCS object has been initialized
    int initialized() const {return rep_ ? 1 : 0;}

    // return a pointer to the internal class
    WCSRep* rep() const {return rep_;}
};

#endif /* _WCS_h_ */

