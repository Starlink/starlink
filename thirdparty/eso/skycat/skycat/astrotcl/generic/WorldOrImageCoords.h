// -*-c++-*-
#ifndef _WorldOrImageCoords_h_
#define _WorldOrImageCoords_h_

/*
 * E.S.O. - VLT project 
 * $Id: WorldOrImageCoords.h,v 1.1.1.1 2006/01/12 16:43:59 abrighto Exp $
 *
 * WorldOrImageCoords.h - class representing either world (ra, dec, equinox)
 *                        or image (x, y) coordinates
 *                        (see also class WorldCoords, class ImageCoords)
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 97  Created
 */

#include "WorldCoords.h"
#include "ImageCoords.h"

/*
 * Class WorldOrImageCoords - This class is designed to be used in
 * situations where you might need to deal with either world coordinates
 * or image coordinates, but don't know ahead of time which.  We could
 * use a common base class and virtual methods, but that would require
 * using pointers or references, while it is often more convenient to use
 * instances of these classes. Also, the methods in both classes have
 * slightly different signatures.
 */
class WorldOrImageCoords {
protected:
    WorldCoords wc_;		// used for world coords
    ImageCoords ic_;		// used for image coords
    int isWcs_;			// flag: true if using world coords

public:

    // constructor - initialize null coords
    WorldOrImageCoords() : isWcs_(0) {}

    // constructor: initialize world coords
    WorldOrImageCoords(WorldCoords wc) 
	: wc_(wc), isWcs_(1) {}

    // constructor: initialize image coords
    WorldOrImageCoords(ImageCoords ic) 
	: ic_(ic), isWcs_(0) {}

    // return true if the coords are null
    int isNull() const {return isWcs_ ? wc_.isNull() : ic_.isNull();}

    // set to the null value
    void setNull() {if (isWcs_) wc_.setNull(); else ic_.setNull();}
   
    // output operator
    friend ostream& operator<<(ostream& os, const WorldOrImageCoords& pos) {
	if (pos.isWcs_) os << pos.wc_; else os << pos.ic_;
	return os;
    }

    // print coords to the given buffer in the given equinox 
    void print(char* x_buf, char* y_buf, double equinox = 2000., int hmsFlag=1) {
	if (isWcs_) 
	    wc_.print(x_buf, y_buf, equinox, hmsFlag);
	else
	    ic_.print(x_buf, y_buf);
    }
    
    // print coords to the given buffer in the given equinox (or system, such as "GALACTIC", "ECLIPTIC")
    void print(char* x_buf, char* y_buf, const char* equinoxStr, int hmsFlag=1) {
	if (isWcs_) 
	    wc_.print(x_buf, y_buf, equinoxStr, hmsFlag);
	else
	    ic_.print(x_buf, y_buf);
    }
    
    // print coords to the given stream in the given equinox 
    void print(ostream& os, double equinox = 2000.) {
	if (isWcs_) 
	    wc_.print(os, equinox);
	else
	    ic_.print(os);
    }

    // print coords to the given stream in the given equinox (or system, such as "GALACTIC", "ECLIPTIC")
    void print(ostream& os, const char* equinoxStr) {
	if (isWcs_) 
	    wc_.print(os, equinoxStr);
	else
	    ic_.print(os);
    }

    // get x and y
    void get(double& x, double& y, double equinox = 2000.) {
	if (isWcs_)
	    wc_.get(x, y, equinox);
	else
	    ic_.get(x, y);
    }
    
    // check for equality
    int operator==(const WorldOrImageCoords& pos) const {
	return isWcs_ ? (wc_ == pos.wc_) : (ic_ == pos.ic_);
    }
    int operator!=(const WorldOrImageCoords& pos) const {
	return isWcs_ ? (wc_ != pos.wc_) : (ic_ != pos.ic_);
    }

    // return the difference between 2 image coord points
    friend WorldOrImageCoords operator-(const WorldOrImageCoords& a, const WorldOrImageCoords& b) {
	if (a.isWcs_ && b.isWcs_)
	    return WorldOrImageCoords(a.wc_ - b.wc_);
	return WorldOrImageCoords(a.ic_ - b.ic_);
    }

    // short cuts

    // return ra and dec in degrees
    double ra_deg() const {return wc_.ra_deg();}
    double dec_deg() const {return wc_.dec_deg();}
    
    // return the internal WorldCoords or ImageCoords class
    WorldCoords& wc() {return wc_;}
    ImageCoords& ic() {return ic_;}
    const WorldCoords& wc() const {return wc_;}
    const ImageCoords& ic() const {return ic_;}

    // get distance between points
    double dist(WorldOrImageCoords& pos, double& pa) const {
	return isWcs_ ? wc_.dist(pos.wc_, pa) : ic_.dist(pos.ic_);
    }

    // get distance between points
    double dist(WorldOrImageCoords& pos) const {
	return isWcs_ ? wc_.dist(pos.wc_) : ic_.dist(pos.ic_);
    }

    // Given a radius, set pos1 and pos2 to the 2 endpoints that form a box
    // with center at this position.
    int box(double radius, WorldOrImageCoords& pos1, WorldOrImageCoords& pos2) const {
	return (pos1.isWcs_ = pos2.isWcs_ = isWcs_)
	    ? wc_.box(radius, pos1.wc_, pos2.wc_) 
	    : ic_.box(radius, pos1.ic_, pos2.ic_);
    }

    // Given the endpoints of a box (pos1, pos2), set width, height and radius 
    // and return the center position of the box.
    static WorldOrImageCoords center(const WorldOrImageCoords& pos1, 
				     const WorldOrImageCoords& pos2,
				     double& radius, 
				     double& width, double& height) {
	
	if (pos1.isWcs_ && pos2.isWcs_)
	    return WorldOrImageCoords(WorldCoords::center(pos1.wc_, pos2.wc_, radius, width, height));
	return WorldOrImageCoords(ImageCoords::center(pos1.ic_, pos2.ic_, radius, width, height));
    }

    // member access
    const HMS& ra() const {return wc_.ra();}
    const HMS& dec() const {return wc_.dec();}
    double x() const {return ic_.x();}
    double y() const {return ic_.y();}

    // return true if using WorldCoords
    double isWcs() const {return isWcs_;}

    int status() const {return isWcs_ ? wc_.status() : ic_.status();}
};



#endif /* _WorldOrImageCoords_h_ */
