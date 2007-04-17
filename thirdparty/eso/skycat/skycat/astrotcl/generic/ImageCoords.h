// -*-c++-*-
#ifndef _ImageCoords_h_
#define _ImageCoords_h_

/*
 * E.S.O. - VLT project 
 * $Id: ImageCoords.h,v 1.1.1.1 2006/01/12 16:44:01 abrighto Exp $
 *
 * ImageCoords.h - class representing image coordinates (x, y)
 *                 (see also class WorldCoords)
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 97  Created
 * pbiereic        17/02/03   Added 'using namespace std'. Removed ::std specs.
 */

using namespace std;
#include <cstdio>
#include <iostream>
#include <cmath>

const double IMAGE_COORD_NULL = HUGE_VAL;	// null value for a double

/*
 * Class ImageCoords
 */
class ImageCoords {
protected:
    double x_, y_;		// coordinates of a pixel
    int status_;		// status for errors in constructor

public:

    // constructor - initialize null coords
    ImageCoords() 
	: x_(IMAGE_COORD_NULL), y_(IMAGE_COORD_NULL), status_(0) {}

    // constructor
    ImageCoords(double x, double y) 
	: x_(x), y_(y), status_(0) {}

    // constructor - parse X and Y in string format
    ImageCoords(const char* x_str, const char* y_str);

    // return true if the coords are null
    int isNull() const {return x_ == IMAGE_COORD_NULL ||  y_ == IMAGE_COORD_NULL;}

    // set to the null value
    void setNull() {x_ = IMAGE_COORD_NULL; y_ = IMAGE_COORD_NULL;}
   
    // output operator: format: "x y"
    friend ostream& operator<<(ostream&, const ImageCoords& pos);

    // print coords to the given buffer
    void print(char* x_buf, char* y_buf);
    
    // print coords to the given stream
    void print(ostream& os);

    // get x and y
    void get(double& x, double& y);
    
    // check for equality
    int operator==(const ImageCoords& pos) const {
	return x_ == pos.x_ && y_ == pos.y_;
    }
    int operator!=(const ImageCoords& pos) const {
	return x_ != pos.x_ || y_ != pos.y_;
    }

    // return the difference between 2 image coord points
    friend ImageCoords operator-(const ImageCoords& a, const ImageCoords& b) {
	return ImageCoords(a.x_ - b.x_, a.y_ - b.y_);
    }

    // short cuts
    
    // return x and y
    double x() const {return x_;}
    double y() const {return y_;}

    // get distance between points
    double dist(ImageCoords& pos) const;

    // static member to get the distance between 2 points
    static double dist(double x0, double y0, double x1, double y1);

    // Given a radius, set pos1 and pos2 to the 2 endpoints that form a box
    // with center at this position.
    int box(double radius, ImageCoords& pos1, ImageCoords& pos2) const;

    // Given the endpoints of a box (pos1, pos2), set width, height and radius 
    // and return the center position of the box.
    static ImageCoords center(const ImageCoords& pos1, const ImageCoords& pos2, 
			      double& radius, double& width, double& height);

    // member access
    int status() const {return status_;}
};



#endif /* _ImageCoords_h_ */
