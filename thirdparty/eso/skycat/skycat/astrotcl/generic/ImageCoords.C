/*
 * E.S.O. - VLT project/ESO Archive 
 * $Id: ImageCoords.C,v 1.1.1.1 2006/01/12 16:44:01 abrighto Exp $
 *
 * ImageCoords.C - method definitions for class ImageCoords
 * 
 * See the man page for a complete description.
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 */
static const char* const rcsId="@(#) $Id: ImageCoords.C,v 1.1.1.1 2006/01/12 16:44:01 abrighto Exp $";


#include <cstdio>
#include <cstring>
#include <cctype>
#include <cmath>
#include "error.h"
#include "ImageCoords.h"



/*
 * constructor  - takes x and y as strings
 */
ImageCoords::ImageCoords(const char* x_str, const char* y_str)
    : x_(IMAGE_COORD_NULL), y_(IMAGE_COORD_NULL), status_(0)
{
    if (sscanf(x_str, "%lf", &x_) != 1 || sscanf(y_str, "%lf", &y_) != 1)
	status_ = fmt_error("bad image coords: (%s, %s)", x_str, y_str);
}


/*
 * Print the coordinates in the given buffers
 */
void ImageCoords::print(char* x_buf, char* y_buf) 
{
    sprintf(x_buf, "%g", x_);
    sprintf(y_buf, "%g", y_);
}


/*
 * Print the coordinates to the given stream.
 */
void ImageCoords::print(ostream& os)
{
    os << *this;
}
    

/*
 * get x and y
 */
void ImageCoords::get(double& x, double& y)
{
    x = x_;
    y = y_;
}

   
/*
 * output operator: ("" for null coords)
 */
ostream& operator<<(ostream& os, const ImageCoords& pos)
{
    if (pos.isNull())
	os << "\"\"";
    else
	os << pos.x_ << " " << pos.y_;
    return os;
}


/*
 * return the distance between this position and the given one 
 * (in pixels)
 */
double ImageCoords::dist(ImageCoords& pos) const
{
    return dist(x_, y_, pos.x_, pos.y_);
}


/* 
 * static member to get the distance between 2 points in pixels
 */
double ImageCoords::dist(double x0, double y0, double x1, double y1)
{
    double x = fabs(x1 - x0);
    double y = fabs(y1 - y0);
    return sqrt(x*x + y*y);
}


/*
 * Given a radius in pixels, set pos1 and pos2 to the 2 endpoints that form a box
 * with center at this position.
 */
int ImageCoords::box(double radius, ImageCoords& pos1, ImageCoords& pos2) const
{
    double w = sqrt((radius * radius)/2.);
    double x0 = x_ - w;
    double y0 = y_ - w;
    double x1 = x_ + w;
    double y1 = y_ + w;
	
    pos1 = ImageCoords(x0, y0);
    pos2 = ImageCoords(x1, y1);

    return 0;
}


/*
 * Given the endpoints of a box (pos1, pos2), set width, height and radius in 
 * pixels, and return the position at the center of the box
 */
ImageCoords ImageCoords::center(const ImageCoords& pos1, const ImageCoords& pos2, 
	       double& radius, double& width, double& height) 
{
    ImageCoords result;
    if (pos1.status() || pos2.status()) {
	error("invalid image position argument");
	return result;
    }

    // get center pos
    double x1 = pos1.x_, y1 = pos1.y_;
    double x2 = pos2.x_, y2 = pos2.y_;
    double x = (x1 + x2)/2.0;
    double y = (y1 + y2)/2.0;
    result = ImageCoords(x, y); 
    
    // get width and height of box
    width = dist(x1, y1, x2, y1);
    height = dist(x1, y1, x1, y2);

    // radius is half the distance from pos1 to pos2 
    radius = dist(x1, y1, x2, y2)/2.;
    
    return result;
}



