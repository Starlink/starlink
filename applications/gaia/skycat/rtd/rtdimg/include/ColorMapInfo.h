// -*-c++-*-
#ifndef _ColorMapInfo_h_
#define _ColorMapInfo_h_

/*
 * E.S.O. - VLT project 
 * "@(#) $Id: ColorMapInfo.h,v 1.4 2005/02/02 01:43:03 brighton Exp $" 
 *
 * ColorMapInfo.h - class definitions for reading in color map files
 *
 * See the man page for a complete description.
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * pbiereic        17/02/03  Added 'using namespace std'. Removed ::std specs.
 */

using namespace std;
#include <sys/types.h>
#include <iostream>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

/* color management */
enum {MAX_COLOR=256};            /* work with 8bit color */



// RGB color info
struct RGBColor {
    float red, green, blue;
    
    // read an RGB entry
    friend istream& operator>>(istream& is, RGBColor& rgb) {
        is >> rgb.red >> rgb.green >> rgb.blue;
	return is;
    }
};


// one of these is used to hold colormap info for each colormap
// file read
class ColorMapInfo {
private:
    char* name_;		// filename
    RGBColor* rgb_;	// array of RGB values
    ColorMapInfo* next_;	// pointer to next colormap

    // copy constructor (not defined)
    ColorMapInfo(const ColorMapInfo&); 

public:
    // constructor - arguments are the name of the colormap
    // and an array of RGB color values. Both are assumed to
    // have been allocated.
    ColorMapInfo(char* name, RGBColor* rgb);

    // destructor
    ~ColorMapInfo();

    // create and return ColorMap from a file description
    static ColorMapInfo* get(char* filename);

    // write a list of colormap files
    static void list(ostream&);

    // member access
    const char* name() const {return name_;}
    ColorMapInfo* next() {return next_;}

    // set the red, green and blue values from the colormap data
    // and interpolate based on the count of available colors
    void interpolate(XColor* colorCells, int colorCount);

    // rotate the colormap by the given amount
    void rotate(int amount, XColor* src, XColor* dest, int colorCount);

    // shift the colormap by the given amount
    void shift(int amount, XColor* src, XColor* dest, int colorCount);
};

#endif /* _ColorMapInfo_h_ */
