// -*-c++-*-
#ifndef _ImageZoom_h_
#define _ImageZoom_h_

/*
 * E.S.O. - VLT project 
 * "@(#) $Id: ImageZoom.h,v 1.1.1.1 2006/01/12 16:39:16 abrighto Exp $" 
 *
 * ImageZoom.h - class definitions for RtdImage Zoom Window
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 */

#include <tk.h>

/*
 * Class ImageZoom
 * 
 * This class implements the Zoom window for the RtdImage class
 */
class ImageZoom {
protected:
    Tk_Window  tkwin_;		// zoom window
    GC gc_;			// graphics context for copying pixels
    GC rect_gc_;		// graphics context for drawing box aroung center pixels
    int width_;			// width of displayed image
    int height_;		// height of displayed image
    int zoomFactor_;		// zoom factor (1...n)
    int zoomStep_;		// value used to calculate zoom = width/factor
    ImageDisplay *xImage_;	// class object for zoom window's X image 
    int status_;		// return value from constructor

public:
    // constructor: initialize the zoom window
    ImageZoom(Tk_Window tkwin, GC copyGC, int width, int height, int factor,
	      int usingXShm, int verbose);
    
    // destructor: clean up resources
    ~ImageZoom();

    // called for motion events in the image to do the zooming
    void zoom(unsigned char* data, int x, int y, int w, int h, 
	      int xs, int ys, unsigned long color0);

    // return status after constructor for error checking
    int status() {return status_;}
};



#endif /* _ImageZoom_h_ */
