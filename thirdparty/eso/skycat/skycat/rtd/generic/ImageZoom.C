/*
 * E.S.O. - VLT project 
 * "@(#) $Id: ImageZoom.C,v 1.1.1.1 2006/01/12 16:39:16 abrighto Exp $"
 *
 * ImageZoom.C - member routines for class ImageZoom,
 *               implementation of Zoom window for RtdImage
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 */
static const char* const rcsId="@(#) $Id: ImageZoom.C,v 1.1.1.1 2006/01/12 16:39:16 abrighto Exp $";



#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <cstring>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "define.h"
#include "ImageDisplay.h"
#include "ImageZoom.h"


/* 
 * constructor: create the zoom GC and XImage. 
 *
 * Args:
 *
 *  tkwin         - the Tk window for displaying the zoomed image
 *
 *  copyGC        - X GC used to copy image to zoom window
 *
 *  width, height - dimensions of tkwin (should be square and a multiple
 *                  of the zoom factor)
 *
 *  zoomFactor    - the zoom magnification factor
 *
 *  usingXShm     - flag: true if using X shared memory
 *
 *  verbose       - flag: if true, print diagnostic messages
 *  
 */
ImageZoom::ImageZoom(Tk_Window tkwin, GC gc, int width, int height, 
		     int zoomFactor, int usingXShm, int verbose)
    : tkwin_(tkwin),
      gc_(gc),
      width_(width),
      height_(height),
      zoomFactor_(zoomFactor),
      zoomStep_(width/zoomFactor),
      status_(TCL_OK)
{
    // make a graphics context using XOR to draw a box marking the pointer hotspot
    XGCValues gcValues;
    gcValues.function = GXcopy;
    gcValues.graphics_exposures = False;
    Tk_MakeWindowExist(tkwin_);
    rect_gc_ = XCreateGC(Tk_Display(tkwin_), Tk_WindowId(tkwin_),
			 GCFunction|GCGraphicsExposures, 
			 &gcValues);
    // create a class object to manage the XImage
    xImage_ = new ImageDisplay(Tk_Display(tkwin_), Tk_Visual(tkwin_), gc, 
			       Tk_Depth(tkwin_), usingXShm, verbose);
    status_ = xImage_->update(width, height);
}

 
ImageZoom::~ImageZoom()
{
    if (xImage_) 
	delete xImage_;
}




/*
 * Called for motion events in image window when zooming is on.
 * 
 * Args: 
 * 
 *   data - pointer to data being displayed
 *   x, y - coords in displayed image (XImage coords)
 *   w, h - width (bytesPerLine) and height of displayed image
 *   xs, ys - x and y magnification factors
 *   color0 - color to use for blank areas with no image data
 */
void ImageZoom::zoom(unsigned char* data, int x, int y, int w, int h, 
		     int xs, int ys, unsigned long color0)
{
    if (status_ != TCL_OK)
	return;

    char pixval = color0;
    register unsigned char* zoomData = xImage_->data();
    int zs = zoomStep_ >> 1;
    int incr = width_ * (zoomFactor_ - 1);
    int xz = x - zs;
    int yz = y - zs;
    int i, j, k, l;

    for (i=0; i<zoomStep_; i++) {
	for (j=0; j<zoomStep_; j++) {
	    if ((xz + j) < 0 || (xz+j) >= w || (yz + i) < 0  || (yz + i) >= h) {
		pixval = color0;
	    }
	    else {
		pixval = data[(yz+i)*w + xz + j];
	    }
	    for (k=0; k<zoomFactor_; k++) {
		for (l=0; l<zoomFactor_; l++) {
		    zoomData[l*width_] = pixval; 
		}
		zoomData++;
	    }
	}
	zoomData += incr;
    }
    
    // send to X server
    xImage_->put(Tk_WindowId(tkwin_), 0, 0, Tk_X(tkwin_), Tk_Y(tkwin_), width_, height_);

    /* draw 2 rectangles around the center pixel(s) */
    int size = zoomFactor_;
    int x0 = width_/2 - size/2;
    int y0 = height_/2 - size/2;

    Screen* screen = Tk_Screen(tkwin_);
    
    XSetForeground(Tk_Display(tkwin_), rect_gc_, WhitePixelOfScreen(screen));
    XSetBackground(Tk_Display(tkwin_), rect_gc_, BlackPixelOfScreen(screen));
    XDrawRectangle(Tk_Display(tkwin_),
		   Tk_WindowId(tkwin_),
		   rect_gc_,
		   x0,
		   y0,
		   size,
		   size);

    XSetForeground(Tk_Display(tkwin_), rect_gc_, BlackPixelOfScreen(screen));
    XSetBackground(Tk_Display(tkwin_), rect_gc_, WhitePixelOfScreen(screen));
    XDrawRectangle(Tk_Display(tkwin_),
		   Tk_WindowId(tkwin_),
		   rect_gc_,
		   x0-1,
		   y0-1,
		   size+2,
		   size+2);
}

