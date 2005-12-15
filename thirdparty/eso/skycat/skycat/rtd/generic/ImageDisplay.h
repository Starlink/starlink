// -*-c++-*-
#ifndef _ImageDisplay_H_
#define _ImageDisplay_H_
/*
 * E.S.O. - VLT project 
 * "@(#) $Id: ImageDisplay.h,v 1.2 2005/02/02 01:43:03 brighton Exp $" 
 *
 * ImageDisplay.h - class managing XImage to Pixmap display including
 *                  optional X shared memory extension usage
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * T. Herlin       06/12/95  Casted NULL return from  ImageDisplay::data
 * P.W. Draper     04/03/98  Added putpixel member.
 */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <X11/extensions/XShm.h>

class ImageDisplay {
protected:
    XImage* xImage_;		// XImage for the basic image 
    Display *display_;		// X token for the window's display
    Visual *visual_;		// X visual for window
    GC gc_;			// Graphics context for copying to screen 
    int depth_;			// bits per pixel
    int bytesPerPixel_;		// bytes per pixel

    // note: it might be that we can use XShm only on smaller images due to
    // system limits on shared memory usage (on SunOS, for example)
    int useXShm_;		// flag: true if we WANT to use X shared memory
    int usingXShm_;		// flag: true if we ARE using X shared memory
    
    XShmSegmentInfo shmInfo_;   // X shared memory Segment info for xImage_

    int verbose_;		// flag: if true, print diagnostic messages


    // destroy the XImage and free any shared memory
    void destroyXImage();

    // try to create an XImage in shared memory
    int updateShm(int width, int height);

public:

    // constructor
    ImageDisplay(Display *display, Visual *visual, GC gc, 
		 int depth, int useXShm, int verbose);

    // destructor
    ~ImageDisplay();

    // create or update an XImage with the given size
    int update(int width, int height);
    
    // copy the XImage to a Drawable in the X Server
    void put(Drawable, int src_x, int src_y, int dest_x, int dest_y, int width, int height);

    // return a pointer to the XImage data
    unsigned char* data() {
	return xImage_ ? (unsigned char*)xImage_->data : (unsigned char*)NULL;
    }

    // clear out the image by setting all pixels to the given value
    void clear(unsigned long val);

    // Assign a value to a pixel (This is the "safe" method for non-byte XImages)
    void putpixel(int x, int y, unsigned long value) {
	// assert(xImage_ != NULL);
	XPutPixel(xImage_, x, y, value);
    }

    // flush X output buffer
    void flushX();

    // other info
    int width() {return xImage_ ? xImage_->width : 0;}
    int height() {return xImage_ ? xImage_->height : 0;}
    int bitmapPad() {return xImage_ ? xImage_->bitmap_pad : 0;}
    int bytesPerLine() {return xImage_ ? xImage_->bytes_per_line : 0;}
    int depth() {return depth_;}
    int bitsPerPixel() {return bytesPerPixel_*8;}
    int bytesPerPixel() {return bytesPerPixel_;}

    // return true if we are really using X shared memory
    int usingXShm() {return usingXShm_;} 

};


#endif /* _ImageDisplay_H_ */


