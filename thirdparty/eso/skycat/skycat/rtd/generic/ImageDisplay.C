/*
 * E.S.O. - VLT project 
 * "@(#) $Id: ImageDisplay.C,v 1.1.1.1 2006/01/12 16:39:29 abrighto Exp $"
 *
 * ImageDisplay.C - member routines for class ImageDisplay,
 *                  for managing XImage to Pixmap display including
 *                  optional X shared memory extension usage
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * Peter W. Draper 04/03/98  Added putpixel member. Fixed allocation
 *                           of data to bytes_per_line*width when not
 *                           using shared memory. 
 *                 18/06/03  Some changes to attempt cleanup when
 *                           shared memory cannot be attached (Solaris
 *                           limit is 6 segments!).
 */
static const char* const rcsId="@(#) $Id: ImageDisplay.C,v 1.1.1.1 2006/01/12 16:39:29 abrighto Exp $";

// #define DEBUG
// #define XXXDEBUG

#include <cstdlib>
#include <cctype>
#include <cassert>
#include <iostream>
#include <cstring>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "define.h"
#include "error.h"
#include "ErrorHandler.h"
#include "ImageDisplay.h"

#ifdef NEED_SHM_PROTO
// missing prototypes (on SunOS at least)
/*extern "C" {
    // should be in sys/shm.h
    void *shmat(int shmid, const void* shmaddr, int shmflg);
    int shmdt(const void* shmaddr);
    int shmget(key_t, size_t, int);
    int shmctl(int shmid, int cmd, shmid_ds *buf);
}*/
#endif


/* 
 * constructor - create an object to manage the XImage
 * 
 * args: 
 *   display, visual, gc - standard X env
 *   width, height, depth - dimensions of image
 *   useXShm - flag: true if we should try to use X Shared Memory
 *   verbose - flag: if true, print diagnostic messages
 */
ImageDisplay::ImageDisplay(Display *display, Visual *visual, GC gc, 
			   int depth, int useXShm, int verbose) 
    : xImage_(NULL),
      display_(display),
      visual_(visual),
      gc_(gc),
      depth_(depth),
      bytesPerPixel_(depth/8),
      useXShm_(useXShm),
      usingXShm_(0),
      verbose_(verbose)
{
    if (depth_ == 24)
	bytesPerPixel_ = 4;
}


/* 
 * Destructor - clean up XImage
 */
ImageDisplay::~ImageDisplay() 
{
    destroyXImage();
}


/* 
 * destroy the XImage and free any allocated shared memory
 * if necessary
 */
void ImageDisplay::destroyXImage() 
{
    if (xImage_) {
	if (usingXShm_) {
	    XShmDetach(display_, &shmInfo_);
	    XDestroyImage(xImage_);
	    shmdt(shmInfo_.shmaddr);
	}
	else {
	    XDestroyImage(xImage_);
	}
	xImage_ = NULL;
    }
}

/* 
 * flush X output buffer and wait until all requests were processed
 * by the X server.
 */
void ImageDisplay::flushX()
{
    if (! xImage_)
	return;
    XSync(display_, False);
}

/* 
 * do an XPutImage or XShmPutImage, depending on the current flags
 */
void ImageDisplay::put(Drawable d, int src_x, int src_y, int dest_x, int dest_y, 
		       int width, int height)
{
    if (! xImage_)
	return;

    // make sure arguments are in range
    if (src_x < 0) 
	src_x = 0;
    if (src_y < 0) 
	src_y = 0;;
	
    width = min(width, xImage_->width - src_x);
    height = min(height, xImage_->height - src_y);
    if (width <= 0 || height <= 0)
	return;
 
    if (usingXShm_) {
	XShmPutImage(display_, d, gc_, xImage_, src_x, src_y, dest_x, dest_y, 
		     width, height, False /*True = send event*/);
    } 
    else {
	XPutImage(display_, d, gc_, xImage_, src_x, src_y, dest_x, dest_y, 
		  width, height);
    }
    return;
}


/*
 * create or update the XImage using X shared memory 
 * so that it has the given width and height and return TCL_OK if all
 * is TCL_OK.
 */
int ImageDisplay::updateShm(int width, int height)
{

    // use this to catch X errors
    ErrorHandler errorHandler(display_, verbose_);

    // create an XImage in shared memory
    xImage_ = XShmCreateImage(display_, visual_, depth_, 
			      ZPixmap, NULL, &shmInfo_, width, height);
    if (!xImage_) {
#ifdef DEBUG
	if (verbose_)
	    cout << "XShmCreateImage failed\n";
#endif
	return TCL_ERROR;
    }

    // allocate enough shared memory to hold the image
    // (plus a fudge factor to keep the X server on HP happy)
    shmInfo_.shmid = shmget(IPC_PRIVATE, 
			    (xImage_->bytes_per_line * (height+1)),
			    IPC_CREAT | 0777);

    if (shmInfo_.shmid < 0) {
	XDestroyImage(xImage_);
	xImage_ = NULL;
#ifdef DEBUG
	if (verbose_) {
	    perror("shmget failed for X shared memory");
	}
#endif
	return TCL_ERROR;
    }
    shmInfo_.shmaddr = (char *) shmat(shmInfo_.shmid, 0, 0);
    if (shmInfo_.shmaddr == ((char *) -1)) {
	XDestroyImage(xImage_);
        shmctl(shmInfo_.shmid, IPC_RMID, 0);
        shmdt(shmInfo_.shmaddr);
	xImage_ = NULL;
#ifdef DEBUG
	if (verbose_)
	    perror("shmat failed for X shared memory");
#endif
	return TCL_ERROR;
    }

    xImage_->data = shmInfo_.shmaddr;
    shmInfo_.readOnly = False;
    XShmAttach(display_, &shmInfo_);

    // check for X errors
    if (errorHandler.errors()) {
        XShmDetach(display_, &shmInfo_);
        XDestroyImage(xImage_);
        shmctl(shmInfo_.shmid, IPC_RMID, 0);
        shmdt(shmInfo_.shmaddr);
	xImage_ = NULL;
#ifdef DEBUG
	if (verbose_)
	    cout << "X shared memory error\n";
#endif
	return TCL_ERROR;
    }

    // this will cause the shared memory to be automatically deleted
    shmctl(shmInfo_.shmid, IPC_RMID, 0);

#ifdef XXXDEBUG
    if (verbose_)
	cout << "Sharing memory\n";
#endif
    return TCL_OK;
}


/*
 * create or update the XImage so that it has the given width and height
 */
int ImageDisplay::update(int width, int height)
{
    if (xImage_) {
	// reuse old XImage if it has the same dimensions
	if (xImage_->width == width && xImage_->height == height) {
	    return TCL_OK; 
	}
	destroyXImage();
	xImage_ = NULL;
    } 
     
    if (useXShm_) {
	// try to create a shared memory XImage
	if (updateShm(width, height) == TCL_OK) {
	    usingXShm_ = 1;
	    return TCL_OK;
	}
	usingXShm_ = 0;
#ifdef DEBUG
	if (verbose_)
	    cout << "Couldn't create X shared memory: reverting to standard X Image\n";
#endif
    }
     
    // fallback: create a normal XImage
    xImage_ = XCreateImage(display_, visual_, depth_,
			   ZPixmap, 0, (char *) NULL, width, height,
			   BitmapPad(display_), 0); 

    // now allocate the image data (which must use the appropriate padding).
    xImage_->data = (char *)malloc(xImage_->bytes_per_line * height);
    if (xImage_->data == NULL) {
#ifdef DEBUG
	if (verbose_) 
	    cout << "out of memory for XImage\n";
#endif

	XDestroyImage(xImage_);
	return error("not enough memory for an image this size");
    }
     
#ifdef XXXDEBUG
    if (verbose_)
	cout << "Not Sharing memory\n";
#endif
    return TCL_OK;
}
     

/*
 * clear out the image by setting all pixels to the given value
 */
void ImageDisplay::clear(unsigned long val)
{
    if (!xImage_)
	return;
    
    // byte data
    if (depth_ == 8) {
	memset(xImage_->data, val, xImage_->bytes_per_line * xImage_->height);
    }
    else {
	//  XImage has depth greater than a byte
	for (int i=0; i<xImage_->width; i++) {
	    for (int j=0; j<xImage_->height; j++) {
		XPutPixel(xImage_, i, j, val);
	    }
	}
    }
}
