// -*-c++-*-
#ifndef _TkImage_H_
#define _TkImage_H_
/*
 * E.S.O. - VLT project 
 * "@(#) $Id: TkImage.h,v 1.1.1.1 2006/01/12 16:41:06 abrighto Exp $" 
 *
 * TkImage.h - base class for Tk images implemented in C++.
 * 
 * This class adds some features to the TclCommand class for 
 * implementing Tk image types.
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 *                 10/03/98  Pass config options to constructor as pointer
 *                           instead of reference.
 */

#include <tk.h>
#include "TclCommand.h"

/* 
 * This class or a class derived from it is used to hold the values
 * modified by the configure image command
 */
class TkImageOptions {
public:
};


/* 
 * This is the base class for classes defining tk images from
 * C++ classes.  
 * In this class, only one instance of an image may be displayed,
 * in a window, however multiple views of the image with varying 
 * sizes may be displayed (I didn't see any need for exact copies
 * of the same image in the same size...)
 */
class TkImage : public TclCommand {
protected:
    Tk_ImageMaster master_;	// passed from the Tk imaging routines

    TkImageOptions* optionsPtr_; // ptr to class or struct holding option values
    Tk_ConfigSpec* configSpecsPtr_; // ptr to configSpecs array for Tk options
    int refCount_;		// Number of instances that share this
				// image (may only be one here). 

    Tk_Window tkwin_;		// Window in which the image will be displayed. 
    Display *display_;		// X token for the window's display
    Visual *visual_;		// X visual for window
    Screen *screen_;		// X screen for window
    GC gc_;			// Graphics context for copying to screen  
    Pixmap pm_;			// Pixmap for storing the image 

    int width_;			// total width of image
    int height_;		// total height of image
    int pixw_;			// width of X pixmap, if used
    int pixh_;			// height of X pixmap, if used
    int depth_;			// depth of screen

    char* pclass_;		// if specified, restrict images to only be displayed
				// in widgets of that class

    int update_pending_;	// flag: true if image needs to be updated

    int initialized_;		// flag: true after image has been initialized
				// (configured, after constructor is done)
    
    // -- member functions --

    // do first time image configuration 
    virtual int initImage(int argc, char* argv[]);

    // configure the image with cmd line options.
    virtual int configureImage(int argc, char* argv[], int flags = 0);
    
    // these are called indirectly by the Tk imaging routines
    virtual TkImage* getImage(Tk_Window);
    virtual void displayImage(Drawable, int imageX, int imageY,
			      int width, int height,
			      int drawableX, int drawableY) = 0;

    // update the image display eventually
    virtual void imageChanged();

    // make the X graphics context
    void makeGC();
    
    // set the image size and create/update a pixmap, if use_pixmap is 1
    int setImageSize(int width, int height, int use_pixmap, int pixw, int pixh);

    // utility method: equivalent of Tk "update idletasks" command
    void updateIdleTasks();

    // call a member function by name
    virtual int call(const char* name, int len, int argc, char* argv[]);

public:

    // constructor
    TkImage(Tcl_Interp* interp, const char* cmdname, const char* instname,
		 Tk_ConfigSpec* specs, TkImageOptions* options, 
		 Tk_ImageMaster master, char* pclass = NULL);

    // destructor
    virtual ~TkImage();

    // the following static methods are called by the Tk image handling routines
    static ClientData GetImage(Tk_Window, ClientData);
    static void DisplayImage(ClientData, Display*, Drawable,
			     int imageX, int imageY, 
			     int width, int height,
			     int drawableX, int drawableY);
    static void FreeImage(ClientData, Display*);
    static void DeleteImage(ClientData);

    // implement the configure Tk command    
    virtual int configureCmd(int argc, char* argv[]);

    // called for the cget image command
    virtual int cgetCmd(int argc, char* argv[]);

    // delete tcl subcommand 
    // (should probably redefine to use "image delete $image")
    virtual int deleteCmd(int argc, char* argv[]);

    // set options pointer 
    void setOptionsPtr(char * ptr) {optionsPtr_ = (TkImageOptions *)ptr;}

};


#endif /* _TkImage_H_ */


