/*
 * E.S.O. - VLT project 
 * "@(#) $Id: TkImage.C,v 1.5 1998/03/12 20:19:41 abrighto Exp $"
 *
 * TkImage.C - base class definitions for Tk images implemented in C++
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * Peter W. Draper 13/10/97  Added makeGC call to constructor.
 * Allan Brighton  10/03/98  Pass config options to constructor as pointer
 *                           instead of reference.
 */
static const char* const rcsId="@(#) $Id: TkImage.C,v 1.5 1998/03/12 20:19:41 abrighto Exp $";



#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <iostream.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "define.h"
#include "error.h"
#include "ErrorHandler.h"
#include "TkImage.h"


/*
 * Constructor - create the image and install the tcl command of the
 * same name.
 *
 * interp - is the Tk interpreter
 * 
 * cmdname - is the name of the tcl command that created this image
 *
 * instname - is the name of this image (and tcl command)
 * 
 * specs - is a pointer to the image's config struct, which defines
 *         the config options for the image
 * 
 * options - is a derived struct that holds the values defined in specs.
 *
 * master - is passed from the Tk imaging routines
 *
 * pclass - if specified, makes sure that the image window has the given
 *          Tk class
 */
TkImage::TkImage(Tcl_Interp* interp, const char* cmdname, const char* instname,
		 Tk_ConfigSpec* specs, TkImageOptions* options, 
		 Tk_ImageMaster master, char* pclass)
: TclCommand(interp, cmdname, instname),
  optionsPtr_(options),
  configSpecsPtr_(specs),
  master_(master),
  tkwin_(Tk_MainWindow(interp)),
  display_(Tk_Display(tkwin_)),
  visual_(Tk_Visual(tkwin_)),
  screen_(Tk_Screen(tkwin_)),
  gc_(None),
  pm_(None),
  width_(1),
  height_(1),
  pixw_(1),
  pixh_(1),
  depth_(Tk_Depth(tkwin_)),
  refCount_(0),
  pclass_(pclass),
  initialized_(0),
  update_pending_(0)
{
    //  Allan: Modification by P.W. Draper (advised by Dave Terrett).
    makeGC();
}


/* 
 * Dxestructor - called when tcl command is deleted
 */
TkImage::~TkImage() 
{
    if (gc_ != None) {	
	Tk_FreeGC(display_, gc_);
    }
    if (pm_ != None) {
       XFreePixmap (display_, pm_);
    }

    Tk_FreeOptions(configSpecsPtr_, (char *) optionsPtr_, display_, 0);
}


/*
 * create the X graphics context for copying the image to the screen
 */
void TkImage::makeGC()
{
    XColor* white = Tk_GetColor(interp_, tkwin_, "white");
    XColor* black = Tk_GetColor(interp_, tkwin_, "black");
    XGCValues gcValues;
    gcValues.foreground = (white != NULL)? white->pixel:
	WhitePixelOfScreen(screen_);
    gcValues.background = (black != NULL)? black->pixel:
	BlackPixelOfScreen(screen_);
    gcValues.graphics_exposures = False;
    gc_ = Tk_GetGC(tkwin_, GCForeground|GCBackground|GCGraphicsExposures, &gcValues);
}


/*
 * set the image dimensions to the given width and height and
 * if use_pixmap is 1, create or update a pixmap to have the same
 * dimensions 
 */
int TkImage::setImageSize(int width, int height, int use_pixmap, int pixw, int pixh)
{
    width_ = width; 
    height_ = height; 
    if (use_pixmap) {
	if (pm_ == None || pixw_ != pixw || pixh_ != pixh) {
	    
	    // delete previous pixmap
	    if (pm_ != None) {
		XFreePixmap(display_, pm_);
		pm_ = None;
	    }
	    
	    // catch errors when image is too large
	    ErrorHandler errorHandler(display_);

	    pm_ = XCreatePixmap(display_, 
				RootWindowOfScreen(screen_), 
				pixw_ = pixw, 
				pixh_ = pixh, 
				depth_);

	    // check for errors
	    if (pm_ == None || errorHandler.errors()) {
		if (pm_ != None) {
		    XFreePixmap(display_, pm_);
		    pm_ = None;
		}
 		error("Can't create pixmap large enough to hold image");
		Tk_BackgroundError(interp_);
		return TCL_ERROR;
	    }
	} 
    } 
    else {
	if (pm_ != None) {
	    XFreePixmap(display_, pm_);
	    pm_ = None;
	}
    }

    return TCL_OK;
}


/* 
 * This static method is called by the Tk image handling routines
 * for each use of the image in a widget.
 */
ClientData TkImage::GetImage(Tk_Window tkwin, ClientData clientData)
{
    TkImage* thisPtr = (TkImage*)clientData;
    return (ClientData)thisPtr->getImage(tkwin);
}


/* 
 * This virtual method is called indirectly by the Tk image handling routines
 * for each use of the image in a widget.
 */
TkImage* TkImage::getImage(Tk_Window tkwin)
{
    // only allow one instance (use views instead of instances, since
    // all instances must be the same size, which is not what we need here)
    if (refCount_) {
	error("Only one instance of this image type is allowed");
	Tk_BackgroundError(interp_);
	return NULL;
    }

    // if pclass was specified, check that the parent window has that class
    if (pclass_ != NULL) {
	if (strcmp(Tk_Class(tkwin), pclass_) != 0) {
	    error("This image type should only be used in a ", pclass_);
	    Tk_BackgroundError(interp_);
	    return NULL;
	}
    }

    refCount_++;

    // Make a new instance of the image.
    tkwin_ = tkwin;
    display_ = Tk_Display(tkwin_);
    visual_ = Tk_Visual(tkwin_);
    depth_ = 8;

    // make the graphics context now that we have the window
    Tk_MakeWindowExist(tkwin_);	// allan: 14.2.96: fix problem reported by 
				// David Terret, when window is not mapped yet
    makeGC();

    Tk_ImageChanged(master_, 0, 0, 0, 0, width_, height_);
    return this;
}


/* 
 * This static method is invoked by the Tk image handling routines 
 * to draw a the image.
 */
void TkImage::DisplayImage(
    ClientData clientData,	// pointer to class instance
    Display *display,		// Display on which to draw image. 
    Drawable drawable,		// Pixmap or window in which to draw image. 
    int imageX, int imageY,	// Upper-left corner of region within image to draw. 
    int width, int height,	// Dimensions of region within image to draw. 
    int drawableX, int drawableY) // Coordinates within drawable that
{
    TkImage* thisPtr = (TkImage*)clientData;
    thisPtr->displayImage(drawable, imageX, imageY, width, height, drawableX, drawableY);
}


/* 
 * This static method is called by the Tk image handling routines
 * when a widget ceases to use a particular instance of an image. 
 * We don't actually get rid of the instance until later because we 
 * may be about	to get this instance again.
 */
void TkImage::FreeImage(ClientData clientData, Display *display)
{
    // TkImage* thisPtr = (TkImage*)clientData;
    // XXX add a do-when-idle call to delete "thisPtr" ?
}



/*
 * This method should be called when the image has changed and should be
 * redrawn eventually
 */
void TkImage::imageChanged()
{
    Tk_ImageChanged(master_, 0, 0, width_, height_, width_, height_);
    update_pending_++;
}


/* 
 * This static method is invoked by Tk_EventuallyFree to clean up 
 * the internal structure of the image at a safe time
 */
void TkImage::DeleteImage(ClientData clientData)
{
    TkImage *thisPtr = (TkImage *)clientData;

    // deleting the tcl command will result in the virtual 
    // destructors being called, which should do the cleanup
    Tcl_DeleteCommand(thisPtr->interp_, thisPtr->instname_);
}


/*
 * This procedure needs to be called from the derived class constructor
 * to complete the image initialization. This can't be done in the 
 * constructor here since the options_ struct wouldn't be initialized yet.
 */
int TkImage::initImage(int argc, char* argv[])
{
    // handle the config options
    if ((status_ = configureImage(argc, argv, 0)) != TCL_OK) {
	// XXX add some clean up code here ...
	return TCL_ERROR;
    }

    // return the name of the image as a tcl result
    Tcl_SetResult(interp_, instname_, TCL_STATIC);
    initialized_ = 1;
    return TCL_OK;
}


/* 
 * utility method: equivalent of Tk "update idletasks" command,
 * process all pending display events.
 */
void TkImage::updateIdleTasks()
{
    while (1) {
	while (Tk_DoOneEvent(TK_IDLE_EVENTS) != 0) {
	    /* Empty loop body */
	}
	XSync(display_, False);
	if (Tk_DoOneEvent(TK_IDLE_EVENTS) == 0) {
	    break;
	}
    }
} 


/*
 * This procedure is called to process an argv/argc list, plus
 * the Tk option database, in order to configure (or reconfigure) 
 * a image.
 */
int TkImage::configureImage(int argc, char* argv[], int flags)
{
    if (Tk_ConfigureWidget(interp_, tkwin_, 
			  configSpecsPtr_, argc, argv, 
			  (char*)optionsPtr_, flags) != TCL_OK) {
	return TCL_ERROR;
    }

    return TCL_OK;
}


/*
 * this is called for the configure image command
 */
int TkImage::configureCmd(int argc, char* argv[])
{
    if (argc == 0) {
	return Tk_ConfigureInfo(interp_, tkwin_, configSpecsPtr_, (char*)optionsPtr_, 
				NULL, 0);
    } else if (argc == 1) {
	return Tk_ConfigureInfo(interp_, tkwin_, configSpecsPtr_, (char*)optionsPtr_, 
				argv[0], 0);
    } else {
	return configureImage(argc, argv, TK_CONFIG_ARGV_ONLY);
    }
}


/*
 * this is called for the tcget image command
 */
int TkImage::cgetCmd(int argc, char* argv[])
{
    if (argc != 1) {
	return error("wrong # args: should be: \"$image cget option\"");
    }
    return Tk_ConfigureValue(interp_, tkwin_, configSpecsPtr_, (char*)optionsPtr_, 
			     argv[0], TK_CONFIG_ARGV_ONLY);
}



/*
 * Call the given method in this class with the given arguments
 */
int TkImage::call(const char* name, int len, int argc, char* argv[])
{
    if (strncmp(name, "configure", len) == 0) {
	return configureCmd(argc, argv);
    } 
    else if (strncmp(name, "cget", len) == 0) {
	return cgetCmd(argc, argv);
    }
    return TclCommand::call(name, len, argc, argv);
}


/*
 * delete subcommand - 
 * (should probably redefine to use "image delete $image")
 */
int TkImage::deleteCmd(int, char**) {
    return error("use \"image delete $image\" to delete the image.");
}

