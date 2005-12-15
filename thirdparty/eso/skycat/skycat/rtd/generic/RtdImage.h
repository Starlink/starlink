#ifndef _RtdImage_h_
#define _RtdImage_h_

/*
 * E.S.O. - VLT project / ESO Archive
 * "@(#) $Id: RtdImage.h,v 1.7 2005/02/02 01:43:03 brighton Exp $"
 *
 * RtdImage.h - class definitions for class RtdImage, a real-time image 
 * display extension for Tk.
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * D.Hopkinson     02/12/96  Added timestamp information for performance tool
 * Allan Brighton  10/03/98  Added optional args to constructor to allow derived
 *                           class to specify its own configuration options.
 *                           loadFile now virtual to aid subclassing.
 * Allan Brighton  13/03/98  Define RTD_OPTIONS as a macro, so that derived
 *                           classes can add new options more easily.
 * Peter W. Draper 13/01/99  Added changes to support non 8 bit
 *                           colors (colorUpdate).
 *                           Made displayImageEvent virtual (need for UKIRT
 *                           quick look updates).
 * P.Biereichel    22/03/99  Added code for bias subtraction
 * P.Biereichel    29/06/99  Added HDU includes (copied from skycat)
 * P.Biereichel    26/05/00  Added options fillWidth / fillHeight
 * P.Biereichel    01/03/01  Copied the include and definitions from RtdImage.C
 * P.Biereichel    23/10/02  Made gcc 3.2 happy which complained about RTD_OPTION:
 *                           (invalid offsetof from non-POD type `class RtdImageOptions'; use 
 *                           pointer to member instead). POD means "Plain Old Data".
 * 
 */

#define PANEL_EDITOR_BUG
#define _HAVE_R6 (XlibSpecificationRelease > 5)

//#define DEBUG

#include <cctype>
#include <cstdlib>
#include <csignal>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/sem.h>
#include <cstring>
#include <fcntl.h>
#include <unistd.h>
#include <cmath>
#include <cassert>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "define.h"
#include "error.h"
#include "WorldCoords.hxx"
#include "ImageColor.h"
#include "ImageDisplay.h"
#include "ImageData.h"
#include "ImageZoom.h"
#include "FitsIO.hxx"
#include "RtdCamera.h"
#include "RtdRemote.h"
#include "rtdImageEvent.h"
#include "BiasData.h"
#include "ImageData.h"
#include "RtdPerf.h"
#include "RtdUtils.h"
#include "TkImage.h"

// we use pointers to classes of these types below
class ImageColor;
class ImageDisplay;
class ImageData;
class BiasData;
class ImageZoom;
class RtdCamera;
class RtdRemote;
class Mem;
class RtdImage;
class RtdPerf;

// RtdImage signal handlers
void RtdImage_cleanup (int);

// cleanup shm routine of Rtd Recorder/Playback tool
void Mem_RPTcleanup();

// from BLT package (now locally because this routine was deleted with blt2.1)
extern "C" int Blt_GraphElement(
    Tcl_Interp *interp,         /* Interpreter of the graph widget */
    char *pathName,             /* Path name of the graph widget */
    char *elemName,             /* Name of the element to reset */
    int numValues,              /* Number of values in array */
    double *valueArr,           /* Array of x,y coordinate pairs */
    char *xVector,              /* Name of x array */
    char *yVector);             /* Name of y array */

/* 
 * image options (used for image configuration)
 */

typedef struct Rtd_Options {
    int displaymode;		// set mode used to display image: 
				// 0 ==> XImage is size of image, update whole image to pixmap
				// 1 ==> XImage is size of window (default mode)

    int fitWidth;		// fit the image in a window with this width 
    int fitHeight;		// and this height by shrinking the image

    int fillWidth;		// fit the image in a window with this width 
    int fillHeight;		// and this height by shrinking or expanding the image

    int subsample;		// if true, don't count neighboring pixels when shrinking image
    int sampmethod;		// sampling method
    int usexshm;		// if true, use X shared memory if available. 
    int usexsync;               // if true, use X synchronisation if available. 
    int verbose;		// if true, print program info to stdout 
    int debug;	  	        // if true, print program info to stdout 

    int shm_header;		// if true, keep image FITS headers in shared memory
    int shm_data;		// if true, keep image FITS data in shared memory
				// (see RtdRemote remote access interface)

    int min_colors;		// min (max) number of colors to allocate, if this many are
    int max_colors;		// not available, use private colormap.

    char* file;			// name of image file, if any

    char* name;			// name for image (for debugging)

    char* newImageCmd;		// tcl command to evaluate whenever a new (different) 
				// image is loaded (for updates, see camera command)

    int fixUpdateRate;          // flag: user has specified a fixed update rate, as below.
    double userUpdateTime;      // the minimum time between updates, as specified
                                // by the user.
} Rtd_Options;


class RtdImageOptions : public TkImageOptions {
 public:

    // constructor
    RtdImageOptions() {
        rtd_options_ = new Rtd_Options;
        manage = 1;
        initialise();
    }
    RtdImageOptions( Rtd_Options *options ) {
        rtd_options_ = options;
        manage = 0;
        initialise();
    }

    struct Rtd_Options *rtd_options_;
    int manage;

    // Accessors
    virtual char *get_rtd_options() {return (char *)rtd_options_;}

    // destructor
    ~RtdImageOptions() {
        if ( manage ) {
            delete rtd_options_;
        }
    }

 private:
    void initialise() {
        memset(rtd_options_, '\0', sizeof(Rtd_Options));
	rtd_options_->displaymode=1;
	rtd_options_->usexshm=1;
	rtd_options_->usexsync=1;
	rtd_options_->min_colors=30;
	rtd_options_->max_colors=60;
    }

};


/*
 * These options are defined here for use in the Tk_ConfigSpec declaration, so that
 * derived classes can more easily add to them. See RtdImage.C for usage.
 */

#define RTD_OPTION(x) Tk_Offset(Rtd_Options, x)

#define RTD_OPTIONS \
    {TK_CONFIG_BOOLEAN, "-usexshm",     NULL, NULL, "1", RTD_OPTION(usexshm),     0}, \
    {TK_CONFIG_BOOLEAN, "-usexsync",    NULL, NULL, "1", RTD_OPTION(usexsync),    0}, \
    {TK_CONFIG_BOOLEAN, "-verbose",     NULL, NULL, "0", RTD_OPTION(verbose),     0}, \
    {TK_CONFIG_BOOLEAN, "-debug",       NULL, NULL, "0", RTD_OPTION(debug),       0}, \
    {TK_CONFIG_BOOLEAN, "-shm_header",  NULL, NULL, "0", RTD_OPTION(shm_header),  0}, \
    {TK_CONFIG_BOOLEAN, "-shm_data",    NULL, NULL, "0", RTD_OPTION(shm_data),    0}, \
    {TK_CONFIG_INT,     "-displaymode", NULL, NULL, "1", RTD_OPTION(displaymode), 0}, \
    {TK_CONFIG_INT,     "-min_colors",  NULL, NULL, "1", RTD_OPTION(min_colors),  0}, \
    {TK_CONFIG_INT,     "-max_colors",  NULL, NULL, "1", RTD_OPTION(max_colors),  0}, \
    {TK_CONFIG_INT,     "-fitwidth",    NULL, NULL, "0", RTD_OPTION(fitWidth),    0}, \
    {TK_CONFIG_INT,     "-fitheight",   NULL, NULL, "0", RTD_OPTION(fitHeight),   0}, \
    {TK_CONFIG_INT,     "-fillwidth",   NULL, NULL, "0", RTD_OPTION(fillWidth),   0}, \
    {TK_CONFIG_INT,     "-fillheight",  NULL, NULL, "0", RTD_OPTION(fillHeight),  0}, \
    {TK_CONFIG_BOOLEAN, "-subsample",   NULL, NULL, "1", RTD_OPTION(subsample),   0}, \
    {TK_CONFIG_INT,     "-sampmethod",  NULL, NULL, "0", RTD_OPTION(sampmethod),  0}, \
    {TK_CONFIG_STRING,  "-file",        NULL, NULL, "",  RTD_OPTION(file),        0}, \
    {TK_CONFIG_STRING,  "-newimagecmd", NULL, NULL, "",  RTD_OPTION(newImageCmd), 0}, \
    {TK_CONFIG_STRING,  "-name",        NULL, NULL, "",  RTD_OPTION(name), 0}


/*
 * Class RtdImage
 * 
 * This class implements the extended Tk image type "rtdimage" for displaying
 * FITS and other images in a Tk canvas window
 */
class RtdImage : public TkImage {

#include "RtdCmds.icc"          // image subcommand methods
#include "RtdHDU.icc"           // methods for hduCmd()
#include "RtdCoords.icc"        // methods for coordinate conversion

protected:
    RtdImageOptions* options_;  // holds image config options

    RtdCamera* camera_;		// class managing interface to realtime image events
    RtdRemote* remote_;		// class managing remote control interface

    char* cameraPreCmd_;	// Tcl command to evaluate when image event is received (before display)
    char* cameraPostCmd_;	// Tcl command to evaluate after image event has been processed
    int imageEvent_;              // image event received from camera
    int frameId_;		// frame Id, for use with rapid frames


    static ImageColor* colors_; // class for managing colors and colormaps

    ImageData* image_;     	// class object managing the image data

    RtdDebugLog* dbl_;     	// class object managing the debug log messages

    static BiasData* biasimage_;// class for managing the bias image
    static RtdPerf* rtdperf_;   // class object for managing the performance test

    char filename_[1024];       // filename or object of master image

    ImageZoom* zoomer_;		// class for managing zoom window
    RtdImage* zoomView_;	// rtdimage instance used for "zoomview"
    RtdImage* zoomView2_;	// optional second rtdimage instance used for additional "zoomview"
    int zoomFactor_;		// relative magnification for zoom
    int zoomSpeed_;		// normally 0 (fast), or < 0 (slow updates)

    int motionX_, motionY_;	// saved motion event position 
    int saveMotion_;            // save motion event flag
    int motionPending_;		// flag for doWhenIdle handler for motion events
    unsigned int motionState_;	// saved state field from motion event

    int propagateScale_;	// flag: if true and this is a view, propagate changes 
				// in magnification from master image to this image

    int autoSetCutLevels_;	// flag: if true we can set cut levels automatically
				// on new images, otherwise keep previous cut levels

    int rapidFrame_;		// flag: if true, this is a rapid frame
    
    int displayLocked_;		// flag: true if image is currently being updated

    // views (copy of image sharing same raw data)
    enum {MAX_VIEWS = 8};	 // maximum number of views of an image
    RtdImage* view_[MAX_VIEWS];  // array of views (tkimage widgets)
    RtdImage* viewMaster_; 	 // the image this image is a view of
    RtdImage* currentView_; 	 // cur view, when more than one image is in a canvas

    // The following distances are normally 0, but might be non-zero for the
    // zoom window or a rapid frame. They are all stored as image coordinate distances 
    // and converted to canvas coords as needed, to be independent of transformations.
    double frameX_, frameY_;	// X,Y offset of image frame in image canvas
    double xOffset_, yOffset_;	// X,Y offset of image origin in raw image.

    // note: this is needed since rapid frames have a separate memory area starting
    // at 0,0, but which corresponds to (rapidX_,rapidY_) in the main image
    double rapidX_, rapidY_;	// X,Y offset of rapid frame coresponding to main image.

    double reqWidth_, reqHeight_; // requested width and height or 0 for entire image

    // panning window  
    int panFactor_;		// shrink (scale) factor for panning window
    char* panCommand_;		// Tcl command to evaluate when image changes size or pos.
    int panx1_, pany1_;		// saved coords of upper left of visible image
    int panx2_, pany2_;		// saved coords of lower right of visible image

    // Tk canvas window info
    Tk_Canvas canvas_;          // handle for image's canvas window
    const char* canvasName_;	// name of canvas window
    int canvasX_, canvasY_; 	// X,Y offset of image in canvas (for scrolling)
    int prevX_, prevY_;         // saved X,Y origin from last display update

    // X shared memory
    int haveXShm_;		// flag: true if X shared memory is available
    int usingXShm_;		// flag: true if we are using X shared memory

    // X Sync extension (the XSyncSetPriority function is used).
    int haveXSync_;             // flag: true if X synchronisation is available
    int usingXSync_;            // flag: true if we are using XSync

    // X image
    ImageDisplay* xImage_;	// class object: manages the X Image 

    // Pixel Table
    double* pixTab_;		// array of pixel values and X,Y indices
    int pixTabRows_,		// dimensions of pixTab_ (minus 1 for x,y headings)
	pixTabCols_;

    // -- member functions  --

private:
     
    // copy constructor: not defined
    RtdImage(const RtdImage&);
    

protected:
    // redefined from parent class to check configuration options
    virtual int configureImage(int argc, char* argv[], int flags);

    // return true if this is an embedded (not embedded) rapid frame (in master image)
    int isEmbeddedRapidFrame();
    int isSeparateRapidFrame();

    // set the X,Y scale (zoom) factors
    int setScale(int xScale, int yScale);

    // set the cut levels to the given min and max values
    int setCutLevels(double min, double max, int scaled, int user);

    // called for new image or when image changes size to update pixmap
    // and XImage and redraw
    int resetImage();

    // call resetImage when idle
    void eventuallyResetImage();

    // Return an ImageData object, given an ImageIO object reference.
    virtual ImageData* makeImage(ImageIO);

    // load an image file
    virtual int loadFile();

    // called to initialize a new image from a file or shared memory
    int initNewImage();

    // called to force image to be redrawn
    int updateImage();

    // called to update an existing image with new raw data of the same size and type
    int updateImageNewData(const Mem&);

    // delete the XImage created by updateXImage
    int deleteXImage();

    // add/remove views, to be updated with this image
    int addView(RtdImage* view);
    int removeView(RtdImage* view);
    void removeViews();

    // update this image with a new view from the master image
    int updateView(ImageData*, int flag = 0);

    // update all views
    int updateViews(int flag = 0);

    // get class object from instance name
    RtdImage* getView(char* name);

    // called when idle for motion events in the image 
    virtual void processMotionEvent();
    
    // zooming methods, when using rtdimage view
    virtual void autoZoomView(double x, double y);
    virtual void updateZoomView(RtdImage* view, double x, double y);

    // called to notify the panning window of the coords of the visible image
    virtual void autoPan(int newImageFlag = 0);
    
    // event methods, called for motion/configure events in image window
    virtual void motionNotify(XEvent* eventPtr);
    virtual void configureNotify(XEvent* eventPtr);

    // these are called indirectly by the Tk imageing routines
    virtual void displayImage( Drawable, int imageX, int imageY, 
			       int width, int height,
			       int drawableX, int drawableY);
    virtual TkImage* getImage(Tk_Window);

    // get fraction of zoomed pixel at point
    void getOffsetInXImage(double px, double py, int& x, int& y);

    // propagate color change
    int colorUpdate( int force = 0);

    // Set detector parameters
    void setDetParms(ImageData* image, const rtdIMAGE_INFO&);

    // set or query the filename of the master image
    void   filename(char *file) {strcpy(filename_, file);}
    char*  filename() {return filename_;}

public:
    // initialize the image with the command line args
    RtdImage(Tcl_Interp*, const char* instname, int argc, char** argv, 
	     Tk_ImageMaster master, const char* imageType,
	     Tk_ConfigSpec* specs = (Tk_ConfigSpec*)NULL, 
	     RtdImageOptions* options = (RtdImageOptions*)NULL);
    
    // destructor - free any allocated resources
    ~RtdImage();
    
    // call a member function by name
    virtual int call(const char* name, int len, int argc, char* argv[]);

    // initialize color map and visual
    static int initColors(Tcl_Interp* interp);

    // initialize bias image object
    static int initBias();

    // initialize performance test object
    static int initPerf(Tcl_Interp* interp);

    // entry point from tcl to create a image
    static int CreateImage(Tcl_Interp*, char *name, int argc, 
#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
                           Tcl_Obj *CONST objv[],
#else
                           char **argv,
#endif
                           Tk_ImageType*, Tk_ImageMaster, ClientData*);

    // event procedure for main image window
    static void eventProc(ClientData clientData, XEvent *eventPtr);

    // doWhenIdle handler for zoom window
    static void motionProc(ClientData clientData);

    // called from the Camera class to display image from shared memory
    virtual int displayImageEvent(const rtdIMAGE_INFO&, const Mem& data);
    
    // utility Tcl command proc to set colormap for popup windows
    static int rtd_set_cmap(ClientData, Tcl_Interp* interp, int argc, char** argv);
 
    // update idle tasks and performance test variables
    void updateRequests();   

    // read-only access to configuration options
    static ImageColor* colors() {return colors_;}
    static RtdPerf* rtdperf() {return rtdperf_;}

    int displaymode() 	const {return options_->rtd_options_->displaymode;}
    int fitWidth() 	const {return options_->rtd_options_->fitWidth;}
    int fitHeight() 	const {return options_->rtd_options_->fitHeight;}
    int fillWidth() 	const {return options_->rtd_options_->fillWidth;}
    int fillHeight() 	const {return options_->rtd_options_->fillHeight;}
    int subsample() 	const {return options_->rtd_options_->subsample;}
    int sampmethod() 	const {return options_->rtd_options_->sampmethod;}
    char* file() 	const {return options_->rtd_options_->file;}
    char* newImageCmd() const {return options_->rtd_options_->newImageCmd;}
    char* name() 	const {return ((options_->rtd_options_->name && *options_->rtd_options_->name) ? 
				       options_->rtd_options_->name : instname_);}
    int usexshm() 	const {return options_->rtd_options_->usexshm;}
    int usexsync() 	const {return options_->rtd_options_->usexsync;}
    int shm_header() 	const {return options_->rtd_options_->shm_header;}
    int shm_data() 	const {return options_->rtd_options_->shm_data;}
    int min_colors() 	const {return options_->rtd_options_->min_colors;}
    int max_colors() 	const {return options_->rtd_options_->max_colors;}
    int verbose() 	const {return options_->rtd_options_->verbose;}
    int debug() 	const {return options_->rtd_options_->debug;}


    // -- short cuts --

    // return the dimensions of the image after transformations
    int dispWidth() {return (image_ ? image_->dispWidth() : 1);}
    int dispHeight() {return (image_ ? image_->dispHeight() : 1);}

    // return the type of the raw image data
    int imageType() {return (image_ ? image_->dataType() : UNKNOWN_IMAGE);}

    // return true if there is an image and it supports world coordinates
    int isWcs() {return (image_ ? image_->wcs().isWcs() : 0);}

    // Return true if no image is loaded.
    int isclear();

    // Return name of instance (= tcl command which corresponds to this image)
    char* instname() {return instname_;}

    // member access
    char* cameraPreCmd() {return cameraPreCmd_;}
    char* cameraPostCmd() {return cameraPostCmd_;}

    // Set state of image event (currently true/false)
    int imageEvent(int state) {return ( imageEvent_ = state );}

    ImageData* image() {return image_;}
};

/*
 * derive a Camera subclass that handles the image events
 * for loading images from shared memory
 */
class RtdImageCamera : public RtdCamera {
    RtdImage* rtdimage_;                // keep this ptr for calling display method 
public:
    // constructor
    RtdImageCamera(RtdImage* rtdimage)
        : RtdCamera(rtdimage->name(), 
		    rtdimage->interp(),
		    rtdimage->verbose(),
		    rtdimage->debug(), 
		    rtdimage->instname()), 
          rtdimage_(rtdimage) {}

    // called from the Camera class to display image from shared memory.
    // pass on the method in RtdImage class
    int display(const rtdIMAGE_INFO&, const Mem& data);
};

/*
 * derive a RtdRemote subclass that handles the remote access to
 * the widget.
 *  
 * This class is a bit different than the above RtdImageCamera class. It
 * is designed to be of more general use (not just for real-time updates)
 * and doesn't make use of the rtdServer daemon.
 */
class RtdImageRemote : public RtdRemote {
    RtdImage* rtdimage_;                // keep this ptr for calling display method 
public:
    // constructor
    RtdImageRemote(RtdImage* rtdimage, int port)
        : RtdRemote(rtdimage->interp(), port, rtdimage->verbose()), 
          rtdimage_(rtdimage) {}
    // call an rtdimage command method by name
    int call(const char* name, int len, int argc, char* argv[]) {
        return rtdimage_->call(name, len, argc, argv);
    }
};

#endif /* _RtdImage_h_ */
