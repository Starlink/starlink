// -*-c++-*-
#ifndef _RtdImage_h_
#define _RtdImage_h_

/*
 * E.S.O. - VLT project / ESO Archive
 * "@(#) $Id: RtdImage.h,v 1.25 1998/10/28 17:41:29 abrighto Exp $"
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
 */

#include "TkImage.h"
#include "ImageData.h"

// we use pointers to classes of these types below
class ImageColor;
class ImageDisplay;
class ImageData;
class ImageZoom;
class RtdCamera;
class RtdRemote;
class Mem;
class RtdImage;
struct rtdIMAGE_INFO;

/* 
 * image options (used for image configuration)
 */
class RtdImageOptions : public TkImageOptions {
public:
    int displaymode;		// set mode used to display image: 
				// 0 ==> XImage is size of image, update whole image to pixmap
				// 1 ==> XImage is size of window (default mode)

    int fitWidth;		// fit the image in a window with this width 
    int fitHeight;		// and this height by shrinking the image

    int subsample;		// if true, don't count neighboring pixels when shrinking image
    int usexshm;		// if true, use X shared memory if available. 
    int usexsync;               // if true, use X synchronisation if available. 
    int verbose;		// if true, print program info to stdout 

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

    // constructor
    RtdImageOptions()
	: displaymode(1),
	  fitWidth(0), fitHeight(0),
	  subsample(0),
	  usexshm(1),
          usexsync(1),
          verbose(0),
	  shm_header(0),
	  shm_data(0),
	  min_colors(30),
	  max_colors(60),
	  file(NULL),
	  name(NULL),
	  newImageCmd(NULL),
          fixUpdateRate(0),
          userUpdateTime(0.) {}
};


/*
 * These options are defined here for use in the Tk_ConfigSpec declaration, so that
 * derived classes can more easily add to them. See RtdImage.C for usage.
 */
#define RTD_OPTION(x) Tk_Offset(RtdImageOptions, x)
#define RTD_OPTIONS \
    {TK_CONFIG_BOOLEAN, "-usexshm",     NULL, NULL, "1", RTD_OPTION(usexshm),     0}, \
    {TK_CONFIG_BOOLEAN, "-usexsync",    NULL, NULL, "1", RTD_OPTION(usexsync),    0}, \
    {TK_CONFIG_BOOLEAN, "-verbose",     NULL, NULL, "0", RTD_OPTION(verbose),     0}, \
    {TK_CONFIG_BOOLEAN, "-shm_header",  NULL, NULL, "0", RTD_OPTION(shm_header),  0}, \
    {TK_CONFIG_BOOLEAN, "-shm_data",    NULL, NULL, "0", RTD_OPTION(shm_data),    0}, \
    {TK_CONFIG_INT,     "-displaymode", NULL, NULL, "1", RTD_OPTION(displaymode), 0}, \
    {TK_CONFIG_INT,     "-min_colors",  NULL, NULL, "1", RTD_OPTION(min_colors),  0}, \
    {TK_CONFIG_INT,     "-max_colors",  NULL, NULL, "1", RTD_OPTION(max_colors),  0}, \
    {TK_CONFIG_INT,     "-fitwidth",    NULL, NULL, "0", RTD_OPTION(fitWidth),    0}, \
    {TK_CONFIG_INT,     "-fitheight",   NULL, NULL, "0", RTD_OPTION(fitHeight),   0}, \
    {TK_CONFIG_BOOLEAN, "-subsample",   NULL, NULL, "1", RTD_OPTION(subsample),   0}, \
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
protected:
    RtdImageOptions* options_;  // holds image config options

    RtdCamera* camera_;		// class managing interface to realtime image events
    char* cameraPreCmd_;	// Tcl command to evaluate when image event is received (before display)
    char* cameraPostCmd_;	// Tcl command to evaluate after image event has been processed
    int frameId_;		// frame Id, for use with rapid frames

    RtdRemote* remote_;		// class managing remote control interface

    static ImageColor* colors_; // class for managing colors and colormaps

    ImageData* image_;     	// class object managing the image data 

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
    short canvasX_, canvasY_; 	// X,Y offset of image in canvas (for scrolling)
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

    // The following properties pertain to the interactive performance 
    // measurement, and are only 'active' when the following flag is set.
    int intPerfTest_;           // Flag: do interactive performance testing.
    double GENtime_;            // Time spent on general (C/C++) processing.
    double TCLtime_;            // Time spent on TCL/TK code interpretation.
    double Xtime_;              // Time spent on X function calls.
    double MEMtime_;            // Time spent on memory management.
    double lastTimeStamp_;      // Timestamp of the last timestamped event.
    int imageCount_;            // Number of images to average over in ptester
    double accGENtime_;         // Accumulative versions of the above...
    double accTCLtime_;
    double accXtime_;
    double accMEMtime_;
    double initTimeStamp_;      // Initial timestamp at start of perf test
    enum perfTestType {
        TIME,
        NORM_TIME,
        PCT_TIME
    } perfTestType;

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

    // these coordinate conversion methods are only needed internally
    int screenToXImageCoords(double& x, double& y);
    int xImageToImageCoords(double& x, double& y, int dist_flag);
    int imageToRawImageCoords(double& x, double& y);
    void coordsToDist(double& x, double& y);
    void distToCoords(double& x, double& y);
    void doTrans(double& x, double& y, int distFlag = 0);
    void undoTrans(double& x, double& y, int distFlag = 0);

    // get fraction of zoomed pixel at point
    void getOffsetInXImage(double px, double py, int& x, int& y);

    // propagate color change
    int colorUpdate( int force = 0);

    // Increment a performance test variable.
    void timeInc(double *);

    // Reset the performance test data.
    void resetPerfTest();

    // Set the performance test variables in the form.
    void setPerfTestVars();    

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

    // entry point from tcl to create a image
    static int CreateImage(Tcl_Interp*, char *name, int argc, char **argv, 
		    Tk_ImageType*, Tk_ImageMaster, ClientData*);

    // event procedure for main image window
    static void eventProc(ClientData clientData, XEvent *eventPtr);

    // doWhenIdle handler for zoom window
    static void motionProc(ClientData clientData);

    // called from the Camera class to display image from shared memory
    int displayImageEvent(const rtdIMAGE_INFO&, const Mem& data);
    
    // utility Tcl command proc to set colormap for popup windows
    static int rtd_set_cmap(ClientData, Tcl_Interp* interp, int argc, char** argv);

    // -- image subcommand methods --
    
    int alloccolorsCmd(int argc, char* argv[]);
    int autocutCmd(int argc, char* argv[]);
    int bitpixCmd(int argc, char* argv[]);
    int cameraCmd(int argc, char* argv[]);
    int clearCmd(int argc, char* argv[]);
    int cmapCmd(int argc, char* argv[]);
    int colorrampCmd(int argc, char* argv[]);
    int colorscaleCmd(int argc, char* argv[]);
    int convertCmd(int argc, char* argv[]);
    int cutCmd(int argc, char* argv[]);
    int dispheightCmd(int argc, char* argv[]);
    int dispwidthCmd(int argc, char* argv[]);
    int dumpCmd(int argc, char* argv[]);
    int fitsCmd(int argc, char* argv[]);
    int flipCmd(int argc, char* argv[]);
    int frameidCmd(int argc, char* argv[]);
    int freqCmd(int argc, char *argv[]);
    int getCmd(int argc, char* argv[]);
    int graphdistCmd(int argc, char* argv[]);
    int heightCmd(int argc, char* argv[]);
    int isclearCmd(int argc, char* argv[]);
    int ittCmd(int argc, char* argv[]);
    int maxCmd(int argc, char* argv[]);
    int maxFreqCmd(int argc, char* argv[]);
    int mbandCmd(int argc, char* argv[]);
    int minCmd(int argc, char* argv[]);
    int mmapCmd(int argc, char* argv[]);
    int motioneventCmd(int argc, char* argv[]);
    int objectCmd(int argc, char* argv[]);
    int panCmd(int argc, char* argv[]);
    int perfTestCmd(int argc, char *argv[]);
    int pixtabCmd(int argc, char* argv[]);
    int previewCmd(int argc, char* argv[]);
    int radecboxCmd(int argc, char* argv[]);
    int remoteCmd(int argc, char* argv[]);
    int remoteTclCmd(int argc, char* argv[]);
    int rotateCmd(int argc, char* argv[]);
    int scaleCmd(int argc, char* argv[]);
    int shmCmd(int argc, char* argv[]);
    int spectrumCmd(int argc, char* argv[]);
    int statisticsCmd(int argc, char* argv[]);
    int typeCmd(int argc, char* argv[]);
    int updateCmd(int argc, char* argv[]);
    int viewCmd(int argc, char* argv[]);
    int warpCmd(int argc, char* argv[]);
    int wcssetCmd(int argc, char* argv[]);
    int wcsshiftCmd(int argc, char* argv[]);
    int wcscenterCmd(int argc, char* argv[]);
    int wcsdistCmd(int argc, char* argv[]);
    int wcsequinoxCmd(int argc, char* argv[]);
    int wcsheightCmd(int argc, char* argv[]);
    int wcsradiusCmd(int argc, char* argv[]);
    int wcswidthCmd(int argc, char* argv[]);
    int widthCmd(int argc, char* argv[]);
    int zoomCmd(int argc, char* argv[]);
    int zoomviewCmd(int argc, char* argv[]);

    // coordinate conversion types
    enum CoordinateType {
	CT_NONE = 0, 
	CT_IMAGE = 'i',
	CT_CANVAS = 'c',
	CT_SCREEN = 's',
	CT_WCS = 'w',
	CT_DEG = 'd',
	CT_CHIP = 'C'
    };

    // return the enum CoordinateType value given the string name
    CoordinateType getCoordinateType(const char* s);

    // convert coords from string form
    int convertCoordsStr(int dist_flag, 
			 const char* inx_buf, const char* iny_buf,
			 char* outx_buf, char* outy_buf,
			 double& x, double& y,
			 const char* in_type, const char* out_type);

    // convert coords from doubles
    int convertCoords(int dist_flag, double& x, double& y, 
		      const char* in_type, const char* out_type);

    // convert coords as doubles (for WCS, assumes equinox of image, 
    // coord type only specifies first letter and no equinox).
    int convertCoords(int dist_flag, double& x, double& y, 
		      char in_type, char out_type);

    // Utility method to change the equinox of ra and dec if dist_flag is 0
    void changeEquinox(int dist_flag, double& ra, double& dec, 
		       double in_equinox, double out_equinox);

    // utility methods to convert between different coordinate systems
    int canvasToScreenCoords(double& x, double& y, int dist_flag);
    int canvasToImageCoords(double& x, double& y, int dist_flag);
    int canvasToWorldCoords(double& x, double& y, int dist_flag);
    int screenToCanvasCoords(double& x, double& y, int dist_flag);
    int screenToImageCoords(double& x, double& y, int dist_flag);
    int screenToWorldCoords(double& x, double& y, int dist_flag);
    int imageToCanvasCoords(double& x, double& y, int dist_flag);
    int imageToScreenCoords(double& x, double& y, int dist_flag);
    int imageToWorldCoords(double& x, double& y, int dist_flag);
    int worldToCanvasCoords(double& x, double& y, int dist_flag);
    int worldToImageCoords(double& x, double& y, int dist_flag);
    int worldToScreenCoords(double& x, double& y, int dist_flag);
    int imageToChipCoords(double& x, double& y, int dist_flag);
    int canvasToChipCoords(double& x, double& y, int dist_flag);
    int screenToChipCoords(double& x, double& y, int dist_flag);
    int worldToChipCoords(double& x, double& y, int dist_flag);
    int chipToImageCoords(double& x, double& y, int dist_flag);
    int chipToCanvasCoords(double& x, double& y, int dist_flag);
    int chipToScreenCoords(double& x, double& y, int dist_flag);
    int chipToWorldCoords(double& x, double& y, int dist_flag);


    // read-only access to configuration options
    static ImageColor* colors() {return colors_;}
    int displaymode() const {return options_->displaymode;}
    int fitWidth() const {return options_->fitWidth;}
    int fitHeight() const {return options_->fitHeight;}
    int subsample() const {return options_->subsample;}
    char* file() const {return options_->file;}
    char* newImageCmd() const {return options_->newImageCmd;}
    char* name() const {return ((options_->name && *options_->name) ? options_->name : instname_);}
    int usexshm() const {return options_->usexshm;}
    int usexsync() const {return options_->usexsync;}
    int shm_header() const {return options_->shm_header;}
    int shm_data() const {return options_->shm_data;}
    int min_colors() const {return options_->min_colors;}
    int max_colors() const {return options_->max_colors;}
    int verbose() const {return options_->verbose;}


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

    // member access
    char* cameraPreCmd() {return cameraPreCmd_;}
    char* cameraPostCmd() {return cameraPostCmd_;}
    ImageData* image() {return image_;}
};

// RtdImage signal handlers
void RtdImage_cleanup (int);

// cleanup shm routine of Rtd Recorder/Playback tool
void Mem_RPTcleanup();

#endif /* _RtdImage_h_ */
