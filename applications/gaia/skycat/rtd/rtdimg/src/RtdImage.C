/*
 * E.S.O. - VLT project 
 * "@(#) $Id: RtdImage.C,v 1.65 1998/11/16 21:25:10 abrighto Exp $"
 *
 * RtdImage.C - member routines for class RtdImage,
 *               implementation of the TCL rtdimage command
 *
 * See the man page for a complete description.
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 *
 * T.Herlin        06/12/95  Setting object name RtdImage::displayImageEvent
 *
 * D.Hopkinson     02/12/96  Added timestamp information for performance tool
 *
 * P.Biereichel    30/06/97  processMotionEvent()'s call to getValues() changed
 *                           Added RtdImage::remoteTclCmd
 *
 * Allan Brighton  10/03/98  Minor changes based on changes made by 
 * 			     Peter W. Draper <P.W.Draper@durham.ac.uk>
 *                           for GAIA, which defines a subclass of
 *                           RtdImage, so that we can use some of the
 *                           GAIA features in our own derived classes.
 *
 *                           Added 2 optional arguments to RtdImage
 *                           constructor to allow derived classes to
 *                           specify thier own list of configuration
 *                           options.
 *
 *                           Changed "blank image" size from 10x10 to 2x2
 *                           image, since people really seem to use 10x10
 *                           pixel images. (The size is sometimes used to
 *                           determine if the image is "blank").
 *
 *                           Changed ImageData::read invocation to use
 *                           FitsIO::read instead. This decouples the
 *                           need for ImageData (and ImageIO) to know
 *                           about what type of data they are dealing
 *                           with (and hence we can create a derived
 *                           class that can deal with other forms of
 *                           data, like NDFs).
 *
 * Allan Brighton  13/03/98  Define RTD_OPTIONS as a macro, so that derived
 *                           classes can add new options more easily.
 *
 *                 30/03/98  Fixed the -shm_data and -shm_header options to 
 *                           work again (they were lost in previous changes at
 *                           some point).
 *
 * Peter W. Draper 13/01/99  Changed to use non 8 bit visuals.
 */
static const char* const rcsId="@(#) $Id: RtdImage.C,v 1.65 1998/11/16 21:25:10 abrighto Exp $";

#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <signal.h>
#include <stdio.h>
#include <iostream.h>
#include <strstream.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/sem.h>
#include <new.h>
#include <unistd.h>
#include <math.h>
#include <assert.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "error.h"
#include "define.h"
#include "config.h"
#include "WorldCoords.h"
#include "ImageColor.h"
#include "ImageDisplay.h"
#include "ImageData.h"
#include "ImageZoom.h"
#include "FitsIO.h"
#include "RtdCamera.h"
#include "RtdRemote.h"
#include "RtdImage.h"
#include "rtdImageEvent.h"

#define _HAVE_R6 (XlibSpecificationRelease > 5)

//#define DEBUG

// should be in X11/extensions/XShm.h
extern "C" int XShmQueryExtension(Display*);

// should be in X11/extensions/sync.h
extern "C" int XSyncInitialize(Display *, int *, int *);
extern "C" int XSyncSetPriority(Display *, XID, int);

#ifdef NEED_GETHOSTNAME_PROTO
// should be in unistd.h ?
extern "C" int gethostname(char *name, unsigned int namelen);
#endif /* NEED_GETHOSTNAME_PROTO */

// generated code for bitmaps used in tcl scripts
void defineRtdBitmaps(Tcl_Interp*);

// generated code for colormap files used
void defineColormaps();

// from BLT package (now locally because this routine was deleted with blt2.1)
extern "C" int Blt_GraphElement(
    Tcl_Interp *interp,         /* Interpreter of the graph widget */
    char *pathName,             /* Path name of the graph widget */
    char *elemName,             /* Name of the element to reset */
    int numValues,              /* Number of values in array */
    double *valueArr,           /* Array of x,y coordinate pairs */
    char *xVector,              /* Name of x array */
    char *yVector);             /* Name of y array */

// initialize these dependent packages here for backward compat
extern "C" int Tclutil_Init(Tcl_Interp *interp);
extern "C" int Astrotcl_Init(Tcl_Interp *interp);

// this is a static member shared by all instances of the image
// and used for managing the color map
ImageColor* RtdImage::colors_ = (ImageColor*) NULL;

// pointer to last RtdImage instance to handle motion events
// (used to update display on real-time image events)
static RtdImage* motionView_ = NULL;

// Rtd Record 
extern "C" int RtdrecordInit(Tcl_Interp *);

/* 
 * declare a table of image subcommands and the methods that handle them.
 *
 * NOTE: keep this table sorted, so we can use a binary search on it !
 * (select lines in emacs and use M-x sort-lines)
 */
static class RtdImageSubCmds {
public:
    char* name;      // method name
    int (RtdImage::*fptr)(int argc, char* argv[]); // ptr to method
    int min_args;    // minimum number of args
    int max_args;    // maximum number of args
} subcmds_[] = { 
    {"alloccolors", &RtdImage::alloccolorsCmd,  0,  1},
    {"autocut",     &RtdImage::autocutCmd,      0,  2},
    {"bitpix",      &RtdImage::bitpixCmd,       0,  0},
    {"camera",      &RtdImage::cameraCmd,       1,  4},
    {"clear",       &RtdImage::clearCmd,        0,  14},
    {"cmap",        &RtdImage::cmapCmd,         1,  2},
    {"colorramp",   &RtdImage::colorrampCmd,    0,  0},
    {"colorscale",  &RtdImage::colorscaleCmd,   0,  1},
    {"convert",     &RtdImage::convertCmd,      7,  7},
    {"cut",         &RtdImage::cutCmd,          0,  3},
    {"dispheight",  &RtdImage::dispheightCmd,   0,  0},
    {"dispwidth",   &RtdImage::dispwidthCmd,    0,  0},
    {"dump",        &RtdImage::dumpCmd,         1,  5},
    {"fits",        &RtdImage::fitsCmd,         1,  2},
    {"flip",        &RtdImage::flipCmd,         1,  2},
    {"frameid",     &RtdImage::frameidCmd,      0,  0},
    {"get",         &RtdImage::getCmd,          3,  5},
    {"graphdist",   &RtdImage::graphdistCmd,    5,  5},
    {"height",      &RtdImage::heightCmd,       0,  0},
    {"isclear",     &RtdImage::isclearCmd,      0,  0},
    {"itt",         &RtdImage::ittCmd,          1,  2},
    {"max",         &RtdImage::maxCmd,          0,  0},
    {"mband",       &RtdImage::mbandCmd,        6,  6},
    {"min",         &RtdImage::minCmd,          0,  0},
    {"mmap",        &RtdImage::mmapCmd,         0,  7},
    {"motionevent", &RtdImage::motioneventCmd,  0,  1},
    {"object",      &RtdImage::objectCmd,       0,  0},
    {"pan",         &RtdImage::panCmd,          1,  3},
    {"perftest",    &RtdImage::perfTestCmd,     1,  2},
    {"pixtab",      &RtdImage::pixtabCmd,       1,  3},
    {"preview",     &RtdImage::previewCmd,      1,  1},
    {"radecbox",    &RtdImage::radecboxCmd,     3,  3},
    {"remote",      &RtdImage::remoteCmd,       0,  1},
    {"remotetcl",   &RtdImage::remoteTclCmd,    1,  1},
    {"rotate",      &RtdImage::rotateCmd,       0,  1},
    {"scale",       &RtdImage::scaleCmd,        0,  2},
    {"shm",         &RtdImage::shmCmd,          0,  7},
    {"spectrum",    &RtdImage::spectrumCmd,     9,  9},
    {"statistics",  &RtdImage::statisticsCmd,   0,  0},
    {"type",        &RtdImage::typeCmd,         0,  0},
    {"update",      &RtdImage::updateCmd,       0,  1},
    {"userfreq",    &RtdImage::maxFreqCmd,      1,  1},
    {"view",        &RtdImage::viewCmd,         2,  11},
    {"warp",        &RtdImage::warpCmd,         2,  2},
    {"wcscenter",   &RtdImage::wcscenterCmd,    0,  2},
    {"wcsdist",     &RtdImage::wcsdistCmd,      4,  4},
    {"wcsequinox",  &RtdImage::wcsequinoxCmd,   0,  0},
    {"wcsheight",   &RtdImage::wcsheightCmd,    0,  0},
    {"wcsradius",   &RtdImage::wcsradiusCmd,    0,  0},
    {"wcsset",      &RtdImage::wcssetCmd,       0,  11},
    {"wcsshift",    &RtdImage::wcsshiftCmd,     3,  3},
    {"wcswidth",    &RtdImage::wcswidthCmd,     0,  0},
    {"width",       &RtdImage::widthCmd,        0,  0},
    {"zoom",        &RtdImage::zoomCmd,         1,  3},
    {"zoomview",    &RtdImage::zoomviewCmd,     1,  5}
};


/* 
 * image config options - used to process command line options and for the
 * image "configure" subcommand.
 */
static Tk_ConfigSpec configSpecs_[] = {
    RTD_OPTIONS,		// See RtdImage.h: defines rtd option list
    {TK_CONFIG_END,     NULL,           NULL, NULL, NULL, 0,                  0}
};


/*
 * Initialize the image control structure with pointers to the handler
 * functions
 */
static Tk_ImageType rtdImageType = {
    "rtdimage",			/* name */
    RtdImage::CreateImage,	/* createProc */
    TkImage::GetImage,	        /* getProc */
    TkImage::DisplayImage,      /* displayProc */
    TkImage::FreeImage,	        /* freeProc */
    TkImage::DeleteImage,	/* deleteProc */
    (Tk_ImageType *) NULL	/* nextPtr */
};


/*
 * derive a simple Camera subclass that handles the image events
 * for loading images from shared memory or other outside sources
 */
class RtdImageCamera : public RtdCamera {
    RtdImage* rtdimage_;		// keep this ptr for calling display method 
public:
    // constructor
    RtdImageCamera(RtdImage* rtdimage)
	: RtdCamera(rtdimage->name(), rtdimage->interp(), rtdimage->verbose()), 
	  rtdimage_(rtdimage) {}

    // called from the Camera class to display image from shared memory.
    // pass on to method in RtdImage class
    int display(const rtdIMAGE_INFO&, const Mem& data);
};


/*
 * eval the pre- and post- camera commands and display the image event.
 * "info" contains all we need to know about the image and "data"
 * gives access to the shared memory for the image.
 */
int RtdImageCamera::display(const rtdIMAGE_INFO& info, const Mem& data)
{
    int status = TCL_OK;
    char buf[1024];

    if (rtdimage_->cameraPreCmd()) {
        sprintf(buf, "%s %d", rtdimage_->cameraPreCmd(), info.frameId);
        status |= Tcl_Eval(interp_, buf);
    }

    status |= rtdimage_->displayImageEvent(info, data);
    
    if (rtdimage_->cameraPostCmd()) {
        sprintf(buf, "%s %d", rtdimage_->cameraPostCmd(), info.frameId);
        status |= Tcl_Eval(interp_, buf);
    }
    return status;
}


/*
 * derive a simple RtdRemote subclass that handles the remote access to
 * the widget.
 *  
 * This class is a bit different than the above RtdImageCamera class. It
 * is designed to be of more general use (not just for real-time updates)
 * and doesn't make use of the rtdServer daemon.
 */
class RtdImageRemote : public RtdRemote {
    RtdImage* rtdimage_;		// keep this ptr for calling display method 
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


/*
 * clip x to withing range x0 .. x1
 */
static void clip(double& x, double x0, double x1)
{
    if (x0 < x1) {
	if (x < x0)
	    x = x0;
	else if (x > x1)
	    x = x1;
    } 
    else {
	if (x > x0)
	    x = x0;
	else if (x < x1)
	    x = x1;
    }
}


/*
 * C++ "new handler" - called for out of memory errors by operator new
 * - just report it
 */
static void rtd_new_handler()
{
    error("Warning: Real-Time DIsplay is out of memory");
}


/* 
 * Delete event handler - called to clean up shared memory when main window
 * is deleted
 */
static void destroy_notify(ClientData, XEvent*)
{
    // XXX this method seems to be called at the wrong times
    // XXX (such as when the mouse enters the window!)
    // Mem::cleanup();   
}


/*
 * Tcl command: rtd_load_cmap
 * For internal use, to pre-load colormaps before startup
 */
static int rtd_load_cmap(ClientData, Tcl_Interp* interp, int argc, char** argv)
{
    if (argc != 2) 
	return error("usage: rtd_load_cmap cmapfile");
    return (ColorMapInfo::get(argv[1]) ? TCL_OK : TCL_ERROR);
}


/*
 * Tcl command: rtd_load_itt
 * For internal use, to pre-load itts before startup
 */
static int rtd_load_itt(ClientData, Tcl_Interp* interp, int argc, char** argv)
{
    if (argc != 2) 
	return error("usage: rtd_load_itt ittfile");
    return (ITTInfo::get(argv[1]) ? TCL_OK : TCL_ERROR);
}


/*
 * This static method implements the "rtd_set_cmap" Tcl command.
 *
 * usage: rtd_set_cmap $toplevel
 *
 * This command can be used to make a top level window use the same colormap
 * as the rtd images, so that there is less color flashing in popup windows 
 * when we are using a private colormap.
 */
int RtdImage::rtd_set_cmap(ClientData, Tcl_Interp* interp, int argc, char** argv)
{
    if (argc != 2) 
	return ::error("usage: rtd_set_cmap $toplevel");
    
    Tk_Window w = Tk_NameToWindow(interp, argv[1], Tk_MainWindow(interp));
    if (w == NULL) 
	return TCL_ERROR;

    if (!colors_)
	return ::error("rtd_set_cmap: colormap is not initialized yet");
    
    return colors_->setColormap(w); // colors_ is a static member
}


/*
 * Install two utility Tcl commands for colormaps/itts
 *
 * These commands are for internal use (for pre-loading colormap and itt
 * files for use with unexec to make a single binary with all the stuff
 * already loaded in memory)
 */
extern "C"
int CmapITT_Init(Tcl_Interp* interp)  
{
    Tcl_CreateCommand(interp, "rtd_load_cmap", rtd_load_cmap, NULL, NULL);
    Tcl_CreateCommand(interp, "rtd_load_itt", rtd_load_itt, NULL, NULL);
    return TCL_OK;
}

/*
 * A call to this function is made from the tkAppInit file at startup
 * to install the RtdImage image type and do global initialization
 */
extern "C"
int Rtd_Init(Tcl_Interp* interp)  
{
    // For backward compatibility, initialize 2 local packages that rtd
    // depends on:

    // initialize the tclutil package 
    if (Tclutil_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage (interp, "Tclutil", Tclutil_Init, (Tcl_PackageInitProc *) NULL);

    // initialize the astrotcl package 
    if (Astrotcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage (interp, "Astrotcl", Astrotcl_Init, (Tcl_PackageInitProc *) NULL);

    // initialize color management (once only per application)
    if (RtdImage::initColors(interp) != TCL_OK)
	return TCL_ERROR;

    // PWD: Remove this as now supports other visuals.
    // need 8 bit color for now, but can only force it through wish option -visual
    //
    //    if (Tk_Depth(Tk_MainWindow(interp)) != 8) {
    //	int depth;
    //	Colormap colormap;
    //	if (Tk_GetVisual(interp, Tk_MainWindow(interp), "pseudocolor 8", &depth, &colormap)) 
    //	    return fmt_error("Unsupported X visual depth: %d: %s", 
    //			     Tk_Depth(Tk_MainWindow(interp)),
    //			     "please use the \"-visual pseudocolor\" command line option.");
    //	else
    //	    return fmt_error("Unsupported X visual depth: %d", Tk_Depth(Tk_MainWindow(interp)));
    //    }

    // set up Rtd Tcl package
    if (Tcl_PkgProvide (interp, "Rtd", RTD_VERSION) != TCL_OK) {
	return TCL_ERROR;
    }

    // define bitmaps used by Tcl library
    defineRtdBitmaps(interp);

    // define colormaps used 
    defineColormaps();

    // add the rtdimage image type
    Tk_CreateImageType(&rtdImageType);

    // add a tcl command to set the colormap on application windows so that
    // there is no flashing when there is a private colormap
    Tcl_CreateCommand(interp, "rtd_set_cmap", RtdImage::rtd_set_cmap, NULL, NULL);
    
    // clean up shared memory on exit
    signal(SIGINT, RtdImage_cleanup);
    signal(SIGTERM, RtdImage_cleanup);
    // ignore floating point exceptions
    //sigignore(SIGFPE);
    signal(SIGFPE, SIG_IGN);  // 11.9.97: allan, need this for linux
    Tk_CreateEventHandler(Tk_MainWindow(interp), DestroyNotify, destroy_notify, NULL);

    // The rtd_library path can be found in several places.  Here is the order
    // in which the are searched.
    //		1) the variable may already exist
    //		2) env array
    //		3) the compiled in value of RTD_LIBRARY
    char* libDir = Tcl_GetVar(interp, "rtd_library", TCL_GLOBAL_ONLY);
    if (libDir == NULL) {
	libDir = Tcl_GetVar2(interp, "env", "RTD_LIBRARY", TCL_GLOBAL_ONLY);
    }
    if (libDir == NULL) {
	libDir = RTD_LIBRARY;
    }

    // Set the global Tcl variables rtd_library and rtd_version 
    // and add rtd_library to the auto_path.
    Tcl_SetVar(interp, "rtd_library", libDir, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "rtd_version", RTD_VERSION, TCL_GLOBAL_ONLY);

    // see expr(n) for a description of this global Tcl variable
    Tcl_SetVar(interp, "tcl_precision", "17", TCL_GLOBAL_ONLY);

    // initialize the rtdrecorder and rtdplayback image types
    RtdrecordInit(interp);

    // append the rtd library dir to the auto_path variable
    char cmd[1048];
    sprintf(cmd, "lappend auto_path %s", libDir);
    if (Tcl_Eval(interp, cmd) != TCL_OK)
	return TCL_ERROR;

    // set up the namespaces used by the itcl/itk classes
    if (Tcl_Eval(interp, 
#if (TCL_MAJOR_VERSION >= 8)
		 "namespace eval rtd {namespace export *}; "
		 "namespace import -force rtd::*; "
#else
		 "namespace ::rtd {}; "
		 "import add rtd; "
#endif
	) != 0)
	return TCL_ERROR;

    return TCL_OK; 
}


/*
 * This static method is called by the Tk image code to create
 * a new rtdimage image.
 */
int RtdImage::CreateImage(
    Tcl_Interp *interp,		// Interpreter for application containing image.
    char *name,			// Name to use for image.
    int argc,			// Number of arguments.
    char **argv,		// Argument strings for options (not including image name or type)
    Tk_ImageType *typePtr,	// Pointer to our type record (not used). 
    Tk_ImageMaster master,	// Token for image, to be used by us in later callbacks.
    ClientData *clientDataPtr)	// Store manager's token (this ptr) for image here
				// it will be returned in later callbacks.
{
    RtdImage* im = new RtdImage(interp, name, argc, argv, master, rtdImageType.name,
				configSpecs_, new RtdImageOptions);
    if (im && im->status() == TCL_OK) {
	*clientDataPtr = (ClientData) im;
	return im->initImage(argc, argv);
    }
    return TCL_ERROR;
}


/* 
 * for backward compat.
 */
extern "C"
int RtdImage_Init(Tcl_Interp* interp)  
{
    return Rtd_Init(interp);
}


/*
 * Constructor: initialize a new rtdimage with the command line args.
 * 
 * This constructor is called for each rtd image declared in tk. The destructor
 * is called when the image image is destroyed. 
 *
 * Args: 
 *   interp          -  Tk interpreter
 *   instname        - instance name of image object
 *   arc, argv       - command line args
 *   master          - Tk struct with image info
 *   imageType       - name of image type ("rtdimage")
 *   specs, options  - optional args to be used by derived class.
 *
 * The last 2 arguments are optional. They are designed to be used by a derived 
 * class to add new image options. If "specs" is specified, then the
 * initImage() method must also be called by the caller to process the options,
 * otherwise initImage() is called here (for backward compatibility).
 */
RtdImage::RtdImage(Tcl_Interp* interp, const char* instname, int argc, char** argv, 
		   Tk_ImageMaster master, const char* imageType,
		   Tk_ConfigSpec* specs, RtdImageOptions* options)
: TkImage(interp, imageType, instname, 
	  (specs ? specs : configSpecs_), 
	  (options ? options : (options = new RtdImageOptions)), 
	  master, "Canvas"),
  image_((ImageData*)NULL),
  options_(options),
  camera_(NULL),
  cameraPreCmd_(NULL),
  cameraPostCmd_(NULL),
  remote_(NULL),
  frameId_(0), 
  frameX_(0.0), frameY_(0.0),
  xOffset_(0.0), yOffset_(0.0),
  rapidX_(0.0), rapidY_(0.0),
  reqWidth_(0.0), reqHeight_(0.0),
  propagateScale_(1),
  autoSetCutLevels_(1),
  rapidFrame_(0),
  canvasX_(0), canvasY_(0), prevX_(0), prevY_(0),
  zoomer_(NULL),
  motionX_(0),
  motionY_(0),
  saveMotion_(1),
  motionPending_(0),
  motionState_(0),
  zoomView_(NULL),
  zoomView2_(NULL),
  zoomFactor_(1),
  zoomSpeed_(1),
  panFactor_(0),
  panCommand_(NULL),
  panx1_(0), pany1_(0), panx2_(0), pany2_(0),
  displayLocked_(0),
  canvas_(NULL),
  canvasName_(NULL),
  haveXShm_(0),
  usingXShm_(0),
  haveXSync_(0),
  usingXSync_(0),
  xImage_(NULL),
  viewMaster_(NULL),
  currentView_(this),
  pixTab_(NULL),
  GENtime_(0.),
  TCLtime_(0.),
  MEMtime_(0.),
  Xtime_(0.),
  intPerfTest_(0),
  perfTestType(TIME)
{
    // errors may have occured in the base class ... 
    if (status() != TCL_OK)
	return;
 
    // if we are on the console, check for XSHM, X shared memory extension
    char hostname[64];
    gethostname(hostname, sizeof(hostname));
    int n = strlen(hostname);
    char* display = DisplayString(display_);
    if (display[0] == ':' || 
	(strncmp(hostname, display, n) == 0 && display[n] == ':')) {
	haveXShm_ = XShmQueryExtension(display_);
    }

#ifdef DEBUG
    if (verbose()) {
        int num;
        char **exts;
        exts = XListExtensions(display_, &num);
        fprintf(stderr, "Extension list:-\n");
        for (int k = 0; k < num; k++) {
            fprintf(stderr, "Extension %d is %s\n", k, exts[k]);
        }

        XFreeExtensionList(exts);
    }
#endif

#if _HAVE_R6
    // Check if we have the Sync extension.
    int ev_br, er_br;           // event_base_return, error_base_return.
    int mjr_opcode;             // major opcode.
    int mj_vr, mn_vr;           // Major/minor version number.
    haveXSync_ = XQueryExtension(display_, "SYNC", &mjr_opcode, &ev_br, &er_br);

    // Initialise if we haven't and it isn't already.
    if (haveXSync_) {
        if (!usingXSync_) { 
            if (XSyncInitialize(display_, &mj_vr, &mn_vr)) {
                usingXSync_ = 1;
            }
        }
    }
#endif          // _HAVE_R6

#ifdef DEBUG
    if (verbose()) {
	if (haveXShm_) {
	    cout << "X Shared memory is supported\n";
	}
	else {
	    cout << "X Shared memory is not supported\n";
	}
        if (haveXSync_) {
            cout << "X Synchronisation is supported\n";
        }
        else {
            cout << "X Synchronisation is not supported\n";
        }
    }
#endif

    // initialize list of views
    for(int i = 0; i < MAX_VIEWS; i++)
	view_[i] = NULL;

    // Do first time image configuration. Note: it is better to call this
    // method from outside the constructor, so that virtual methods can be
    // overridden. This is kept here for backward compatibility, if "options"
    // is NULL (default).
    if (! specs)
	initImage(argc, argv);
}


/*
 * destructor - clean up image when image cmd is destroyed
 */
RtdImage::~RtdImage()
{
#ifdef DEBUG
    if (verbose()) 
	printf("RtdImage::~RtdImage(): deleting %s (%s)\n", instname(), name());
#endif

    // there might be more than one image in a canvas window
    // Tk_DeleteEventHandler(tkwin_, ButtonMotionMask|StructureNotifyMask, 
    // eventProc, (ClientData)this);

    // if this is a view, remove it from the master's list
    if (viewMaster_) {
	if (viewMaster_->currentView_ == this)
	    viewMaster_->currentView_  = viewMaster_;
	viewMaster_->removeView(this);
	viewMaster_ = NULL;
	zoomer_ = NULL;
    }

    // reset this static pointer if it points to this instance
    if (motionView_ == this)
	motionView_ = NULL;

    if (image_) {
	delete image_;
	image_ = NULL;
    }

    deleteXImage();

    if (zoomer_) {
	Tk_CancelIdleCall(motionProc, (ClientData)this);
	delete zoomer_;
	zoomer_ = NULL;
    }

    if (panCommand_) {
	free(panCommand_);
	panCommand_ = NULL;
    }

    if (camera_) {
	delete camera_;
	camera_ = NULL;
    }

    if (cameraPreCmd_) {
	free(cameraPreCmd_);
	cameraPreCmd_ = NULL;
    }

    if (cameraPostCmd_) {
	free(cameraPostCmd_);
	cameraPostCmd_ = NULL;
    }

    if (remote_) {
	delete remote_;
	remote_ = NULL;
    }

    if (pixTab_) {
	delete pixTab_;
	pixTab_ = NULL;
    }

    // remove any views of this image
    removeViews();
}



/*
 * Call the given method in this class with the given arguments
 * If the method is not defined here, pass on the search to the
 * parent class. Since this is a virtual function, the search starts
 * in the most specific class.
 */
int RtdImage::call(const char* name, int len, int argc, char* argv[])
{
    // since this tcl command has a lot of subcommands, 
    // we do a binary search on the method table
    int low = 0, 
	high = sizeof(subcmds_)/sizeof(*subcmds_) - 1,
	mid, 
	cond;

    while (low <= high) {
	mid = (low + high) / 2;
	if ((cond = strncmp(name, subcmds_[mid].name, len)) < 0) 
	    high = mid - 1;
	else if (cond > 0)
	    low = mid + 1;
	else {
	    RtdImageSubCmds& t = subcmds_[mid];
	    if (check_args(name, argc, t.min_args, t.max_args) != TCL_OK)
		return TCL_ERROR;
	    return (this->*t.fptr)(argc, argv);
	}
    }
    
    // not found ? extend search to parent class
    return TkImage::call(name, len, argc, argv);
}


/*
 * initialize the color visual and colormap
 */
int RtdImage::initColors(Tcl_Interp* interp)
{
    // we should only do this once
    if (colors_)
	return TCL_OK;

    int depth = 8;  // default, will be set by Tk_GetVisual below
    Colormap colormap;

    // make sure the window exists now before setting the colormap
    Tk_Window tkwin = Tk_MainWindow(interp);
    
    //  XXX these should be options: min and max number of colors to allocate.
    // If this many are not available, use private colormap.
    int min_colors = 30, max_colors = 60;
    
    // Use the default visual for now...
    //    Visual* visual = Tk_GetVisual(interp, tkwin, "default", &depth, &colormap);

    //  PWD: use "." here as "default" returns screen visual, not the
    //  "-visual" command-line option. Making this function static has 
    //  somw drawbacks (the tkwin was previously the parent of the image?).
    Visual* visual = Tk_GetVisual(interp, tkwin, ".", &depth, &colormap);
    if (! visual)
      return TCL_ERROR;
    
    //  PWD: remove to support non-8 bit displays
    //     // if its 8-bit color, make sure it is pseudocolor. 
    //     // XXX Is this too much of a restriction?
    //     if (depth <= 8) {
    // 	visual = Tk_GetVisual(interp, tkwin, "pseudocolor 8", &depth, &colormap);
    // 	if (! visual)
    // 	    return TCL_ERROR;
    //     }

    Tk_MakeWindowExist(tkwin);
    colors_ = new ImageColor(Tk_Display(tkwin), visual, depth, max_colors);
    if (colors_->status() != 0) {
	return TCL_ERROR;
    }
    if (colors_->colorCount() < min_colors) {
	if (colors_->usePrivateCmap() || colors_->allocate(max_colors)) {
	    return TCL_ERROR;
	}
	return colors_->setColormap(tkwin);
    }
    return TCL_OK;
}


/*
 * Utility method to change the equinox of ra and dec
 * from in_quinox to out_equinox, if dist_flag is 0.
 */
void RtdImage::changeEquinox(int dist_flag, double& ra, double& dec, 
			     double in_equinox, double out_equinox)
{
    if (!dist_flag) {
	if (in_equinox != out_equinox) {
	    WorldCoords wcs(ra, dec, in_equinox);
	    wcs.get(ra, dec, out_equinox);
	}
    }
}


/*
 * return the enum CoordinateType value given the string name
 */
 RtdImage::CoordinateType RtdImage::getCoordinateType(const char* s)
{
    switch (*s) {
    case 'i':
	return CT_IMAGE;
    case 's':
	return CT_SCREEN;
    case 'w':
	return CT_WCS;
    case 'd':
	return CT_DEG;
    case 'c':
	int n = strlen(s);
	if (strncmp(s, "canvas", n) == 0)
	    return CT_CANVAS;
	if (strncmp(s, "chip", n) == 0)
	    return CT_CHIP;
    }
    error("unknown coord type: ", s);
    return CT_NONE;
}


/*
 * This method converts between different coordinate representations
 * where the coordinates are passed in string form.
 *
 * If dist_flag is non-zero, the coords are treated as a distance,
 * otherwise as a point.
 *
 * inx_buf and iny_buf hold the input coords (or distance) in the given
 * input coordinate system. If outx_buf and outy_buf are not NULL, they
 * hold the string form of the resulting coordinates in the target (out)
 * coordinate system.  The decimal result values are written to "x" and
 * "y".
 *
 * The return value is the Tcl status.
 *
 * The available coordinate systems are:
 *
 *     canvas     - canvas coordinates (canvas scroll area)
 *     screen     - canvas window coords (visible area)
 *     image      - basic image pixel coords (at mag 1, no transformations)
 *     chip       - detector chip coordinates
 *     wcs        - world coordinates in H:M:S
 *     deg        - world coordinates in degrees
 *
 * The world coordinate types: "wcs" and "deg" may also include the equinox,
 * for example: "wcs 1950" or "deg 2000". The default equinox is 2000.
 *
 * Note: the coordinate types may be abbrieviated.
 */
int RtdImage::convertCoordsStr(int dist_flag, const char* inx_buf, const char* iny_buf,
			       char* outx_buf, char* outy_buf, 
			       double& x, double& y,
			       const char* in_type, const char* out_type)
{
    char in = *in_type, out = *out_type;
    
    if (outx_buf)
	outx_buf[0] = '\0';
    if (outy_buf)
	outy_buf[0] = '\0';

    // get x and y as doubles
    if (in == 'w') { 
	// convert H:M:S to degrees
	WorldCoords wcs(inx_buf, iny_buf);
	if (wcs.status() != TCL_OK)
	    return TCL_ERROR;
	x = wcs.ra_deg();
	y = wcs.dec_deg();
    }
    else {
	if (Tcl_GetDouble(interp_, (char*)inx_buf, &x) != TCL_OK 
	    || Tcl_GetDouble(interp_, (char*)iny_buf, &y) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
 
    if (convertCoords(dist_flag, x, y, in_type, out_type) != TCL_OK)
	return TCL_ERROR;
    
    // format world coords in h:m:s if needed
    if (out == 'w' && outx_buf && outy_buf) { 
	WorldCoords wcs(x, y);
	wcs.print(outx_buf, outy_buf);
    }
    else {
	if (outx_buf)
	    sprintf(outx_buf, "%.17g", x);
	if (outy_buf)
	    sprintf(outy_buf, "%.17g", y);
    }
    return TCL_OK;
}


/*
 * apply the current transformations to the given coordinates
 * If distFlag is 1, x and y are treated as a distance, otherwise
 * they are treated as a point and flipped as needed.
 */
void RtdImage::doTrans(double& x, double& y, int distFlag)
{
    if (distFlag) {
	image_->doTrans(x, y, distFlag);
    }
    else if (viewMaster_) {
	if (viewMaster_->tkwin_ == tkwin_) { 
	    viewMaster_->doTrans(x, y, distFlag);
	}
	else {
	    image_->doTrans(x, y, distFlag, rapidX_, rapidY_, 
			    viewMaster_->image_->width(), viewMaster_->image_->height());
	}
    }
    else {
	image_->doTrans(x, y, distFlag, rapidX_, rapidY_);
    }
}


/* 
 * undo the current transformations on the given coordinates.
 * If distFlag is 1, x and y are treated as a distance, otherwise
 * they are treated as a point and flipped as needed.
 */
void RtdImage::undoTrans(double& x, double& y, int distFlag)
{
    if (distFlag) {
	image_->undoTrans(x, y, distFlag);
    }
    else if (viewMaster_) {
	if (viewMaster_->tkwin_ == tkwin_) { 
	    viewMaster_->undoTrans(x, y, distFlag);
	}
	else {
	    image_->undoTrans(x, y, distFlag, rapidX_, rapidY_, 
			    viewMaster_->image_->width(), viewMaster_->image_->height());
	}
    }
    else {
	image_->undoTrans(x, y, distFlag, rapidX_, rapidY_);
    }
}


/*
 * convert x,y image coords to a distance
 */
void RtdImage::coordsToDist(double& x, double& y)
{
    // for image frame in same window, use master coords
    if (viewMaster_) {
	if (viewMaster_->tkwin_ == tkwin_) 
	    viewMaster_->coordsToDist(x, y);
	else 
	    image_->coordsToDist(x, y, viewMaster_->image_->width(), viewMaster_->image_->height());
    }
    else {
	image_->coordsToDist(x, y);
    }
}


/*
 * convert x,y distance to coordinates
 */
void RtdImage::distToCoords(double& x, double& y)
{
    // for image frame in same window, use master coords
    if (viewMaster_) {
	if (viewMaster_->tkwin_ == tkwin_) 
	    viewMaster_->distToCoords(x, y);
	else 
	    image_->distToCoords(x, y, viewMaster_->image_->width(), viewMaster_->image_->height());
    }
    else {
	image_->distToCoords(x, y);
    }
}


/*
 * convert canvas to screen coords
 */
int RtdImage::canvasToScreenCoords(double& x, double& y, int dist_flag)
{
    if (!dist_flag) {
	x += canvasX_;
	y += canvasY_;
    }
    return TCL_OK;
}


/*
 * convert canvas to image coords
 *
 * Note: there are 2 cases to handle here: 
 * 1. a frame displaying an image at an offset with the origin at 0,0
 * 2. same as above, but with the origin at xOffset,yOffset
 * 
 * In the normal case, the offset and origin are both zero...
 */
int RtdImage::canvasToImageCoords(double& x, double& y, int dist_flag)
{
    if (!dist_flag) {
	double dx = xOffset_, dy = yOffset_;
	doTrans(dx, dy, 1);
	if (frameX_ == 0)
	    x += dx;
	if (frameY_ == 0)
	    y += dy;
    }
    undoTrans(x, y, dist_flag);
    return TCL_OK;
}


/*
 * convert canvas to world coords
 */
int RtdImage::canvasToWorldCoords(double& x, double& y, int dist_flag)
{
    return canvasToImageCoords(x, y, dist_flag) ||
	imageToWorldCoords(x, y, dist_flag);
}


/*
 * convert screen to canvas coords
 */
int RtdImage::screenToCanvasCoords(double& x, double& y, int dist_flag)
{
    if (!dist_flag) {
	x -= canvasX_;
	y -= canvasY_;
    }
    return TCL_OK;
}


/*
 * convert screen to image coords
 */
int RtdImage::screenToImageCoords(double& x, double& y, int dist_flag)
{
    return screenToCanvasCoords(x, y, dist_flag) ||
	canvasToImageCoords(x, y, dist_flag);
}


/*
 * convert screen to world coords
 */
int RtdImage::screenToWorldCoords(double& x, double& y, int dist_flag)
{
    return screenToImageCoords(x, y, dist_flag) ||
	imageToWorldCoords(x, y, dist_flag);
}


/*
 * convert image to canvas coords
 */
int RtdImage::imageToCanvasCoords(double& x, double& y, int dist_flag)
{
    doTrans(x, y, dist_flag);
    if (!dist_flag) {
	double dx = xOffset_, dy = yOffset_;
	doTrans(dx, dy, 1);
	if (frameX_ == 0)
	    x -= dx;
	if (frameY_ == 0)
	    y -= dy;
    }
    return TCL_OK;
}


/*
 * convert image to screen coords
 */
int RtdImage::imageToScreenCoords(double& x, double& y, int dist_flag)
{
    return imageToCanvasCoords(x, y, dist_flag) ||
	canvasToScreenCoords(x, y, dist_flag);
}


/*
 * convert image to world coords in the equinox of the image
 */
int RtdImage::imageToWorldCoords(double& x, double& y, int dist_flag)
{
    double ra, dec;
    if (dist_flag) {
	if (image_->wcs().pix2wcsDist(x, y, ra, dec) != 0) 
	    return TCL_ERROR;
    }
    else {
	if (image_->wcs().pix2wcs(x, y, ra, dec) != 0) 
	    return TCL_ERROR;
    }

    x = ra;
    y = dec;

    return TCL_OK;
}


/*
 * convert world to canvas coords
 */
int RtdImage::worldToCanvasCoords(double& x, double& y, int dist_flag)
{
    return worldToImageCoords(x, y, dist_flag) || 
	imageToCanvasCoords(x, y, dist_flag);
}


/*
 * convert world to screen coords
 */
int RtdImage::worldToScreenCoords(double& x, double& y, int dist_flag)
{
    return worldToCanvasCoords(x, y, dist_flag) ||
	canvasToScreenCoords(x, y, dist_flag);
}


/*
 * convert world to image coords
 */
int RtdImage::worldToImageCoords(double& x, double& y, int dist_flag)
{
    double ra = x, dec = y;
    if (dist_flag) {
	if (image_->wcs().wcs2pixDist(ra, dec, x, y) != 0) 
	    return TCL_ERROR;
    }
    else if (image_->wcs().wcs2pix(ra, dec, x, y) != 0) 
	return TCL_ERROR;

    return TCL_OK;
}


int RtdImage::imageToChipCoords(double& x, double& y, int dist_flag)
{
    if (! dist_flag)
	image_->imageToChipCoords(x, y);
    return TCL_OK;
}


int RtdImage::canvasToChipCoords(double& x, double& y, int dist_flag)
{
    return canvasToImageCoords(x, y, dist_flag)
	|| imageToChipCoords(x, y, dist_flag);
}


int RtdImage::screenToChipCoords(double& x, double& y, int dist_flag)
{
    return screenToImageCoords(x, y, dist_flag)
	|| imageToChipCoords(x, y, dist_flag);
}


int RtdImage::worldToChipCoords(double& x, double& y, int dist_flag)
{
    return worldToImageCoords(x, y, dist_flag)
	|| imageToChipCoords(x, y, dist_flag);
}


int RtdImage::chipToImageCoords(double& x, double& y, int dist_flag)
{
    if (! dist_flag) 
	image_->chipToImageCoords(x, y);
    return TCL_OK;
}


int RtdImage::chipToCanvasCoords(double& x, double& y, int dist_flag)
{
    return chipToImageCoords(x, y, dist_flag)
	|| imageToCanvasCoords(x, y, dist_flag);
}


int RtdImage::chipToScreenCoords(double& x, double& y, int dist_flag)
{
    return chipToImageCoords(x, y, dist_flag)
	|| imageToScreenCoords(x, y, dist_flag);
}


int RtdImage::chipToWorldCoords(double& x, double& y, int dist_flag)
{
    return chipToImageCoords(x, y, dist_flag)
	|| imageToWorldCoords(x, y, dist_flag);
}


/*
 * set X and Y to values between 0 and $xyscale, where $xyscale
 * is the scale factor. The values indicate the fraction of the
 * pixel at the point px,py based on the current scale factor.
 * i.e.: if the scale factor is 10, x,y = 5,5 means px,py is at
 * the center of the zoomed pixel.
 */
void RtdImage::getOffsetInXImage(double px, double py, int& x, int& y)
{
    int xyscale = image_->xScale();
    if (xyscale > 1) {
	x = int((px-int(px))*xyscale);
	y = int((py-int(py))*xyscale);

	// printf("%s: getOffsetInXImage: (%.17g,%.17g) ==> (%d,%d) ", instname(), px, py, x, y);

	if (px < 0) 
	    x += xyscale;
	if (py < 0)
	    y += xyscale;
	if (image_->rotate())
	    swap(x, y);
	
	// printf("==> (%d,%d)\n", x, y);
    }
}


/*
 * convert Ximage to image coords
 */
int RtdImage::xImageToImageCoords(double& x, double& y, int dist_flag)
{
    double dx = xOffset_, dy = yOffset_;
    doTrans(dx, dy, 1);
    x += dx;
    y += dy;
    undoTrans(x, y, dist_flag);
    return TCL_OK;
}


/*
 * convert main image coordinates to raw image array coordinates.
 *
 * The resulting coordinates can be used to index into the raw image
 * array, which might begin at a logical offset other than 0,0. This is
 * only neeeded for rapid frames, since these display coordinates for the
 * main image, but have smaller data arrays that start at some x,y offset.
 */
int RtdImage::imageToRawImageCoords(double& x, double& y)
{
    if (rapidFrame_) {		// only rapid frames need this...
	// get offset for rapid frame in main image:
	// (only one of rapidX_,frameX_ etc. will be non-zero...)
	double dx = (rapidX_ + frameX_), dy = (rapidY_ + frameY_);

	if (image_->flipY())
	    y -= dy;
	else
	    y -= viewMaster_->image_->height() - image_->height() - dy;
	
	if (image_->flipX())
	    x -= viewMaster_->image_->width() - image_->width() - dx;
	else
	    x -= dx;
    }

    return 0;
}


/*
 * convert screen to Ximage coords (index in X Image)
 */
int RtdImage::screenToXImageCoords(double& x, double& y)
{
    if (displaymode() == 0) {
	x -= canvasX_;
	y -= canvasY_;
    } 
    else {
	// if the xImage smaller than the window, add the neg. scroll offset
	double fx = frameX_, fy = frameY_;
	doTrans(fx, fy, 1);

	if (canvasX_ > 0)
	    x += -canvasX_ - fx;
	else if (fx)
	    x -= (fx + canvasX_) ;

	if (canvasY_ > 0)
	    y += -canvasY_ - fy;
	else if (fy)
	    y -= (fy + canvasY_);
	
    }
    return TCL_OK;
}


/*
 * NOTE: This method should not be called. It is only for backward
 * compatibility.
 *
 * Convert inx,iny in the given coord system to outx,outy in the given
 * output coord system.  The coord types here are single char
 * abbrieviations:
 *
 * 	c = canvas coords
 * 	i = image coords
 * 	s = screen coords
 * 	w = world coords (in the equinox of the image)
 * 	d = world coords in degrees (in the equinox of the image)
 *
 * For backward compatibility, this method just calls the more general
 * version, which checks if the equinox needs to be changed.
 */
int RtdImage::convertCoords(int dist_flag, double& x, double& y, char in_type, char out_type)
{
    char in[2], out[2];
    in[0] = in_type;
    in[1] = '\0';
    out[0] = out_type;
    out[1] = '\0';
    return convertCoords(dist_flag, x, y, in, out);
}


/*
 * Convert inx,iny in the given coord system to outx,outy in the given
 * output coord system.  The coord types are the same as for
 * convertCoordsStr():
 *
 *     canvas     - canvas coordinates (canvas scroll area)
 *     screen     - canvas window coords (visible area)
 *     image      - basic image pixel coords (at mag 1, no transformations)
 *     chip       - detector chip or CCD coordinates
 *     wcs        - world coordinates in H:M:S
 *     deg        - world coordinates in degrees
 *
 * The world coordinate types: "wcs" and "deg" may also include the equinox,
 * for example: "wcs 1950" or "deg 2000". The default equinox is 2000.
 *
 * To convert coordinates we have to take some or all of the following
 * into account:
 *
 * 1. Transformations: rotate, scale, flipX,Y 
 * 	These handled by the ImageData methods doTrans() and
 * 	undoTrans().
 *
 * 2. Scrolling offsets in canvas
 * 	member vars: canvasX_ and canvasY
 *
 * 3. Origin of image in image coords
 * 	member vars: frameX_, frameY_
 *
 * 4. Image coordinates at origin:
 * 	member vars: xOffset_, yOffset_
 *
 * For "chip" coordinates, special fields in the real-time image events,
 * or for files, the FITS keywords: HIERARCH ESO DET WIN STRX and
 * STRY indicate the chip origin. If this information is not available,
 * then chip coordinates are the same as image coordinates.
 */
int RtdImage::convertCoords(int dist_flag, double& x, double& y, 
			    const char* in_type, const char* out_type)
{
    CoordinateType in = getCoordinateType(in_type),
	out = getCoordinateType(out_type);

    if (in == CT_NONE || out == CT_NONE)
	return TCL_ERROR;

    if (in == out) 
	return TCL_OK;

    double in_equinox = 2000.0, out_equinox = 2000.0;

    char* msg = "unknown coordinate type";

    switch(in) {

    case CT_CANVAS:			// input is canvas coords
	switch(out) {
	case CT_SCREEN:		// convert canvas to screen coords
	    return canvasToScreenCoords(x, y, dist_flag);
	case CT_IMAGE:		// convert canvas to image coords
	    return canvasToImageCoords(x, y, dist_flag);
	case CT_CHIP:		// convert canvas to chip coords
	    return canvasToChipCoords(x, y, dist_flag);
	case CT_WCS:		// convert canvas to world coords
	case CT_DEG:
	    if (canvasToWorldCoords(x, y, dist_flag) == TCL_OK) {
		sscanf(out_type, "%*[^0-9]%lf", &out_equinox);
		changeEquinox(dist_flag, x, y, image_->wcs().equinox(), out_equinox);
		return TCL_OK;
	    }
	    return TCL_ERROR;
	default:
	    return error(msg);
	}
	break;

    case CT_SCREEN:			// input is screen coords
	switch(out) {
	case CT_CANVAS:		// convert screen to canvas coords
	    return screenToCanvasCoords(x, y, dist_flag);
	case CT_IMAGE:		// convert screen to image coords
	    return screenToImageCoords(x, y, dist_flag);
	case CT_CHIP:		// convert screen to chip coords
	    return screenToChipCoords(x, y, dist_flag);
	case CT_WCS:		// convert screen to world coords
	case CT_DEG:
	    if (screenToWorldCoords(x, y, dist_flag) == TCL_OK) {
		sscanf(out_type, "%*[^0-9]%lf", &out_equinox);
		changeEquinox(dist_flag, x, y, image_->wcs().equinox(), out_equinox);
		return TCL_OK;
	    }
	    return TCL_ERROR;
	default:
	    return error(msg);
	}
	break;

    case CT_IMAGE:			// input is image coords
	switch(out) {
	case CT_CANVAS:		// convert image to canvas coords
	    return imageToCanvasCoords(x, y, dist_flag);
	case CT_SCREEN:		// convert image to screen coords
	    return imageToScreenCoords(x, y, dist_flag);
	case CT_CHIP:		// convert image to chip coords
	    return imageToChipCoords(x, y, dist_flag);
	case CT_WCS:		// convert image to world coords
	case CT_DEG:
	    if (imageToWorldCoords(x, y, dist_flag) == TCL_OK) {
		sscanf(out_type, "%*[^0-9]%lf", &out_equinox);
		changeEquinox(dist_flag, x, y, image_->wcs().equinox(), out_equinox);
		return TCL_OK;
	    }
	    return TCL_ERROR;
	default:
	    return error(msg);
	}
	break;

    case CT_CHIP:			// input is chip coords
	switch(out) {
	case CT_CANVAS:		// convert chip to canvas coords
	    return chipToCanvasCoords(x, y, dist_flag);
	case CT_SCREEN:		// convert chip to screen coords
	    return chipToScreenCoords(x, y, dist_flag);
	case CT_IMAGE:		// convert chip to image coords
	    return chipToImageCoords(x, y, dist_flag);
	case CT_WCS:		// convert chip to world coords
	case CT_DEG:
	    if (chipToWorldCoords(x, y, dist_flag) == TCL_OK) {
		sscanf(out_type, "%*[^0-9]%lf", &out_equinox);
		changeEquinox(dist_flag, x, y, image_->wcs().equinox(), out_equinox);
		return TCL_OK;
	    }
	    return TCL_ERROR;
	default:
	    return error(msg);
	}
	break;

    case CT_DEG:			// input is world coords
    case CT_WCS:		
	// convert to image equinox
	sscanf(in_type, "%*[^0-9]%lf", &in_equinox);
	changeEquinox(dist_flag, x, y, in_equinox, image_->wcs().equinox());
	
	switch(out) {
	case CT_CANVAS:		// convert world to canvas coords
	    return worldToCanvasCoords(x, y, dist_flag);
	case CT_SCREEN:		// convert world to screen coords
	    return worldToScreenCoords(x, y, dist_flag);
	case CT_IMAGE:		// convert world to image coords
	    return worldToImageCoords(x, y, dist_flag);
	case CT_CHIP:		// convert world to chip coords
	    return worldToChipCoords(x, y, dist_flag);
	case CT_WCS:		// convert world to world coords
	case CT_DEG:
	    sscanf(out_type, "%*[^0-9]%lf", &out_equinox);
	    changeEquinox(dist_flag, x, y, image_->wcs().equinox(), out_equinox);
	    return TCL_OK;
	default:
	    return error(msg);
	}
	break;
    }

    return TCL_OK;
}


/*
 * This method is called from the RtdImageCamera class to display the
 * image from shared memory.  "info" contains all we need to know about
 * the image and "data" gives access to the shared memory for the image.
 */
int RtdImage::displayImageEvent(const rtdIMAGE_INFO& info, const Mem& data) 
{
    // if this event is for us, display it
    if (info.frameId == frameId_) {
	int status = TCL_ERROR;

        // Reset the performance test data.
        resetPerfTest();

#ifdef DEBUG
	if (verbose()) 
	    printf("%s: got image event\n", name());
#endif

	// update the x and y offsets 
	// (these are normally 0, when the rapid frame has its own shared memory area)
	xOffset_ = info.frameX;
	yOffset_ = info.frameY;
	
	// if the image changed, delete it and create a new one, otherwise reuse
	if (image_ == NULL 
	    || info.xPixels != image_->width() 
	    || info.yPixels != image_->height() 
	    || info.dataType != image_->dataType()) {
	    
#ifdef DEBUG
	    if (verbose())
		printf("%s: new image received: %d x %d\n", 
		       name(), info.xPixels, info.yPixels);
#endif

	    // save previous image parameters so we can restore the settings later
	    ImageDataParams p;
	    if (image_) {
		image_->saveParams(p);
		delete image_; 
		image_ = NULL;
		updateViews();
	    }

	    // make a FitsIO object to represent the new image data and use it to
	    // make a new ImageData object, which manages the image and transformations.
	    Mem header;		// empty header (use wcs command to set wcs keywords)
	    FitsIO* fits = new FitsIO(info.xPixels, info.yPixels, info.dataType, 
				      0.0, 1.0, header, data);
	    if (!fits || fits->status() != 0)
		return TCL_ERROR;

	    image_ = makeImage(fits);
	    if (!image_)
		return TCL_ERROR;

            if (camera_) {
                camera_->timeStamp("INIT_MEM_IMAGELOAD");
            }
            timeInc(&MEMtime_);

	    // set object name to camera name 
	    if (camera_)
		image_->object(camera_->camera()); 
	    
	    //  restore transformations, cut levels, etc from the previous image
	    image_->restoreParams(p, !autoSetCutLevels_);

	    // if the image event contains valid cut levels, use them
	    if (autoSetCutLevels_ && info.lowCut != info.highCut)
		image_->setCutLevels(info.lowCut, info.highCut, 1);

	    // initialize the new image
	    status =  initNewImage();

            if (camera_) {
                camera_->timeStamp("INIT_TCL_INITDRAW");
            }
            timeInc(&TCLtime_);
	}
	else {
	    // reuse the current image and just update the image data
#ifdef DEBUG
	    if (verbose())
		printf("%s: new image data received: %d x %d (size: %d bytes)\n", 
		       name(), info.xPixels, info.yPixels, data.length());
#endif
	    // if the image event contains valid cut levels, use them
	    if (info.lowCut != info.highCut) 
		setCutLevels(info.lowCut, info.highCut, 1, 0);

	    // update the image with the new data
            if (camera_) {
                camera_->timeStamp("MEM_NEWIMAGELOAD");
            }
            timeInc(&MEMtime_);
	    status = updateImageNewData(data);
            if (camera_) {
                camera_->timeStamp("GEN_XPROC");
            }
            timeInc(&Xtime_);
	} 
	
	// now we have the new image...

	// set detector parameters
	image_->startX(info.startX);
	image_->startY(info.startY);
	if (info.binningX > 0)
	    image_->binX(info.binningX);
	if (info.binningY > 0)
	    image_->binY(info.binningY);
	    
	// set WCS info, if available
	if (info.secpix != 0.) {
	    if (image_->wcs().set(info.ra, info.dec, 
				  info.secpix, info.xrefpix, info.yrefpix,
				  info.xPixels, info.yPixels, info.rotate, 
				  info.equinox, info.epoch, info.proj) != 0)
		return TCL_ERROR;
	}

	// update zoom window and panel labels, etc, if necessary
	if (motionView_)
	    motionView_->processMotionEvent();
        if (camera_) {
            camera_->timeStamp("TCL_ZOOMUPDATEVARS");
            timeInc(&TCLtime_);
        }

	// pause here until image is displayed
	updateIdleTasks();
        if (camera_) {
            camera_->timeStamp("X_SYNC");
            timeInc(&Xtime_);
        }

        // Set the performance test variables, if necessary.
        setPerfTestVars();

	return status;
    }
    else {
	// must be a rapid frame event. Pass it on...
	int i = info.frameId-1;
	if (i >= 0 && i < MAX_VIEWS && view_[i] && view_[i]->rapidFrame_) {
	    return view_[i]->displayImageEvent(info, data);
	}
    }
    return TCL_OK;
}


/*
 * This virtual method is invoked indirectly by the Tk image handling
 * routines to draw the image.
 *
 * If displaymode is 0, try to optimize smooth scrolling by copying the
 * entire image to the X server. If using X shared memory, we can do this
 * each time otherwise we use an offscreen pixmap and copy the image
 * there only when needed.
 *
 * if displaymode is 1, optimize memory usage by only allocating space for
 * the visible image.
 */
void RtdImage::displayImage(Drawable d, int imageX, int imageY, 
			    int width, int height,
			    int drawableX, int drawableY)
{
    char buffer[32];

    if (displayLocked_ || ! initialized_ || ! xImage_ || ! xImage_->data() || ! image_)
	return;
    displayLocked_ = 1;

    // get the canvas X,Y offsets
    // Note: the Tk routine below clips the coordinates to "short" range
    // (-32768 .. 32767), which can cause problems in very large images
    // and/or at large magnification. We could access the internal tkCanvas
    // object to get the unclipped int coords, but that would require including
    // private Tk header files that are not normally installed.
    Tk_CanvasWindowCoords(canvas_, 0, 0, &canvasX_, &canvasY_);
    
    if (displaymode() == 0) { 
	// do entire image, as needed
	if (xImage_->usingXShm()) {
	    // with X shared memory, should be fast enough to write directly to X server
	    if (update_pending_)  
		image_->update();
	    xImage_->put(d, imageX, imageY, drawableX, drawableY, width, height);
	}
	else {
	    // not using X shared mem, use an offscreen pixmap
	    if (update_pending_) { 
		image_->update();
		xImage_->put(pm_, 0, 0, 0, 0, dispWidth(), dispHeight());
	    }
	    // do a quick copy within the X server
	    if (pm_)
		XCopyArea(display_, pm_, d, gc_, imageX, imageY, width, height,
			  drawableX, drawableY);
	}
    }
    else {
	// optimize for fast updates, do only visible area of image
	double fx = frameX_, fy = frameY_, dx = xOffset_, dy = yOffset_;
	if (fx || fy)
	    doTrans(fx, fy, 1); // get canvas distance for frameX,Y
	if (dx || dy)
	    doTrans(dx, dy, 1); // get offset for origin in raw image
	int x = max(-canvasX_ - int(fx), 0), y = max(-canvasY_ - int(fy), 0);
	int update = (update_pending_ || x != prevX_ || y != prevY_);
	prevX_ = x;
	prevY_ = y;
	dx += x, dy += y;
	undoTrans(dx, dy, 1); // get image coord x,y offsets in image coords

	// if image is zoomed, adjust for correct fraction of pixel at origin
	int px = 0, py = 0;
	getOffsetInXImage(dx, dy, px, py);

	if (xImage_->usingXShm()) {
	    // with X shared memory, should be fast enough to write directly to X server
	    if (update) 
		image_->updateOffset(dx, dy);
	    xImage_->put(d, imageX-x+px, imageY-y+py, drawableX, drawableY, width, height);
	}
	else {
	    // not using X shared mem, use an offscreen pixmap
	    if (update) { 
		image_->updateOffset(dx, dy);
		xImage_->put(pm_, 0, 0, 0, 0, pixw_, pixh_);
	    }
	    // do a quick copy within the X server
	    if (pm_)
		XCopyArea(display_, pm_, d, gc_, imageX-x+px, imageY-y+py, 
			  width, height, drawableX, drawableY);
	}
    }

    sprintf(buffer, "X_UPD%s", options_->name);
    if (camera_) {
        // then this is the mainImage - add timestamp.
        camera_->timeStamp(buffer);
        timeInc(&Xtime_);
    }
    else if (viewMaster_ && viewMaster_->camera_) {
        // a child image - get the main image camera_ to perform timestamping.
        viewMaster_->camera_->timeStamp(buffer);
        timeInc(&Xtime_);
    }
    
    // notify the panning window, if necessary
    if (panCommand_)
	autoPan();

    // reset flags
    update_pending_ = displayLocked_ = 0;
}

/*
 * This routine is called as part of the interactive performance testing.
 * It evaluates the time between the present and the last timestamp, and
 * increments the variable pointed to be the argument.
 *
 * Arguments:
 *      double *timeval - address of performance indicator to be incremented.
 */
void RtdImage::timeInc(double *timevar)
{
    // Return immediately if the testing is not activated.
    if (!intPerfTest_) {
        return;
    }

    struct timeval currentTime;
    double curTimeStamp;

    // Get the current time.
    gettimeofday(&currentTime, NULL);
    curTimeStamp = (double)currentTime.tv_sec + 
        (double)(currentTime.tv_usec / 1000000.);

    // Increment the appropriate variable.
    (*timevar) += curTimeStamp - lastTimeStamp_;

    // Update the last timestamp.
    lastTimeStamp_ = curTimeStamp;
}

/*
 * This routine is triggered by receipt of an image event, and simply sets the
 * timestamp information for the beginning of a cycle.
 *
 * Arguments:
 *      None.
 */
void RtdImage::resetPerfTest()
{
    if (!intPerfTest_) {
        return;
    }

    struct timeval currentTime;

    // Set the last time stamp for future events.
    gettimeofday(&currentTime, NULL);
    lastTimeStamp_ = (double)currentTime.tv_sec + 
        (double)(currentTime.tv_usec / 1000000.);

    // Reset the performance variables.
    GENtime_ = 0.;
    TCLtime_ = 0.;
    Xtime_ = 0.;
    MEMtime_ = 0.;
}

/*
 * This routine sets the variables of the performance test indicator form,
 * when it is realised. The frequency read-out is considered separately, as
 * it is not always updated.
 *
 * Arguments:
 *      None
 */
void RtdImage::setPerfTestVars()
{
    // Return immediately if the performance testing is not activated.
    if (!intPerfTest_) {
        return;
    }

    struct timeval tmStamp;             // Timestamp information.
    double tm;                          // Current time information.
    static double lastTime = 0.;        // Last time method was called.
    double aveXtime, aveGENtime, aveMEMtime, aveTCLtime; // Accumulated averages
    char freqStr[32];
    char* var = (viewMaster_ ? viewMaster_->instname_ : instname_);

    // Get time and update the frequency display.
    gettimeofday(&tmStamp, NULL);
    tm = tmStamp.tv_sec + (double)tmStamp.tv_usec / 1000000.;

    /*
     * If this is the first image in this performance test then set the initial
     * timestamp. Also increment the image count.
     */
    if (imageCount_++ == 0) {
        initTimeStamp_ = tm;
    }

    // Set the frequency fields if applicable
    if (imageCount_ > 1) {
        // then set the frequency
        sprintf(freqStr, "%8.5lf", 1 / (tm - lastTime));
        Tcl_SetVar2(interp_, var, "FREQ", freqStr, TCL_GLOBAL_ONLY);
        sprintf(freqStr, "%8.5lf", (imageCount_ - 1) / (tm - initTimeStamp_));
        Tcl_SetVar2(interp_, var, "FREQ_AVE", freqStr, TCL_GLOBAL_ONLY);
    }
    lastTime = tm;

    // Set the total time for the image event.
    double TOTtime_ = GENtime_ + MEMtime_ + Xtime_ + TCLtime_;

    // Accumulate times (these are total times over all images).
    accGENtime_ += GENtime_;
    accMEMtime_ += MEMtime_;
    accTCLtime_ += TCLtime_;
    accXtime_ += Xtime_;

    // Average all the totals.
    double aveTOTtime = (accGENtime_ + accMEMtime_ + accTCLtime_ + accXtime_)
        / imageCount_;
    aveGENtime = accGENtime_ / imageCount_;
    aveXtime = accXtime_ / imageCount_;
    aveTCLtime = accTCLtime_ / imageCount_;
    aveMEMtime = accMEMtime_ / imageCount_;

    // Alter the values to pass to the Tcl code depending on the current
    // display mode.
    switch (perfTestType) {
        case NORM_TIME:
            // Normalise the times depending on the image size.
            // Note: only Mem managment and X calls affected.
            Xtime_ /= (image_->data().size() / 1024.);
            aveXtime /= (image_->data().size() / 1024.);
            MEMtime_ /= (image_->data().size() / 1024.);
            aveMEMtime /= (image_->data().size() / 1024.);
            break;
        case PCT_TIME:
            GENtime_ /= (0.01 * TOTtime_);
            aveGENtime /= (0.01 * aveTOTtime);
            MEMtime_ /= (0.01 * TOTtime_);
            aveMEMtime /= (0.01 * aveTOTtime);
            Xtime_ /= (0.01 * TOTtime_);
            aveXtime /= (0.01 * aveTOTtime);
            TCLtime_ /= (0.01 * TOTtime_);
            aveTCLtime /= (0.01 * aveTOTtime);
            break;
        case TIME:
            // No change required for time/image event
            break;
        default:
            // Unknown? Just output a message to stderr
            fprintf(stderr, "Unknown performance test display mode\n");
            break;
    }

    // Set up strings to pass to the Tcl interpreter.
    char genStr[32], memStr[32], xStr[32], tclStr[32], totStr[32];

    sprintf(genStr, "%9.6lf", GENtime_);
    sprintf(memStr, "%9.6lf", MEMtime_);
    sprintf(xStr, "%9.6lf", Xtime_);
    sprintf(tclStr, "%9.6lf", TCLtime_);
    sprintf(totStr, "%9.6lf", TOTtime_);

    // Set the Tcl variables.
    Tcl_SetVar2(interp_, var, "GEN", genStr, TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp_, var, "MEM", memStr, TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp_, var, "XFUNC", xStr, TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp_, var, "TCL", tclStr, TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp_, var, "TOTAL", totStr, TCL_GLOBAL_ONLY);

    // Do the same for the averaged amounts.
    sprintf(genStr, "%9.6lf", aveGENtime);
    sprintf(memStr, "%9.6lf", aveMEMtime);
    sprintf(xStr, "%9.6lf", aveXtime);
    sprintf(tclStr, "%9.6lf", aveTCLtime);
    sprintf(totStr, "%9.6lf", aveTOTtime);

    // Set the Tcl variables.
    Tcl_SetVar2(interp_, var, "GEN_AVE", genStr, TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp_, var, "MEM_AVE", memStr, TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp_, var, "XFUNC_AVE", xStr, TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp_, var, "TCL_AVE", tclStr, TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp_, var, "TOTAL_AVE", totStr, TCL_GLOBAL_ONLY);
}

/*
 * This procedure is called to process an argv/argc list, plus
 * the Tk option database, in order to configure (or reconfigure) 
 * a image.
 * 
 * redefined here to check which changes were made.
 */
int RtdImage::configureImage(int argc, char* argv[], int flags)
{
    if (TkImage::configureImage(argc, argv, flags) != TCL_OK) 
	return TCL_ERROR;
    
    int status = TCL_OK;
    int reset = 0;
    int coordsFlag = 0;

    // note if we are using X shared memory
    usingXShm_ = haveXShm_ && usexshm();

    // find out which options were specified and process them
    // if necessary (note: Tk sets a flag in the config entry when
    // the option is specified. We use the OFFSET macro defined above
    // as an efficient way to compare options)
    for (Tk_ConfigSpec* p=configSpecs_; p->type != TK_CONFIG_END; p++) {
	if (p->specFlags & TK_CONFIG_OPTION_SPECIFIED) {
	    switch(p->offset) {

	    case RTD_OPTION(usexshm):
		if (initialized_) {
		    deleteXImage();
		    reset++;
		}
		break;

#if _HAVE_R6            
            case RTD_OPTION(usexsync):
                if (usingXSync_ && usexsync()) {
#ifdef DEBUF
                    fprintf(stderr, "Raising priority of client %s\n", name());
#endif
                    XSyncSetPriority(display_, None, 65535);
                }
                break;
#endif  // _HAVE_R6
		
	    case RTD_OPTION(displaymode):
	    case RTD_OPTION(shm_header):
	    case RTD_OPTION(shm_data):
		if (initialized_) 
		    reset++;
		break;

	    case RTD_OPTION(fitWidth):
	    case RTD_OPTION(fitHeight):
		if (initialized_) {
		    if (image_ && fitWidth() && fitHeight()) {
			image_->shrinkToFit(fitWidth(), fitHeight());
		    }
		    reset++;
		}
		break;

	    case RTD_OPTION(file):
		status = loadFile();
		break;
	    }
	}
    }
    
    if (reset)
	return resetImage();
    return status;
}


/* 
 * util: return true if this is an embedded rapid frame
 * (i.e.: a rapid frame in the same canvas with the master image)
 */
int RtdImage::isEmbeddedRapidFrame()
{
    return (rapidFrame_ && viewMaster_ && viewMaster_->tkwin_ == tkwin_);
}


/* 
 * util: return true if this is a rapid frame that is not embedded 
 * (i.e.: a rapid frame that is not in the same canvas with the master image)
 */
int RtdImage::isSeparateRapidFrame()
{
    return (rapidFrame_ && viewMaster_ && viewMaster_->tkwin_ != tkwin_);
}


/*
 * Set the cut levels to the given min and max values and update the
 * image and any views of it.  If scaled is 1, the values are assumed yo
 * be already scaled with bzero and bscale.  If user is 1, the call is a
 * result of a user action.
 */
int RtdImage::setCutLevels(double min, double max, int scaled, int user)
{
    // don't overwrite user's cut levels
    if (!user && !autoSetCutLevels_)
	return TCL_OK;

    // check if there is a change from the previous cut levels
    if (scaled && min == image_->lowCut() && max == image_->highCut())
	return TCL_OK; // no change

    image_->setCutLevels(min, max, scaled);
    image_->colorScale(colors_->colorCount(), colors_->pixelval());

    // assume the user has set the cut levels, so we wont change them
    // if a new image is loaded...
    if (user)
	autoSetCutLevels_ = 0;

    // make sure the new lookup table is propagated
    LookupTable lookup = image_->lookup();
    for(int i = 0; i < MAX_VIEWS; i++) {
	if (view_[i] && view_[i]->image_ && !view_[i]->isSeparateRapidFrame()) 
	    view_[i]->image_->lookup(lookup);
    }

    return updateViews(1) || updateImage(); 
}


/*
 * set the X,Y scale factors for the image and propagate to any views
 * of the image as needed.
 */
int RtdImage::setScale(int xScale, int yScale)
{
    // ignore invalid scale values
    if (xScale == 0 || xScale == -1)
	xScale = 1;
    if (yScale == 0 || yScale == -1)
	yScale = 1;
    
    // magnify by zoom factor, if requested
    if (zoomFactor_ > 1) {
	if (xScale > 0) {
	    xScale *= zoomFactor_;
	    yScale *= zoomFactor_;
	}
	else {
	    xScale = yScale = zoomFactor_;
	}
#ifdef XXXDEBUG
	if (verbose()) 
	    printf("%s: setting scale to (%d, %d), factor %d\n",
		   name(), xScale, yScale, zoomFactor_);
#endif
    }

    // ignore if the same scale is now in effect
    int xs = image_->xScale() , ys = image_->yScale();
    if (xScale == xs && yScale == ys) {
	// notify the panning window, if necessary, even if scale didn't change
	// since it may need to update the panning display
	if (panCommand_) {
	    panx1_ = pany1_ = panx2_ = pany2_ = 0;
	    autoPan();
	}
	return TCL_OK;
    }
	
    image_->setScale(xScale, yScale);
    panx1_ = pany1_ = panx2_ = pany2_ = 0; // make sure panning is updated

    if (resetImage() != TCL_OK)
	return TCL_ERROR;

    // also scale any views that don't have a fixed scale
    return updateViews(2);

    return TCL_OK;
}


/*
 * This method is called for new images or whenever the image may
 * have changed size. It updates the size of the pixmap and XImage
 * as needed and arranges to have the image redrawn.
 */
int RtdImage::resetImage()
{
    if (! image_)
	return TCL_OK;

    // only use a pixmap if not using X shared mem
    int w = dispWidth(), h = dispHeight();
    double rw = reqWidth_, rh = reqHeight_;
    doTrans(rw, rh, 1);

    // if an explicit size was requested and it is less than the image
    // size, use it.
    if (rw && rw < w) {
	w = int(rw);
    }
    if (rh && rh < h) {
	h = int(rh);
    }

    // size of XImage and pixmap, if used
    int pixw = w, pixh = h;

    // if displaymode is 1, the image size is not larger than the window size
    if (displaymode() == 1) {
	pixw = Tk_Width(tkwin_);  // note: making this any smaller than the window
	pixh = Tk_Height(tkwin_); // caused problems on HP (X server crash..)

	if (pixw == 1 && pixh == 1)
	    return TCL_OK;	// wait for resize event on image window

	if (w < pixw)		// requested smaller size ?
	    pixw = w;
	if (h < pixh)
	    pixh = h;

	// round up the size to the nearest scale factor multiple (+1?), so that no 
	// empty spaces are left around the edges of the image (allan, 10.10.95)
	int xs = image_->xScale(), ys = image_->yScale();
	if (xs > 1) {
	    pixw += (xs - pixw % xs) + xs;
	    pixh += (ys - pixh % ys) + ys;
	}
    }

    // check min size for X image
    if (pixw <= 0 || pixh <= 0) {
	pixw = pixh = 1;
    }

    // make a new X image if needed and update the size
    if (!xImage_)
	xImage_ = new ImageDisplay(display_, visual_, gc_, depth_, 
				   usingXShm_, verbose());

    if (xImage_->update(pixw, pixh) != TCL_OK) {
	deleteXImage();
	return TCL_ERROR;
    }

    // tell that class that manages the image data about the new xImage data
    image_->setXImage( xImage_ );

    // always set Tk's idea of the image size to the full size,
    // even though the pixmap and XImage may be smaller
    int status = setImageSize(w, h, !xImage_->usingXShm(), pixw, pixh);

    // tell Tk we want to redraw the image
    imageChanged();
    return status;
}


/*
 * Load a Fits image file and display it.
 */
int RtdImage::loadFile()
{
    // -file may have been set to "", just clear image then
    if (strlen(file()) == 0)
	return clearCmd(0, NULL);

    // used to save and restore image transformation parameters
    ImageDataParams p;
    
    if (image_) {
	image_->saveParams(p);
	delete image_; 
	image_ = NULL;
	updateViews();
    }

    // if not '-' (stdin) check that it is a file
    if (strcmp(file(), "-") != 0) { 
	struct stat buf;
	if (stat(file(), &buf) != 0 || S_ISREG(buf.st_mode) == 0)
	    return error("expected a file, but got: ", file());
    }

    // read the FITS image 
    image_ = makeImage(FitsIO::read(file()));
    if (! image_)
	return TCL_ERROR;

    // restore transformations
    image_->restoreParams(p, !autoSetCutLevels_);

    // if sysV shared memory was requested, use it in place of mmap
    if (shm_header())
	image_->header().shared(1);
    if (shm_data())
	image_->data().shared(1);

    return initNewImage();
}


/*
 * this method is called to do all the necessary work
 * after a new image has been loaded from file or shared memory
 */
int RtdImage::initNewImage() 
{
    if (!image_) {
	return updateViews();
    }

    // reset options
    image_->subsample(subsample());
    image_->verbose(verbose());
    if (fitWidth() || fitHeight()) {
	image_->shrinkToFit(fitWidth(), fitHeight());
    }

    image_->colorScale(colors_->colorCount(), colors_->pixelval());

    // update other views
    if (updateViews(1) != TCL_OK)
	return TCL_ERROR;

    if (camera_) {
        camera_->timeStamp("INIT_GEN_IMAGEPROC");
        timeInc(&GENtime_);
    }

    if (resetImage() != TCL_OK)
	return TCL_ERROR;

    // update the panning window if needed
    if (panCommand_) {
	if (Tk_Width(tkwin_) <= 1) {
	    // pause here until window is displayed, so that the pan
	    // window can determine the size of the window
	    updateIdleTasks();
	}
	autoPan(1);
    }

    if (camera_) {
        camera_->timeStamp("INIT_GEN_XPROC");
        timeInc(&Xtime_);
    }
    
    // evaluate the newImageCmd, if there is one
    if (strlen(newImageCmd())) {
	return Tcl_Eval(interp_, newImageCmd());
    }

    return TCL_OK;
}


/*
 * this method causes the image and its views to be redrawn.
 */
int RtdImage::updateImage() 
{
    imageChanged();
    if (image_)
	image_->update_pending(1);
    return updateViews();
}


/*
 * This method is called to update an existing image with new
 * raw data of the same size and type.
 */
int RtdImage::updateImageNewData(const Mem& data) 
{
#ifdef DEBUG
    if (verbose()) 
	printf("%s: update image with new data (size: %d)\n", name(), data.length());
#endif
    
    if (image_)
	image_->data(data);

    // update the data in any views
    for(int i = 0; i < MAX_VIEWS; i++) {
	if (view_[i] && view_[i]->image_ && !view_[i]->rapidFrame_ 
	    && view_[i] != zoomView_ && view_[i] != zoomView2_) {
#ifdef DEBUG
	    if (verbose()) 
		printf("%s: update %s with new data\n", name(), view_[i]->name());
#endif

	    view_[i]->image_->data(data);
	}
    }
    if (camera_) {
        camera_->timeStamp("GEN_IMAGEPROC");
        timeInc(&GENtime_);
    }

    return updateImage();
}


/* 
 * delete the XImage used to display the image
 */
int RtdImage::deleteXImage()
{
    if (xImage_) {
	delete xImage_;
	xImage_ = NULL;
    }
    
    // also reset the pointer in the class that manages the XImage data
    if (image_) 
	image_->setXImage( NULL );

    return 0;
}


/*
 * this method is called in the master image to update all of the
 * views.
 * "flag" is set to 1 if a new image was loaded from file or
 * shared memory, 2 if the image has be scaled (zoomed) and 0
 * otherwise.
 */
int RtdImage::updateViews(int flag)
{
    int status = TCL_OK;
    for(int i = 0; i < MAX_VIEWS; i++) {
	if (view_[i])
	    status |= view_[i]->updateView(image_, flag);
    }
    return status;
}


/*
 * add the given image to the list of views, images
 * that are updated from this one.
 */
int RtdImage::addView(RtdImage* view)
{
    for(int i = 0; i < MAX_VIEWS; i++) {
	if (view_[i] == NULL) {
	    view_[i] = view;
	    view->frameId_ = i+1;
	    view->viewMaster_ = this;
	    if (image_) {
		return view->updateView(image_, 1);
	    }
	    return TCL_OK;
	}
    }
    return error("too many RtdImage views");
}



/*
 * remove the given image from the list of views
 */
int RtdImage::removeView(RtdImage* view)
{
    if (view) {
	for(int i = 0; i < MAX_VIEWS; i++) {
	    if (view_[i] == view) {
		view->viewMaster_ = NULL;
		view_[i]->zoomer_ = NULL;
		view_[i]->zoomView_ = NULL;
		view_[i]->zoomView2_ = NULL;
		view_[i] = NULL;
		return TCL_OK;
	    }
	}
    }
    return error("tried to remove nonexistant RtdImage view");
}


/*
 * remove all views from the view list
 */
void RtdImage::removeViews()
{
    for(int i = 0; i < MAX_VIEWS; i++) {
	if (view_[i]) {
	    view_[i]->viewMaster_ = NULL;
	    view_[i]->zoomer_ = NULL;
	    view_[i]->zoomView_ = NULL;
	    view_[i]->zoomView2_ = NULL;
	    view_[i] = NULL;
	}
    }
}


/*
 * This method is called from outside by the master image to update the
 * view in a slave image. "im" is the master image data, which we make a copy
 * of here (with reference counting, so the actual data is shared)
 *
 * "flag" is set to 1 if a new image was loaded from file or shared
 * memory, 2 if the image has been scaled (zoomed) and 0 otherwise.
 */
int RtdImage::updateView(ImageData* im, int flag)
{
    if (!im) {
	// image was unloaded ?
	if (image_)
	    delete image_;
	image_ = NULL;
	return TCL_OK;
    }
    
    if (image_ && flag != 1) {
	// this is an existing image, update it
	if (flag == 2) {  // only transformations changed
	    if (propagateScale_ == 0) {
		// no change  - scale is fixed
		return TCL_OK;
	    }
	    else {
		// set new x,y scale factors
		return setScale(im->xScale(), im->yScale());
	    }
	}
	return updateImage();
    }

    // save scale factors, if any
    int xs = 0, ys = 0;
    if (image_) {
	xs = image_->xScale();
	ys = image_->yScale();
    }

#ifdef XXXDEBUG
    if (verbose())
	printf("%s: update view from %s (rapid?: %d)\n", name(), im->name(), rapidFrame_);
#endif

    // if its not a rapid frame in a separate window, copy the image from the master image,
    // otherwise just copy the transformations
    if (rapidFrame_) {
	if (image_ && isEmbeddedRapidFrame()) {
	    ImageDataParams p;
	    im->saveParams(p);
	    image_->restoreParams(p, !autoSetCutLevels_);
	}
    }
    else {
	// this is a new image, or it changed dimensions or position in some way.
	// delete old one and copy the new one.
	if (image_)
	    delete image_;
#ifdef XXXDEBUG
	if (verbose())
	    printf("%s: copy data from %s\n", name(), im->name());
#endif
	image_ = im->copy();
	image_->name(name()); // reset the name of the slave image
    }

    if (fitWidth() || fitHeight()) { 
	// used for pan window - shrink to fit whole image in window
	image_->shrinkToFit(fitWidth(), fitHeight());
    }
    else if (zoomFactor_ > 1) {
	// used for zoom view: make scale relative to main image
	if (setScale(im->xScale(), im->yScale()) != TCL_OK) 
	    return TCL_ERROR;
    } else if (xs && propagateScale_ == 0) {
	// if the scale factors should not propagate to the view, restore them here
	image_->setScale(xs, ys);
    }
    
    if (resetImage() != TCL_OK)
	return TCL_ERROR;

    return TCL_OK;
}


/*
 * return a pointer to the RtdImage class object for the
 * given rtdimage tcl instance name, or NULL
 * if the name is not an rtdimage.
 */
RtdImage* RtdImage::getView(char* name)
{
    // check that the argument really is an rtdimage...
    if (strncmp(name, "image", 5) != 0) {
	error("expected an rtdimage id but got: ", name);
	return NULL;
    }

    // get class instance pointer from name
    Tcl_CmdInfo info;
    if (Tcl_GetCommandInfo(interp_, name, &info) == 0) {
	error("expected an \"rtdimage\" type image");
	return NULL;
    }
    return (RtdImage*)info.clientData;
}



/* 
 * This virtual method is called indirectly by the Tk image handling routines
 * for each use of the image in a widget. 
 * It is redefined here so that we can set up an event handler on the instance
 * window for autozooming.
 */
TkImage* RtdImage::getImage(Tk_Window tkwin)
{
    TkImage* ret = TkImage::getImage(tkwin);
    if (! ret)
	return ret;

    // note the name of the canvas window
    canvasName_ = Tk_PathName(tkwin);

    // set up event handler for motion events and resizing
    Tk_CreateEventHandler(tkwin, ButtonMotionMask|StructureNotifyMask, 
			  eventProc, (ClientData)this);

    // get a handle to the canvas and save it for later reference
    Tcl_CmdInfo info;
    if (Tcl_GetCommandInfo(interp_, (char*)canvasName_, &info) == 0) {
	char* msg = "internal error: couldn't get canvas info";
	error(msg);
	fprintf(stderr, "rtd: %s for %s\n", msg, canvasName_);
	Tk_BackgroundError(interp_);
	return NULL;		// this crashes Tk, either way...
    }
    canvas_ = (Tk_Canvas)info.clientData;

    // set colormap if needed for image window and add to window list for later
    colors_->setColormap(tkwin_);

    return ret;
}


/* 
 * This static method is called for motion and configure events 
 * in the image window
 */
void RtdImage::eventProc(ClientData clientData, XEvent* eventPtr)
{
    RtdImage* thisPtr = (RtdImage*)clientData;

    if (thisPtr) {
	if (eventPtr->type == MotionNotify) {
	    // use current view as set by view command, which defaults to this
	    // image, in case more than one image is in canvas
	    (motionView_ = thisPtr->currentView_)->motionNotify(eventPtr);
	} 
	else if (eventPtr->type == ConfigureNotify) {
	    thisPtr->configureNotify(eventPtr);
	}
    }
}


/* 
 * This method is called for MotionNotify events in the image window
 * Arrange for the zoom window to be updated.
 */
void RtdImage::motionNotify(XEvent* eventPtr)
{
    // (eventuallY) update zoom window, if shift button not pressed
    if ((eventPtr->xmotion.state & (ShiftMask | LockMask)) == 0) {

        if (saveMotion_) {
	    motionX_ = eventPtr->xmotion.x;
	    motionY_ = eventPtr->xmotion.y;
        }
	motionState_ = eventPtr->xmotion.state;

	if (!motionPending_) {
	    if (motionState_ == 0 && zoomSpeed_ >= 0) {
		// speed up zoom if no keys or mouse buttons are pressed
		processMotionEvent();
	    }
	    else {
		// slow down zoom if buttons are pressed, or zoomSpeed is
		// negative, since it might drag down the performance for drawing, etc...
		motionPending_ = 1;
		Tk_DoWhenIdle(motionProc, (ClientData)this);
	    }
	}
    }
}


/* 
 * This method is called for ConfigureNotify events in the image window
 * (when the image window is resized).
 * Make sure it is redrawn.
 */
void RtdImage::configureNotify(XEvent* eventPtr)
{
    if (image_ && displaymode() == 1) { 
#ifdef DEBUG
	if (verbose()) 
	    printf("RtdImage::configureNotify: %d, %d\n", 
		   eventPtr->xconfigure.width, eventPtr->xconfigure.height);
#endif
	resetImage();
    }
}


/* 
 * This static method is called, if needed, when there is nothing else to do
 * to update the zoom window and other motion sensitive widgets
 */
void RtdImage::motionProc(ClientData clientData)
{
    RtdImage* thisPtr = (RtdImage*)clientData;
    if (thisPtr) {
	thisPtr->motionPending_ = 0;
	thisPtr->processMotionEvent();
    }
}



/*
 * this virtual method is called for motion events to update the zoom window
 * and set trace variables based on the current mouse position.
 */
void RtdImage::processMotionEvent()
{
    if (image_ && xImage_ && xImage_->data() && image_->width() > 2 && image_->height() > 2) {
	double x = motionX_, y = motionY_;

        timeInc(&GENtime_);

	screenToImageCoords(x, y, 0);

	// there are 2 different kinds of zoom windows implemented...
	if (zoomView_ || zoomView2_) {
	    autoZoomView(x, y);
	}
	else if (zoomer_) {
	    double zx = motionX_, zy = motionY_;
	    screenToXImageCoords(zx, zy);
	    zoomer_->zoom(xImage_->data(), 
			  int(zx), int(zy), 
			  xImage_->bytesPerLine(), xImage_->height(), 
			  image_->xScale(), image_->yScale(),
			  colors_->pixelval(0));
	}

	// update a global Tcl array with the current x, y, pixval, ra, dec, equinox
	// values for speedy trace update in Tk. The array has the same name as 
	// the main image and is indexed by X, Y, VALUE, RA, DEC, EQUINOX.
	char xStr[32], yStr[32], valueStr[32], raStr[32], decStr[32], equinoxStr[32];
	char* var = (viewMaster_ ? viewMaster_->instname_ : instname_);
	double rx = x, ry = y;
	imageToRawImageCoords(rx, ry);
	image_->getValues(x, y, rx, ry, xStr, yStr, valueStr, raStr, decStr, equinoxStr);

	Tcl_SetVar2(interp_, var, "X", xStr, TCL_GLOBAL_ONLY);
	Tcl_SetVar2(interp_, var, "Y", yStr, TCL_GLOBAL_ONLY);
	Tcl_SetVar2(interp_, var, "VALUE", valueStr, TCL_GLOBAL_ONLY);
	Tcl_SetVar2(interp_, var, "RA", raStr, TCL_GLOBAL_ONLY);
	Tcl_SetVar2(interp_, var, "DEC", decStr, TCL_GLOBAL_ONLY);
	Tcl_SetVar2(interp_, var, "EQUINOX", equinoxStr, TCL_GLOBAL_ONLY);
	
	// update the pixtab trace variables, if necessary
	// (ease up when B3 is pressed, due to measure band...)
	if (pixTab_ && (motionState_ & Button1Mask & Button3Mask) == 0) {
	    char indexStr[32];
	    int w = pixTabCols_+1;
	    double d;
	    double sum=0.0, sumsq=0.0, minv, maxv, rms, ave;
	    int npix=0;
	    image_->getValues(x, y, rx, ry, pixTab_, pixTabRows_, pixTabCols_);
	    for(int j = 0; j <= pixTabRows_; j++) {
		for(int i = 0; i <= pixTabCols_; i++) {
		    sprintf(indexStr, "%d,%d", j, i);
		    if ((d = pixTab_[j*w+i]) > -HUGE_VAL) {// not outside image ?
			if (i && j) {
			    sprintf(valueStr, "%g", d);	  // pix value
			    // calculate statistics on pixels
			    if (npix == 0) {
				minv = d;
				maxv = d;
			    }
			    npix++;
			    sum += d;
			    sumsq += d * d;
			    if (d < minv)
				minv = d;
			    if (d > maxv)
				maxv = d;
			}
			else
			    sprintf(valueStr, "%.1f", d); // x,y index
  		    }
		    else {
			*valueStr = '\0'; // pixel is outside of image
		    }
		    Tcl_SetVar2(interp_, var, indexStr, valueStr, TCL_GLOBAL_ONLY);
		}
	    }
	    if (npix > 0) {
		ave = sum / (double)npix;
		sprintf(valueStr, "%g", ave);	  // average
		Tcl_SetVar2(interp_, var, "PIXTAB_AVE", valueStr, TCL_GLOBAL_ONLY);
		sprintf(valueStr, "%g", minv);	  // min
		Tcl_SetVar2(interp_, var, "PIXTAB_MIN", valueStr, TCL_GLOBAL_ONLY);
		sprintf(valueStr, "%g", maxv);	  // max
		Tcl_SetVar2(interp_, var, "PIXTAB_MAX", valueStr, TCL_GLOBAL_ONLY);
		sprintf(valueStr, "%d", npix);	  // npix
		Tcl_SetVar2(interp_, var, "PIXTAB_N", valueStr, TCL_GLOBAL_ONLY);
	    }
	    else {
		Tcl_SetVar2(interp_, var, "PIXTAB_AVE", "\0", TCL_GLOBAL_ONLY);
		Tcl_SetVar2(interp_, var, "PIXTAB_MIN", "\0", TCL_GLOBAL_ONLY);
		Tcl_SetVar2(interp_, var, "PIXTAB_MAX", "\0", TCL_GLOBAL_ONLY);
		Tcl_SetVar2(interp_, var, "PIXTAB_N", "\0", TCL_GLOBAL_ONLY);
	    }
	    if (npix > 1) {
		rms = sqrt((sumsq - ((sum * sum) / npix)) / (npix -1));
		sprintf(valueStr, "%g", rms);	  // rms
		Tcl_SetVar2(interp_, var, "PIXTAB_RMS", valueStr, TCL_GLOBAL_ONLY);
	    }
	    else {
		Tcl_SetVar2(interp_, var, "PIXTAB_RMS", "\0", TCL_GLOBAL_ONLY);
	    }
	}
    }
}


/*
 * this virtual method is called for motion events when 
 * a "zoomview" is being used. x and y are the image
 * coordinates distance from origin.
 */
void RtdImage::autoZoomView(double x, double y)
{
    if (image_) {
	coordsToDist(x, y);
	updateZoomView(zoomView_, x, y);
	updateZoomView(zoomView2_, x, y);
    }
}


/*
 * update the given zoom view. x and y are the image coordinate offsets
 */
void RtdImage::updateZoomView(RtdImage* view, double x, double y)
{
    if (view && view->image_) {
	double rw = view->reqWidth_, rh = view->reqHeight_;
	view->undoTrans(rw, rh, 1); // get only visible area
	view->xOffset_ = x - rapidX_ - rw/2.0 - frameX_;
	view->yOffset_ = y - rapidY_ - rh/2.0 - frameY_;
	view->updateView(image_, 1);
    }
}


/*
 * This method is called to notify the panning window of the coords 
 * of the visible image. If the size or position of the visible part
 * of the image has changed, evaluate the panning command with the
 * new coordinates and a flag indicating whether the image is new or
 * just an update.
 * newImageFlag is set to one if a new image was loaded from file or
 * shared memory.
 */
void RtdImage::autoPan(int newImageFlag) 
{
    int x1 = -canvasX_;
    if (x1 < 0)
    	x1 = 0;

    int y1 = -canvasY_; 
     if (y1 < 0)
	 y1 = 0;

    int w = dispWidth(), h = dispHeight();

    int x2 = x1+Tk_Width(tkwin_)-1; 
    if (x2 >= (w-1))
	x2 = w-1;
    if (x2 <= x1)
	x2 = x1+1;

    int y2 = y1+Tk_Height(tkwin_)-1;
    if (y2 >= (h-1))
	y2 = h-1;
    if (y2 <= y1)
	y2 = y1+1;
    
    // if the coords have changed...
    if (newImageFlag || x1 != panx1_ || y1 != pany1_ || x2 != panx2_ || y2 != pany2_) {
	// save the coords
	panx1_ = x1;
	pany1_ = y1;
	panx2_ = x2;
	pany2_ = y2;

	// make sure the coords are in range and  take the scale factors of 
	// both the pan image and the target image into account
	int xs = image_->xScale(), ys = image_->yScale();

	if (xs > 0) {
	    x1 /= (xs * -panFactor_);
	    x2 /= (xs * -panFactor_);
	}
	else {
	    x1 = (x1 * xs) / panFactor_;
	    x2 = (x2 * xs) / panFactor_;
	}
	if (ys > 0) {
	    y1 /= (ys * -panFactor_);
	    y2 /= (ys * -panFactor_);
	}
	else {
	    y1 = (y1 * ys) / panFactor_;
	    y2 = (y2 * ys) / panFactor_;
	}

	char buf[1024];
	sprintf(buf, "%s %d %d %d %d %d", 
		panCommand_, x1, y1, x2, y2, newImageFlag);
	if (Tcl_Eval(interp_, buf) != TCL_OK) {
	    Tk_BackgroundError(interp_);
	    panCommand_ = NULL;
	}

        // Timestamp this process.
        if (camera_) {
            camera_->timeStamp("INIT_TCL_PANUPDATE");
            timeInc(&TCLtime_);
        }
   }
}

/*
 * Make a new image from the given ImageIO object and return a pointer to
 * a derived class of ImageData specialized in that type of image.
 *
 * Note that pointers to ImageIORep subclasses, such as FitsIO are
 * automatically converted to an ImageIO object through a special
 * constructor. In this way, you can add new image types by deriving a
 * new classes in the same way as the FitsIO class (from ImageIORep).
 *
 * imio - is a reference to an ImageIO object for the image (or ptr, see
 * above).
 */
ImageData* RtdImage::makeImage(ImageIO imio)
{
    return ImageData::makeImage(name(), imio, verbose());
}


// -- image subcommands --


/*
 * Generate and load a blank image. 
 *
 * usage:  $image clear
 *
 *         $image clear ?ximage?
 *
 *         $image clear ?-reuse $reuse 
 *                       -ra $ra -dec $dec -equinox $equinox -radius $radius \
 *                       -width $width -height $height?
 *
 * In the last case, the optional arguments are used to generate a dummy
 * image that supports world coordinates, so that you can plot objects on
 * a blank background. Any missing values are set to a default value.
 *
 * If "clear ximage" is specified, only the XImage is cleared out
 * (temporary clear).
 *
 * Optional args:
 *
 * 	ximage  - flag: if true, only clear out the XImage (temporary clear)
 *
 * 	reuse   - flag: if true, reuse previous image, if it is the same
 * 	ra, dec - center point for WCS coords (in decimal degrees)
 * 	radius  - used to initialize WCS coords (CDELT1 and 2)
 * 	equinox - equinox for WCS coords
 * 	width   - width of generated image in pixels
 * 	height  - height of generated image in pixels
 */
int RtdImage::clearCmd(int argc, char* argv[])
{
    if (argc == 1 && strcmp(argv[0], "ximage") == 0) {
	if (image_) {
	    image_->clear();
	    imageChanged();
	}
	return TCL_OK;
    }

    double ra = -1.0, dec = 0.0, equinox = 2000.0, radius = 1;
    int reuse = 0, width = 2, height = 2;
    
    // parse options
    for (int i = 0; i < argc; i+=2) {
	char* opt = argv[i];
	char* arg = argv[i+1];
	if ((strcmp(opt, "-reuse") == 0 && Tcl_GetBoolean(interp_, arg, &reuse) != TCL_OK)
	    || (strcmp(opt, "-ra") == 0 && Tcl_GetDouble(interp_, arg, &ra) != TCL_OK)
	    || (strcmp(opt, "-dec") == 0 && Tcl_GetDouble(interp_, arg, &dec) != TCL_OK)
	    || (strcmp(opt, "-radius") == 0 && Tcl_GetDouble(interp_, arg, &radius) != TCL_OK)
	    || (strcmp(opt, "-equinox") == 0 && Tcl_GetDouble(interp_, arg, &equinox) != TCL_OK)
	    || (strcmp(opt, "-width") == 0 && Tcl_GetInt(interp_, arg, &width) != TCL_OK)
	    || (strcmp(opt, "-height") == 0 && Tcl_GetInt(interp_, arg, &height) != TCL_OK))
	    return TCL_ERROR;
    }
    
    // if -ra and -dec were specified, generate an image with world coords, otherwise
    // just a plain image with image coords
    if (ra >= 0) {
	// for world coords, to make it easier, use an equal width and height
	width = height = max(width, height);
    
	// see if we can reuse the current image.
	// (only if no file or object is loaded and the coords are the same)
	double image_ra, image_dec, err = .1;
	if (reuse && image_ && strlen(file()) == 0 && strlen(image_->object()) == 0 &&
	    width == image_->width() && height == image_->height() 
	    && fabs(radius - image_->wcs().radius()) < err) {

	    image_->wcs().pix2wcs(width/2, height/2, image_ra, image_dec);
	    if (fabs(ra - image_ra) < err && fabs(dec - image_dec) < err) {
		return TCL_OK;	// its all the same, so reuse it
	    }
	}
    }

    // save previous image parameters so we can restore the settings later
    ImageDataParams p;
    if (image_) {
	image_->saveParams(p);
	delete image_;
	image_ = NULL;
    }
    
    // generate the blank image
    FitsIO* fits = FitsIO::blankImage(ra, dec, equinox, radius, width, height,
				      colors_->pixelval(0));
    if (fits) 
	image_ = makeImage(fits);

    //  restore transformations, cut levels, etc from the previous image
    if (image_)
	image_->restoreParams(p, !autoSetCutLevels_);

    return initNewImage();
}

/*
 * Return true if no image is loaded.
 */
int RtdImage::isclear()
{
    return (!image_ || (image_->width() <= 2 && image_->height() <= 2 
			&& image_->header().length() == 0));
}


/*
 * implement the "isclear" subcommand to return true if the image is
 * cleared and 0 if there is an image loaded
 *
 * usage: $image isclear
 */
int RtdImage::isclearCmd(int argc, char* argv[])
{
    // XXX: we should do something with the -file option ?, camera ?
    return set_result(isclear());
}



/*
 * This method implements the "preview" subcommand
 *
 * usage: <pathName> preview <boolValue>
 *
 * if boolValue is true, enter preview mode, otherwise go back to
 * real-time mode. In preview mode, the camera is stopped (if it was
 * running) and a local copy of the shared memory image is made,
 * so that it can be freed or modified without affecting the image.
 *
 */
int RtdImage::previewCmd(int argc, char* argv[])
{
    
#ifdef DEBUG
    if (verbose()) {
	printf("%s: previewCmd: camera = %x\n", name(), camera_);
    }
#endif

    if (! camera_)
	return TCL_OK;

    int flag; 
    if (Tcl_GetBoolean(interp_, argv[0], &flag) != TCL_OK) 
	return TCL_ERROR;
  
    if (flag) {
	// enter preview mode, get local copy of image data
	if (camera_->attached()) {
	    image_->data().shared(0);
	    image_->data().shared(shm_data()); // in case a diff shared memory was requested

	    // also preview the rapid frame, if any
	    for(int i = 0; i < MAX_VIEWS; i++) {
		if (view_[i] && view_[i]->rapidFrame_ && view_[i]->image_ ) {
		    view_[i]->image_->data().shared(0);
		}
	    }

	    if (camera_->pause() != TCL_OK)
		return TCL_ERROR;

	    // update zoom window with the new data ?
	    processMotionEvent();
	}
    }
    else {
	// resume
	return camera_->cont();
    }
    return TCL_OK;
}



/*
 * This method implements the "camera" subcommand
 *
 * usage: camera attach cameraName ?preCommand? ?postCommand?
 *        camera detach
 *        camera pause
 *        camera continue
 *
 * The optional "preCommand" argument to "attach" should be a string
 * containing a Tcl command to be evaluated whenever a new image event
 * is received and before it is displayed. The "postCommand" argument
 * is evaluated after the image event is processed.
 *
 * note: for backward compat., "start" and "stop" are also accepted
 * for "attach" and "detach".
 */
int RtdImage::cameraCmd(int argc, char* argv[])
{
    if (! camera_)
	camera_ = new RtdImageCamera(this);

    if (strcmp(argv[0],"pause") == 0) {
	return camera_->pause();
    }
    else if  (strcmp(argv[0],"continue") == 0) {
	return camera_->cont();
    }
    else if (strcmp(argv[0],"attach") == 0 || strcmp(argv[0],"start") == 0) {
	if (argc < 2)
	    return error("expected \"pathName camera attach cameraName ?preCommand? ?postCommand?\"");
	if (argc > 2) {
	    if (cameraPreCmd_)
		free(cameraPreCmd_);
	    cameraPreCmd_ = (strlen(argv[2]) ? strdup(argv[2]) : (char*)NULL);
	}
	if (argc > 3) {
	    if (cameraPostCmd_)
		free(cameraPostCmd_);
	    cameraPostCmd_ = (strlen(argv[3]) ? strdup(argv[3]) : (char*)NULL);
	}
	return camera_->start(argv[1]);
    }
    else if  (strcmp(argv[0],"detach") == 0 || strcmp(argv[0],"stop") == 0) {
	return camera_->stop();
    }
    else {
	return error("invalid camera subcommand: expected: start, stop, pause or continue");	
    }
    return TCL_OK;
}


/*
 * Implement the "cmap" subcommand
 *
 * usage: 
 *      <path> cmap file ?<colormapFile>?
 *      <path> cmap rotate <amount>
 *      <path> cmap shift <amount>
 *      <path> cmap set <window>
 *      <path> cmap pixels
 *      <path> cmap reset
 *      <path> cmap list
 *      <path> cmap private
 *      <path> cmap isprivate
 *
 * where the file, if specified, should contain 256 lines of R, G, B
 * values. Each line should contain 3 floating point values between 0.0
 * and 1.0, for the reg, grean, and blue color scale values. 
 * If the filename is not specified, the current colormap file name is
 * returned.
 *
 * For rotate and shift, the amount and be any integer. The colormap will be
 * rotated or shifted by that amount.
 *
 * "set" sets the image colormap to be the colormap for the given window. This
 * can be used for popup windows to avoid color flashing when moving between
 * popup windows and the image.
 *
 * "pixels" returns a Tcl list of the colormap pixel values (for use by external
 * applications using the RTI library (ImageData)).
 *
 * "reset" resets the colormap to its original state.
 *
 * "list" returns a list of all of the colormap files currently loaded
 *
 * "private" says to start using a private colormap.
 * "isprivate" returns true if the colormap is private.
 */
int RtdImage::cmapCmd(int argc, char* argv[])
{
    int ret = TCL_OK;
    if (argc == 2) {
	if (strcmp(argv[0], "file") == 0) {
            ret = colors_->loadColorMap(argv[1]);
	}
	if (strcmp(argv[0], "rotate") == 0) {
            int amount;
            if (Tcl_GetInt(interp_, argv[1], &amount) != TCL_OK) {
                ret = TCL_ERROR;
            } else {
                ret = colors_->rotateColorMap(amount);
            }
	}
	if (strcmp(argv[0], "shift") == 0) {
	    int amount;
	    if (Tcl_GetInt(interp_, argv[1], &amount) != TCL_OK) {
                ret = TCL_ERROR;
            } else {
                ret = colors_->shiftColorMap(amount);
            }
	}
	if (strcmp(argv[0], "set") == 0) {
            Tk_Window w = Tk_NameToWindow(interp_, argv[1], tkwin_);
	    if (w == NULL) {
              ret = TCL_ERROR;
            } else {
              ret = colors_->setColormap(w);
            }
	}
        //  Force image update if colour changes do not transfer
        //  automatically (i.e. non-pseudocolor visual).
        if ( ret == TCL_OK && colors_->readOnly() ) {
            return colorUpdate();
        } else {
            return ret;
        }
    }

    if (strcmp(argv[0], "reset") == 0) {
	ret = colors_->reset();
        if ( ret == TCL_OK ) {
            return colorUpdate();
        } else {
            return ret;
        }


    }
    if (strcmp(argv[0], "pixels") == 0) {
	int n = colors_->colorCount();
	unsigned long* p = colors_->pixelval();
	char buf[MAX_COLOR*10];
	ostrstream os(buf, sizeof(buf));
	for (int i = 0; i < n; i++) 
	    os << *p++ << " ";
	os << ends;
	return set_result(buf);
    }
    if (strcmp(argv[0], "list") == 0) {
	 ostrstream os;
	 ColorMapInfo::list(os);
	 os << ends;
	 set_result(os.str());
	 delete os.str();
	 return TCL_OK;
     }
     if (strcmp(argv[0], "private") == 0) {
	 return colors_->usePrivateCmap();
     }
     if (strcmp(argv[0], "isprivate") == 0) {
	 return set_result(colors_->usingPrivateCmap());
     }
  
    return error("unknown rtdimage cmap subcommand");
}


/*
 * Implement the "itt" subcommand
 *
 * usage: <path> itt file <ITTFile>
 *        <path> itt scale <amount>
 *        <path> itt list
 *
 * where the file should contain 256 intensity values (0.0..1.0), 
 * one per line.
 *
 * "file" sets the itt file, 
 * "scale" scales the itt by the given amount
 * "list" returns a list of itt files loaded
 */
int RtdImage::ittCmd(int argc, char* argv[])
{

    if (argc == 2) {
        int ret = TCL_OK;
        if (strcmp(argv[0], "file") == 0) {
	    ret = colors_->loadITT(argv[1]);
	}
	else if (strcmp(argv[0], "scale") == 0) {
	    int amount;
	    if (Tcl_GetInt(interp_, argv[1], &amount) != TCL_OK) {
                ret = TCL_ERROR;
            } else {
              ret = colors_->scaleITT(amount);
            }
	}
        //  Force image update if colour changes do not transfer
        //  automatically (i.e. non-pseudocolor visual).
        if ( ret == TCL_OK ) {
            return colorUpdate();
        } else {
            return ret;
        }
    }

    if (strcmp(argv[0], "file") == 0) {
	return set_result(colors_->itt()->name());
    }

    if (strcmp(argv[0], "list") == 0) {
	 ostrstream os;
	 ITTInfo::list(os);
	 os << ends;
	 set_result(os.str());
	 delete os.str();
	 return TCL_OK;
    }

    return error("expected: \"itt file\" or \"itt scale\"");

    return TCL_OK;
}


/*
 * Implement the "cut" subcommand to set cut levels:
 *
 * usage: <path> cut <min> <max>  ?fromUser?
 *    or: <path> cut 
 *
 * If the min and max arguments are specified, the cut levels are set.
 *
 * The optional ?fromUser? argument indicates whether or not this is 
 * a result of a user action and defaults to 1 (true). Once a user has
 * set the cut levels, automatic cut level setting is disabled.
 *
 * If no args are given, the current cut levels are returned in a list
 * of {min max}.
 *
 */
int RtdImage::cutCmd(int argc, char* argv[])
{
    if (!image_)
	return TCL_OK;

    int fromUser = 1;
    if (argc == 3 && Tcl_GetInt(interp_, argv[2], &fromUser) != TCL_OK)
	return TCL_ERROR;

    if (argc >= 2) {
	// set the cut levels
	double min, max;
	
	if (Tcl_GetDouble(interp_, argv[0], &min) != TCL_OK 
	    || Tcl_GetDouble(interp_, argv[1], &max) != TCL_OK) {
	    return TCL_ERROR;
	}

	return setCutLevels(min, max, 1, fromUser);
    }
    else if (argc == 0) {
	// return the current cut levels
	char buf[80];
	sprintf(buf, "%g %g", image_->lowCut(), image_->highCut());
	return set_result(buf);
    }

    return TCL_OK;
}


/*
 * Implement the "autocut" subcommand to set cut levels automatically to
 * some reasonable values:
 *
 * usage: <path> autocut ?-percent number?
 *
 * Two different algorithms are supported. The default is median
 * filtering. 
 *
 * If -percent is specified, the argument is a number between 0 and 100,
 * such as 90 for 90%, where that percent of the image should be visible
 * within the cut values. i.e.: if you look at the graph (see graphdist
 * command) of the pixel value distribution, you would take the top 90%
 * of the graph and set the cut levels to left and right ends of the
 * graph.
 */
int RtdImage::autocutCmd(int argc, char* argv[])
{
    if (!image_ || image_->dataType() == X_IMAGE)
	return TCL_OK;
    
    if (argc == 2) {
	if (strcmp(argv[0], "-percent") == 0) {
	    double percent;
	    if (Tcl_GetDouble(interp_, argv[1], &percent) != TCL_OK
		|| percent < 0.0 || percent > 100.0)
		return TCL_ERROR;
	    image_->autoSetCutLevels(percent);
	} 
	else {
	    return error("expected -percent arg for autocut");
	}
    }
    else if (argc == 0) {
	image_->medianFilter();
    }
    else {
	return error("wrong number of args: expected none or -percent followed by arg");
    }

    image_->colorScale(colors_->colorCount(), colors_->pixelval());

    // assume the user has not set the cut levels, so we can change them
    // if a new image is loaded...
    autoSetCutLevels_ = 1;


    // make sure the new lookup table is propagated
    LookupTable lookup = image_->lookup();
    for(int i = 0; i < MAX_VIEWS; i++) {
	if (view_[i] && view_[i]->image_ && !view_[i]->isSeparateRapidFrame()) 
	    view_[i]->image_->lookup(lookup);
    }
    return updateViews(1) || updateImage(); 
}


/*
 * Implement the "width" subcommand - returns the
 * unscaled width of the current image.
 */
int RtdImage::widthCmd(int argc, char* argv[])
{
    if (!image_)
	return set_result(0);

    return set_result(image_->width());
}



/*
 * Implement the "height" subcommand - returns the
 * unscaled height of the current image.
 */
int RtdImage::heightCmd(int argc, char* argv[])
{
    if (!image_)
	return set_result(0);

    return set_result(image_->height());
}


/*
 * Implement the "dispwidth" subcommand - returns the
 * width of the current image after scaling.
 */
int RtdImage::dispwidthCmd(int argc, char* argv[])
{
    if (!image_)
	return set_result(0);
    
    double rw = reqWidth_, rh = reqHeight_;
    doTrans(rw, rh, 1);
    return set_result(rw ? rw : dispWidth());
}


/*
 * Implement the "dispheight" subcommand - returns the
 * height of the current image after scaling.
 */
int RtdImage::dispheightCmd(int argc, char* argv[])
{
    if (!image_)
	return set_result(0);

    double rw = reqWidth_, rh = reqHeight_;
    doTrans(rw, rh, 1);
    return set_result(rh ? rh : dispHeight());
}


/*
 * Implement the "min" subcommand - returns the
 * lowest pixel value in the image.
 */
int RtdImage::minCmd(int argc, char* argv[])
{
    if (!image_)
	return TCL_OK;

    return set_result(image_->minValue());
}


/*    else {
	src_x_inc = 1;
    }

 * Implement the "max" subcommand - returns the
 * highest pixel value in the image.
 */
int RtdImage::maxCmd(int argc, char* argv[])
{
    if (!image_)
	return TCL_OK;

    return set_result(image_->maxValue());
}


/*
 * Set the maximum update frequency for the RTD.
 */
int RtdImage::maxFreqCmd(int argc, char* argv[])
{
    double maxFreq;

    if (argc != 1) {
        return TCL_ERROR;
    }

    if (Tcl_GetDouble(interp_, argv[0], &maxFreq) != TCL_OK) {
        return TCL_ERROR;
    }

    // This becomes the maximum update frequency in the main image, unless it
    // is negative, which indicates that the feature should be turned off.
    if (maxFreq < 0.) {
        options_->fixUpdateRate = 0;
        options_->userUpdateTime = 0.;
    }
    else {
        options_->fixUpdateRate = 1;
        options_->userUpdateTime = 1./maxFreq;
    }

    return TCL_OK;
}

/*
 * Implement the "bitpix" subcommand - returns the
 * BITPIX field of the image header to indicate the
 * type of the image (8 - byte, 16 = short, etc).
 */
int RtdImage::bitpixCmd(int argc, char* argv[])
{
    if (!image_)
	return TCL_OK;

    return set_result(image_->dataType());
}


/*
 * Implement the "object" subcommand - returns the
 * OBJECT field of the image header to indicate the
 * name of the astronomical object
 */
int RtdImage::objectCmd(int argc, char* argv[])
{
    if (!image_)
	return TCL_OK;

    return set_result(image_->object());
}


/*
 * Implement the "type" subcommand - returns the
 * data type of the raw image as a string 
 */
int RtdImage::typeCmd(int argc, char* argv[])
{
    if (!image_)
	return TCL_OK;

    switch (image_->dataType()) {
    case FLOAT_IMAGE: 
	return set_result("float");
    case SHORT_IMAGE: 
	return set_result("short");
    case USHORT_IMAGE: 
	return set_result("ushort");
    case LONG_IMAGE: 
	return set_result("long");
    case BYTE_IMAGE: 
	return set_result("byte");
    case X_IMAGE: 
	return set_result("XImage");
    default:
	break;
    }
    return TCL_OK;
}


/*
 * Implement the "update" subcommand 
 *
 * usage:  $image update
 * or:     $image update idletasks
 *
 * With no arguments, just make sure that the image is up to date with
 * the raw data (which may have changed via shared memory).  
 *
 * With 1 arg, do the equivalent of the Tk update idletasks command
 * (this one is for use via the remote interface).
 *
 */
int RtdImage::updateCmd(int argc, char* argv[])
{
    if (argc == 0)
	return updateImage();
    updateIdleTasks();
    return 0;
}


/*
 * Implement the "alloccolors" subcommand 
 *
 * usage: alloccolors ?numColors?
 *
 * With no args, return a Tcl list containing the number of 
 * allocated and the number of free colors, 
 * with one arg reallocate up to numColors colors.
 */
int RtdImage::alloccolorsCmd(int argc, char* argv[])
{
    if (argc == 0) {
	char buf[80];
	sprintf(buf, "%d %d", colors_->colorCount(), colors_->freeCount());
	return set_result(buf);
    }
    
    int numColors;
    if (Tcl_GetInt(interp_, argv[0], &numColors) != TCL_OK) {
	return TCL_ERROR;
    }
    if (colors_->reallocate(numColors)) 
	return TCL_ERROR;

    if (image_) {
	image_->colorScale(colors_->colorCount(), colors_->pixelval());
	return updateImage(); 
    }
    
   return TCL_OK;
}


/*
 * implement the graphdist subcommand to display the distribution
 * of values in the image.
 *
 * usage:  
 *        pathName graphdist bltGraph bltElem numValues xVector yVector
 *
 *
 *      xVector    is the name of the BLT x vector
 *
 *      yVector    is the name of the BLT y vector
 *
 * The data for the given element in the given graph will be set
 * directly from here without going through tcl.
 */
int RtdImage::graphdistCmd(int argc, char* argv[])
{

    if (!image_)
	return TCL_OK;

    int numValues;
    if (Tcl_GetInt(interp_, argv[2], &numValues) != TCL_OK) {
	return TCL_ERROR;
    }

    // seems like you can't always depend on being allowed to use a variable
    // for the array bounds...
#ifdef __GNUC__
    double xyvalues[numValues*2];
#else
    double* xyvalues = new double[numValues*2];
#endif
    image_->getDist(numValues, xyvalues);
    int status = TCL_OK;
    if (numValues > 0)
	status =  Blt_GraphElement(interp_, argv[0], argv[1], numValues*2, xyvalues, argv[3], argv[4]);
    else
	status = error("all image pixels have the same value");
#ifndef __GNUC__
    delete xyvalues;
#endif
    return status;
}


/*
 * implement the "get" image command to return a Tcl list of image values
 * at the given X,Y coordinates
 *
 * usage: <path> get $x $y  coord_type ?nrows ncols?
 *
 * x and y are the coordinates in the image window in the given coordinate
 * system (one of: canvas, image, screen, wcs, deg). 
 *
 * The return value is a tcl list where each item consists of a list of
 * {X Y Value}, where X and Y are the image coords in the raw image
 * and Value is the raw data value there or "-" if out of range.
 *
 * If nrows and ncols are greater than 1, return a Tcl list of n rows
 * x n cols values each (a list of rows...), centered at the given point.
 */
int RtdImage::getCmd(int argc, char* argv[])
{
    if (!image_)
	return TCL_OK;

    double x, y;
    int nrows = 1, ncols = 1; 
    char buf[80];

    if (convertCoordsStr(0, argv[0], argv[1], NULL, NULL, 
			 x, y, argv[2], "image") != TCL_OK)
	return TCL_ERROR;
    
    if (argc == 5) {
	if (Tcl_GetInt(interp_, argv[3], &nrows) != TCL_OK ||
	    Tcl_GetInt(interp_, argv[4], &ncols) != TCL_OK) {
	    return TCL_ERROR;
	}
    }

    if (nrows == 1 && ncols == 1) {
	return set_result(image_->getValue(buf, x, y));
    }
    
    int n = nrows/2;
    int m = ncols/2;
    for (int j = -m; j <= m; j++) {
	Tcl_AppendResult(interp_, " { ", NULL);
	for (int i = -n; i <= n; i++) {
	    Tcl_AppendResult(interp_, 
			     " { ", 
			     image_->getValue(buf, x+i, y+j), 
			     " } ", 
			     NULL);
	}
	Tcl_AppendResult(interp_, " } ", NULL);
    }

    return TCL_OK;
}


/*
 * implement the "wcscenter" image command to return the world
 * coordinates of the center of the image.
 *
 * usage: <path> wcscenter ?-format $format?
 *
 * The optional format option determines the format of the result:
 *     -format 0 ==> H:M:S [+-]D:M:S Equinox (default)
 *     -format 1 ==> RA DEC Equinox (RA and DEC in degrees) 
 *
 * The return value is a tcl list, formatted according to the format
 * option, or an empty string if the coordinates are out of range or WCS
 * is not supported.
 */
int RtdImage::wcscenterCmd(int argc, char* argv[])
{
    if (!isWcs())
	return TCL_OK;

    // get the format
    int format = 0;
    if (argc == 2) {
	if (strcmp(argv[0], "-format") == 0) {
	    if (Tcl_GetInt(interp_, argv[1], &format) != TCL_OK) {
		return TCL_ERROR;
	    }
	}
    }

    // get x and y
    double x = image_->width()/2., y = image_->height()/2.; 
    
    // do the conversion and return the result
    switch(format) {
    case 0:
	char buf[80];
	return set_result(image_->wcs().pix2wcs(x, y, buf, sizeof(buf)));
    case 1:
	double ra, dec;
	image_->wcs().pix2wcs(x, y, ra, dec);
	return set_result(ra, dec);
    default:
	return error("unknown format for pix2wcs: try 0 or 1");
    }

    return TCL_OK;
}


/*
 * implement the "wcsradius" image command to return the world
 * coordinates radius of the image - the distance in arc-minutes from the
 * center point to the origin.
 *
 * usage: <path> wcsradius
 *
 * The return value in Tcl is the radius in arc-minutes or an empty
 * string if WCS is not supported.
 */
int RtdImage::wcsradiusCmd(int argc, char* argv[])
{
    if (!isWcs())
	return TCL_OK;

    return set_result(image_->wcs().radius());
}


/*
 * implement the "wcswidth" image command to return the world
 * coordinates width of the image
 *
 * usage: <path> wcswidth
 *
 * The return value in Tcl is the width in arcmin or an empty
 * string if WCS is not supported.
 */
int RtdImage::wcswidthCmd(int argc, char* argv[])
{
    if (!isWcs())
	return TCL_OK;

    return set_result(image_->wcs().width());
}


/*
 * implement the "wcsheight" image command to return the world
 * coordinates height of the image
 *
 * usage: <path> wcsheight
 *
 * The return value in Tcl is the height in arcmin or an empty
 * string if WCS is not supported.
 */
int RtdImage::wcsheightCmd(int argc, char* argv[])
{
    if (!isWcs())
	return TCL_OK;

    return set_result(image_->wcs().height());
}


/*
 * This method implements the radecbox subcommand.
 *
 * usage:
 * 	lassign [$image radecbox $ra $dec $radius] ra0 dec0 ra1 dec1
 *
 * ra and dec are the world coords (h:m:s or decimal deg) and radius is expected in
 * arcmin. 
 *
 * The return value in Tcl is a list of 4 values {ra0 dec0 ra1 dec1} that form a
 * ra,dec box with the given center point and radius
 */
int RtdImage::radecboxCmd(int argc, char* argv[])
{
    WorldCoords pos(argv[0], argv[1]);
    if (pos.status() != 0)
	return TCL_ERROR;

    double radius;
    if (Tcl_GetDouble(interp_, argv[2], &radius) != TCL_OK)
	return TCL_ERROR;
    
    WorldCoords pos1, pos2;
    pos.box(radius, pos1, pos2);
    
    char buf[255];
    ostrstream os(buf, sizeof(buf));
    os << pos1 << ' ' << pos2 << ends;
    return set_result(buf);
}



/*
 * This method implements the wcsequinox subcommand.
 *
 * usage:
 * 	set equinox [$image wcsequinox]
 *
 * The return value in Tcl is the world coordinate equinox 
 * for the values of RA and DEC returned by the wcs...
 * commands.
 */
int RtdImage::wcsequinoxCmd(int argc, char* argv[])
{
    if (!isWcs())
	return TCL_OK;
    double equinox = image_->wcs().equinox();
    if (equinox != 0.0) {
	char buf[32];
	sprintf(buf, "%.2f", equinox);
	return set_result(buf);
    }
    return TCL_OK;
}


/*
 * This method implements the wcsdist subcommand.
 *
 * usage:
 * 	set dist [$image wcsdist x0 y0 x1 y1]
 *
 * The arguments are expected in canvas coords (canvasx, canvasy,
 * doubles).
 * The return value in Tcl is the WCS distance in arcsec between 2 
 * points (after transformations).
 */
int RtdImage::wcsdistCmd(int argc, char* argv[])
{
    if (!isWcs())
	return TCL_OK;

    double x0, y0, x1, y1;
    if (Tcl_GetDouble(interp_, argv[0], &x0) != TCL_OK
	|| Tcl_GetDouble(interp_, argv[1], &y0) != TCL_OK
	|| Tcl_GetDouble(interp_, argv[2], &x1) != TCL_OK
	|| Tcl_GetDouble(interp_, argv[3], &y1) != TCL_OK) {
	return TCL_ERROR;
    }

#if 0
    // XXX use new "convert command here ???
    undoTrans(x0, y0);
    undoTrans(x1, y1);

    // convert to WCS
    double ra0 = 0.0, dec0 = 0.0, ra1 = 0.0, dec1 = 0.0;
    if (image_->wcs().pix2wcs((int)x0, (int)y0, ra0, dec0) != 0
	|| image_->wcs().pix2wcs((int)x1, (int)y1, ra1, dec1) != 0)
	return TCL_ERROR;

    // check results
    if (ra0 == 0.0 || dec0 == 0.0 || ra1 == 0.0 || dec1 == 0.0)
	return TCL_OK;

    double dist = WorldCoords::dist(ra0, dec0, ra1, dec1)*60.;
#else
    canvasToWorldCoords(x0, y0, 0);
    canvasToWorldCoords(x1, y1, 0);
    double dist = WorldCoords::dist(x0, y0, x1, y1)*60.;
#endif

    return set_result(dist);
}


/*
 * This method implements the spectrum subcommand.
 *
 * usage:
 *
 *     <pathName> spectrum <bltGraph> <bltElem> x0 y0 x1 y1 coord_type xVector yVector
 *
 * where: 
 *      x0, y0, x1 and y1 are the end points of a line in the image in the
 *                  given coordinate system (canvas, image, screen, wcs, deg).
 *
 *      <bltGraph> is the path name of a BLT graph widget to display
 *                 the plot of the pixel intensities along the line
 *
 *      <bltElem>  is the name of the element in the graph that should
 *                 receive the data
 *
 *      xVector    is the name of the BLT x vector
 *
 *      yVector    is the name of the BLT y vector
 *
 * The data is sent directly to the graph for display.
 * The return value in Tcl is the number of points to plot.
 */
int RtdImage::spectrumCmd(int argc, char* argv[])
{
    if (!image_)
	return TCL_OK;

    // convert to image coords
    double rx0, ry0, rx1, ry1;
    if (convertCoordsStr(0, argv[2], argv[3], NULL, NULL, 
			 rx0, ry0, argv[6], "image") != TCL_OK
	|| convertCoordsStr(0, argv[4], argv[5], NULL, NULL, 
			    rx1, ry1, argv[6], "image") != TCL_OK)
	return TCL_ERROR;
    
    // get distance between endpoints (add a little to be safe)
    int x0 = int(rx0), y0 = int(ry0), x1 = int(rx1), y1 = int(ry1);
    int w = abs(x1-x0) + 1;
    int h = abs(y1-y0) + 1;
    int dist = (int)sqrt(w*w + h*h) + 2;

    double* xyvalues = new double[dist*2];

    // fill the xyvalues array and set numValues to the actual number of points
    int numValues = image_->getSpectrum(xyvalues, x0, y0, x1, y1);
    assert(numValues <= dist);
    if (Blt_GraphElement(interp_, argv[0], argv[1], numValues*2, xyvalues, argv[7], argv[8]) != TCL_OK) {
	delete xyvalues;
	return TCL_ERROR;
    }
    delete xyvalues;
    return set_result(numValues);
}

int RtdImage::motioneventCmd(int argc, char* argv[])
{
    if (argc == 0) 
	// return the current values
	return set_result(saveMotion_);

    if (argc != 1) 
        return error("wrong number of args: should be <path> motionevent ?0/1");
    int value;
    if (Tcl_GetInt(interp_, argv[0], &value) != TCL_OK) {
        return error("invalid argument, expected 0 or 1");
    }
    saveMotion_ = value;
}


/*
 * Implement the "scale" image subcommand
 *
 *  usage: <path> scale ?sx sy restFlag?
 *
 * With 2 args scale (zoom) the image by the specified X and Y
 * amount. 
 *
 * With no args, return the current scaling factors.
 *
 */
int RtdImage::scaleCmd(int argc, char* argv[])
{
    if (!image_)  
	return TCL_OK;

    int xs = image_->xScale() , ys = image_->yScale();
    if (argc == 0) 
	// return the current values
	return set_result(xs, ys);
     
    
    if (argc != 2) 
	return error("wrong number of args: should be <path> scale ?sx sy?");

    // set the scale factor:
    // 2 means twice the normal size, -2 means 1/2 normal size
    // don't allow zero scale or shrink in X and grow in Y...
    int xScale, yScale;
    if (Tcl_GetInt(interp_, argv[0], &xScale) != TCL_OK ||
	Tcl_GetInt(interp_, argv[1], &yScale) != TCL_OK) {
	return error("invalid arguments, expected x and y scale factors");
    }
    
    // check arguments
    if (xScale == -1 || xScale == 0)
	xScale = 1;
    if (yScale == -1 || yScale == 0)
	yScale = 1;
    if ((xScale < 0 && yScale > 0) || (xScale > 0 && yScale < 0)) 
	return error("invalid arguments, expected 2 positive or 2 negative integer values");

    // add a check for the Tk limit on canvas coords
    if (xScale > 0 && xScale * image_->width() > 32767 || yScale > 0 && yScale * image_->height() > 32767) 
	return error("sorry, can't scale image to this size without exceeding maximum Tk canvas coordinate range");

    return setScale(xScale, yScale);
}


/*
 * Implement the "fits" image subcommand
 *
 *  usage: <path> fits get ?keyword?
 *
 * If "fits get <keyword>" is specified, this command returns the value
 * for the keyword in the FITS header, otherwise "get" with no arguments
 * returns a formatted copy of the entire header.
 */
int RtdImage::fitsCmd(int argc, char* argv[])
{
    if (!image_)
	return TCL_OK;

    const ImageIO& imio = image_->image();

    if (strcmp(argv[0], "get") == 0) {
	if (argc == 1 && image_->header().size()) {
	    // return a copy of the FITS header, format it in 80 char lines and
	    // replace any NULL chars with blanks
	
	    ostrstream os;
	    image_->getFitsHeader(os);
	    set_result(os.str());
	    delete os.str();
	    return TCL_OK;
	}

	// return the value for the given FITS keyword
	char* s = imio.get(argv[1]);
	return set_result(s ? s : "");
    }
    return error("unknown argument: expected \"fits get ?keyword?\"");
}


/*
 * Implement the "flip" image subcommand
 *
 *  usage: <path> flip <direction> ?bool?
 *
 * where direction is one of x, y, xy or "none" for flipping in the x, y
 * or x and y directions or neither.
 *
 * The boolean value turns flipping on (1) or off (0) in the given
 * direction(s).
 *
 * With one arg, return the current value for the given arg.
 */
int RtdImage::flipCmd(int argc, char* argv[])
{
    if (!image_)  
	return TCL_OK;

    int flipX = 0, flipY = 0, arg = 1;

    if (argc == 2) {
	if (Tcl_GetBoolean(interp_, argv[1], &arg) != TCL_OK) 
	    return TCL_ERROR;
    }
    if (strcmp(argv[0], "x") == 0 || strcmp(argv[0], "X") == 0) {
	flipX++;
    }
    else if (strcmp(argv[0], "y") == 0 || strcmp(argv[0], "Y") == 0) {
	flipY++;
    }
    else if (strcmp(argv[0], "xy") == 0 || strcmp(argv[0], "XY") == 0) {
	flipX++; flipY++;
    }
    else if (strcmp(argv[0], "none") != 0)
	return error("expected: flip, followed by: x, y, xy or none");

    // if the image is rotated, it is more intuitive to exchange the X and Y axis
    if (image_->rotate()) 
	swap(flipX, flipY);

    if (flipX) {
	if (argc == 2)
	    image_->flipX(arg);
	else
	    return set_result(image_->flipX());
    }
    if (flipY) {
	if (image_->dataType() == X_IMAGE) {
	    if (argc == 2)
		image_->flipY(!arg);
	    else 
		return set_result(!image_->flipY());
	}
	else {
	    if (argc == 2)
		image_->flipY(arg);
	    else 
		return set_result(image_->flipY());
	}
    }
    
    // update other views
    if (updateViews(1) != TCL_OK)
	return TCL_ERROR;

    // make sure the image is regenerated
    if (resetImage() != TCL_OK)
	return TCL_ERROR;

    if (panCommand_) {
	if (Tk_Width(tkwin_) <= 1) {
	    // pause here until window is displayed, so that the pan
	    // window can determine the size of the window
	    updateIdleTasks();
	}
	autoPan(1);
    }
    return TCL_OK;
}


/*
 * Implement the "rotate" image subcommand
 *
 *  usage: <path> rotate ?bool?
 *
 * currently, rotation is only done by swapping x and y axis (-90 deg.)
 * If bool is specified, rotation is turned on(1) or off(0).
 * Otherwise, the current setting is returned.
 */
int RtdImage::rotateCmd(int argc, char* argv[])
{
    if (!image_)  
	return TCL_OK;

    int angle = 0;
    if (argc == 1) {
	if (Tcl_GetInt(interp_, argv[0], &angle) != TCL_OK)
	    return TCL_ERROR;  
    }
    else {
	return set_result(image_->rotate());
    }
    
    image_->rotate(angle != 0);

    // update other views
    if (updateViews(1) != TCL_OK)
	return TCL_ERROR;

    // make sure the image is regenerated
    if (resetImage() != TCL_OK)
	return TCL_ERROR;

    if (panCommand_) {
	if (Tk_Width(tkwin_) <= 1) {
	    // pause here until window is displayed, so that the pan
	    // window can determine the size of the window
	    updateIdleTasks();
	}
	autoPan(1);
    }
    return TCL_OK;
}


/* Implement the "colorscale" image subcommand
 *
 *  usage: <path> colorscale ?scale_type?
 *
 * With 1 arg, color scale the image using the given algorithm
 * (one of: linear, log, sqrt, histeq)
 * With no args, return the current color scale type.
 */
int RtdImage::colorscaleCmd(int argc, char* argv[])
{
    if (!image_)  
	return TCL_OK;
    
    if (argc == 0) {
	switch(image_->colorScaleType()) {
	case ImageData::LINEAR_SCALE:
	    return set_result("linear");
	case ImageData::LOG_SCALE:
	    return set_result("log");
	case ImageData::SQRT_SCALE:
	    return set_result("sqrt");
	case ImageData::HISTEQ_SCALE:
	    return set_result("histeq");
	default:
	    return set_result("none");
	}
    }
    
    if (argc != 1) 
	return error("wrong number of args: should be <path> colorscale ?scale_type?");

    // set the color scale type
    if (strcmp(argv[0], "linear") == 0)
	image_->colorScaleType(ImageData::LINEAR_SCALE);
    else if (strcmp(argv[0], "log") == 0)
	image_->colorScaleType(ImageData::LOG_SCALE);
    else if (strcmp(argv[0], "sqrt") == 0)
	image_->colorScaleType(ImageData::SQRT_SCALE);
    else if (strcmp(argv[0], "histeq") == 0)
	image_->colorScaleType(ImageData::HISTEQ_SCALE);
    else 
	return fmt_error("unknown color scale algorithm: %s, %s", 
			 argv[0], "should be one of: linear, log, sqrt, histeq");

    // color scale the image
    image_->colorScale(colors_->colorCount(), colors_->pixelval());
    
    // make sure the image is regenerated
    return updateImage(); 
}


/*
 * zoom subcommand: 
 * usage: 
 *     zoom start <frame> <zoomFactor>
 *     zoom stop
 *
 *     zoom slow
 *     zoom fast
 *
 * "$image zoom slow" can be used to slow down the zoom window
 * updates so they don't drag down performance, otherwise zoom
 * window updates are forced to be displayed immediately for better
 * feedback (zoom fast).
 */
int RtdImage::zoomCmd(int argc, char* argv[])
{
    int status = TCL_OK;
    if (strcmp(argv[0], "start") == 0) {
	// zoom start subcommand
	if (argc != 3) 
	    return error("wrong # of args: should be \"pathName zoom start win factor\"");

	int zoomFactor;
	if (Tcl_GetInt(interp_, argv[2], &zoomFactor) != TCL_OK)
	    return TCL_ERROR;  
 
	if (zoomFactor < 1 || zoomFactor > 10) 
	    return error("zoomFactor should be between 1 and 10");

	Tk_Window zoomWin = Tk_NameToWindow(interp_, argv[1], tkwin_);
	if (zoomWin == NULL) 
	    return TCL_ERROR;

	int width = Tk_Width(zoomWin);
 	int height = Tk_Height(zoomWin);

	// round off size to be a multiple of the zoom factor
	width += (zoomFactor - width % zoomFactor);
	height += (zoomFactor - height % zoomFactor);
	
	if (zoomer_)
	    delete zoomer_;
	zoomer_ = new ImageZoom(zoomWin, gc_, width, height, zoomFactor, 
				usingXShm_, verbose());
	
	status = zoomer_->status();
    }
    else if (strcmp(argv[0], "stop") == 0) {
	// zoom unset subcommand
	delete zoomer_;
	zoomer_ = NULL;
    }
    else if (strcmp(argv[0], "slow") == 0) {
	// zoom slow subcommand
	zoomSpeed_ = -1;
    }
    else if (strcmp(argv[0], "fast") == 0) {
	// zoom fast subcommand
	zoomSpeed_ = 1;
    }
    else {
	return error("invalid image zoom subcommand: should be \"start\" or \"stop\"");
    }

    // tell the other views to use the zoom window too
    for(int i = 0; i < MAX_VIEWS; i++) {
	if (view_[i]) {
	    view_[i]->zoomer_ = zoomer_;
	    view_[i]->zoomSpeed_ = zoomSpeed_;
	}
    }
	
    return TCL_OK;
}


/*
 * zoomview subcommand: alternative zoom window, using rtdimage view, so that
 * zoom is always accurate, even when image is shrunk.
 *
 * usage: 
 *     zoomview start <view> <zoomFactor> <propagateScale?> ?count?
 *     zoomview stop ?count?
 *     zoom slow
 *     zoom fast
 *
 * where view is the name of a second rtdimage.
 *
 * start - starts zooming using the given zoom factor magnification.
 *         If <propagateScale is true, changes in the master image
 *         scale will propagate to the view. "count" defaults to 1,
 *         but can be set to "2" for a second zoom window.
 *
 * stop - stops zooming the view
 *
 * "$image zoomview slow" can be used to slow down the zoom window
 * updates so they don't drag down performance, otherwise zoom
 * window updates are forced to be displayed immediately for better
 * feedback (zoomview fast).
 *
 */
int RtdImage::zoomviewCmd(int argc, char* argv[])
{
    if (strcmp(argv[0],"start") == 0) {
	// zoom start subcommand
	if (argc < 4) 
	    return error("wrong # of args: should be \"pathName zoom start $view $zoomFactor $propagateScale?\"");

	int zoomFactor, propagateScale, count = 1;
	if (Tcl_GetInt(interp_, argv[2], &zoomFactor) != TCL_OK 
	    || Tcl_GetInt(interp_, argv[3], &propagateScale) != TCL_OK)
	    return TCL_ERROR;  
	
	if (argc > 4 && Tcl_GetInt(interp_, argv[4], &count) != TCL_OK)
 	    return TCL_ERROR;  

	if (zoomFactor < 1 || zoomFactor > 10) 
	    return error("zoomFactor should be between 1 and 10");

	// allow an optional second zoom view
	RtdImage*& view = (count == 1) ? zoomView_ : zoomView2_;

	view = getView(argv[1]);
	if (view == NULL) 
	    return TCL_ERROR;
	view->propagateScale_ = propagateScale;
	view->zoomFactor_ = zoomFactor;
	if (updateViews(2) != TCL_OK)
	    return TCL_ERROR;
    }
    else if (strcmp(argv[0], "stop") == 0) {
	// zoom unset subcommand
	int count = 1;
	if (argc > 1 && Tcl_GetInt(interp_, argv[1], &count) != TCL_OK)
 	    return TCL_ERROR;  
	RtdImage*& view = (count == 1) ? zoomView_ : zoomView2_;
	view = NULL;
    }
    else if (strcmp(argv[0], "slow") == 0) {
	// zoom slow subcommand
	zoomSpeed_ = -1;
    }
    else if (strcmp(argv[0], "fast") == 0) {
	// zoom fast subcommand
	zoomSpeed_ = 1;
    }
    else {
	return error("invalid image zoomview subcommand: should be \"start\", \"stop\", ...");
    }	

    // tell the other views to use the zoom window too
    // note: the check for displaymode() != 0 is to keep the pan window
    // from using the zoom window, which can be very slow on huge images.
    for(int i = 0; i < MAX_VIEWS; i++) {
	if (view_[i] && view_[i]->displaymode() != 0) { 
	    view_[i]->zoomView_ = zoomView_;
	    view_[i]->zoomView2_ = zoomView2_;
	    view_[i]->zoomSpeed_ = zoomSpeed_;
	}
    }
    if (zoomView_) {
	zoomView_->zoomView_ = NULL;
	zoomView_->zoomView2_ = NULL;
    }
    if (zoomView2_) {
	zoomView2_->zoomView_ = NULL;
	zoomView2_->zoomView2_ = NULL;
    }
	
    return TCL_OK;
}


/*
 * pan subcommand: 
 *
 * usage: 
 *     pan start <tclCommand> <shrinkFactor>
 *     pan stop
 *     pan update
 *
 * start - arrange to have a tcl command evaluated whenever the image size changes
 *     (due to scaling or loading a new image) or whenever the image position has 
 *     changed (due to scrolling)
 *
 * pan stop - stop evaluating the tcl command... 
 *
 * pan upadte - force an update of the pan window (normally done whenever
 *              image size or position changes)
 *
 * The tclCommand will be called with 5 arguments: x1 y1 x2 y2, which are the coords 
 * of the visible part of  the image, scaled by the given shrinkFactor, and a flag
 * indicating whether the image is new (1) or an update of the existing image (0). 
 * This can be used to draw the panning rectangle on the panning image.
 */
int RtdImage::panCmd(int argc, char* argv[])
{
    if (strcmp(argv[0],"start") == 0) {
	// pan start subcommand
	if (argc != 3) 
	    return error("wrong # of args: should be \"pathName pan start tclCommand shrinkFactor\"");
	
	if (panCommand_)
	    free(panCommand_);
	panCommand_ = strdup(argv[1]);

	if (Tcl_GetInt(interp_, argv[2], &panFactor_) != TCL_OK)
	    return TCL_ERROR;  
 
	if (panFactor_ > -2 && panFactor_ != 1 && panFactor_ != -1) 
	    return error("pan shrinkFactor should be -2 for 1/2 size, -3 for 1/3, etc. or 1");

	if (panFactor_ == 1) 
	    panFactor_ = -1;	// for calculations, should be negative

	// cause panning window to be reset
	panx1_ = pany1_ = panx2_ = pany2_ = 0;
	if (image_)
	    autoPan();
    }
    else if (strcmp(argv[0],"stop") == 0) {
	// pan stop subcommand
	if (panCommand_)
	    free(panCommand_);
	panCommand_ = NULL;
    }
    else if (strcmp(argv[0],"update") == 0) {
	// pan update subcommand
 	// cause panning window to be reset
	panx1_ = pany1_ = panx2_ = pany2_ = 0;
	if (image_)
	    autoPan();
   }
    else {
	return error("invalid image pan subcommand: should be \"start\" or \"stop\"");
    }	
    return TCL_OK;
}

/*
 * The following function is called when the interactive performance testing
 * is enabled or disabled. It sets the required flag, and resets the variables 
 * if required.
 *
 * Usage:
 *      perftest off - turn performance testing off
 *      perftest on - turn performance testing on
 *      perftest reset - reset the performance parameters
 *      perftest count - return the performance image count
 *      perftest convert <unit number>  - convert the current units to
 *                                        <unit number>
 *
 * Arguments:
 *      int argc, char *argv - argument list
 */
int RtdImage::perfTestCmd(int argc, char *argv[])
{
    if (!camera_) {
        return error("Camera must be attached first");
    }

    // Check the arguments and act according to the usage instructions
    if (strcmp(argv[0], "on") == 0 || strcmp(argv[0], "reset") == 0) {
        // Set the testing on and reset the various parameters
        imageCount_ = 0;
        accGENtime_ = accXtime_ = accTCLtime_ = accMEMtime_ = 0.;
        if (strcmp(argv[0], "on") == 0) {
            intPerfTest_ = 1;
            perfTestType = TIME;
        }
    }
    else if (strcmp(argv[0], "off") == 0) {
        intPerfTest_ = 0;
    }
    else if (strcmp(argv[0], "convert") == 0) {
        // change the current display mode.
        perfTestType = (enum perfTestType)atoi(argv[1]);
    }
    else if (strcmp(argv[0], "count") == 0) {
        char retbuf[4];
        sprintf(retbuf, "%d", imageCount_);
        return set_result(retbuf);
    }
    else {
        return error("Unknown argument to perftest command");
    }

    return TCL_OK;
}

/*
 * view subcommand: specify a viewing image to view the same
 * image, possibly at a different size. 
 *
 * The new view will share data with the original and be updated 
 * when the original is updated.
 *
 * This can be used, for example, to build a panning window 
 * or a rapid frame.
 *
 * usage: 
 *     view add    <path> ?propagateScale? ?rapidFrame?
 *     view remove <path>
 *     view update <path> xOffset yOffset width height frameX frameY rapidX rapidY coordType
 *     view update <path> width height coordType
 *     view enter  <path>
 *     view leave  <path>
 *
 * <path> must be the name of a second rtdimage image. The two
 * images will communicate internally to always display the
 * same image, possibly scaled to different sizes.
 *
 * add    - adds a new view to this image
 * 	    If the optional argument "propagateScale" is true, changes in
 * 	    the scale factors in the master image will propagate to the
 * 	    view (this is the default behavior). If "rapidFrame" is specified
 *          as true, then the view is treated as a rapid frame and is only updated
 *          from image events and not from the main image.
 *  
 * remove - removes the view
 *
 * update - updates the frame from this image with the given sizes and offsets: 
 *
 *          xOffset, yOffset - X,Y offset of image frame in image canvas
 *          width, height    - dimensions of image
 *          frameX, frameY   - X,Y offset of image frame in image canvas
 *          rapidX, rapidY   - X,Y offset of rapid frame coresponding to main image.
 *          coordType        - type of the input coords (canvas, image, screen, etc)
 *
 * enter  - if 2 images are in the same canvas, make <path> the current one
 *          (receives motion events, ...)
 *
 * leave  - undo the enter command
 *
 */
int RtdImage::viewCmd(int argc, char* argv[])
{
    RtdImage* view = getView(argv[1]);
    if (! view)
	return TCL_ERROR;

    if (strcmp(argv[0], "update") == 0) {
	if (! image_)
	    return TCL_OK;	// can't update if image doesn't exist

	if (argc == 5) {
	    // only update width and height of image
	    double width, height;
	    char* from_type = argv[4];
	    char* to_type = "image";
	    if (convertCoordsStr(1, argv[2], argv[3], NULL, NULL, width, height, 
				 from_type, to_type) != TCL_OK) 
		return TCL_ERROR;

	    // add 1 so that there is no space left at right and bottom
	    // when only a partial zoomed pixel is displayed on the left or top
	    view->reqWidth_ = width+1;  
	    view->reqHeight_ = height+1; 
	    return view->updateView(image_, 1);
	} 
	else if (argc != 11)
	    return error("usage: $image view update $view xOffset yOffset ",
			 "width height frameX frameY rapidX rapidY coordType");

	// get image coords and update image offsets
	double xOffset, yOffset, width, height, frameX, frameY, rapidX, rapidY;
	char* from_type = argv[10];
	char* to_type = "image";
	if (convertCoordsStr(1, argv[2], argv[3], NULL, NULL, xOffset, yOffset, 
			     from_type, to_type) != TCL_OK
	    || convertCoordsStr(1, argv[4], argv[5], NULL, NULL, width, height, 
				from_type, to_type) != TCL_OK
	    || convertCoordsStr(1, argv[6], argv[7], NULL, NULL, frameX, frameY, 
				from_type, to_type) != TCL_OK 
	    || convertCoordsStr(1, argv[8], argv[9], NULL, NULL, rapidX, rapidY, 
				from_type, to_type) != TCL_OK) 
	    return TCL_ERROR;
#ifdef DEBUG
	if (verbose()) 
	    printf("update %s from %s: xyOffset(%g,%g), size(%g,%g), frame(%g,%g), rapid(%g,%g)\n", 
		   view->name(), name(), xOffset, yOffset, width, height, frameX, frameY, 
		   rapidX, rapidY);
#endif
	
	view->xOffset_ = xOffset;
	view->yOffset_ = yOffset;
	// add 1 so that there is no space left at right and bottom
	// when only a partial zoomed pixel is displayed on the left or top
	view->reqWidth_ = width+1;
	view->reqHeight_ = height+1;
	view->frameX_ = frameX;
	view->frameY_ = frameY;
	view->rapidX_ = rapidX;
	view->rapidY_ = rapidY;
	return view->updateView(image_, 1);
    }
    else if (strcmp(argv[0], "add") == 0) {
	// add the view to the list
	int propagateScale = 1, rapidFrame = 0;
	if (argc >= 3) {
	    if (Tcl_GetBoolean(interp_, argv[2], &propagateScale) != TCL_OK) 
		return TCL_ERROR;
	}
	if (argc >= 4) {
	    if (Tcl_GetBoolean(interp_, argv[3], &rapidFrame) != TCL_OK) 
		return TCL_ERROR;
	}
	// allow the zoom window to be updated from the new view also
	// note: the check for displaymode() != 0 is to keep the pan window
	// from using the zoom window, which can be very slow on huge images.
	if (view->displaymode() != 0) {
	    view->zoomer_ = zoomer_;
	    view->zoomView_ = zoomView_;
	    view->zoomView2_ = zoomView2_;
	    view->zoomSpeed_ = zoomSpeed_;
	}

	// set flags
	view->propagateScale_ = propagateScale;
	view->rapidFrame_ = rapidFrame;

	// we only need one event handler per window
	if (view->tkwin_ == tkwin_) {
	    Tk_DeleteEventHandler(tkwin_, ButtonMotionMask|StructureNotifyMask, 
				  eventProc, (ClientData)view);
	}

	return addView(view);
    }
    else if (strcmp(argv[0], "remove") == 0) {
	return removeView(view);
    }
    else if (strcmp(argv[0], "enter") == 0) {
	currentView_ = view;
    }
    else if (strcmp(argv[0], "leave") == 0) {
	currentView_ = this;
    }
    else {
	return error("invalid rtdimage view subcommand");
    }	
    return TCL_OK;
}


/*
 * return the frame Id of this image. The frame Id is used to
 * identify the image to the Rtd Server for use with rapid frames.
 */
int RtdImage::frameidCmd(int argc, char* argv[])
{
    return set_result(frameId_);
}


/*
 * colorramp subcommand: generate an image displaying the colors
 * in the colormap (no arguments required).
 */
int RtdImage::colorrampCmd(int argc, char* argv[])
{
    int w = Tk_Width(tkwin_); 
    int h = Tk_Height(tkwin_); 
    if (w == 1 && h == 1)
	return TCL_OK; // wait for resize event on image window

    Mem data(w*h, 0), header;
    if (data.status() != 0)
	return TCL_ERROR;

    double scale = 255.0/w;
    char* p = (char*)data.ptr();
    for (int i = 0; i < w; i++) {
	p[i] = (int)(i * scale);
    }
    for (int j = 0; j < h; j++) {
	memcpy(p+(j*w), p, w);
    }
    if (image_)
	delete image_;
    FitsIO* fits = new FitsIO(w, h, BYTE_IMAGE, 0.0, 1.0, header, data);
    if (fits) {
	image_ = makeImage(fits);
	return initNewImage();
    }
    return ERROR;
}


/*
 * This method implements the rtd dump subcommand to dump (save) the
 * current image or a section of it to the given file in FITS format.
 *
 * Usage: $image dump $filename ?x0 y0 x1 y1?
 *
 * If the coordinates are specified, the given section of the image (in
 * image coordinates) is saved in the given file, otherwise the entire
 * image. The FITS header from the original image is reused and the
 * relevant values are modified if only a section of the image is being
 * saved.
 */
int RtdImage::dumpCmd(int argc, char* argv[])
{
    if (! image_)
	return error("no image is currently loaded");

    if (argc == 1) {
	// save whole image
	return image_->write(argv[0]);
    }

    // save image section
    double x0, y0, x1, y1;
    if (Tcl_GetDouble(interp_, argv[1], &x0) != TCL_OK
	|| Tcl_GetDouble(interp_, argv[2], &y0) != TCL_OK
	|| Tcl_GetDouble(interp_, argv[3], &x1) != TCL_OK
	|| Tcl_GetDouble(interp_, argv[4], &y1) != TCL_OK) {
	return TCL_ERROR;
    }

    return image_->write(argv[0], x0, y0, x1, y1);
}


/* 
 * local utility to format a floting point value in arcsec 
 * as minutes and seconds
 */
static void formatHM(double val, char* buf)
{
    int sign = 1;
    if (val < 0.0) {
	sign = -1;
	val = -val;
    }
    double dd = val + 0.0000000001;
    double md = dd / 60;
    int min = (int)md;
    double sec = (md - min) * 60;
    if (min != 0.0) {
	sprintf(buf, "%02d:%02.2f",  min*sign, sec);
    }
    else {
	sprintf(buf, "%02.2f",  sec*sign);
    }
    // cout << "formatHM: " << val << " == " << buf << endl;
}


/*
 * mband subcommand - draw a measure band on the canvas to show the
 * distance in world coordinates (diagonal, vertical and horizontal).
 *
 * This method was originaly implemented in Tcl/[incr Tcl], but was
 * redone here for better performance. The canvas tags used correspond to
 * items created in the itcl class RtdImageMBand:
 *
 * 	mband               all items         
 * 	mband_line          diagonal line
 * 	mband_angle         angle line (horiz. and vert.)
 *
 * 	mband_width_rect    boxs around labels
 * 	mband_height_rect
 * 	mband_diag_rect
 *
 * 	mband_width_text    labels
 * 	mband_height_text
 * 	mband_diag_text
 *
 * Usage:
 * 	$image mband x0 y0 x1 y1 cord_type show_angle
 *
 * Where x0 and y0 are the starting coordinates of the drag, x1 and y1
 * are the coordinates from the motion events, both in the given
 * coordinate system.
 *
 * show_angle is a flag: if true, show the horizontal and vertical
 * distance, otherwise only the diagonal.
 */
int RtdImage::mbandCmd(int argc, char* argv[])
{
    if (!isWcs())
	return TCL_OK;

    // get args
    double x0, y0, x1, y1;
    int show_angle; 
    char* from_type = argv[4];
    char* to_type = "canvas";
    char buf[1024];

    if (Tcl_GetInt(interp_, argv[5], &show_angle) != TCL_OK) 
	return TCL_OK;

    // convert to canvas coords
    if (convertCoordsStr(0, argv[0], argv[1], NULL, NULL, x0, y0, from_type, to_type) != TCL_OK
	|| convertCoordsStr(0, argv[2], argv[3], NULL, NULL, x1, y1, from_type, to_type) != TCL_OK) {
	return TCL_OK;
    }

    // clip to image bounds
    double ix0 = 1, 
	   iy0 = 1, 
	   ix1 = image_->width()-1, 
	   iy1 = image_->height()-1;
    if (imageToCanvasCoords(ix0, iy0, 0) != TCL_OK
	|| imageToCanvasCoords(ix1, iy1, 0) != TCL_OK)
	return TCL_OK;
    clip(x0, ix0, ix1);
    clip(x1, ix0, ix1);
    clip(y0, iy0, iy1);
    clip(y1, iy0, iy1);
    
    // note: wcs coords are not linear, so we need all 3 points in wcs
    double ra0 = x0, dec0 = y0, ra1 = x1, dec1 = y1, ra2 = x1, dec2 = y0;
    if (canvasToWorldCoords(ra0, dec0, 0) != TCL_OK
	|| canvasToWorldCoords(ra1, dec1, 0) != TCL_OK
	|| canvasToWorldCoords(ra2, dec2, 0) != TCL_OK)
	return TCL_OK;
   
    // get distances in world coords
    double width, height, dist = WorldCoords::dist(ra0, dec0, ra1, dec1)*60.;
    char widthStr[32], heightStr[32], distStr[32];
    formatHM(dist, distStr);
    if (show_angle) {
 	width = WorldCoords::dist(ra0, dec0, ra2, dec2)*60.;
	formatHM(width, widthStr);
 	height = WorldCoords::dist(ra2, dec2, ra1, dec1)*60;
 	formatHM(height, heightStr);
    }
	
    // calculate canvas coords for lines and labels and
    // try to keep the labels out of the way so they don't block anything
    double mx = (x0 + x1)/2;
    double my = (y0 + y1)/2;
    int offset = 10;		// offset of labels from lines

    char* diag_anchor = "c";	// label anchors
    char* width_anchor = "c";
    char* height_anchor = "c";

    int diag_xoffset = 0,	// x,y offsets for labels
	diag_yoffset = 0,
	width_yoffset = 0,
	height_xoffset = 0;
	
    if (fabs(y0 - y1) < 5) {
	diag_anchor = "s";
	diag_yoffset = offset;
	show_angle = 0;
    } 
    else if (y0 < y1) {
	width_anchor = "s";
	width_yoffset = -offset;
    } 
    else {
	width_anchor = "n";
	width_yoffset = offset;
    }
  
    if (fabs(x0 - x1) < 5) {
	diag_anchor  = "w";
	diag_xoffset = offset;
	diag_yoffset = 0;
	show_angle = 0;
    } else if (x0 < x1) {
	diag_anchor = "se";
	diag_xoffset = -offset;
	diag_yoffset = offset;
	height_anchor = "w";
	height_xoffset = offset;
    } 
    else {
	diag_anchor = "nw";
	diag_xoffset = offset;
	diag_yoffset = -offset;
	height_anchor = "e";
	height_xoffset = -offset;
    }

    // evaluate Tk canvas commands in the image's canvas
    const char* canvas = canvasName_;

    // set diagonal line coords
    sprintf(buf, "%s coords mband_line %g %g %g %g\n", 
	      canvas, x0, y0, x1, y1);
    Tcl_Eval(interp_, buf);

    // adjust labels
    sprintf(buf, "%s coords mband_diag_text %g %g\n", 
	      canvas, mx+diag_xoffset, my+diag_yoffset);
    Tcl_Eval(interp_, buf);

    sprintf(buf, "%s itemconfig mband_diag_text -text %s -anchor %s\n", 
	    canvas, distStr, diag_anchor);
    Tcl_Eval(interp_, buf);

    sprintf(buf, "%s bbox mband_diag_text\n", canvas);
    Tcl_Eval(interp_, buf);

    double rx0, ry0, rx1, ry1;
    if (sscanf(interp_->result, "%lf %lf %lf %lf", &rx0, &ry0, &rx1, &ry1) != 4) 
	return TCL_OK;

    sprintf(buf, "%s coords mband_diag_rect %g %g %g %g\n", 
	      canvas,  rx0, ry0, rx1, ry1);
    Tcl_Eval(interp_, buf);

    if (show_angle) {
	// set angle line coords
	sprintf(buf, "%s coords mband_angle %g %g %g %g %g %g\n", 
		  canvas, x0, y0, x1, y0, x1, y1);
	Tcl_Eval(interp_, buf);
    
	sprintf(buf, "%s coords mband_width_text %g %g\n",
		  canvas, mx, y0+width_yoffset);
	Tcl_Eval(interp_, buf);

	sprintf(buf, "%s itemconfig mband_width_text -text %s -anchor %s\n",
		  canvas, widthStr, width_anchor);
	Tcl_Eval(interp_, buf);

	sprintf(buf, "%s bbox mband_width_text\n", canvas);
	Tcl_Eval(interp_, buf);

	if (sscanf(interp_->result, "%lf %lf %lf %lf", &rx0, &ry0, &rx1, &ry1) != 4) 
	    return TCL_OK;
	sprintf(buf, "%s coords mband_width_rect %g %g %g %g\n", 
		  canvas,  rx0, ry0, rx1, ry1);
	Tcl_Eval(interp_, buf);

	sprintf(buf, "%s coords mband_height_text %g %g\n",
		  canvas, x1+height_xoffset, my);
	Tcl_Eval(interp_, buf);

	sprintf(buf, "%s itemconfig mband_height_text -text %s -anchor %s\n",
		  canvas, heightStr, height_anchor);
	Tcl_Eval(interp_, buf);

	sprintf(buf, "%s bbox mband_height_text\n", canvas);
	Tcl_Eval(interp_, buf);

	if (sscanf(interp_->result, "%lf %lf %lf %lf", &rx0, &ry0, &rx1, &ry1) != 4) 
	    return TCL_OK;
	sprintf(buf, "%s coords mband_height_rect %g %g %g %g\n", 
		  canvas,  rx0, ry0, rx1, ry1);
	Tcl_Eval(interp_, buf);
    } 
    else {
	// hide the width and height labels and lines
	    x1 = x0 + 1;
	    y1 = y0 + 1;
	    sprintf(buf, "%s coords mband_angle %g %g %g %g\n", canvas, x0, y0, x1, y1);
	    Tcl_Eval(interp_, buf);

	    sprintf(buf, "%s itemconfig mband_width_text -text {}\n", canvas);
	    Tcl_Eval(interp_, buf);

	    sprintf(buf, "%s coords mband_width_rect %g %g %g %g\n", canvas, x0, y0, x1, y1);
	    Tcl_Eval(interp_, buf);

	    sprintf(buf, "%s itemconfig mband_height_text -text {}\n", canvas);
	    Tcl_Eval(interp_, buf);

	    sprintf(buf, "%s coords mband_height_rect %g %g %g %g", canvas, x0, y0, x1, y1);
	    Tcl_Eval(interp_, buf);
     }
    return TCL_OK;
}


/*
 * warp the mouse pointer by the given x and y amounts:
 *
 * usage: $image warp $x $y
 */
int RtdImage::warpCmd(int argc, char* argv[])
{
    int x, y;

    if ((Tcl_GetInt(interp_, argv[0], &x) == TCL_ERROR) ||
	(Tcl_GetInt(interp_, argv[1], &y) == TCL_ERROR))
	return TCL_ERROR;

    XWarpPointer(display_, None, None, 0, 0, 0, 0, x, y);
    return TCL_OK;
}


/*
 * implement the pixtab subcommand to support displaying a table of
 * pixel values around a point.
 *
 * usage: $image pixtab start nrows ncols
 *        $image pixtab stop
 *
 * All this commmand does is set a flag causing Tcl array variables
 * to be updated on motion events, which can cause the display to be 
 * updated via the "-textvariable" widget option on the table items.
 *
 * The array name is fixed as: RtdPixTab and the elements are indexed as
 * $RtdPixTab(i,j), where the left and top sides of the table (array) are
 * the X and Y image coordinates, resp. and the rest are image pixel
 * values.
 *
 */
int RtdImage::pixtabCmd(int argc, char* argv[])
{
    if (strcmp(argv[0], "start") == 0) {
	if (argc != 3)
	    return error("expected: $image pixtab start nrows ncols");

	int nrows, ncols;
	if ((Tcl_GetInt(interp_, argv[1], &nrows) == TCL_ERROR) ||
	    (Tcl_GetInt(interp_, argv[2], &ncols) == TCL_ERROR))
	    return TCL_ERROR;

	if (nrows <= 0 || ncols <= 0)
	    return error("number of rows and columns should be positive");

	// force value to be odd so we can center it
	if ((nrows&1) == 0)
	    nrows++;
	if ((ncols&1) == 0)
	    ncols++;

	pixTabRows_ = nrows;
	pixTabCols_ = ncols;

	if (pixTab_)
	    delete pixTab_;

	// generate an array of pixel values with left and right headings
	// for the x/y coordinates
	pixTab_ = new double[++nrows*++ncols];
	if (pixTab_) 
	    memset((void*)pixTab_, '\0', nrows*ncols*sizeof(double));
    }
    else if (strcmp(argv[0], "stop") == 0) {
	if (pixTab_)
	    delete pixTab_;
	pixTab_ = NULL;
    }
    else {
	return error("expected image pixtab 'start nrows ncols' or 'stop'");
    }
    return TCL_OK;
}


/*
 * implement the "remote" subcommand for remote control of the RTD image
 * widget.
 *
 * usage: $image remote ?$port?
 *
 * If a port number argument is specified The widget will start listening
 * for commands on the given port. If port is 0, a port number will be
 * chosen.
 *
 * If no port number is specified, the current port number is returned,
 * or "" if there is none. This is a way to determine the port number 
 * at the Tcl level.
 */
int RtdImage::remoteCmd(int argc, char* argv[])
{
    if (argc == 0) {
	if (remote_)
	    return set_result(remote_->port());
	return TCL_OK;
    }

    char* cmd = "";
    int port = 0;

    if (Tcl_GetInt(interp_, argv[0], &port) == TCL_ERROR)
	return TCL_ERROR;
    
    if (remote_)
	delete remote_;

    remote_ = new RtdImageRemote(this, port);
    if (remote_) 
	return remote_->status();

    return TCL_ERROR;
}


/*
 * implement the "remotetcl" subcommand to evaluate a Tcl command
 * in the RTD Tcl interpreter.
 *
 * usage: $image remotetcl $command
 */
int RtdImage::remoteTclCmd(int argc, char* argv[])
{
    Tcl_Eval(interp_, argv[0]);
    return set_result(interp_->result);
}


/*
 * implement the "convert" subcommand to convert between different
 * coordinate representations.
 *
 * usage: 
 *     $image convert coords inx iny input_coord_type outx outy output_coord_type
 *     $image convert dist inx iny input_coord_type outx outy output_coord_type
 *
 * where inx and iny and the input coords (or distance) in the given
 * input coordinate system. "convert coords" treats x,y as a point, while
 * "convert dist" treats it as a distance. outx and outy, if not empty,
 * are the names of variables that will hold the resulting coordinates.
 * If outx and outy are empty strings, the values are returned as a tcl
 * list "x y".
 *
 * The available coordinate systems are:
 *
 *     canvas     - canvas coordinates (canvas scroll area)
 *     screen     - canvas window coords (visible area)
 *     image      - basic image pixel coords (at mag 1, no transformations)
 *     wcs        - world coordinates in H:M:S
 *     deg        - world coordinates in degrees
 *
 * The world coordinate types: "wcs" and "deg" may also include the
 * equinox (default is 2000): Example:
 *
 *     $image convert coords $ra $dec "wcs 1950" x y canvas
 *
 * Note: the coordinate types may be abbrieviated, since only the first
 * char is actually checked.
 */
int RtdImage::convertCmd(int argc, char* argv[])
{
    if (!image_)
	return error("no image loaded");

    char* usage = "usage: $image convert [coords|dist] inx iny in_coord_type outx outy out_coord_type";
    int dist_flag = 0;
    if (strcmp(argv[0], "dist") == 0)
	dist_flag++;
    else if (strcmp(argv[0], "coords") != 0)
	return error(usage);

    char outx_buf[32], outy_buf[32];
    char* outx_name = argv[4];
    char* outy_name = argv[5];

    // if no variable names are specified, return the values as a list
    if (strlen(outx_name) == 0) 
	outx_name = NULL;
    if (strlen(outy_name) == 0) 
	outy_name = NULL;
    
    double x, y;
    if (convertCoordsStr(dist_flag, argv[1], argv[2], outx_buf, outy_buf, 
			 x, y, argv[3], argv[6]) != TCL_OK)
	return TCL_ERROR;

    Tcl_ResetResult(interp_);
    if (outx_name) 
	Tcl_SetVar(interp_, outx_name, outx_buf, 0);
    else
	Tcl_AppendElement(interp_, outx_buf);

    if (outy_name) 
	Tcl_SetVar(interp_, outy_name, outy_buf, 0);
    else
	Tcl_AppendElement(interp_, outy_buf);

    return TCL_OK;
}


/*
 * implement the "mmap" subcommand:
 *
 * usage: $image mmap set $data_filename $data_offset $data_owner \
 *                       ?$header_filename $header_offset $header_owner?
 *        $image mmap get data
 *        $image mmap get header
 *        $image mmap create $filename $size
 *        $image mmap delete $filename
 *        $image mmap update
 *
 * This subcommand provides access to the mmap shared memory in which the
 * FITS image data and header are stored. Image files are always mapped
 * with mmap by default (since it is faster than reading the
 * file). Applications can take advantage of this to modify the image
 * data and then notify the application to update the image. This command
 * makes it posible to put the image data and header in separate files,
 * so that they can be more easily updated by other applications. If you
 * want to put both header and data in the same file in the normal way,
 * just use "$image config -file". Otherwise you can use this command to
 * quickly update the image data in a separate file.
 *
 * The "set" command allow you to set the files to use to for the image
 * data and header. The data and header in the specified files should be
 * in FITS format (i.e.:, a FITS file split in 2 parts). If the header
 * is not specified, the previous header is reused, if there was one.
 * The offset arguments indicate an offset in the file where the header
 * or data start. If the file contains only the data or only the header,
 * the offset argument should be set to 0.
 * A flag indicating who "owns" the file may be specified (if true, then
 * the file will be deleted when no longer needed).
 *
 * Example: $image mmap set datafile1 0 0 headerfile1 0 0
 *          $image mmap set datafile2 0 0
 *          ...
 *
 * The "get" command returns mmap information about the data or header.
 * If the data or header is not currently mapped, an error is returned.
 * The return value is a list of the form {filename offset owner}, the
 * same as the arguments to the "$image mmap set" command.
 *
 * The "create" command creates a new mmapped file with the given name
 * and the given size. The mmaped file/memory should be released with the
 * "delete" subcommand when no longer needed.
 *
 * The "delete" command unmaps the given file and deletes it, if it was
 * created with the "mmap create" subcommand.
 *
 * The "update" command causes the display to be updated to reflect any
 * changes in the image memory.
 */
int RtdImage::mmapCmd(int argc, char* argv[])
{
    char* msg = "invalid arguments for mmap subcommand";
    
    // this is used to keep track of memory areas for the "create" and
    // "delete" subcommands here
    static Mem* mem_areas[10];
    const int max_mem_areas = sizeof(mem_areas)/sizeof(Mem*);

    if (strcmp(argv[0], "set") == 0) { // $image mmap set ... 
	if (argc != 4 && argc != 7)
	    return error(msg);

	char* data_filename = argv[1];
	char* header_filename = NULL;
	int data_offset = 0, data_owner = 0, data_size = 0,
	    header_offset = 0, header_owner = 0, header_size = 0;

	if (Tcl_GetInt(interp_, argv[2], &data_offset) == TCL_ERROR
	    || Tcl_GetBoolean(interp_, argv[3], &data_owner) == TCL_ERROR)
	    return TCL_ERROR;
	
	if (argc == 7) {
	    header_filename = argv[4];
	    if (Tcl_GetInt(interp_, argv[5], &header_offset) == TCL_ERROR
		|| Tcl_GetBoolean(interp_, argv[6], &header_owner) == TCL_ERROR)
		return TCL_ERROR;
	}
	Mem data(data_filename, verbose());
	if (data.status() != 0)
	    return TCL_ERROR;
	if (data_offset)
	    data.offset(data_offset);
	if (data_owner)
	    data.owner(data_owner);

	Mem header;
	if (! header_filename) {
	    // if there is no header, check that image has right size at least
	    if (! image_)
		return error("no current image header to go with mmap data");
	    if (data.length() < image_->data().length())
		return error("mmap data file is to small for current image header");
	    header = image_->header();
	} 
	else {
	    header = Mem(header_filename, verbose());
	    if (header.status() != 0)
		return TCL_ERROR;
	    if (header_offset)
		header.offset(header_offset);
	    if (header_owner)
		header.owner(header_owner);
	}

	// used to save and restore image transformation parameters
	ImageDataParams p;
    
	if (image_) {
	    image_->saveParams(p);
	    delete image_; 
	    image_ = NULL;
	    updateViews();
	}

	FitsIO* fits = FitsIO::initialize(header, data);
	image_ = makeImage(fits);
	if (! image_)
	    return TCL_ERROR;

	// restore transformations
	image_->restoreParams(p, !autoSetCutLevels_);

	return initNewImage();
    }
    else  if (strcmp(argv[0], "get") == 0) { // $image mmap get ...
	if (argc != 2)
	    return error(msg);
	if (! image_)
	    return error("no image is currently loaded");
	Mem m;
	if (strcmp(argv[1], "data") == 0) {
	    if (image_->data().filename() == NULL)
		return error("image data is not mapped");
	    m = image_->data();
	}
	else if (strcmp(argv[1], "header") == 0) {
	    if (image_->header().filename() == NULL)
		return error("image header is not mapped");
	    m = image_->header();
	}
	else {
	    return error(msg);
	}
	reset_result();
	append_element(m.filename());
	append_element((int)m.offset());
	return append_element(m.owner());
    }
    else  if (strcmp(argv[0], "update") == 0) {
	return updateImage();
    }
    else  if (strcmp(argv[0], "create") == 0) {
	if (argc != 3) 
	    return error(msg);
	char* filename = argv[1];
	int size = 0;
	if (Tcl_GetInt(interp_, argv[2], &size) == TCL_ERROR)
	    return TCL_ERROR;
	for (int i = 0; i<max_mem_areas; i++) {
	    if (mem_areas[i] == NULL) {
		Mem* m = new Mem(size, filename, 1, verbose());
		if (m && m->status() == 0) {
		    mem_areas[i] = m;
		    return TCL_OK;
		}
		return TCL_ERROR;
	    }
	}
	return error("too many mmap files for 'mmap create' subcommand");
    }
    else  if (strcmp(argv[0], "delete") == 0) {
	if (argc != 2) 
	    return error(msg);
	char* filename = argv[1];
	for (int i = 0; i<max_mem_areas; i++) {
	    if (mem_areas[i] && strcmp(mem_areas[i]->filename(), filename) == 0) {
		delete mem_areas[i];
		mem_areas[i] = NULL;
		return TCL_OK;
	    }
	}
	return error("the specified file was not created with the 'mmap create' subcommand");
    }
    else {
 	return error(msg);
    }
    return TCL_OK;
}


/*
 * implement the "shm" subcommand to manipulate image sysV shared memory:
 *
 * usage: $image shm set $data_size $data_id $data_owner \
 *                       ?$header_size $header_id $header_owner?
 *        $image shm get data
 *        $image shm get header
 *        $image shm create $size
 *        $image shm delete $Id
 *        $image shm update
 *
 * This subcommand provides access to the sysV shared memory in which the
 * FITS raw image data and header are stored. The raw image is stored in
 * sysV shared memory if the -shm_data option was specified when creating
 * the image and the header is stored in sysV shared memory if the
 * -shm_headr option was specified.
 *
 * The "set" command allow you to set the shared memory Ids to use to
 * access the image data and header. The data and header in the area
 * specified should be in FITS format. If the header is not specified,
 * the previous header is reused. For both data and header, the size of
 * the area (in bytes) and the shared memory Id must be specified. In
 * addition a flag indicating who "owns" the shared memory is specified
 * (if true, then the area will be deleted when no longer needed).
 *
 * The "get" command returns the shared memory Id of the data or header.
 * If the data or header is not currently in shared memory, it is copied
 * to a new shared memory area and the Id for this area is returned.
 *
 * The "create" command creates a new shared memory area with the given
 * size and returns the Id. The memory should be deleted with the "delete"
 * subcommand when no longer needed.
 *
 * The "delete" command deletes the shared memory with the given Id (which
 * should have been returned from the "create" subcommand).
 *
 * The "update" command causes the display to be updated to reflect any changes
 * in the image memory.
 */
int RtdImage::shmCmd(int argc, char* argv[])
{
    char* msg = "invalid arguments for shm subcommand";
    
    // this is used to keep track of memory areas for the "create" and
    // "delete" subcommands here
    static Mem* mem_areas[10];
    const int max_mem_areas = sizeof(mem_areas)/sizeof(Mem*);

    if (strcmp(argv[0], "set") == 0) {
	if (argc != 4 && argc != 7)
	    return error(msg);

	int data_size = 0, data_id = -1, data_owner = 0,
	    header_size = 0, header_id = -1, header_owner = 0;

	if (Tcl_GetInt(interp_, argv[1], &data_size) == TCL_ERROR
	    || Tcl_GetInt(interp_, argv[2], &data_id) == TCL_ERROR
	    || Tcl_GetBoolean(interp_, argv[3], &data_owner) == TCL_ERROR)
	    return TCL_ERROR;
	
	if (argc == 7) {
	    if (Tcl_GetInt(interp_, argv[4], &header_size) == TCL_ERROR
		|| Tcl_GetInt(interp_, argv[5], &header_id) == TCL_ERROR
		|| Tcl_GetBoolean(interp_, argv[6], &header_owner) == TCL_ERROR)
		return TCL_ERROR;
	}
	Mem data(data_size, data_id, data_owner, verbose());
	if (data.status() != 0)
	    return TCL_ERROR;

	Mem header;
	if (header_id < 0) {
	    // if there is no header, check that image has right size at least
	    if (! image_)
		return error("no current image header to go with shm data");
	    if (data_size < image_->data().length())
		return error("shared memory area is to small for current image");
	    header = image_->header();
	} 
	else {
	    header = Mem(header_size, header_id, header_owner, verbose());
	}
	if (header.status() != 0)
	    return TCL_ERROR;

	// used to save and restore image transformation parameters
	ImageDataParams p;
    
	if (image_) {
	    image_->saveParams(p);
	    delete image_; 
	    image_ = NULL;
	    updateViews();
	}

	FitsIO* fits = FitsIO::initialize(header, data);
	image_ = makeImage(fits);
	if (! image_)
	    return TCL_ERROR;

	// restore transformations
	image_->restoreParams(p, !autoSetCutLevels_);

	return initNewImage();
	
    }
    else  if (strcmp(argv[0], "get") == 0) {
	if (argc != 2)
	    return error(msg);
	if (! image_)
	    return error("no image is currently loaded");
	if (strcmp(argv[1], "data") == 0) {
	    if (image_->data().shared(1) != 0)
		return TCL_ERROR;
	    return set_result(image_->data().shmId());
	}
	else if (strcmp(argv[1], "header") == 0) {
	    if (image_->header().shared(1) != 0)
		return TCL_ERROR;
	    return set_result(image_->header().shmId());
	}
	else {
	    return error(msg);
	}
    }
    else  if (strcmp(argv[0], "update") == 0) {
	return updateImage();
    }
    else  if (strcmp(argv[0], "create") == 0) {
	if (argc != 2) 
	    return error(msg);
	int size = 0;
	if (Tcl_GetInt(interp_, argv[1], &size) == TCL_ERROR)
	    return TCL_ERROR;
	for (int i = 0; i<max_mem_areas; i++) {
	    if (mem_areas[i] == NULL) {
		Mem* m = new Mem(size, 1, verbose());
		if (m && m->status() == 0) {
		    mem_areas[i] = m;
		    return set_result(m->shmId());
		}
		return TCL_ERROR;
	    }
	}
	return error("too many shared memory areas for 'shm create' subcommand");
    }
    else  if (strcmp(argv[0], "delete") == 0) {
	if (argc != 2) 
	    return error(msg);
	int shmId = -1;
	if (Tcl_GetInt(interp_, argv[1], &shmId) == TCL_ERROR)
	    return TCL_ERROR;
	for (int i = 0; i<max_mem_areas; i++) {
	    if (mem_areas[i] && mem_areas[i]->shmId() == shmId) {
		delete mem_areas[i];
		mem_areas[i] = NULL;
		return TCL_OK;
	    }
	}
	return error("the specified shared memory area was not created with the 'shm create' subcommand");
    }
    else {
 	return error(msg);
    }
    return TCL_OK;
}


/*
 * statistics subcommand: calculate statistics on the section of
 * the image being displayed.
 *
 * usage:  set list [$image statistics]
 *
 * The return value in Tcl is a list of the following values: 
 *
 * {x y ra dec equinox fwhmX fwhmY angle objectPeak meanBackground}
 *
 * where:
 *
 * x              = X image coordinate
 * y              = Y image coordinate
 * ra             = RA position (calculated from mean X pos)
 * dec            = DEC position (calculated from mean Y position) 
 * equinox        = equinox of RA and DEC
 * fwhmX          = FWHM in X
 * fwhmY          = FWHM in Y
 * angle          = angle of major axis, degrees, along X
 * objectPeak     = peak value of object above background
 * meanBackground = mean background level
 *
 */
int RtdImage::statisticsCmd(int argc, char* argv[])
{
    if (!image_)
	return error("no image loaded");

    double w = reqWidth_, h = reqHeight_;
    undoTrans(w, h, 1); // get only visible area
    double x = xOffset_, y = yOffset_;

    // XXX not sure why this is needed
    switch(image_->flipX()<<1|image_->flipY()) {
    case 0: // none
	y += h;
	break;
    case 1: // flipY
	break;
    case 2: // flipX
	y += h;
	x += w;
	break;
    case 3: // flipX and flipY
	x += w;
	break;
    }

    distToCoords(x, y); // get coords from offsets
    
    double meanX=0., meanY=0., fwhmX=0., fwhmY=0., angle=0., 
	objectPeak=0., meanBackground=0.;

    if (image_->getStatistics(x, y, int(w), int(h), meanX, meanY, fwhmX, fwhmY,
			      angle, objectPeak, meanBackground) != 0) {
	// if we could not get the FWHM (because the user clicked on the background
	// of the image), we still want to return the X,Y values of the area clicked.
	meanX = w/2;  // allan: 22.4.98, by request
	meanY = h/2;
    }
    // get the image coords from the offsets
    x += meanX;
    y +=  meanY;
    double ix = x, iy = y;
    
    // get the world coords position from the image coords
    WorldCoords pos;
    if (imageToWorldCoords(x, y, 0) == TCL_OK) {
	pos = WorldCoords(x, y);
	if (pos.status() != 0)
	    pos = WorldCoords();
    }

    char buf[1024];
    ostrstream os(buf, sizeof(buf));
    os << ix << ' ' << iy << ' ';
    
    if (pos.status() == 0 && ! pos.isNull())
	os << pos << " J2000 ";	// ra, dec, equinox: XXX use default equinox ?
    else 
	os << "{} {} {} ";	// no world coords

    os << fwhmX << ' ' << fwhmY << ' ' << angle << ' ' 
       << objectPeak << ' ' << meanBackground 
       << ends;

    return set_result(buf);
}


/*
 * wcsset subcommand: 
 *
 * usage:
 *
 *    $image wcsset $ra $dec $secpix $xrefpix $yrefpix $nxpix $nypix $rotate \ 
 *                  $equinox $epoch $proj
 *    $image wcsset
 *
 * If arguments are specified, this subcommand sets up the WCS structure
 * from the given information about the image:
 *
 *    Args:
 * 	ra      = Center right ascension in degrees 
 * 	dec     = Center declination in degrees 
 * 	secpix  = Number of arcseconds per pixel 
 *      xrefpix = Reference pixel X coordinate
 *      yrefpix	= Reference pixel Y coordinate
 * 	nxpix   = Number of pixels along x-axis 
 * 	nypix   = Number of pixels along y-axis 
 * 	rotate  = Rotation angle (clockwise positive) in degrees 
 * 	equinox = Equinox of coordinates, 1950 and 2000 supported 
 * 	epoch   = Epoch of coordinates, used for FK4/FK5 conversion no effect if 0 
 *      proj    = Projection 
 *
 * With no arguments, the command returns a list of the basic WCS
 * parameter values: {ra dec secpix nxpix nypix rotate equinox epoch}.
 *
 *
 */
int RtdImage::wcssetCmd(int argc, char* argv[])
{
    if (!image_)
	return TCL_OK;

    // get a reference to the WCS object for the current image
    WCS& wcs = image_->wcs();

    if (argc == 0) {
	// no args, return list of values
	char buf[256];
	if (wcs.isWcs()) {
	    
	    // get RA and DEC of the center of the image
	    char raStr[32], decStr[32];
	    raStr[0] = decStr[0] = '\0';
	    WorldCoords pos = wcs.center();
	    if (pos.status() != 0) 
		return TCL_ERROR;
	    pos.print(raStr, decStr, wcs.equinox());

	    // make the result list
	    sprintf(buf, "%s %s %g %g %g %d %d %g %g %g %s", 
		    raStr, decStr, wcs.secPix(), 
		    wcs.xRefPix(), wcs.yRefPix(),
		    wcs.pixWidth(), wcs.pixHeight(), 
		    wcs.rotate(), 
		    wcs.equinox(), wcs.epoch(),
		    wcs.projection());
	    return set_result(buf);
	}
	else {
	    // no WCS, return default info
	    sprintf(buf, "{} {} {} {} {} %d %d 0 2000 2000 {}", 
		    image_->width(), image_->height());
	    return set_result(buf);
	}
    }
    else if (argc == 11) {
	double ra, dec, secpix, xrefpix, yrefpix, rotate, equinox, epoch;
	int nxpix, nypix;
	char* proj = "";

	// skip over B in B1950 and J in J2000
	if (strcmp(argv[8], "B1950") == 0)
	    equinox = 1950.;
	else if (strcmp(argv[8], "J2000") == 0)
	    equinox = 2000.;
	else {
	    if (Tcl_GetDouble(interp_, argv[8], &equinox) != TCL_OK)
		return TCL_ERROR;
	    if (equinox != 2000. && equinox != 1950.)
		return error("expected equinox to be 2000. or 1950.");
	}

	// expect ra and dec in H:M:S D:M:S format
	WorldCoords pos(argv[0], argv[1], equinox);
	if (pos.status() != 0)
	    return TCL_ERROR;
	pos.get(ra, dec, equinox); // get ra and dec in the right equinox
	
	// set WCS info
	if (Tcl_GetDouble(interp_, argv[2], &secpix) != TCL_OK
	    || Tcl_GetDouble(interp_, argv[3], &xrefpix) != TCL_OK
	    || Tcl_GetDouble(interp_, argv[4], &yrefpix) != TCL_OK
	    || Tcl_GetInt(interp_, argv[5], &nxpix) != TCL_OK
	    || Tcl_GetInt(interp_, argv[6], &nypix) != TCL_OK
	    || Tcl_GetDouble(interp_, argv[7], &rotate) != TCL_OK
	    || Tcl_GetDouble(interp_, argv[9], &epoch) != TCL_OK) 
	    return TCL_ERROR;

	proj = argv[10];
	return wcs.set(ra, dec, secpix, xrefpix, yrefpix, nxpix, nypix, rotate, 
		       int(equinox), epoch, proj);
    }
    return error("wrong number of arguments for wcsset subcommand");
}


/*
 * wcsshift subcommand
 *
 * usage:
 *    $image wcsshift $ra $dec $coorsys
 *
 * This command resets the center of the WCS structure.
 * 
 *    Args:
 * 	ra        = New center right ascension in degrees 
 * 	dec       = New center declination in degrees 
 * 	equinox   = (must be 2000 or 1950)
 *
 */
int RtdImage::wcsshiftCmd(int argc, char* argv[])
{
    if (!isWcs())
	return TCL_OK;

    double ra, dec, equinox;
    if (Tcl_GetDouble(interp_, argv[0], &ra) != TCL_OK 
	|| Tcl_GetDouble(interp_, argv[1], &dec) != TCL_OK
	|| Tcl_GetDouble(interp_, argv[2], &equinox) != TCL_OK) 
	return TCL_ERROR;
    return image_->wcs().shift(ra, dec, equinox);
}

/*
 *  This method is called to update with new colors if needed. A
 *  forced color update will be necessary when using X visuals which do
 *  not support read-write colour cells.
 */
int RtdImage::colorUpdate( int force )
{
    if ( colors_->readOnly() || force ) {
        if (image_) {
            image_->colorScale(colors_->colorCount(), colors_->pixelval());

            //  Make sure new lookup table is propagated.
            LookupTable lookup = image_->lookup();
            for(int i = 0; i < MAX_VIEWS; i++) {
                if (view_[i] && view_[i]->image_ && !view_[i]->isSeparateRapidFrame())
                  view_[i]->image_->lookup(lookup);
            }
        }
        return updateViews(1) || updateImage();
    }
    return TCL_OK;
}
    

// RtdImage signal handler

void RtdImage_cleanup(int sig) 
{
    Mem_RPTcleanup();    // cleanup shm of Rtd Recorder/Playback tool
    Mem_cleanup(sig);    // cleanup shm of RtdImage and exit
}


