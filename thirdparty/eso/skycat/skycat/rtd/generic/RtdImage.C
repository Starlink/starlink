/*
 * E.S.O. - VLT project 
 * "@(#) $Id: RtdImage.C,v 1.5 2006/02/02 17:36:47 abrighto Exp $"
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
 * P.Biereichel    22/03/99  Added biasimage subcommand
 * Allan Brighton  16/05/99  Fixed isClear() method (removed check for empty header)
 * P.Biereichel    23/08/99  Added HDU methods (copied from skycat)
 *                           Pass on the rapid frame event to the first rapid frame when
 *                           the frameId in the rtdIMAGE_INFO structure doesn't exists.
 *                           Changed RtdImage::Rtd_Init()
 *                 14/09/99  call to initColors() moved from Rtd_Init() to the RtdImage constructor:
 *                           colors are only initialized when needed
 * Allan Brighton  27/09/99  Reversed change, should fix panel editor to only do the package require
 *                           if rtd will be used. The colormap has to be initialized before any
 *                           widgets are created (each widget inherits the colormap from the
 *                           parent widget).
 * pbiereic        11/10/99  Until the panel editor has been fixed (Feb 2000) we use a compile flag
 *                           PANEL_EDITOR_BUG
 *                           Added wcsFlags and wcsdeltset command.
 * pbiereic        13/11/99  More accurate X, Y coords from RtdImage::statisticsCmd()
 * pbiereic        28/04/00  RtdImage::loadFile() opens the file with reading only
 * Peter W. Draper 10/05/00  Always update colormap, some private maps
 *                           are used when screen visual differs to
 *                           requested one and would not otherwise be
 *                           installed. 
 * pbiereic        26/05/00  Added options fillWidth / fillHeight
 * Peter W. Draper 30/05/01  Added "double" as a valid data type.
 * pbiereic        01/03/01  o Added option 'debug'
 *                           o Added static class for performance tests (removed old code)
 *                           o Added method updateRequests() which updates X events
 *                             before calling updateIdleTasks(); only for image events.
 *                           o Since RtdImage.C was becoming too big all member fuctions
 *                             dealing with commands were copied to RtdCmds.C
 *                           o Removed XSyncSetPriority() since this blocked all
 *                             other X applications 
 * pbiereic        28/05/01  Included Allan's changes for tcl8.3.3
 *                           Removed LockMask in motionNotify()  (SPR VLTSW20010313)
 * Allan Brighton  30/07/01  Fixed problem with X shared memory, ssh and X11 forwarding:
 *                           (X shm areas are not freed, so don't use them in this case)
 * pbiereic        17/02/03  Byte order for shm data is determined by the application
 *                           (flag shmEndian in the image event info structure).
 * Peter W. Draper 28/04/03  Added PIXTAB_MINX, MAXX, MINY and MAXY to report
 *                           the positions that the minimum and maximum values
 *                           are found in a table of values.
 * Peter W. Draper 21/10/03  Modified processMotionEvent to pass through
 *                           long thin images of 2 pixels or less in
 *                           either height or width. A blank image is
 *                           2x2, not nx2 or 2xn.
 *                 19/12/05  Undo change that removed LockMask in motionNotify.
 *                           I think that's a handy feature.
 *                 08/01/07  Change isclear to check for RTD_BLANK as the 
 *                           OBJECT value, don't use the size at all. 
 *                           Change processMotionEvent to work for images
 *                           of all sizes (including blank ones). One
 *                           pixel images are possible in cube sections.
 *
 * Allan Brighton  16/12/05  Added local Tk_CanvasWindowCoordsNoClip method (moved from tclutil)
 * Allan Brighton  28/12/05  Replaced init script
 */
static const char* const rcsId="@(#) $Id: RtdImage.C,v 1.5 2006/02/02 17:36:47 abrighto Exp $";

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "define.h"
#include "RtdImage.h"

// Tcl procedure to search for an init for Rtd startup file.  
static char initScript[] = "if {[info proc ::rtd::Init]==\"\"} {\n\
  namespace eval ::rtd {}\n\
  proc ::rtd::Init {} {\n"
#ifdef MAC_TCL
"    source -rsrc RtdInit.tcl\n"
#else
"    global rtd_library\n\
     tcl_findLibrary rtd " PACKAGE_VERSION " " PACKAGE_VERSION " RtdInit.tcl RTD_LIBRARY rtd_library\n"
#endif
"  }\n\
}\n\
::rtd::Init";

// should be in X11/extensions/XShm.h
extern "C" int XShmQueryExtension(Display*);

// should be in X11/extensions/sync.h
extern "C" int XSyncInitialize(Display *, int *, int *);
// extern "C" int XSyncSetPriority(Display *, XID, int);

#if 0
#ifdef NEED_GETHOSTNAME_PROTO
// should be in unistd.h ?
extern "C" int gethostname(char *name, unsigned int namelen);
#endif /* NEED_GETHOSTNAME_PROTO */
#endif

// generated code for bitmaps used in tcl scripts
void defineRtdBitmaps(Tcl_Interp*);

// generated code for colormap files used
void defineColormaps();

// initialize these dependent packages here for backward compat
extern "C" int Tclutil_Init(Tcl_Interp *interp);
extern "C" int Astrotcl_Init(Tcl_Interp *interp);

// these are static members shared by all instances of the image
// and used for managing the color map and bias image, etc.
ImageColor*   RtdImage::colors_     = (ImageColor*) NULL;
BiasData*     RtdImage::biasimage_  = (BiasData*) NULL;
RtdPerf*      RtdImage::rtdperf_    = (RtdPerf*) NULL;

// pointer to last RtdImage instance to handle motion events
// (used to update display on real-time image events)
static RtdImage* motionView_ = NULL;

// Rtd Record 
extern "C" int RtdrecordInit(Tcl_Interp *);

/* 
 * image config options - used to process command line options and for the
 * image "configure" subcommand.
 */

static Tk_ConfigSpec configSpecs_[] = {
    RTD_OPTIONS,		// See RtdImage.h: defines rtd option list
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};


/*
 * Initialize the image control structure with pointers to the handler
 * functions
 */
static Tk_ImageType rtdImageType = {
    "rtdimage",                 /* name */
    RtdImage::CreateImage,      /* createProc */
    TkImage::GetImage,          /* getProc */
    TkImage::DisplayImage,      /* displayProc */
    TkImage::FreeImage,         /* freeProc */
    TkImage::DeleteImage,       /* deleteProc */

#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
    // XXX RtdImage::Postscript,        /* postscriptProc */
    (Tk_ImagePostscriptProc *) NULL,    /* postscriptProc */
#endif

    (Tk_ImageType *) NULL       /* nextPtr */
};

/*
 * eval the pre- and post- camera commands and display the image event.
 * "info" contains all we need to know about the image and "data"
 * gives access to the shared memory for the image.
 */
int RtdImageCamera::display(const rtdIMAGE_INFO& info, const Mem& data)
{
    int status = TCL_OK;
    char buf[2048];

    RtdPerf *rtdperf = rtdimage_->rtdperf();

    rtdimage_->imageEvent(1);    // set image event state
    rtdperf->newCycle();  // start performance test if applicable

    if (rtdimage_->cameraPreCmd()) {
	sprintf(buf, "%s %d", rtdimage_->cameraPreCmd(), info.frameId);
	status |= Tcl_Eval(interp_, buf);
	rtdperf->TCLtime();
    }

    rtdperf->GENtime();
    status |= rtdimage_->displayImageEvent(info, data);
    rtdperf->GENtime();
 
    if (rtdimage_->cameraPostCmd()) {
	sprintf(buf, "%s %d", rtdimage_->cameraPostCmd(), info.frameId);
	status |= Tcl_Eval(interp_, buf);
	rtdperf->TCLtime();
    }
    rtdperf->endCycle(); // set the performance test variables
    rtdimage_->imageEvent(0);    // unset image event state

    return status;
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
    Tcl_CreateCommand(interp, "rtd_load_cmap", (Tcl_CmdProc*)rtd_load_cmap, NULL, NULL);
    Tcl_CreateCommand(interp, "rtd_load_itt", (Tcl_CmdProc*)rtd_load_itt, NULL, NULL);
    return TCL_OK;
}

/*
 * A call to this function is made from the tkAppInit file at startup
 * to install the RtdImage image type and do global initialization
 */
extern "C"
int Rtd_Init(Tcl_Interp* interp)  
{
    // Initialize the local packages that rtd depends on

    // initialize the tclutil package 
    if (Tclutil_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    // initialize the astrotcl package 
    if (Astrotcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    char buf[1024];
#ifndef PANEL_EDITOR_BUG
    // initialize color management (once only per application)
    if (RtdImage::initColors(interp) != TCL_OK)
	return TCL_ERROR;
#endif

    // initialize bias image object
    if (RtdImage::initBias() != TCL_OK)
	return TCL_ERROR;

    // initialize the peformance test object
    if (RtdImage::initPerf(interp) != TCL_OK)
	return TCL_ERROR;

    // set up Rtd Tcl package
    if (Tcl_PkgProvide (interp, "Rtd", PACKAGE_VERSION) != TCL_OK) {
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
    Tcl_CreateCommand(interp, "rtd_set_cmap", (Tcl_CmdProc*)RtdImage::rtd_set_cmap, NULL, NULL);
    
    // clean up shared memory on exit
    signal(SIGINT, RtdImage_cleanup);
    signal(SIGTERM, RtdImage_cleanup);
    // ignore floating point exceptions
    //sigignore(SIGFPE);
    signal(SIGFPE, SIG_IGN);  // 11.9.97: allan, need this for linux
    Tk_CreateEventHandler(Tk_MainWindow(interp), DestroyNotify, destroy_notify, NULL);

    // initialize the rtdrecorder and rtdplayback image types
    RtdrecordInit(interp);

    Tcl_SetVar(interp, "rtd_version", PACKAGE_VERSION, TCL_GLOBAL_ONLY);
    return Tcl_Eval(interp, initScript);
}

/*
 * This static method is called by the Tk image code to create
 * a new rtdimage image.
 *
 * Note: There was an incompatible change made in tk8.3: argv was
 * replaced with objv. Therefore the #ifdefs.
 */
int RtdImage::CreateImage(
    Tcl_Interp *interp,         // Interpreter for application containing image.
    char *name,                 // Name to use for image.
    int argc,                   // Number of arguments.
#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
    Tcl_Obj *CONST objv[],      // Argument objects for options (not including image name or type)
#else
    char **argv,                // Argument strings for options (not including image name or type)
#endif
    Tk_ImageType *typePtr,      // Pointer to our type record (not used). 
    Tk_ImageMaster master,      // Token for image, to be used by us in later callbacks.
    ClientData *clientDataPtr)  // Store manager's token (this ptr) for image here
                                // it will be returned in later callbacks.
{
    
#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
    // just generate an argv from the objv argument
    char* argv[64];  // there shouldn't be more than a few options...
    for(int i = 0; i < argc; i++)
        argv[i] = Tcl_GetString(objv[i]);
    argv[argc] = NULL;
#endif

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
	      (specs ? specs : configSpecs_), options, 
	      master, "Canvas"),
      image_((ImageData*)NULL),
      options_(options),
      camera_(NULL),
      cameraPreCmd_(NULL),
      cameraPostCmd_(NULL),
      imageEvent_(0),
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
      dbl_(NULL)
{
    if (! options_) {
        options_ = new RtdImageOptions;
    }
    // since gcc 3.2 is complaining about non-POD we use a structure
    // which holds the options.
    setOptionsPtr(options_->get_rtd_options());

    // errors may have occured in the base class ... 
    if (status() != TCL_OK)
	return;
    filename_[0] = '\0';

    // debug log object
    dbl_ = new RtdDebugLog((char *)instname, (int)(debug() & verbose()));

    // if we are on the console, check for XSHM, X shared memory extension
    char hostname[64];
    gethostname(hostname, sizeof(hostname));
    int n = strlen(hostname);
    char* display = DisplayString(display_);

    // allan: 7/01: check for screen number "0", since when using X11 forwarding with ssh,
    // the display is set to something like "host:10:0" - a proxy server, and the X shared
    // memory is never freed.
    if (display[0] == ':' || 
        (strncmp(hostname, display, n) == 0 && display[n] == ':' && display[n+1] == '0')) {
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

    if (haveXShm_)
	dbl_->log("X Shared memory is supported\n");
    else
	dbl_->log("X Shared memory is not supported\n");

    if (haveXSync_)
	dbl_->log("X Synchronisation is supported\n");
    else
	dbl_->log("X Synchronisation is not supported\n");

  // initialize list of views
    for(int i = 0; i < MAX_VIEWS; i++)
	view_[i] = NULL;

#ifdef PANEL_EDITOR_BUG
    // initialize color management (once only per application)
    if (RtdImage::initColors(interp) != TCL_OK) {
	status_ = TCL_ERROR;
	return;
    }
#endif

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
    if (dbl_) {
	dbl_->log("~RtdImage(): deleting %s (%s)\n", instname(), name());
	delete dbl_;
	dbl_ = NULL;
    }

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
	delete[] pixTab_;
	pixTab_ = NULL;
    }

    // remove any views of this image
    removeViews();
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
    //  some drawbacks (the tkwin was previously the parent of the image?).
    Visual* visual = Tk_GetVisual(interp, tkwin, ".", &depth, &colormap);
    if (! visual)
	return TCL_ERROR;
    
    Tk_MakeWindowExist(tkwin);
    colors_ = new ImageColor(Tk_Display(tkwin), visual, depth, max_colors);
    if (colors_->status() != 0) {
	return TCL_ERROR;
    }
    if (colors_->colorCount() < min_colors) {
	if (colors_->usePrivateCmap() || colors_->allocate(max_colors)) {
	    return TCL_ERROR;
	}
    }

    //  PWD: always set the colormap. This can be "private" when the
    //  default visual doesn't match the one requested, in which case
    //  we still need to set the colormap (I noticed this using a
    //  default truecolor visual when attempting to get a pseudocolor
    //  visual).
    return colors_->setColormap(tkwin);
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
 * This method is called from the RtdImageCamera class to display the
 * image from shared memory.  "info" contains all we need to know about
 * the image and "data" gives access to the shared memory for the image.
 */
int RtdImage::displayImageEvent(const rtdIMAGE_INFO& info, const Mem& data) 
{
    // if this event is for us, display it
    if (info.frameId == frameId_) {
	int status = TCL_ERROR;

	// update the x and y offsets 
	// (these are normally 0, when the rapid frame has its own shared memory area)
	xOffset_ = info.frameX;
	yOffset_ = info.frameY;

	// use native byte ordering for data in shared memory, unless
	// the camera process wants RTD to do the byte swap (eg. RTD
	// running on a little Endian machine and data are produced
	// on a big Endian machine).

	int usingNetBO = ((BIGENDIAN && info.shmEndian == -1) || (info.shmEndian == 0));
	
	// if the image changed, delete it and create a new one, otherwise reuse
	if (image_ == NULL 
            || image_->data().shared() == 0
	    || info.xPixels != image_->width() 
	    || info.yPixels != image_->height() 
	    || usingNetBO   != image_->image().usingNetBO() 
	    || info.dataType != image_->dataType()) {
	    
	    if (dbl_)
		dbl_->log("%s: new image received: %d x %d\n", 
			  name(), info.xPixels, info.yPixels);

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

	    fits->usingNetBO(usingNetBO);

	    image_ = makeImage(fits);
	    if (!image_)
		return TCL_ERROR;

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
	}
	else {
	    // reuse the current image and just update the image data
	    if (dbl_)
		dbl_->log("%s: new image data received: %d x %d (size: %d bytes)\n", 
			  name(), info.xPixels, info.yPixels, data.length());

	    // if the image event contains valid cut levels, use them
	    if (info.lowCut != info.highCut) 
		setCutLevels(info.lowCut, info.highCut, 1, 0);

	    // update the image with the new data
	    status = updateImageNewData(data);
	} 
	
	// now we have the new image...

	// set detector parameters
	setDetParms(image_, info);
	for(int i = 0; i < MAX_VIEWS; i++) {
	    if (view_[i] && view_[i]->image_ && !view_[i]->isSeparateRapidFrame()) 
		setDetParms(view_[i]->image_, info);
	}
	filename(image_->object());

	// set WCS info, if available
	int binningOk = 1;
	if (info.binningX > 0 || info.binningY > 0) {
	    if (info.binningX != info.binningY)
		binningOk = 0;
	}
	if (info.secpix != 0. && binningOk == 1) {
	    double xrefpix=info.xrefpix, yrefpix=info.yrefpix;
	    double asecpix=info.secpix;

	    if (info.binningX > 1)
		asecpix = asecpix / info.binningX;
	    image_->chipToImageCoords(xrefpix, yrefpix);

	    if (image_->wcs().set(info.ra, info.dec, 
				  asecpix, xrefpix, yrefpix,
				  info.xPixels, info.yPixels, info.rotate, 
				  info.equinox, info.epoch, info.proj) != 0)
		return TCL_ERROR;
	    if (info.wcsFlags & (RTD_WCS_FLIP_RA | RTD_WCS_FLIP_DEC)) {
		double cdelt1 = - info.secpix / 3600.0;
		double cdelt2 =   info.secpix / 3600.0;
		if (info.wcsFlags & RTD_WCS_FLIP_RA)
		    cdelt1 = -cdelt1;
		if (info.wcsFlags & RTD_WCS_FLIP_DEC)
		    cdelt2 = -cdelt2;
		image_->wcs().deltset(cdelt1, cdelt2, info.rotate);
	    }
	}

	// update zoom window and panel labels, etc, if necessary
	if (motionView_)
	    motionView_->processMotionEvent();

	// pause here until image is displayed
	updateRequests();

	// notify the panning window after WCS was set
	if (panCommand_)
	    autoPan(1);

	return status;
    }
    else {
	// must be a rapid frame event. Pass it on...
	int i = info.frameId-1;
	if (i >= 0 && i < MAX_VIEWS && view_[i] && view_[i]->rapidFrame_) {
	    return view_[i]->displayImageEvent(info, data);
	}
	// frameId is wrong: pass the event on to the first rapid frame
	for (i = 1; i < MAX_VIEWS; i++) {
	    if (view_[i] && view_[i]->rapidFrame_) {
		rtdIMAGE_INFO infoRapid;
		memcpy(&infoRapid, &info, sizeof(rtdIMAGE_INFO));
		infoRapid.frameId = i+1;
		return view_[i]->displayImageEvent(infoRapid, data);
	    }
	}
    }
    return TCL_OK;
}

/*
 * Set detector paramters
 */
void RtdImage::setDetParms(ImageData* image, const rtdIMAGE_INFO& info)
{
    image->startX(info.startX);
    image->startY(info.startY);
    if (info.binningX > 0)
	image->binX(info.binningX);
    if (info.binningY > 0)
	image->binY(info.binningY);
}

/*
 * call updateIdleTasks() if no image event from camera was received.
 * Otherwise update window and idle events to a) avoid too many
 * pending X requests and b) set the times for the performance test
 * 
 */
void RtdImage::updateRequests()
{
    if (! imageEvent_ ) {
	updateIdleTasks();  // backwards compatible for loaded images
	return;
    }

    if (! dbl_ || ! rtdperf_ || ! xImage_)
	return;

    rtdperf_->GENtime();  // update the time stamp
    xImage_->flushX();
    rtdperf_->Xtime();
    updateIdleTasks();
}


// Fix for Tk clipping coordinates to short range: See CanvasWindowCoordsNoClip() below.
#ifdef HAVE_TKCANVAS_H
#include "tkCanvas.h"
#else
// The structure we need hasn't changed for a long time, so just include a local copy.
#include "tkCanvas.h-tk8.4.11"
#define HAVE_TKCANVAS_H 
#endif

/*
 * Hack to work around the tk canvas clipping coordinates to short range: 
 * same as Tk_CanvasWindowCoords, but with no clipping.
 * Without this fix, there will be problems with very large, or zoomed in images.
 */
static void
Tk_CanvasWindowCoordsNoClip(Tk_Canvas canvas, double x, double y, int* screenXPtr, int* screenYPtr)
{
#ifdef HAVE_TKCANVAS_H 
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    double tmp;

    tmp = x - canvasPtr->xOrigin;
    if (tmp > 0) {
	tmp += 0.5;
    } else {
	tmp -= 0.5;
    }
    *screenXPtr = (int)tmp;

    tmp = y  - canvasPtr->yOrigin;
    if (tmp > 0) {
	tmp += 0.5;
    } else {
	tmp -= 0.5;
    }
    *screenYPtr = (int)tmp;
#else 
    short sx, sy;
    Tk_CanvasWindowCoords(canvas, x, y, &sx, &sy);
    if (sx == 32767 || sx == -32768 || sy == 32767 || sy == -32768) {
        fprintf(stderr, "Warrning: Tk clipped the RTD image coordinates to short range!\n");
    }
    *screenXPtr = sx;
    *screenYPtr = sy;
#endif
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

    rtdperf_->TCLtime();

    if (displayLocked_ || ! initialized_ || ! xImage_ || ! xImage_->data() || ! image_)
	return;
    displayLocked_ = 1;

    // get the canvas X,Y offsets
    // Note: the Tk routine Tk_CanvasWindowCoords clips the coordinates to "short" range
    // (-32768 .. 32767), which can cause problems in very large images
    // and/or at large magnification. The call below is to a replacement defined here.
    Tk_CanvasWindowCoordsNoClip(canvas_, 0., 0., &canvasX_, &canvasY_);
    
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
    
    // notify the panning window, if necessary
    if (panCommand_)
	autoPan();

    // reset flags
    update_pending_ = displayLocked_ = 0;

    rtdperf_->Xtime();
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
		    /*
		     * XSyncSetPriority() was commented out since it can block all
		     * other X-applications when fast image events are received-
		     */
		    // XSyncSetPriority(display_, None, 65535);
		    // fprintf(stderr, "Raising priority of client %s\n", name());
		}
		break;
#endif  // _HAVE_R6
		
	    case RTD_OPTION(displaymode):
	    case RTD_OPTION(shm_header):
	    case RTD_OPTION(shm_data):
		if (initialized_) 
		    reset++;
		break;
	    case RTD_OPTION(verbose):
	    case RTD_OPTION(debug):
		if (dbl_)
		    dbl_->setlog ((int)(debug() & verbose()));
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

	    case RTD_OPTION(fillWidth):
	    case RTD_OPTION(fillHeight):
		if (initialized_) {
		    if (image_ && fillWidth() && fillHeight()) {
			image_->fillToFit(fillWidth(), fillHeight());
		    }
		    reset++;
		}
		break;

	    case RTD_OPTION(file):
		status = loadFile();
		break;
		
	    case RTD_OPTION(sampmethod):
		if (initialized_ && image_) {
		    if (image_->sampmethod() != sampmethod()) {
			image_->sampmethod(sampmethod());
			reset++;
		    }
		}
		break;

	    case RTD_OPTION(subsample):
		if (initialized_ && image_) {
		    if (image_->subsample() != subsample()) {
			image_->subsample(subsample());
			reset++;
		    }
		}
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
    LookupTable lookup = image_->lookupTable();
    for(int i = 0; i < MAX_VIEWS; i++) {
	if (view_[i] && view_[i]->image_ && !view_[i]->isSeparateRapidFrame()) 
	    view_[i]->image_->lookupTable(lookup);
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
	if (dbl_)
	    dbl_->log("%s: setting scale to (%d, %d), factor %d\n",
		      name(), xScale, yScale, zoomFactor_);
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

    // if sysV shared memory was requested, use it in place of mmap
    // XXX 18.06.04 shared memory is only used for image event data
    FitsIO* fits = NULL;
    if (0 && (shm_header() || shm_data())) {
	// read the FITS image into shared memory
	Mem m(file());
	if (m.status() != 0)
	    return TCL_ERROR;
	m.shared(1);
	fits = FitsIO::initialize(m);
    }
    else {
	// read the FITS image using the default mmap. S_IRUSR is used to cheat
	// FitsIO.C which assumes that no options are given when 0
	// but O_RDONLY is defined as 0.
	fits = FitsIO::read(file(), O_RDONLY | S_IRUSR);
    }

    if (!fits || fits->status() != 0)
	return TCL_ERROR;
    image_ = makeImage(fits);
    if (! image_)
	return TCL_ERROR;

    // restore transformations
    image_->restoreParams(p, !autoSetCutLevels_);
    filename(file());  // keep filename

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
    image_->sampmethod(sampmethod());
    image_->verbose(verbose());

    if (fitWidth() || fitHeight()) {
	image_->shrinkToFit(fitWidth(), fitHeight());
    }

    if (fillWidth() || fillHeight()) {
	image_->fillToFit(fillWidth(), fillHeight());
    }

    image_->colorScale(colors_->colorCount(), colors_->pixelval());

    // update other views
    if (updateViews(1) != TCL_OK)
	return TCL_ERROR;

    if (resetImage() != TCL_OK)
	return TCL_ERROR;

    // update the panning window if needed
    if (panCommand_) {
	if (Tk_Width(tkwin_) <= 1) {
	    // pause here until window is displayed, so that the pan
	    // window can determine the size of the window
	    updateRequests();
	}
	autoPan(1);
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
    if (dbl_)
	dbl_->log("%s: update image with new data (size: %d)\n", name(), data.length());
    
    if (image_)
	image_->data(data);

    // update the data in any views
    for(int i = 0; i < MAX_VIEWS; i++) {
	if (view_[i] && view_[i]->image_ && !view_[i]->rapidFrame_ 
	    && view_[i] != zoomView_ && view_[i] != zoomView2_) {

	    if (dbl_)
		dbl_->log("%s: update %s with new data\n", name(), view_[i]->name());
	    view_[i]->image_->data(data);
	}
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
	// only transformations changed
	if (flag == 2) {
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

    if (dbl_)
	dbl_->log("%s: update view from %s (rapid?: %d)\n", name(), im->name(), rapidFrame_);

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
	image_->sampmethod(sampmethod());
	image_->subsample(subsample());
    }

    if (fitWidth() || fitHeight()) { 
	// used for pan window - shrink to fit whole image in window
	image_->shrinkToFit(fitWidth(), fitHeight());
    }
    else if (zoomFactor_ > 1) {
	// used for zoom view: make scale relative to main image
	if (setScale(im->xScale(), im->yScale()) != TCL_OK) 
	    return TCL_ERROR;
    }
    else if (xs && propagateScale_ == 0) {
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
#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
    canvas_ = (Tk_Canvas)info.objClientData;
#else
    canvas_ = (Tk_Canvas)info.clientData;
#endif

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
    if ((eventPtr->xmotion.state & ( ShiftMask | LockMask ) )  == 0) {

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
	if (dbl_)
	    dbl_->log("configureNotify: %d, %d\n", 
		      eventPtr->xconfigure.width, eventPtr->xconfigure.height);
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
    if (image_ && xImage_ && xImage_->data() ) {
	double x = motionX_, y = motionY_;

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
            int maxx = 0, maxy = 0, minx = 0, miny = 0;
 	    image_->getValues(x, y, rx, ry, pixTab_, pixTabRows_, pixTabCols_);
	    for(int j = 0; j <= pixTabRows_; j++) {
		for(int i = 0; i <= pixTabCols_; i++) {
		    sprintf(indexStr, "%d,%d", j, i);
		    // not outside image ?
		    if ((d = pixTab_[j*w+i]) > -HUGE_VAL) {
			if (i && j) {
			    sprintf(valueStr, "%g", d);	  // pix value
			    // calculate statistics on pixels
			    if (npix == 0) {
				minv = d;
				maxv = d;
                                maxx = minx = j;
                                maxy = miny = i;
			    }
			    npix++;
			    sum += d;
			    sumsq += d * d;
			    if (d < minv) {
				minv = d;
                                minx = j;
                                miny = i;
                            }
			    if (d > maxv) {
				maxv = d;
                                maxx = j;
                                maxy = i;
                            }
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

		sprintf(valueStr, "%d", maxx);	  // maxx
		Tcl_SetVar2(interp_, var, "PIXTAB_MAXX", valueStr, TCL_GLOBAL_ONLY);
		sprintf(valueStr, "%d", maxy);	  // maxy
		Tcl_SetVar2(interp_, var, "PIXTAB_MAXY", valueStr, TCL_GLOBAL_ONLY);

		sprintf(valueStr, "%d", minx);	  // minx
		Tcl_SetVar2(interp_, var, "PIXTAB_MINX", valueStr, TCL_GLOBAL_ONLY);
		sprintf(valueStr, "%d", miny);	  // miny
		Tcl_SetVar2(interp_, var, "PIXTAB_MINY", valueStr, TCL_GLOBAL_ONLY);
	    }
	    else {
		Tcl_SetVar2(interp_, var, "PIXTAB_AVE", "\0", TCL_GLOBAL_ONLY);
		Tcl_SetVar2(interp_, var, "PIXTAB_MIN", "\0", TCL_GLOBAL_ONLY);
		Tcl_SetVar2(interp_, var, "PIXTAB_MAX", "\0", TCL_GLOBAL_ONLY);
		Tcl_SetVar2(interp_, var, "PIXTAB_N", "\0", TCL_GLOBAL_ONLY);
		Tcl_SetVar2(interp_, var, "PIXTAB_MAXX", "\0", TCL_GLOBAL_ONLY);
		Tcl_SetVar2(interp_, var, "PIXTAB_MAXY", "\0", TCL_GLOBAL_ONLY);
		Tcl_SetVar2(interp_, var, "PIXTAB_MINX", "\0", TCL_GLOBAL_ONLY);
		Tcl_SetVar2(interp_, var, "PIXTAB_MINY", "\0", TCL_GLOBAL_ONLY);
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
    return ImageData::makeImage(name(), imio, biasimage_->biasInfo(), verbose());
}

/*
 * Return true if no image is loaded.
 * 
 * PWD: don't use image size (of 2x2), use OBJECT "RTD_BLANK" value instead.
 */
int RtdImage::isclear()
{
    if ( image_ ) {
        const char *object = image_->object();
        if ( ! object || object && strcmp( "RTD_BLANK", object ) != 0  ) {
            return 0;
        }
    }
    return 1;
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
	    LookupTable lookup = image_->lookupTable();
	    for(int i = 0; i < MAX_VIEWS; i++) {
		if (view_[i] && view_[i]->image_ && !view_[i]->isSeparateRapidFrame())
		    view_[i]->image_->lookupTable(lookup);
	    }
	}
	return updateViews(1) || updateImage();
    }
    return TCL_OK;
}

/*
 * initialize the bias image object
 */
int RtdImage::initBias()
{
    // we should do this only once
    if (biasimage_)
	return TCL_OK;

  // create bias image object
    RtdImage::biasimage_ = new BiasData();
    return TCL_OK;
}

/*
 * initialize the performance object
 */
int RtdImage::initPerf(Tcl_Interp* interp)
{
    // we should do this only once
    if (rtdperf_)
	return TCL_OK;

  // create performance object
    RtdImage::rtdperf_ = new RtdPerf(interp);
    return TCL_OK;
}

// RtdImage signal handler

void RtdImage_cleanup(int sig) 
{
    Mem_RPTcleanup();    // cleanup shm of Rtd Recorder/Playback tool
    Mem_cleanup(sig);    // cleanup shm of RtdImage and exit
}
