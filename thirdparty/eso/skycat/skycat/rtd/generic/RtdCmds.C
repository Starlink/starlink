/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: RtdCmds.C,v 1.1.1.1 2006/01/12 16:39:12 abrighto Exp $"
*
* who          when      what
* --------     --------  ----------------------------------------------
* A. Brighton  05/10/95  Created in RtdImage.C
* pbiereic     01/03/01  copied from RtdImage.C
* pbiereic     01/04/01  bltgraph subcommand
* pbiereic     27/06/01  added "statistics noise" subcommand
* pbiereic     14/08/01  added "hdu fits" subcommand
* pbiereic     17/02/03  "autocut" uses autoSetCutLevels(99.5) instead of
*                        medianFilter(). This gives much better results
*                        and is also faster.
* pbiereic     19/03/03  The previous change was undone (on request)
* Peter W. Draper 14/11/05 Merge my RtdImage changes.
* Peter W. Draper 26/10/06 Change mbandCmd so that clipping happens in 
*                          centre of last pixel, not previous pixel.
*                 08/01/07 Mark blank images with an OBJECT keyword
*                          with value RTD_BLANK. Do not use image size.
*                 18/01/07 Back to mband, clip that at 0.5->width/height+0.5
*                          so that measurement happens to the edge.
*                 04/06/07 Increase upper limit of zoom factor (supports
*                          adaptive zoom in GAIA for very small, one pixel,
*                          images).
*                 09/07/07 Reset zoomFactor_ when stopping a zoomed view.
*                          Will be re-used in reference counted copies
*                          (inappropriately when extra high factors are in
*                          use, leading to memory allocation errors).
*/

/************************************************************************
*   NAME
*   
*   RtdCmds.C - rtd subimage commands
* 
*   SYNOPSIS
*   
* 
*   DESCRIPTION
*
*   This file contains all RtdImage member functions which perform
*   an image subcommand. These functions were originally contained
*   in RtdImage.C and moved to this file since RtdImage.C were becoming
*   much too big.
*
*   FILES
*
*   ENVIRONMENT
*
*   CAUTIONS 
*
*   SEE ALSO
*    RtdImage(3), RTD documentation
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

static char *rcsId="@(#) $Id: RtdCmds.C,v 1.1.1.1 2006/01/12 16:39:12 abrighto Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "RtdImage.h"
#include "define.h"   // for max function
#include <sstream>

/* 
 * declare a table of image subcommands and the methods that handle them.
 *
 * NOTE: keep this table sorted, so we can use a binary search on it !
 * (select lines in emacs and use M-x sort-lines)
 */
static class RtdImageSubCmds
{
public:
    char* name;      // method name
    int (RtdImage::*fptr)(int argc, char* argv[]); // ptr to method
    int min_args;    // minimum number of args
    int max_args;    // maximum number of args
} subcmds_[] = { 
    {"alloccolors", &RtdImage::alloccolorsCmd,  0,  1},
    {"autocut",     &RtdImage::autocutCmd,      0,  2},
    {"biasimage",   &RtdImage::biasimageCmd,    0,  3},
    {"bitpix",      &RtdImage::bitpixCmd,       0,  0},
    {"bltgraph",    &RtdImage::bltgraphCmd,     6,  10},
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
    {"flip",        &RtdImage::flipCmd,         0,  2},
    {"frameid",     &RtdImage::frameidCmd,      0,  0},
    {"get",         &RtdImage::getCmd,          3,  5},
    {"graphdist",   &RtdImage::graphdistCmd,    5,  5},
    {"hdu",         &RtdImage::hduCmd,          0,  6},
    {"height",      &RtdImage::heightCmd,       0,  0},
    {"info",        &RtdImage::infoCmd,         0,  6},
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
    {"statistics",  &RtdImage::statisticsCmd,   0,  5},
    {"type",        &RtdImage::typeCmd,         0,  0},
    {"update",      &RtdImage::updateCmd,       0,  1},
    {"userfreq",    &RtdImage::maxFreqCmd,      1,  1},
    {"view",        &RtdImage::viewCmd,         2,  11},
    {"warp",        &RtdImage::warpCmd,         2,  2},
    {"wcscenter",   &RtdImage::wcscenterCmd,    0,  2},
    {"wcsdeltset",  &RtdImage::wcsdeltsetCmd,   0,  4},
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
 * Call the given method in this class with the given arguments
 * If the method is not defined here, pass on the search to the
 * parent class. Since this is a virtual function, the search starts
 * in the most specific class.
 */
int RtdImage::call(const char* name, int len, int argc, char* argv[])
{
    if (dbl_)
	if (dbl_->setlog()) {
	    char buf[4096];
	    *buf = '\0';
	    int l = 0;
	    for(int i = 0; i < argc; i++) {
		l += strlen(argv[i]);
		if (l < sizeof(buf) + 2) {
		    strcat(buf, argv[i]);
		    strcat(buf, " ");
		}
	    }
	    if (dbl_)
		dbl_->log("subcommand: %s %s\n", name, buf);
	}

    // since this tcl command has a lot of subcommands, 
    // we do a binary search on the method table
    int low = 0;
    int high = sizeof(subcmds_)/sizeof(*subcmds_) - 1;
    int mid;
    int cond;

    while (low <= high) {
	mid = (low + high) / 2;
	if ((cond = strcmp(name, subcmds_[mid].name)) < 0) 
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

// -- image subcommands --


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
	//  image_->autoSetCutLevels(99.5);
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
    LookupTable lookup = image_->lookupTable();
    for(int i = 0; i < MAX_VIEWS; i++) {
	if (view_[i] && view_[i]->image_ && !view_[i]->isSeparateRapidFrame()) 
	    view_[i]->image_->lookupTable(lookup);
    }

    // keep status in a global Tcl variable which can be traced by the Tcl code
    char* var = (viewMaster_ ? viewMaster_->instname_ : instname_);
    char sts[100];
    sprintf(sts, "%g %g",  image_->lowCut(), image_->highCut());
    Tcl_SetVar2(interp_, var, "CUT", sts, TCL_GLOBAL_ONLY);

    return updateViews(1) || updateImage(); 
}


/*
 * biasimage subcommand
 *
 * usage:
 *    $image biasimage <command>
 *
 * The available commands are
 *      copy $nr           - copy current image to bias image "nr"
 *      file $filename $nr - load bias image "nr" from file "filename"
 *      file $nr           - return filename of bias image "nr"
 *      clear $nr          - clear bias image "nr"
 *      clear all          - clear all bias images
 *      on                 - switch bias subtraction on
 *      off                - switch bias subtraction off
 *      status             - return status (-1 no bias image loaded, 0 = off, 1 = on)
 *      update             - update the global Tcl status variable
 *      display            - display the selected bias image
 *      select $nr         - select bias image "nr"
 *      maxbias            - return max. number of bias images
 */
int RtdImage::biasimageCmd(int argc, char* argv[])
{
    char* usage = 
	"usage: $image biasimage copy|clear|on|off|status|select|display|file|update|maxbias ?filename? ?nr?";
    int nr;
    char buf[1024];
    FitsIO *fits = NULL;

    // bias images for images with extensions are not supported
    if (image_) {
	fits = (FitsIO*)image_->image().rep();
	if (fits->getNumHDUs() > 1) {
	    biasimage_->off();
	    for (int i=0; i<MAXBIAS; i++)
		biasimage_->clear(i);
	}
    }

    if (argc < 1)
	return error(usage);
    else if (strcmp(argv[0], "update") == 0) {
    }
    else if (strcmp(argv[0], "status") == 0) {
	sprintf(buf, "%d", biasimage_->status());
	return set_result(buf);
    }
    else if (strcmp(argv[0], "off") == 0)
	biasimage_->off();
    else if (strcmp(argv[0], "maxbias") == 0) {
	sprintf(buf, "%d", MAXBIAS);
	return set_result(buf);
    }
    else if (strcmp(argv[0], "clear") == 0) {
	if (Tcl_GetInt(interp_, argv[1], &nr) != TCL_OK) {
	    for (int i=0; i<MAXBIAS; i++)  // clear all
		biasimage_->clear(i);
	}
	else
	    biasimage_->clear(nr);
    }
    else if (fits && fits->getNumHDUs() > 1) {
	//return error("Bias image not supported for images with extensions");
    }
    else if (strcmp(argv[0], "on") == 0) {
	if (biasimage_->on() != 0)
	    return TCL_ERROR;
    }
    else if (strcmp(argv[0], "select") == 0) {
	if (argc < 2) {
	    sprintf(buf, "%d", biasimage_->select());
	    return set_result(buf);
	}
	if (Tcl_GetInt(interp_, argv[1], &nr) != TCL_OK)
	    return error("usage: $image biasimage select nr");
	if (biasimage_->select(nr) != 0) {
	    sprintf(buf, "biasimage select: number must be in range 0-%d", MAXBIAS-1);
	    return error(buf);
	}
    }
    else if (strcmp(argv[0], "display") == 0) {
	if (!biasimage_->image())
	    return error("selected bias image is not loaded");
	ImageDataParams p;
	if (image_) {
	    image_->saveParams(p);
	    delete image_;
	    image_ = NULL;
	    updateViews();
	}
	image_ = biasimage_->image()->copy();
	filename(biasimage_->file(biasimage_->select()));
	image_->restoreParams(p, 1);
	return initNewImage();
    }
    else if (strcmp(argv[0], "copy") == 0) {
	if (isclear())
	    return error("no image loaded");
	if (Tcl_GetInt(interp_, argv[1], &nr) != TCL_OK)
	    return error("usage: $image biasimage copy nr");
	if (biasimage_->copy(image_, filename(), nr) != 0)
	    return TCL_ERROR;
    }
    else if (strcmp(argv[0], "file") == 0) {
	if (argc == 2) {
	    if (Tcl_GetInt(interp_, argv[1], &nr) != TCL_OK)
		return error("usage: $image biasimage file nr");
	    strcpy (buf, biasimage_->file(nr));
	    return set_result(buf);
	}
	if (argc != 3 || Tcl_GetInt(interp_, argv[2], &nr) != TCL_OK)
	    return error("usage: $image biasimage file filename nr");
	if (biasimage_->file(argv[1], nr) != 0)
	    return TCL_ERROR;
    }
    else
	return error(usage);

    // keep status in a global Tcl variable which can be traced by the Tcl code
    char* var = (viewMaster_ ? viewMaster_->instname_ : instname_);
    char sts[10];
    sprintf(sts, "%d", biasimage_->status());
    Tcl_SetVar2(interp_, var, "BIAS", sts, TCL_GLOBAL_ONLY);

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
 * This method implements the "camera" subcommand
 *
 * usage: camera attach ?cameraName? ?preCommand? ?postCommand?
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
    int stat = TCL_OK;
    char buf[128];
    if (! camera_)
	camera_ = new RtdImageCamera(this);

    if (strcmp(argv[0],"pause") == 0) 
	stat = camera_->pause();
    else if  (strcmp(argv[0],"continue") == 0) 
	stat = camera_->cont();
    else if (strcmp(argv[0],"attach") == 0 || strcmp(argv[0],"start") == 0) {
	if (argc < 2) {
	    sprintf(buf, "%d", camera_->attached());
	    stat = set_result(buf);
	}
	else{
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
	    stat = camera_->start(argv[1]);
	}
    }
    else if (strcmp(argv[0],"detach") == 0 || strcmp(argv[0],"stop") == 0) {
	stat = camera_->stop();
    }
    else {
	return error("invalid camera subcommand: expected: start, stop, pause or continue");	
    }
    camera_->updateGlobals();
    return stat;
}



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
    for (int i = 0; i < argc; i+=2) 
    {
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
    filename("");
    
    // generate the blank image
    FitsIO* fits = FitsIO::blankImage(ra, dec, equinox, radius, width, height,
				      colors_->pixelval(0));
    if (fits) {
	image_ = makeImage(fits);

        // mark a blank image so we can identify it.
        if ( width == 2 && height == 2 ) {
            image_->object( "RTD_BLANK" );
        }
    }

    //  restore transformations, cut levels, etc from the previous image
    if (image_)
	image_->restoreParams(p, !autoSetCutLevels_);

    return initNewImage();
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
 *      <path> cmap isreadonly
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
 *
 * "isreadonly" returns true if the colormap is readonly.
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
	    }
	    else {
		ret = colors_->rotateColorMap(amount);
	    }
	}
	if (strcmp(argv[0], "shift") == 0) {
	    int amount;
	    if (Tcl_GetInt(interp_, argv[1], &amount) != TCL_OK) {
		ret = TCL_ERROR;
	    } 
	    else {
		ret = colors_->shiftColorMap(amount);
	    }
	}
	if (strcmp(argv[0], "set") == 0) {
	    Tk_Window w = Tk_NameToWindow(interp_, argv[1], tkwin_);
	    if (w == NULL) {
		ret = TCL_ERROR;
	    } 
	    else {
		ret = colors_->setColormap(w);
	    }
	}
	//  Force image update if colour changes do not transfer
	//  automatically (i.e. non-pseudocolor visual).
	if ( ret == TCL_OK && colors_->readOnly() ) {
	    return colorUpdate();
	} 
	else {
	    return ret;
	}
    }
  
    if (strcmp(argv[0], "file") == 0) {
	return set_result(colors_->cmap()->name());
    }
    if (strcmp(argv[0], "reset") == 0) {
	ret = colors_->reset();
	if ( ret == TCL_OK ) {
	    return colorUpdate();
	} 
	else {
	    return ret;
	}
    }
    if (strcmp(argv[0], "pixels") == 0) {
	int n = colors_->colorCount();
	unsigned long* p = colors_->pixelval();
	ostringstream os;
	for (int i = 0; i < n; i++) 
	    os << *p++ << " ";
	return set_result(os.str().c_str());
    }
    if (strcmp(argv[0], "list") == 0) {
	ostringstream os;
	ColorMapInfo::list(os);
	set_result(os.str().c_str());
	return TCL_OK;
    }
    if (strcmp(argv[0], "private") == 0) {
	return colors_->usePrivateCmap();
    }
    if (strcmp(argv[0], "isprivate") == 0) {
	return set_result(colors_->usingPrivateCmap());
    }
    if (strcmp(argv[0], "isreadonly") == 0) {
	return set_result(colors_->readOnly());
    }
  
    return error("unknown rtdimage cmap subcommand");
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
	memmove(p+(j*w), p, w);
    }
    if (image_)
	delete image_;
    FitsIO* fits = new FitsIO(w, h, BYTE_IMAGE, 0.0, 1.0, header, data);
    if (fits) {
	image_ = makeImage(fits);
	image_->name("Ramp");
	return initNewImage();
    }
    return ERROR;
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
 * Implement the "cut" subcommand to set cut levels:
 *
 * usage: <path> cut <min> <max>  ?fromUser?
 *    or: <path> cut 
 *    or: <path> <rtdimage> 
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
    int stat = TCL_OK;
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

	stat = setCutLevels(min, max, 1, fromUser);
    }
    else if (argc == 0) {
	// return the current cut levels
	char buf[80];
	sprintf(buf, "%g %g", image_->lowCut(), image_->highCut());
	return set_result(buf);
    }

    // keep status in a global Tcl variable which can be traced by the Tcl code
    char* var = (viewMaster_ ? viewMaster_->instname_ : instname_);
    char sts[100];
    sprintf(sts, "%g %g",  image_->lowCut(), image_->highCut());
    Tcl_SetVar2(interp_, var, "CUT", sts, TCL_GLOBAL_ONLY);

    return stat;
}

/*
 * Implement the "info" subcommand
 *
 * usage: <path> info bbox
 *    or: <path> minmax x0 y0 x1 y1
 */
int RtdImage::infoCmd(int argc, char* argv[])
{
    char* msg = "invalid arguments for info subcommand";

    if (! image_)
	return set_result(0);

    if (argc < 1)
	return error(msg);

    if (strcmp(argv[0], "bbox") == 0) {
	double x0, y0, x1, y1;
	char buf[80];
	image_->getBbox(&x0, &y0, &x1, &y1);
	sprintf(buf, "%.1f %.1f %.1f %.1f", x0, y0, x1, y1);
	return set_result(buf);
    }

    if (strcmp(argv[0], "minmax") == 0) {
	if (argc < 5)
	    return error(msg);

	// get image area
	double x0, y0, minv = 0.0, maxv = 0.0;
	int w, h;
	int numval = 0;
	if (Tcl_GetDouble(interp_, argv[1], &x0) != TCL_OK
	    || Tcl_GetDouble(interp_, argv[2], &y0) != TCL_OK
	    || Tcl_GetInt(interp_, argv[3], &w) != TCL_OK
	    || Tcl_GetInt(interp_, argv[4], &h) != TCL_OK) {
	    return TCL_ERROR;
	}
	image_->getMinMax(x0, y0, w, h, &minv, &maxv);
	// return the min/max values
	char buf[80];
	sprintf(buf, "%g %g", minv, maxv);
	return set_result(buf);
    }
    return error(msg);
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
	
	    ostringstream os;
	    image_->getFitsHeader(os);
	    set_result(os.str().c_str());
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
 * With no arg. return the current status of flipX and flipY
 */
int RtdImage::flipCmd(int argc, char* argv[])
{
    if (!image_)  
	return TCL_OK;

    if (argc == 0) {
	char buf[80];
	sprintf(buf, "%d %d", image_->flipX(), image_->flipY());
	return set_result(buf);
    }

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

    if (flipX && flipY && argc == 1) {
	return set_result(image_->flipX() && image_->flipY());
    }

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
	    updateRequests();
	}
	autoPan(1);
    }

    // keep status in a global Tcl variable which can be traced by the Tcl code
    char* var = (viewMaster_ ? viewMaster_->instname_ : instname_);
    char sts[10];
    sprintf(sts, "%d %d", image_->flipX(), image_->flipY());
    Tcl_SetVar2(interp_, var, "FLIP", sts, TCL_GLOBAL_ONLY);
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
    delete[] xyvalues;
#endif
    return status;
}


/*
 * This method implements the "hdu" subcommand, to access different
 * FITS HDUs (header data units). Each HDU may be of type "image",
 * "binary" table or "ascii" table.
 *
 *  usage: <path> hdu count
 *  or:    <path> hdu list
 *  or:    <path> hdu listheadings
 *  or:    <path> hdu type ?number?
 *  or:    <path> hdu headings ?$number?
 *  or:    <path> hdu fits ?$number?
 *  or:    <path> hdu get ?$number? ?$filename? ?$entry?
 *  or:    <path> hdu create $type $extname $headings $tform $data
 *  or:    <path> hdu delete $number
 *  or:    <path> hdu set $number
 *  or:    <path> hdu ?$number?
 *  or:    <path> hdu display ?hduList?
 *
 * If the "hdu count" subcommand is specified, it returns the number of
 * HDUs in the current image.
 *
 * The "hdu type" subcommand returns the type of the current or given HDU
 * as a string "ascii", "binary" or "image".
 *
 * If the "hdu list" subcommand is specified, it returns a Tcl list of
 * FITS HDU information of the form:
 *
 *   {{number type extname naxis naxis1 naxis2 naxis3 crpix1 crpix2} ...}
 *
 * Where: 
 *
 *   - number is the HDU number
 *   - type is the HDU type: one of "image", "binary table", "ascii table".
 *   - extname is the value of the EXTNAME keyword, if set
 *   - naxis, naxis1, naxis2, naxis3 match the FITS keyword values.
 *
 * The "hdu listheadings" subcommand returns a list of the column names
 * returned by the "hdu list" subcommand. This can be used to set the
 * title of a table listing of the HDUs in a FITS file.
 *
 * The "hdu headings" subcommand returns a list of the column names
 * in the current or given FITS table.
 *
 * The "hdu fits" subcommand returns the FITS header
 * of the current or given HDU.
 *
 * The "hdu get" subcommand with no arguments returns the contents of the
 * current ASCII or binary table as a Tcl list (a list of rows, where
 * each row is a list of column values). If the HDU number is given, the
 * contents of the given HDU are returned. If a filename argument is
 * given, the FITS table is written to the given file in the form of a
 * local (tab separated) catalog.  If optional "entry" argument is given,
 * it specifies the catalog config entry as a list of {{keyword value}
 * {keyword value} ...}, as defined in the catalog config file
 * (~/.skycat/skycat.cfg). The entry is written to the header of the
 * local catalog file and is used mainly to specify plot symbol
 * information for the catalog.
 *
 * The "hdu create" command creates a new FITS table in the current image
 * file. $type maye be "ascii" for an ASCII table or "binary" for a
 * binary FITS table. The name of the table is given by extname. The
 * table headings and data correspond to the catalog headings and
 * data. The tform argument is a list of FITS storage formats, one for
 * each column, of the form {16A 2D 12A ...} (similar to FORTRAN formats,
 * see the FITS docs). 
 *
 * The "hdu delete" command deletes the given HDU. The argument is the HDU
 * number. The other HDUs in the file following the deleted one are moved to 
 * fill the gap.
 *
 * If the "hdu" subcommand is specified with no arguments, it returns the
 * current HDU number. If a number argument is given, the current HDU is
 * set to that number.
 *
 * The "hdu set" subcommand sets the current HDU to the given number.
 * The keyword "set" is optional (see below).
 *
 * The "hdu display ?hduList?" subcommand displays all of the image HDUs 
 * (or the ones specified) at once, combined in one image, using the 
 * world coordinates information in each HDU header as the reference for
 * the pixel positions.
 *
 * An optional numerical argument may be passed to the "hdu" subcommand,
 * in which case the "current HDU" is set to the given number.
 */
int RtdImage::hduCmd(int argc, char* argv[])
{
    if (!image_)
	return TCL_OK;

    // HDU operations: make sure it is a FITS file
    ImageIO imio = image_->image();
    if (!imio.rep() || strcmp(imio.rep()->classname(), "FitsIO") != 0)
	return error("The \"hdu\" subcommand is only supported for FITS files");
    FitsIO* fits = (FitsIO*)imio.rep();

    // <path> hdu  (return the current HDU number)
    if (argc == 0) 
	return set_result(fits->getHDUNum());

    // <path> hdu count
    if (strcmp(argv[0], "count") == 0) 
	return set_result(fits->getNumHDUs());

    // <path> hdu type ?number?
    if (strcmp(argv[0], "type") == 0) 
	return hduCmdType(argc, argv, fits);

    // <path> hdu listheadings
    // (return a list of table headings matching the "hdu list" output)
    if (strcmp(argv[0], "listheadings") == 0)   
	return set_result("HDU Type ExtName NAXIS NAXIS1 NAXIS2 NAXIS3 CRPIX1 CRPIX2");

    // <path> hdu headings ?$number?
    if (strcmp(argv[0], "headings") == 0) 
	return hduCmdHeadings(argc, argv, fits);

    // <path> hdu fits ?$number?
    if (strcmp(argv[0], "fits") == 0) 
	return hduCmdFits(argc, argv, fits);

    // <path> hdu get ?number? ?$filename? ?$entry?
    if (strcmp(argv[0], "get") == 0) 
	return hduCmdGet(argc, argv, fits);

    // <path> hdu create $type $extname $headings $tform $data
    if (strcmp(argv[0], "create") == 0) 
	return hduCmdCreate(argc, argv, fits);

    // <path> hdu delete $number
    if (strcmp(argv[0], "delete") == 0) 
	return hduCmdDelete(argc, argv, fits);

    // <path> hdu list
    if (strcmp(argv[0], "list") == 0)   
	return hduCmdList(argc, argv, fits);

    // <path> hdu set $number
    if (strcmp(argv[0], "set") == 0)    
	return hduCmdSet(argc, argv, fits);

    // <path> hdu display $hduList
    if (strcmp(argv[0], "display") == 0)    
	return hduCmdDisplay(argc, argv, fits);

    // <path> hdu $number (Set the current HDU)
    return hduCmdSet(argc, argv, fits);
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
	    }
	    else {
		ret = colors_->scaleITT(amount);
	    }
	}
	//  Force image update if colour changes do not transfer
	//  automatically (i.e. non-pseudocolor visual).
	if ( ret == TCL_OK ) {
	    return colorUpdate();
	}
	else {
	    return ret;
	}
    }

    if (strcmp(argv[0], "file") == 0) {
	return set_result(colors_->itt()->name());
    }

    if (strcmp(argv[0], "list") == 0) {
	ostringstream os;
	ITTInfo::list(os);
	set_result(os.str().c_str());
	return TCL_OK;
    }

    return error("expected: \"itt file\" or \"itt scale\"");

    return TCL_OK;
}

/*
 * Implement the "max" subcommand - returns the
 * highest pixel value in the visible image.
 *
 * usage:
 *         <path> max
 */
int RtdImage::maxCmd(int argc, char* argv[])
{
    if (!image_)
	return TCL_OK;

    return set_result(image_->maxValue());
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

    // clip to image bounds (full)
    double ix0 = 0.5, 
	iy0 = 0.5, 
	ix1 = 0.5 + image_->width(), 
	iy1 = 0.5 + image_->height();
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
    }
    else if (x0 < x1) {
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
 * Implement the "min" subcommand - returns the
 * lowest pixel value in the image.
 */
int RtdImage::minCmd(int argc, char* argv[])
{
    if (!image_)
	return TCL_OK;

    return set_result(image_->minValue());
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
		return error("mmap data file is too small for current image header");
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
	filename(data_filename);  // keep filename

	return initNewImage();
    }
    // $image mmap get ...
    else  if (strcmp(argv[0], "get") == 0) {
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
 *      perftest off   - turn performance testing off
 *      perftest on    - turn performance testing on
 *      perftest reset - reset the performance parameters
 *
 * Arguments:
 *      int argc, char *argv - argument list
 */
int RtdImage::perfTestCmd(int argc, char *argv[])
{
    // Check the arguments and act according to the usage instructions
    if (strcmp(argv[0], "on") == 0 || strcmp(argv[0], "reset") == 0) {
	// Set the testing on and reset the various parameters
	rtdperf_->reset();
	if (strcmp(argv[0], "on") == 0) {
	    rtdperf_->verbose(verbose());
	    rtdperf_->debug(debug());
	    if (argc > 1)
		rtdperf_->name(argv[1]);
	    else
		rtdperf_->name(viewMaster_ ? viewMaster_->instname_ : instname_);
	    rtdperf_->on();
	}
    }
    else if (strcmp(argv[0], "off") == 0) {
	rtdperf_->reset();
	rtdperf_->off();
    }
    else 
	return error("Unknown argument to perftest command");

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
	    delete[] pixTab_;

	// generate an array of pixel values with left and right headings
	// for the x/y coordinates
	pixTab_ = new double[++nrows*++ncols];
	if (pixTab_) 
	    memset((void*)pixTab_, '\0', nrows*ncols*sizeof(double));
    }
    else if (strcmp(argv[0], "stop") == 0) {
	if (pixTab_)
	    delete[] pixTab_;
	pixTab_ = NULL;
    }
    else {
	return error("expected image pixtab 'start nrows ncols' or 'stop'");
    }
    return TCL_OK;
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
    //dbg_->log("%s: previewCmd: camera = %x\n", name(), camera_);

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
    
    ostringstream os;
    os << pos1 << ' ' << pos2;
    return set_result(os.str().c_str());
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
	    updateRequests();
	}
	autoPan(1);
    }

    // keep status in a global Tcl variable which can be traced by the Tcl code
    char* var = (viewMaster_ ? viewMaster_->instname_ : instname_);
    char sts[10];
    sprintf(sts, "%d", image_->rotate());
    Tcl_SetVar2(interp_, var, "ROTATE", sts, TCL_GLOBAL_ONLY);
    return TCL_OK;
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

#if 0
    // add a check for the Tk limit on canvas coords
    if (xScale > 0 && xScale * image_->width() > 32767 
	|| yScale > 0 && yScale * image_->height() > 32767) 
	return error("sorry, can't scale image to this size without exceeding maximum Tk canvas coordinate range");
#endif

    int stat;
    stat =  setScale(xScale, yScale);

    // keep status in a global Tcl variable which can be traced by the Tcl code
    char* var = (viewMaster_ ? viewMaster_->instname_ : instname_);
    char sts[100];
    sprintf(sts, "%d %d", image_->xScale(), image_->yScale());
    Tcl_SetVar2(interp_, var, "SCALE", sts, TCL_GLOBAL_ONLY);

    return stat;
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
 * The "get" command returns the shared memory Id of the data or header
 * as well as the offset in the shared memory area where the header or
 * data begins, the length of the header or data and the total size of
 * the shared memory. The result of the "get header" or "get data"
 * command is a Tcl list of the 4 numbers {shmId offset length size},
 * where length is the length of the header or data and size is the total
 * size of the shared memory area. If the data or header is not currently
 * in shared memory, an error is returned. (RTD must be started with
 * option -shm_data 1 and/or -shm_header 1 for this command to work).
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
	char buf[80];
	if (strcmp(argv[1], "data") == 0) {
	    if (!image_->data().shared())
		return error("rtd was not started with the -shm_data option");
	    Mem m = image_->data();
	    sprintf(buf, "%d %d %d %d", m.shmId(), m.offset(), m.length(), m.size());
	    return set_result(buf);
	}
	else if (strcmp(argv[1], "header") == 0) {
	    if (!image_->header().shared())
		return error("rtd was not started with the -shm_header option");
	    Mem m = image_->header();
	    sprintf(buf, "%d %d %d %d", m.shmId(), m.offset(), m.length(), m.size());
	    return set_result(buf);
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
    int dist = (int)sqrt((double)w*w + h*h) + 2;

    double* xyvalues = new double[dist*2];

    // fill the xyvalues array and set numValues to the actual number of points
    int numValues = image_->getSpectrum(xyvalues, x0, y0, x1, y1);
    assert(numValues <= dist);
    if (Blt_GraphElement(interp_, argv[0], argv[1], numValues*2, xyvalues, argv[7], argv[8]) != TCL_OK) {
	delete xyvalues;
	return TCL_ERROR;
    }
    delete[] xyvalues;
    return set_result(numValues);
}

/*
 * statistics subcommand: calculate statistics.
 *
 * usage:  set list [$image statistics]
 *  or:    set list [$image statistics noise x0 y0 nx ny]
 *
 * no args -
 *           With no arguments the statistics on the section of the image
 *           being displayed is calculated (used for "pick object").
 *           The return value in Tcl is a list of the following values: 
 *
 *           {x y ra dec equinox fwhmX fwhmY angle objectPeak meanBackground}
 *
 *           where:
 *
 *           x              = X image coordinate
 *           y              = Y image coordinate
 *           ra             = RA position (calculated from mean X pos)
 *           dec            = DEC position (calculated from mean Y position) 
 *           equinox        = equinox of RA and DEC
 *           fwhmX          = FWHM in X
 *           fwhmY          = FWHM in Y
 *           angle          = angle of major axis, degrees, along X
 *           objectPeak     = peak value of object above background
 *           meanBackground = mean background level
 *
 *           Note: compare results with Midas (center/gauss) when changes are done
 *
 * noise -
 *           "noise" computes the noise statistics (RMS) on an image area.
 *           x0, y0 is the lower left corner (in image coordinates) and
 *           nx, ny the width and the height.
 *           The return value in Tcl is a list of the following values: 
 *
 *           {min, max, av, rms, n, xs, xe, ys, ye}
 *
 *           where:
 *
 *           min    = minimum value
 *           max    = maximum value
 *           av     = average value
 *           rms    = RMS value
 *           n      = number of samples used
 *           xs, ys = lower left corner (image coordinates) of area
 *           xe, ye = upper right corner (image coordinates) of area
 */
int RtdImage::statisticsCmd(int argc, char* argv[])
{
    char* msg = "invalid arguments for statistics subcommand";

    if (!image_)
	return error("no image loaded");

    if (argc == 0) {
	double w = reqWidth_, h = reqHeight_;
	undoTrans(w, h, 1); // get only visible area
	double x = xOffset_, y = yOffset_;
      
	// adapt transformations for getStatistics()
	switch(image_->flipX() << 1 | image_->flipY()) {
	case 0: // none
	    y += h;  // upper left corner for image area to be examined
	    break;
	case 1: // flipY
	    break;
	case 2: // flipX
	    x += w;
	    y += h;
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
	    // allan: 22.4.98, by request
	    x += w / 2.0;
	    y += h / 2.0;
	}
	else {
	    // get the image coords from the offsets 
	    x = meanX + (int)(x - 0.5) + 1.0;
	    y = meanY + (int)(y - 0.5) + 1.0;
	}
      
	double ix = x, iy = y;
      
	// get the world coords position from the image coords
	WorldCoords pos;
	if (imageToWorldCoords(x, y, 0) == TCL_OK) {
	    pos = WorldCoords(x, y);
	    if (pos.status() != 0)
		pos = WorldCoords();
	}
      
	ostringstream os;
	os << ix << ' ' << iy << ' ';
      
	if (pos.status() == 0 && ! pos.isNull())
	    os << pos << " J2000 "; // ra, dec, equinox: XXX use default equinox ?
	else 
	    os << "{} {} {} ";      // no world coords
      
	os << fwhmX << ' ' << fwhmY << ' ' << angle << ' ' 
	   << objectPeak << ' ' << meanBackground;
      
	return set_result(os.str().c_str());
    }

    if (argc == 5 && strcmp(argv[0], "noise") == 0) {
	/*
	 * $image statistics area x0 y0 nx ny
	 */
	double x0 = 0., y0 = 0., nx = 0., ny = 0.;
	double dmin = 0., dmax = 0., av = 0., rms = 0.;
	int n = 0, xs = 0, xe = 0, ys = 0, ye = 0;
	if (Tcl_GetDouble(interp_, argv[1], &x0) == TCL_ERROR ||
	    Tcl_GetDouble(interp_, argv[2], &y0) == TCL_ERROR ||
	    Tcl_GetDouble(interp_, argv[3], &nx) == TCL_ERROR ||
	    Tcl_GetDouble(interp_, argv[4], &ny) == TCL_ERROR)
	    return error(msg);

	char buf[1024];
	n = image_->noiseStatistics(x0, y0, int(nx), int(ny), 
				    &dmin, &dmax, &av, &rms,
				    &xs, &xe, &ys, &ye);
	sprintf (buf, "%g %g %g %g %d %d %d %d %d", 
		 dmin, dmax, av, rms, n, xs, xe, ys, ye);
	return set_result(buf);
    }

    return error(msg);
}

/*
 * bltgraph subcommand: various commands to display 2-D data
 * in a blt::graph widget
 *
 * usage:
 *         <path> bltgraph xline bltGraph bltElem xVector yVector y <from> <to> xr0 dxr
 *  or:    <path> bltgraph yline bltGraph bltElem xVector yVector x <from> <to>
 *
 *  or:    <path> bltgraph spectrum bltGraph bltElem x0 y0 x1 y1 coord_type xVector yVector
 *  or:    <path> bltgraph graphdist bltGraph bltElem numValues xVector yVector
 */
int RtdImage::bltgraphCmd(int argc, char* argv[])
{
    char* msg = "invalid arguments for bltgraph subcommand";
    int stat, numValues;

   if (argc < 6)
	return error(msg);
    if (!image_)
	return error("no image loaded");

    // for compatibility:
    if (strcmp(argv[0], "spectrum") == 0) {
	return spectrumCmd(argc - 1, argv++);
    }
    if (strcmp(argv[0], "graphdist") == 0) {
	return graphdistCmd(argc - 1, argv++);
    }

    if (strcmp(argv[0], "yline") == 0) {
	double x = 0;
	if (Tcl_GetDouble(interp_, argv[5], &x) == TCL_ERROR)
	    return error(msg);
	
	double y0 = 0, y1 = image_->height() - 1;
	if (argc > 6) {
	    if (Tcl_GetDouble(interp_, argv[6], &y0) == TCL_ERROR)
		return error(msg);
	}
	if (argc > 7) {
	    if (Tcl_GetDouble(interp_, argv[7], &y1) == TCL_ERROR)
		return error(msg);
	}
	double *xyvalues = new double[image_->height()*4];
	int numValues = image_->getYline4((int)x, (int)y0, (int)y1, xyvalues);
	stat = Blt_GraphElement(interp_, argv[1], argv[2], numValues*4, xyvalues, 
				argv[3], argv[4]);
	delete xyvalues;
	return stat;
    }

    if (strcmp(argv[0], "xline") == 0) {
	double y = 0, xr0 = 0.0, dxr = 1.0;
	if (Tcl_GetDouble(interp_, argv[5], &y) == TCL_ERROR)
	    return error(msg);
	
	double x0 = 0, x1 = image_->width() - 1;
	if (argc > 6) {
	    if (Tcl_GetDouble(interp_, argv[6], &x0) == TCL_ERROR)
		return error(msg);
	}
	if (argc > 7) {
	    if (Tcl_GetDouble(interp_, argv[7], &x1) == TCL_ERROR)
		return error(msg);
	}
	if (argc > 8) {
	    if (Tcl_GetDouble(interp_, argv[8], &xr0) == TCL_ERROR)
		return error(msg);
	}
	if (argc > 9) {
	    if (Tcl_GetDouble(interp_, argv[9], &dxr) == TCL_ERROR)
		return error(msg);
	}
	double *xyvalues = new double[image_->width()*4];
	if (argc > 9) {
	    numValues = image_->getXline4((int)y, (int)x0, (int)x1, xyvalues, xr0, dxr);
	}
	else {
	    numValues = image_->getXline4((int)y, (int)x0, (int)x1, xyvalues);
	}
	stat = Blt_GraphElement(interp_, argv[1], argv[2], numValues*4, xyvalues, 
				argv[3], argv[4]);
	delete xyvalues;
	return stat;
    }
    return error(msg);
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
    case DOUBLE_IMAGE: 
	return set_result("double");
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
    updateRequests();
    return 0;
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
	options_->rtd_options_->fixUpdateRate = 0;
	options_->rtd_options_->userUpdateTime = 0.;
    }
    else {
	options_->rtd_options_->fixUpdateRate = 1;
	options_->rtd_options_->userUpdateTime = 1./maxFreq;
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

	//      dbg_->log("%s: update %s: xyOffset(%g,%g), size(%g,%g), frame(%g,%g), rapid(%g,%g)\n", 
	//  	     name(), view->name(), xOffset, yOffset, width, height, frameX, frameY, 
	//  	     rapidX, rapidY);
	
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
 * wcsdeltset subcommand
 *
 * usage:
 *    $image wcsdeltset $cdelt1 $cdelt2 $rotation
 *
 * Set rotation and scaling
 *
 *    Args:
 *      cdelt1     = scale in degrees/pixel (axis 1); degrees = arcsec/3600.
 *      cdelt2     = scale in degrees/pixel (axis 2)
 *      rotation   = rotation angle in degrees
 *
 */
int RtdImage::wcsdeltsetCmd(int argc, char* argv[])
{
    if (!isWcs())
	return TCL_OK;

    double cdelt1, cdelt2, rotation;
    if (Tcl_GetDouble(interp_, argv[0], &cdelt1) != TCL_OK
	|| Tcl_GetDouble(interp_, argv[1], &cdelt2) != TCL_OK
	|| Tcl_GetDouble(interp_, argv[2], &rotation) != TCL_OK)
	return TCL_ERROR;
    return image_->wcs().deltset(cdelt1, cdelt2, rotation);
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
    // XXX use new convert command here ???
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
 
	if (zoomFactor < 1 || zoomFactor > 160) 
	    return error("zoomFactor should be between 1 and 160");

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

	if (zoomFactor < 1 || zoomFactor > 160) 
	    return error("zoomFactor should be between 1 and 160");

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
        
        //  Reset zoomfactor for reference counted copies.
	view->zoomFactor_ = 1;
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
	    view_[i]->zoomView_  = zoomView_;
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
