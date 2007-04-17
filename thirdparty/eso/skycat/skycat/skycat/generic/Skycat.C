/*
 * E.S.O. - VLT project 
 * "@(#) $Id: Skycat.C,v 1.3 2006/03/26 14:03:13 abrighto Exp $"
 *
 * Skycat.C - Initialize Skycat package
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  21/11/97  Created
 *
 *                 10/03/98  Added optional args to constructor to allow derived
 *                           class to specify its own configuration options.
 *
 *                 21/02/00  Moved HDU command to bass class RtdImage                 
 */
static const char* const rcsId="@(#) $Id: Skycat.C,v 1.3 2006/03/26 14:03:13 abrighto Exp $";

#include <cstring>
#include <cstdlib>
#include <csignal>
#include <cstdio>
#include <iostream>
#include <sstream>
#include <fstream>
#include <sys/types.h>
#include <unistd.h>
#include <cmath>
#include <cassert>
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "tcl.h"
#include "tk.h"
#include "error.h"
#include "define.h"
#include "util.h"
#include "SkySearch.h"
#include "TcsSkySearch.h"
#include "Fits_IO.h"
#include "Skycat.h"

extern "C" int Cat_Init(Tcl_Interp *interp);
extern "C" int Rtd_Init(Tcl_Interp *interp);


// Tcl procedure to search for an init for Skycat startup file.  
static char initScript[] = "if {[info proc ::skycat::Init]==\"\"} {\n\
  namespace eval ::skycat {}\n\
  proc ::skycat::Init {} {\n"
#ifdef MAC_TCL
"    source -rsrc SkycatInit.tcl\n"
#else
"    global skycat_library\n\
     tcl_findLibrary skycat " PACKAGE_VERSION " " PACKAGE_VERSION " SkycatInit.tcl SKYCAT_LIBRARY skycat_library\n"
#endif
"  }\n\
}\n\
::skycat::Init";


// math constants
static const double pi_ = 3.14159265358979323846;
static const double rad_ = pi_/180.;

// size of buffer to use to hold tcl commands to evaluate
static const int eval_buf_size_ = 4*1024;


/* 
 * declare a table of skycat image subcommands and the methods that 
 * handle them.
 */
static class SkycatSubCmds {
public:
    char* name;      // method name
    int (Skycat::*fptr)(int argc, char* argv[]); // ptr to method
    int min_args;    // minimum number of args
    int max_args;    // maximum number of args
} subcmds_[] = { 
    {"symbol", &Skycat::symbolCmd,  9,  13}
};


/*
 * Initialize the image control structure with pointers to the handler
 * functions
 */
static Tk_ImageType skycatImageType = {
    "rtdimage",			/* name */
    Skycat::CreateImage,	/* createProc */
    TkImage::GetImage,	        /* getProc */
    TkImage::DisplayImage,      /* displayProc */
    TkImage::FreeImage,	        /* freeProc */
    TkImage::DeleteImage,	/* deleteProc */

#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
    // XXX RtdImage::Postscript,	/* postscriptProc */
    (Tk_ImagePostscriptProc *) NULL,    /* postscriptProc */
#endif

    (Tk_ImageType *) NULL	/* nextPtr */
};


/*
 * A call to this function is made from the tkAppInit file at startup
 * to initialize this package
 */
extern "C"
int Skycat_Init(Tcl_Interp* interp)  
{
    // initialize the Rtd package 
    if (Rtd_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    // initialize the Cat package 
    if (Cat_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    // set up Tcl package
    if (Tcl_PkgProvide (interp, "Skycat", PACKAGE_VERSION) != TCL_OK) {
	return TCL_ERROR;
    }

    // install the new, extended astrocat command 
    Tcl_CreateCommand(interp, "astrocat", (Tcl_CmdProc*)SkySearch::astroCatCmd, NULL, NULL);
    
    // also extend the tcscat command
    Tcl_CreateCommand(interp, "tcscat", (Tcl_CmdProc*)TcsSkySearch::tcsCatCmd, NULL, NULL);

    // add the skycat image type
    Tk_CreateImageType(&skycatImageType);

    Tcl_SetVar(interp, "skycat_version", PACKAGE_VERSION, TCL_GLOBAL_ONLY);
    return Tcl_Eval(interp, initScript);
}


/*
 * This static method is called by the Tk image code to create
 * a new skycat image.
 */
int Skycat::CreateImage(
    Tcl_Interp *interp,		// Interpreter for application containing image.
    char *name,			// Name to use for image.
    int argc,			// Number of arguments.
#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
    Tcl_Obj *CONST objv[],      // Argument objects for options (not including image name or type)
#else
    char **argv,		// Argument strings for options (not including image name or type)
#endif
    Tk_ImageType *typePtr,	// Pointer to our type record (not used). 
    Tk_ImageMaster master,	// Token for image, to be used by us in later callbacks.
    ClientData *clientDataPtr)	// Store manager's token (this ptr) for image here
				// it will be returned in later callbacks.
{
#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
    // just generate an argv from the objv argument
    char* argv[64];  // there shouldn't be more than a few options...
    for(int i = 0; i < argc; i++)
	argv[i] = Tcl_GetString(objv[i]);
    argv[argc] = NULL;
#endif

    Skycat* im = new Skycat(interp, name, argc, argv, master, skycatImageType.name);
    *clientDataPtr = (ClientData) im;
    return im->status();
}


/*
 * Constructor: initialize a new skycat image with the command line args
 * 
 * This constructor is called for each skycat image declared in tk. The destructor
 * is called when the image image is destroyed.
 */
Skycat::Skycat(Tcl_Interp* interp, const char* instname, int argc, char** argv,
		   Tk_ImageMaster master, const char* imageType,
		   Tk_ConfigSpec* specs, RtdImageOptions* options)
: RtdImage(interp, instname, argc, argv, master, imageType, specs, options)
{
}


/*
 * Call the given method in this class with the given arguments
 */
int Skycat::call(const char* name, int len, int argc, char* argv[])
{
    for(unsigned int i = 0; i < sizeof(subcmds_)/sizeof(*subcmds_); i++) {
	SkycatSubCmds* t = &subcmds_[i];
	if (strncmp(t->name, name, len) == 0) {
	    if (check_args(name, argc, t->min_args, t->max_args) != TCL_OK)
		return TCL_ERROR;
	    return (this->*t->fptr)(argc, argv);
	}
    }
    return RtdImage::call(name, len, argc, argv);
}


/*
 * return a pointer to the Skycat class object for the given tcl rtdimage
 * instance name, or NULL if the name is not an rtdimage.
 */
Skycat* Skycat::getInstance(char* name)
{
    // check that the argument really is an rtdimage...
    if (strncmp(name, "image", 5) != 0) {
	::error("expected an rtdimage id but got: ", name);
	return NULL;
    }

    // get class instance pointer from name
    Tcl_CmdInfo info;
    if (Tcl_GetCommandInfo(maininterp_, name, &info) == 0) {
	::error("expected an \"rtdimage\" type image");
	return NULL;
    }
    return (Skycat*)info.clientData;
}


/*
 * Draw a symbol on the image with the given shape at the given coordinates
 * (in the given x,y units), with the given radius (in radius_units), 
 * bg and fg color, canvas tags list, x/y ratio and rotation angle.
 *
 * shape may be one of "circle", "square", "plus", "cross", "triangle",
 * "diamond", "ellipse", "compass", "line", "arrow".
 *
 * x and y are the coordinates in "xy_units", which is one of the units
 * accepted by the Rtd commands (canvas, image, screen, "wcs $equinox", 
 * "deg $equinox").
 *
 * The radius value is interpreted in radius_units.
 * 
 * bg and fg are X color names for the symbol (may be the same).
 *
 * symbol_tags should be a Tcl list of canvas tags for the symbol.
 *
 * ratio and angle are used to stretch/shrink and rotate the symbol.
 *
 * label is an optional text for a label to place near the symbol.
 *
 * label_tags should be a Tcl list of canvas tags for the label, or
 * an empty or null string, if there is no label.
 *
 * Returns an error if the coordinates or part of the symbol are off 
 * the image.
 *
 * Uses world coordinates, if available, for the rotation and orientation, 
 * for symbols that support it (i.e.: rotation is relative to WCS north).
 */
int Skycat::draw_symbol(const char* shape, 
			double x, double y, const char* xy_units, 
			double radius, const char* radius_units, 
			const char* bg, const char* fg, 
			const char* symbol_tags, 
			double ratio, double angle,
			const char* label, const char* label_tags)
{
    static struct SymbolTab {
	// symbol name
	char* name;
	 // ptr to method to draw the symbol
	int (Skycat::*fptr)(double x, double y, const char* xy_units, 
			    double radius, const char* radius_units, 
			    const char* bg, const char* fg, 
			    const char* symbol_tags, 
			    double ratio, double angle,
			    const char* label, const char* label_tags);
    } symbols[] = {
	{"circle", &Skycat::draw_circle},
	{"square", &Skycat::draw_square},
	{"plus", &Skycat::draw_plus},
	{"cross", &Skycat::draw_cross},
	{"triangle", &Skycat::draw_triangle},
	{"diamond", &Skycat::draw_diamond},
	{"ellipse", &Skycat::draw_ellipse},
	{"compass", &Skycat::draw_compass},
	{"line", &Skycat::draw_line},
	{"arrow", &Skycat::draw_arrow}
    };
    static int nsymbols = sizeof(symbols)/sizeof(SymbolTab);
   
    // symbol shape
    for (int i = 0; i < nsymbols; i++) {
	if (strcmp(shape, symbols[i].name) == 0) {
	    return (this->*symbols[i].fptr)(x, y, xy_units, radius, radius_units, 
					    bg, fg, symbol_tags, ratio, angle,
					    label, label_tags);
	}
    }
    return error("invalid plot symbol");
}


/*
 * Write a Tcl canvas command to the given stream to add a label to the
 * image at the given canvas coordinates with the given label
 * text, color and canvas tags.
 */
int Skycat::make_label(std::ostream& os, const char* label, double x, double y, 
		       const char* tags, const char* color, const char* font)
{
    os << canvasName_ << " create text "
       << x << ' ' << y
       << " -text {" << label 
       << "} -anchor sw -fill " << color
       << " -font " << font
       << " -tags " << "{" << tags << "}"
       << std::endl;
    
    return TCL_OK;
}


/*
 * draw a circle at the given coords
 * See "draw_symbol" for a description of the arguments.
 */
int Skycat::draw_circle(double x, double y, const char* xy_units, 
			double radius, const char* radius_units, 
			const char* bg, const char* fg, 
			const char* symbol_tags, double ratio, double angle,
			const char* label, const char* label_tags)
{
    double rx = radius, ry = radius;
    if (convertCoords(0, x, y, xy_units, "canvas") != TCL_OK
	|| convertCoords(1, rx, ry, radius_units, "canvas") != TCL_OK) {
	reset_result(); // ignore off scale symbols
	return TCL_OK;
    }
    double x0 = x - rx;
    double y0 = y - ry;
    double x1 = x + rx;
    double y1 = y + ry;

    // if using 2 colors, draw 2 symbols, for visibility, one thicker
    std::ostringstream os;
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create oval "
	   << x0-1 << ' ' << y0-1 << ' ' << x1+1 << ' ' << y1+1
	   << " -outline " << bg
	   << " -fill " << bg
	   << " -width 1 -stipple pat7 -tags " << "{" << symbol_tags << "}" 
	   << std::endl;
    }
    os << canvasName_ << " create oval "
       << x0 << ' ' << y0 << ' ' << x1 << ' ' << y1
       << " -outline " << fg
       << " -fill " << bg
       << " -width 1 -stipple pat7 -tags " << "{" << symbol_tags << "}"
       << std::endl;

    if (label && strlen(label)) 
	make_label(os, label, x, y, label_tags, fg);
    
    return eval(os.str().c_str());
}


/*
 * draw a square at the given coords
 * See "draw_symbol" for a description of the arguments.
 */
int Skycat::draw_square(double x, double y, const char* xy_units, 
			double radius, const char* radius_units, 
			const char* bg, const char* fg, 
			const char* symbol_tags, double ratio, double angle,
			const char* label, const char* label_tags)
{
    double rx = radius, ry = radius;
    if (convertCoords(0, x, y, xy_units, "canvas") != TCL_OK
	|| convertCoords(1, rx, ry, radius_units, "canvas") != TCL_OK) {
	reset_result(); // ignore off scale symbols
	return TCL_OK;
    }
    double x0 = x - rx;
    double y0 = y - ry;
    double x1 = x + rx;
    double y1 = y + ry;

    // if using 2 colors, draw 2 symbols, for visibility, one thicker
    std::ostringstream os;
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create rect "
	   << x0-1 << ' ' << y0-1 << ' ' << x1+1 << ' ' << y1+1
	   << " -outline " << bg
	   << " -fill " << bg
	   << " -width 1 -stipple pat7 -tags " << "{" << symbol_tags << "}" 
	   << std::endl;
    }
    os << canvasName_ << " create rect "
       << x0 << ' ' << y0 << ' ' << x1 << ' ' << y1
       << " -outline " << fg
       << " -fill " << bg
       << " -width 1 -stipple pat7 -tags " << "{" << symbol_tags << "}"
       << std::endl;
    
    if (label && strlen(label)) 
	make_label(os, label, x, y, label_tags, fg);
    
    return eval(os.str().c_str());
}


/*
 * draw a cross symbol at the given coords
 * See "draw_symbol" for a description of the arguments.
 */
int Skycat::draw_cross(double x, double y, const char* xy_units, 
			double radius, const char* radius_units, 
			const char* bg, const char* fg, 
			const char* symbol_tags, double ratio, double angle,
			const char* label, const char* label_tags)
{
    double rx = radius, ry = radius;
    if (convertCoords(0, x, y, xy_units, "canvas") != TCL_OK
	|| convertCoords(1, rx, ry, radius_units, "canvas") != TCL_OK) {
	reset_result(); // ignore off scale symbols
	return TCL_OK;
    }
    double x0 = x - rx;
    double y0 = y - ry;
    double x1 = x + rx;
    double y1 = y + ry;

    // if using 2 colors, draw 2 symbols, for visibility, one thicker
    std::ostringstream os;
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create line "
	   << x0 << ' ' << y0 << ' ' << x1 << ' ' << y1
	   << " -fill " << bg
	   << " -width 2 -tags " << "{" << symbol_tags << "}" 
	   << std::endl;
	os << canvasName_ << " create line "
	   << x1 << ' ' << y0 << ' ' << x0 << ' ' << y1
	   << " -fill " << bg
	   << " -width 2 -tags " << "{" << symbol_tags << "}" 
	   << std::endl;
    }
    os << canvasName_ << " create line "
       << x0 << ' ' << y0 << ' ' << x1 << ' ' << y1
       << " -fill " << fg
       << " -width 1 -tags " << "{" << symbol_tags << "}"
       << std::endl;
    os << canvasName_ << " create line "
       << x1 << ' ' << y0 << ' ' << x0 << ' ' << y1
       << " -fill " << fg
       << " -width 1 -tags " << "{" << symbol_tags << "}"
       << std::endl;
    
    if (label && strlen(label)) 
	make_label(os, label, x, y, label_tags, fg);
    
    return eval(os.str().c_str());
}


/*
 * draw a triangle at the given coords
 * See "draw_symbol" for a description of the arguments.
 */
int Skycat::draw_triangle(double x, double y, const char* xy_units, 
			double radius, const char* radius_units, 
			const char* bg, const char* fg, 
			const char* symbol_tags, double ratio, double angle,
			const char* label, const char* label_tags)
{
    double rx = radius, ry = radius;
    if (convertCoords(0, x, y, xy_units, "canvas") != TCL_OK
	|| convertCoords(1, rx, ry, radius_units, "canvas") != TCL_OK) {
	reset_result(); // ignore off scale symbols
	return TCL_OK;
    }
    double x0 = x - rx;
    double y0 = y - ry;
    double x1 = x + rx;
    double y1 = y + ry;

    // if using 2 colors, draw 2 symbols, for visibility, one thicker
    std::ostringstream os;
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create polygon "
	   << x0 << ' ' << y1 << ' ' << x << ' ' << y0 << ' ' << x1 << ' ' << y1
	   << " -outline " << bg
	   << " -fill " << bg
	   << " -width 2 -stipple pat7 -tags " << "{" << symbol_tags << "}" 
	   << std::endl;
    }
    os << canvasName_ << " create polygon "
       << x0 << ' ' << y1 << ' ' << x << ' ' << y0 << ' ' << x1 << ' ' << y1
       << " -outline " << fg
       << " -fill " << bg
       << " -width 1 -stipple pat7 -tags " << "{" << symbol_tags << "}" 
       << std::endl;
    
    if (label && strlen(label)) 
	make_label(os, label, x, y, label_tags, fg);
    
    return eval(os.str().c_str());
}


/*
 * draw a diamond at the given coords
 * See "draw_symbol" for a description of the arguments.
 */
int Skycat::draw_diamond(double x, double y, const char* xy_units, 
			double radius, const char* radius_units, 
			const char* bg, const char* fg, 
			const char* symbol_tags, double ratio, double angle,
			const char* label, const char* label_tags)
{
    double rx = radius, ry = radius;
    if (convertCoords(0, x, y, xy_units, "canvas") != TCL_OK
	|| convertCoords(1, rx, ry, radius_units, "canvas") != TCL_OK) {
	reset_result(); // ignore off scale symbols
	return TCL_OK;
    }
    double x0 = x - rx;
    double y0 = y - ry;
    double x1 = x + rx;
    double y1 = y + ry;

    // if using 2 colors, draw 2 symbols, for visibility, one thicker
    std::ostringstream os;
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create polygon "
	   << x0 << ' ' << y << ' ' << x << ' ' << y0 << ' ' << x1 << ' ' << y << ' ' << x << ' ' << y1
	   << " -outline " << bg
	   << " -fill " << bg
	   << " -width 2 -stipple pat7 -tags " << "{" << symbol_tags << "}" 
	   << std::endl;
    }
    os << canvasName_ << " create polygon "
       << x0 << ' ' << y << ' ' << x << ' ' << y0 << ' ' << x1 << ' ' << y << ' ' << x << ' ' << y1
       << " -outline " << fg
       << " -fill " << bg
       << " -width 1 -stipple pat7 -tags " << "{" << symbol_tags << "}" 
       << std::endl;
    
    if (label && strlen(label)) 
	make_label(os, label, x, y, label_tags, fg);
    
    return eval(os.str().c_str());
}


/*
 * rotate the point x,y around the center point cx,cy by the given
 * angle in deg.
 */
int Skycat::rotate_point(double& x, double& y, double cx, double cy, double angle)
{
    x = x-cx;
    y = y-cy;
    double tmp = x;
    double rad = angle* rad_;
    double cosa = cos(rad);
    double sina = sin(rad);
    x = x*cosa+y*sina+cx;
    y = -tmp*sina+y*cosa+cy;
    
    return TCL_OK;
}


/*
 * Return the canvas coordinates of the 3 points: center, north and east, 
 * given the center point and radius in the given units, an optional 
 * rotation angle, and an x/y ellipticity ratio. 
 * If the image supports world coordinates, that is taken into account 
 * (the calculations are done in RA,DEC before converting to canvas coords). 
 * The conversion to canvas coords automatically takes the current zoom and 
 * rotate settings into account. The return arguments {cx cy nx ny ex ey} 
 * are for the 3 points center, north and east.
 */
int Skycat::get_compass(double x, double y, const char* xy_units, 
			double radius, const char* radius_units, 
			double ratio, double angle,
			double& cx, double& cy, double& nx, double& ny, 
			double& ex, double& ey)
{
    double rx = radius, ry = radius;
    cx = x;
    cy = y;

    if (isWcs()) {
	// get center and radius in deg 2000
	if (convertCoords(0, cx, cy, xy_units, "deg") != TCL_OK
	    || convertCoords(1, rx, ry, radius_units, "deg") != TCL_OK) {
	    return TCL_ERROR;
	}

	// adjust the radius by the ratio
	if (ratio < 1.) 
	    ry *= 1.0/ratio;
	else if (ratio > 1.) 
	    rx *= ratio;

	// (cx,cy) is center, (nx,ny) is north, (ex,ey) is east, in world coords deg
	ex = fmod(cx+fabs(rx)/cos((cy/180.)*pi_),360.);
	ey = cy;
	if (ex < 0.)
	    ex += 360;

	nx = cx;
	ny = cy + fabs(ry);
	if (ny >= 90.) 
	    ny = 180.-ny;
	else if (ny <= -90.) 
	    ny = -180.-ny;
	    
	// convert to canvas coords
	if (convertCoords(0, nx, ny, "deg", "canvas") != TCL_OK
	    || convertCoords(0, ex, ey, "deg", "canvas") != TCL_OK
	    || convertCoords(0, cx, cy, "deg", "canvas") != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    else {
	// not using world coords, go right to canvas coords
	if (convertCoords(0, cx, cy, xy_units, "canvas") != TCL_OK
	    || convertCoords(1, rx, ry, radius_units, "canvas") != TCL_OK) {
	    return TCL_ERROR;
	}

	// adjust the radius by the ratio
	if (ratio < 1.) 
	    ry *= 1.0/ratio;
	else if (ratio > 1.) 
	    rx *= ratio;

	ex = cx-rx;
	ey = cy;
	nx = cx;
	ny = cy-ry;

    }

    // rotate by angle
    if (angle) {
	rotate_point(nx, ny, cx, cy, angle);
	rotate_point(ex, ey, cx, cy, angle);
    }

    return TCL_OK;
}


/*
 * draw a plus symbol at the given coords
 * See "draw_symbol" for a description of the arguments.
 */
int Skycat::draw_plus(double x, double y, const char* xy_units, 
			double radius, const char* radius_units, 
			const char* bg, const char* fg, 
			const char* symbol_tags, double ratio, double angle,
			const char* label, const char* label_tags)
{
    double cx, cy, nx, ny, ex, ey;
    if (get_compass(x, y, xy_units, radius, radius_units, ratio, angle,
		    cx, cy, nx, ny, ex, ey) != TCL_OK) {
	reset_result(); // ignore off scale symbols
	return TCL_OK;
    }

    // flip to get mirror image for S and W
    double sx = cx-(nx-cx);
    double sy = cy-(ny-cy);
    double wx = cx+(cx-ex);
    double wy = cy+(cy-ey);

    // if using 2 colors, draw 2 symbols, for visibility, one thicker
    char buf[eval_buf_size_];
    std::ostringstream os;
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create line "
	   << nx << ' ' << ny << ' ' << sx << ' ' << sy
	   << " -fill " << bg
	   << " -width 2 -tags " << "{" << symbol_tags << "}" 
	   << std::endl;
	os << canvasName_ << " create line "
	   << ex << ' ' << ey << ' ' << wx << ' ' << wy
	   << " -fill " << bg
	   << " -width 2 -tags " << "{" << symbol_tags << "}" 
	   << std::endl;
    }
    os << canvasName_ << " create line "
       << nx << ' ' << ny << ' ' << sx << ' ' << sy
       << " -fill " << fg
       << " -width 1 -tags " << "{" << symbol_tags << "}" 
       << std::endl;
    os << canvasName_ << " create line "
       << ex << ' ' << ey << ' ' << wx << ' ' << wy
       << " -fill " << fg
       << " -width 1 -tags " << "{" << symbol_tags << "}" 
       << std::endl;
    
    if (label && strlen(label)) 
	make_label(os, label, cx, cy, label_tags, fg);
    
    return eval(os.str().c_str());
}


/*
 * draw an ellipse at the given coords
 * See "draw_symbol" for a description of the arguments.
 */
int Skycat::draw_ellipse(double x, double y, const char* xy_units, 
			double radius, const char* radius_units, 
			const char* bg, const char* fg, 
			const char* symbol_tags, double ratio, double angle,
			const char* label, const char* label_tags)
{
    double cx, cy, nx, ny, ex, ey;
    if (get_compass(x, y, xy_units, radius, radius_units, ratio, angle,
		    cx, cy, nx, ny, ex, ey) != TCL_OK) {
	reset_result(); // ignore off scale symbols
	return TCL_OK;
    }

    // flip to get mirror image for S and W
    double sx = cx-(nx-cx);
    double sy = cy-(ny-cy);
    double wx = cx+(cx-ex);
    double wy = cy+(cy-ey);

    // if using 2 colors, draw 2 symbols, for visibility, one thicker
    std::ostringstream os;
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create polygon "
	   << nx << ' ' << ny << ' ' << ex << ' ' << ey << ' ' 
	   << sx << ' ' << sy << ' ' << wx << ' ' << wy
	   << " -outline " << bg
	   << " -fill " << bg
	   << " -width 2  -smooth 1 -stipple pat7 -tags " << "{" << symbol_tags << "}" 
	   << std::endl;
    }
    os << canvasName_ << " create polygon "
       << nx << ' ' << ny << ' ' << ex << ' ' << ey << ' ' 
       << sx << ' ' << sy << ' ' << wx << ' ' << wy
       << " -outline " << bg
       << " -fill " << fg
       << " -width 1  -smooth 1 -stipple pat7 -tags " << "{" << symbol_tags << "}" 
       << std::endl;
    
    if (label && strlen(label)) 
	make_label(os, label, cx, cy, label_tags, fg);
    
    return eval(os.str().c_str());
}


/*
 * draw a compass at the given coords
 * See "draw_symbol" for a description of the arguments.
 */
int Skycat::draw_compass(double x, double y, const char* xy_units, 
			double radius, const char* radius_units, 
			const char* bg, const char* fg, 
			const char* symbol_tags, double ratio, double angle,
			const char* label, const char* label_tags)
{
    double cx, cy, nx, ny, ex, ey;
    if (get_compass(x, y, xy_units, radius, radius_units, ratio, angle,
		    cx, cy, nx, ny, ex, ey) != TCL_OK) {
	reset_result(); // ignore off scale symbols
	return TCL_OK;
    }

    // if using 2 colors, draw 2 symbols, for visibility, one thicker
    std::ostringstream os;
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create line "
	   << cx << ' ' << cy << ' ' << nx << ' ' << ny
	   << " -fill " << bg
	   << " -width 2 -arrow last -tags " << "{" << symbol_tags << "}" 
	   << std::endl;
	os << canvasName_ << " create line "
	   << cx << ' ' << cy << ' ' << ex << ' ' << ey
	   << " -fill " << bg
	   << " -width 2 -tags " << "{" << symbol_tags << "}" 
	   << std::endl;
    }
    os << canvasName_ << " create line "
       << cx << ' ' << cy << ' ' << nx << ' ' << ny
       << " -fill " << fg
       << " -width 1 -arrow last -tags " << "{" << symbol_tags << "}" 
       << std::endl;
    os << canvasName_ << " create line "
       << cx << ' ' << cy << ' ' << ex << ' ' << ey
       << " -fill " << fg
       << " -width 1 -tags " << "{" << symbol_tags << "}" 
       << std::endl;
    
    if (label && strlen(label)) 
	make_label(os, label, cx, cy, label_tags, fg);
    
    return eval(os.str().c_str());
}


/*
 * draw a line at the given coords
 * See "draw_symbol" for a description of the arguments.
 */
int Skycat::draw_line(double x, double y, const char* xy_units, 
			double radius, const char* radius_units, 
			const char* bg, const char* fg, 
			const char* symbol_tags, double ratio, double angle,
			const char* label, const char* label_tags)
{
    double cx, cy, nx, ny, ex, ey;
    if (get_compass(x, y, xy_units, radius, radius_units, ratio, angle,
		    cx, cy, nx, ny, ex, ey) != TCL_OK) {
	reset_result(); // ignore off scale symbols
	return TCL_OK;
    }

    // flip to get mirror image for S
    double sx = cx-(nx-cx);
    double sy = cy-(ny-cy);

    // if using 2 colors, draw 2 symbols, for visibility, one thicker
    std::ostringstream os;
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create line "
	   << nx << ' ' << ny << ' ' << sx << ' ' << sy
	   << " -fill " << bg
	   << " -width 2 -tags " << "{" << symbol_tags << "}" 
	   << std::endl;
    }
    os << canvasName_ << " create line "
       << nx << ' ' << ny << ' ' << sx << ' ' << sy
       << " -fill " << fg
       << " -width 1 -tags " << "{" << symbol_tags << "}" 
       << std::endl;
    
    if (label && strlen(label)) 
	make_label(os, label, cx, cy, label_tags, fg);
    
    return eval(os.str().c_str());
}


/*
 * draw an arrow at the given coords
 * See "draw_symbol" for a description of the arguments.
 */
int Skycat::draw_arrow(double x, double y, const char* xy_units, 
			double radius, const char* radius_units, 
			const char* bg, const char* fg, 
			const char* symbol_tags, double ratio, double angle,
			const char* label, const char* label_tags)
{
    double cx, cy, nx, ny, ex, ey;
    if (get_compass(x, y, xy_units, radius, radius_units, ratio, angle,
		    cx, cy, nx, ny, ex, ey) != TCL_OK) {
	reset_result(); // ignore off scale symbols
	return TCL_OK;
    }

    // if using 2 colors, draw 2 symbols, for visibility, one thicker
    std::ostringstream os;
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create line "
	   << cx << ' ' << cy << ' ' << nx << ' ' << ny
	   << " -fill " << bg
	   << " -width 2 -arrow last -tags " << "{" << symbol_tags << "}" 
	   << std::endl;
    }
    os << canvasName_ << " create line "
       << cx << ' ' << cy << ' ' << nx << ' ' << ny
       << " -fill " << fg
       << " -width 1 -arrow last -tags " << "{" << symbol_tags << "}" 
       << std::endl;
    
    if (label && strlen(label)) 
	make_label(os, label, cx, cy, label_tags, fg);
    
    return eval(os.str().c_str());
}

/*
 * Skycat symbol subcommand:
 * 
 * Usage: $instName symbol $shape $x $y $xy_units $radius $radius_units \
 *                  $bg $fg $symbol_tags ?$ratio $angle $label $label_tags?
 *
 * Draw a symbol on the image with the given shape at the given coordinates
 * (in the given x,y units), with the given radius (in radius_units), 
 * bg and fg color, canvas tags list, x/y ratio and rotation angle.
 *
 * shape may be one of "circle", "square", "plus", "cross", "triangle",
 * "diamond", "ellipse", "compass", "line", "arrow".
 *
 * x and y are the coordinates in "xy_units", which is one of the units
 * accepted by the Rtd commands (canvas, image, screen, "wcs $equinox", 
 * "deg $equinox").
 *
 * The radius value is interpreted in radius_units.
 * 
 * bg and fg are X color names for the symbol (may be the same).
 *
 * symbol_tags should be a Tcl list of canvas tags for the symbol.
 *
 * ratio and angle are optional and used to stretch/shrink and 
 * rotate the symbol. The default ratio is 1, default angle 0.
 *
 * label is an optional text for a label to place near the symbol.
 *
 * label_tags should be a Tcl list of canvas tags for the label, or
 * an empty or null string, if there is no label.
 *
 * Returns an error if the coordinates or part of the symbol are off 
 * the image.
 *
 * Uses world coordinates, if available, for the rotation and orientation, 
 * for symbols that support it (i.e.: rotation is relative to WCS north).
 */
int Skycat::symbolCmd(int argc, char* argv[])
{
    char* shape = argv[0];
    double x, y;
    char* xy_units = argv[3];
    double radius;
    char* radius_units = argv[5];
    char* bg = argv[6];
    char* fg = argv[7];
    char* symbol_tags = argv[8];
    double ratio = 1.;
    double angle = 0.;
    char* label = NULL;
    char* label_tags = NULL;

    if (Tcl_GetDouble(interp_, argv[1], &x) != TCL_OK 
	|| Tcl_GetDouble(interp_, argv[2], &y) != TCL_OK 
	|| Tcl_GetDouble(interp_, argv[4], &radius) != TCL_OK) {
	return TCL_ERROR;
    }
    
    if (argc >= 10) {
	if (Tcl_GetDouble(interp_, argv[9], &ratio) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    if (argc >= 11) {
	if (Tcl_GetDouble(interp_, argv[10], &angle) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    if (argc >= 12) {
	label = argv[11];
    }
    if (argc >= 13) {
	label = argv[12];
    }

    return draw_symbol(shape, x, y, xy_units, radius, radius_units,
		       bg, fg, symbol_tags, ratio, angle, label, label_tags);
}


