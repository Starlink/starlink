/*
 * E.S.O. - VLT project 
 * "@(#) $Id: Skycat.C,v 1.12 1998/11/16 21:26:58 abrighto Exp $"
 *
 * Skycat.C - Initialize Skycat package
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  21 Nov 97  Created
 *
 *                 10/03/98  Added optional args to constructor to allow derived
 *                           class to specify its own configuration options.
 */
static const char* const rcsId="@(#) $Id: Skycat.C,v 1.12 1998/11/16 21:26:58 abrighto Exp $";

#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <stdio.h>
#include <iostream.h>
#include <strstream.h>
#include <fstream.h>
#include <sys/types.h>
#include <new.h>
#include <unistd.h>
#include <math.h>
#include <assert.h>
#include <tcl.h>
#include <tk.h>
#include "error.h"
#include "define.h"
#include "util.h"
#include "config.h"
#include "SkySearch.h"
#include "TcsSkySearch.h"
#include "FitsIO.h"
#include "Skycat.h"

// generated code for bitmaps used in tcl scripts
void defineSkycatBitmaps(Tcl_Interp*);

// math constants
static const double pi_ = 3.14159265358979323846;
static const double rad_ = pi_/180.;

// size of buffer to use to hold tcl commands to evaluate
static const eval_buf_size_ = 4*1024;

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
    {"hdu",    &Skycat::hduCmd,     0,  2},
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
    (Tk_ImageType *) NULL	/* nextPtr */
};


/*
 * A call to this function is made from the tkAppInit file at startup
 * to initialize this package
 */
extern "C"
int Skycat_Init(Tcl_Interp* interp)  
{
    // set up Tcl package
    if (Tcl_PkgProvide (interp, "Skycat", SKYCAT_VERSION) != TCL_OK) {
	return TCL_ERROR;
    }

    // install the new, extended astrocat command 
    Tcl_CreateCommand(interp, "astrocat", SkySearch::astroCatCmd, NULL, NULL);
    
    // also extend the tcscat command
    Tcl_CreateCommand(interp, "tcscat", TcsSkySearch::tcsCatCmd, NULL, NULL);

    // define bitmaps used by Tcl library
    defineSkycatBitmaps(interp);

    // add the skycat image type
    Tk_CreateImageType(&skycatImageType);

    // The skycat_library path can be found in several places.  Here is the order
    // in which the are searched.
    //		1) the variable may already exist
    //		2) env array
    //		3) the compiled in value of SKYCAT_LIBRARY
    char* libDir = Tcl_GetVar(interp, "skycat_library", TCL_GLOBAL_ONLY);
    if (libDir == NULL) {
	libDir = Tcl_GetVar2(interp, "env", "SKYCAT_LIBRARY", TCL_GLOBAL_ONLY);
    }
    if (libDir == NULL) {
	libDir = SKYCAT_LIBRARY;
    }

    // Set the global Tcl variables skycat_library and skycat_version 
    // and add skycat_library to the auto_path.
    Tcl_SetVar(interp, "skycat_library", libDir, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "skycat_version", SKYCAT_VERSION, TCL_GLOBAL_ONLY);

    char cmd[1048];
    sprintf(cmd, "lappend auto_path %s", libDir);
    if (Tcl_Eval(interp, cmd) != TCL_OK)
	return TCL_ERROR;

    // set up the namespaces used by the itcl/itk classes
    if (Tcl_Eval(interp, 
#if (TCL_MAJOR_VERSION >= 8)
		 "namespace eval skycat {namespace export *};"
		 "namespace import -force skycat::*;"
#else
		 "namespace ::skycat {};"
		 "import add skycat;"
#endif
	) != 0)
	return TCL_ERROR;

    return TCL_OK; 
}


/*
 * This static method is called by the Tk image code to create
 * a new skycat image.
 */
int Skycat::CreateImage(
    Tcl_Interp *interp,		// Interpreter for application containing image.
    char *name,			// Name to use for image.
    int argc,			// Number of arguments.
    char **argv,		// Argument strings for options (not including image name or type)
    Tk_ImageType *typePtr,	// Pointer to our type record (not used). 
    Tk_ImageMaster master,	// Token for image, to be used by us in later callbacks.
    ClientData *clientDataPtr)	// Store manager's token (this ptr) for image here
				// it will be returned in later callbacks.
{
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
int Skycat::make_label(ostream& os, const char* label, double x, double y, 
		       const char* tags, const char* color, const char* font)
{
    os << canvasName_ << " create text "
       << x << ' ' << y
       << " -text {" << label 
       << "} -anchor sw -fill " << color
       << " -font " << font
       << " -tags " << "{" << tags << "}"
       << endl;
    
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
    char buf[eval_buf_size_];
    ostrstream os(buf, sizeof(buf));
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create oval "
	   << x0-1 << ' ' << y0-1 << ' ' << x1+1 << ' ' << y1+1
	   << " -outline " << bg
	   << " -fill " << bg
	   << " -width 1 -stipple pat7 -tags " << "{" << symbol_tags << "}" 
	   << endl;
    }
    os << canvasName_ << " create oval "
       << x0 << ' ' << y0 << ' ' << x1 << ' ' << y1
       << " -outline " << fg
       << " -fill " << bg
       << " -width 1 -stipple pat7 -tags " << "{" << symbol_tags << "}"
       << endl;

    if (label && strlen(label)) 
	make_label(os, label, x, y, label_tags, fg);
    
    os << ends;
    return eval(os.str());
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
    char buf[eval_buf_size_];
    ostrstream os(buf, sizeof(buf));
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create rect "
	   << x0-1 << ' ' << y0-1 << ' ' << x1+1 << ' ' << y1+1
	   << " -outline " << bg
	   << " -fill " << bg
	   << " -width 1 -stipple pat7 -tags " << "{" << symbol_tags << "}" 
	   << endl;
    }
    os << canvasName_ << " create rect "
       << x0 << ' ' << y0 << ' ' << x1 << ' ' << y1
       << " -outline " << fg
       << " -fill " << bg
       << " -width 1 -stipple pat7 -tags " << "{" << symbol_tags << "}"
       << endl;
    
    if (label && strlen(label)) 
	make_label(os, label, x, y, label_tags, fg);
    
    os << ends;
    return eval(os.str());
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
    char buf[eval_buf_size_];
    ostrstream os(buf, sizeof(buf));
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create line "
	   << x0 << ' ' << y0 << ' ' << x1 << ' ' << y1
	   << " -fill " << bg
	   << " -width 2 -tags " << "{" << symbol_tags << "}" 
	   << endl;
	os << canvasName_ << " create line "
	   << x1 << ' ' << y0 << ' ' << x0 << ' ' << y1
	   << " -fill " << bg
	   << " -width 2 -tags " << "{" << symbol_tags << "}" 
	   << endl;
    }
    os << canvasName_ << " create line "
       << x0 << ' ' << y0 << ' ' << x1 << ' ' << y1
       << " -fill " << fg
       << " -width 1 -tags " << "{" << symbol_tags << "}"
       << endl;
    os << canvasName_ << " create line "
       << x1 << ' ' << y0 << ' ' << x0 << ' ' << y1
       << " -fill " << fg
       << " -width 1 -tags " << "{" << symbol_tags << "}"
       << endl;
    
    if (label && strlen(label)) 
	make_label(os, label, x, y, label_tags, fg);
    
    os << ends;
    return eval(os.str());
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
    char buf[eval_buf_size_];
    ostrstream os(buf, sizeof(buf));
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create polygon "
	   << x0 << ' ' << y1 << ' ' << x << ' ' << y0 << ' ' << x1 << ' ' << y1
	   << " -outline " << bg
	   << " -fill " << bg
	   << " -width 2 -stipple pat7 -tags " << "{" << symbol_tags << "}" 
	   << endl;
    }
    os << canvasName_ << " create polygon "
       << x0 << ' ' << y1 << ' ' << x << ' ' << y0 << ' ' << x1 << ' ' << y1
       << " -outline " << fg
       << " -fill " << bg
       << " -width 1 -stipple pat7 -tags " << "{" << symbol_tags << "}" 
       << endl;
    
    if (label && strlen(label)) 
	make_label(os, label, x, y, label_tags, fg);
    
    os << ends;
    return eval(os.str());
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
    char buf[eval_buf_size_];
    ostrstream os(buf, sizeof(buf));
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create polygon "
	   << x0 << ' ' << y << ' ' << x << ' ' << y0 << ' ' << x1 << ' ' << y << ' ' << x << ' ' << y1
	   << " -outline " << bg
	   << " -fill " << bg
	   << " -width 2 -stipple pat7 -tags " << "{" << symbol_tags << "}" 
	   << endl;
    }
    os << canvasName_ << " create polygon "
       << x0 << ' ' << y << ' ' << x << ' ' << y0 << ' ' << x1 << ' ' << y << ' ' << x << ' ' << y1
       << " -outline " << fg
       << " -fill " << bg
       << " -width 1 -stipple pat7 -tags " << "{" << symbol_tags << "}" 
       << endl;
    
    if (label && strlen(label)) 
	make_label(os, label, x, y, label_tags, fg);
    
    os << ends;
    return eval(os.str());
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
    ostrstream os(buf, sizeof(buf));
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create line "
	   << nx << ' ' << ny << ' ' << sx << ' ' << sy
	   << " -fill " << bg
	   << " -width 2 -tags " << "{" << symbol_tags << "}" 
	   << endl;
	os << canvasName_ << " create line "
	   << ex << ' ' << ey << ' ' << wx << ' ' << wy
	   << " -fill " << bg
	   << " -width 2 -tags " << "{" << symbol_tags << "}" 
	   << endl;
    }
    os << canvasName_ << " create line "
       << nx << ' ' << ny << ' ' << sx << ' ' << sy
       << " -fill " << fg
       << " -width 1 -tags " << "{" << symbol_tags << "}" 
       << endl;
    os << canvasName_ << " create line "
       << ex << ' ' << ey << ' ' << wx << ' ' << wy
       << " -fill " << fg
       << " -width 1 -tags " << "{" << symbol_tags << "}" 
       << endl;
    
    if (label && strlen(label)) 
	make_label(os, label, cx, cy, label_tags, fg);
    
    os << ends;
    return eval(os.str());
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
    char buf[eval_buf_size_];
    ostrstream os(buf, sizeof(buf));
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create polygon "
	   << nx << ' ' << ny << ' ' << ex << ' ' << ey << ' ' 
	   << sx << ' ' << sy << ' ' << wx << ' ' << wy
	   << " -outline " << bg
	   << " -fill " << bg
	   << " -width 2  -smooth 1 -stipple pat7 -tags " << "{" << symbol_tags << "}" 
	   << endl;
    }
    os << canvasName_ << " create polygon "
       << nx << ' ' << ny << ' ' << ex << ' ' << ey << ' ' 
       << sx << ' ' << sy << ' ' << wx << ' ' << wy
       << " -outline " << bg
       << " -fill " << fg
       << " -width 1  -smooth 1 -stipple pat7 -tags " << "{" << symbol_tags << "}" 
       << endl;
    
    if (label && strlen(label)) 
	make_label(os, label, cx, cy, label_tags, fg);
    
    os << ends;
    return eval(os.str());
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
    char buf[eval_buf_size_];
    ostrstream os(buf, sizeof(buf));
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create line "
	   << cx << ' ' << cy << ' ' << nx << ' ' << ny
	   << " -fill " << bg
	   << " -width 2 -arrow last -tags " << "{" << symbol_tags << "}" 
	   << endl;
	os << canvasName_ << " create line "
	   << cx << ' ' << cy << ' ' << ex << ' ' << ey
	   << " -fill " << bg
	   << " -width 2 -tags " << "{" << symbol_tags << "}" 
	   << endl;
    }
    os << canvasName_ << " create line "
       << cx << ' ' << cy << ' ' << nx << ' ' << ny
       << " -fill " << fg
       << " -width 1 -arrow last -tags " << "{" << symbol_tags << "}" 
       << endl;
    os << canvasName_ << " create line "
       << cx << ' ' << cy << ' ' << ex << ' ' << ey
       << " -fill " << fg
       << " -width 1 -tags " << "{" << symbol_tags << "}" 
       << endl;
    
    if (label && strlen(label)) 
	make_label(os, label, cx, cy, label_tags, fg);
    
    os << ends;
    return eval(os.str());
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
    char buf[eval_buf_size_];
    ostrstream os(buf, sizeof(buf));
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create line "
	   << nx << ' ' << ny << ' ' << sx << ' ' << sy
	   << " -fill " << bg
	   << " -width 2 -tags " << "{" << symbol_tags << "}" 
	   << endl;
    }
    os << canvasName_ << " create line "
       << nx << ' ' << ny << ' ' << sx << ' ' << sy
       << " -fill " << fg
       << " -width 1 -tags " << "{" << symbol_tags << "}" 
       << endl;
    
    if (label && strlen(label)) 
	make_label(os, label, cx, cy, label_tags, fg);
    
    os << ends;
    return eval(os.str());
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
    char buf[eval_buf_size_];
    ostrstream os(buf, sizeof(buf));
    if (strcmp(fg, bg) != 0) {
	os << canvasName_ << " create line "
	   << cx << ' ' << cy << ' ' << nx << ' ' << ny
	   << " -fill " << bg
	   << " -width 2 -arrow last -tags " << "{" << symbol_tags << "}" 
	   << endl;
    }
    os << canvasName_ << " create line "
       << cx << ' ' << cy << ' ' << nx << ' ' << ny
       << " -fill " << fg
       << " -width 1 -arrow last -tags " << "{" << symbol_tags << "}" 
       << endl;
    
    if (label && strlen(label)) 
	make_label(os, label, cx, cy, label_tags, fg);
    
    os << ends;
    return eval(os.str());
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



/*
 * This method implements the "hdu" subcommand, to access different
 * FITS HDUs (header data units). Each HDU may be of type "image",
 * "binary" table or "ascii" table.
 *
 *  usage: <path> hdu count
 *  or:    <path> hdu list
 *  or:    <path> hdu listheadings
 *  or:    <path> hdu headings
 *  or:    <path> hdu get ?filename?
 *  or:    <path> hdu ?number?
 *
 * If the "hdu count" subcommand is specified, it returns the number of
 * HDUs in the current image.
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
 * in the current ASCII or binary table.
 *
 * The "hdu get" subcommand with no arguments returns the contents of the
 * current ASCII or binary table as a Tcl list (list of rows, where each
 * row is a list of column values). If a filename argument is given, the
 * FITS table is written to the given file in the form of a local (tab
 * separated) catalog.
 *
 * If the "hdu" subcommand is specified with no arguments, it returns the
 * current HDU number. If a number argument is given, the current HDU is
 * set to that number.
 *
 * An optional numerical argument may be passed to the "hdu" subcommand,
 * in which case the "current HDU" is set to the given number.
 */
int Skycat::hduCmd(int argc, char* argv[])
{
    if (!image_)
	return TCL_OK;

    // get a (reference counted) copy of the image
    ImageIO imio = image_->image();

    // HDU operations: make sure it is a FITS file
    if (!imio.rep() || strcmp(imio.rep()->classname(), "FitsIO") != 0)
	return error("The \"hdu\" subcommand is only supported for FITS files");
    FitsIO* fits = (FitsIO*)imio.rep();

    if (argc == 0) {
	// $image hdu: return the current HDU number
	return set_result(fits->getHDUNum());
    }	
	
    // must be: $image hdu $arg
    if (strcmp(argv[0], "count") == 0) {
	return set_result(fits->getNumHDUs());
    }	

    if (strcmp(argv[0], "listheadings") == 0) {	
	// return a list of table headings matching the "hdu list" output
	return set_result("HDU Type ExtName NAXIS NAXIS1 NAXIS2 NAXIS3 CRPIX1 CRPIX2");
    } 

    if (strcmp(argv[0], "headings") == 0) {	
	// return a list of table headings for the current FITS table
	const char* type = fits->getHDUType();
	if (!type || *type == 'i')
	    return error("current HDU is not a FITS table");
	long nrows = 0;
	int ncols = 0;
	if (fits->getTableDims(nrows, ncols) != 0)
	    return TCL_ERROR;
	reset_result();
	for(int col = 1; col <= ncols; col++) {
	    char* s = fits->getTableHead(col);
	    if (!s)
		return TCL_ERROR;
	    append_element(s);
	}
	return TCL_OK;
    } 

    if (strcmp(argv[0], "get") == 0) {

	const char* type = fits->getHDUType();
	if (!type || *type == 'i')
	    return error("current HDU is not a FITS table");

	long nrows = 0;
	int ncols = 0;
	if (fits->getTableDims(nrows, ncols) != 0)
	    return TCL_ERROR;

	if (argc == 1) {
	    // return the contents of the table as a tcl list of rows
	    reset_result();
	    for(int row = 1; row <= nrows; row++) {
		append_result(" {");
		for(int col = 1; col <= ncols; col++) {
		    char* s = fits->getTableValue(row, col);
		    if (!s)
			return TCL_ERROR;
		    append_element(s);
		}
		append_result("}");
	    }
	    return TCL_OK;
	}

	if (argc == 2) {
	    // write the contents of the table to a local catalog file
	    char* filename = argv[1];
	    ofstream os(filename);
	    if (! os)
		return sys_error("can't open file: ", filename);

	    // output the table header
	    os << "QueryResult\n\n"
	       << "# Config entry\n"
	       << "serv_type: local\n"
	       << "long_name: " << filename << "\n"
	       << "short_name: " << fileBasename(filename) << "\n"
	       << "url: " << filename << "\n"
	       << "ra_col: -1\n"
	       << "dec_col: -1\n"
	       << "x_col: -1\n"
	       << "y_col: -1\n"
	       << endl;
	    
	    // output the column headings
	    int col;
	    for(col = 1; col <= ncols; col++) {
		char* s = fits->getTableHead(col);
		if (!s)
		    return TCL_ERROR;
		os << s;
		if (col < ncols)
		    os << '\t';
	    }
	    os << "\n---\n";	// heading separator (dashed line)

	    // output the data
	    for(long row = 1; row <= nrows; row++) {
		for(col = 1; col <= ncols; col++) {
		    char* s = fits->getTableValue(row, col);
		    if (!s)
			return TCL_ERROR;
		    os << s;
		    if (col < ncols)
			os << '\t';
		}
		os << endl;
	    }
	    return TCL_OK;
	}
	return error("hdu get: wrong number of args");
    }

    if (strcmp(argv[0], "list") == 0) {	
	// return a list of HDUs
	int numHDUs = fits->getNumHDUs();
	if (numHDUs <= 0)
	    return TCL_OK;	// empty return list

	// save current HDU, then loop through all HDUs to get info
	int curHDU = fits->getHDUNum();
	ostrstream os;
	int status = 0;
	int count = 0;
	for (int i = 1; i <= numHDUs; i++) {
	    if (fits->setHDU(i) != 0) {
		status++;
		break;
	    }
	    const char* type = fits->getHDUType();
	    if (!type) {
		status++;
		break;
	    }

	    // get these keyword values and default to ""
	    char extName[80], naxis[32], naxis1[32], naxis2[32], naxis3[32];
	    char crpix1[32], crpix2[32]; 
	    fits->get("EXTNAME", extName, sizeof(extName));
	    fits->get("NAXIS", naxis, sizeof(naxis));
	    fits->get("NAXIS1", naxis1, sizeof(naxis1));
	    fits->get("NAXIS2", naxis2, sizeof(naxis2));
	    fits->get("NAXIS3", naxis3, sizeof(naxis3));
	    fits->get("CRPIX1", crpix1, sizeof(crpix1));
	    fits->get("CRPIX2", crpix2, sizeof(crpix2));

	    os << "{" 
	       << i 
	       << " " << type 
	       << " {" << extName << "}"
	       << " {" << naxis << "}"
	       << " {" << naxis1 << "}"
	       << " {" << naxis2 << "}"
	       << " {" << naxis3 << "}"
	       << " {" << crpix1 << "}"
	       << " {" << crpix2 << "}"
	       << "} ";
	    count++;
	}
	if (count) {
	    if (status == TCL_OK) {
		os << ends;
		set_result(os.str());
	    }
	    delete os.str();
	    fits->setHDU(curHDU);
	}
	return status;
    }
	
    if (strcmp(argv[0], "mkcat") == 0) {	
	// make a skycat local catalog file from an ASCII or binary table
	return TCL_OK;
    } 

    // must be: "$image hdu $num": Set the current HDU
    int num = 0;
    if (Tcl_GetInt(interp_, argv[0], &num) != TCL_OK
	|| fits->setHDU(num) != 0)
	return TCL_ERROR;
    
    const char* hduType = fits->getHDUType();
    if (!hduType)
	return TCL_ERROR;
    
    if (*hduType != 'i')
	return TCL_OK;		// FITS table, not image: don't display 

    // save image transformation parameters to restore later
    ImageDataParams p;
    image_->saveParams(p);

    // delete old image
    delete image_; 
    image_ = NULL;
    updateViews();
	
    // Re-initialize the image from the given HDU
    ImageData* im = makeImage(imio);
    if (! im)
	return TCL_ERROR;
    image_ = im;

    // The WCS info will be different in this HDU
    imio.wcsinit();

    // restore transformations
    image_->restoreParams(p, !autoSetCutLevels_);

    // update the display
    return initNewImage();
}

