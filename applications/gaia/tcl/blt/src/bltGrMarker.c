
/*
 * bltGrMarker.c --
 *
 *	This module implements markers for the BLT graph widget.
 *
 * Copyright 1993-1998 Lucent Technologies, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the names
 * of Lucent Technologies any of their entities not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 *
 * Lucent Technologies disclaims all warranties with regard to this
 * software, including all implied warranties of merchantability and
 * fitness.  In no event shall Lucent Technologies be liable for any
 * special, indirect or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits, whether in
 * an action of contract, negligence or other tortuous action, arising
 * out of or in connection with the use or performance of this
 * software.
 */

#include "bltGraph.h"
#include "bltGrElem.h"
#include <ctype.h>

static int StringToCoordinates _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int offset));
static char *CoordinatesToString _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption coordsOption =
{
    StringToCoordinates, CoordinatesToString, (ClientData)0
};

extern Tk_CustomOption bltColorPairOption;
extern Tk_CustomOption bltDashesOption;
extern Tk_CustomOption bltLengthOption;
extern Tk_CustomOption bltListOption;
extern Tk_CustomOption bltPadOption;
extern Tk_CustomOption bltShadowOption;
extern Tk_CustomOption bltXAxisOption;
extern Tk_CustomOption bltYAxisOption;
extern Tk_CustomOption bltUidOption;

/*
 * Marker types:
 *
 * 	A marker can be either a text, bitmap, image, line, polygon,
 *	or window.
 */

static char *markerClasses[] =
{
    "BitmapMarker",
    "ImageMarker",
    "LineMarker",
    "PolygonMarker",
    "TextMarker",
    "WindowMarker"
};

/*
 * This structure corresponds with the specific types of markers.
 * Don't change this structure without changing the individual
 * marker structures of each type below.
 */

typedef void (MarkerDrawProc) _ANSI_ARGS_((Marker *markerPtr, Drawable drawable));
typedef void (MarkerFreeProc) _ANSI_ARGS_((Graph *graphPtr, Marker *markerPtr));
typedef int (MarkerConfigProc) _ANSI_ARGS_((Marker *markerPtr));
typedef void (MarkerCoordsProc) _ANSI_ARGS_((Marker *markerPtr));
typedef void (MarkerPrintProc) _ANSI_ARGS_((Marker *markerPtr,
	Printable printable));
typedef int (MarkerPointProc) _ANSI_ARGS_((Marker *markerPtr, int x, int y));

/*
 * -------------------------------------------------------------------
 *
 * Marker --
 *
 *	Structure defining the generic marker.  In C++ parlance
 *	this would be the base type from which all markers are
 *	derived.
 *
 * -------------------------------------------------------------------
 */
struct Marker {
    Tk_Uid nameId;		/* Identifier for marker in list */
    ObjectType type;		/* Type of marker. */
    Graph *graphPtr;		/* Graph widget of marker. */
    unsigned int flags;
    char **tags;
    int hidden;			/* If non-zero, don't display the marker. */

    Tcl_HashEntry *hashPtr;
    Blt_ListItem item;
    Point2D *coordArr;		/* Coordinate array to position marker */
    int numCoords;		/* Number of points in above array */
    Tk_ConfigSpec *configSpecs;	/* Marker configuration specifications */
    Tk_Uid elemId;		/* Element associated with marker */
    Axis2D axes;

    int drawUnder;		/* If non-zero, draw the marker underneath
				 * any elements. This can be a performance
				 * penalty because the graph must be redraw
				 * entirely each time the marker is
				 * redrawn. */
    int clipped;		/* Indicates if the marker is totally clipped
				 * by the plotting area. */
    int xOffset, yOffset;	/* Pixel offset from graph position */

    MarkerDrawProc *drawProc;
    MarkerFreeProc *freeProc;
    MarkerConfigProc *configProc;
    MarkerCoordsProc *coordsProc;
    MarkerPrintProc *printProc;
    MarkerPointProc *pointProc;
};

#define DEF_MARKER_ANCHOR	"center"
#define DEF_MARKER_BG_COLOR	RGB_COLOR_WHITE
#define DEF_MARKER_BG_MONO	RGB_COLOR_WHITE
#define DEF_MARKER_BITMAP	(char *)NULL
#define DEF_MARKER_CAP_STYLE	"butt"
#define DEF_MARKER_COORDS	(char *)NULL
#define DEF_MARKER_DASHES	(char *)NULL
#define DEF_MARKER_DASH_OFFSET	"0"
#define DEF_MARKER_ELEMENT	(char *)NULL
#define DEF_MARKER_FG_COLOR	RGB_COLOR_BLACK
#define DEF_MARKER_FG_MONO	RGB_COLOR_BLACK
#define DEF_MARKER_FILL_COLOR	RGB_COLOR_RED
#define DEF_MARKER_FILL_MONO	RGB_COLOR_WHITE
#define DEF_MARKER_FONT		STD_FONT
#define DEF_MARKER_GAP_COLOR	RGB_COLOR_PINK
#define DEF_MARKER_GAP_MONO	RGB_COLOR_BLACK
#define DEF_MARKER_HEIGHT	"0"
#define DEF_MARKER_HIDE		"no"
#define DEF_MARKER_JOIN_STYLE	"miter"
#define DEF_MARKER_JUSTIFY	"left"
#define DEF_MARKER_LINE_WIDTH	"1"
#define DEF_MARKER_MAP_X	"x"
#define DEF_MARKER_MAP_Y	"y"
#define DEF_MARKER_NAME		(char *)NULL
#define DEF_MARKER_OUTLINE_COLOR RGB_COLOR_BLACK
#define DEF_MARKER_OUTLINE_MONO	RGB_COLOR_BLACK
#define DEF_MARKER_PAD		"4"
#define DEF_MARKER_ROTATE	"0.0"
#define DEF_MARKER_SCALE	"1.0"
#define DEF_MARKER_SHADOW_COLOR	(char *)NULL
#define DEF_MARKER_SHADOW_MONO	(char *)NULL
#define DEF_MARKER_STIPPLE	(char *)NULL
#define DEF_MARKER_TEXT		(char *)NULL
#define DEF_MARKER_UNDER	"no"
#define DEF_MARKER_WIDTH	"0"
#define DEF_MARKER_WINDOW	(char *)NULL
#define DEF_MARKER_XOR		"no"
#define DEF_MARKER_X_OFFSET	"0"
#define DEF_MARKER_Y_OFFSET	"0"

#define DEF_MARKER_TEXT_TAGS	"Text all"
#define DEF_MARKER_IMAGE_TAGS	"Image all"
#define DEF_MARKER_BITMAP_TAGS	"Bitmap all"
#define DEF_MARKER_WINDOW_TAGS	"Window all"
#define DEF_MARKER_POLYGON_TAGS	"Polygon all"
#define DEF_MARKER_LINE_TAGS	"Line all"

/*
 * -------------------------------------------------------------------
 *
 * TextMarker --
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    Tk_Uid nameId;		/* Identifier for marker */
    ObjectType type;		/* Type of marker */
    Graph *graphPtr;		/* The graph this marker belongs to */
    unsigned int flags;
    char **tags;
    int hidden;			/* If non-zero, don't display the marker. */

    Tcl_HashEntry *hashPtr;
    Blt_ListItem item;
    Point2D *coordArr;		/* Coordinate array to position marker */
    int numCoords;		/* Number of points */
    Tk_ConfigSpec *configSpecs;	/* Configuration specifications */
    Tk_Uid elemId;		/* Element associated with marker */
    Axis2D axes;
    int drawUnder;		/* If non-zero, draw the marker underneath
				 * any elements. There can be a performance
				 * because the graph must be redraw entirely
				 * each time this marker is redrawn. */
    int clipped;		/* Indicates if the marker is totally clipped
				 * by the plotting area. */
    int xOffset, yOffset;	/* pixel offset from anchor */

    MarkerDrawProc *drawProc;
    MarkerFreeProc *freeProc;
    MarkerConfigProc *configProc;
    MarkerCoordsProc *coordsProc;
    MarkerPrintProc *printProc;
    MarkerPointProc *pointProc;

    /*
     * Text specific fields and attributes
     */
#ifdef notdef
    char *textVarName;		/* Name of variable (malloc'ed) or NULL. If
				 * non-NULL, graph displays the contents of
				 * this variable. */
#endif
    char *string;
    int x, y;
    XColor *fillColor;
    GC fillGC;
    TextAttributes attrs;	/* Text attributes (font, fg, anchor, etc) */
    Tk_Anchor anchor;
    int width, height;
} TextMarker;


static Tk_ConfigSpec textConfigSpecs[] =
{
    {TK_CONFIG_ANCHOR, "-anchor", "anchor", "Anchor",
	DEF_MARKER_ANCHOR, Tk_Offset(TextMarker, anchor), 0},
    {TK_CONFIG_COLOR, "-background", "background", "MarkerBackground",
	(char *)NULL, Tk_Offset(TextMarker, fillColor),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_SYNONYM, "-bg", "background", "Background",
	(char *)NULL, 0, 0},
    {TK_CONFIG_CUSTOM, "-bindtags", "bindTags", "BindTags",
	DEF_MARKER_TEXT_TAGS, Tk_Offset(Marker, tags),
	TK_CONFIG_NULL_OK, &bltListOption},
    {TK_CONFIG_CUSTOM, "-coords", "coords", "Coords",
	DEF_MARKER_COORDS, Tk_Offset(Marker, coordArr),
	TK_CONFIG_NULL_OK, &coordsOption},
    {TK_CONFIG_CUSTOM, "-element", "element", "Element",
	DEF_MARKER_ELEMENT, Tk_Offset(Marker, elemId), 
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_SYNONYM, "-fg", "foreground", "Foreground",
	(char *)NULL, 0, 0},
    {TK_CONFIG_SYNONYM, "-fill", "background", (char *)NULL,
	(char *)NULL, 0, 0},
    {TK_CONFIG_FONT, "-font", "font", "Font",
	DEF_MARKER_FONT, Tk_Offset(TextMarker, attrs.font), 0},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_MARKER_FG_COLOR, Tk_Offset(TextMarker, attrs.color),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_MARKER_FG_MONO, Tk_Offset(TextMarker, attrs.color),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_JUSTIFY, "-justify", "justify", "Justify",
	DEF_MARKER_JUSTIFY, Tk_Offset(TextMarker, attrs.justify),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BOOLEAN, "-hide", "hide", "Hide",
	DEF_MARKER_HIDE, Tk_Offset(Marker, hidden),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-mapx", "mapX", "MapX",
	DEF_MARKER_MAP_X, Tk_Offset(Marker, axes.x), 0, &bltXAxisOption},
    {TK_CONFIG_CUSTOM, "-mapy", "mapY", "MapY",
	DEF_MARKER_MAP_Y, Tk_Offset(Marker, axes.y), 0, &bltYAxisOption},
    {TK_CONFIG_CUSTOM, "-name", (char *)NULL, (char *)NULL,
	DEF_MARKER_NAME, Tk_Offset(Marker, nameId), 
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_SYNONYM, "-outline", "foreground", (char *)NULL,
	(char *)NULL, 0, 0},
    {TK_CONFIG_CUSTOM, "-padx", "padX", "PadX",
	DEF_MARKER_PAD, Tk_Offset(TextMarker, attrs.padX),
	TK_CONFIG_DONT_SET_DEFAULT, &bltPadOption},
    {TK_CONFIG_CUSTOM, "-pady", "padY", "PadY",
	DEF_MARKER_PAD, Tk_Offset(TextMarker, attrs.padY),
	TK_CONFIG_DONT_SET_DEFAULT, &bltPadOption},
    {TK_CONFIG_DOUBLE, "-rotate", "rotate", "Rotate",
	DEF_MARKER_ROTATE, Tk_Offset(TextMarker, attrs.theta),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-shadow", "shadow", "Shadow",
	DEF_MARKER_SHADOW_COLOR, Tk_Offset(TextMarker, attrs.shadow),
	TK_CONFIG_COLOR_ONLY, &bltShadowOption},
    {TK_CONFIG_CUSTOM, "-shadow", "shadow", "Shadow",
	DEF_MARKER_SHADOW_MONO, Tk_Offset(TextMarker, attrs.shadow),
	TK_CONFIG_MONO_ONLY, &bltShadowOption},
    {TK_CONFIG_STRING, "-text", "text", "Text",
	DEF_MARKER_TEXT, Tk_Offset(TextMarker, string), TK_CONFIG_NULL_OK},
    {TK_CONFIG_BOOLEAN, "-under", "under", "Under",
	DEF_MARKER_UNDER, Tk_Offset(Marker, drawUnder),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-xoffset", "xOffset", "XOffset",
	DEF_MARKER_X_OFFSET, Tk_Offset(Marker, xOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-yoffset", "yOffset", "YOffset",
	DEF_MARKER_Y_OFFSET, Tk_Offset(Marker, yOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};


/*
 * -------------------------------------------------------------------
 *
 * WindowMarker --
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    Tk_Uid nameId;		/* Identifier for marker */
    ObjectType type;		/* Type of marker */
    Graph *graphPtr;		/* Graph marker belongs to */
    unsigned int flags;
    char **tags;
    int hidden;			/* Indicates if the marker is currently
				 * hidden or not. */

    Tcl_HashEntry *hashPtr;
    Blt_ListItem item;
    Point2D *coordArr;		/* Coordinate array to position marker */
    int numCoords;		/* Number of points */
    Tk_ConfigSpec *configSpecs;	/* Configuration specifications */
    Tk_Uid elemId;		/* Element associated with marker */
    Axis2D axes;
    int drawUnder;		/* If non-zero, draw the marker underneath
				 * any elements. There can be a performance
				 * because the graph must be redraw entirely
				 * each time this marker is redrawn. */
    int clipped;		/* Indicates if the marker is totally clipped
				 * by the plotting area. */
    int xOffset, yOffset;	/* Pixel offset from anchor */

    MarkerDrawProc *drawProc;
    MarkerFreeProc *freeProc;
    MarkerConfigProc *configProc;
    MarkerCoordsProc *coordsProc;
    MarkerPrintProc *printProc;
    MarkerPointProc *pointProc;

    /*
     * Window specific attributes
     */
    char *pathName;		/* Name of child window to be displayed */
    Tk_Window tkwin;		/* Window to display */
    int reqWidth, reqHeight;	/* Requested window extents */
    int width, height;		/* Actual window extents */
    Tk_Anchor anchor;		/* Anchor */
    int x, y;

} WindowMarker;

static Tk_ConfigSpec windowConfigSpecs[] =
{
    {TK_CONFIG_ANCHOR, "-anchor", "anchor", "Anchor",
	DEF_MARKER_ANCHOR, Tk_Offset(WindowMarker, anchor), 0},
    {TK_CONFIG_CUSTOM, "-bindtags", "bindTags", "BindTags",
	DEF_MARKER_WINDOW_TAGS, Tk_Offset(Marker, tags),
	TK_CONFIG_NULL_OK, &bltListOption},
    {TK_CONFIG_CUSTOM, "-coords", "coords", "Coords",
	DEF_MARKER_COORDS, Tk_Offset(WindowMarker, coordArr),
	TK_CONFIG_NULL_OK, &coordsOption},
    {TK_CONFIG_CUSTOM, "-element", "element", "Element",
	DEF_MARKER_ELEMENT, Tk_Offset(Marker, elemId),
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_CUSTOM, "-height", "height", "Height",
	DEF_MARKER_HEIGHT, Tk_Offset(WindowMarker, reqHeight),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_BOOLEAN, "-hide", "hide", "Hide",
	DEF_MARKER_HIDE, Tk_Offset(Marker, hidden),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-mapx", "mapX", "MapX",
	DEF_MARKER_MAP_X, Tk_Offset(Marker, axes.x), 0, &bltXAxisOption},
    {TK_CONFIG_CUSTOM, "-mapy", "mapY", "MapY",
	DEF_MARKER_MAP_Y, Tk_Offset(Marker, axes.y), 0, &bltYAxisOption},
    {TK_CONFIG_CUSTOM, "-name", (char *)NULL, (char *)NULL,
	DEF_MARKER_NAME, Tk_Offset(Marker, nameId), 
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_BOOLEAN, "-under", "under", "Under",
	DEF_MARKER_UNDER, Tk_Offset(Marker, drawUnder),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-width", "width", "Width",
	DEF_MARKER_WIDTH, Tk_Offset(WindowMarker, reqWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_STRING, "-window", "window", "Window",
	DEF_MARKER_WINDOW, Tk_Offset(WindowMarker, pathName),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_PIXELS, "-xoffset", "xOffset", "XOffset",
	DEF_MARKER_X_OFFSET, Tk_Offset(Marker, xOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-yoffset", "yOffset", "YOffset",
	DEF_MARKER_Y_OFFSET, Tk_Offset(Marker, yOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

/*
 * -------------------------------------------------------------------
 *
 * BitmapMarker --
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    Tk_Uid nameId;		/* Identifier for marker */
    ObjectType type;		/* Type of marker */
    Graph *graphPtr;		/* Graph marker belongs to */
    unsigned int flags;
    char **tags;
    int hidden;			/* Indicates if the marker is currently
				 * hidden or not. */

    Tcl_HashEntry *hashPtr;
    Blt_ListItem item;
    Point2D *coordArr;		/* Coordinate array to position marker */
    int numCoords;		/* Number of points */
    Tk_ConfigSpec *configSpecs;	/* Configuration specifications */
    Tk_Uid elemId;		/* Element associated with marker */
    Axis2D axes;
    int drawUnder;		/* If non-zero, draw the marker underneath
				 * any elements. There can be a performance
				 * because the graph must be redraw entirely
				 * each time this marker is redrawn. */
    int clipped;		/* Indicates if the marker is totally clipped
				 * by the plotting area. */
    int xOffset, yOffset;	/* Pixel offset from origin of bitmap */

    MarkerDrawProc *drawProc;
    MarkerFreeProc *freeProc;
    MarkerConfigProc *configProc;
    MarkerCoordsProc *coordsProc;
    MarkerPrintProc *printProc;
    MarkerPointProc *pointProc;

    /*
     * Bitmap specific attributes
     */
    Pixmap bitmap;		/* Original bitmap. May be further scaled or
				 * rotated */
    double rotate;		/* Requested rotation of the bitmap */
    double theta;		/* Normalized rotation (0..360 degrees) */
    XColor *fgColor;		/* foreground color */
    XColor *bgColor;		/* background color */

    GC gc;			/* Private graphic context */
    GC fillGC;			/* Shared graphic context */
    Tk_Anchor anchor;		/* Anchor */
    int x, y;			/* Origin of the bitmap */
    Pixmap transBitmap;		/* Transformed bitmap (rotated or scaled) */
    Pixmap draw;		/* Bitmap to be drawn */
    int width, height;		/* Final dimensions of the bitmap */
    XPoint pointArr[4];		/* Polygon forming the background of the
				 * bitmap. It's used to draw the background
				 * of rotated bitmaps.  We also use it to print
				 * a background in PostScript. */
} BitmapMarker;

static Tk_ConfigSpec bitmapConfigSpecs[] =
{
    {TK_CONFIG_ANCHOR, "-anchor", "anchor", "Anchor",
	DEF_MARKER_ANCHOR, Tk_Offset(BitmapMarker, anchor), 0},
    {TK_CONFIG_COLOR, "-background", "background", "Background",
	DEF_MARKER_BG_COLOR, Tk_Offset(BitmapMarker, bgColor),
	TK_CONFIG_COLOR_ONLY | TK_CONFIG_NULL_OK},
    {TK_CONFIG_COLOR, "-background", "background", "Background",
	DEF_MARKER_BG_MONO, Tk_Offset(BitmapMarker, bgColor),
	TK_CONFIG_MONO_ONLY | TK_CONFIG_NULL_OK},
    {TK_CONFIG_SYNONYM, "-bg", "background", (char *)NULL,
	(char *)NULL, 0, 0},
    {TK_CONFIG_CUSTOM, "-bindtags", "bindTags", "BindTags",
	DEF_MARKER_BITMAP_TAGS, Tk_Offset(Marker, tags),
	TK_CONFIG_NULL_OK, &bltListOption},
    {TK_CONFIG_BITMAP, "-bitmap", "bitmap", "Bitmap",
	DEF_MARKER_BITMAP, Tk_Offset(BitmapMarker, bitmap), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-coords", "coords", "Coords",
	DEF_MARKER_COORDS, Tk_Offset(Marker, coordArr),
	TK_CONFIG_NULL_OK, &coordsOption},
    {TK_CONFIG_CUSTOM, "-element", "element", "Element",
	DEF_MARKER_ELEMENT, Tk_Offset(Marker, elemId),
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *)NULL,
	(char *)NULL, 0, 0},
    {TK_CONFIG_SYNONYM, "-fill", "background", (char *)NULL,
	(char *)NULL, 0, 0},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_MARKER_FG_COLOR, Tk_Offset(BitmapMarker, fgColor),
	TK_CONFIG_COLOR_ONLY | TK_CONFIG_NULL_OK},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_MARKER_FG_MONO, Tk_Offset(BitmapMarker, fgColor),
	TK_CONFIG_MONO_ONLY | TK_CONFIG_NULL_OK},
    {TK_CONFIG_BOOLEAN, "-hide", "hide", "Hide",
	DEF_MARKER_HIDE, Tk_Offset(Marker, hidden),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-mapx", "mapX", "MapX",
	DEF_MARKER_MAP_X, Tk_Offset(Marker, axes.x), 0, &bltXAxisOption},
    {TK_CONFIG_CUSTOM, "-mapy", "mapY", "MapY",
	DEF_MARKER_MAP_Y, Tk_Offset(Marker, axes.y), 0, &bltYAxisOption},
    {TK_CONFIG_CUSTOM, "-name", (char *)NULL, (char *)NULL,
	DEF_MARKER_NAME, Tk_Offset(Marker, nameId), 
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_SYNONYM, "-outline", "foreground", (char *)NULL,
	(char *)NULL, 0, 0},
    {TK_CONFIG_DOUBLE, "-rotate", "rotate", "Rotate",
	DEF_MARKER_ROTATE, Tk_Offset(BitmapMarker, rotate),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BOOLEAN, "-under", "under", "Under",
	DEF_MARKER_UNDER, Tk_Offset(Marker, drawUnder),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-xoffset", "xOffset", "XOffset",
	DEF_MARKER_X_OFFSET, Tk_Offset(Marker, xOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-yoffset", "yOffset", "YOffset",
	DEF_MARKER_Y_OFFSET, Tk_Offset(Marker, yOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};


/*
 * -------------------------------------------------------------------
 *
 * ImageMarker --
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    Tk_Uid nameId;		/* Identifier for marker */
    ObjectType type;		/* Type of marker */
    Graph *graphPtr;		/* Graph marker belongs to */
    unsigned int flags;
    char **tags;
    int hidden;			/* Indicates if the marker is currently
				 * hidden or not. */

    Tcl_HashEntry *hashPtr;
    Blt_ListItem item;
    Point2D *coordArr;		/* Coordinate array to position marker */
    int numCoords;		/* Number of points */
    Tk_ConfigSpec *configSpecs;	/* Configuration specifications */
    Tk_Uid elemId;		/* Element associated with marker */
    Axis2D axes;
    int drawUnder;		/* If non-zero, draw the marker underneath
				 * any elements. There can be a performance
				 * because the graph must be redraw entirely
				 * each time this marker is redrawn. */
    int clipped;		/* Indicates if the marker is totally clipped
				 * by the plotting area. */
    int xOffset, yOffset;	/* Pixel offset from anchor */

    MarkerDrawProc *drawProc;
    MarkerFreeProc *freeProc;
    MarkerConfigProc *configProc;
    MarkerCoordsProc *coordsProc;
    MarkerPrintProc *printProc;
    MarkerPointProc *pointProc;

    /*
     * Image specific attributes
     */
    char *imageName;		/* Name of image to be displayed */
    Tk_Image tkImage;		/* Image to be displayed */
    Tk_Anchor anchor;		/* Anchor */
    int x, y;			/* Window x,y position of the image */
    int width, height;		/* Zoomed dimensions of the bitmap */
    Tk_PhotoHandle photo;
    Pixmap pixmap;		/* Pixmap of zoomed image */
    ColorTable colorTable;	/* Pointer to color table */
    ColorImage srcImage, destImage;
    GC gc;
} ImageMarker;

static Tk_ConfigSpec imageConfigSpecs[] =
{
    {TK_CONFIG_ANCHOR, "-anchor", "anchor", "Anchor",
	DEF_MARKER_ANCHOR, Tk_Offset(ImageMarker, anchor), 0},
    {TK_CONFIG_CUSTOM, "-bindtags", "bindTags", "BindTags",
	DEF_MARKER_IMAGE_TAGS, Tk_Offset(Marker, tags),
	TK_CONFIG_NULL_OK, &bltListOption},
    {TK_CONFIG_CUSTOM, "-coords", "coords", "Coords",
	DEF_MARKER_COORDS, Tk_Offset(Marker, coordArr),
	TK_CONFIG_NULL_OK, &coordsOption},
    {TK_CONFIG_CUSTOM, "-element", "element", "Element",
	DEF_MARKER_ELEMENT, Tk_Offset(Marker, elemId),
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_BOOLEAN, "-hide", "hide", "Hide",
	DEF_MARKER_HIDE, Tk_Offset(Marker, hidden),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_STRING, "-image", "image", "Image",
	(char *)NULL, Tk_Offset(ImageMarker, imageName), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-mapx", "mapX", "MapX",
	DEF_MARKER_MAP_X, Tk_Offset(Marker, axes.x), 0, &bltXAxisOption},
    {TK_CONFIG_CUSTOM, "-mapy", "mapY", "MapY",
	DEF_MARKER_MAP_Y, Tk_Offset(Marker, axes.y), 0, &bltYAxisOption},
    {TK_CONFIG_CUSTOM, "-name", (char *)NULL, (char *)NULL,
	DEF_MARKER_NAME, Tk_Offset(Marker, nameId), 
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_BOOLEAN, "-under", "under", "Under",
	DEF_MARKER_UNDER, Tk_Offset(Marker, drawUnder),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-xoffset", "xOffset", "XOffset",
	DEF_MARKER_X_OFFSET, Tk_Offset(Marker, xOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-yoffset", "yOffset", "YOffset",
	DEF_MARKER_Y_OFFSET, Tk_Offset(Marker, yOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

/*
 * -------------------------------------------------------------------
 *
 * LineMarker --
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    Tk_Uid nameId;		/* Identifier for marker */
    ObjectType type;		/* Type is TYPE_MARKER_LINE */
    Graph *graphPtr;		/* Graph marker belongs to */
    unsigned int flags;
    char **tags;
    int hidden;			/* Indicates if the marker is currently
				 * hidden or not. */

    Tcl_HashEntry *hashPtr;
    Blt_ListItem item;
    Point2D *coordArr;		/* Coordinate array to position marker */
    int numCoords;		/* Number of points */
    Tk_ConfigSpec *configSpecs;	/* Configuration specifications */
    Tk_Uid elemId;		/* Element associated with marker */
    Axis2D axes;
    int drawUnder;		/* If non-zero, draw the marker underneath
				 * any elements. There can be a performance
				 * because the graph must be redraw entirely
				 * each time this marker is redrawn. */
    int clipped;		/* Indicates if the marker is totally clipped
				 * by the plotting area. */
    int xOffset, yOffset;	/* Pixel offset */

    MarkerDrawProc *drawProc;
    MarkerFreeProc *freeProc;
    MarkerConfigProc *configProc;
    MarkerCoordsProc *coordsProc;
    MarkerPrintProc *printProc;
    MarkerPointProc *pointProc;

    /*
     * Line specific attributes
     */
    XColor *fillColor;
    XColor *outlineColor;	/* Foreground and background colors */
    int lineWidth;		/* line width */
    int capStyle;
    int joinStyle;
    Dashes dashes;		/* Dash list values (max 11) */
    GC gc;			/* Private graphic context */
    XSegment *segArr;
    int numSegments;

    int xor;
    int xorState;		/* State of the XOR drawing. Indicates
				 * if the marker is current drawn on not. */
} LineMarker;

static Tk_ConfigSpec lineConfigSpecs[] =
{
    {TK_CONFIG_CUSTOM, "-bindtags", "bindTags", "BindTags",
	DEF_MARKER_LINE_TAGS, Tk_Offset(Marker, tags),
	TK_CONFIG_NULL_OK, &bltListOption},
    {TK_CONFIG_CAP_STYLE, "-cap", "cap", "Cap",
	DEF_MARKER_CAP_STYLE, Tk_Offset(LineMarker, capStyle),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-coords", "coords", "Coords",
	DEF_MARKER_COORDS, Tk_Offset(Marker, coordArr),
	TK_CONFIG_NULL_OK, &coordsOption},
    {TK_CONFIG_CUSTOM, "-dashes", "dashes", "Dashes",
	DEF_MARKER_DASHES, Tk_Offset(LineMarker, dashes),
	TK_CONFIG_NULL_OK, &bltDashesOption},
    {TK_CONFIG_CUSTOM, "-dashoffset", "dashOffset", "DashOffset",
	DEF_MARKER_DASH_OFFSET, Tk_Offset(LineMarker, dashes.offset),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-element", "element", "Element",
	DEF_MARKER_ELEMENT, Tk_Offset(Marker, elemId), 
        TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_COLOR, "-fill", "fill", "Fill",
        (char *)NULL, Tk_Offset(LineMarker, fillColor), TK_CONFIG_NULL_OK},
    {TK_CONFIG_JOIN_STYLE, "-join", "join", "Join",
	DEF_MARKER_JOIN_STYLE, Tk_Offset(LineMarker, joinStyle),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-linewidth", "lineWidth", "LineWidth",
	DEF_MARKER_LINE_WIDTH, Tk_Offset(LineMarker, lineWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_BOOLEAN, "-hide", "hide", "Hide",
	DEF_MARKER_HIDE, Tk_Offset(Marker, hidden),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-mapx", "mapX", "MapX",
	DEF_MARKER_MAP_X, Tk_Offset(Marker, axes.x), 0, &bltXAxisOption},
    {TK_CONFIG_CUSTOM, "-mapy", "mapY", "MapY",
	DEF_MARKER_MAP_Y, Tk_Offset(Marker, axes.y), 0, &bltYAxisOption},
    {TK_CONFIG_CUSTOM, "-name", (char *)NULL, (char *)NULL,
	DEF_MARKER_NAME, Tk_Offset(Marker, nameId), 
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_COLOR, "-outline", "outline", "Outline",
	DEF_MARKER_OUTLINE_COLOR, Tk_Offset(LineMarker, outlineColor),
	TK_CONFIG_COLOR_ONLY | TK_CONFIG_NULL_OK},
    {TK_CONFIG_COLOR, "-outline", "outline", "Outline",
	DEF_MARKER_OUTLINE_MONO, Tk_Offset(LineMarker, outlineColor),
	TK_CONFIG_MONO_ONLY | TK_CONFIG_NULL_OK},
    {TK_CONFIG_BOOLEAN, "-under", "under", "Under",
	DEF_MARKER_UNDER, Tk_Offset(Marker, drawUnder),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-xoffset", "xOffset", "XOffset",
	DEF_MARKER_X_OFFSET, Tk_Offset(Marker, xOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BOOLEAN, "-xor", "xor", "Xor",
	DEF_MARKER_XOR, Tk_Offset(LineMarker, xor), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-yoffset", "yOffset", "YOffset",
	DEF_MARKER_Y_OFFSET, Tk_Offset(Marker, yOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

/*
 * -------------------------------------------------------------------
 *
 * PolygonMarker --
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    Tk_Uid nameId;		/* Identifier for marker */
    ObjectType type;		/* Type of marker */
    Graph *graphPtr;		/* Graph marker belongs to */
    unsigned int flags;
    char **tags;
    int hidden;			/* Indicates if the marker is currently
				 * hidden or not. */

    Tcl_HashEntry *hashPtr;
    Blt_ListItem item;
    Point2D *coordArr;		/* Coordinate array to position marker */
    int numCoords;		/* Number of points */
    Tk_ConfigSpec *configSpecs;	/* Configuration specifications */
    Tk_Uid elemId;		/* Element associated with marker */
    Axis2D axes;
    int drawUnder;		/* If non-zero, draw the marker underneath
				 * any elements. There can be a performance
				 * because the graph must be redraw entirely
				 * each time this marker is redrawn. */
    int clipped;		/* Indicates if the marker is totally clipped
				 * by the plotting area. */
    int xOffset, yOffset;	/* Pixel offset */

    MarkerDrawProc *drawProc;
    MarkerFreeProc *freeProc;
    MarkerConfigProc *configProc;
    MarkerCoordsProc *coordsProc;
    MarkerPrintProc *printProc;
    MarkerPointProc *pointProc;

    /*
     * Polygon specific attributes and fields
     */
    ColorPair outline;
    ColorPair fill;

    Pixmap stipple;		/* stipple pattern */
    int lineWidth;		/* line width */
    int capStyle;
    int joinStyle;
    Dashes dashes;		/* dash list value */
    
    GC outlineGC;		/* Private graphic context */
    GC fillGC;			/* Private graphic context */

    XPoint *pointArr;		/* Points needed to draw polygon */
    int numPoints;		/* Number of points in above array */
    int xor;
    int xorState;		/* State of the XOR drawing. Indicates
				 * if the marker is current drawn on not. */
} PolygonMarker;

static Tk_ConfigSpec polygonConfigSpecs[] =
{
    {TK_CONFIG_CUSTOM, "-bindtags", "bindTags", "BindTags",
	DEF_MARKER_POLYGON_TAGS, Tk_Offset(Marker, tags),
	TK_CONFIG_NULL_OK, &bltListOption},
    {TK_CONFIG_CAP_STYLE, "-cap", "cap", "Cap",
	DEF_MARKER_CAP_STYLE, Tk_Offset(PolygonMarker, capStyle),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-coords", "coords", "Coords",
	DEF_MARKER_COORDS, Tk_Offset(Marker, coordArr),
	TK_CONFIG_NULL_OK, &coordsOption},
    {TK_CONFIG_CUSTOM, "-dashes", "dashes", "Dashes",
	DEF_MARKER_DASHES, Tk_Offset(PolygonMarker, dashes),
	TK_CONFIG_NULL_OK, &bltDashesOption},
    {TK_CONFIG_CUSTOM, "-element", "element", "Element",
	DEF_MARKER_ELEMENT, Tk_Offset(Marker, elemId),
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_CUSTOM, "-fill", "fill", "Fill",
	DEF_MARKER_FILL_COLOR, Tk_Offset(PolygonMarker, fill),
	TK_CONFIG_COLOR_ONLY | TK_CONFIG_NULL_OK, &bltColorPairOption},
    {TK_CONFIG_CUSTOM, "-fill", "fill", "Fill",
	DEF_MARKER_FILL_MONO, Tk_Offset(PolygonMarker, fill),
	TK_CONFIG_MONO_ONLY | TK_CONFIG_NULL_OK, &bltColorPairOption},
    {TK_CONFIG_JOIN_STYLE, "-join", "join", "Join",
	DEF_MARKER_JOIN_STYLE, Tk_Offset(PolygonMarker, joinStyle),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-linewidth", "lineWidth", "LineWidth",
	DEF_MARKER_LINE_WIDTH, Tk_Offset(PolygonMarker, lineWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_BOOLEAN, "-hide", "hide", "Hide",
	DEF_MARKER_HIDE, Tk_Offset(Marker, hidden),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-mapx", "mapX", "MapX",
	DEF_MARKER_MAP_X, Tk_Offset(Marker, axes.x), 0, &bltXAxisOption},
    {TK_CONFIG_CUSTOM, "-mapy", "mapY", "MapY",
	DEF_MARKER_MAP_Y, Tk_Offset(Marker, axes.y), 0, &bltYAxisOption},
    {TK_CONFIG_CUSTOM, "-name", (char *)NULL, (char *)NULL,
	DEF_MARKER_NAME, Tk_Offset(Marker, nameId), 
	TK_CONFIG_NULL_OK, &bltUidOption},
    {TK_CONFIG_CUSTOM, "-outline", "outline", "Outline",
	DEF_MARKER_OUTLINE_COLOR, Tk_Offset(PolygonMarker, outline),
	TK_CONFIG_COLOR_ONLY | TK_CONFIG_NULL_OK, &bltColorPairOption},
    {TK_CONFIG_CUSTOM, "-outline", "outline", "Outline",
	DEF_MARKER_OUTLINE_MONO, Tk_Offset(PolygonMarker, outline),
	TK_CONFIG_MONO_ONLY | TK_CONFIG_NULL_OK, &bltColorPairOption},
    {TK_CONFIG_BITMAP, "-stipple", "stipple", "Stipple",
	DEF_MARKER_STIPPLE, Tk_Offset(PolygonMarker, stipple),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_BOOLEAN, "-under", "under", "Under",
	DEF_MARKER_UNDER, Tk_Offset(Marker, drawUnder),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-xoffset", "xOffset", "XOffset",
	DEF_MARKER_X_OFFSET, Tk_Offset(Marker, xOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BOOLEAN, "-xor", "xor", "Xor",
	DEF_MARKER_XOR, Tk_Offset(PolygonMarker, xor), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-yoffset", "yOffset", "YOffset",
	DEF_MARKER_Y_OFFSET, Tk_Offset(Marker, yOffset),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};


#ifdef __STDC__
static MarkerDrawProc DrawBitmapMarker, DrawLineMarker, DrawImageMarker, 
	DrawPolygonMarker, DrawTextMarker, DrawWindowMarker;
static MarkerFreeProc DestroyBitmapMarker, DestroyLineMarker, DestroyImageMarker,
	DestroyPolygonMarker, DestroyTextMarker, DestroyWindowMarker;
static MarkerConfigProc ConfigureBitmapMarker, ConfigureLineMarker, 
	ConfigureImageMarker, ConfigurePolygonMarker, ConfigureTextMarker, 
	ConfigureWindowMarker;
static MarkerCoordsProc TransformBitmapMarker, TransformLineMarker, 
	TransformImageMarker, TransformPolygonMarker, TransformTextMarker, 
	TransformWindowMarker;
static MarkerPrintProc PrintBitmapMarker, PrintLineMarker, PrintImageMarker, 
	PrintPolygonMarker, PrintTextMarker, PrintWindowMarker;
static MarkerPointProc PointInBitmapMarker, PointInLineMarker, PointInImageMarker,
	PointInPolygonMarker, PointInTextMarker, PointInWindowMarker;

static Tk_ImageChangedProc ImageChangedProc;

#endif /* __STDC__ */
/*
 * Only for polygons right now.  Will be removed with polygon clip routine.
 */
INLINE static void
BoundPoint(pointPtr)
    XPoint *pointPtr;
{
    /* Should really figure out a good offset value and test for that because
     * we could still generate bogus numbers */

    if (pointPtr->x >= SHRT_MAX) {
	pointPtr->x = SHRT_MAX - 1000;
    } else if (pointPtr->x <= SHRT_MIN) {
	pointPtr->x = SHRT_MIN + 1000;
    }
    if (pointPtr->y >= SHRT_MAX) {
	pointPtr->y = SHRT_MAX - 1000;
    } else if (pointPtr->y <= SHRT_MIN) {
	pointPtr->y = SHRT_MIN + 1000;
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * BoxesDontOverlap --
 *
 *	Tests if the bounding box of a marker overlaps the plotting
 *	area in any way.  If so, the marker will be drawn.  Just do a
 *	min/max test on the extents of both boxes.
 *
 * Results:
 *	Returns 0 is the marker is visible in the plotting area, and
 *	1 otherwise (marker is clipped).
 *
 * ----------------------------------------------------------------------
 */
static int
BoxesDontOverlap(graphPtr, extentsPtr)
    Graph *graphPtr;
    Extents2D *extentsPtr;
{
    return (((double)graphPtr->xMax < extentsPtr->xMin) ||
	((double)graphPtr->yMax < extentsPtr->yMin) ||
	(extentsPtr->xMax < (double)graphPtr->xMin) ||
	(extentsPtr->yMax < (double)graphPtr->yMin));
}

/*
 * ----------------------------------------------------------------------
 *
 * PrintCoordinate --
 *
 * 	Convert the double precision value into its string
 * 	representation.  The only reason this routine is used in
 * 	instead of sprintf, is to handle the "elastic" bounds.  That
 * 	is, convert the values DBL_MAX and -(DBL_MAX) into "+Inf" and
 * 	"-Inf" respectively.
 *
 * Results:
 *	The return value is a standard Tcl result.  The string of the
 * 	expression is passed back via string.
 *
 * ---------------------------------------------------------------------- */
static char *
PrintCoordinate(interp, x)
    Tcl_Interp *interp;
    double x;			/* Numeric value */
{
    if (x == bltPosInfinity) {
	return "+Inf";
    } else if (x == bltNegInfinity) {
	return "-Inf";
    } else {
	static char string[TCL_DOUBLE_SPACE + 1];

	Tcl_PrintDouble(interp, x, string);
	return string;
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * ParseCoordinates --
 *
 *	If no coordinates are specified (coordList is NULL), this
 *	routine returns the coordinates of a given marker. Otherwise,
 *	the Tcl coordinate list is converted to their floating point
 *	values. It will then replace the current marker coordinates.
 *
 *	Since different marker types require different number of
 *	coordinates this must be checked here.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side effects:
 *	If the marker coordinates are reset, the graph is eventually
 *	redrawn with at the new marker coordinates.
 *
 * ----------------------------------------------------------------------
 */

static int
ParseCoordinates(interp, markerPtr, numExprs, exprArr)
    Tcl_Interp *interp;
    Marker *markerPtr;
    int numExprs;
    char **exprArr;
{
    int numCoords;
    int minArgs, maxArgs;
    double x, y;
    Point2D *newArr;
    register int i;
    register Point2D *pointPtr;

    /* Split the list of coordinates and check the values */

    if (numExprs == 0) {
	return TCL_OK;
    }
    if (numExprs & 1) {
	Tcl_AppendResult(interp, "odd number of marker coordinates specified",
	    (char *)NULL);
	return TCL_ERROR;
    }
    switch (markerPtr->type) {
    case TYPE_MARKER_LINE:
	minArgs = 4, maxArgs = 0;
	break;
    case TYPE_MARKER_POLYGON:
	minArgs = 6, maxArgs = 0;
	break;
    case TYPE_MARKER_WINDOW:
    case TYPE_MARKER_TEXT:
	minArgs = 2, maxArgs = 2;
	break;
    case TYPE_MARKER_IMAGE:
    case TYPE_MARKER_BITMAP:
	minArgs = 2, maxArgs = 4;
	break;
    default:
	Tcl_AppendResult(interp, "unknown marker type", (char *)NULL);
	return TCL_ERROR;
    }

    if (numExprs < minArgs) {
	Tcl_AppendResult(interp, "too few marker coordinates specified",
	    (char *)NULL);
	return TCL_ERROR;
    }
    if ((maxArgs > 0) && (numExprs > maxArgs)) {
	Tcl_AppendResult(interp, "too many marker coordinates specified",
	    (char *)NULL);
	return TCL_ERROR;
    }
    numCoords = numExprs / 2;
    newArr = (Point2D *)malloc(numCoords * sizeof(Point2D));
    if (newArr == NULL) {
	Tcl_AppendResult(interp, "can't allocate new coordinate array",
	    (char *)NULL);
	return TCL_ERROR;
    }
    /*
     * A new coordinate array is allocated each time so that we
     * can check the coordinates without overwriting the current
     * marker coordinates.
     */
    pointPtr = newArr;
    for (i = 0; i < numExprs; i += 2) {
	if ((Blt_GetCoordinate(interp, exprArr[i], &x) != TCL_OK) ||
	    (Blt_GetCoordinate(interp, exprArr[i + 1], &y) != TCL_OK)) {
	    free((char *)newArr);
	    return TCL_ERROR;
	}
	pointPtr->x = x, pointPtr->y = y;
	pointPtr++;
    }
    if (markerPtr->coordArr != NULL) {
	free((char *)markerPtr->coordArr);
    }
    markerPtr->coordArr = newArr;
    markerPtr->numCoords = numCoords;
    markerPtr->flags |= COORDS_NEEDED;
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * StringToCoordinates --
 *
 *	Given a Tcl list of numeric expression representing the
 *	element values, convert into an array of double precision
 *	values. In addition, the minimum and maximum values are saved.
 *	Since elastic values are allow (values which translate to the
 *	min/max of the graph), we must try to get the non-elastic
 *	minimum and maximum.
 *
 * Results:
 *	The return value is a standard Tcl result.  The vector is
 *	passed back via the vecPtr.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToCoordinates(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* Tcl list of numeric expressions */
    char *widgRec;		/* Marker record */
    int offset;			/* not used */
{
    Marker *markerPtr = (Marker *)widgRec;
    int numExprs;
    char **exprArr;
    int result;

    if ((string == NULL) || (*string == '\0')) {
      noCoordinates:
	if (markerPtr->coordArr != NULL) {
	    free((char *)markerPtr->coordArr);
	}
	markerPtr->numCoords = 0;
	return TCL_OK;
    }
    if (Tcl_SplitList(interp, string, &numExprs, &exprArr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (numExprs == 0) {
	goto noCoordinates;
    }
    result = ParseCoordinates(interp, markerPtr, numExprs, exprArr);
    free((char *)exprArr);
    return (result);
}

/*
 * ----------------------------------------------------------------------
 *
 * CoordinatesToString --
 *
 *	Convert the vector of floating point values into a Tcl list.
 *
 * Results:
 *	The string representation of the vector is returned.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
CoordinatesToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Marker record */
    int offset;			/* not used */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    Marker *markerPtr = (Marker *)widgRec;
    Tcl_DString dStr;
    char *result;
    register int i;
    register Point2D *pointPtr;

    if (markerPtr->numCoords < 1) {
	return "";
    }
    Tcl_DStringInit(&dStr);
    pointPtr = markerPtr->coordArr;
    for (i = 0; i < markerPtr->numCoords; i++, pointPtr++) {
	Tcl_DStringAppendElement(&dStr,
	    PrintCoordinate(markerPtr->graphPtr->interp, pointPtr->x));
	Tcl_DStringAppendElement(&dStr,
	    PrintCoordinate(markerPtr->graphPtr->interp, pointPtr->y));
    }
    result = Tcl_DStringValue(&dStr);

    /*
     * If memory wasn't allocated for the dynamic string, do it here (it's
     * currently on the stack), so that Tcl can free it normally.
     */
    if (result == dStr.staticSpace) {
	result = strdup(result);
    }
    *freeProcPtr = (Tcl_FreeProc *)free;
    return (result);
}

/* Map graph coordinates to normalized coordinates [0..1] */
#define NORMALIZE(A,x) 	(((x) - (A)->limitsPtr->min) / (A)->limitsPtr->range)

/*
 * ----------------------------------------------------------------------
 *
 * HMap --
 *
 *	Map the given graph coordinate value to its axis, returning a
 *	window position.
 *
 * Results:
 *	Returns a double precision number representing the window
 *	coordinate position on the given axis.
 *
 * ----------------------------------------------------------------------
 */
static double
HMap(graphPtr, axisPtr, x)
    Graph *graphPtr;
    VirtualAxis *axisPtr;
    double x;
{
    register double norm;

    if (x == bltPosInfinity) {
	norm = 1.0;
    } else if (x == bltNegInfinity) {
	norm = 0.0;
    } else {
	if (axisPtr->logScale) {
	    if (x > 0.0) {
		x = log10(x);
	    } else if (x < 0.0) {
		x = 0.0;
	    }
	}
	norm = NORMALIZE(axisPtr, x);
    }
    if (axisPtr->descending) {
	norm = 1.0 - norm;
    }
    /* Horizontal transformation */
    return ((norm * (graphPtr->hRange)) + graphPtr->hOffset);
}

/*
 * ----------------------------------------------------------------------
 *
 * VMap --
 *
 *	Map the given graph coordinate value to its axis, returning a
 *	window position.
 *
 * Results:
 *	Returns a double precision number representing the window
 *	coordinate position on the given axis.
 *
 * ----------------------------------------------------------------------
 */
static double
VMap(graphPtr, axisPtr, y)
    Graph *graphPtr;
    VirtualAxis *axisPtr;
    double y;
{
    register double norm;

    if (y == bltPosInfinity) {
	norm = 1.0;
    } else if (y == bltNegInfinity) {
	norm = 0.0;
    } else {
	if (axisPtr->logScale) {
	    if (y > 0.0) {
		y = log10(y);
	    } else if (y < 0.0) {
		y = 0.0;
	    }
	}
	norm = NORMALIZE(axisPtr, y);
    }
    if (axisPtr->descending) {
	norm = 1.0 - norm;
    }
    /* Vertical transformation */
    return (((1.0 - norm) * (graphPtr->vRange)) + graphPtr->vOffset);
}

/*
 * ----------------------------------------------------------------------
 *
 * TransformCoordinate --
 *
 *	Maps the given graph x,y coordinate values to a window position.
 *
 * Results:
 *	Returns a XPoint structure containing the window coordinates
 *	of the given graph x,y coordinate.
 *
 * ----------------------------------------------------------------------
 */
static void
TransformCoordinate(graphPtr, coordPtr, axesPtr, xPtr, yPtr)
    Graph *graphPtr;
    Point2D *coordPtr;		/* Graph x and y coordinates */
    Axis2D *axesPtr;		/* Specifies which axes to use */
    int *xPtr, *yPtr;
{
    double x, y;

    if (graphPtr->inverted) {
	x = HMap(graphPtr, axesPtr->y, coordPtr->y);
	y = VMap(graphPtr, axesPtr->x, coordPtr->x);
    } else {
	x = HMap(graphPtr, axesPtr->x, coordPtr->x);
	y = VMap(graphPtr, axesPtr->y, coordPtr->y);
    }
    *xPtr = ROUND(x);
    *yPtr = ROUND(y);
}



static void
DestroyMarker(markerPtr)
    Marker *markerPtr;
{
    Graph *graphPtr = markerPtr->graphPtr;

    if (markerPtr->drawUnder) {
	graphPtr->flags |= REDRAW_BACKING_STORE;
    }
    /* Free the resources allocated for the particular type of marker */
    (*markerPtr->freeProc) (graphPtr, markerPtr);
    if (markerPtr->coordArr != NULL) {
	free((char *)markerPtr->coordArr);
    }
    Blt_DeleteAllBindings(graphPtr->bindTable, (ClientData)markerPtr);
    Tk_FreeOptions(markerPtr->configSpecs, (char *)markerPtr,
	graphPtr->display, 0);
    if (markerPtr->hashPtr != NULL) {
	Tcl_DeleteHashEntry(markerPtr->hashPtr);
    }
    if (markerPtr->item != NULL) {
	Blt_ListDeleteItem(markerPtr->item);
    }
    if (markerPtr->nameId != NULL) {
	Blt_FreeUid(markerPtr->nameId);
    }
    if (markerPtr->elemId != NULL) {
	Blt_FreeUid(markerPtr->elemId);
    }
    if (markerPtr->tags != NULL) {
	free((char *)markerPtr->tags);
    }
    free((char *)markerPtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * ConfigureBitmapMarker --
 *
 *	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or reconfigure)
 *	a bitmap marker.
 *
 * Results:
 *	A standard Tcl result.  If TCL_ERROR is returned, then
 *	interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as bitmap pixmap, colors,
 *	rotation, etc. get set for markerPtr; old resources get freed,
 *	if there were any.  The marker is eventually redisplayed.
 *
 * ----------------------------------------------------------------------
 */
/* ARGSUSED */
static int
ConfigureBitmapMarker(markerPtr)
    Marker *markerPtr;
{
    Graph *graphPtr = markerPtr->graphPtr;
    BitmapMarker *bmapPtr = (BitmapMarker *)markerPtr;
    GC newGC;
    XGCValues gcValues;
    unsigned long gcMask;

    if (bmapPtr->bitmap == None) {
	return TCL_OK;
    }
    bmapPtr->theta = FMOD(bmapPtr->rotate, 360.0);
    if (bmapPtr->theta < 0.0) {
	bmapPtr->theta += 360.0;
    }
    gcMask = 0;
    if (bmapPtr->fgColor != NULL) {
	gcMask |= GCForeground;
	gcValues.foreground = bmapPtr->fgColor->pixel;
    }
    if (bmapPtr->bgColor != NULL) {
	gcValues.background = bmapPtr->bgColor->pixel;
	gcMask |= GCBackground;
    } else {
	gcValues.clip_mask = bmapPtr->bitmap;
	gcMask |= GCClipMask;
    }


    /*
     * Note that while this is a "shared" GC, we're going to change
     * the clip origin right before the bitmap is drawn anyways.  This
     * assumes that any drawing code using this GC (with GCClipMask
     * set) is going to want to set the clip origin anyways.
     */
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (bmapPtr->gc != NULL) {
	Tk_FreeGC(graphPtr->display, bmapPtr->gc);
    }
    bmapPtr->gc = newGC;

    /* Create background GC color */

    if (bmapPtr->bgColor != NULL) {
	gcValues.foreground = bmapPtr->bgColor->pixel;
	newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
	if (bmapPtr->fillGC != NULL) {
	    Tk_FreeGC(graphPtr->display, bmapPtr->fillGC);
	}
	bmapPtr->fillGC = newGC;
    }
    if (!bmapPtr->hidden) {
	bmapPtr->flags |= COORDS_NEEDED;
	if (bmapPtr->drawUnder) {
	    graphPtr->flags |= REDRAW_BACKING_STORE;
	}
	Blt_EventuallyRedrawGraph(graphPtr);
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * TranformBitmap --
 *
 * 	This procedure gets called each time the layout of the graph
 *	changes.  The x, y window coordinates of the bitmap marker are
 *	saved in the marker structure.
 *
 *	Additionly, if no background color was specified, the
 *	GCTileStipXOrigin and GCTileStipYOrigin attributes are set in
 *	the private GC.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Window coordinates are saved and if no background color was
 * 	set, the GC stipple origins are changed to calculated window
 *	coordinates.
 *
 * ----------------------------------------------------------------------
 */
static void
TransformBitmapMarker(markerPtr)
    Marker *markerPtr;
{
    Graph *graphPtr = markerPtr->graphPtr;
    BitmapMarker *bmapPtr = (BitmapMarker *)markerPtr;
    int width, height, bmWidth, bmHeight, scaledWidth, scaledHeight;
    int x, y;
    int x1, y1, x2, y2;
    Extents2D extents;
    double sx, sy;
    double p, q;
    register int i;

    if (bmapPtr->transBitmap != None) {
	Tk_FreePixmap(graphPtr->display, bmapPtr->transBitmap);
    }
    bmapPtr->transBitmap = None;
    if (bmapPtr->bitmap == None) {
	return;
    }
    Tk_SizeOfBitmap(graphPtr->display, bmapPtr->bitmap, &width, &height);
    bmWidth = width;
    bmHeight = height;
    TransformCoordinate(graphPtr, bmapPtr->coordArr, &bmapPtr->axes, &x1, &y1);
    if (bmapPtr->numCoords > 1) {
	TransformCoordinate(graphPtr, bmapPtr->coordArr + 1, &bmapPtr->axes,
	    &x2, &y2);
	scaledWidth = x1 - x2;
	scaledHeight = y1 - y2;
	scaledWidth = ABS(scaledWidth) + 1;
	scaledHeight = ABS(scaledHeight) + 1;
	x = MIN(x1, x2);
	y = MIN(y1, y2);
    } else {
	scaledWidth = width;
	scaledHeight = height;
	x = x1;
	y = y1;
    }

    if (bmapPtr->numCoords == 1) {
	Blt_TranslateAnchor(x, y, scaledWidth, scaledHeight, bmapPtr->anchor,
	    &x, &y);
    }
    x += bmapPtr->xOffset;
    y += bmapPtr->yOffset;

    /*
     * Determine the bounding box of the bitmap and test to see if it
     * is at least partially contained within the plotting area.
     */
    extents.xMin = x;
    extents.yMax = y + scaledHeight;
    extents.yMin = y;
    extents.xMax = x + scaledWidth;
    bmapPtr->clipped = BoxesDontOverlap(graphPtr, &extents);
    if (bmapPtr->clipped) {
	return;			/* Bitmap is offscreen. Don't generate
				 * rotated or scaled bitmaps. */
    }
    /* Start of pointing to the original bitmap */
    bmapPtr->draw = bmapPtr->bitmap;
    if (bmapPtr->theta != 0.0) {
	bmapPtr->draw = Blt_RotateBitmap(graphPtr->tkwin, bmapPtr->draw, 
		width, height, bmapPtr->theta, &width, &height);
	bmapPtr->transBitmap = bmapPtr->draw;
    }
    if ((scaledWidth != width) || (scaledHeight != height)) {
	ImageRegion region;
	int xMin, xMax, yMin, yMax;

	/*
	 * Determine if the bitmap is clipped by the graph.
	 */
	xMin = MAX(x, graphPtr->xMin);
	yMin = MAX(y, graphPtr->yMin);
	xMax = MIN(x + scaledWidth - 1, graphPtr->xMax);
	yMax = MIN(y + scaledHeight - 1, graphPtr->yMax);

	region.x = xMin - x;
	region.y = yMin - y;
	region.width = xMax - xMin + 1;
	region.height = yMax - yMin + 1;

	bmapPtr->draw = Blt_ScaleBitmapRegion(graphPtr->tkwin, bmapPtr->draw,
	    width, height, scaledWidth, scaledHeight, &region);
	if (bmapPtr->transBitmap != None) {
	    Tk_FreePixmap(graphPtr->display, bmapPtr->transBitmap);
	}
	bmapPtr->transBitmap = bmapPtr->draw;

	/* Reset bitmap location and coordinates to that of the region */
	width = region.width;
	height = region.height;
	x = xMin;
	y = yMin;
    }
    bmapPtr->x = x;
    bmapPtr->y = y;
    bmapPtr->width = width;
    bmapPtr->height = height;

    /*
     * Compute the background area of the bitmap.  This is needed to
     * draw a background for arbitrarily rotated bitmaps.  We also use
     * it to print a background in PostScript. For right-angle
     * rotations, this is the bounding box same as above.
     */
    Blt_GetBoundingBox(bmWidth, bmHeight, bmapPtr->theta, &width, &height,
	bmapPtr->pointArr);
    sx = (double)scaledWidth / (double)width;
    sy = (double)scaledHeight / (double)height;

    for (i = 0; i < 4; i++) {
	p = (double)bmapPtr->pointArr[i].x * sx;
	q = (double)bmapPtr->pointArr[i].y * sy;
	bmapPtr->pointArr[i].x = ROUND(p) + bmapPtr->x + (scaledWidth / 2);
	bmapPtr->pointArr[i].y = ROUND(q) + bmapPtr->y + (scaledHeight / 2);
    }
}

static int
PointInBitmapMarker(markerPtr, x, y)
    Marker *markerPtr;
    int x, y;
{
    BitmapMarker *bmapPtr = (BitmapMarker *)markerPtr;

    return ((x >= bmapPtr->x) && (x < (bmapPtr->x + bmapPtr->width)) &&
	(y >= bmapPtr->y) && (y < (bmapPtr->y + bmapPtr->height)));
}

/*
 * ----------------------------------------------------------------------
 *
 * DrawBitmap --
 *
 *	This procedure is invoked to draw a bitmap marker.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	GC stipple origins are changed to current window coordinates.
 *	Commands are output to X to draw the marker in its current
 *	mode.
 *
 * ----------------------------------------------------------------------
 */
static void
DrawBitmapMarker(markerPtr, drawable)
    Marker *markerPtr;
    Drawable drawable;		/* Pixmap or window to draw into */
{
    Graph *graphPtr = markerPtr->graphPtr;
    BitmapMarker *bmapPtr = (BitmapMarker *)markerPtr;
    double theta;

    if ((bmapPtr->bitmap == None) ||
	(bmapPtr->width < 1) || (bmapPtr->height < 1)) {
	return;
    }
    theta = FMOD(bmapPtr->theta, (double)90.0);
    if ((bmapPtr->bgColor == NULL) || (theta != 0.0)) {
	if (bmapPtr->bgColor != NULL) {
	    XFillPolygon(graphPtr->display, drawable, bmapPtr->fillGC,
		bmapPtr->pointArr, 4, Convex, CoordModeOrigin);
	}
	XSetClipMask(graphPtr->display, bmapPtr->gc, bmapPtr->draw);
	XSetClipOrigin(graphPtr->display, bmapPtr->gc, bmapPtr->x, bmapPtr->y);
    } else {
	XSetClipMask(graphPtr->display, bmapPtr->gc, None);
	XSetClipOrigin(graphPtr->display, bmapPtr->gc, 0, 0);
    }
    XCopyPlane(graphPtr->display, bmapPtr->draw, drawable, bmapPtr->gc, 0, 0,
	bmapPtr->width, bmapPtr->height, bmapPtr->x, bmapPtr->y, 1);
}

/*
 * ----------------------------------------------------------------------
 *
 * PrintBitmap --
 *
 *	This procedure is invoked to print a bitmap marker.
 *
 * Results:
 *	None.
 *
 * ----------------------------------------------------------------------
 */
static void
PrintBitmapMarker(markerPtr, printable)
    Marker *markerPtr;		/* Marker to be printed */
    Printable printable;
{
    Graph *graphPtr = markerPtr->graphPtr;
    BitmapMarker *bmapPtr = (BitmapMarker *)markerPtr;

    if (bmapPtr->bitmap == None) {
	return;
    }
    if (bmapPtr->bgColor != NULL) {
	Blt_BackgroundToPostScript(printable, bmapPtr->bgColor);
	Blt_PolygonToPostScript(printable, bmapPtr->pointArr, 4);
    }
    Blt_ForegroundToPostScript(printable, bmapPtr->fgColor);

    Blt_PrintFormat(printable,
	"  gsave\n    %d %d translate\n    %d %d scale\n", bmapPtr->x,
	bmapPtr->y + bmapPtr->height, bmapPtr->width, -bmapPtr->height);
    Blt_PrintFormat(printable, "    %d %d true [%d 0 0 %d 0 %d] {",
	bmapPtr->width, bmapPtr->height, bmapPtr->width, -bmapPtr->height,
	bmapPtr->height);
    Blt_BitmapToPostScript(printable, graphPtr->display, bmapPtr->draw,
	bmapPtr->width, bmapPtr->height);
    Blt_PrintAppend(printable,
	"    } imagemask\n",
	"grestore\n", (char *)NULL);
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyBitmapMarker --
 *
 *	Destroys the structure containing the attributes of the bitmap
 * 	marker.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Bitmap attributes (GCs, colors, bitmap, etc) get destroyed.
 *	Memory is released, X resources are freed, and the graph is
 *	redrawn.
 *
 * ----------------------------------------------------------------------
 */
static void
DestroyBitmapMarker(graphPtr, markerPtr)
    Graph *graphPtr;
    Marker *markerPtr;
{
    BitmapMarker *bmapPtr = (BitmapMarker *)markerPtr;

    if (bmapPtr->gc != NULL) {
	Tk_FreeGC(graphPtr->display, bmapPtr->gc);
    }
    if (bmapPtr->fillGC != NULL) {
	Tk_FreeGC(graphPtr->display, bmapPtr->fillGC);
    }
    if (bmapPtr->transBitmap != None) {
	Tk_FreePixmap(graphPtr->display, bmapPtr->transBitmap);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * CreateBitmapMarker --
 *
 *	Allocate memory and initialize methods for the new bitmap marker.
 *
 * Results:
 *	The pointer to the newly allocated marker structure is returned.
 *
 * Side effects:
 *	Memory is allocated for the bitmap marker structure.
 *
 * ----------------------------------------------------------------------
 */
static Marker *
CreateBitmapMarker()
{
    BitmapMarker *bmapPtr;

    bmapPtr = (BitmapMarker *)calloc(1, sizeof(BitmapMarker));
    if (bmapPtr != NULL) {
	bmapPtr->configSpecs = bitmapConfigSpecs;
	bmapPtr->configProc = ConfigureBitmapMarker;
	bmapPtr->freeProc = DestroyBitmapMarker;
	bmapPtr->drawProc = DrawBitmapMarker;
	bmapPtr->coordsProc = TransformBitmapMarker;
	bmapPtr->printProc = PrintBitmapMarker;
	bmapPtr->pointProc = PointInBitmapMarker;
	bmapPtr->type = TYPE_MARKER_BITMAP;
    }
    return ((Marker *)bmapPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * ImageChangedProc
 *
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
/* ARGSUSED */
static void
ImageChangedProc(clientData, x, y, width, height, imageWidth, imageHeight)
    ClientData clientData;
    int x, y, width, height;	/* Not used */
    int imageWidth, imageHeight;/* Not used */
{
    ImageMarker *imagePtr = (ImageMarker *)clientData;

    imagePtr->graphPtr->flags |= REDRAW_BACKING_STORE;
    imagePtr->flags |= COORDS_NEEDED;
    Blt_EventuallyRedrawGraph(imagePtr->graphPtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * ConfigureImageMarker --
 *
 *	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or reconfigure)
 *	a image marker.
 *
 * Results:
 *	A standard Tcl result.  If TCL_ERROR is returned, then
 *	interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as image pixmap, colors,
 *	rotation, etc. get set for markerPtr; old resources get freed,
 *	if there were any.  The marker is eventually redisplayed.
 *
 * ----------------------------------------------------------------------
 */
 /* ARGSUSED */
static int
ConfigureImageMarker(markerPtr)
    Marker *markerPtr;
{
    ImageMarker *imagePtr = (ImageMarker *)markerPtr;
    Graph *graphPtr = markerPtr->graphPtr;

    if (Blt_ConfigModified(imagePtr->configSpecs, "-image", (char *)NULL)) {
	if (imagePtr->tkImage != NULL) {
	    Tk_FreeImage(imagePtr->tkImage);
	    imagePtr->tkImage = NULL;
	}
	if (imagePtr->imageName[0] != '\0') {
	    GC newGC;

	    imagePtr->tkImage = Tk_GetImage(graphPtr->interp, graphPtr->tkwin,
		imagePtr->imageName, ImageChangedProc, (ClientData)imagePtr);
	    if (imagePtr->tkImage == NULL) {
		Tcl_AppendResult(graphPtr->interp, "can't find an image \"",
		    imagePtr->imageName, "\"", (char *)NULL);
		free((char *)imagePtr->imageName);
		imagePtr->imageName = NULL;
		return TCL_ERROR;
	    }
	    imagePtr->photo = Blt_FindPhoto(graphPtr->interp,
		imagePtr->imageName);
	    if (imagePtr->photo != NULL) {
		ImageRegion region;
		int imageWidth, imageHeight;

		/* Convert the photo into a color image */
		Tk_SizeOfImage(imagePtr->tkImage, &imageWidth, &imageHeight);
		region.x = region.y = 0;
		region.width = imageWidth;
		region.height = imageHeight;
		imagePtr->srcImage = Blt_PhotoToColorImage(imagePtr->photo,
		    &region);
		newGC = Tk_GetGC(graphPtr->tkwin, 0L, (XGCValues *)NULL);
		if (imagePtr->gc != NULL) {
		    Tk_FreeGC(graphPtr->display, imagePtr->gc);
		}
		imagePtr->gc = newGC;
	    }
	}
    }
    if (!imagePtr->hidden) {
	imagePtr->flags |= COORDS_NEEDED;
	if (imagePtr->drawUnder) {
	    graphPtr->flags |= REDRAW_BACKING_STORE;
	}
	Blt_EventuallyRedrawGraph(graphPtr);
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * TranformImageMarker --
 *
 * 	This procedure gets called each time the layout of the graph
 *	changes.  The x, y window coordinates of the image marker are
 *	saved in the marker structure.
 *
 *	Additionly, if no background color was specified, the
 *	GCTileStipXOrigin and GCTileStipYOrigin attributes are set in
 *	the private GC.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Window coordinates are saved and if no background color was *
 *	set, the GC stipple origins are changed to calculated window
 *	coordinates.
 *
 * ----------------------------------------------------------------------
 */
static void
TransformImageMarker(markerPtr)
    Marker *markerPtr;
{
    ImageMarker *imagePtr = (ImageMarker *)markerPtr;
    Graph *graphPtr = imagePtr->graphPtr;
    int width, height;
    int x, y;
    int x1, x2, y1, y2;
    int scaledWidth, scaledHeight;
    float xScale, yScale;
    Extents2D extents;

    if (imagePtr->tkImage == NULL) {
	return;
    }
    if (imagePtr->srcImage == NULL) {
	Tk_SizeOfImage(imagePtr->tkImage, &width, &height);
    } else {
	width = ColorImageWidth(imagePtr->srcImage);
	height = ColorImageHeight(imagePtr->srcImage);
    }
    TransformCoordinate(graphPtr, imagePtr->coordArr, &imagePtr->axes,
	&x1, &y1);
    xScale = yScale = 1.0;
    if ((imagePtr->srcImage != NULL) && (imagePtr->numCoords > 1)) {
	TransformCoordinate(graphPtr, imagePtr->coordArr + 1, &imagePtr->axes,
	    &x2, &y2);
	scaledWidth = x1 - x2;
	scaledHeight = y1 - y2;
	scaledWidth = ABS(scaledWidth) + 1;
	scaledHeight = ABS(scaledHeight) + 1;
	x = MIN(x1, x2);
	y = MIN(y1, y2);
	xScale = (float)scaledWidth / (float)width;
	yScale = (float)scaledHeight / (float)height;
    } else {
	scaledWidth = width;
	scaledHeight = height;
	x = x1;
	y = y1;
    }
    if (imagePtr->numCoords == 1) {
	Blt_TranslateAnchor(x, y, scaledWidth, scaledHeight, imagePtr->anchor,
	    &x, &y);
    }
    x += imagePtr->xOffset;
    y += imagePtr->yOffset;

    /*
     * Determine the bounding box of the bitmap and test to see if it
     * is at least partially contained within the plotting area.
     */
    extents.xMin = x;
    extents.yMax = y + scaledHeight;
    extents.yMin = y;
    extents.xMax = x + scaledWidth;
    imagePtr->clipped = BoxesDontOverlap(graphPtr, &extents);
    if (imagePtr->clipped) {
	return;			/* Image is totally offscreen. Don't generate
				 * scaled version. */
    }
    if ((imagePtr->srcImage != NULL) && ((xScale != 1.0) || (yScale != 1.0))) {
	ImageRegion srcRegion, destRegion;
	int xMin, xMax, yMin, yMax;

	/* Determine if the image is clipped within the graph's
           plotting area. */
	xMin = MAX(x, graphPtr->xMin);
	yMin = MAX(y, graphPtr->yMin);
	xMax = MIN(x + scaledWidth - 1, graphPtr->xMax);
	yMax = MIN(y + scaledHeight - 1, graphPtr->yMax);

	destRegion.x = ABS(x - xMin);
	destRegion.y = ABS(y - yMin);
	destRegion.width = xMax - xMin + 1;
	destRegion.height = yMax - yMin + 1;

	/* Compute the source region to scale */
	srcRegion.x = (int)(((float)destRegion.x / xScale) + 0.5);
	srcRegion.y = (int)(((float)destRegion.y / yScale) + 0.5);
	srcRegion.width = (int)(((float)destRegion.width / xScale) + 0.5);
	srcRegion.height = (int)(((float)destRegion.height / yScale) + 0.5);

	if (imagePtr->destImage != NULL) {
	    Blt_FreeColorImage(imagePtr->destImage);
	}
	imagePtr->destImage = Blt_ResizeColorImage(imagePtr->srcImage,
	    &srcRegion, &destRegion);

	/* Reset bitmap location and coordinates to that of the region */
	width = destRegion.width;
	height = destRegion.height;
	x = xMin;
	y = yMin;

#ifdef notdef
	/* Now convert the color image into a pixmap */
	if (imagePtr->pixmap != None) {
	    Blt_FreeColorTable(imagePtr->colorTable);
	    Tk_FreePixmap(Tk_Display(graphPtr->tkwin), imagePtr->pixmap);
	    imagePtr->colorTable = NULL;
	}
	imagePtr->pixmap = Blt_ColorImageToPixmap(graphPtr->interp,
	    graphPtr->tkwin, imagePtr->destImage, &(imagePtr->colorTable));
#else
	imagePtr->pixmap = None;
	Blt_ColorImageToPhoto(imagePtr->destImage, imagePtr->photo);
#endif
    }
    imagePtr->x = x;
    imagePtr->y = y;
    imagePtr->width = width;
    imagePtr->height = height;
}

static int
PointInImageMarker(markerPtr, x, y)
    Marker *markerPtr;
    int x, y;
{
    ImageMarker *imagePtr = (ImageMarker *)markerPtr;

    return ((x >= imagePtr->x) && (x < (imagePtr->x + imagePtr->width)) &&
	(y >= imagePtr->y) && (y < (imagePtr->y + imagePtr->height)));
}

/*
 * ----------------------------------------------------------------------
 *
 * DrawImageMarker --
 *
 *	This procedure is invoked to draw a image marker.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	GC stipple origins are changed to current window coordinates.
 *	Commands are output to X to draw the marker in its current mode.
 *
 * ----------------------------------------------------------------------
 */
static void
DrawImageMarker(markerPtr, drawable)
    Marker *markerPtr;
    Drawable drawable;		/* Pixmap or window to draw into */
{
    ImageMarker *imagePtr = (ImageMarker *)markerPtr;
    int width, height;

    if ((imagePtr->tkImage == NULL) || (Blt_TkImageDeleted(imagePtr->tkImage))) {
	return;
    }
    if (imagePtr->pixmap == None) {
	Tk_SizeOfImage(imagePtr->tkImage, &width, &height);
	Tk_RedrawImage(imagePtr->tkImage, 0, 0, width, height, drawable,
	    imagePtr->x, imagePtr->y);
    } else {
	XCopyArea(imagePtr->graphPtr->display, imagePtr->pixmap, drawable,
	    imagePtr->gc, 0, 0, imagePtr->width, imagePtr->height,
	    imagePtr->x, imagePtr->y);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * PrintImageMarker --
 *
 *	This procedure is invoked to print a image marker.
 *
 * Results:
 *	None.
 *
 * ----------------------------------------------------------------------
 */
static void
PrintImageMarker(markerPtr, printable)
    Marker *markerPtr;		/* Marker to be printed */
    Printable printable;
{
    ImageMarker *imagePtr = (ImageMarker *)markerPtr;

    if ((imagePtr->tkImage == NULL) || (Blt_TkImageDeleted(imagePtr->tkImage))) {
	return;			/* Image doesn't exist anymore */
    }
    if (imagePtr->photo == NULL) {
	return;			/* Image isn't a photo image */
    }
    if (imagePtr->destImage == NULL) {
	Blt_PhotoToPostScript(printable, imagePtr->photo, imagePtr->x,
	    imagePtr->y);
    } else {
	Blt_ColorImageToPostScript(printable, imagePtr->destImage, imagePtr->x,
	    imagePtr->y);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyImageMarker --
 *
 *	Destroys the structure containing the attributes of the image
 * 	marker.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Image attributes (GCs, colors, image, etc) get destroyed.
 *	Memory is released, X resources are freed, and the graph is
 *	redrawn.
 *
 * ----------------------------------------------------------------------
 */
/* ARGSUSED */
static void
DestroyImageMarker(graphPtr, markerPtr)
    Graph *graphPtr;		/* Unused */
    Marker *markerPtr;
{
    ImageMarker *imagePtr = (ImageMarker *)markerPtr;

    if (imagePtr->tkImage != NULL) {
	Tk_FreeImage(imagePtr->tkImage);
    }
    if (imagePtr->pixmap != None) {
	Tk_FreePixmap(graphPtr->display, imagePtr->pixmap);
    }
    if (imagePtr->srcImage != NULL) {
	Blt_FreeColorImage(imagePtr->srcImage);
    }
    if (imagePtr->destImage != NULL) {
	Blt_FreeColorImage(imagePtr->destImage);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * CreateImageMarker --
 *
 *	Allocate memory and initialize methods for the new image marker.
 *
 * Results:
 *	The pointer to the newly allocated marker structure is returned.
 *
 * Side effects:
 *	Memory is allocated for the image marker structure.
 *
 * ----------------------------------------------------------------------
 */
static Marker *
CreateImageMarker()
{
    ImageMarker *imagePtr;

    imagePtr = (ImageMarker *)calloc(1, sizeof(ImageMarker));
    if (imagePtr != NULL) {
	imagePtr->configSpecs = imageConfigSpecs;
	imagePtr->configProc = ConfigureImageMarker;
	imagePtr->freeProc = DestroyImageMarker;
	imagePtr->drawProc = DrawImageMarker;
	imagePtr->coordsProc = TransformImageMarker;
	imagePtr->printProc = PrintImageMarker;
	imagePtr->pointProc = PointInImageMarker;
	imagePtr->type = TYPE_MARKER_IMAGE;
    }
    return ((Marker *)imagePtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * ConfigureTextMarker --
 *
 *	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or
 *	reconfigure) a text marker.
 *
 * Results:
 *	A standard Tcl result.  If TCL_ERROR is returned, then
 *	interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as text string, colors, font,
 *	etc. get set for markerPtr;  old resources get freed, if there
 *	were any.  The marker is eventually redisplayed.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ConfigureTextMarker(markerPtr)
    Marker *markerPtr;
{
    Graph *graphPtr = markerPtr->graphPtr;
    TextMarker *textPtr = (TextMarker *)markerPtr;
    GC newGC;
    XGCValues gcValues;
    unsigned int gcMask;

    textPtr->attrs.theta = FMOD(textPtr->attrs.theta, 360.0);
    if (textPtr->attrs.theta < 0.0) {
	textPtr->attrs.theta += 360.0;
    }
    newGC = NULL;
    if (textPtr->fillColor != NULL) {
	gcMask = GCForeground;
	gcValues.foreground = textPtr->fillColor->pixel;
	newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    }
    if (textPtr->fillGC != NULL) {
	Tk_FreeGC(graphPtr->display, textPtr->fillGC);
    }
    textPtr->fillGC = newGC;
    Blt_ResetTextAttributes(graphPtr->tkwin, &(textPtr->attrs));
    textPtr->width = textPtr->height = 0;
    if (textPtr->string != NULL) {
	int width, height;

	Blt_GetTextExtents(&(textPtr->attrs), textPtr->string, &width, &height);
	Blt_GetBoundingBox(width, height, textPtr->attrs.theta,
	    &(textPtr->width), &(textPtr->height), (XPoint *)NULL);
    }
    if (!textPtr->hidden) {
	textPtr->flags |= COORDS_NEEDED;
	if (textPtr->drawUnder) {
	    graphPtr->flags |= REDRAW_BACKING_STORE;
	}
	Blt_EventuallyRedrawGraph(graphPtr);
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * TranformTextMarker --
 *
 *	Calculate the layout position for a text marker.  Positional
 *	information is saved in the marker.  If the text is rotated,
 *	a bitmap containing the text is created.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	If no background color has been specified, the GC stipple
 *	origins are changed to current window coordinates. For both
 *	rotated and non-rotated text, if any old bitmap is leftover,
 *	it is freed.
 *
 * ----------------------------------------------------------------------
 */
static void
TransformTextMarker(markerPtr)
    Marker *markerPtr;
{
    Graph *graphPtr = markerPtr->graphPtr;
    TextMarker *textPtr = (TextMarker *)markerPtr;
    int x, y;
    Extents2D extents;

    if (textPtr->string == NULL) {
	return;
    }
    TransformCoordinate(graphPtr, textPtr->coordArr, &textPtr->axes, &x, &y);
    x += textPtr->xOffset;
    y += textPtr->yOffset;
    Blt_TranslateAnchor(x, y, textPtr->width, textPtr->height, textPtr->anchor,
	&x, &y);
    textPtr->x = x;
    textPtr->y = y;
    /*
     * Determine the bounding box of the text and test to see if it
     * is at least partially contained within the plotting area.
     */
    extents.xMin = (double)textPtr->x;
    extents.yMin = (double)textPtr->y;
    extents.xMax = (double)textPtr->x + textPtr->width;
    extents.yMax = (double)textPtr->y + textPtr->height;
    textPtr->clipped = BoxesDontOverlap(graphPtr, &extents);

}

static int
PointInTextMarker(markerPtr, x, y)
    Marker *markerPtr;
    int x, y;
{
    TextMarker *textPtr = (TextMarker *)markerPtr;

    return ((x >= textPtr->x) && (x < (textPtr->x + textPtr->width)) &&
	(y >= textPtr->y) && (y < (textPtr->y + textPtr->height)));
}

/*
 * ----------------------------------------------------------------------
 *
 * DrawTextMarker --
 *
 *	Draws the text marker on the graph.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Commands are output to X to draw the marker in its current
 *	mode.
 *
 * ----------------------------------------------------------------------
 */
static void
DrawTextMarker(markerPtr, drawable)
    Marker *markerPtr;
    Drawable drawable;		/* Pixmap or window to draw into */
{
    TextMarker *textPtr = (TextMarker *)markerPtr;
    CompoundText *compTextPtr;
    Graph *graphPtr = markerPtr->graphPtr;

    if (textPtr->string == NULL) {
	return;
    }
    compTextPtr = Blt_GetCompoundText(textPtr->string, &(textPtr->attrs));
    if (textPtr->fillGC != NULL) {
	int width, height;
	XPoint pointArr[4];
	int cx, cy;
	register int i;

	/*
	 * Simulate the rotated background of the bitmap by
	 * computing the bounding polygon and filling it with
	 * the background color.
	 */
	width = compTextPtr->width, height = compTextPtr->height;
	Blt_GetBoundingBox(width, height, textPtr->attrs.theta, &width,
	    &height, pointArr);
	Blt_TranslateAnchor(textPtr->x, textPtr->y, width, height,
	    textPtr->attrs.anchor, &cx, &cy);
	/* Translate the polygon to the center of the rotated text */
	cx += (width / 2);
	cy += (height / 2);
	for (i = 0; i < 4; i++) {
	    pointArr[i].x += cx;
	    pointArr[i].y += cy;
	}
	XFillPolygon(graphPtr->display, drawable, textPtr->fillGC, pointArr, 4,
	    Convex, CoordModeOrigin);
    }
    if (textPtr->attrs.color != NULL) {
	Blt_DrawCompoundText(graphPtr->tkwin, drawable, compTextPtr,
	    &(textPtr->attrs), textPtr->x, textPtr->y);
    }
    free((char *)compTextPtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * PrintTextMarker --
 *
 *	Outputs PostScript commands to draw a text marker at a given
 *	x,y coordinate, rotation, anchor, and font.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	PostScript font and color settings are changed.
 *
 * ----------------------------------------------------------------------
 */
static void
PrintTextMarker(markerPtr, printable)
    Marker *markerPtr;
    Printable printable;
{
    TextMarker *textPtr = (TextMarker *)markerPtr;

    if (textPtr->string == NULL) {
	return;
    }
    if (textPtr->fillGC != NULL) {
	int width, height;
	XPoint pointArr[4];
	int cx, cy;
	register int i;
	CompoundText *compTextPtr;

	compTextPtr = Blt_GetCompoundText(textPtr->string, &(textPtr->attrs));
	/*
	 * Simulate the rotated background of the bitmap by
	 * computing the bounding polygon and filling it with
	 * the background color.
	 */
	width = compTextPtr->width;
	height = compTextPtr->height;
	free((char *)compTextPtr);
	Blt_GetBoundingBox(width, height, textPtr->attrs.theta, &width, &height,
	    pointArr);
	/* Translate the anchor to the center of the rotated text */
	cx = textPtr->x + (width / 2);
	cy = textPtr->y + (height / 2);
	for (i = 0; i < 4; i++) {
	    pointArr[i].x += cx;
	    pointArr[i].y += cy;
	}
	Blt_PolygonToPostScript(printable, pointArr, 4);
    }
    Blt_PrintText(printable, textPtr->string, &(textPtr->attrs), textPtr->x,
	textPtr->y);
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyTextMarker --
 *
 *	Destroys the structure containing the attributes of the text
 * 	marker.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Text attributes (GCs, colors, stipple, font, etc) get destroyed.
 *	Memory is released, X resources are freed, and the graph is
 *	redrawn.
 *
 * ----------------------------------------------------------------------
 */
static void
DestroyTextMarker(graphPtr, markerPtr)
    Graph *graphPtr;
    Marker *markerPtr;
{
    TextMarker *textPtr = (TextMarker *)markerPtr;

    Blt_FreeTextAttributes(graphPtr->display, &(textPtr->attrs));
}

/*
 * ----------------------------------------------------------------------
 *
 * CreateTextMarker --
 *
 *	Allocate memory and initialize methods for the new text marker.
 *
 * Results:
 *	The pointer to the newly allocated marker structure is returned.
 *
 * Side effects:
 *	Memory is allocated for the text marker structure.
 *
 * ----------------------------------------------------------------------
 */
static Marker *
CreateTextMarker()
{
    TextMarker *textPtr;

    textPtr = (TextMarker *)calloc(1, sizeof(TextMarker));
    assert(textPtr);

    textPtr->configSpecs = textConfigSpecs;
    textPtr->configProc = ConfigureTextMarker;
    textPtr->freeProc = DestroyTextMarker;
    textPtr->drawProc = DrawTextMarker;
    textPtr->coordsProc = TransformTextMarker;
    textPtr->printProc = PrintTextMarker;
    textPtr->pointProc = PointInTextMarker;
    textPtr->type = TYPE_MARKER_TEXT;
    Blt_InitTextAttributes(&(textPtr->attrs));
    textPtr->attrs.anchor = TK_ANCHOR_NW;
    textPtr->attrs.padLeft = textPtr->attrs.padRight = 4;
    textPtr->attrs.padTop = textPtr->attrs.padBottom = 4;

    return (Marker *) textPtr;
}


static void ChildEventProc _ANSI_ARGS_((ClientData clientData,
	XEvent *eventPtr));
static void ChildGeometryProc _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin));

static void ChildCustodyProc _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin));

static Tk_GeomMgr winMarkerMgrInfo =
{
    "graph",			/* Name of geometry manager used by winfo */
    ChildGeometryProc,		/* Procedure to for new geometry requests */
    ChildCustodyProc,		/* Procedure when window is taken away */
};

/*
 * ----------------------------------------------------------------------
 *
 * ConfigureWindowMarker --
 *
 *	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or reconfigure)
 *	a window marker.
 *
 * Results:
 *	A standard Tcl result.  If TCL_ERROR is returned, then
 *	interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as window pathname, placement,
 *	etc. get set for markerPtr; old resources get freed, if there
 *	were any.  The marker is eventually redisplayed.
 *
 * ----------------------------------------------------------------------
 */
static int
ConfigureWindowMarker(markerPtr)
    Marker *markerPtr;
{
    Graph *graphPtr = markerPtr->graphPtr;
    WindowMarker *winPtr = (WindowMarker *)markerPtr;
    Tk_Window tkwin;

    if (winPtr->pathName == NULL) {
	return TCL_OK;
    }
    tkwin = Tk_NameToWindow(graphPtr->interp, winPtr->pathName, graphPtr->tkwin);
    if (tkwin == NULL) {
	return TCL_ERROR;
    }
    if (Tk_Parent(tkwin) != graphPtr->tkwin) {
	Tcl_AppendResult(graphPtr->interp, "\"", winPtr->pathName,
	    "\" is not a child of \"", Tk_PathName(graphPtr->tkwin), "\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    if (tkwin != winPtr->tkwin) {
	if (winPtr->tkwin != NULL) {
	    Tk_DeleteEventHandler(winPtr->tkwin, StructureNotifyMask,
		ChildEventProc, (ClientData)winPtr);
	    Tk_ManageGeometry(winPtr->tkwin, (Tk_GeomMgr *) 0, (ClientData)0);
	    Tk_UnmapWindow(winPtr->tkwin);
	}
	Tk_CreateEventHandler(tkwin, StructureNotifyMask, ChildEventProc,
	    (ClientData)winPtr);
	Tk_ManageGeometry(tkwin, &winMarkerMgrInfo, (ClientData)winPtr);
    }
    winPtr->tkwin = tkwin;

    if (!winPtr->hidden) {
	winPtr->flags |= COORDS_NEEDED;
	if (winPtr->drawUnder) {
	    graphPtr->flags |= REDRAW_BACKING_STORE;
	}
	Blt_EventuallyRedrawGraph(graphPtr);
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * TransformWindowMarker --
 *
 *	Calculate the layout position for a window marker.  Positional
 *	information is saved in the marker.
 *
 * Results:
 *	None.
 *
 * ----------------------------------------------------------------------
 */
static void
TransformWindowMarker(markerPtr)
    Marker *markerPtr;
{
    WindowMarker *winPtr = (WindowMarker *)markerPtr;
    Graph *graphPtr = markerPtr->graphPtr;
    Extents2D extents;
    int x, y, width, height;

    if (winPtr->tkwin == (Tk_Window)NULL) {
	return;
    }
    TransformCoordinate(graphPtr, winPtr->coordArr, &winPtr->axes, &x, &y);

    width = (winPtr->reqWidth > 0)
	? winPtr->reqWidth : Tk_ReqWidth(winPtr->tkwin);
    height = (winPtr->reqHeight > 0)
	? winPtr->reqHeight : Tk_ReqHeight(winPtr->tkwin);
    x += winPtr->xOffset;
    y += winPtr->yOffset;
    Blt_TranslateAnchor(x, y, width, height, winPtr->anchor, &x, &y);

    winPtr->x = x;
    winPtr->y = y;
    winPtr->width = width;
    winPtr->height = height;

    /*
     * Determine the bounding box of the window and test to see if it
     * is at least partially contained within the plotting area.
     */
    extents.xMin = (double)winPtr->x;
    extents.yMax = (double)winPtr->y + winPtr->height;
    extents.xMax = (double)winPtr->x + winPtr->width;
    extents.yMin = (double)winPtr->y;
    winPtr->clipped = BoxesDontOverlap(graphPtr, &extents);
}

/*
 * ----------------------------------------------------------------------
 *
 * PointInWindowMarker --
 *
 * ----------------------------------------------------------------------
 */
static int
PointInWindowMarker(markerPtr, x, y)
    Marker *markerPtr;
    int x, y;
{
    WindowMarker *winPtr = (WindowMarker *)markerPtr;

    return ((x >= winPtr->x) && (x < (winPtr->x + winPtr->width)) &&
	(y >= winPtr->y) && (y < (winPtr->y + winPtr->height)));
}

/*
 * ----------------------------------------------------------------------
 *
 * DrawWindowMarker --
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static void
DrawWindowMarker(markerPtr, drawable)
    Marker *markerPtr;
    Drawable drawable;		/* Pixmap or window to draw into */
{
    WindowMarker *winPtr = (WindowMarker *)markerPtr;

    if (winPtr->tkwin == NULL) {
	return;
    }
    if ((winPtr->height != Tk_Height(winPtr->tkwin)) ||
	(winPtr->width != Tk_Width(winPtr->tkwin)) ||
	(winPtr->x != Tk_X(winPtr->tkwin)) ||
	(winPtr->y != Tk_Y(winPtr->tkwin))) {
	Tk_MoveResizeWindow(winPtr->tkwin, winPtr->x, winPtr->y,
	    winPtr->width, winPtr->height);
    }
    if (!Tk_IsMapped(winPtr->tkwin)) {
	Tk_MapWindow(winPtr->tkwin);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * PrintWindowMarker --
 *
 * ----------------------------------------------------------------------
 */
static void
PrintWindowMarker(markerPtr, printable)
    Marker *markerPtr;
    Printable printable;
{
    WindowMarker *winPtr = (WindowMarker *)markerPtr;

    if (winPtr->tkwin == NULL) {
	return;
    }
    if (Tk_IsMapped(winPtr->tkwin)) {
	Blt_WindowToPostScript(printable, winPtr->tkwin, winPtr->x, winPtr->y);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyWindowMarker --
 *
 *	Destroys the structure containing the attributes of the window
 *      marker.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Window is destroyed and removed from the screen.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static void
DestroyWindowMarker(graphPtr, markerPtr)
    Graph *graphPtr;
    Marker *markerPtr;
{
    WindowMarker *winPtr = (WindowMarker *)markerPtr;

    if (winPtr->tkwin != NULL) {
	Tk_DeleteEventHandler(winPtr->tkwin, StructureNotifyMask,
	    ChildEventProc, (ClientData)winPtr);
	Tk_ManageGeometry(winPtr->tkwin, (Tk_GeomMgr *) 0, (ClientData)0);
	Tk_DestroyWindow(winPtr->tkwin);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * CreateWindowMarker --
 *
 *	Allocate memory and initialize methods for the new window marker.
 *
 * Results:
 *	The pointer to the newly allocated marker structure is returned.
 *
 * Side effects:
 *	Memory is allocated for the window marker structure.
 *
 * ----------------------------------------------------------------------
 */
static Marker *
CreateWindowMarker()
{
    WindowMarker *winPtr;

    winPtr = (WindowMarker *)calloc(1, sizeof(WindowMarker));
    if (winPtr != NULL) {
	winPtr->configSpecs = windowConfigSpecs;
	winPtr->configProc = ConfigureWindowMarker;
	winPtr->freeProc = DestroyWindowMarker;
	winPtr->drawProc = DrawWindowMarker;
	winPtr->coordsProc = TransformWindowMarker;
	winPtr->printProc = PrintWindowMarker;
	winPtr->pointProc = PointInWindowMarker;
	winPtr->type = TYPE_MARKER_WINDOW;
    }
    return ((Marker *)winPtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * ChildEventProc --
 *
 *	This procedure is invoked whenever StructureNotify events
 *	occur for a window that's managed as part of a graph window
 *	marker. This procedure's only purpose is to clean up when
 *	windows are deleted.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The window is disassociated from the window item when it is
 *	deleted.
 *
 * ----------------------------------------------------------------------
 */
static void
ChildEventProc(clientData, eventPtr)
    ClientData clientData;	/* Pointer to record describing window item. */
    XEvent *eventPtr;		/* Describes what just happened. */
{
    WindowMarker *winPtr = (WindowMarker *)clientData;

    if (eventPtr->type == DestroyNotify) {
	winPtr->tkwin = NULL;
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * ChildGeometryProc --
 *
 *	This procedure is invoked whenever a window that's associated
 *	with a window item changes its requested dimensions.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The size and location on the window of the window may change,
 *	depending on the options specified for the window item.
 *
 * ----------------------------------------------------------------------
 */
/* ARGSUSED */
static void
ChildGeometryProc(clientData, tkwin)
    ClientData clientData;	/* Pointer to record for window item. */
    Tk_Window tkwin;		/* Window that changed its desired size. */
{
    WindowMarker *winPtr = (WindowMarker *)clientData;

    if (winPtr->reqWidth == 0) {
	winPtr->width = Tk_ReqWidth(tkwin);
    }
    if (winPtr->reqHeight == 0) {
	winPtr->height = Tk_ReqHeight(tkwin);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * ChildCustodyProc --
 *
 *	This procedure is invoked when a slave window has been
 *	stolen by another geometry manager.  The information and
 *	memory associated with the slave window is released.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Arranges for the widget formerly associated with the slave
 *	window to have its layout re-computed and arranged at the
 *	next idle point.
 *
 * ----------------------------------------------------------------------
 */
 /* ARGSUSED */
static void
ChildCustodyProc(clientData, tkwin)
    ClientData clientData;	/* Window marker to be destroyed. */
    Tk_Window tkwin;		/* Not used. */
{
    Marker *markerPtr = (Marker *)clientData;
    Graph *graphPtr;

    graphPtr = markerPtr->graphPtr;
    DestroyMarker(markerPtr);
    /*
     * Not really needed. We should get an Expose event when the
     * child window is unmapped
     */
    Blt_EventuallyRedrawGraph(graphPtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * TransformLine --
 *
 *	Calculate the layout position for a line marker.  Positional
 *	information is saved in the marker.  The line positions are
 *	stored in an array of points (malloc'ed).
 *
 * Results:
 *	None.
 *
 * ----------------------------------------------------------------------
 */
static void
TransformLineMarker(markerPtr)
    Marker *markerPtr;
{
    Graph *graphPtr = markerPtr->graphPtr;
    LineMarker *linePtr = (LineMarker *)markerPtr;
    int x, y;
    register Point2D *p;
    XSegment *segArr;
    int count;
    register int i;
    Point2D p1, p2;
    Extents2D extents;

    linePtr->numSegments = 0;
    if (linePtr->segArr != NULL) {
	free((char *)linePtr->segArr);
    }
    if (linePtr->numCoords < 2) {
	return;			/* Too few points */
    }
    Blt_SetClipRegion(graphPtr, &extents);
    segArr = (XSegment *)calloc(linePtr->numCoords, sizeof(XSegment));

    count = 0;
    p = linePtr->coordArr;
    TransformCoordinate(graphPtr, p, &linePtr->axes, &x, &y);
    p1.x = x + linePtr->xOffset;
    p1.y = y + linePtr->yOffset;

    p++;
    for (i = 1; i < linePtr->numCoords; i++, p++) {
	TransformCoordinate(graphPtr, p, &linePtr->axes, &x, &y);
	p2.x = x + linePtr->xOffset;
	p2.y = y + linePtr->yOffset;
	if (Blt_ClipSegment(&extents, &p1, &p2, segArr + count)) {
	    count++;
	}
	p1 = p2;
    }
    linePtr->numSegments = count;
    linePtr->segArr = segArr;
    linePtr->clipped = (linePtr->numSegments == 0);
}

static int
PointInLineMarker(markerPtr, x, y)
    Marker *markerPtr;
    int x, y;
{
    LineMarker *linePtr = (LineMarker *)markerPtr;
    XSegment *segPtr;
    Point2D proj;
    int xMin, xMax, yMin, yMax;
    register int i;
    double bx, by;
    double dist, minDist;

    minDist = bltPosInfinity;
    segPtr = linePtr->segArr;
    for (i = 0; i < linePtr->numSegments; i++) {
	proj = Blt_GetProjection(x, y, segPtr->x1, segPtr->y1, segPtr->x2,
	    segPtr->y2);
	if (segPtr->x1 > segPtr->x2) {
	    xMax = segPtr->x1, xMin = segPtr->x2;
	} else {
	    xMax = segPtr->x2, xMin = segPtr->x1;
	}
	if (segPtr->y1 > segPtr->y2) {
	    yMax = segPtr->y1, yMin = segPtr->y2;
	} else {
	    yMax = segPtr->y2, yMin = segPtr->y1;
	}
	bx = BOUND(proj.x, xMin, xMax);
	by = BOUND(proj.y, yMin, yMax);
	dist = hypot(bx - x, by - y);
	if (dist < minDist) {
	    minDist = dist;
	}
    }
    return (minDist < (double)markerPtr->graphPtr->halo);
}

static void
DrawLineMarker(markerPtr, drawable)
    Marker *markerPtr;
    Drawable drawable;		/* Pixmap or window to draw into */
{
    LineMarker *linePtr = (LineMarker *)markerPtr;

    if (linePtr->numSegments > 0) {
	Graph *graphPtr = markerPtr->graphPtr;

	XDrawSegments(graphPtr->display, drawable, linePtr->gc, linePtr->segArr,
	    linePtr->numSegments);
	/* Toggle the drawing state */
	if (linePtr->xor) {
	    linePtr->xorState = (linePtr->xorState == 0);
	}
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * ConfigureLineMarker --
 *
 *	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or reconfigure)
 *	a line marker.
 *
 * Results:
 *	A standard Tcl result.  If TCL_ERROR is returned, then
 *	interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as line width, colors, dashes,
 *	etc. get set for markerPtr; old resources get freed, if there
 *	were any.  The marker is eventually redisplayed.
 *
 * ---------------------------------------------------------------------- 
 */
/*ARGSUSED*/
static int
ConfigureLineMarker(markerPtr)
    Marker *markerPtr;
{
    Graph *graphPtr = markerPtr->graphPtr;
    LineMarker *linePtr = (LineMarker *)markerPtr;
    GC newGC;
    XGCValues gcValues;
    unsigned long gcMask;
    Drawable drawable;

    drawable = Tk_WindowId(graphPtr->tkwin);
    gcMask = (GCLineWidth | GCLineStyle | GCCapStyle | GCJoinStyle);
    if (linePtr->outlineColor != NULL) {
	gcMask |= GCForeground;
	gcValues.foreground = linePtr->outlineColor->pixel;
    }
    if (linePtr->fillColor != NULL) {
	gcMask |= GCBackground;
	gcValues.background = linePtr->fillColor->pixel;
    }
    gcValues.cap_style = linePtr->capStyle;
    gcValues.join_style = linePtr->joinStyle;
    gcValues.line_width = LineWidth(linePtr->lineWidth);
    gcValues.line_style = LineSolid;
    if (linePtr->dashes.numValues > 0) {
	gcValues.line_style = 
	    (gcMask & GCBackground) ? LineDoubleDash : LineOnOffDash;
    }
    if (linePtr->xor) {
	unsigned long pixel;
	gcValues.function = GXxor;

	gcMask |= GCFunction;
	if (graphPtr->plotBg == NULL) {
	    pixel = WhitePixelOfScreen(Tk_Screen(graphPtr->tkwin));
	} else {
	    pixel = graphPtr->plotBg->pixel;
	}
	if (gcMask & GCBackground) {
	    gcValues.background ^= pixel;
	}
	gcValues.foreground ^= pixel;
	if (drawable != None) {
	    DrawLineMarker(markerPtr, drawable);
	}
    }
    newGC = Blt_GetPrivateGC(graphPtr->tkwin, gcMask, &gcValues);
    if (linePtr->gc != NULL) {
	Blt_FreePrivateGC(graphPtr->display, linePtr->gc);
    }
    if (linePtr->dashes.numValues > 0) {
	Blt_SetDashes(graphPtr->display, newGC, &(linePtr->dashes));
    }
    linePtr->gc = newGC;
    if (linePtr->xor) {
	if (drawable != None) {
	    TransformLineMarker(markerPtr);
	    DrawLineMarker(markerPtr, drawable);
	}
	return TCL_OK;
    }
    if (!linePtr->hidden) {
	linePtr->flags |= COORDS_NEEDED;
	if (linePtr->drawUnder) {
	    graphPtr->flags |= REDRAW_BACKING_STORE;
	}
	Blt_EventuallyRedrawGraph(graphPtr);
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * PrintLineMarker --
 *
 *	Prints postscript commands to display the connect line.
 *	Dashed lines need to be handled specially, especially if a
 *	background color is designated.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	PostScript output commands are saved in the interpreter
 *	(infoPtr->interp) result field.
 *
 * ---------------------------------------------------------------------- 
 */
static void
PrintLineMarker(markerPtr, printable)
    Marker *markerPtr;
    Printable printable;
{
    LineMarker *linePtr = (LineMarker *)markerPtr;

    if (linePtr->numSegments > 0) {
	Blt_LineAttributesToPostScript(printable, linePtr->outlineColor, 
		linePtr->lineWidth, &(linePtr->dashes), linePtr->capStyle, 
		linePtr->joinStyle);
	if ((linePtr->dashes.numValues > 0) && (linePtr->fillColor != NULL)) {
	    Blt_PrintAppend(printable, "/DashesProc {\n  gsave\n    ",
		(char *)NULL);
	    Blt_BackgroundToPostScript(printable, linePtr->fillColor);
	    Blt_PrintAppend(printable, "    ", (char *)NULL);
	    Blt_LineDashesToPostScript(printable, (Dashes *)NULL);
	    Blt_PrintAppend(printable,
		"stroke\n",
		"  grestore\n",
		"} def\n", (char *)NULL);
	} else {
	    Blt_PrintAppend(printable, "/DashesProc {} def\n",
		(char *)NULL);
	}
	Blt_SegmentsToPostScript(printable, linePtr->segArr,
	    linePtr->numSegments);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyLineMarker --
 *
 *	Destroys the structure and attributes of a line marker.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Line attributes (GCs, colors, stipple, etc) get released.
 *	Memory is deallocated, X resources are freed.
 *
 * ----------------------------------------------------------------------
 */
static void
DestroyLineMarker(graphPtr, markerPtr)
    Graph *graphPtr;
    Marker *markerPtr;
{
    LineMarker *linePtr = (LineMarker *)markerPtr;

    if (linePtr->gc != NULL) {
	Blt_FreePrivateGC(graphPtr->display, linePtr->gc);
    }
    if (linePtr->segArr != NULL) {
	free((char *)linePtr->segArr);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * CreateLineMarker --
 *
 *	Allocate memory and initialize methods for a new line marker.
 *
 * Results:
 *	The pointer to the newly allocated marker structure is returned.
 *
 * Side effects:
 *	Memory is allocated for the line marker structure.
 *
 * ----------------------------------------------------------------------
 */
static Marker *
CreateLineMarker()
{
    LineMarker *linePtr;

    linePtr = (LineMarker *)calloc(1, sizeof(LineMarker));
    if (linePtr != NULL) {
	linePtr->configSpecs = lineConfigSpecs;
	linePtr->configProc = ConfigureLineMarker;
	linePtr->freeProc = DestroyLineMarker;
	linePtr->drawProc = DrawLineMarker;
	linePtr->coordsProc = TransformLineMarker;
	linePtr->printProc = PrintLineMarker;
	linePtr->pointProc = PointInLineMarker;
	linePtr->type = TYPE_MARKER_LINE;
	linePtr->xor = FALSE;
	linePtr->capStyle = CapButt;
	linePtr->joinStyle = JoinMiter;
    }
    return ((Marker *)linePtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * TransformPolygonMarker --
 *
 *	Calculate the layout position for a polygon marker.  Positional
 *	information is saved in the polygon in an array of points
 *	(malloc'ed).
 *
 * Results:
 *	None.
 *
 * ----------------------------------------------------------------------
 */
static void
TransformPolygonMarker(markerPtr)
    Marker *markerPtr;
{
    Graph *graphPtr = markerPtr->graphPtr;
    PolygonMarker *polyPtr = (PolygonMarker *)markerPtr;
    register int i;
    XPoint *pointArr;
    register XPoint *pointPtr;
    int x, y;
    Extents2D extents;
    int numPoints;

    if (polyPtr->pointArr != NULL) {
	free((char *)polyPtr->pointArr);
	polyPtr->pointArr = NULL;
    }
    polyPtr->numPoints = 0;
    if (polyPtr->numCoords < 3) {
	return;			/* Too few points */
    }
    numPoints = polyPtr->numCoords + 1;
    pointArr = (XPoint *)malloc(numPoints * sizeof(XPoint));
    if (pointArr == NULL) {
	return;			/* Can't allocate point array */
    }
    pointPtr = pointArr;

    /*
     * Determine the bounding box of the polygon and test to see if it
     * is at least partially contained within the plotting area.
     */
    extents.yMin = extents.xMin = bltPosInfinity;
    extents.yMax = extents.xMax = bltNegInfinity;
    for (i = 0; i < polyPtr->numCoords; i++) {
	TransformCoordinate(graphPtr, polyPtr->coordArr + i, &polyPtr->axes,
	    &x, &y);
	x += polyPtr->xOffset;
	y += polyPtr->yOffset;
	pointPtr->x = x;
	if (x > extents.xMax) {
	    extents.xMax = x;
	} else if (x < extents.xMin) {
	    extents.xMin = x;
	}
	pointPtr->y = ROUND(y);
	if (y > extents.yMax) {
	    extents.yMax = y;
	} else if (y < extents.yMin) {
	    extents.yMin = y;
	}
	BoundPoint(pointPtr);	/* Bound the points of the polygon to
				 * fit the size of a signed short int.
				 * We'll let X do the clipping here,
				 * because we really don't want to
				 * write a polygon clipping routine.
				 * Just yet. */
	pointPtr++;
    }
    *pointPtr = pointArr[0];	/* Ensure that the polygon is closed */

    polyPtr->clipped = BoxesDontOverlap(graphPtr, &extents);
    polyPtr->clipped = 0;
    if (polyPtr->clipped) {
	polyPtr->numPoints = 0;
	free((char *)pointArr);
    } else {
	polyPtr->numPoints = numPoints;
	polyPtr->pointArr = pointArr;
    }
}

static int
PointInPolygonMarker(markerPtr, x, y)
    Marker *markerPtr;
    int x, y;
{
    PolygonMarker *polyPtr = (PolygonMarker *)markerPtr;
    register int i;
    register float x1, x2, y1, y2;
    register int intersections;

    intersections = 0;
    for (i = 1; i < polyPtr->numPoints; i++) {
	x1 = (float)polyPtr->pointArr[i - 1].x;
	y1 = (float)polyPtr->pointArr[i - 1].y;
	x2 = (float)polyPtr->pointArr[i].x;
	y2 = (float)polyPtr->pointArr[i].y;
	if (((y1 <= y) && (y < y2)) || ((y2 <= y) && (y < y1))) {
	    float b;

	    b = (x2 - x1) * (y - y1) / (y2 - y1) + x1;
	    if (x < b) {
		intersections++;
	    }
	}
    }
    return (intersections & 0x01);
}

static void
DrawPolygonMarker(markerPtr, drawable)
    Marker *markerPtr;
    Drawable drawable;		/* Pixmap or window to draw into */
{
    Graph *graphPtr = markerPtr->graphPtr;
    PolygonMarker *polyPtr = (PolygonMarker *)markerPtr;

    if (polyPtr->numPoints < 3) {
	return;
    }
    if (polyPtr->fill.fgColor != NULL) {
	XFillPolygon(graphPtr->display, drawable, polyPtr->fillGC,
	    polyPtr->pointArr, polyPtr->numPoints, Complex,
	    CoordModeOrigin);
    }
    if ((polyPtr->lineWidth > 0) && (polyPtr->outline.fgColor != NULL)) {
	XDrawLines(graphPtr->display, drawable, polyPtr->outlineGC,
	    polyPtr->pointArr, polyPtr->numPoints, CoordModeOrigin);
    }
}


static void
PrintPolygonMarker(markerPtr, printable)
    Marker *markerPtr;
    Printable printable;
{
    Graph *graphPtr = markerPtr->graphPtr;
    PolygonMarker *polyPtr = (PolygonMarker *)markerPtr;

    if (polyPtr->numPoints < 3) {
	return;
    }
    Blt_LinesToPostScript(printable, polyPtr->pointArr, polyPtr->numPoints);
    Blt_PrintAppend(printable, "closepath\n", (char *)NULL);

    if (polyPtr->fill.fgColor != NULL) {
	/*
	 * Options:  fg bg
	 *			Draw outline only.
	 *	     x          Draw solid or stipple.
	 *	     x  x       Draw solid or stipple.
	 */
	/*
	 * If the background fill color was specified, draw the polygon
	 * in a solid fashion with that color.
	 */
	if (polyPtr->fill.bgColor != NULL) {
	    Blt_BackgroundToPostScript(printable, polyPtr->fill.bgColor);
	    Blt_PrintAppend(printable, "Fill\n", (char *)NULL);
	}
	Blt_ForegroundToPostScript(printable, polyPtr->fill.fgColor);
	if (polyPtr->stipple != None) {
	    int width, height;
	    /*
	     * Draw the stipple in the foreground color.
	     */
	    Tk_SizeOfBitmap(graphPtr->display, polyPtr->stipple,
		&width, &height);
	    Blt_StippleToPostScript(printable, graphPtr->display,
		polyPtr->stipple, width, height, True);
	} else {
	    Blt_PrintAppend(printable, "Fill\n", (char *)NULL);
	}
    }
    /*
     * Draw the outline in the foreground color.
     */
    if ((polyPtr->lineWidth > 0) && (polyPtr->outline.fgColor != NULL)) {
	/*
	 * Set up the line attributes.  We set up a PostScript procedure
	 * "DashesProc" that gets invoke during the Polygon drawing routine.
	 * If the line isn't dashed, we simply make this an empty definition.
	 */
	Blt_LineAttributesToPostScript(printable, polyPtr->outline.fgColor,
	    polyPtr->lineWidth, &(polyPtr->dashes), polyPtr->capStyle,
	    polyPtr->joinStyle);
	if ((polyPtr->outline.bgColor != NULL) &&
	    (polyPtr->dashes.numValues > 0)) {
	    Blt_PrintAppend(printable,
		"/DashesProc {\n",
		"gsave\n    ", (char *)NULL);
	    Blt_BackgroundToPostScript(printable, polyPtr->outline.bgColor);
	    Blt_PrintAppend(printable, "    ", (char *)NULL);
	    Blt_LineDashesToPostScript(printable, (Dashes *)NULL);
	    Blt_PrintAppend(printable,
		"stroke\n",
		"  grestore\n",
		"} def\n", (char *)NULL);
	} else {
	    Blt_PrintAppend(printable, "/DashesProc {} def\n",
		(char *)NULL);
	}
	Blt_PrintLine(printable, polyPtr->pointArr, polyPtr->numPoints);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * ConfigurePolygonMarker --
 *
 *	This procedure is called to process an argv/argc list, plus
 *	the Tk option database, in order to configure (or
 *	reconfigure) a polygon marker.
 *
 * Results:
 *	A standard Tcl result.  If TCL_ERROR is returned, then
 *	interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information, such as polygon color, dashes, fillstyle,
 *	etc. get set for markerPtr;  old resources get freed, if there
 *	were any.  The marker is eventually redisplayed.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ConfigurePolygonMarker(markerPtr)
    Marker *markerPtr;
{
    Graph *graphPtr = markerPtr->graphPtr;
    PolygonMarker *polyPtr = (PolygonMarker *)markerPtr;
    GC newGC;
    XGCValues gcValues;
    unsigned long gcMask;
    Drawable drawable;

    drawable = Tk_WindowId(graphPtr->tkwin);
    gcMask = (GCLineWidth | GCLineStyle);
    if (polyPtr->outline.fgColor != NULL) {
	gcMask |= GCForeground;
	gcValues.foreground = polyPtr->outline.fgColor->pixel;
    }
    if (polyPtr->outline.bgColor != NULL) {
	gcMask |= GCBackground;
	gcValues.background = polyPtr->outline.bgColor->pixel;
    }
    gcMask |= (GCCapStyle | GCJoinStyle);
    gcValues.cap_style = polyPtr->capStyle;
    gcValues.join_style = polyPtr->joinStyle;
    gcValues.line_style = LineSolid;
    gcValues.dash_offset = 0;
    gcValues.line_width = LineWidth(polyPtr->lineWidth);
    if (polyPtr->dashes.numValues > 0) {
	gcValues.line_style = (polyPtr->outline.bgColor == NULL)
	    ? LineOnOffDash : LineDoubleDash;
    }
    if (polyPtr->xor) {
	unsigned long pixel;
	gcValues.function = GXxor;

	gcMask |= GCFunction;
	if (graphPtr->plotBg == NULL) {
	    /* The graph's color option may not have been set yet */
	    pixel = WhitePixelOfScreen(Tk_Screen(graphPtr->tkwin));
	} else {
	    pixel = graphPtr->plotBg->pixel;
	}
	if (gcMask & GCBackground) {
	    gcValues.background ^= pixel;
	}
	gcValues.foreground ^= pixel;
	if (drawable != None) {
	    DrawPolygonMarker(markerPtr, drawable);
	}
    }
    newGC = Blt_GetPrivateGC(graphPtr->tkwin, gcMask, &gcValues);
    if (polyPtr->dashes.numValues > 0) {
	Blt_SetDashes(graphPtr->display, newGC, &(polyPtr->dashes));
    }
    if (polyPtr->outlineGC != NULL) {
	Blt_FreePrivateGC(graphPtr->display, polyPtr->outlineGC);
    }
    polyPtr->outlineGC = newGC;

    gcMask = 0;
    if (polyPtr->fill.fgColor != NULL) {
	gcMask |= GCForeground;
	gcValues.foreground = polyPtr->fill.fgColor->pixel;
    }
    if (polyPtr->fill.bgColor != NULL) {
	gcMask |= GCBackground;
	gcValues.background = polyPtr->fill.bgColor->pixel;
    }
    if (polyPtr->stipple != None) {
	gcValues.stipple = polyPtr->stipple;
	gcValues.fill_style = (polyPtr->fill.bgColor != NULL)
	    ? FillOpaqueStippled : FillStippled;
	gcMask |= (GCStipple | GCFillStyle);
    }
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (polyPtr->fillGC != NULL) {
	Tk_FreeGC(graphPtr->display, polyPtr->fillGC);
    }
    polyPtr->fillGC = newGC;

    if ((gcMask == 0) && !(graphPtr->flags & RESET_AXES) && (polyPtr->xor)) {
	if (drawable != None) {
	    TransformPolygonMarker(markerPtr);
	    DrawPolygonMarker(markerPtr, drawable);
	}
	return TCL_OK;
    }
    if (!polyPtr->hidden) {
	polyPtr->flags |= COORDS_NEEDED;
	if (polyPtr->drawUnder) {
	    graphPtr->flags |= REDRAW_BACKING_STORE;
	}
	Blt_EventuallyRedrawGraph(graphPtr);
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyPolygonMarker --
 *
 *	Release memory and resources allocated for the polygon element.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Everything associated with the polygon element is freed up.
 *
 * ----------------------------------------------------------------------
 */
static void
DestroyPolygonMarker(graphPtr, markerPtr)
    Graph *graphPtr;
    Marker *markerPtr;
{
    PolygonMarker *polyPtr = (PolygonMarker *)markerPtr;

    if (polyPtr->fillGC != NULL) {
	Tk_FreeGC(graphPtr->display, polyPtr->fillGC);
    }
    if (polyPtr->outlineGC != NULL) {
	Blt_FreePrivateGC(graphPtr->display, polyPtr->outlineGC);
    }
    if (polyPtr->pointArr != NULL) {
	free((char *)polyPtr->pointArr);
    }
    Blt_FreeColorPair(&polyPtr->outline);
    Blt_FreeColorPair(&polyPtr->fill);
}

/*
 * ----------------------------------------------------------------------
 *
 * CreatePolygonMarker --
 *
 *	Allocate memory and initialize methods for the new polygon marker.
 *
 * Results:
 *	The pointer to the newly allocated marker structure is returned.
 *
 * Side effects:
 *	Memory is allocated for the polygon marker structure.
 *
 * ----------------------------------------------------------------------
 */
static Marker *
CreatePolygonMarker()
{
    PolygonMarker *polyPtr;

    polyPtr = (PolygonMarker *)calloc(1, sizeof(PolygonMarker));
    if (polyPtr != NULL) {
	polyPtr->configSpecs = polygonConfigSpecs;
	polyPtr->configProc = ConfigurePolygonMarker;
	polyPtr->freeProc = DestroyPolygonMarker;
	polyPtr->drawProc = DrawPolygonMarker;
	polyPtr->coordsProc = TransformPolygonMarker;
	polyPtr->printProc = PrintPolygonMarker;
	polyPtr->pointProc = PointInPolygonMarker;
	polyPtr->type = TYPE_MARKER_POLYGON;
	polyPtr->capStyle = CapButt;
	polyPtr->joinStyle = JoinMiter;

    }
    return ((Marker *)polyPtr);
}

int
Blt_NameToMarker(graphPtr, name, markerPtrPtr)
    Graph *graphPtr;
    char *name;
    Marker **markerPtrPtr;
{
    Tk_Uid nameId;

    nameId = Blt_FindUid(name);
    if (nameId != NULL) {
	Tcl_HashEntry *hPtr;

	hPtr = Tcl_FindHashEntry(&(graphPtr->markerTable), nameId);
	if (hPtr != NULL) {
	    *markerPtrPtr = (Marker *)Tcl_GetHashValue(hPtr);
	    return TCL_OK;
	}
    } 
    Tcl_AppendResult(graphPtr->interp, "can't find marker \"", name, "\" in \"", 
	Tk_PathName(graphPtr->tkwin), (char *)NULL);
    return TCL_ERROR;
}

/*
 * ----------------------------------------------------------------------
 *
 * StringToMarkerType --
 *
 *	Convert the marker type string value into a numeric value.
 *
 * Results:
 *	The value representing the marker type is returned.
 *
 * ----------------------------------------------------------------------
 */
static ObjectType
StringToMarkerType(interp, string)
    Tcl_Interp *interp;
    char *string;
{
    ObjectType type;
    unsigned int length;
    char c;

    c = string[0];
    length = strlen(string);
    if ((c == 't') && (strncmp(string, "text", length) == 0)) {
	type = TYPE_MARKER_TEXT;
    } else if ((c == 'l') && (strncmp(string, "line", length) == 0)) {
	type = TYPE_MARKER_LINE;
    } else if ((c == 'b') && (strncmp(string, "bitmap", length) == 0)) {
	type = TYPE_MARKER_BITMAP;
    } else if ((c == 'i') && (strncmp(string, "image", length) == 0)) {
	type = TYPE_MARKER_IMAGE;
    } else if ((c == 'p') && (strncmp(string, "polygon", length) == 0)) {
	type = TYPE_MARKER_POLYGON;
    } else if ((c == 'w') && (strncmp(string, "window", length) == 0)) {
	type = TYPE_MARKER_WINDOW;
    } else {
	Tcl_AppendResult(interp, "unknown marker type \"", string,
    "\": should be \"text\", \"line\", \"polygon\", \"bitmap\", \"image\", or \
\"window\"", (char *)NULL);
	return TYPE_UNKNOWN;
    }
    return type;
}

/*
 * ----------------------------------------------------------------------
 *
 * MarkerTypeToString --
 *
 *	Convert the marker type value into a string.
 *
 * Results:
 *	The string representing the marker type is returned.
 *
 * ----------------------------------------------------------------------
 */
static char *
MarkerTypeToString(type)
    ObjectType type;
{
    switch (type) {
    case TYPE_MARKER_BITMAP:
	return "bitmap";
    case TYPE_MARKER_IMAGE:
	return "image";
    case TYPE_MARKER_LINE:
	return "line";
    case TYPE_MARKER_POLYGON:
	return "polygon";
    case TYPE_MARKER_TEXT:
	return "text";
    case TYPE_MARKER_WINDOW:
	return "window";
    default:
	return "unknown marker type";
    }
}

static int
RenameMarker(graphPtr, markerPtr, oldId, newId)
    Graph *graphPtr;
    Marker *markerPtr;
    Tk_Uid oldId, newId;
{
    int isNew;
    Tcl_HashEntry *hPtr;
    /*
     * Rename the marker only if no marker already exists by that name
     */
    hPtr = Tcl_CreateHashEntry(&(graphPtr->markerTable), newId, &isNew);
    if (!isNew) {
	Tcl_AppendResult(graphPtr->interp, "can't rename marker: \"", newId,
		 "\" already exists", (char *)NULL);
	return TCL_ERROR;
    }
    markerPtr->nameId = newId;
    markerPtr->hashPtr = hPtr;
    Tcl_SetHashValue(hPtr, (char *)markerPtr);
    /* Delete the old hash entry */
    hPtr = Tcl_FindHashEntry(&(graphPtr->markerTable), oldId);
    Blt_FreeUid(oldId);
    Tcl_DeleteHashEntry(hPtr);
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * NamesOp --
 *
 *	Returns a list of marker identifiers in interp->result;
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * ----------------------------------------------------------------------
 */
static int
NamesOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Marker *markerPtr;
    Blt_ListItem item;
    register int i;

    Tcl_ResetResult(interp);
    for (item = Blt_ListFirstItem(&(graphPtr->markerList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	markerPtr = (Marker *)Blt_ListGetValue(item);
	if (argc == 3) {
	    Tcl_AppendElement(interp, markerPtr->nameId);
	    continue;
	}
	for (i = 3; i < argc; i++) {
	    if (Tcl_StringMatch(markerPtr->nameId, argv[i])) {
		Tcl_AppendElement(interp, markerPtr->nameId);
		break;
	    }
	}
    }
    return TCL_OK;
}

ClientData
Blt_MakeMarkerTag(graphPtr, tagName)
    Graph *graphPtr;
    char *tagName;
{
    Tcl_HashEntry *hPtr;
    int isNew;

    hPtr = Tcl_CreateHashEntry(&(graphPtr->markerTagTable), tagName, &isNew);
    return (ClientData) Tcl_GetHashKey(&(graphPtr->markerTagTable), hPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * BindOp --
 *
 *	.g element bind elemName sequence command
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
BindOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    return Blt_ConfigureBindings(interp, graphPtr->bindTable,
	Blt_MakeMarkerTag(graphPtr, argv[3]), argc - 4, argv + 4);
}

/*
 * ----------------------------------------------------------------------
 *
 * CgetOp --
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
CgetOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Marker *markerPtr;

    if (Blt_NameToMarker(graphPtr, argv[3], &markerPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Tk_ConfigureValue(interp, graphPtr->tkwin, markerPtr->configSpecs,
	    (char *)markerPtr, argv[4], 0) != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * ConfigureOp --
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *
 * ----------------------------------------------------------------------
 */
static int
ConfigureOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Marker *markerPtr;
    int flags = TK_CONFIG_ARGV_ONLY;
    Tk_Uid oldId;
    int numNames, numOpts;
    char **options;
    register int i;

    /* Figure out where the option value pairs begin */
    argc -= 3;
    argv += 3;
    for (i = 0; i < argc; i++) {
	if (argv[i][0] == '-') {
	    break;
	}
	if (Blt_NameToMarker(graphPtr, argv[i], &markerPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    numNames = i;		/* Number of element names specified */
    numOpts = argc - i;		/* Number of options specified */
    options = argv + numNames;	/* Start of options in argv  */

    for (i = 0; i < numNames; i++) {
	Blt_NameToMarker(graphPtr, argv[i], &markerPtr);
	if (numOpts == 0) {
	    return (Tk_ConfigureInfo(interp, graphPtr->tkwin, 
		markerPtr->configSpecs, (char *)markerPtr, (char *)NULL, flags));
	} else if (numOpts == 1) {
	    return (Tk_ConfigureInfo(interp, graphPtr->tkwin,
		markerPtr->configSpecs, (char *)markerPtr, options[0], flags));
	}
	/* Save the old marker. */
	oldId = Blt_GetUid(markerPtr->nameId);
	if (Tk_ConfigureWidget(interp, graphPtr->tkwin, markerPtr->configSpecs, 
		numOpts, options, (char *)markerPtr, flags) != TCL_OK) {
	    goto error;
	}
	if (oldId != markerPtr->nameId) {
	    if (RenameMarker(graphPtr, markerPtr, oldId, 
		     markerPtr->nameId) != TCL_OK) {
		Blt_FreeUid(markerPtr->nameId);
		markerPtr->nameId = oldId;
		return TCL_ERROR;
	    }
	}
	Blt_FreeUid(oldId);
	if ((*markerPtr->configProc) (markerPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    return TCL_OK;
  error:
    Blt_FreeUid(oldId);
    return TCL_ERROR;
}

/*
 * ----------------------------------------------------------------------
 *
 * CreateOp --
 *
 *	This procedure creates and initializes a new marker.
 *
 * Results:
 *	The return value is a pointer to a structure describing
 *	the new element.  If an error occurred, then the return
 *	value is NULL and an error message is left in interp->result.
 *
 * Side effects:
 *	Memory is allocated, etc.
 *
 * ----------------------------------------------------------------------
 */
static int
CreateOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Marker *markerPtr;
    Tcl_HashEntry *hPtr;
    int isNew;
    ObjectType type;
    register int i;
    char *name;
    char string[200];
    unsigned int length;

    markerPtr = NULL;

    /* Create the new marker based upon the given type */

    type = StringToMarkerType(interp, argv[3]);
    switch (type) {
    case TYPE_MARKER_LINE:
	markerPtr = CreateLineMarker();
	break;
    case TYPE_MARKER_WINDOW:
	markerPtr = CreateWindowMarker();
	break;
    case TYPE_MARKER_BITMAP:
	markerPtr = CreateBitmapMarker();
	break;
    case TYPE_MARKER_IMAGE:
	markerPtr = CreateImageMarker();
	break;
    case TYPE_MARKER_POLYGON:
	markerPtr = CreatePolygonMarker();
	break;
    case TYPE_MARKER_TEXT:
	markerPtr = CreateTextMarker();
	break;
    default:
	return TCL_ERROR;
    }
    assert(markerPtr);
    markerPtr->graphPtr = graphPtr;
    markerPtr->type = type;
    markerPtr->hidden = markerPtr->drawUnder = FALSE;
    markerPtr->flags |= COORDS_NEEDED;

    /* Scan for "-name" option. We need it for the component name */
    name = NULL;
    for (i = 4; i < argc; i += 2) {
	length = strlen(argv[i]);
	if ((length > 1) && (strncmp(argv[i], "-name", length) == 0)) {
	    name = argv[i + 1];
	    break;
	}
    }
    /* If no name was given for the marker, make up one. */
    if (name == NULL) {
	sprintf(string, "marker%d", graphPtr->nextMarkerId++);
	name = string;
    }
    markerPtr->nameId = Blt_GetUid(name);
    if (Blt_ConfigureWidgetComponent(interp, graphPtr->tkwin,
	    markerPtr->nameId, markerClasses[type - 4], markerPtr->configSpecs,
	    argc - 4, argv + 4, (char *)markerPtr, 0) != TCL_OK) {
	DestroyMarker(markerPtr);
	return TCL_ERROR;
    }
    if ((*markerPtr->configProc) (markerPtr) != TCL_OK) {
	DestroyMarker(markerPtr);
	return TCL_ERROR;
    }
    hPtr = Tcl_CreateHashEntry(&(graphPtr->markerTable), markerPtr->nameId, &isNew);
    if (!isNew) {
	Marker *oldMarkerPtr;
	/*
	 * Marker by the same name already exists.  Delete the old
	 * marker and it's list entry.  But save the hash entry.
	 */
	oldMarkerPtr = (Marker *)Tcl_GetHashValue(hPtr);
	oldMarkerPtr->hashPtr = NULL;
	DestroyMarker(oldMarkerPtr);
    }
    Tcl_SetHashValue(hPtr, (ClientData)markerPtr);
    markerPtr->hashPtr = hPtr;

    markerPtr->item = Blt_ListAppend(&(graphPtr->markerList), markerPtr->nameId,
	(ClientData)markerPtr);
    if (markerPtr->drawUnder) {
	graphPtr->flags |= REDRAW_BACKING_STORE;
    }
    Blt_EventuallyRedrawGraph(graphPtr);
    Tcl_SetResult(interp, markerPtr->nameId, TCL_STATIC);
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * DeleteOp --
 *
 *	Deletes the marker given by markerId.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	Graph will be redrawn to reflect the new display list.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
DeleteOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;		/* Not used. */
    int argc;
    char **argv;
{
    Marker *markerPtr;
    register int i;

    for (i = 3; i < argc; i++) {
	if (Blt_NameToMarker(graphPtr, argv[i], &markerPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	DestroyMarker(markerPtr);
    }
    Blt_EventuallyRedrawGraph(graphPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * GetOp --
 *
 * 	Find the legend entry from the given argument.  The argument
 *	can be either a screen position "@x,y" or the name of an
 *	element.
 *
 *	I don't know how useful it is to test with the name of an
 *	element.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side Effects:
 *	Graph will be redrawn to reflect the new legend attributes.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
GetOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char *argv[];
{
    register Marker *markerPtr;

    if ((argv[3][0] == 'c') && (strcmp(argv[3], "current") == 0)) {
	markerPtr = (Marker *)Blt_GetCurrentItem(graphPtr->bindTable);
	/* Report only on markers. */
	if ((markerPtr != NULL) && (markerPtr->type >= TYPE_MARKER_BITMAP)) {
	    Tcl_SetResult(interp, markerPtr->nameId, TCL_STATIC);
	}
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * RelinkOp --
 *
 *	Reorders the marker (given by the first markerId) before/after
 *	the another marker (given by the second markerId) in the
 *	marker display list.  If no second markerId is given, the
 *	marker is placed at the beginning/end of the list.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side Effects:
 *	Graph will be redrawn to reflect the new display list.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
RelinkOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;		/* Not used. */
    int argc;
    char **argv;
{
    Blt_ListItem item, placeItem;
    Marker *markerPtr;

    /* Find the new marker to be inserted into the display list */
    if (Blt_NameToMarker(graphPtr, argv[3], &markerPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    /* Now use the marker to find the entry in the display list */
    item = markerPtr->item;

    placeItem = NULL;
    if (argc == 5) {
	if (Blt_NameToMarker(graphPtr, argv[4], &markerPtr) != TCL_OK) {
	    return TCL_ERROR;
	}
	placeItem = markerPtr->item;
    }
    /* Unlink the list item and relink it at the new location */
    Blt_ListUnlinkItem(item);

    if (argv[2][0] == 'a') {
	Blt_ListLinkAfter(&(graphPtr->markerList), item, placeItem);
    } else {
	Blt_ListLinkBefore(&(graphPtr->markerList), item, placeItem);
    }
    if (markerPtr->drawUnder) {
	graphPtr->flags |= REDRAW_BACKING_STORE;
    }
    Blt_EventuallyRedrawGraph(graphPtr);
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * ExistsOp --
 *
 *	Returns if marker by a given ID currently exists.
 *
 * Results:
 *	A standard Tcl result.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ExistsOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Tcl_HashEntry *hPtr;

    hPtr = Tcl_FindHashEntry(&(graphPtr->markerTable), argv[3]);
    Tcl_SetResult(interp, (hPtr == NULL) ? "0" : "1", TCL_STATIC);
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * TypeOp --
 *
 *	Returns a symbolic name for the type of the marker whose ID is
 *	given.
 *
 * Results:
 *	A standard Tcl result. interp->result will contain the symbolic
 *	type of the marker.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
TypeOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Marker *markerPtr;

    if (Blt_NameToMarker(graphPtr, argv[3], &markerPtr) != TCL_OK) {
	return TCL_ERROR;
    }
    Tcl_SetResult(interp, MarkerTypeToString(markerPtr->type), TCL_STATIC);
    return TCL_OK;
}

/* Public routines */

/*
 * ----------------------------------------------------------------------
 *
 * Blt_MarkerOp --
 *
 *	This procedure is invoked to process the Tcl command
 *	that corresponds to a widget managed by this module.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 * ----------------------------------------------------------------------
 */

static Blt_OpSpec markerOps[] =
{
    {"after", 1, (Blt_Operation)RelinkOp, 4, 5, "marker ?afterMarker?",},
    {"before", 2, (Blt_Operation)RelinkOp, 4, 5, "marker ?beforeMarker?",},
    {"bind", 2, (Blt_Operation)BindOp, 4, 6, "marker sequence command",},
    {"cget", 2, (Blt_Operation)CgetOp, 5, 5, "marker option",},
    {"configure", 2, (Blt_Operation)ConfigureOp, 4, 0,
	"marker ?marker?... ?option value?...",},
    {"create", 2, (Blt_Operation)CreateOp, 4, 0,
	"type ?option value?...",},
    {"delete", 1, (Blt_Operation)DeleteOp, 3, 0, "?marker?...",},
    {"exists", 1, (Blt_Operation)ExistsOp, 4, 4, "marker",},
    {"get", 1, (Blt_Operation)GetOp, 4, 4, "name",},
    {"names", 1, (Blt_Operation)NamesOp, 3, 0, "?pattern?...",},
    {"type", 1, (Blt_Operation)TypeOp, 4, 4, "marker",},
};
static int numMarkerOps = sizeof(markerOps) / sizeof(Blt_OpSpec);

/*ARGSUSED*/
int
Blt_MarkerOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;		/* not used */
    int argc;
    char **argv;
{
    Blt_Operation proc;
    int result;

    proc = Blt_GetOperation(interp, numMarkerOps, markerOps, BLT_OPER_ARG2,
	argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    result = (*proc) (graphPtr, interp, argc, argv);
    return (result);
}

/*
 * -------------------------------------------------------------------------
 *
 * Blt_PrintMarkers --
 *
 * -------------------------------------------------------------------------
 */
void
Blt_PrintMarkers(graphPtr, printable, under)
    Graph *graphPtr;
    Printable printable;
    int under;
{
    Blt_ListItem item;
    register Marker *markerPtr;

    for (item = Blt_ListFirstItem(&(graphPtr->markerList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	markerPtr = (Marker *)Blt_ListGetValue(item);
	if ((markerPtr->printProc == NULL) || (markerPtr->numCoords == 0)) {
	    continue;
	}
	if (markerPtr->drawUnder != under) {
	    continue;
	}
	if (markerPtr->hidden) {
	    continue;
	}
	if (markerPtr->elemId != NULL) {
	    Tcl_HashEntry *hPtr;

	    hPtr = Tcl_FindHashEntry(&(graphPtr->elemTable), markerPtr->elemId);
	    if (hPtr != NULL) {
		Element *elemPtr;

		elemPtr = (Element *)Tcl_GetHashValue(hPtr);
		if (elemPtr->hidden) {
		    continue;
		}
	    }
	}
	Blt_PrintAppend(printable, "\n% Marker \"", markerPtr->nameId,
	    "\" is a ", MarkerTypeToString(markerPtr->type), "\n", (char *)NULL);
	(*markerPtr->printProc) (markerPtr, printable);
    }
}

/*
 * -------------------------------------------------------------------------
 *
 * Blt_DrawMarkers --
 *
 *	Calls the individual drawing routines (based on marker type)
 *	for each marker in the display list.
 *
 *	A marker will not be drawn if
 *
 *	1) there is an element linked with the marker (whose name is
 *	   elemName) and that element is not currently being displayed.
 *
 *	2) there are no coordinates available for the marker.
 *
 *	3) the smarkere at which we're drawing is different from how
 *	   the marker wants to be displayed (either above/below the
 *	   elements).
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Markers are drawn into the drawable (pixmap) which will eventually
 *	be displayed in the graph window.
 *
 * -------------------------------------------------------------------------
 */
void
Blt_DrawMarkers(graphPtr, drawable, under)
    Graph *graphPtr;
    Drawable drawable;		/* Pixmap or window to draw into */
    int under;
{
    Blt_ListItem item;
    Marker *markerPtr;

    for (item = Blt_ListFirstItem(&(graphPtr->markerList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	markerPtr = (Marker *)Blt_ListGetValue(item);

	if ((markerPtr->numCoords == 0) || (markerPtr->drawUnder != under) ||
	    (markerPtr->hidden) || (markerPtr->clipped)) {
	    continue;
	}
	if (markerPtr->elemId != NULL) {
	    Tcl_HashEntry *hPtr;

	    hPtr = Tcl_FindHashEntry(&(graphPtr->elemTable), markerPtr->elemId);
	    if (hPtr != NULL) {
		Element *elemPtr;

		elemPtr = (Element *)Tcl_GetHashValue(hPtr);
		if (elemPtr->hidden) {
		    continue;
		}
	    }
	}
	(*markerPtr->drawProc) (markerPtr, drawable);
    }
}


void
Blt_TransformMarkers(graphPtr)
    Graph *graphPtr;
{
    Blt_ListItem item;
    Marker *markerPtr;

    for (item = Blt_ListFirstItem(&(graphPtr->markerList)); item != NULL;
	item = Blt_ListNextItem(item)) {
	markerPtr = (Marker *)Blt_ListGetValue(item);
	if ((markerPtr->numCoords == 0) || (markerPtr->hidden)) {
	    continue;
	}
	if ((graphPtr->flags & COORDS_ALL_PARTS) ||
	    (markerPtr->flags & COORDS_NEEDED)) {
	    (*markerPtr->coordsProc) (markerPtr);
	    markerPtr->flags &= ~COORDS_NEEDED;
	}
    }
}


void
Blt_DestroyMarkers(graphPtr)
    Graph *graphPtr;
{
    Tcl_HashEntry *hPtr;
    Tcl_HashSearch cursor;
    Marker *markerPtr;

    for (hPtr = Tcl_FirstHashEntry(&(graphPtr->markerTable), &cursor);
	hPtr != NULL; hPtr = Tcl_NextHashEntry(&cursor)) {
	markerPtr = (Marker *)Tcl_GetHashValue(hPtr);
	/*
	 * Dereferencing the pointer to the hash table prevents the
	 * hash table entry from being automatically deleted.  
	 */
	markerPtr->hashPtr = NULL;
	DestroyMarker(markerPtr);
    }
    Tcl_DeleteHashTable(&(graphPtr->markerTable));
    Tcl_DeleteHashTable(&(graphPtr->markerTagTable));
    Blt_ListReset(&(graphPtr->markerList));
}

Marker *
Blt_NearestMarker(graphPtr, x, y, under)
    Graph *graphPtr;
    int x, y;
    int under;
{
    Blt_ListItem item;
    Marker *markerPtr;

    for (item = Blt_ListLastItem(&(graphPtr->markerList)); item != NULL;
	item = Blt_ListPrevItem(item)) {
	markerPtr = (Marker *)Blt_ListGetValue(item);
	if ((markerPtr->drawUnder == under) &&
	    (markerPtr->numCoords > 0) && (!markerPtr->hidden)) {
	    if ((*markerPtr->pointProc) (markerPtr, x, y)) {
		return markerPtr;
	    }
	}
    }
    return NULL;
}
