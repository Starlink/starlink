
/*
 * bltGrLine.c --
 *
 *	This module implements line graph and stripchart elements for
 *	the BLT graph widget.
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

#include <ctype.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#include "bltGrElem.h"

#define LINE_INIT_SYMBOL_SCALE	(1<<10)
#define COLOR_DEFAULT	(XColor *)1

#define PEN_INCREASING  1	/* Draw line segments for only those data points
				 * whose abscissas are monotonically increasing
				 * in order */
#define PEN_DECREASING  2	/* Lines will be drawn between only those points
				 * whose abscissas are decreasing in order */

#define PEN_BOTH_DIRECTIONS	(PEN_INCREASING | PEN_DECREASING)
 /* Lines will be drawn between points regardless of the ordering of
  * the abscissas */

#define BROKEN_TRACE(penDir,last,next) \
    (((((penDir) & PEN_DECREASING) == 0) && ((next) < (last))) || \
     ((((penDir) & PEN_INCREASING) == 0) && ((next) > (last))))

#define PEN_STEP	1
#define PEN_LINEAR	2
#define PEN_NATURAL	3
#define PEN_QUADRATIC	4

static int StringToSmooth _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int offset));
static char *SmoothToString _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption smoothOption =
{
    StringToSmooth, SmoothToString, (ClientData)0
};

static int StringToStyles _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int offset));
static char *StylesToString _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption stylesOption =
{
    StringToStyles, StylesToString, (ClientData)0
};

static int StringToPenDir _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int offset));
static char *PenDirToString _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption penDirOption =
{
    StringToPenDir, PenDirToString, (ClientData)0
};

static int StringToSymbol _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int offset));
static char *SymbolToString _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption bltSymbolOption =
{
    StringToSymbol, SymbolToString, (ClientData)0
};

extern Tk_CustomOption bltColorOption;
extern Tk_CustomOption bltDashesOption;
extern Tk_CustomOption bltDataOption;
extern Tk_CustomOption bltDataPairsOption;
extern Tk_CustomOption bltLengthOption;
extern Tk_CustomOption bltLinePenOption;
extern Tk_CustomOption bltListOption;
extern Tk_CustomOption bltSymbolOption;
extern Tk_CustomOption bltXAxisOption;
extern Tk_CustomOption bltYAxisOption;

typedef struct TransformInfo {
    Point2D *points;		/* Array of transformed coordinates */
    int numPoints;		/* Number of coordinates */
    int *styleDir;		/* Index of pen styles  */
    int *indexArr;		/* Maps segments/traces to data points */

} TransformInfo;

/*
 * Symbol types for line elements
 */
typedef enum {
    SYMBOL_NONE,
    SYMBOL_SQUARE,
    SYMBOL_CIRCLE,
    SYMBOL_DIAMOND,
    SYMBOL_PLUS,
    SYMBOL_CROSS,
    SYMBOL_SPLUS,
    SYMBOL_SCROSS,
    SYMBOL_TRIANGLE,
    SYMBOL_BITMAP
} SymbolType;

typedef struct {
    SymbolType type;		/* Type of symbol to be drawn/printed */
    int size;			/* Requested size of symbol in pixels */

    /* The last two fields are used only for symbols of type SYMBOL_BITMAP */

    Pixmap bitmap;		/* Bitmap to determine foreground/background
				 * pixels of the symbol */
    Pixmap mask;		/* Bitmap representing the transparent pixels
				 * of the symbol */
} Symbol;

typedef struct {
    int numPoints;		/* Number of points in the continuous trace */
    XPoint *pointArr;		/* Array of points (malloc-ed) forming the
				 * trace. */
    int *indexArr;		/* Indices of points in data vectors */
} Trace;

typedef struct LinePen {
    char *name;			/* Pen style identifier.  Statically allocated
				 * pens have no name (NULL). */
    ObjectType type;		/* Type of pen */
    char *typeId;		/* String token identifying the type of pen */
    unsigned int flags;		/* Indicates if the pen element is active or
				 * normal */
    int refCount;		/* Reference count for elements using this
				 * pen. */
    Tcl_HashEntry *hashPtr;
    Tk_ConfigSpec *configSpecs;	/* Configuration specifications */

    PenConfigureProc *configProc;
    PenDestroyProc *destroyProc;

    Symbol symbol;		/* Element symbol type */

    XColor *outlineColor;	/* Symbol outline color */
    int outlineWidth;		/* Width of the outline */
    GC outlineGC;		/* Outline graphics context */

    XColor *fillColor;		/* Normal symbol fill color */
    GC fillGC;			/* Fill graphics context */

    int lineWidth;		/* Width of the line segments. If lineWidth is
				 * 0, no line will be drawn, only symbols. */
    Dashes dashes;		/* Dash on-off list value */
    XColor *penColor;		/* Line segment color */
    XColor *penOffColor;	/* Line segment dash gap color */
    GC penGC;			/* Line segment graphics context */

} LinePen;

typedef struct LineStyle {

    LinePen *penPtr;		/* Pen to draw */
    Limits weight;		/* Range of weights */

    int symbolSize;		/* Scaled size of the pen's symbol. */

    XPoint *pointPtr;		/* Points to start of array for this pen. */
    int numPoints;		/* Number of points for this pen. */

    /* The following fields are used only for stripcharts. */

    XSegment *segPtr;		/* Points to start of line segments for
				 * this pen */
    int numSegments;		/* Number of line segments for this pen. */

} LineStyle;

typedef struct {
    char *name;			/* Identifier to refer the element. Used in the
				 * "insert", "delete", or "show", commands. */
    ObjectType type;		/* Type of element; either TYPE_ELEM_BAR,
				 * TYPE_ELEM_LINE, or TYPE_ELEM_STRIP */
    Graph *graphPtr;		/* Graph widget of element*/
    unsigned int flags;		/* Indicates if the entire element is active, or
				 * if coordinates need to be calculated */
    char **tags;
    int hidden;			/* If non-zero, don't display the element. */

    Tcl_HashEntry *hashPtr;
    char *label;		/* Label displayed in legend */
    int labelRelief;		/* Relief of label in legend. */

    Axis2D axes;
    ElemVector x, y, w;		/* Contains array of numeric values */

    int *reqActiveArr;		/* Array of indices (malloc-ed) which indicate
				 * which data points are active (drawn with
				 * "active" colors). */
    int reqNumActive;		/* Number of active data points. Special case:
				 * if reqNumActive<0, then all data points
				 * are drawn active. */
    ElemClassInfo *infoPtr;

    /*
     * Line specific configurable attributes
     */

    /* Symbol scaling */
    int scaleSymbols;		/* If non-zero, the symbols will scale
				 * in size as the graph is zoomed
				 * in/out.  */
    double xRange, yRange;	/* Initial X-axis and Y-axis ranges:
				 * used to scale the size of element's
				 * symbol. */

    /* Standard Pens */
    LinePen normalPen;
    LinePen *activePenPtr, *normalPenPtr;

    /* Alternate pens */
    LineStyle *styles;		/* Array of pen styles */
    int numStyles;		/* # of pen styles */

    /* Line smoothing */
    int reqSmooth;		/* Requested smoothing function to use for line
				 * segments connecting the points */
    int smooth;			/* Actual smoothing function used. */

    /*
     * Drawing related data structures.
     */

    /* Fill area under curve */
    ColorPair fillColors;
    GC fillGC;
    Pixmap stipple;		/* Stipple for fill area */
    XPoint *fillArr;		/* Array of points used to draw polygon to fill
				 * area under the curve */

    /* Symbol points */
    XPoint *pointArr;		/* Holds the screen coordinates of all the data
				 * points for the element. */
    int numPoints;		/* Number of points */
    int *pointMap;		/* Contains indices of data points. It's
				 * first used to map pens to visible points
				 * to sort them by pen style, and later to
				 * find data points from the index of a
				 * visible point.
				 */

    /* Active symbol points */
    XPoint *activeArr;		/* Active points */
    int numActive;


    /* Line graph specific fields */
    int penDir;			/* Indicates if a change in the pen direction
				 * should be considered a retrace (line segment
				 * is not drawn). */
    Blt_List traces;		/* List of traces. A trace is a continuous
				 * segmented line.  New traces are generated
				 * when either 1) the segment changes the pen
				 * direction, or 2) the end point is clipped by
				 * the plotting area. */

    /* Stripchart specific fields */
    XSegment *segArr;		/* Holds the all the drawn line segments of the
				 * element trace. The segments are grouped by
				 * pen style. */
    int numSegments;		/* Number of line segments to be drawn */
    int *segMap;		/* Pen to visible line segment mapping */

} Line;

#define DEF_LINE_ACTIVE_PEN		"activeLine"
#define DEF_LINE_AXIS_X			"x"
#define DEF_LINE_AXIS_Y			"y"
#define DEF_LINE_DASHES			(char *)NULL
#define DEF_LINE_DATA			(char *)NULL
#define DEF_LINE_FILL_COLOR    		"defcolor"
#define DEF_LINE_FILL_MONO		"defcolor"
#define DEF_LINE_LABEL			(char *)NULL
#define DEF_LINE_LABEL_RELIEF		"flat"
#define DEF_LINE_HIDE			"no"
#define DEF_LINE_OFFDASH_COLOR    	(char *)NULL
#define DEF_LINE_OFFDASH_MONO		(char *)NULL
#define DEF_LINE_OUTLINE_COLOR		"defcolor"
#define DEF_LINE_OUTLINE_MONO		"defcolor"
#define DEF_LINE_OUTLINE_WIDTH 		"1"
#define DEF_LINE_PIXELS			"0.125i"
#define DEF_LINE_SCALE_SYMBOLS		"no"
#define DEF_LINE_SMOOTH			"linear"
#define DEF_LINE_STYLES			""
#define DEF_LINE_STIPPLE		(char *)NULL
#define DEF_LINE_SYMBOL			"circle"
#define DEF_LINE_TAGS			"all"
#define DEF_LINE_PEN_DIRECTION		"both"
#define DEF_LINE_PEN_COLOR		RGB_COLOR_NAVYBLUE
#define DEF_LINE_PEN_MONO		RGB_COLOR_BLACK
#define DEF_LINE_PEN_WIDTH		"1"
#define DEF_LINE_X_DATA			(char *)NULL
#define DEF_LINE_Y_DATA			(char *)NULL

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_CUSTOM, "-activepen", "activePen", "ActivePen",
	DEF_LINE_ACTIVE_PEN, Tk_Offset(Line, activePenPtr),
	TK_CONFIG_NULL_OK | GRAPH | STRIPCHART, &bltLinePenOption},
    {TK_CONFIG_CUSTOM, "-bindtags", "bindTags", "BindTags",
	DEF_LINE_TAGS, Tk_Offset(Line, tags),
	TK_CONFIG_NULL_OK | GRAPH | STRIPCHART, &bltListOption},
    {TK_CONFIG_COLOR, "-color", "color", "Color",
	DEF_LINE_PEN_COLOR, Tk_Offset(Line, normalPen.penColor),
	TK_CONFIG_COLOR_ONLY | GRAPH | STRIPCHART},
    {TK_CONFIG_COLOR, "-color", "color", "Color",
	DEF_LINE_PEN_MONO, Tk_Offset(Line, normalPen.penColor),
	TK_CONFIG_MONO_ONLY | GRAPH | STRIPCHART},
    {TK_CONFIG_CUSTOM, "-dashes", "dashes", "Dashes",
	DEF_LINE_DASHES, Tk_Offset(Line, normalPen.dashes),
	TK_CONFIG_NULL_OK | GRAPH | STRIPCHART, &bltDashesOption},
    {TK_CONFIG_CUSTOM, "-data", "data", "Data",
	DEF_LINE_DATA, 0, GRAPH | STRIPCHART, &bltDataPairsOption},
    {TK_CONFIG_CUSTOM, "-fill", "fill", "Fill",
	DEF_LINE_FILL_COLOR, Tk_Offset(Line, normalPen.fillColor),
	TK_CONFIG_NULL_OK | TK_CONFIG_COLOR_ONLY | GRAPH | STRIPCHART,
	&bltColorOption},
    {TK_CONFIG_CUSTOM, "-fill", "fill", "Fill",
	DEF_LINE_FILL_MONO, Tk_Offset(Line, normalPen.fillColor),
	TK_CONFIG_NULL_OK | TK_CONFIG_MONO_ONLY | GRAPH | STRIPCHART,
	&bltColorOption},
    {TK_CONFIG_BOOLEAN, "-hide", "hide", "Hide",
	DEF_LINE_HIDE, Tk_Offset(Line, hidden),
	TK_CONFIG_DONT_SET_DEFAULT | GRAPH | STRIPCHART},
    {TK_CONFIG_STRING, "-label", "label", "Label",
	(char *)NULL, Tk_Offset(Line, label),
	TK_CONFIG_NULL_OK | GRAPH | STRIPCHART},
    {TK_CONFIG_RELIEF, "-labelrelief", "labelRelief", "LabelRelief",
	DEF_LINE_LABEL_RELIEF, Tk_Offset(Line, labelRelief),
	TK_CONFIG_DONT_SET_DEFAULT | GRAPH | STRIPCHART},
    {TK_CONFIG_CUSTOM, "-linewidth", "lineWidth", "LineWidth",
	DEF_LINE_PEN_WIDTH, Tk_Offset(Line, normalPen.lineWidth),
	GRAPH | STRIPCHART, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-mapx", "mapX", "MapX",
	DEF_LINE_AXIS_X, Tk_Offset(Line, axes.x),
	GRAPH | STRIPCHART, &bltXAxisOption},
    {TK_CONFIG_CUSTOM, "-mapy", "mapY", "MapY",
	DEF_LINE_AXIS_Y, Tk_Offset(Line, axes.y), GRAPH | STRIPCHART,
	&bltYAxisOption},
    {TK_CONFIG_CUSTOM, "-offdash", "offDash", "OffDash",
	DEF_LINE_OFFDASH_COLOR, Tk_Offset(Line, normalPen.penOffColor),
	TK_CONFIG_NULL_OK | TK_CONFIG_COLOR_ONLY | GRAPH | STRIPCHART,
	&bltColorOption},
    {TK_CONFIG_CUSTOM, "-offdash", "offDash", "OffDash",
	DEF_LINE_OFFDASH_MONO, Tk_Offset(Line, normalPen.penOffColor),
	TK_CONFIG_NULL_OK | TK_CONFIG_MONO_ONLY | GRAPH | STRIPCHART,
	&bltColorOption},
    {TK_CONFIG_CUSTOM, "-outline", "outline", "Outline",
	DEF_LINE_OUTLINE_COLOR, Tk_Offset(Line, normalPen.outlineColor),
	TK_CONFIG_COLOR_ONLY | GRAPH | STRIPCHART, &bltColorOption},
    {TK_CONFIG_CUSTOM, "-outline", "outline", "Outline",
	DEF_LINE_OUTLINE_MONO, Tk_Offset(Line, normalPen.outlineColor),
	TK_CONFIG_MONO_ONLY | GRAPH | STRIPCHART, &bltColorOption},
    {TK_CONFIG_CUSTOM, "-outlinewidth", "outlineWidth", "OutlineWidth",
	DEF_LINE_OUTLINE_WIDTH, Tk_Offset(Line, normalPen.outlineWidth),
	TK_CONFIG_DONT_SET_DEFAULT | GRAPH | STRIPCHART, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-pen", "pen", "Pen",
	(char *)NULL, Tk_Offset(Line, normalPenPtr),
	TK_CONFIG_NULL_OK | GRAPH | STRIPCHART, &bltLinePenOption},
    {TK_CONFIG_CUSTOM, "-pixels", "pixels", "Pixels",
	DEF_LINE_PIXELS, Tk_Offset(Line, normalPen.symbol.size),
	GRAPH | STRIPCHART, &bltLengthOption},
    {TK_CONFIG_BOOLEAN, "-scalesymbols", "scaleSymbols", "ScaleSymbols",
	DEF_LINE_SCALE_SYMBOLS, Tk_Offset(Line, scaleSymbols),
	TK_CONFIG_DONT_SET_DEFAULT | GRAPH | STRIPCHART},
    {TK_CONFIG_CUSTOM, "-smooth", "smooth", "Smooth",
	DEF_LINE_SMOOTH, Tk_Offset(Line, reqSmooth),
	TK_CONFIG_DONT_SET_DEFAULT | GRAPH | STRIPCHART, &smoothOption},
#ifdef notdef
    {TK_CONFIG_BITMAP, "-stipple", "stipple", "Stipple",
	DEF_LINE_STIPPLE, Tk_Offset(Line, stipple),
	TK_CONFIG_NULL_OK | GRAPH},
#endif
    {TK_CONFIG_CUSTOM, "-styles", "styles", "Styles",
	DEF_LINE_STYLES, 0,
	GRAPH | STRIPCHART | TK_CONFIG_NULL_OK, &stylesOption},
    {TK_CONFIG_CUSTOM, "-symbol", "symbol", "Symbol",
	DEF_LINE_SYMBOL, Tk_Offset(Line, normalPen.symbol),
	TK_CONFIG_DONT_SET_DEFAULT | GRAPH | STRIPCHART, &bltSymbolOption},
    {TK_CONFIG_CUSTOM, "-trace", "trace", "Trace",
	DEF_LINE_PEN_DIRECTION, Tk_Offset(Line, penDir),
	TK_CONFIG_DONT_SET_DEFAULT | GRAPH, &penDirOption},
    {TK_CONFIG_CUSTOM, "-weights", "weights", "Weights",
	(char *)NULL, Tk_Offset(Line, w), GRAPH | STRIPCHART, &bltDataOption},
    {TK_CONFIG_CUSTOM, "-xdata", "xData", "XData",
	(char *)NULL, Tk_Offset(Line, x), GRAPH | STRIPCHART, &bltDataOption},
    {TK_CONFIG_CUSTOM, "-ydata", "yData", "YData",
	(char *)NULL, Tk_Offset(Line, y), GRAPH | STRIPCHART, &bltDataOption},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};


#define DEF_PEN_ACTIVE_COLOR		RGB_COLOR_BLUE
#define DEF_PEN_ACTIVE_MONO		RGB_COLOR_BLACK
#define DEF_PEN_DASHES			(char *)NULL
#define DEF_PEN_FILL_COLOR    		"defcolor"
#define DEF_PEN_FILL_MONO		"defcolor"
#define DEF_PEN_LINE_WIDTH		"1"
#define DEF_PEN_NORMAL_COLOR		RGB_COLOR_NAVYBLUE
#define DEF_PEN_NORMAL_MONO		RGB_COLOR_BLACK
#define DEF_PEN_OFFDASH_COLOR    	(char *)NULL
#define DEF_PEN_OFFDASH_MONO		(char *)NULL
#define DEF_PEN_OUTLINE_COLOR		"defcolor"
#define DEF_PEN_OUTLINE_MONO		"defcolor"
#define DEF_PEN_OUTLINE_WIDTH 		"1"
#define DEF_PEN_PIXELS			"0.125i"
#define DEF_PEN_SYMBOL			"circle"
#define DEF_PEN_TYPE			"line"

static Tk_ConfigSpec penConfigSpecs[] =
{
    {TK_CONFIG_COLOR, "-color", "color", "Color",
	DEF_PEN_ACTIVE_COLOR, Tk_Offset(LinePen, penColor),
	TK_CONFIG_COLOR_ONLY | ACTIVE_PEN},
    {TK_CONFIG_COLOR, "-color", "color", "Color",
	DEF_PEN_ACTIVE_MONO, Tk_Offset(LinePen, penColor),
	TK_CONFIG_MONO_ONLY | ACTIVE_PEN},
    {TK_CONFIG_COLOR, "-color", "color", "Color",
	DEF_PEN_NORMAL_COLOR, Tk_Offset(LinePen, penColor),
	TK_CONFIG_COLOR_ONLY | NORMAL_PEN},
    {TK_CONFIG_COLOR, "-color", "color", "Color",
	DEF_PEN_NORMAL_MONO, Tk_Offset(LinePen, penColor),
	TK_CONFIG_MONO_ONLY | NORMAL_PEN},
    {TK_CONFIG_CUSTOM, "-dashes", "dashes", "Dashes",
	DEF_PEN_DASHES, Tk_Offset(LinePen, dashes),
	TK_CONFIG_NULL_OK | ALL_PENS, &bltDashesOption},
    {TK_CONFIG_CUSTOM, "-fill", "fill", "Fill",
	DEF_PEN_FILL_COLOR, Tk_Offset(LinePen, fillColor),
	TK_CONFIG_NULL_OK | TK_CONFIG_COLOR_ONLY | ALL_PENS, &bltColorOption},
    {TK_CONFIG_CUSTOM, "-fill", "fill", "Fill",
	DEF_PEN_FILL_MONO, Tk_Offset(LinePen, fillColor),
	TK_CONFIG_NULL_OK | TK_CONFIG_MONO_ONLY | ALL_PENS, &bltColorOption},
    {TK_CONFIG_CUSTOM, "-linewidth", "lineWidth", "LineWidth",
	(char *)NULL, Tk_Offset(LinePen, lineWidth), ALL_PENS, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-offdash", "offDash", "OffDash",
	DEF_PEN_OFFDASH_COLOR, Tk_Offset(LinePen, penOffColor),
	TK_CONFIG_NULL_OK | TK_CONFIG_COLOR_ONLY | ALL_PENS, &bltColorOption},
    {TK_CONFIG_CUSTOM, "-offdash", "offDash", "OffDash",
	DEF_PEN_OFFDASH_MONO, Tk_Offset(LinePen, penOffColor),
	TK_CONFIG_NULL_OK | TK_CONFIG_MONO_ONLY | ALL_PENS, &bltColorOption},
    {TK_CONFIG_CUSTOM, "-outline", "outline", "Outline",
	DEF_PEN_OUTLINE_COLOR, Tk_Offset(LinePen, outlineColor),
	TK_CONFIG_COLOR_ONLY | ALL_PENS, &bltColorOption},
    {TK_CONFIG_CUSTOM, "-outline", "outline", "Outline",
	DEF_PEN_OUTLINE_MONO, Tk_Offset(LinePen, outlineColor),
	TK_CONFIG_MONO_ONLY | ALL_PENS, &bltColorOption},
    {TK_CONFIG_CUSTOM, "-outlinewidth", "outlineWidth", "OutlineWidth",
	DEF_PEN_OUTLINE_WIDTH, Tk_Offset(LinePen, outlineWidth),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_PENS, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-pixels", "pixels", "Pixels",
	DEF_PEN_PIXELS, Tk_Offset(LinePen, symbol.size),
	ALL_PENS, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-symbol", "symbol", "Symbol",
	DEF_PEN_SYMBOL, Tk_Offset(LinePen, symbol),
	TK_CONFIG_DONT_SET_DEFAULT | ALL_PENS, &bltSymbolOption},
    {TK_CONFIG_STRING, "-type", (char *)NULL, (char *)NULL,
	DEF_PEN_TYPE, Tk_Offset(Pen, typeId), ALL_PENS | TK_CONFIG_NULL_OK},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

/* Forward declarations */
#ifdef __STDC__
static PenConfigureProc ConfigurePen;
static PenDestroyProc DestroyPen;
static ElemClosestProc ClosestLine;
static ElemConfigProc ConfigureLine;
static ElemDestroyProc DestroyLine;
static ElemDrawProc DrawActiveLine;
static ElemDrawProc DrawNormalLine;
static ElemDrawSymbolProc DrawSymbol;
static ElemExtentsProc ExtentsLine;
static ElemPrintProc PrintActiveLine;
static ElemPrintProc PrintNormalLine;
static ElemPrintSymbolProc PrintSymbol;
static ElemTransformProc TransformLine;
#endif /* __STDC__ */

INLINE static int
Round(x)
    register double x;
{
    return (int) (x + ((x < 0.0) ? -0.5 : 0.5));
}

/*
 * ----------------------------------------------------------------------
 *
 * InRange --
 *
 *	Determines if a value lies within a given interval.
 *
 *	The value is normalized and compared against the interval
 *	[0..1], where 0.0 is the minimum and 1.0 is the maximum.
 *	DBL_EPSILON is the smallest number that can be represented
 *	on the host machine, such that (1.0 + epsilon) != 1.0.
 *
 *	Please note, *max* can't equal *min*.
 *
 * Results:
 *	Returns whether the value lies outside of the given range.
 *	If value is outside of the interval [min..max], 1 is returned;
 *	0 otherwise.
 *
 * ----------------------------------------------------------------------
 */
INLINE static int
OutOfRange(value, limitsPtr)
    register double value;
    Limits *limitsPtr;
{
    register double norm;

    norm = (value - limitsPtr->min) / (limitsPtr->range);
    return (((norm - 1.0) > DBL_EPSILON) || (((1.0 - norm) - 1.0) > DBL_EPSILON));
}

/*
 * ----------------------------------------------------------------------
 * 	Custom configuration option (parse and print) routines
 * ----------------------------------------------------------------------
 */

/*
 *----------------------------------------------------------------------
 *
 * NameOfSymbol --
 *
 *	Converts the symbol value into its string representation.
 *
 * Results:
 *	The static string representing the symbol type is returned.
 *
 *----------------------------------------------------------------------
 */
static char *
NameOfSymbol(symbolPtr)
    Symbol *symbolPtr;
{
    switch (symbolPtr->type) {
    case SYMBOL_NONE:
	return "none";
    case SYMBOL_SQUARE:
	return "square";
    case SYMBOL_CIRCLE:
	return "circle";
    case SYMBOL_DIAMOND:
	return "diamond";
    case SYMBOL_PLUS:
	return "plus";
    case SYMBOL_CROSS:
	return "cross";
    case SYMBOL_SPLUS:
	return "splus";
    case SYMBOL_SCROSS:
	return "scross";
    case SYMBOL_TRIANGLE:
	return "triangle";
    case SYMBOL_BITMAP:
	return "bitmap";
    }
    return NULL;
}

static int
StringToBitmap(interp, tkwin, symbolPtr, string)
    Tcl_Interp *interp;
    Tk_Window tkwin;
    Symbol *symbolPtr;
    char string[];
{
    Pixmap bitmap, mask;
    char **nameArr;
    int numBitmaps;
    int result;

    if (Tcl_SplitList(interp, string, &numBitmaps, &nameArr) != TCL_OK) {
	return TCL_ERROR;
    }
    bitmap = mask = None;
    if (numBitmaps > 2) {
	Tcl_AppendResult(interp, "too many elements in bitmap list \"", string,
	    "\": should be \"bitmap mask\"", (char *)NULL);
	result = TCL_ERROR;
	goto error;
    }
    bitmap = Tk_GetBitmap(interp, tkwin, Tk_GetUid(nameArr[0]));
    if (bitmap == None) {
	result = TCL_BREAK;
	Tcl_ResetResult(interp);
	goto error;
    }
    if (numBitmaps > 1) {
	mask = Tk_GetBitmap(interp, tkwin, Tk_GetUid(nameArr[1]));
	if (mask == None) {
	    Tk_FreeBitmap(Tk_Display(tkwin), bitmap);
	    result = TCL_ERROR;
	    goto error;
	}
    }
    free((char *)nameArr);
    if (symbolPtr->bitmap != None) {
	Tk_FreeBitmap(Tk_Display(tkwin), symbolPtr->bitmap);
    }
    symbolPtr->bitmap = bitmap;
    if (symbolPtr->mask != None) {
	Tk_FreeBitmap(Tk_Display(tkwin), symbolPtr->mask);
    }
    symbolPtr->mask = mask;
    return TCL_OK;
  error:
    free((char *)nameArr);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * StringToSymbol --
 *
 *	Convert the string representation of a line style or symbol name
 *	into its numeric form.
 *
 * Results:
 *	The return value is a standard Tcl result.  The symbol type is
 *	written into the widget record.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToSymbol(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* String representing symbol type */
    char *widgRec;		/* Element information record */
    int offset;			/* Offset of symbol type field in record */
{
    Symbol *symbolPtr = (Symbol *)(widgRec + offset);
    unsigned int length;
    char c;

    c = string[0];
    length = strlen(string);
    if (c == '\0') {
	symbolPtr->type = SYMBOL_NONE;
    } else if ((c == 'n') && (strncmp(string, "none", length) == 0)) {
	symbolPtr->type = SYMBOL_NONE;
    } else if ((c == 'c') && (length > 1) &&
	(strncmp(string, "circle", length) == 0)) {
	symbolPtr->type = SYMBOL_CIRCLE;
    } else if ((c == 's') && (length > 1) &&
	(strncmp(string, "square", length) == 0)) {
	symbolPtr->type = SYMBOL_SQUARE;
    } else if ((c == 'd') && (strncmp(string, "diamond", length) == 0)) {
	symbolPtr->type = SYMBOL_DIAMOND;
    } else if ((c == 'p') && (strncmp(string, "plus", length) == 0)) {
	symbolPtr->type = SYMBOL_PLUS;
    } else if ((c == 'c') && (length > 1) &&
	(strncmp(string, "cross", length) == 0)) {
	symbolPtr->type = SYMBOL_CROSS;
    } else if ((c == 's') && (length > 1) &&
	(strncmp(string, "splus", length) == 0)) {
	symbolPtr->type = SYMBOL_SPLUS;
    } else if ((c == 's') && (length > 1) &&
	(strncmp(string, "scross", length) == 0)) {
	symbolPtr->type = SYMBOL_SCROSS;
    } else if ((c == 't') && (strncmp(string, "triangle", length) == 0)) {
	symbolPtr->type = SYMBOL_TRIANGLE;
    } else {
	int result;

	result = StringToBitmap(interp, tkwin, symbolPtr, string);
	if (result != TCL_OK) {
	    if (result != TCL_ERROR) {
		Tcl_AppendResult(interp, "bad symbol \"", string, "\": should be \
\"none\", \"circle\", \"square\", \"diamond\", \"plus\", \"cross\", \"splus\", \
\"scross\", \"triangle\", or the name of a bitmap", (char *)NULL);
	    }
	    return TCL_ERROR;
	}
	symbolPtr->type = SYMBOL_BITMAP;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SymbolToString --
 *
 *	Convert the symbol value into a string.
 *
 * Results:
 *	The string representing the symbol type or line style is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
SymbolToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;
    char *widgRec;		/* Element information record */
    int offset;			/* Offset of symbol type field in record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    Symbol *symbolPtr = (Symbol *)(widgRec + offset);
    char *result;

    if (symbolPtr->type == SYMBOL_BITMAP) {
	Tcl_DString dStr;

	Tcl_DStringInit(&dStr);
	Tcl_DStringAppendElement(&dStr,
	    Tk_NameOfBitmap(Tk_Display(tkwin), symbolPtr->bitmap));
	Tcl_DStringAppendElement(&dStr, (symbolPtr->mask == None) ? "" :
	    Tk_NameOfBitmap(Tk_Display(tkwin), symbolPtr->mask));
	result = Tcl_DStringValue(&dStr);
	if (result == dStr.staticSpace) {
	    result = strdup(result);
	}
	Tcl_DStringFree(&dStr);
	*freeProcPtr = (Tcl_FreeProc *)free;
    } else {
	result = NameOfSymbol(symbolPtr);
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * NameOfSmooth --
 *
 *	Converts the smooth value into its string representation.
 *
 * Results:
 *	The static string representing the smooth type is returned.
 *
 *----------------------------------------------------------------------
 */
static char *
NameOfSmooth(smooth)
    int smooth;
{
    switch (smooth) {
    case PEN_LINEAR:
	return "linear";
    case PEN_STEP:
	return "step";
    case PEN_NATURAL:
	return "natural";
    case PEN_QUADRATIC:
	return "quadratic";
    default:
	return "unknown smooth value";
    }
}

/*
 *----------------------------------------------------------------------
 *
 * StringToSmooth --
 *
 *	Convert the string representation of a line style or smooth name
 *	into its numeric form.
 *
 * Results:
 *	The return value is a standard Tcl result.  The smooth type is
 *	written into the widget record.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToSmooth(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* String representing smooth type */
    char *widgRec;		/* Element information record */
    int offset;			/* Offset of smooth type field in record */
{
    int *smoothPtr = (int *)(widgRec + offset);
    unsigned int length;
    char c;

    c = string[0];
    length = strlen(string);
    if ((c == 'n') && (strncmp(string, "natural", length) == 0)) {
	*smoothPtr = PEN_NATURAL;
    } else if ((c == 'c') && (strncmp(string, "cubic", length) == 0)) {
	*smoothPtr = PEN_NATURAL;
    } else if ((c == 'q') && (strncmp(string, "quadratic", length) == 0)) {
	*smoothPtr = PEN_QUADRATIC;
    } else if ((c == 's') && (strncmp(string, "step", length) == 0)) {
	*smoothPtr = PEN_STEP;
    } else if ((c == 'l') && (strncmp(string, "linear", length) == 0)) {
	*smoothPtr = PEN_LINEAR;
    } else {
	Tcl_AppendResult(interp, "bad smooth value \"", string, "\": should be \
linear, natural, quadratic, or step", (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SmoothToString --
 *
 *	Convert the smooth value into a string.
 *
 * Results:
 *	The string representing the smooth type or line style is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
SmoothToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Element information record */
    int offset;			/* Offset of smooth type field in record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    int smooth = *(int *)(widgRec + offset);

    return NameOfSmooth(smooth);
}

/*
 *----------------------------------------------------------------------
 *
 * StringToPenDir --
 *
 *	Convert the string representation of a line style or symbol name
 *	into its numeric form.
 *
 * Results:
 *	The return value is a standard Tcl result.  The symbol type is
 *	written into the widget record.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToPenDir(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* String representing pen direction */
    char *widgRec;		/* Element information record */
    int offset;			/* Offset of pen direction field in record */
{
    int *penDirPtr = (int *)(widgRec + offset);
    unsigned int length;
    char c;

    c = string[0];
    length = strlen(string);
    if ((c == 'i') && (strncmp(string, "increasing", length) == 0)) {
	*penDirPtr = PEN_INCREASING;
    } else if ((c == 'd') && (strncmp(string, "decreasing", length) == 0)) {
	*penDirPtr = PEN_DECREASING;
    } else if ((c == 'b') && (strncmp(string, "both", length) == 0)) {
	*penDirPtr = PEN_BOTH_DIRECTIONS;
    } else {
	Tcl_AppendResult(interp, "bad trace value \"", string,
	    "\" : should be \"increasing\", \"decreasing\", or \"both\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * NameOfPenDir --
 *
 *	Convert the pen direction into a string.
 *
 * Results:
 *	The static string representing the pen direction is returned.
 *
 *----------------------------------------------------------------------
 */
static char *
NameOfPenDir(penDir)
    int penDir;			/* Direction for pen drawing between points */
{
    switch (penDir) {
    case PEN_INCREASING:
	return "increasing";
    case PEN_DECREASING:
	return "decreasing";
    case PEN_BOTH_DIRECTIONS:
	return "both";
    default:
	return "unknown trace direction";
    }
}

/*
 *----------------------------------------------------------------------
 *
 * PenDirToString --
 *
 *	Convert the pen direction into a string.
 *
 * Results:
 *	The string representing the pen drawing direction is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PenDirToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Element information record */
    int offset;			/* Offset of pen direction field in record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    int penDir = *(int *)(widgRec + offset);

    return NameOfPenDir(penDir);
}

int
Blt_GetPenStyle(graphPtr, string, type, stylePtr)
    Graph *graphPtr;
    char *string;
    ObjectType type;
    PenStyle *stylePtr;
{
    int numElem;
    char **elemArr;
    Pen *penPtr;
    double min, max;

    if (Tcl_SplitList(graphPtr->interp, string, &numElem, &elemArr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Blt_GetPen(graphPtr, elemArr[0], type, &penPtr) != TCL_OK) {
	free((char *)elemArr);
	return TCL_ERROR;
    }
    if ((numElem == 3) &&
	((Tcl_GetDouble(graphPtr->interp, elemArr[1], &min) != TCL_OK) ||
	    (Tcl_GetDouble(graphPtr->interp, elemArr[2], &max) != TCL_OK))) {
	free((char *)elemArr);
	return TCL_ERROR;
    }
    free((char *)elemArr);
    stylePtr->penPtr = penPtr;
    SetLimits(stylePtr->weight, min, max);
    return TCL_OK;
}

static void
FreeLineStyles(linePtr, styles, numStyles)
    Line *linePtr;
    LineStyle styles[];
    int numStyles;
{
    register int i;

    /*
     * Always ignore the first array slot. It's occupied by the built-in
     * "normal" pen of the element.
     */
    for (i = 1; i < numStyles; i++) {
	Blt_FreePen(linePtr->graphPtr, (Pen *)styles[i].penPtr);
    }
    free((char *)styles);
}

/*
 * Clear the number of points and segments, in case there are no segments or
 * points
 */
static void
ClearStyles(linePtr)
    Line *linePtr;
{
    register LineStyle *stylePtr;
    register int i;

    for (stylePtr = linePtr->styles, i = 0; i < linePtr->numStyles;
	i++, stylePtr++) {
	stylePtr->numSegments = stylePtr->numPoints = 0;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * StringToStyles --
 *
 *	Parse the list of style names.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToStyles(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* String representing style list */
    char *widgRec;		/* Element information record */
    int offset;			/* Offset of symbol type field in record */
{
    Line *linePtr = (Line *)(widgRec);
    register int i;
    int numStyles;
    char **elemArr;
    LineStyle *styles, *stylePtr;

    elemArr = NULL;
    if ((string == NULL) || (*string == '\0')) {
	numStyles = 0;
    } else {
	if (Tcl_SplitList(interp, string, &numStyles, &elemArr) != TCL_OK) {
	    numStyles = 0;
	}
    }
    /* Convert the styles into pen pointers and store in an array  */
    stylePtr = styles = (LineStyle *)calloc(numStyles + 1, sizeof(LineStyle));
    assert(styles);

    /* First pen is always the "normal" pen, we'll set the style later */
    stylePtr++;

    for (i = 0; i < numStyles; i++, stylePtr++) {
	SetLimits(stylePtr->weight, (double)i, (double)(i + 1));
	if (Blt_GetPenStyle(linePtr->graphPtr, elemArr[i], linePtr->type,
		(PenStyle *)stylePtr) != TCL_OK) {
	    free((char *)elemArr);
	    FreeLineStyles(linePtr, styles, i);
	    return TCL_ERROR;
	}
    }
    if (elemArr != NULL) {
	free((char *)elemArr);
    }
    if (linePtr->styles != NULL) {
	FreeLineStyles(linePtr, linePtr->styles, linePtr->numStyles);
    }
    linePtr->numStyles = numStyles + 1;
    linePtr->styles = styles;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * StylesToString --
 *
 *	Convert the style information into a string.
 *
 * Results:
 *	The string representing the style information is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
StylesToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Element information record */
    int offset;			/* not used */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    Line *linePtr = (Line *)(widgRec);
    char string[TCL_DOUBLE_SPACE];
    Tcl_DString dStr;
    Tcl_Interp *interp = linePtr->graphPtr->interp;
    register LineStyle *stylePtr;
    register int i;
    char *result;

    if (linePtr->numStyles < 2) {
	return "";
    }
    Tcl_DStringInit(&dStr);
    for (i = 1; i < linePtr->numStyles; i++) {
	stylePtr = linePtr->styles + i;
	Tcl_DStringStartSublist(&dStr);
	Tcl_DStringAppendElement(&dStr, stylePtr->penPtr->name);
	Tcl_PrintDouble(interp, stylePtr->weight.min, string);
	Tcl_DStringAppendElement(&dStr, string);
	Tcl_PrintDouble(interp, stylePtr->weight.max, string);
	Tcl_DStringAppendElement(&dStr, string);
	Tcl_DStringEndSublist(&dStr);
    }
    result = Tcl_DStringValue(&dStr);

    /*
     * If memory wasn't allocated for the dynamic string, do it here (it's
     * currently on the stack), so that Tcl can free it normally.
     *
     * Of course, we'll eventually get burned by directly using the fields of
     * the Tcl_DString structure.  It works today, but maybe not tomorrow.
     * But then again, Tcl's C API guarantees nothing.
     */
    if (result == dStr.staticSpace) {
	result = strdup(result);
    }
    *freeProcPtr = (Tcl_FreeProc *)free;
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigurePen --
 *
 *	Sets up the appropriate configuration parameters in the GC.
 *      It is assumed the parameters have been previously set by
 *	a call to Tk_ConfigureWidget.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 *	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information such as line width, line style, color
 *	etc. get set in a new GC.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ConfigurePen(graphPtr, penPtr)
    Graph *graphPtr;
    Pen *penPtr;
{
    LinePen *lpPtr = (LinePen *)penPtr;
    unsigned long gcMask;
    GC newGC;
    XGCValues gcValues;
    XColor *colorPtr;

    /*
     * Set the outline GC for this pen: GCForeground is outline color.
     * GCBackground is the fill color (only used for bitmap symbols).
     */
    gcMask = (GCLineWidth | GCForeground);
    colorPtr = lpPtr->outlineColor;
    if (colorPtr == COLOR_DEFAULT) {
	colorPtr = lpPtr->penColor;
    }
    gcValues.foreground = colorPtr->pixel;
    if (lpPtr->symbol.type == SYMBOL_BITMAP) {
	colorPtr = lpPtr->fillColor;
	if (colorPtr == COLOR_DEFAULT) {
	    colorPtr = lpPtr->penColor;
	}
	/*
	 * Use a clip mask if either
	 *	1) no background color was designated or
	 *	2) a masking bitmap was specified.
	 *
	 * These aren't the bitmaps we'll be using, but it
	 * makes it unlikely that anyone else will be sharing this
	 * when we set the clip origin (as the bitmap is drawn).
	 */
	if (colorPtr != NULL) {
	    gcValues.background = colorPtr->pixel;
	    gcMask |= GCBackground;
	    if (lpPtr->symbol.mask != None) {
		gcValues.clip_mask = lpPtr->symbol.mask;
		gcMask |= GCClipMask;
	    }
	} else {
	    gcValues.clip_mask = lpPtr->symbol.bitmap;
	    gcMask |= GCClipMask;
	}
    }
    gcValues.line_width = LineWidth(lpPtr->outlineWidth);
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (lpPtr->outlineGC != NULL) {
	Tk_FreeGC(graphPtr->display, lpPtr->outlineGC);
    }
    lpPtr->outlineGC = newGC;

    /* Fills for symbols: GCForeground is fill color */

    gcMask = (GCLineWidth | GCForeground);
    colorPtr = lpPtr->fillColor;
    if (colorPtr == COLOR_DEFAULT) {
	colorPtr = lpPtr->penColor;
    }
    newGC = NULL;
    if (colorPtr != NULL) {
	gcValues.foreground = colorPtr->pixel;
	newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    }
    if (lpPtr->fillGC != NULL) {
	Tk_FreeGC(graphPtr->display, lpPtr->fillGC);
    }
    lpPtr->fillGC = newGC;

    /* Line segments */

    gcMask = (GCLineWidth | GCForeground | GCLineStyle | GCCapStyle |
	GCJoinStyle);
    gcValues.cap_style = CapButt;
    gcValues.join_style = JoinRound;
    gcValues.line_style = LineSolid;
    gcValues.line_width = LineWidth(lpPtr->lineWidth);

    colorPtr = lpPtr->penOffColor;
    if (colorPtr == COLOR_DEFAULT) {
	colorPtr = lpPtr->penColor;
    }
    if (colorPtr != NULL) {
	gcMask |= GCBackground;
	gcValues.background = colorPtr->pixel;
    }
    gcValues.foreground = lpPtr->penColor->pixel;
    if (lpPtr->dashes.numValues > 0) {
	gcValues.line_width = lpPtr->lineWidth;
	gcValues.line_style =
	    (colorPtr == NULL) ? LineOnOffDash : LineDoubleDash;
    }
    newGC = Blt_GetPrivateGC(graphPtr->tkwin, gcMask, &gcValues);
    if (lpPtr->penGC != NULL) {
	Blt_FreePrivateGC(graphPtr->display, lpPtr->penGC);
    }
    if (lpPtr->dashes.numValues > 0) {
	lpPtr->dashes.offset = lpPtr->dashes.valueArr[0] / 2;
	Blt_SetDashes(graphPtr->display, newGC, &(lpPtr->dashes));
    }
    lpPtr->penGC = newGC;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyPen --
 *
 *	Release memory and resources allocated for the style.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Everything associated with the pen style is freed up.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyPen(graphPtr, penPtr)
    Graph *graphPtr;
    Pen *penPtr;
{
    LinePen *lpPtr = (LinePen *)penPtr;

    if (lpPtr->outlineGC != NULL) {
	Tk_FreeGC(graphPtr->display, lpPtr->outlineGC);
    }
    if (lpPtr->fillGC != NULL) {
	Tk_FreeGC(graphPtr->display, lpPtr->fillGC);
    }
    if (lpPtr->penGC != NULL) {
	Blt_FreePrivateGC(graphPtr->display, lpPtr->penGC);
    }
    if (lpPtr->symbol.bitmap != None) {
	Tk_FreeBitmap(graphPtr->display, lpPtr->symbol.bitmap);
	lpPtr->symbol.bitmap = None;
    }
    if (lpPtr->symbol.mask != None) {
	Tk_FreeBitmap(graphPtr->display, lpPtr->symbol.mask);
	lpPtr->symbol.mask = None;
    }
}


static void
InitPen(penPtr)
    LinePen *penPtr;
{
    penPtr->outlineWidth = 1;
    penPtr->lineWidth = 1;
    penPtr->symbol.type = SYMBOL_CIRCLE;
    penPtr->symbol.bitmap = None;
    penPtr->symbol.mask = None;
    penPtr->outlineColor = COLOR_DEFAULT;
    penPtr->fillColor = COLOR_DEFAULT;
    penPtr->configSpecs = penConfigSpecs;
    penPtr->configProc = ConfigurePen;
    penPtr->destroyProc = DestroyPen;
    penPtr->flags = NORMAL_PEN;
    penPtr->name = "";
}

Pen *
Blt_LinePen(penName)
    char *penName;
{
    LinePen *penPtr;

    penPtr = (LinePen *)calloc(1, sizeof(LinePen));
    assert(penPtr);
    InitPen(penPtr);
    penPtr->name = strdup(penName);
    if (strcmp(penName, "activeLine") == 0) {
	penPtr->flags = ACTIVE_PEN;
    }
    return (Pen *) penPtr;
}

/*
 * ----------------------------------------------------------------------
 *
 *	In this section, the routines deal with building and filling
 *	the element's data structures with transformed screen
 *	coordinates.  They are triggered from TranformLine which is
 *	called whenever the data or coordinates axes have changed and
 *	new screen coordinates need to be calculated.
 *
 * ----------------------------------------------------------------------
 */

/*
 *----------------------------------------------------------------------
 *
 * ScaleSymbol --
 *
 *	Returns the scaled size for the line element. Scaling depends
 *	upon when the base line ranges for the element were set and
 *	the current range of the graph.
 *
 * Results:
 *	The new size of the symbol, after considering how much the
 *	graph has been scaled, is returned.
 *
 *----------------------------------------------------------------------
 */
static int
ScaleSymbol(linePtr, normalSize)
    Line *linePtr;
    int normalSize;
{
    int maxSize;
    double scale;
    int newSize;

    scale = 1.0;
    if (linePtr->scaleSymbols) {
	double xRange, yRange;

	xRange = linePtr->axes.x->range;
	yRange = linePtr->axes.y->range;
	if (linePtr->flags & LINE_INIT_SYMBOL_SCALE) {
	    /* Save the ranges as a baseline for future scaling. */
	    linePtr->xRange = xRange;
	    linePtr->yRange = yRange;
	    linePtr->flags &= ~LINE_INIT_SYMBOL_SCALE;
	} else {
	    double xScale, yScale;

	    /* Scale the symbol by the smallest change in the X or Y axes */
	    xScale = linePtr->xRange / xRange;
	    yScale = linePtr->yRange / yRange;
	    scale = MIN(xScale, yScale);
	}
    }
    newSize = Round(normalSize * scale);
    /*
     * Don't let the size of symbols go unbounded. The X drawing routines assume
     * coordinates to be a signed short int.
     */
    maxSize = (int)MIN(linePtr->graphPtr->hRange, linePtr->graphPtr->vRange);
    if (newSize > maxSize) {
	newSize = maxSize;
    }
    /* Make the symbol size odd so that its center is a single pixel. */
    newSize |= 0x01;
    return newSize;
}

/*
 *----------------------------------------------------------------------
 *
 * GetWeights --
 *
 *	Creates an array of style indices and fills it based on the weight
 *	of each data point.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Memory is freed and allocated for the index array.
 *
 *----------------------------------------------------------------------
 */
static int *
GetWeights(linePtr, numPoints)
    Line *linePtr;
    int numPoints;
{
    register int i;
    register int styleNum;
    int numWeights;		/* Number of weights to be examined.  If there
				 * are more data points, they will default to
				 * the normal pen. */
    int *styleDir;		/* Directory of styles.  Each element represents
				 * the style for the data point at that index */
    LineStyle *stylePtr;
    double *w;			/* Weight vector */

    /*
     * Create an array of style indices, initializing each index to 0.  The 0th
     * index represents the default line style (i.e. "normal" pen).
     */
    styleDir = (int *)calloc(numPoints, sizeof(int));
    assert(styleDir);
    numWeights = MIN(linePtr->w.numValues, numPoints);
    w = linePtr->w.valueArr;

    stylePtr = linePtr->styles;
    stylePtr++;

    for (styleNum = 1; styleNum < linePtr->numStyles; styleNum++, stylePtr++) {
	stylePtr->symbolSize = ScaleSymbol(linePtr, stylePtr->penPtr->symbol.size);
	for (i = 0; i < numWeights; i++) {
	    if ((styleDir[i] > 0) || (OutOfRange(w[i], &(stylePtr->weight)))) {
		continue;	/* Don't overwrite styles already set */
	    }
	    styleDir[i] = styleNum;
	}
    }
    return styleDir;
}

/*
 *----------------------------------------------------------------------
 *
 * GetScreenPoints --
 *
 *	Generates a coordinate array of transformed screen coordinates
 *	from the data points.
 *
 * Results:
 *	The transformed screen coordinates are returned.
 *
 * Side effects:
 *	Memory is allocated for the coordinate array.
 *
 *----------------------------------------------------------------------
 */
static void
GetScreenPoints(graphPtr, linePtr, tmpPtr)
    Graph *graphPtr;
    Line *linePtr;
    TransformInfo *tmpPtr;
{
    double *x, *y;
    register int i;
    register Point2D *pointArr;
    register int *indexArr;
    int numPoints;

    numPoints = NumberOfPoints(linePtr);
    x = linePtr->x.valueArr;
    y = linePtr->y.valueArr;
    pointArr = (Point2D *)malloc(sizeof(Point2D) * numPoints);
    assert(pointArr);
    indexArr = (int *)malloc(sizeof(int) * numPoints);
    assert(indexArr);

    for (i = 0; i < numPoints; i++) {
	pointArr[i] = Blt_Transform2DPt(graphPtr, x[i], y[i], &(linePtr->axes));
	indexArr[i] = i;
    }
    tmpPtr->points = pointArr;
    tmpPtr->numPoints = numPoints;
    tmpPtr->indexArr = indexArr;
}

/*
 *----------------------------------------------------------------------
 *
 * ReducePoints --
 *
 *	Generates a coordinate array of transformed screen coordinates
 *	from the data points.
 *
 * Results:
 *	The transformed screen coordinates are returned.
 *
 * Side effects:
 *	Memory is allocated for the coordinate array.
 *
 *----------------------------------------------------------------------
 */
static void
ReducePoints(tmpPtr)
    TransformInfo *tmpPtr;
{
    register int i, n;
    Point2D *points;
    int *indexArr;

    points = tmpPtr->points;
    indexArr = tmpPtr->indexArr;
    n = 0;
    for (i = 1; i < tmpPtr->numPoints; i++) {
	if ((ROUND(points[i].x) == ROUND(points[n].x)) &&
	    (ROUND(points[i].y) == ROUND(points[n].y))) {
	    continue;		/* Points are the same. */
	}
	n++;
	if (n < i) {
	    points[n] = points[i];
	    indexArr[n] = tmpPtr->indexArr[i];
	}
    }
    n++;
#ifdef notdef
    fprintf(stderr, "reduced from %d to %d\n", tmpPtr->numPoints, n);
#endif
    tmpPtr->numPoints = n;
}

/*
 *----------------------------------------------------------------------
 *
 * GenerateSteps --
 *
 *	Resets the coordinate and pen index arrays adding new held
 *	points for step-and-hold type smoothing.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	The temporary arrays for screen coordinates and pen indices
 *	are updated.
 *
 *----------------------------------------------------------------------
 */
static void
GenerateSteps(tmpPtr)
    TransformInfo *tmpPtr;
{
    int newSize;
    register int i, count;
    Point2D *newArr;
    int *indexArr;

    newSize = ((tmpPtr->numPoints - 1) * 2) + 1;
    newArr = (Point2D *)malloc(newSize * sizeof(Point2D));
    assert(newArr);
    indexArr = (int *)malloc(sizeof(int) * newSize);
    assert(indexArr);

    newArr[0] = tmpPtr->points[0];
    indexArr[0] = 0;

    count = 1;
    for (i = 1; i < tmpPtr->numPoints; i++) {
	newArr[count + 1] = tmpPtr->points[i];

	/* Hold last y coordinate, use new x coordinate */
	newArr[count].x = newArr[count + 1].x;
	newArr[count].y = newArr[count - 1].y;

	/* Use the same style for both the hold and the step points */
	indexArr[count] = indexArr[count + 1] = tmpPtr->indexArr[i];
	count += 2;
    }
    free((char *)tmpPtr->points);
    free((char *)tmpPtr->indexArr);
    tmpPtr->indexArr = indexArr;
    tmpPtr->points = newArr;
    tmpPtr->numPoints = newSize;
}

/*
 *----------------------------------------------------------------------
 *
 * GenerateSpline --
 *
 *	Computes a spline based upon the data points, returning a
 *	new (larger) coordinate array or points.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	The temporary arrays for screen coordinates and data indices
 *	are updated based upon spline.
 *
 *----------------------------------------------------------------------
 */
static void
GenerateSpline(graphPtr, linePtr, tmpPtr)
    Graph *graphPtr;
    Line *linePtr;
    TransformInfo *tmpPtr;
{
    int extraPoints;
    double *x1, *x2, *y1, *y2;
    register int i, j, count;
    Point2D *newArr;
    int *indexArr;
    int newSize;
    int failed;
    int x;

    assert(tmpPtr->numPoints > 0);
    for (i = 0, j = 1; j < tmpPtr->numPoints; i++, j++) {
	if (tmpPtr->points[j].x <= tmpPtr->points[i].x) {
	    return;		/* Points are not monotonically increasing */
	}
    }
    if ((tmpPtr->points[0].x > (double)graphPtr->xMax) ||
	(tmpPtr->points[tmpPtr->numPoints - 1].x < (double)graphPtr->xMin)) {
	return;			/* All points are clipped */
    }
    /*
     * The abscissas of the extra interpolated points will be each pixel across
     * the plotting area.  This is the advantage of interpolating the screen
     * coordinates instead of the data points.
     */

    extraPoints = (graphPtr->xMax - graphPtr->xMin) + 1;
    if (extraPoints < 1) {
	return;
    }
    newSize = tmpPtr->numPoints + extraPoints + 1;
    newArr = (Point2D *)malloc(newSize * sizeof(Point2D));
    assert(newArr);

    indexArr = (int *)malloc(sizeof(int) * newSize);
    assert(indexArr);
    /*
     * Allocate one big array which will contain smaller arrays of
     *	(2) the original x and y coordinates, and
     *	(2) the interpolated x and y coordinates
     */

    x1 = (double *)malloc(((tmpPtr->numPoints + newSize) * 2) * sizeof(double));
    y1 = x1 + tmpPtr->numPoints;
    x2 = y1 + tmpPtr->numPoints;
    y2 = x2 + newSize;
    assert(x1);

    /*
     * Copy the transformed screen coordinates into x1 and y1 vectors.  This is
     * necessary because the spline routines need the x-y coordinates passed as
     * individual arrays, not an array of Point2D coordinates.
     */

    for (i = 0; i < tmpPtr->numPoints; i++) {
	x1[i] = tmpPtr->points[i].x;
	y1[i] = tmpPtr->points[i].y;
    }

    /*
     * Now populate the x2 array with original x cooridinates and extra
     * x-coordinates representing each pixel in the plotting area the line
     * segment contains.
     */

    count = 0;
    for (i = 0, j = 1; j < tmpPtr->numPoints; i++, j++) {

	/* Add the original x-coordinate */
	x2[count] = x1[i];

	/* Include the starting offset of the point in the offset array */
	indexArr[count] = tmpPtr->indexArr[i];
	count++;

	/*
	 * Is any part of the interval (line segment) in the plotting area?
	 */
	if ((x1[j] >= (double)graphPtr->xMin) ||
	    (x1[i] <= (double)graphPtr->xMax)) {
	    int last;

	    x = (int)(x1[i] + 1.0);

	    /*
	     * Since the line segment may be partially clipped on the left or
	     * right side, the points we need to interpolate will be interior to
	     * the plotting area.
	     *
	     *           xMin			    xMax
	     *      x1----|--------------------------|---x2
	     *
	     * So pick the greater of starting x-coordinate and the left edge
	     * and the lesser of the last x-coordinate and the right edge.
	     */

	    x = MAX(x, graphPtr->xMin);
	    last = (int)MIN(x1[j], graphPtr->xMax);

	    /* Now add extra x-coordinates to visible portion of the interval */
	    while (x < last) {
		indexArr[count] = tmpPtr->indexArr[i];
		x2[count++] = (double)x;
		x++;
	    }
	}
    }

    failed = 0;
    if (linePtr->smooth == PEN_NATURAL) {
	failed = Blt_NaturalSpline(x1, y1, tmpPtr->numPoints, x2, y2, count);
    } else if (linePtr->smooth == PEN_QUADRATIC) {
	failed = Blt_QuadraticSpline(x1, y1, tmpPtr->numPoints, x2, y2, count,
	    0.0);
    }
    if (!failed) {
	/*
	 * Merge the points of the spline, from the x and y vectors, back into
	 * the array of Point2D structures.  Also, update the pen indices.
	 */
	free((char *)tmpPtr->points);
	for (i = 0; i < count; i++) {
	    newArr[i].x = x2[i], newArr[i].y = y2[i];
	}
	free((char *)tmpPtr->indexArr);
	tmpPtr->indexArr = indexArr;
	tmpPtr->points = newArr;
	tmpPtr->numPoints = count;
    } else {
	/*
	 * The spline interpolation failed.  We'll fallback to the current
	 * coordinates and do no smoothing (standard line segments).
	 */
	linePtr->smooth = PEN_LINEAR;
	free((char *)newArr);
	free((char *)indexArr);
    }
    free((char *)x1);
}

/*
 *----------------------------------------------------------------------
 *
 * ComputePoints --
 *
 *	Creates two arrays of points and pen indices, filled with
 *	the screen coordinates of the visible
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Memory is freed and allocated for the index array.
 *
 *----------------------------------------------------------------------
 */
static void
ComputePoints(graphPtr, linePtr, tmpPtr)
    Graph *graphPtr;
    Line *linePtr;
    TransformInfo *tmpPtr;
{
    Extents2D extents;
    XPoint *pointArr;
    int *indexArr;
    register int i, count;
    register Point2D *coordPtr;

    Blt_SetClipRegion(graphPtr, &extents);
    pointArr = (XPoint *)malloc(sizeof(XPoint) * tmpPtr->numPoints);
    assert(pointArr);
    indexArr = (int *)malloc(sizeof(int) * tmpPtr->numPoints);
    assert(indexArr);
    count = 0;
    for (coordPtr = tmpPtr->points, i = 0; i < tmpPtr->numPoints;
	i++, coordPtr++) {
	if (PointInRegion(&extents, coordPtr->x, coordPtr->y)) {
	    pointArr[count].x = (int)(coordPtr->x);
	    pointArr[count].y = (int)(coordPtr->y);
	    indexArr[count] = tmpPtr->indexArr[i];
	    count++;
	}
    }
    linePtr->pointArr = pointArr;
    linePtr->numPoints = count;
    linePtr->pointMap = indexArr;
}

/*
 *----------------------------------------------------------------------
 *
 * ComputeActivePoints --
 *
 *	Creates an array of points of the active graph coordinates.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Memory is freed and allocated for the active point array.
 *
 *----------------------------------------------------------------------
 */
static void
ComputeActivePoints(graphPtr, linePtr)
    Graph *graphPtr;
    Line *linePtr;
{
    Extents2D extents;
    double x, y;
    int numActive;
    Point2D point;
    XPoint *activeArr;
    register int i;
    int activeIndex;
    int numPoints;

    Blt_SetClipRegion(graphPtr, &extents);
    numActive = 0;
    activeArr = (XPoint *)malloc(sizeof(XPoint) * linePtr->reqNumActive);
    assert(activeArr);
    numPoints = NumberOfPoints(linePtr);
    for (i = 0; i < linePtr->reqNumActive; i++) {
	activeIndex = linePtr->reqActiveArr[i];
	if (activeIndex >= numPoints) {
	    continue;		/* Index not available */
	}
	x = linePtr->x.valueArr[activeIndex];
	y = linePtr->y.valueArr[activeIndex];
	point = Blt_Transform2DPt(graphPtr, x, y, &(linePtr->axes));
	if (PointInRegion(&extents, point.x, point.y)) {
	    activeArr[numActive].x = (int)(point.x);
	    activeArr[numActive].y = (int)(point.y);
	    numActive++;
	}
    }
    if (numActive > 0) {
	linePtr->activeArr = activeArr;
    } else {
	/* No indices were available */
	free((char *)activeArr);
    }
    linePtr->numActive = numActive;
    linePtr->flags &= ~ELEM_UPDATE_ACTIVE;
}

/*
 *----------------------------------------------------------------------
 *
 * ComputeSegments --
 *
 *	Creates an array of line segments of the graph coordinates.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Memory is  allocated for the line segment array.
 *
 *----------------------------------------------------------------------
 */
static void
ComputeSegments(graphPtr, linePtr, tmpPtr)
    Graph *graphPtr;
    Line *linePtr;
    TransformInfo *tmpPtr;
{
    Extents2D extents;
    XSegment *segArr;
    int *indexArr;
    register Point2D *p1Ptr, *p2Ptr;
    register int count;
    register int i;

    Blt_SetClipRegion(graphPtr, &extents);
    indexArr = (int *)malloc(sizeof(int) * tmpPtr->numPoints);
    assert(indexArr);
    segArr = (XSegment *)malloc(tmpPtr->numPoints * sizeof(XSegment));
    assert(segArr);
    count = 0;
    p1Ptr = tmpPtr->points;
    p2Ptr = p1Ptr + 1;
    if (tmpPtr->indexArr != NULL) {
	for (i = 1; i < tmpPtr->numPoints; i++, p1Ptr++, p2Ptr++) {
	    if (Blt_ClipSegment(&extents, p1Ptr, p2Ptr, segArr + count)) {
		indexArr[count] = tmpPtr->indexArr[i];
		count++;
	    }
	}
    } else {
	for (i = 1; i < tmpPtr->numPoints; i++, p1Ptr++, p2Ptr++) {
	    if (Blt_ClipSegment(&extents, p1Ptr, p2Ptr, segArr + count)) {
		indexArr[count] = tmpPtr->indexArr[i];
		count++;
	    }
	}
    }
    linePtr->segMap = indexArr;
    linePtr->numSegments = count;
    linePtr->segArr = segArr;
}

/*
 *----------------------------------------------------------------------
 *
 * MergePens --
 *
 *	Reorders the both arrays of points and segments to merge pens.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The old arrays are freed and new ones allocated containing
 *	the reordered points and segments.
 *
 *----------------------------------------------------------------------
 */
static void
MergePens(linePtr)
    Line *linePtr;
{
    LineStyle *stylePtr;
    register int count;
    register int i, styleNum;
    int *styleDir;

    stylePtr = linePtr->styles;
    stylePtr->symbolSize = ScaleSymbol(linePtr, stylePtr->penPtr->symbol.size);

    if (linePtr->numStyles < 2) {
	stylePtr->numSegments = linePtr->numSegments;
	stylePtr->segPtr = linePtr->segArr;
	stylePtr->numPoints = linePtr->numPoints;
	stylePtr->pointPtr = linePtr->pointArr;
	return;
    }
    styleDir = GetWeights(linePtr, NumberOfPoints(linePtr));

    /*
     * We have more than one style, so we need to group line segments and points
     * of like pen styles.
     */
    if (linePtr->numSegments > 0) {
	XSegment *segArr;
	int *indexArr;
	register XSegment *segPtr;
	register int *indexPtr;

	segArr = (XSegment *)malloc(linePtr->numSegments * sizeof(XSegment));
	indexArr = (int *)malloc(linePtr->numSegments * sizeof(int));
	assert(segArr && indexArr);
	segPtr = segArr, indexPtr = indexArr;
	for (stylePtr = linePtr->styles, styleNum = 0;
	    styleNum < linePtr->numStyles; styleNum++, stylePtr++) {
	    count = 0;
	    stylePtr->segPtr = segPtr;
	    for (i = 0; i < linePtr->numSegments; i++) {
		if (styleDir[linePtr->segMap[i]] == styleNum) {
		    *segPtr++ = linePtr->segArr[i];
		    *indexPtr++ = i;
		    count++;
		}
	    }
	    stylePtr->numSegments = count;
	}
	free((char *)linePtr->segArr);
	linePtr->segArr = segArr;
	free((char *)linePtr->segMap);
	linePtr->segMap = indexArr;
    }
    if (linePtr->numPoints > 0) {
	XPoint *pointArr;
	int *indexPtr;
	register XPoint *pointPtr;
	register int *indexArr;

	pointArr = (XPoint *)malloc(linePtr->numPoints * sizeof(XPoint));
	indexArr = (int *)malloc(linePtr->numPoints * sizeof(int));
	assert(pointArr && indexArr);
	pointPtr = pointArr, indexPtr = indexArr;
	for (stylePtr = linePtr->styles, styleNum = 0;
	    styleNum < linePtr->numStyles; styleNum++, stylePtr++) {
	    count = 0;
	    stylePtr->pointPtr = pointPtr;
	    for (i = 0; i < linePtr->numPoints; i++) {
		if (styleDir[linePtr->pointMap[i]] == styleNum) {
		    *pointPtr++ = linePtr->pointArr[i];
		    *indexPtr++ = i;
		    count++;
		}
	    }
	    stylePtr->numPoints = count;
	}
	free((char *)linePtr->pointArr);
	linePtr->pointArr = pointArr;
	free((char *)linePtr->pointMap);
	linePtr->pointMap = indexArr;
    }
    free((char *)styleDir);
}

#define CLIP_TOP	(1<<0)
#define CLIP_BOTTOM	(1<<1)
#define CLIP_RIGHT	(1<<2)
#define CLIP_LEFT	(1<<3)


INLINE static int
OutCode(extentsPtr, x, y)
    Extents2D *extentsPtr;
    double x, y;
{
    int code;

    code = 0;
    if (x > extentsPtr->xMax) {
	code |= CLIP_RIGHT;
    } else if (x < extentsPtr->xMin) {
	code |= CLIP_LEFT;
    }
    if (y > extentsPtr->yMax) {
	code |= CLIP_BOTTOM;
    } else if (y < extentsPtr->yMin) {
	code |= CLIP_TOP;
    }
    return code;
}

static int
ClipSegment(extentsPtr, code1, code2, p1Ptr, p2Ptr)
    Extents2D *extentsPtr;
    register int code1, code2;
    register Point2D *p1Ptr, *p2Ptr;
{
    int inside, outside;

    inside = ((code1 | code2) == 0);
    outside = ((code1 & code2) != 0);

    /*
     * In the worst case, we'll clip the line segment against each of
     * the four sides of the bounding rectangle.
     */
    while ((!outside) && (!inside)) {
	if (code1 == 0) {
	    Point2D *ptr;
	    int code;

	    ptr = p1Ptr;
	    p1Ptr = p2Ptr, p2Ptr = ptr;
	    code = code1, code1 = code2, code2 = code;
	}
	if (code1 & CLIP_LEFT) {
	    p1Ptr->y += (p2Ptr->y - p1Ptr->y) *
		(extentsPtr->xMin - p1Ptr->x) / (p2Ptr->x - p1Ptr->x);
	    p1Ptr->x = extentsPtr->xMin;
	} else if (code1 & CLIP_RIGHT) {
	    p1Ptr->y += (p2Ptr->y - p1Ptr->y) *
		(extentsPtr->xMax - p1Ptr->x) / (p2Ptr->x - p1Ptr->x);
	    p1Ptr->x = extentsPtr->xMax;
	} else if (code1 & CLIP_BOTTOM) {
	    p1Ptr->x += (p2Ptr->x - p1Ptr->x) *
		(extentsPtr->yMax - p1Ptr->y) / (p2Ptr->y - p1Ptr->y);
	    p1Ptr->y = extentsPtr->yMax;
	} else if (code1 & CLIP_TOP) {
	    p1Ptr->x += (p2Ptr->x - p1Ptr->x) *
		(extentsPtr->yMin - p1Ptr->y) / (p2Ptr->y - p1Ptr->y);
	    p1Ptr->y = extentsPtr->yMin;
	}
	code1 = OutCode(extentsPtr, p1Ptr->x, p1Ptr->y);

	inside = ((code1 | code2) == 0);
	outside = ((code1 & code2) != 0);
    }
    return (!inside);
}

/*
 *----------------------------------------------------------------------
 *
 * SaveTrace --
 *
 *	Creates a new trace and inserts it into the line's
 *	list of traces.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
SaveTrace(linePtr, start, length, tmpPtr)
    Line *linePtr;
    int start;			/* Starting index of the trace in data point
				 * array.  Used to figure out closest point */
    int length;			/* Number of points forming the trace */
    TransformInfo *tmpPtr;
{
    Trace *tracePtr;
    XPoint *pointArr;
    int *indexArr;
    register int i, j;

    tracePtr = (Trace *)malloc(sizeof(Trace));
    assert(tracePtr);
    pointArr = (XPoint *)malloc(sizeof(XPoint) * length);
    assert(pointArr);
    indexArr = (int *)malloc(sizeof(int) * length);
    assert(indexArr);

    /* Copy the screen coordinates of the trace into the point array */

    if (tmpPtr->indexArr != NULL) {
	for (i = 0, j = start; i < length; i++, j++) {
	    pointArr[i].x = (int)(tmpPtr->points[j].x);
	    pointArr[i].y = (int)(tmpPtr->points[j].y);
	    indexArr[i] = tmpPtr->indexArr[j];
	}
    } else {
	for (i = 0, j = start; i < length; i++, j++) {
	    pointArr[i].x = (int)(tmpPtr->points[j].x);
	    pointArr[i].y = (int)(tmpPtr->points[j].y);
	    indexArr[i] = j;
	}
    }
    tracePtr->numPoints = length;
    tracePtr->pointArr = pointArr;
    tracePtr->indexArr = indexArr;
    Blt_ListAppend(&(linePtr->traces), (char *)start, (ClientData)tracePtr);
}

/*
 *----------------------------------------------------------------------
 *
 * DeleteTraces --
 *
 *	Deletes all the traces for the line.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
DeleteTraces(linePtr)
    Line *linePtr;
{
    register Blt_ListItem item;
    register Trace *tracePtr;

    for (item = Blt_ListFirstItem(&(linePtr->traces)); item != NULL;
	item = Blt_ListNextItem(item)) {
	tracePtr = (Trace *)Blt_ListGetValue(item);
	free((char *)tracePtr->indexArr);
	free((char *)tracePtr->pointArr);
	free((char *)tracePtr);
    }
    Blt_ListReset(&(linePtr->traces));
}

/*
 *----------------------------------------------------------------------
 *
 * ComputeTraces --
 *
 *	Creates an array of line segments of the graph coordinates.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Memory is  allocated for the line segment array.
 *
 *----------------------------------------------------------------------
 */

static void
ComputeTraces(graphPtr, linePtr, tmpPtr)
    Graph *graphPtr;
    Line *linePtr;
    TransformInfo *tmpPtr;
{
    int start, count;
    int code1, code2;
    Point2D *p1Ptr, *p2Ptr;
    Point2D savePoint;
    Extents2D extents;
    register int i;
    int broken, offscreen;

    Blt_SetClipRegion(graphPtr, &extents);
    count = 1;
    code1 = OutCode(&extents, tmpPtr->points[0].x, tmpPtr->points[0].y);
    p1Ptr = tmpPtr->points;
    p2Ptr = p1Ptr + 1;
    for (i = 1; i < tmpPtr->numPoints; i++, p1Ptr++, p2Ptr++) {
	code2 = OutCode(&extents, p2Ptr->x, p2Ptr->y);
	if (code2 != 0) {
	    /* Save the coordinates of the last point, before clipping */
	    savePoint = *p2Ptr;
	}
	broken = BROKEN_TRACE(linePtr->penDir, p1Ptr->x, p2Ptr->x);
	offscreen = ClipSegment(&extents, code1, code2, p1Ptr, p2Ptr);
	if (broken || offscreen) {

	    /*
	     * The last line segment is either totally clipped by the plotting
	     * area or the x-direction is wrong, breaking the trace.  Either
	     * way, save information about the last trace (if one exists),
	     * discarding the current line segment
	     */

	    if (count > 1) {
		start = i - count;
		SaveTrace(linePtr, start, count, tmpPtr);
		count = 1;
	    }
	} else {
	    count++;		/* Add the point to the trace. */
	    if (code2 != 0) {

		/*
		 * If the last point is clipped, this means that the trace is
		 * broken after this point.  Restore the original coordinate
		 * (before clipping) after saving the trace.
		 */

		start = i - (count - 1);
		SaveTrace(linePtr, start, count, tmpPtr);
		tmpPtr->points[i] = savePoint;
		count = 1;
	    }
	}
	code1 = code2;
    }
    if (count > 1) {
	start = i - count;
	SaveTrace(linePtr, start, count, tmpPtr);
    }
}

static void
ResetLineInfo(linePtr)
    Line *linePtr;
{
    DeleteTraces(linePtr);
    if (linePtr->pointArr != NULL) {
	free((char *)linePtr->pointArr);
    }
    if (linePtr->pointMap != NULL) {
	free((char *)linePtr->pointMap);
    }
    if (linePtr->segArr != NULL) {
	free((char *)linePtr->segArr);
    }
    if (linePtr->segMap != NULL) {
	free((char *)linePtr->segMap);
    }
    if (linePtr->activeArr != NULL) {
	free((char *)linePtr->activeArr);
    }
    linePtr->segArr = NULL;
    linePtr->pointArr = NULL;
    linePtr->activeArr = NULL;
    linePtr->segMap = linePtr->pointMap = NULL;
    ClearStyles(linePtr);
    linePtr->numActive = linePtr->numPoints = linePtr->numSegments = 0;
}

/*
 *----------------------------------------------------------------------
 *
 * TransformLine --
 *
 *	Calculates the actual window coordinates of the line element.
 *	The window coordinates are saved in an allocated point array.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Memory is (re)allocated for the point array.
 *
 *----------------------------------------------------------------------
 */
static void
TransformLine(graphPtr, elemPtr)
    Graph *graphPtr;		/* Graph widget record */
    Element *elemPtr;		/* Element component record */
{
    Line *linePtr = (Line *)elemPtr;
    TransformInfo tmp;
    int numPoints;

    ResetLineInfo(linePtr);
    numPoints = NumberOfPoints(linePtr);
    if (numPoints < 1) {
	return;			/* No data points */
    }
    GetScreenPoints(graphPtr, linePtr, &tmp);
    ReducePoints(&tmp);
    ComputePoints(graphPtr, linePtr, &tmp);
    if ((linePtr->flags & ELEM_UPDATE_ACTIVE) && (linePtr->reqNumActive > 0)) {
	ComputeActivePoints(graphPtr, linePtr);
    }
    /*
     * Compute connecting line segments if they are to be displayed.
     */
    if ((numPoints > 1) &&
	((graphPtr->type == TYPE_ELEM_STRIP) ||
	    (linePtr->normalPen.lineWidth > 0))) {
	linePtr->smooth = linePtr->reqSmooth;

	/*
	 * Do smoothing if necessary.  This can extend the coordinate array,
	 * so both tmp.points and tmp.numPoints may be change.
	 */

	switch (linePtr->smooth) {
	case PEN_STEP:
	    GenerateSteps(&tmp);
	    break;

	case PEN_NATURAL:
	case PEN_QUADRATIC:
	    if (tmp.numPoints < 3) {
		/* Can't interpolate with only two points. */
		linePtr->smooth = PEN_LINEAR;
	    } else {
		GenerateSpline(graphPtr, linePtr, &tmp);
	    }
	    break;
	}

	if (graphPtr->type == TYPE_ELEM_STRIP) {
	    ComputeSegments(graphPtr, linePtr, &tmp);
	} else {
	    ComputeTraces(graphPtr, linePtr, &tmp);
	}
    }
    free((char *)tmp.points);
    free((char *)tmp.indexArr);
    MergePens(linePtr);
}

/*
 *----------------------------------------------------------------------
 *
 * ClosestPoint --
 *
 *	Find the element whose data point is closest to the given screen
 *	coordinate.
 *
 * Results:
 *	If a new minimum distance is found, the information regarding
 *	it is returned via searchPtr.
 *
 *----------------------------------------------------------------------
 */
static void
ClosestPoint(linePtr, searchPtr)
    Line *linePtr;		/* Line element that we are looking at */
    ClosestSearch *searchPtr;	/* Assorted information related to searching
				 * for the closest point */
{
    double dist, minDist;
    double dx, dy;
    register int i;
    int dataIndex;

    minDist = searchPtr->dist;

    /*
     * Test the sample point from the array of transformed points instead of
     * the line's data points.
     *
     * The advantages are
     *   1) we won't select a point that's offscreen, and
     *   2) the computed distance is already in screen coordinates.
     */
    for (i = 0; i < linePtr->numPoints; i++) {
	dx = (double)(searchPtr->x - linePtr->pointArr[i].x);
	dy = (double)(searchPtr->y - linePtr->pointArr[i].y);
	dist = hypot(dx, dy);
	if (dist < minDist) {
	    dataIndex = linePtr->pointMap[i];
	    minDist = dist;
	}
    }
    if (minDist < searchPtr->dist) {
	searchPtr->elemPtr = (Element *)linePtr;
	searchPtr->dist = minDist;
	searchPtr->index = dataIndex;
	searchPtr->point.x = linePtr->x.valueArr[dataIndex];
	searchPtr->point.y = linePtr->y.valueArr[dataIndex];
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ClosestSegment --
 *
 *	Find the line segment closest to the given window coordinate
 *	in the element.
 *
 * Results:
 *	If a new minimum distance is found, the information regarding
 *	it is returned via searchPtr.
 *
 *----------------------------------------------------------------------
 */
static void
ClosestSegment(graphPtr, linePtr, searchPtr)
    Graph *graphPtr;		/* Graph widget record */
    Line *linePtr;		/* Line element record */
    ClosestSearch *searchPtr;	/* Info about closest point in element */
{
    double dist, minDist;
    Point2D savePoint, proj;
    register XSegment *segPtr;
    register int i;
    int location = 0;
    int xMin, xMax, yMin, yMax;

    minDist = searchPtr->dist;
    for (segPtr = linePtr->segArr, i = 0; i < linePtr->numSegments;
	i++, segPtr++) {
	proj = Blt_GetProjection(searchPtr->x, searchPtr->y,
	    segPtr->x1, segPtr->y1, segPtr->x2, segPtr->y2);
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
	if (proj.x > xMax) {
	    proj.x = xMax;
	} else if (proj.x < xMin) {
	    proj.x = xMin;
	}
	if (proj.y > yMax) {
	    proj.y = yMax;
	} else if (proj.y < yMin) {
	    proj.y = yMin;
	}
	dist = hypot((proj.x - searchPtr->x), (proj.y - searchPtr->y));
	if (dist < minDist) {
	    savePoint = proj;
	    location = linePtr->segMap[i];
	    minDist = dist;
	}
    }
    if (minDist < searchPtr->dist) {
	searchPtr->dist = minDist;
	searchPtr->elemPtr = (Element *)linePtr;
	searchPtr->index = location;
	searchPtr->point = Blt_InvTransform2DPt(graphPtr, savePoint.x,
	    savePoint.y, &(linePtr->axes));
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ClosestTrace --
 *
 *	Find the line segment closest to the given window coordinate
 *	in the element.
 *
 * Results:
 *	If a new minimum distance is found, the information regarding
 *	it is returned via searchPtr.
 *
 *----------------------------------------------------------------------
 */
static void
ClosestTrace(graphPtr, linePtr, searchPtr)
    Graph *graphPtr;		/* Graph widget record */
    Line *linePtr;		/* Line element record */
    ClosestSearch *searchPtr;	/* Info about closest point in element */
{
    double dist, minDist;
    Point2D savePoint, proj;
    XPoint *p1Ptr, *p2Ptr;
    register int i;
    int location = 0;
    Trace *tracePtr;
    Blt_ListItem item;
    int xMin, xMax, yMin, yMax;

    minDist = searchPtr->dist;
    for (item = Blt_ListFirstItem(&(linePtr->traces)); item != NULL;
	item = Blt_ListNextItem(item)) {
	tracePtr = (Trace *)Blt_ListGetValue(item);
	p1Ptr = tracePtr->pointArr, p2Ptr = p1Ptr + 1;
	for (i = 0; i < (tracePtr->numPoints - 1); i++, p1Ptr++, p2Ptr++) {
	    proj = Blt_GetProjection(searchPtr->x, searchPtr->y, p1Ptr->x,
		p1Ptr->y, p2Ptr->x, p2Ptr->y);
	    if (p1Ptr->x > p2Ptr->x) {
		xMax = p1Ptr->x, xMin = p2Ptr->x;
	    } else {
		xMin = p1Ptr->x, xMax = p2Ptr->x;
	    }
	    if (p1Ptr->y > p2Ptr->y) {
		yMax = p1Ptr->y, yMin = p2Ptr->y;
	    } else {
		yMin = p1Ptr->y, yMax = p2Ptr->y;
	    }
	    if (proj.x > (double)xMax) {
		proj.x = (double)xMax;
	    } else if (proj.x < (double)xMin) {
		proj.x = (double)xMin;
	    }
	    if (proj.y > (double)yMax) {
		proj.y = (double)yMax;
	    } else if (proj.y < (double)yMin) {
		proj.y = (double)yMin;
	    }
	    dist = hypot((proj.x - (double)searchPtr->x),
		(proj.y - (double)searchPtr->y));
	    if (dist < minDist) {
		savePoint = proj;
		location = tracePtr->indexArr[i];
		minDist = dist;
	    }
	}
    }
    if (minDist < searchPtr->dist) {
	searchPtr->dist = minDist;
	searchPtr->elemPtr = (Element *)linePtr;
	searchPtr->index = location;
	searchPtr->point = Blt_InvTransform2DPt(graphPtr, savePoint.x,
	    savePoint.y, &(linePtr->axes));
    }
}

/*
 *----------------------------------------------------------------------
 *
 * GetLimits --
 *
 *	Retrieves the limits of the line element
 *
 * Results:
 *	Returns the number of data points in the element.
 *
 *----------------------------------------------------------------------
 */
static void
GetLimits(vecPtr, logScale, minPtr, maxPtr)
    ElemVector *vecPtr;
    int logScale;
    double *minPtr, *maxPtr;
{
    *minPtr = bltPosInfinity, *maxPtr = bltNegInfinity;
    if (vecPtr->numValues > 0) {
	*minPtr = vecPtr->min;
	if ((*minPtr <= 0.0) && (logScale)) {
	    *minPtr = Blt_FindElemVectorMinimum(vecPtr, DBL_MIN);
	}
	*maxPtr = vecPtr->max;
    }
}

/*ARGSUSED*/
static void
ExtentsLine(elemPtr, extsPtr)
    Element *elemPtr;
    Extents2D *extsPtr;
{
    GetLimits(&(elemPtr->x), elemPtr->axes.x->logScale,
	&(extsPtr->xMin), &(extsPtr->xMax));
    GetLimits(&(elemPtr->y), elemPtr->axes.y->logScale,
	&(extsPtr->yMin), &(extsPtr->yMax));
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureLine --
 *
 *	Sets up the appropriate configuration parameters in the GC.
 *      It is assumed the parameters have been previously set by
 *	a call to Tk_ConfigureWidget.
 *
 * Results:
 *	The return value is a standard Tcl result.  If TCL_ERROR is
 *	returned, then interp->result contains an error message.
 *
 * Side effects:
 *	Configuration information such as line width, line style, color
 *	etc. get set in a new GC.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ConfigureLine(graphPtr, elemPtr)
    Graph *graphPtr;
    Element *elemPtr;
{
    Line *linePtr = (Line *)elemPtr;
    Tk_ConfigSpec *specsPtr;

    if (ConfigurePen(graphPtr, (Pen *)&(linePtr->normalPen)) != TCL_OK) {
	return TCL_ERROR;
    }
    /*
     * Point to the static normal/active pens if no external pens have
     * been selected.
     */
    if (linePtr->normalPenPtr == NULL) {
	linePtr->normalPenPtr = &(linePtr->normalPen);
    }
    if (linePtr->styles != NULL) {
	linePtr->styles[0].penPtr = linePtr->normalPenPtr;
    }
    specsPtr = linePtr->infoPtr->configSpecs;
    if (Blt_ConfigModified(specsPtr, "-scalesymbols", (char *)NULL)) {
	linePtr->flags |= (COORDS_NEEDED | LINE_INIT_SYMBOL_SCALE);
    }
    if (Blt_ConfigModified(specsPtr, "-pixels", "-trace", "-*data", "-smooth",
	    "-map*", "-label", "-hide", (char *)NULL)) {
	linePtr->flags |= COORDS_NEEDED;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ClosestLine --
 *
 *	Find the closest point or line segment (if interpolated) to
 *	the given window coordinate in the line element.
 *
 * Results:
 *	Returns the distance of the closest point among other
 *	information.
 *
 *----------------------------------------------------------------------
 */
static void
ClosestLine(graphPtr, elemPtr, searchPtr)
    Graph *graphPtr;		/* Graph widget record */
    Element *elemPtr;		/* Element to examine */
    ClosestSearch *searchPtr;	/* Info about closest point in element */
{
    Line *linePtr = (Line *)elemPtr;

    if (searchPtr->interpolate) {
	if (elemPtr->type == TYPE_ELEM_STRIP) {
	    ClosestSegment(graphPtr, linePtr, searchPtr);
	} else {
	    ClosestTrace(graphPtr, linePtr, searchPtr);
	}
    } else {
	ClosestPoint(linePtr, searchPtr);
    }
}

/*
 * -----------------------------------------------------------------
 *
 * DrawSymbols --
 *
 * 	Draw the symbols centered at the each given x,y coordinate
 *	in the array of points.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Draws a symbol at each coordinate given.  If active,
 *	only those coordinates which are currently active are
 *	drawn.
 *
 * -----------------------------------------------------------------
 */
static void
DrawSymbols(graphPtr, drawable, penPtr, size, numPoints, pointPtr)
    Graph *graphPtr;		/* Graph widget record */
    Drawable drawable;		/* Pixmap or window to draw into */
    LinePen *penPtr;
    int size;			/* Size of element */
    int numPoints;		/* Number of coordinates in array */
    register XPoint *pointPtr;	/* Array of x,y coordinates for line */
{
    XPoint template[13];	/* Template for polygon symbols */
    int r1, r2;
    register int i, n;
    int maxReqWords;
#define SQRT_PI		1.77245385090552
#define S_RATIO		0.886226925452758

    /*
     * XDrawLines() points:  XMaxRequestSize(dpy) - 3
     * XFillPolygon() points:  XMaxRequestSize(dpy) - 4
     * XDrawSegments() segments:  (XMaxRequestSize(dpy) - 3) / 2
     * XDrawRectangles() rectangles:  (XMaxRequestSize(dpy) - 3) / 2
     * XFillRectangles() rectangles:  (XMaxRequestSize(dpy) - 3) / 2
     * XDrawArcs() or XFillArcs() arcs:  (XMaxRequestSize(dpy) - 3) / 3
     */

#define MAXDRAWLINES		(maxReqWords - 3)
#define MAXDRAWPOLYGON		(maxReqWords - 4)
#define MAXDRAWSEGMENTS		((maxReqWords - 3) / 2)
#define MAXDRAWRECTANGLES	((maxReqWords - 3) / 2)
#define MAXDRAWARCS		((maxReqWords - 3) / 3)

    maxReqWords = XMaxRequestSize(graphPtr->display);
    if (size < 3) {
	if (penPtr->fillGC != NULL) {
	    XDrawPoints(graphPtr->display, drawable, penPtr->fillGC, pointPtr,
		numPoints, CoordModeOrigin);
	}
	return;
    }
    r1 = (int)ceil(size * 0.5);
    r2 = (int)ceil(size * S_RATIO * 0.5);

    switch (penPtr->symbol.type) {
    case SYMBOL_NONE:
	break;

    case SYMBOL_SQUARE:
	{
	    XRectangle *rectArr;
	    register XRectangle *rectPtr;
	    int reqSize, numRects;
	    int s;

	    s = r2 + r2;
	    rectArr = (XRectangle *)malloc(numPoints * sizeof(XRectangle));
	    rectPtr = rectArr;
	    for (i = 0; i < numPoints; i++, pointPtr++, rectPtr++) {
		rectPtr->x = pointPtr->x - r2;
		rectPtr->y = pointPtr->y - r2;
		rectPtr->width = rectPtr->height = (unsigned short)s;
	    }
	    reqSize = MAXDRAWRECTANGLES;
	    for (i = 0; i < numPoints; i += reqSize) {
		numRects =
		    ((i + reqSize) > numPoints) ? (numPoints - i) : reqSize;
		if (penPtr->fillGC != NULL) {
		    XFillRectangles(graphPtr->display, drawable, penPtr->fillGC,
			rectArr + i, numRects);
		}
		if (penPtr->outlineWidth > 0) {
		    XDrawRectangles(graphPtr->display, drawable,
			penPtr->outlineGC, rectArr + i, numRects);
		}
	    }
	    free((char *)rectArr);
	}
	break;

    case SYMBOL_CIRCLE:
	{
	    XArc *arcArr;	/* Array of arcs (circle) */
	    register XArc *arcPtr;
	    int reqSize, numArcs;
	    int s;

	    s = r1 + r1;
	    arcArr = (XArc *) malloc(numPoints * sizeof(XArc));
	    arcPtr = arcArr;
	    for (i = 0; i < numPoints; i++, arcPtr++, pointPtr++) {
		arcPtr->x = pointPtr->x - r1;
		arcPtr->y = pointPtr->y - r1;
		arcPtr->width = arcPtr->height = (unsigned short)s;
		arcPtr->angle1 = 0;
		arcPtr->angle2 = 23040;
	    }
	    reqSize = MAXDRAWARCS;
	    for (i = 0; i < numPoints; i += reqSize) {
		numArcs = reqSize;
		if ((i + reqSize) > numPoints) {
		    numArcs = numPoints - i;
		}
		if (penPtr->fillGC != NULL) {
		    XFillArcs(graphPtr->display, drawable, penPtr->fillGC,
			arcArr + i, numArcs);
		}
		if (penPtr->outlineWidth > 0) {
		    XDrawArcs(graphPtr->display, drawable, penPtr->outlineGC,
			arcArr + i, numArcs);
		}
	    }
	    free((char *)arcArr);
	}
	break;

    case SYMBOL_SPLUS:
    case SYMBOL_SCROSS:
	{
	    XSegment *segArr;	/* Array of line segments (splus, scross) */
	    register XSegment *segPtr;
	    int reqSize, numSegments, numSegs;

	    if (penPtr->symbol.type == SYMBOL_SCROSS) {
		r2 = Round(r2 * M_SQRT1_2);
		template[3].y = template[2].x = template[0].x = template[0].y = -r2;
		template[3].x = template[2].y = template[1].y = template[1].x = r2;
	    } else {
		template[0].y = template[1].y = template[2].x = template[3].x = 0;
		template[0].x = template[2].y = -r2;
		template[1].x = template[3].y = r2;
	    }
	    numSegments = numPoints * 2;
	    segArr = (XSegment *)malloc(numSegments * sizeof(XSegment));
	    segPtr = segArr;
	    for (i = 0; i < numPoints; i++, pointPtr++) {
		segPtr->x1 = template[0].x + pointPtr->x;
		segPtr->y1 = template[0].y + pointPtr->y;
		segPtr->x2 = template[1].x + pointPtr->x;
		segPtr->y2 = template[1].y + pointPtr->y;
		segPtr++;
		segPtr->x1 = template[2].x + pointPtr->x;
		segPtr->y1 = template[2].y + pointPtr->y;
		segPtr->x2 = template[3].x + pointPtr->x;
		segPtr->y2 = template[3].y + pointPtr->y;
		segPtr++;
	    }
	    /* Always draw skinny symbols regardless of the outline width */
	    reqSize = MAXDRAWSEGMENTS;
	    for (i = 0; i < numSegments; i += reqSize) {
		numSegs = ((i + reqSize) > numSegments)
		    ? (numSegments - i) : reqSize;
		XDrawSegments(graphPtr->display, drawable, penPtr->outlineGC,
		    segArr + i, numSegs);
	    }
	    free((char *)segArr);
	}
	break;

    case SYMBOL_PLUS:
    case SYMBOL_CROSS:
	{
	    XPoint *polygon;
	    register XPoint *pntPtr;
	    int d;		/* Small delta for cross/plus thickness */

	    d = (r2 / 3);

	    /*
	     *
	     *          2   3       The plus/cross symbol is a closed polygon
	     *                      of 12 points. The diagram to the left
	     *    0,12  1   4    5  represents the positions of the points
	     *           x,y        which are computed below. The extra
	     *     11  10   7    6  (thirteenth) point connects the first and
	     *                      last points.
	     *          9   8
	     */

	    template[0].x = template[11].x = template[12].x = -r2;
	    template[2].x = template[1].x = template[10].x = template[9].x = -d;
	    template[3].x = template[4].x = template[7].x = template[8].x = d;
	    template[5].x = template[6].x = r2;
	    template[2].y = template[3].y = -r2;
	    template[0].y = template[1].y = template[4].y = template[5].y =
		template[12].y = -d;
	    template[11].y = template[10].y = template[7].y = template[6].y = d;
	    template[9].y = template[8].y = r2;

	    if (penPtr->symbol.type == SYMBOL_CROSS) {
		double dx, dy;

		/* For the cross symbol, rotate the points by 45 degrees. */
		for (n = 0; n < 12; n++) {
		    dx = (double)template[n].x * M_SQRT1_2;
		    dy = (double)template[n].y * M_SQRT1_2;
		    template[n].x = Round(dx - dy);
		    template[n].y = Round(dx + dy);
		}
		template[12] = template[0];
	    }
	    polygon = (XPoint *)malloc(numPoints * 13 * sizeof(XPoint));
	    pntPtr = polygon;
	    for (i = 0; i < numPoints; i++, pointPtr++) {
		for (n = 0; n < 13; n++, pntPtr++) {
		    pntPtr->x = template[n].x + pointPtr->x;
		    pntPtr->y = template[n].y + pointPtr->y;
		}
	    }

	    if (penPtr->fillGC != NULL) {
		for (pntPtr = polygon, i = 0; i < numPoints; i++, pntPtr += 13) {
		    XFillPolygon(graphPtr->display, drawable, penPtr->fillGC,
			pntPtr, 13, Complex, CoordModeOrigin);
		}
	    }
	    if (penPtr->outlineWidth > 0) {
		for (pntPtr = polygon, i = 0; i < numPoints; i++, pntPtr += 13) {
		    XDrawLines(graphPtr->display, drawable, penPtr->outlineGC,
			pntPtr, 13, CoordModeOrigin);
		}
	    }
	    free((char *)polygon);
	}
	break;

    case SYMBOL_DIAMOND:
	{
	    XPoint *polygon;
	    register XPoint *pntPtr;

	    /*
	     *
	     *                      The plus symbol is a closed polygon
	     *            1         of 4 points. The diagram to the left
	     *                      represents the positions of the points
	     *       0,4 x,y  2     which are computed below. The extra
	     *                      (fifth) point connects the first and
	     *            3         last points.
	     *
	     */
	    template[1].y = template[0].x = -r1;
	    template[2].y = template[3].x = template[0].y = template[1].x = 0;
	    template[3].y = template[2].x = r1;
	    template[4] = template[0];

	    polygon = (XPoint *)malloc(numPoints * 5 * sizeof(XPoint));
	    for (pntPtr = polygon, i = 0; i < numPoints; i++, pointPtr++) {
		for (n = 0; n < 5; n++, pntPtr++) {
		    pntPtr->x = template[n].x + pointPtr->x;
		    pntPtr->y = template[n].y + pointPtr->y;
		}
	    }
	    if (penPtr->fillGC != NULL) {
		for (pntPtr = polygon, i = 0; i < numPoints; i++, pntPtr += 5) {
		    XFillPolygon(graphPtr->display, drawable, penPtr->fillGC,
			pntPtr, 5, Convex, CoordModeOrigin);

		}
	    }
	    if (penPtr->outlineWidth > 0) {
		for (pntPtr = polygon, i = 0; i < numPoints; i++, pntPtr += 5) {
		    XDrawLines(graphPtr->display, drawable, penPtr->outlineGC,
			pntPtr, 5, CoordModeOrigin);
		}
	    }
	    free((char *)polygon);
	}
	break;

    case SYMBOL_TRIANGLE:
	{
	    XPoint *polygon;
	    register XPoint *pntPtr;
	    double b;
	    int b2, h1, h2;
#define H_RATIO		1.1663402261671607
#define B_RATIO		1.3467736870885982
#define TAN30		0.57735026918962573
#define COS30		0.86602540378443871

	    b = Round(size * B_RATIO * 0.7);
	    b2 = Round(b * 0.5);
	    h2 = Round(TAN30 * b2);
	    h1 = Round(b2 / COS30);
	    /*
	     *
	     *                      The triangle symbol is a closed polygon
	     *           0,3         of 3 points. The diagram to the left
	     *                      represents the positions of the points
	     *           x,y        which are computed below. The extra
	     *                      (fourth) point connects the first and
	     *      2           1   last points.
	     *
	     */

	    template[3].x = template[0].x = 0;
	    template[3].y = template[0].y = -h1;
	    template[1].x = b2;
	    template[2].y = template[1].y = h2;
	    template[2].x = -b2;

	    polygon = (XPoint *)malloc(numPoints * 4 * sizeof(XPoint));
	    pntPtr = polygon;
	    for (i = 0; i < numPoints; i++, pointPtr++) {
		for (n = 0; n < 4; n++, pntPtr++) {
		    pntPtr->x = template[n].x + pointPtr->x;
		    pntPtr->y = template[n].y + pointPtr->y;
		}
	    }
	    if (penPtr->fillGC != NULL) {
		for (pntPtr = polygon, i = 0; i < numPoints; i++, pntPtr += 4) {
		    XFillPolygon(graphPtr->display, drawable, penPtr->fillGC,
			pntPtr, 4, Convex, CoordModeOrigin);
		}
	    }
	    if (penPtr->outlineWidth > 0) {
		for (pntPtr = polygon, i = 0; i < numPoints; i++, pntPtr += 4) {
		    XDrawLines(graphPtr->display, drawable, penPtr->outlineGC,
			pntPtr, 4, CoordModeOrigin);
		}
	    }
	    free((char *)polygon);
	}
	break;
    case SYMBOL_BITMAP:
	{
	    Pixmap bitmap, mask;
	    int width, height, bmWidth, bmHeight;
	    double scale, sx, sy;
	    int dx, dy;
	    register int x, y;

	    Tk_SizeOfBitmap(graphPtr->display, penPtr->symbol.bitmap,
		&width, &height);
	    mask = None;

	    /*
	     * Compute the size of the scaled bitmap.  Stretch the bitmap
	     * to fit a nxn bounding box.
	     */
	    sx = (double)size / (double)width;
	    sy = (double)size / (double)height;
	    scale = MIN(sx, sy);
	    bmWidth = (int)(width * scale);
	    bmHeight = (int)(height * scale);

	    if (penPtr->symbol.mask != None) {
		mask = Blt_ScaleBitmap(graphPtr->tkwin, penPtr->symbol.mask, 
		   width, height, bmWidth, bmHeight);
		XSetClipMask(graphPtr->display, penPtr->outlineGC, mask);
	    }
	    bitmap = Blt_ScaleBitmap(graphPtr->tkwin, penPtr->symbol.bitmap, 
		width, height, bmWidth, bmHeight);
	    if (penPtr->fillGC == NULL) {
		XSetClipMask(graphPtr->display, penPtr->outlineGC, bitmap);
	    }
	    dx = bmWidth / 2;
	    dy = bmHeight / 2;
	    for (i = 0; i < numPoints; i++, pointPtr++) {
		x = pointPtr->x - dx;
		y = pointPtr->y - dy;
		if ((penPtr->fillGC == NULL) || (mask != None)) {
		    XSetClipOrigin(graphPtr->display, penPtr->outlineGC, x, y);
		}
		XCopyPlane(graphPtr->display, bitmap, drawable,
		    penPtr->outlineGC, 0, 0, bmWidth, bmHeight, x, y, 1);
	    }
	    Tk_FreePixmap(graphPtr->display, bitmap);
	    if (mask != None) {
		Tk_FreePixmap(graphPtr->display, mask);
	    }
	}
	break;
    }
}

/*
 * -----------------------------------------------------------------
 *
 * DrawSymbol --
 *
 * 	Draw the symbol centered at the each given x,y coordinate.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Draws a symbol at the coordinate given.
 *
 * -----------------------------------------------------------------
 */
static void
DrawSymbol(graphPtr, drawable, elemPtr, x, y, size)
    Graph *graphPtr;		/* Graph widget record */
    Drawable drawable;		/* Pixmap or window to draw into */
    Element *elemPtr;		/* Line element information */
    int x, y;			/* Center position of symbol */
    int size;			/* Size of element */
{
    Line *linePtr = (Line *)elemPtr;
    LinePen *penPtr = linePtr->normalPenPtr;

    if (penPtr->lineWidth > 0) {
	/*
	 * Draw an extra line offset by one pixel from the previous to give
	 * a thicker appearance.  This is only for the legend entry.  This
	 * routine is never called for drawing the actual line segments.
	 */
	XDrawLine(graphPtr->display, drawable, penPtr->penGC, x - size, y,
	    x + size, y);
	XDrawLine(graphPtr->display, drawable, penPtr->penGC, x - size,
	    y + 1, x + size, y + 1);
    }
    if (penPtr->symbol.type != SYMBOL_NONE) {
	XPoint point;

	point.x = x, point.y = y;
	DrawSymbols(graphPtr, drawable, linePtr->normalPenPtr, size, 1, &point);
    }
}

static void
DrawTraces(graphPtr, drawable, linePtr, penPtr)
    Graph *graphPtr;
    Drawable drawable;		/* Pixmap or window to draw into */
    Line *linePtr;
    LinePen *penPtr;
{
    register Blt_ListItem item;
    register Trace *tracePtr;
    int maxPoints, numPoints;
    register int i;
    int start, extra;

    maxPoints = ((XMaxRequestSize(graphPtr->display) * 4) / sizeof(XPoint)) - 2;
    for (item = Blt_ListFirstItem(&(linePtr->traces)); item != NULL;
	item = Blt_ListNextItem(item)) {
	tracePtr = (Trace *)Blt_ListGetValue(item);

	/*
	 * If the trace has to be split into separate XDrawLines
	 * calls, then we need to make sure we make the last point
	 * to the starting point of the new split.
	 */
	start = extra = 0;
	for (i = 0; i < tracePtr->numPoints; i += maxPoints) {
	    numPoints = MIN((tracePtr->numPoints - i), maxPoints);
	    XDrawLines(graphPtr->display, drawable, penPtr->penGC,
		tracePtr->pointArr + start, numPoints + extra, CoordModeOrigin);
	    start = i + maxPoints - 1;
	    extra = 1;
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DrawActiveLine --
 *
 *	Draws the connected line(s) representing the element. If the
 *	line is made up of non-line symbols and the line width parameter
 *	has been set (linewidth > 0), the element will also be drawn as
 *	a line (with the linewidth requested).  The line may consist of
 *	separate line segments.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	X drawing commands are output.
 *
 *----------------------------------------------------------------------
 */
static void
DrawActiveLine(graphPtr, drawable, elemPtr)
    Graph *graphPtr;		/* Graph widget record */
    Drawable drawable;		/* Pixmap or window to draw into */
    Element *elemPtr;		/* Element to be drawn */
{
    Line *linePtr = (Line *)elemPtr;
    LinePen *penPtr = linePtr->activePenPtr;
    int symbolSize;

    if (penPtr == NULL) {
	return;
    }
    symbolSize = ScaleSymbol(linePtr, penPtr->symbol.size);
    if (linePtr->reqNumActive > 0) {
	if (linePtr->flags & ELEM_UPDATE_ACTIVE) {
	    ComputeActivePoints(graphPtr, linePtr);
	}
	if (penPtr->symbol.type != SYMBOL_NONE) {
	    DrawSymbols(graphPtr, drawable, penPtr, symbolSize,
		linePtr->numActive, linePtr->activeArr);
	}
    } else if (linePtr->reqNumActive < 0) {
	if (penPtr->lineWidth > 0) {
	    if (linePtr->numSegments > 0) {
		XDrawSegments(graphPtr->display, drawable, penPtr->penGC,
		    linePtr->segArr, linePtr->numSegments);
	    } else if (Blt_ListGetLength(&(linePtr->traces)) > 0) {
		DrawTraces(graphPtr, drawable, linePtr, penPtr);
	    }
	}
	if (penPtr->symbol.type != SYMBOL_NONE) {
	    DrawSymbols(graphPtr, drawable, penPtr, symbolSize,
		linePtr->numPoints, linePtr->pointArr);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DrawNormalLine --
 *
 *	Draws the connected line(s) representing the element. If the
 *	line is made up of non-line symbols and the line width parameter
 *	has been set (linewidth > 0), the element will also be drawn as
 *	a line (with the linewidth requested).  The line may consist of
 *	separate line segments.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	X drawing commands are output.
 *
 *----------------------------------------------------------------------
 */
static void
DrawNormalLine(graphPtr, drawable, elemPtr)
    Graph *graphPtr;		/* Graph widget record */
    Drawable drawable;		/* Pixmap or window to draw into */
    Element *elemPtr;		/* Element to be drawn */
{
    Line *linePtr = (Line *)elemPtr;
    LinePen *penPtr = linePtr->normalPenPtr;
    register LineStyle *stylePtr;
    register int i;

    /* Draw lines */
    if (graphPtr->type == TYPE_ELEM_STRIP) {
	for (stylePtr = linePtr->styles, i = 0; i < linePtr->numStyles;
	    i++, stylePtr++) {
	    if ((stylePtr->numSegments > 0) &&
		(stylePtr->penPtr->lineWidth > 0)) {
		XDrawSegments(graphPtr->display, drawable,
		    stylePtr->penPtr->penGC, stylePtr->segPtr,
		    stylePtr->numSegments);
	    }
	}
    } else if ((Blt_ListGetLength(&(linePtr->traces)) > 0) &&
	(penPtr->lineWidth > 0)) {
	DrawTraces(graphPtr, drawable, linePtr, penPtr);
    }
    /* Draw symbols */
    for (stylePtr = linePtr->styles, i = 0; i < linePtr->numStyles;
	i++, stylePtr++) {
	if ((stylePtr->numPoints > 0) &&
	    (stylePtr->penPtr->symbol.type != SYMBOL_NONE)) {
	    DrawSymbols(graphPtr, drawable, stylePtr->penPtr,
		stylePtr->symbolSize, stylePtr->numPoints, stylePtr->pointPtr);
	}
    }
}

/*
 * -----------------------------------------------------------------
 *
 * GetSymbolPrintInfo --
 *
 *	Set up the PostScript environment with the macros and
 *	attributes needed to draw the symbols of the element.
 *
 * Results:
 *	None.
 *
 * -----------------------------------------------------------------
 */
static void
GetSymbolPrintInfo(graphPtr, printable, penPtr, size)
    Graph *graphPtr;
    Printable printable;
    LinePen *penPtr;
    int size;
{
    XColor *outlineColor, *fillColor, *defaultColor;

    /* Set line and foreground attributes */
    outlineColor = penPtr->outlineColor;
    fillColor = penPtr->fillColor;
    defaultColor = penPtr->penColor;

    if (fillColor == COLOR_DEFAULT) {
	fillColor = defaultColor;
    }
    if (outlineColor == COLOR_DEFAULT) {
	outlineColor = defaultColor;
    }
    if (penPtr->symbol.type == SYMBOL_NONE) {
	Blt_LineAttributesToPostScript(printable, defaultColor,
	    penPtr->lineWidth + 2, &(penPtr->dashes), CapButt, JoinMiter);
    } else {
	Blt_LineWidthToPostScript(printable, penPtr->outlineWidth);
	Blt_LineDashesToPostScript(printable, (Dashes *)NULL);
    }

    /*
     * Build a PostScript procedure to draw the symbols.  For bitmaps,
     * paint both the bitmap and its mask. Otherwise fill and stroke
     * the path formed already.
     */
    Blt_PrintAppend(printable, "\n/DrawSymbolProc {\n", (char *)NULL);
    switch (penPtr->symbol.type) {
    case SYMBOL_NONE:
	break;			/* Do nothing */
    case SYMBOL_BITMAP:
	{
	    int width, height;
	    double sx, sy, scale;

	    /*
	     * Compute how much to scale the bitmap.  Don't let the scaled
	     * bitmap exceed the bounding square for the symbol.
	     */
	    Tk_SizeOfBitmap(graphPtr->display, penPtr->symbol.bitmap,
		&width, &height);
	    sx = (double)size / (double)width;
	    sy = (double)size / (double)height;
	    scale = MIN(sx, sy);

	    if ((penPtr->symbol.mask != None) && (fillColor != NULL)) {
		Blt_PrintAppend(printable,
		    "\n  % Bitmap mask is \"",
		    Tk_NameOfBitmap(graphPtr->display, penPtr->symbol.mask),
		    "\"\n\n  ", (char *)NULL);
		Blt_BackgroundToPostScript(printable, fillColor);
		Blt_PrintBitmap(printable, graphPtr->display,
		    penPtr->symbol.mask, scale, scale);
	    }
	    Blt_PrintAppend(printable,
		"\n  % Bitmap symbol is \"",
		Tk_NameOfBitmap(graphPtr->display, penPtr->symbol.bitmap),
		"\"\n\n  ", (char *)NULL);
	    Blt_ForegroundToPostScript(printable, outlineColor);
	    Blt_PrintBitmap(printable, graphPtr->display, penPtr->symbol.bitmap,
		scale, scale);
	}
	break;
    default:
	Blt_PrintAppend(printable, "  gsave\n", (char *)NULL);
	if (fillColor != NULL) {
	    Blt_PrintAppend(printable, "    ", (char *)NULL);
	    Blt_BackgroundToPostScript(printable, fillColor);
	    Blt_PrintAppend(printable, "    Fill\n", (char *)NULL);
	}
	if ((outlineColor != NULL) && (penPtr->outlineWidth > 0)) {
	    Blt_PrintAppend(printable, "    ", (char *)NULL);
	    Blt_ForegroundToPostScript(printable, outlineColor);
	    Blt_PrintAppend(printable, "    stroke\n", (char *)NULL);
	}
	Blt_PrintAppend(printable, "  grestore\n", (char *)NULL);
	break;
    }
    Blt_PrintAppend(printable, "} def\n\n", (char *)NULL);
}

/*
 * -----------------------------------------------------------------
 *
 * PrintSymbols --
 *
 * 	Draw a symbol centered at the given x,y window coordinate
 *	based upon the element symbol type and size.
 *
 * Results:
 *	None.
 *
 * Problems:
 *	Most notable is the round-off errors generated when
 *	calculating the centered position of the symbol.
 *
 * -----------------------------------------------------------------
 */
static void
PrintSymbols(graphPtr, printable, penPtr, size, numPoints, pointPtr)
    Graph *graphPtr;
    Printable printable;
    LinePen *penPtr;
    int size;
    int numPoints;
    register XPoint *pointPtr;
{
    float symbolSize;
    register int i;
    static char *symbolMacros[] =
    {
	"Li", "Sq", "Ci", "Di", "Pl", "Cr", "Sp", "Sc", "Tr", "Bm", (char *)NULL,
    };

    GetSymbolPrintInfo(graphPtr, printable, penPtr, size);

    symbolSize = (float)size;
    switch (penPtr->symbol.type) {
    case SYMBOL_SQUARE:
    case SYMBOL_CROSS:
    case SYMBOL_PLUS:
    case SYMBOL_SCROSS:
    case SYMBOL_SPLUS:
	symbolSize = (float)Round(size * S_RATIO);
	break;
    case SYMBOL_TRIANGLE:
	symbolSize = (float)Round(size * 0.7);
	break;
    case SYMBOL_DIAMOND:
	symbolSize = (float)Round(size * M_SQRT1_2);
	break;

    default:
	break;
    }
    for (i = 0; i < numPoints; i++, pointPtr++) {
	Blt_PrintFormat(printable, "%d %d %g %s\n", pointPtr->x,
	    pointPtr->y, symbolSize, symbolMacros[penPtr->symbol.type]);
    }
}

/*
 * -----------------------------------------------------------------
 *
 * PrintSymbol --
 *
 * 	Draw the symbol centered at the each given x,y coordinate.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Draws a symbol at the coordinate given.
 *
 * -----------------------------------------------------------------
 */
static void
PrintSymbol(graphPtr, printable, elemPtr, x, y, size)
    Graph *graphPtr;		/* Graph widget record */
    Printable printable;
    Element *elemPtr;		/* Line element information */
    int x, y;			/* Center position of symbol */
    int size;			/* Size of element */
{
    Line *linePtr = (Line *)elemPtr;
    LinePen *penPtr = linePtr->normalPenPtr;

    if (penPtr->lineWidth > 0) {
	/*
	 * Draw an extra line offset by one pixel from the previous to give
	 * a thicker appearance.  This is only for the legend entry.  This
	 * routine is never called for drawing the actual line segments.
	 */
	Blt_LineAttributesToPostScript(printable, penPtr->penColor,
	    penPtr->lineWidth + 2, &(penPtr->dashes), CapButt, JoinMiter);
	Blt_PrintFormat(printable, "%d %d %d Li\n", x, y, size + size);
    }
    if (penPtr->symbol.type != SYMBOL_NONE) {
	XPoint point;

	point.x = x, point.y = y;
	PrintSymbols(graphPtr, printable, penPtr, size, 1, &point);
    }
}


static void
SetLineAttributes(printable, penPtr)
    Printable printable;
    LinePen *penPtr;
{
    /* Set the attributes of the line (color, dashes, linewidth) */
    Blt_LineAttributesToPostScript(printable, penPtr->penColor,
	penPtr->lineWidth, &(penPtr->dashes), CapButt, JoinMiter);
    if ((penPtr->dashes.numValues > 0) && (penPtr->penOffColor != NULL)) {
	Blt_PrintAppend(printable, "/DashesProc {\n  gsave\n    ",
	    (char *)NULL);
	Blt_BackgroundToPostScript(printable, penPtr->penOffColor);
	Blt_PrintAppend(printable, "    ", (char *)NULL);
	Blt_LineDashesToPostScript(printable, (Dashes *)NULL);
	Blt_PrintAppend(printable, "stroke\n  grestore\n} def\n",
	    (char *)NULL);
    } else {
	Blt_PrintAppend(printable, "/DashesProc {} def\n", (char *)NULL);
    }
}

static void
PrintTraces(printable, linePtr, penPtr)
    Printable printable;
    Line *linePtr;
    LinePen *penPtr;
{
    register Blt_ListItem item;
    register Trace *tracePtr;

    SetLineAttributes(printable, penPtr);
    for (item = Blt_ListFirstItem(&(linePtr->traces)); item != NULL;
	item = Blt_ListNextItem(item)) {
	tracePtr = (Trace *)Blt_ListGetValue(item);
	Blt_PrintLine(printable, tracePtr->pointArr, tracePtr->numPoints);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * PrintActiveLine --
 *
 *	Generates PostScript commands to draw as "active" the points
 *	(symbols) and or line segments (trace) representing the
 *	element.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	PostScript pen width, dashes, and color settings are changed.
 *
 *----------------------------------------------------------------------
 */
static void
PrintActiveLine(graphPtr, printable, elemPtr)
    Graph *graphPtr;
    Printable printable;
    Element *elemPtr;
{
    Line *linePtr = (Line *)elemPtr;
    LinePen *penPtr = linePtr->activePenPtr;
    int symbolSize;

    if (penPtr == NULL) {
	return;
    }
    symbolSize = ScaleSymbol(linePtr, penPtr->symbol.size);
    if (linePtr->reqNumActive > 0) {
	if (linePtr->flags & ELEM_UPDATE_ACTIVE) {
	    ComputeActivePoints(graphPtr, linePtr);
	}
	if (penPtr->symbol.type != SYMBOL_NONE) {
	    PrintSymbols(graphPtr, printable, penPtr, symbolSize,
		linePtr->numActive, linePtr->activeArr);
	}
    } else if (linePtr->reqNumActive < 0) {
	if (penPtr->lineWidth > 0) {
	    if (linePtr->numSegments > 0) {
		SetLineAttributes(printable, penPtr);
		Blt_SegmentsToPostScript(printable, linePtr->segArr,
		    linePtr->numSegments);
	    }
	    if (Blt_ListGetLength(&(linePtr->traces)) > 0) {
		PrintTraces(printable, linePtr, (LinePen *)penPtr);
	    }
	}
	if (penPtr->symbol.type != SYMBOL_NONE) {
	    PrintSymbols(graphPtr, printable, penPtr, symbolSize,
		linePtr->numPoints, linePtr->pointArr);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * PrintLine --
 *
 *	Similar to the DrawLine procedure, prints PostScript related
 *	commands to form the connected line(s) representing the element.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	PostScript pen width, dashes, and color settings are changed.
 *
 *----------------------------------------------------------------------
 */
static void
PrintNormalLine(graphPtr, printable, elemPtr)
    Graph *graphPtr;
    Printable printable;
    Element *elemPtr;
{
    Line *linePtr = (Line *)elemPtr;
    LineStyle *stylePtr;
    register int i;

    /* Draw lines */
    if (graphPtr->type == TYPE_ELEM_STRIP) {
	for (stylePtr = linePtr->styles, i = 0; i < linePtr->numStyles;
	    i++, stylePtr++) {
	    if ((stylePtr->numSegments > 0) &&
		(stylePtr->penPtr->lineWidth > 0)) {
		SetLineAttributes(printable, stylePtr->penPtr);
		Blt_SegmentsToPostScript(printable, stylePtr->segPtr,
		    stylePtr->numSegments);
	    }
	}
    } else if ((Blt_ListGetLength(&(linePtr->traces)) > 0) &&
	(linePtr->normalPenPtr->lineWidth > 0)) {
	PrintTraces(printable, linePtr, linePtr->normalPenPtr);
    }
    /* Draw symbols */
    for (stylePtr = linePtr->styles, i = 0; i < linePtr->numStyles;
	i++, stylePtr++) {
	if ((stylePtr->numPoints > 0) &&
	    (stylePtr->penPtr->symbol.type != SYMBOL_NONE)) {
	    PrintSymbols(graphPtr, printable, stylePtr->penPtr,
		stylePtr->symbolSize, stylePtr->numPoints, stylePtr->pointPtr);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyLine --
 *
 *	Release memory and resources allocated for the line element.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Everything associated with the line element is freed up.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyLine(graphPtr, elemPtr)
    Graph *graphPtr;
    Element *elemPtr;
{
    Line *linePtr = (Line *)elemPtr;

    if (linePtr->normalPenPtr != &(linePtr->normalPen)) {
	Blt_FreePen(graphPtr, (Pen *)linePtr->normalPenPtr);
    }
    DestroyPen(graphPtr, (Pen *)&(linePtr->normalPen));
    if (linePtr->activePenPtr != NULL) {
	Blt_FreePen(graphPtr, (Pen *)linePtr->activePenPtr);
    }
    if (linePtr->x.clientId != NULL) {
	Blt_FreeVectorId(linePtr->x.clientId);
    } else if (linePtr->x.valueArr != NULL) {
	free((char *)linePtr->x.valueArr);
    }
    if (linePtr->y.clientId != NULL) {
	Blt_FreeVectorId(linePtr->y.clientId);
    } else if (linePtr->y.valueArr != NULL) {
	free((char *)linePtr->y.valueArr);
    }
    if (linePtr->w.clientId != NULL) {
	Blt_FreeVectorId(linePtr->w.clientId);
    } else if (linePtr->w.valueArr != NULL) {
	free((char *)linePtr->w.valueArr);
    }
    if (linePtr->reqActiveArr != NULL) {
	free((char *)linePtr->reqActiveArr);
    }
    if (linePtr->tags != NULL) {
	free((char *)linePtr->tags);
    }
    ResetLineInfo(linePtr);
    if (linePtr->styles != NULL) {
	free((char *)linePtr->styles);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_LineElement --
 *
 *	Allocate memory and initialize methods for the new line element.
 *
 * Results:
 *	The pointer to the newly allocated element structure is returned.
 *
 * Side effects:
 *	Memory is allocated for the line element structure.
 *
 *----------------------------------------------------------------------
 */

static ElemClassInfo lineClassInfo =
{
    configSpecs,
    ClosestLine,		/* Find closest element/data point */
    ConfigureLine,		/* Configure line element */
    DestroyLine,		/* Destroy the line element */
    DrawActiveLine,		/* Draw the line using its "active" attributes */
    DrawNormalLine,		/* Draw the line using its "normal" attributes */
    DrawSymbol,			/* Draw the line's symbol */
    ExtentsLine,		/* Find the extents of the element's data */
    PrintActiveLine,		/* Print the line using its "active" attributes */
    PrintNormalLine,		/* Print the line using its "normal" attributes */
    PrintSymbol,		/* Print the line's symbol */
    TransformLine,		/* Compute the screen coordinates for the line */
};

Element *
Blt_LineElement()
{
    register Line *linePtr;

    linePtr = (Line *)calloc(1, sizeof(Line));
    assert(linePtr);
    linePtr->infoPtr = &lineClassInfo;
    linePtr->penDir = PEN_BOTH_DIRECTIONS;
    linePtr->reqSmooth = PEN_LINEAR;
    linePtr->flags = LINE_INIT_SYMBOL_SCALE;
    linePtr->normalPenPtr = &(linePtr->normalPen);
    linePtr->labelRelief = TK_RELIEF_FLAT;
    InitPen(linePtr->normalPenPtr);
    Blt_InitList(&(linePtr->traces), TCL_ONE_WORD_KEYS);
    return (Element *) linePtr;
}
