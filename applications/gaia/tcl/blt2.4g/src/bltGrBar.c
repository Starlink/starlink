
/*
 * bltGrBar.c --
 *
 *	This module implements barchart elements for the BLT graph widget.
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

typedef struct BarPen {
    char *name;			/* Pen style identifier.  If NULL pen
				 * was statically allocated. */
    ObjectType type;		/* Type of pen */
    char *typeId;		/* String token identifying the type of pen */
    unsigned int flags;		/* Indicates if the pen element is active or
				 * normal */
    int refCount;		/* Reference count for elements using
				 * this pen. */
    Tcl_HashEntry *hashPtr;
    Tk_ConfigSpec *configSpecs;	/* Configuration specifications */

    PenConfigureProc *configProc;
    PenDestroyProc *destroyProc;

    XColor *fg;			/* Foreground color of bar */
    Tk_3DBorder border;		/* 3D border and background color */
    int borderWidth;		/* 3D border width of bar */
    int relief;			/* Relief of the bar */
    Pixmap stipple;		/* Stipple */
    GC gc;			/* Graphics context */

} BarPen;

typedef struct BarStyle {

    BarPen *penPtr;		/* Pen to draw */
    Limits weight;		/* Range of weights */

    XRectangle *rectPtr;	/* Points to start of bar array for this pen. */
    int numRects;		/* Number of bar segments for this pen. */

} BarStyle;

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

    int *reqActiveArr;		/* Array of indices (malloc-ed) which
				 * indicate which data points are active
				 * (drawn with "active" colors). */
    int reqNumActive;		/* Number of active data points. Special
				 * case: if reqNumActive < 0 and the
				 * active bit is set in "flags", then all
				 * data points are drawn active. */

    ElemClassInfo *infoPtr;	/* Class information pertaining to bar elements */

    /*
     * Bar specific attributes
     */

    /* Standard Pens */
    BarPen normalPen;
    BarPen *activePenPtr, *normalPenPtr;

    /* Alternate pens */
    BarStyle *styleArr;		/* Array of pen style information */
    int numStyles;		/* Number of pen styles */

    int *rectMap;
    XRectangle *rectArr;	/* Array of rectangles comprising the bar
				 * segments of the element. */
    int numRects;		/* Number of visible bar segments for element */

    int padX;			/* Spacing on either side of bar */
    double barWidth;

    int numActive;
    XRectangle *activeArr;

} Bar;

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

static int StringToBarMode _ANSI_ARGS_((ClientData, Tcl_Interp *, Tk_Window,
	char *, char *, int));
static char *BarModeToString _ANSI_ARGS_((ClientData, Tk_Window, char *, int,
	Tcl_FreeProc **));

Tk_CustomOption bltBarModeOption =
{
    StringToBarMode, BarModeToString, (ClientData)0
};

extern Tk_CustomOption bltBarPenOption;
extern Tk_CustomOption bltDataOption;
extern Tk_CustomOption bltDataPairsOption;
extern Tk_CustomOption bltLengthOption;
extern Tk_CustomOption bltListOption;
extern Tk_CustomOption bltXAxisOption;
extern Tk_CustomOption bltYAxisOption;

#define DEF_PEN_ACTIVE_BG_COLOR		"red"
#define DEF_PEN_ACTIVE_BG_MONO		WHITE
#define DEF_PEN_ACTIVE_FG_COLOR     	"pink"
#define DEF_PEN_ACTIVE_FG_MONO		BLACK
#define DEF_PEN_BORDERWIDTH		"2"
#define DEF_PEN_NORMAL_BG_COLOR		"navyblue"
#define DEF_PEN_NORMAL_BG_MONO		BLACK
#define DEF_PEN_NORMAL_FG_COLOR		"blue"
#define DEF_PEN_NORMAL_FG_MONO		WHITE
#define DEF_PEN_RELIEF			"raised"
#define DEF_PEN_STIPPLE			""
#define DEF_PEN_TYPE			"bar"

static Tk_ConfigSpec penConfigSpecs[] =
{
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_PEN_ACTIVE_BG_COLOR, Tk_Offset(BarPen, border),
	TK_CONFIG_COLOR_ONLY | ACTIVE_PEN},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_PEN_ACTIVE_BG_COLOR, Tk_Offset(BarPen, border),
	TK_CONFIG_MONO_ONLY | ACTIVE_PEN},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_PEN_NORMAL_BG_COLOR, Tk_Offset(BarPen, border),
	TK_CONFIG_COLOR_ONLY | NORMAL_PEN},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_PEN_NORMAL_BG_COLOR, Tk_Offset(BarPen, border),
	TK_CONFIG_MONO_ONLY | NORMAL_PEN},
    {TK_CONFIG_SYNONYM, "-bd", "borderWidth", (char *)NULL,
	(char *)NULL, 0, ALL_PENS},
    {TK_CONFIG_SYNONYM, "-bg", "background", (char *)NULL,
	(char *)NULL, 0, ALL_PENS},
    {TK_CONFIG_CUSTOM, "-borderwidth", "borderWidth", "BorderWidth",
	DEF_PEN_BORDERWIDTH, Tk_Offset(BarPen, borderWidth), ALL_PENS,
	&bltLengthOption},
    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *)NULL,
	(char *)NULL, 0, ALL_PENS},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_PEN_ACTIVE_FG_COLOR, Tk_Offset(BarPen, fg),
	TK_CONFIG_COLOR_ONLY | ACTIVE_PEN},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_PEN_ACTIVE_FG_COLOR, Tk_Offset(BarPen, fg),
	TK_CONFIG_MONO_ONLY | ACTIVE_PEN},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_PEN_NORMAL_FG_COLOR, Tk_Offset(BarPen, fg),
	TK_CONFIG_COLOR_ONLY | NORMAL_PEN},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_PEN_NORMAL_FG_COLOR, Tk_Offset(BarPen, fg),
	TK_CONFIG_MONO_ONLY | NORMAL_PEN},
    {TK_CONFIG_RELIEF, "-relief", "relief", "Relief",
	DEF_PEN_RELIEF, Tk_Offset(BarPen, relief), ALL_PENS},
    {TK_CONFIG_BITMAP, "-stipple", "stipple", "Stipple",
	DEF_PEN_STIPPLE, Tk_Offset(BarPen, stipple),
	TK_CONFIG_NULL_OK | ALL_PENS},
    {TK_CONFIG_STRING, "-type", (char *)NULL, (char *)NULL,
	DEF_PEN_TYPE, Tk_Offset(BarPen, typeId), ALL_PENS | TK_CONFIG_NULL_OK},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

#define DEF_BAR_ACTIVE_PEN		"activeBar"
#define DEF_BAR_AXIS_X			"x"
#define DEF_BAR_AXIS_Y			"y"
#define DEF_BAR_BG_COLOR		"navyblue"
#define DEF_BAR_BG_MONO			BLACK
#define DEF_BAR_BORDERWIDTH		"2"
#define DEF_BAR_DATA			(char *)NULL
#define DEF_BAR_FG_COLOR		"blue"
#define DEF_BAR_FG_MONO			WHITE
#define DEF_BAR_HIDE			"no"
#define DEF_BAR_LABEL			(char *)NULL
#define DEF_BAR_LABEL_RELIEF		"flat"
#define DEF_BAR_NORMAL_STIPPLE		""
#define DEF_BAR_RELIEF			"raised"
#define DEF_BAR_STYLES			""
#define DEF_BAR_TAGS			"Element"
#define DEF_BAR_WIDTH			"0.0"
#define DEF_BAR_X_DATA			(char *)NULL
#define DEF_BAR_Y_DATA			(char *)NULL

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_CUSTOM, "-activepen", "activePen", "ActivePen",
	DEF_BAR_ACTIVE_PEN, Tk_Offset(Bar, activePenPtr),
	TK_CONFIG_NULL_OK | BARCHART, &bltBarPenOption},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_BAR_BG_COLOR, Tk_Offset(Bar, normalPen.border),
	TK_CONFIG_COLOR_ONLY | BARCHART},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_BAR_BG_COLOR, Tk_Offset(Bar, normalPen.border),
	TK_CONFIG_MONO_ONLY | BARCHART},
    {TK_CONFIG_DOUBLE, "-barwidth", "barWidth", "BarWidth",
	DEF_BAR_WIDTH, Tk_Offset(Bar, barWidth),
	TK_CONFIG_DONT_SET_DEFAULT | BARCHART},
    {TK_CONFIG_SYNONYM, "-bd", "borderWidth", (char *)NULL,
	(char *)NULL, 0, BARCHART},
    {TK_CONFIG_SYNONYM, "-bg", "background", (char *)NULL,
	(char *)NULL, 0, BARCHART},
    {TK_CONFIG_CUSTOM, "-bindtags", "bindTags", "BindTags",
	DEF_BAR_TAGS, Tk_Offset(Bar, tags), TK_CONFIG_NULL_OK, &bltListOption},
    {TK_CONFIG_CUSTOM, "-borderwidth", "borderWidth", "BorderWidth",
	DEF_BAR_BORDERWIDTH, Tk_Offset(Bar, normalPen.borderWidth),
	BARCHART, &bltLengthOption},
    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *)NULL,
	(char *)NULL, 0, BARCHART},
    {TK_CONFIG_CUSTOM, "-data", "data", "Data",
	(char *)NULL, 0, BARCHART, &bltDataPairsOption},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_BAR_FG_COLOR, Tk_Offset(Bar, normalPen.fg),
	TK_CONFIG_COLOR_ONLY | BARCHART},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_BAR_FG_COLOR, Tk_Offset(Bar, normalPen.fg),
	TK_CONFIG_MONO_ONLY | BARCHART},
    {TK_CONFIG_STRING, "-label", "label", "Label",
	DEF_BAR_LABEL, Tk_Offset(Bar, label), TK_CONFIG_NULL_OK | BARCHART},
    {TK_CONFIG_RELIEF, "-labelrelief", "labelRelief", "LabelRelief",
	DEF_BAR_LABEL_RELIEF, Tk_Offset(Bar, labelRelief),
	TK_CONFIG_DONT_SET_DEFAULT | BARCHART},
    {TK_CONFIG_BOOLEAN, "-hide", "hide", "Hide",
	DEF_BAR_HIDE, Tk_Offset(Bar, hidden),
	TK_CONFIG_DONT_SET_DEFAULT | BARCHART},
    {TK_CONFIG_CUSTOM, "-mapx", "mapX", "MapX",
	DEF_BAR_AXIS_X, Tk_Offset(Bar, axes.x), BARCHART, &bltXAxisOption},
    {TK_CONFIG_CUSTOM, "-mapy", "mapY", "MapY",
	DEF_BAR_AXIS_Y, Tk_Offset(Bar, axes.y), BARCHART, &bltYAxisOption},
    {TK_CONFIG_CUSTOM, "-pen", "pen", "Pen",
	(char *)NULL, Tk_Offset(Bar, normalPenPtr),
	TK_CONFIG_NULL_OK | BARCHART, &bltBarPenOption},
    {TK_CONFIG_RELIEF, "-relief", "relief", "Relief",
	DEF_BAR_RELIEF, Tk_Offset(Bar, normalPen.relief), BARCHART},
    {TK_CONFIG_BITMAP, "-stipple", "stipple", "Stipple",
	DEF_BAR_NORMAL_STIPPLE, Tk_Offset(Bar, normalPen.stipple),
	TK_CONFIG_NULL_OK | BARCHART},
    {TK_CONFIG_CUSTOM, "-styles", "styles", "Styles",
	DEF_BAR_STYLES, 0, BARCHART | TK_CONFIG_NULL_OK, &stylesOption},
    {TK_CONFIG_CUSTOM, "-weights", "weights", "Weights",
	(char *)NULL, Tk_Offset(Bar, w), BARCHART, &bltDataOption},
    {TK_CONFIG_CUSTOM, "-xdata", "xdata", "Xdata",
	DEF_BAR_X_DATA, Tk_Offset(Bar, x), BARCHART, &bltDataOption},
    {TK_CONFIG_CUSTOM, "-ydata", "ydata", "Ydata",
	DEF_BAR_Y_DATA, Tk_Offset(Bar, y), BARCHART, &bltDataOption},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

/* Forward declarations */
#ifdef __STDC__
static PenConfigureProc ConfigurePen;
static PenDestroyProc DestroyPen;
static ElemClosestProc ClosestBar;
static ElemConfigProc ConfigureBar;
static ElemDestroyProc DestroyBar;
static ElemDrawProc DrawActiveBar;
static ElemDrawProc DrawNormalBar;
static ElemDrawSymbolProc DrawSymbol;
static ElemExtentsProc ExtentsBar;
static ElemPrintProc PrintActiveBar;
static ElemPrintProc PrintNormalBar;
static ElemPrintSymbolProc PrintSymbol;
static ElemTransformProc TransformBar;
#endif /* __STDC__ */

INLINE static double
Fabs(x)
    register double x;
{
    return ((x < 0.0) ? -x : x);
}

INLINE static int
Round(x)
    register double x;
{
    return (int) (x + ((x < 0.0) ? -0.5 : 0.5));
}

/*
 * ----------------------------------------------------------------------
 *
 * OutOfRange --
 *
 *	Determines if a value does not lie within a given range.
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
    return (((norm - 1.0) > DBL_EPSILON) ||
	(((1.0 - norm) - 1.0) > DBL_EPSILON));
}

/*
 * ----------------------------------------------------------------------
 * Custom option parse and print procedures
 * ----------------------------------------------------------------------
 */

/*
 * ----------------------------------------------------------------------
 *
 * NameOfBarMode --
 *
 *	Converts the integer representing the mode style into a string.
 *
 * ----------------------------------------------------------------------
 */
static char *
NameOfBarMode(mode)
    BarMode mode;
{
    switch (mode) {
    case MODE_NORMAL:
	return "normal";
    case MODE_OVERLAP:
	return "overlap";
    case MODE_STACKED:
	return "stacked";
    case MODE_ALIGNED:
	return "aligned";
    default:
	return "unknown mode value";
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * StringToMode --
 *
 *	Converts the mode string into its numeric representation.
 *
 *	Valid mode strings are:
 *
 *      "normal"    Draw a full bar at each point in the element.
 *
 * 	"stacked"   Stack bar segments vertically. Each stack is defined
 *		    by each ordinate at a particular abscissa. The height
 *		    of each segment is represented by the sum the previous
 *		    ordinates.
 *
 *	"aligned"   Align bar segments as smaller slices one next to
 *		    the other.  Like "stacks", aligned segments are
 *		    defined by each ordinate at a particular abscissa.
 *
 * Results:
 *	A standard Tcl result.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToBarMode(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* Mode style string */
    char *widgRec;		/* Cubicle structure record */
    int offset;			/* Offset of style in record */
{
    BarMode *modePtr = (BarMode *)(widgRec + offset);
    unsigned int length;
    char c;

    c = string[0];
    length = strlen(string);
    if ((c == 'n') && (strncmp(string, "normal", length) == 0)) {
	*modePtr = MODE_NORMAL;
    } else if ((c == 's') && (strncmp(string, "stacked", length) == 0)) {
	*modePtr = MODE_STACKED;
    } else if ((c == 'a') && (strncmp(string, "aligned", length) == 0)) {
	*modePtr = MODE_ALIGNED;
    } else if ((c == 'o') && (strncmp(string, "overlap", length) == 0)) {
	*modePtr = MODE_OVERLAP;
    } else {
	Tcl_AppendResult(interp, "bad mode argument \"", string,
	    "\": should be \"normal\", \"stacked\", \"overlap\", or \"aligned\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * BarModeToString --
 *
 *	Returns the mode style string based upon the mode flags.
 *
 * Results:
 *	The mode style string is returned.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
BarModeToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Row/column structure record */
    int offset;			/* Offset of mode in Partition record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    BarMode mode = *(BarMode *)(widgRec + offset);

    return (NameOfBarMode(mode));
}


static void
FreeBarStyles(barPtr, styleArr, numStyles)
    Bar *barPtr;
    BarStyle styleArr[];
    int numStyles;
{
    register int i;

    /*
     * Always ignore the first array slot. It's occupied by the built-in
     * "normal" pen of the element.
     */
    for (i = 1; i < numStyles; i++) {
	Blt_FreePen(barPtr->graphPtr, (Pen *)styleArr[i].penPtr);
    }
    free((char *)styleArr);
}

/*
 * Clear the number of points and segments, in case there are no segments or
 * points
 */
static void
ClearStyles(barPtr)
    Bar *barPtr;
{
    register BarStyle *stylePtr;
    register int i;

    for (stylePtr = barPtr->styleArr, i = 0; i < barPtr->numStyles;
	i++, stylePtr++) {
	stylePtr->numRects = 0;
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
    Bar *barPtr = (Bar *)(widgRec);
    register int i;
    int numStyles;
    char **elemArr;
    BarStyle *styleArr, *stylePtr;

    elemArr = NULL;
    if ((string == NULL) || (*string == '\0')) {
	numStyles = 0;
    } else {
	if (Tcl_SplitList(interp, string, &numStyles, &elemArr) != TCL_OK) {
	    numStyles = 0;
	}
    }
    /* Convert the styles into pen pointers and store in an array  */
    stylePtr = styleArr = (BarStyle *)calloc(numStyles + 1, sizeof(BarStyle));
    assert(styleArr);

    /* First pen is always the "normal" pen, we'll set the style later */
    stylePtr++;

    for (i = 0; i < numStyles; i++, stylePtr++) {
	SetLimits(stylePtr->weight, (double)i, (double)(i + 1));
	if (Blt_GetPenStyle(barPtr->graphPtr, elemArr[i], TYPE_ELEM_BAR,
		(PenStyle *)stylePtr) != TCL_OK) {
	    free((char *)elemArr);
	    FreeBarStyles(barPtr, styleArr, i);
	    return TCL_ERROR;
	}
    }
    if (elemArr != NULL) {
	free((char *)elemArr);
    }
    if (barPtr->styleArr != NULL) {
	FreeBarStyles(barPtr, barPtr->styleArr, barPtr->numStyles);
    }
    barPtr->numStyles = numStyles + 1;
    barPtr->styleArr = styleArr;
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
    Bar *barPtr = (Bar *)(widgRec);
    char string[TCL_DOUBLE_SPACE];
    Tcl_DString dStr;
    Tcl_Interp *interp = barPtr->graphPtr->interp;
    register BarStyle *stylePtr;
    register int i;
    char *result;

    if (barPtr->numStyles < 2) {
	return "";
    }
    Tcl_DStringInit(&dStr);
    for (i = 1; i < barPtr->numStyles; i++) {
	stylePtr = barPtr->styleArr + i;
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
    return (result);
}



static int
ConfigurePen(graphPtr, penPtr)
    Graph *graphPtr;
    Pen *penPtr;
{
    BarPen *bpPtr = (BarPen *)penPtr;
    XColor *colorPtr;
    XGCValues gcValues;
    unsigned long gcMask;
    GC newGC;

    gcMask = GCForeground | GCBackground;
    colorPtr = bpPtr->fg;
    gcValues.foreground = colorPtr->pixel;
    gcValues.background = (Tk_3DBorderColor(bpPtr->border))->pixel;
    if (bpPtr->stipple != None) {
	gcValues.stipple = bpPtr->stipple;
	gcValues.fill_style = FillOpaqueStippled;
	gcMask |= (GCStipple | GCFillStyle);
    }
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (bpPtr->gc != NULL) {
	Tk_FreeGC(graphPtr->display, bpPtr->gc);
    }
    bpPtr->gc = newGC;
    return TCL_OK;
}

static void
DestroyPen(graphPtr, penPtr)
    Graph *graphPtr;
    Pen *penPtr;
{
    BarPen *bpPtr = (BarPen *)penPtr;

    if (bpPtr->gc != NULL) {
	Tk_FreeGC(graphPtr->display, bpPtr->gc);
    }
}

static void
InitPen(penPtr)
    BarPen *penPtr;
{
    penPtr->configSpecs = penConfigSpecs;
    penPtr->configProc = ConfigurePen;
    penPtr->destroyProc = DestroyPen;
    penPtr->relief = TK_RELIEF_RAISED;
    penPtr->flags = NORMAL_PEN;
    penPtr->borderWidth = 2;
}

Pen *
Blt_BarPen(penName)
    char *penName;
{
    BarPen *penPtr;

    penPtr = (BarPen *)calloc(1, sizeof(BarPen));
    assert(penPtr);
    InitPen(penPtr);
    penPtr->name = strdup(penName);
    if (strcmp(penName, "activeBar") == 0) {
	penPtr->flags = ACTIVE_PEN;
    }
    return (Pen *) penPtr;
}

/*
 * ----------------------------------------------------------------------
 *
 * CheckStacks --
 *
 *	Check that the data limits are not superseded by the heights
 *	of stacked bar segments.  The heights are calculated by
 *	Blt_ComputeStacks.
 *
 * Results:
 *	If the y-axis limits need to be adjusted for stacked segments,
 *	*minPtr* or *maxPtr* are updated.
 *
 * Side effects:
 *	Autoscaling of the y-axis is affected.
 *
 * ----------------------------------------------------------------------
 */
static void
CheckStacks(graphPtr, pairPtr, minPtr, maxPtr)
    Graph *graphPtr;
    Axis2D *pairPtr;
    double *minPtr, *maxPtr;	/* Current minimum maximum for y-axis */
{
    FreqInfo *infoPtr;
    register int i;

    if ((graphPtr->mode != MODE_STACKED) || (graphPtr->numStacks == 0)) {
	return;
    }
    infoPtr = graphPtr->freqArr;
    for (i = 0; i < graphPtr->numStacks; i++) {
	if ((infoPtr->axes.x == pairPtr->x) && (infoPtr->axes.y == pairPtr->y)) {
	    /*

	     * Check if any of the y-values (because of stacking) are
	     * greater than the current limits of the graph.
	     */
	    if (infoPtr->sum < 0.0) {
		if (*minPtr > infoPtr->sum) {
		    *minPtr = infoPtr->sum;
		}
	    } else {
		if (*maxPtr < infoPtr->sum) {
		    *maxPtr = infoPtr->sum;
		}
	    }
	}
	infoPtr++;
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * ConfigureBar --
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
 *	Configuration information such as bar foreground/background
 *	color and stipple etc. get set in a new GC.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ConfigureBar(graphPtr, elemPtr)
    Graph *graphPtr;
    register Element *elemPtr;
{
    Bar *barPtr = (Bar *)elemPtr;

    if (ConfigurePen(graphPtr, (Pen *)&(barPtr->normalPen)) != TCL_OK) {
	return TCL_ERROR;
    }
    /*
     * Point to the static normal pen if no external pens have
     * been selected.
     */
    if (barPtr->normalPenPtr == NULL) {
	barPtr->normalPenPtr = &(barPtr->normalPen);
    }
    if (barPtr->styleArr != NULL) {
	barPtr->styleArr[0].penPtr = barPtr->normalPenPtr;
    }
    return TCL_OK;
}

static void
ExtentsBar(elemPtr, extsPtr)
    Element *elemPtr;
    Extents2D *extsPtr;
{
    Graph *graphPtr = elemPtr->graphPtr;
    Bar *barPtr = (Bar *)elemPtr;
    double middle, barWidth;

    extsPtr->yMin = extsPtr->xMin = bltPosInfinity;
    extsPtr->yMax = extsPtr->xMax = bltNegInfinity;

    if (NumberOfPoints(barPtr) < 1) {
	return;			/* No data points */
    }
    barWidth = graphPtr->barWidth;
    if (barPtr->barWidth > 0.0) {
	barWidth = barPtr->barWidth;
    }
    middle = barWidth * 0.5;
    extsPtr->xMin = barPtr->x.min - middle;
    extsPtr->xMax = barPtr->x.max + middle;

    extsPtr->yMin = barPtr->y.min;
    extsPtr->yMax = barPtr->y.max;
    if (extsPtr->yMax < graphPtr->baseline) {
	extsPtr->yMax = graphPtr->baseline;
    }
    /*
     * Handle "stacked" bar elements specially.
     *
     * If element is stacked, the sum of its ordinates may be outside
     * the minimum/maximum limits of the element's data points.
     */
    if ((graphPtr->mode == MODE_STACKED) && (graphPtr->numStacks > 0)) {
	CheckStacks(graphPtr, &(elemPtr->axes), &(extsPtr->yMin), &(extsPtr->yMax));
    }
    /* Warning: You get what you deserve if the x-axis is logScale */
    if (elemPtr->axes.x->logScale) {
	extsPtr->xMin = Blt_FindElemVectorMinimum(&(barPtr->x), DBL_MIN) + middle;
    }
    /* Fix y-min limits for barchart */
    if (elemPtr->axes.y->logScale) {
	if ((extsPtr->yMin <= 0.0) || (extsPtr->yMin > 1.0)) {
	    extsPtr->yMin = 1.0;
	}
    } else {
	if (extsPtr->yMin > 0.0) {
	    extsPtr->yMin = 0.0;
	}
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * ClosestBar --
 *
 *	Find the bar segment closest to the window coordinates	point
 *	specified.
 *
 *	Note:  This does not return the height of the stacked segment
 *	       (in graph coordinates) properly.
 *
 * Results:
 *	Returns 1 if the point is width any bar segment, otherwise 0.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static void
ClosestBar(graphPtr, elemPtr, searchPtr)
    Graph *graphPtr;		/* Graph widget record */
    Element *elemPtr;		/* Bar element */
    ClosestSearch *searchPtr;	/* Info of closest point in element */
{
    Bar *barPtr = (Bar *)elemPtr;
    double minDist, dist;
    int minIndex;
    Point2D point;
    register int i;
    XPoint pointArr[5];
    register XRectangle *rectPtr;
    double xMin, xMax, yMin, yMax;
    int side;

    minDist = searchPtr->dist;
    minIndex = 0;
    for (rectPtr = barPtr->rectArr, i = 0; i < barPtr->numRects;
	i++, rectPtr++) {
	if (PointInRectangle(rectPtr, searchPtr->x, searchPtr->y)) {
	    minIndex = barPtr->rectMap[i];
	    minDist = 0.0;
	    break;
	}
	xMin = rectPtr->x, yMin = rectPtr->y;
	xMax = (int)rectPtr->x + (int)rectPtr->width;
	yMax = (int)rectPtr->y + (int)rectPtr->height;
	pointArr[4].x = pointArr[3].x = pointArr[0].x = (short)xMin;
	pointArr[4].y = pointArr[1].y = pointArr[0].y = (short)yMin;
	pointArr[2].x = pointArr[1].x = (short)xMax;
	pointArr[3].y = pointArr[2].y = (short)yMax;

	for (side = 0; side < 4; side++) {
	    point = Blt_GetProjection(searchPtr->x, searchPtr->y,
		pointArr[side].x, pointArr[side].y,
		pointArr[side + 1].x, pointArr[side + 1].y);
	    if (point.x > xMax) {
		point.x = xMax;
	    } else if (point.x < xMin) {
		point.x = xMin;
	    }
	    if (point.y > yMax) {
		point.y = yMax;
	    } else if (point.y < yMin) {
		point.y = yMin;
	    }
	    dist = hypot((point.x - searchPtr->x), (point.y - searchPtr->y));
	    if (dist < minDist) {
		minDist = dist;
		minIndex = barPtr->rectMap[i];
	    }
	}
    }
    if (minDist < searchPtr->dist) {
	searchPtr->elemPtr = (Element *)elemPtr;
	searchPtr->dist = minDist;
	searchPtr->index = minIndex;
	searchPtr->point.x = (double)barPtr->x.valueArr[minIndex];
	searchPtr->point.y = (double)barPtr->y.valueArr[minIndex];
    }
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
GetWeights(barPtr)
    Bar *barPtr;
{
    register int i;
    register int styleNum;
    int numWeights;		/* Number of weights to be examined.  If there
				 * are more data points, they will default to
				 * the normal pen. */
    int *newArr;		/* New index array */
    BarStyle *stylePtr;
    double *w;			/* Weight vector */
    int numPoints;

    /*
     * Create an array of style indices, initializing each index to 0.  The 0th
     * index represents the default bar style (i.e. "normal" pen).
     */
    /* FIXME: Use number of actual rectangles instead of data points
     *	which can be smaller. Just have to check for bogus weight indices */
    numPoints = NumberOfPoints(barPtr);
    newArr = (int *)calloc(numPoints, sizeof(int));
    assert(newArr);
    numWeights = MIN(barPtr->w.numValues, numPoints);
    w = barPtr->w.valueArr;

    stylePtr = barPtr->styleArr;
    stylePtr++;

    for (styleNum = 1; styleNum < barPtr->numStyles; styleNum++, stylePtr++) {
	for (i = 0; i < numWeights; i++) {
	    if ((newArr[i] > 0) || (OutOfRange(w[i], &(stylePtr->weight)))) {
		continue;	/* Don't overwrite styles already set */
	    }
	    newArr[i] = styleNum;
	}
    }
    return newArr;
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
MergePens(barPtr)
    Bar *barPtr;
{
    BarStyle *stylePtr;

    stylePtr = barPtr->styleArr;
    if (barPtr->numStyles < 2) {
	stylePtr->numRects = barPtr->numRects;
	stylePtr->rectPtr = barPtr->rectArr;
	return;
    }
    /*
     * We have more than one style, so we need to group bar segments
     * of like pen styles together.
     */
    if (barPtr->numRects > 0) {
	register int count;
	register int i, styleNum;
	register XRectangle *rectPtr;
	XRectangle *rectArr;
	register int *indexPtr;
	int *indexArr, *styleDir;

	styleDir = GetWeights(barPtr);
	rectArr = (XRectangle *)malloc(barPtr->numRects * sizeof(XRectangle));
	indexArr = (int *)malloc(barPtr->numRects * sizeof(int));
	assert(rectArr && indexArr);

	rectPtr = rectArr, indexPtr = indexArr;
	for (stylePtr = barPtr->styleArr, styleNum = 0;
	    styleNum < barPtr->numStyles; styleNum++, stylePtr++) {
	    count = 0;
	    stylePtr->rectPtr = rectPtr;
	    for (i = 0; i < barPtr->numRects; i++) {
		if (styleDir[barPtr->rectMap[i]] == styleNum) {
		    *rectPtr++ = barPtr->rectArr[i];
		    *indexPtr++ = barPtr->rectMap[i];
		    count++;
		}
	    }
	    stylePtr->numRects = count;
	}
	free((char *)barPtr->rectArr);
	barPtr->rectArr = rectArr;
	free((char *)barPtr->rectMap);
	barPtr->rectMap = indexArr;
	free((char *)styleDir);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ComputeActiveBars --
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
ComputeActiveBars(barPtr)
    Bar *barPtr;
{
    XRectangle *activeArr;
    register XRectangle *activePtr;
    register int i, n;
    register int count;

    if (barPtr->activeArr != NULL) {
	free((char *)barPtr->activeArr);
    }
    barPtr->activeArr = NULL;
    barPtr->numActive = 0;

    if (barPtr->reqNumActive <= 0) {
	return;			/* No active rectangles needed */
    }
    activePtr = activeArr =
	(XRectangle *)malloc(sizeof(XRectangle) * barPtr->reqNumActive);
    assert(activeArr);

    count = 0;
    for (i = 0; i < barPtr->numRects; i++) {
	for (n = 0; n < barPtr->reqNumActive; n++) {
	    if (barPtr->rectMap[i] == barPtr->reqActiveArr[n]) {
		*activePtr++ = barPtr->rectArr[i];
		count++;
	    }
	}
    }
    barPtr->numActive = count;
    barPtr->activeArr = activeArr;
    barPtr->flags &= ~ELEM_UPDATE_ACTIVE;
}

/*
 * ----------------------------------------------------------------------
 *
 * TransformBar --
 *
 *	Calculates the actual window coordinates of the bar element.
 *	The window coordinates are saved in the bar element structure.
 *
 * Results:
 *	None.
 *
 * Notes:
 *	A bar can have multiple segments (more than one x,y pairs).
 *	In this case, the bar can be represented as either a set of
 *	non-contiguous bars or a single multi-segmented (stacked) bar.
 *
 *	The x-axis layout for a barchart may be presented in one of
 *	two ways.  If abscissas are used, the bars are placed at those
 *	coordinates.  Otherwise, the range will represent the number
 *	of values.
 *
 * ----------------------------------------------------------------------
 */

static void
TransformBar(graphPtr, elemPtr)
    Graph *graphPtr;
    Element *elemPtr;
{
    Bar *barPtr = (Bar *)elemPtr;
    double baseline;
    Point2D c1, c2;		/* Two opposite corners of the rectangle
				 * in graph coordinates. */
    double barWidth, barOffset;
    FreqKey key;
    int numPoints, count;
    register XRectangle *rectPtr, *rectArr;
    int *rectMap;		/* Maps rectangles to data point indices */
    register int i;
    int invertBar;
    int height;
    double *x, *y;

    /* Release any storage associated with the display of the bar */
    if (barPtr->rectArr != NULL) {
	free((char *)barPtr->rectArr);
    }
    barPtr->rectArr = NULL;
    barPtr->numRects = 0;
    if (barPtr->rectMap != NULL) {
	free((char *)barPtr->rectMap);
    }
    barPtr->rectMap = NULL;
    ClearStyles(barPtr);

    numPoints = NumberOfPoints(barPtr);
    if (numPoints < 1) {
	return;			/* No data points */
    }
    barWidth = graphPtr->barWidth;
    if (barPtr->barWidth > 0.0) {
	barWidth = barPtr->barWidth;
    }
    baseline = (barPtr->axes.y->logScale) ? 1.0 : graphPtr->baseline;
    barOffset = barWidth * 0.5;

    /*
     * Create an array of rectangles representing the screen coordinates
     * of all the segments in the bar.
     */
    rectPtr = rectArr = (XRectangle *)malloc(numPoints * sizeof(XRectangle));
    assert(rectArr);
    rectMap = (int *)calloc(numPoints, sizeof(int));
    assert(rectMap);

    x = barPtr->x.valueArr, y = barPtr->y.valueArr;
    count = 0;
    for (i = 0; i < numPoints; i++) {
	if (((x[i] - barWidth) > barPtr->axes.x->limitsPtr->max) ||
	    ((x[i] + barWidth) < barPtr->axes.x->limitsPtr->min)) {
	    continue;		/* Abscissa is out of range of the x-axis */
	}
	c1.x = x[i] - barOffset;
	c1.y = y[i];
	c2.x = c1.x + barWidth;
	c2.y = baseline;

	/*
	 * If the mode is "aligned" or "stacked" we need to adjust the
	 * x or y coordinates of the two corners.
	 */

	if ((graphPtr->numStacks > 0) && (graphPtr->mode != MODE_NORMAL)) {
	    Tcl_HashEntry *hPtr;

	    key.value = x[i];
	    key.axes = barPtr->axes;
	    hPtr = Tcl_FindHashEntry(&(graphPtr->freqTable), (char *)&key);
	    if (hPtr != NULL) {
		FreqInfo *infoPtr;
		double slice, width;

		infoPtr = (FreqInfo *)Tcl_GetHashValue(hPtr);
		switch (graphPtr->mode) {
		case MODE_STACKED:
		    c2.y = infoPtr->lastY;
		    c1.y += c2.y;
		    infoPtr->lastY = c1.y;
		    break;

		case MODE_ALIGNED:
		    slice = barWidth / (double)infoPtr->freq;
		    c1.x += (slice * infoPtr->count);
		    c2.x = c1.x + slice;
		    infoPtr->count++;
		    break;

		case MODE_OVERLAP:
		    slice = barWidth / (double)(infoPtr->freq * 2);
		    width = slice * (infoPtr->freq + 1);
		    c1.x += (slice * infoPtr->count);
		    c2.x = c1.x + width;
		    infoPtr->count++;
		    break;
		case MODE_NORMAL:
		    break;
		}
	    }
	}
	invertBar = FALSE;
	if (c1.y < c2.y) {
	    double temp;

	    /* Handle negative bar values by swapping ordinates */
	    temp = c1.y, c1.y = c2.y, c2.y = temp;
	    invertBar = TRUE;
	}
	/*
	 * Get the two corners of the bar segment and compute the rectangle
	 */
	c1 = Blt_Transform2DPt(graphPtr, c1.x, c1.y, &barPtr->axes);
	c2 = Blt_Transform2DPt(graphPtr, c2.x, c2.y, &barPtr->axes);

	/* Bound the bars vertically by the size of the graph window */
	if (c1.y < 0.0) {
	    c1.y = 0.0;
	} else if (c1.y > (double)graphPtr->height) {
	    c1.y = (double)graphPtr->height;
	}
	if (c2.y < 0.0) {
	    c2.y = 0.0;
	} else if (c2.y > (double)graphPtr->height) {
	    c2.y = (double)graphPtr->height;
	}
	height = (int)Round(Fabs(c1.y - c2.y));
	if (invertBar) {
	    rectPtr->y = (int)MIN(c1.y, c2.y);
	} else {
	    rectPtr->y = (int)(MAX(c1.y, c2.y)) - height;
	}
	rectPtr->x = (int)MIN(c1.x, c2.x);
	rectPtr->width = (int)Round(Fabs(c1.x - c2.x)) + 1;
	if (rectPtr->width < 1) {
	    rectPtr->width = 1;
	}
	rectPtr->height = height + 1;
	if (rectPtr->height < 1) {
	    rectPtr->height = 1;
	}
	rectMap[count] = i;	/* Save the data index corresponding to the
				 * rectangle */
	rectPtr++;
	count++;
    }
    barPtr->numRects = count;
    barPtr->rectArr = rectArr;
    barPtr->rectMap = rectMap;
    if (barPtr->reqNumActive > 0) {
	ComputeActiveBars(barPtr);
    }
    MergePens(barPtr);
}

/*
 * -----------------------------------------------------------------
 *
 * DrawSymbol --
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
 * -----------------------------------------------------------------
 */
/*ARGSUSED*/
static void
DrawSymbol(graphPtr, drawable, elemPtr, x, y, size)
    Graph *graphPtr;
    Drawable drawable;		/* Pixmap or window to draw into */
    Element *elemPtr;
    int x, y;
    int size;
{
    BarPen *penPtr = ((Bar *)elemPtr)->normalPenPtr;
    int radius;

    radius = (size / 2);
    size--;

    x -= radius;
    y -= radius;
    XSetTSOrigin(graphPtr->display, penPtr->gc, x, y);
    XFillRectangle(graphPtr->display, drawable, penPtr->gc, x, y, size, size);
    XSetTSOrigin(graphPtr->display, penPtr->gc, 0, 0);
}

static void
DrawSegments(graphPtr, drawable, penPtr, rectPtr, numRects)
    Graph *graphPtr;
    Drawable drawable;		/* Pixmap or window to draw into */
    BarPen *penPtr;
    register XRectangle *rectPtr;
    int numRects;
{
    XFillRectangles(graphPtr->display, drawable, penPtr->gc, rectPtr, numRects);
    if ((penPtr->borderWidth > 0) && (penPtr->relief != TK_RELIEF_FLAT)) {
	int twiceBW;
	register int i;

	twiceBW = (2 * penPtr->borderWidth);
	for (i = 0; i < numRects; i++, rectPtr++) {
	    if (twiceBW >= (int)MIN(rectPtr->width, rectPtr->height)) {
		continue;
	    }
	    Tk_Draw3DRectangle(graphPtr->tkwin, drawable, penPtr->border,
		rectPtr->x, rectPtr->y, rectPtr->width, rectPtr->height,
		penPtr->borderWidth, penPtr->relief);
	}
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * DrawNormalBar --
 *
 *	Draws the rectangle representing the bar element.  If the
 *	relief option is set to "raised" or "sunken" and the bar
 *	borderwidth is set (borderwidth > 0), a 3D border is drawn
 *	around the bar.
 *
 *	Don't draw bars that aren't visible (i.e. within the limits
 *	of the axis).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	X drawing commands are output.
 *
 * ----------------------------------------------------------------------
 */
static void
DrawNormalBar(graphPtr, drawable, elemPtr)
    Graph *graphPtr;
    Drawable drawable;
    Element *elemPtr;
{
    Bar *barPtr = (Bar *)elemPtr;
    register BarStyle *stylePtr;
    register int i;

    for (stylePtr = barPtr->styleArr, i = 0; i < barPtr->numStyles;
	i++, stylePtr++) {
	if (stylePtr->numRects > 0) {
	    DrawSegments(graphPtr, drawable, stylePtr->penPtr,
		stylePtr->rectPtr, stylePtr->numRects);
	}
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * DrawActiveBar --
 *
 *	Draws rectangles representing the active segments of the
 *	bar element.  If the -relief option is set (other than "flat")
 *	and the borderwidth is greater than 0, a 3D border is drawn
 *	around the each bar segment.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	X drawing commands are output.
 *
 * ----------------------------------------------------------------------
 */
static void
DrawActiveBar(graphPtr, drawable, elemPtr)
    Graph *graphPtr;
    Drawable drawable;
    Element *elemPtr;
{
    Bar *barPtr = (Bar *)elemPtr;

    if (barPtr->activePenPtr == NULL) {
	return;
    }
    if (barPtr->reqNumActive > 0) {
	if (barPtr->flags & ELEM_UPDATE_ACTIVE) {
	    ComputeActiveBars(barPtr);
	}
	DrawSegments(graphPtr, drawable, barPtr->activePenPtr,
	    barPtr->activeArr, barPtr->numActive);
    } else if (barPtr->reqNumActive < 0) {
	DrawSegments(graphPtr, drawable, barPtr->activePenPtr, barPtr->rectArr,
	    barPtr->numRects);
    }
}

/*
 * -----------------------------------------------------------------
 *
 * PrintSymbol --
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
/*ARGSUSED*/
static void
PrintSymbol(graphPtr, printable, elemPtr, x, y, size)
    Graph *graphPtr;
    Printable printable;
    Element *elemPtr;
    int size;
    int x, y;
{
    Bar *barPtr = (Bar *)elemPtr;

    /*
     * Build a PostScript procedure to draw the fill and outline of
     * the symbol after the path of the symbol shape has been formed
     */
    Blt_PrintAppend(printable, "\n",
	"/DrawSymbolProc {\n",
	"  gsave\n    ", (char *)NULL);
    if (barPtr->normalPenPtr->stipple != None) {
	int width, height;

	Blt_BackgroundToPostScript(printable,
	    Tk_3DBorderColor(barPtr->normalPenPtr->border));
	Blt_PrintAppend(printable, "    Fill\n    ", (char *)NULL);
	Blt_ForegroundToPostScript(printable, barPtr->normalPenPtr->fg);
	Tk_SizeOfBitmap(graphPtr->display, barPtr->normalPenPtr->stipple,
	    &width, &height);
	Blt_StippleToPostScript(printable, graphPtr->display,
	    barPtr->normalPenPtr->stipple, width, height, 1);
    } else {
	Blt_ForegroundToPostScript(printable, barPtr->normalPenPtr->fg);
	Blt_PrintAppend(printable, "    fill\n", (char *)NULL);
    }
    Blt_PrintAppend(printable, "  grestore\n", (char *)NULL);
    Blt_PrintAppend(printable, "} def\n\n", (char *)NULL);
    Blt_PrintFormat(printable, "%d %d %d Sq\n", x, y, size);
}

static void
PrintSegments(graphPtr, printable, penPtr, rectPtr, numRects)
    Graph *graphPtr;
    Printable printable;
    BarPen *penPtr;
    register XRectangle *rectPtr;
    int numRects;
{
    register int i;

    for (i = 0; i < numRects; i++, rectPtr++) {
	if ((rectPtr->width < 1) || (rectPtr->height < 1)) {
	    continue;
	}
	if (penPtr->stipple != None) {
	    int width, height;

	    Blt_BackgroundToPostScript(printable,
		Tk_3DBorderColor(penPtr->border));
	    Blt_RectangleToPostScript(printable, rectPtr->x, rectPtr->y,
		(int)rectPtr->width, (int)rectPtr->height);
	    Tk_SizeOfBitmap(graphPtr->display, penPtr->stipple, &width,
		&height);
	    Blt_ForegroundToPostScript(printable, penPtr->fg);
	    Blt_StippleToPostScript(printable, graphPtr->display,
		penPtr->stipple, width, height, True);
	} else {
	    Blt_ForegroundToPostScript(printable, penPtr->fg);
	    Blt_RectangleToPostScript(printable, rectPtr->x, rectPtr->y,
		(int)rectPtr->width, (int)rectPtr->height);
	}
	if ((penPtr->borderWidth > 0) && (penPtr->relief != TK_RELIEF_FLAT)) {
	    Blt_3DRectangleToPostScript(printable, penPtr->border, rectPtr->x,
		rectPtr->y, (int)rectPtr->width, (int)rectPtr->height,
		penPtr->borderWidth, penPtr->relief);
	}
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * PrintActiveBar --
 *
 *	Similar to the PrintNormalBar procedure, generates PostScript
 *	commands to display the rectangles representing the active bar
 *	segments of the element.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	PostScript pen width, dashes, and color settings are changed.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static void
PrintActiveBar(graphPtr, printable, elemPtr)
    Graph *graphPtr;
    Printable printable;
    Element *elemPtr;
{
    Bar *barPtr = (Bar *)elemPtr;

    if (barPtr->activePenPtr == NULL) {
	return;
    }
    if (barPtr->reqNumActive > 0) {
	if (barPtr->flags & ELEM_UPDATE_ACTIVE) {
	    ComputeActiveBars(barPtr);
	}
	PrintSegments(graphPtr, printable, barPtr->activePenPtr,
	    barPtr->activeArr, barPtr->numActive);
    } else if (barPtr->reqNumActive < 0) {
	PrintSegments(graphPtr, printable, barPtr->activePenPtr,
	    barPtr->rectArr, barPtr->numRects);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * PrintNormalBar --
 *
 *	Generates PostScript commands to form the rectangles
 *	representing the segments of the bar element.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	PostScript pen width, dashes, and color settings are changed.
 *
 * ----------------------------------------------------------------------
 */
/*ARGSUSED*/
static void
PrintNormalBar(graphPtr, printable, elemPtr)
    Graph *graphPtr;
    Printable printable;
    Element *elemPtr;
{
    Bar *barPtr = (Bar *)elemPtr;
    register BarStyle *stylePtr;
    register int i;

    for (stylePtr = barPtr->styleArr, i = 0; i < barPtr->numStyles;
	i++, stylePtr++) {
	if (stylePtr->numRects > 0) {
	    PrintSegments(graphPtr, printable, stylePtr->penPtr,
		stylePtr->rectPtr, stylePtr->numRects);
	}
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * DestroyBar --
 *
 *	Release memory and resources allocated for the bar element.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Everything associated with the bar element is freed up.
 *
 * ----------------------------------------------------------------------
 */
static void
DestroyBar(graphPtr, elemPtr)
    Graph *graphPtr;
    Element *elemPtr;
{
    Bar *barPtr = (Bar *)elemPtr;

    if (barPtr->normalPenPtr != &(barPtr->normalPen)) {
	Blt_FreePen(graphPtr, (Pen *)barPtr->normalPenPtr);
    }
    DestroyPen(graphPtr, (Pen *)&(barPtr->normalPen));
    if (barPtr->activePenPtr != NULL) {
	Blt_FreePen(graphPtr, (Pen *)barPtr->activePenPtr);
    }
    if (barPtr->rectArr != NULL) {
	free((char *)barPtr->rectArr);
    }
    if (barPtr->x.clientId != NULL) {
	Blt_FreeVectorId(barPtr->x.clientId);
    } else if (barPtr->x.valueArr != NULL) {
	free((char *)barPtr->x.valueArr);
    }
    if (barPtr->y.clientId != NULL) {
	Blt_FreeVectorId(barPtr->y.clientId);
    } else if (barPtr->y.valueArr != NULL) {
	free((char *)barPtr->y.valueArr);
    }
    if (barPtr->rectMap != NULL) {
	free((char *)barPtr->rectMap);
    }
    if (barPtr->reqActiveArr != NULL) {
	free((char *)barPtr->reqActiveArr);
    }
    if (barPtr->styleArr != NULL) {
	FreeBarStyles(barPtr, barPtr->styleArr, barPtr->numStyles);
    }
    if (barPtr->tags != NULL) {
	free((char *)barPtr->tags);
    }
}

/*
 * ----------------------------------------------------------------------
 *
 * Blt_BarElement --
 *
 *	Allocate memory and initialize methods for the new bar element.
 *
 * Results:
 *	The pointer to the newly allocated element structure is returned.
 *
 * Side effects:
 *	Memory is allocated for the bar element structure.
 *
 * ----------------------------------------------------------------------
 */

static ElemClassInfo barClassInfo =
{
    configSpecs,
    ClosestBar,
    ConfigureBar,
    DestroyBar,
    DrawActiveBar,
    DrawNormalBar,
    DrawSymbol,
    ExtentsBar,
    PrintActiveBar,
    PrintNormalBar,
    PrintSymbol,
    TransformBar,
};


Element *
Blt_BarElement()
{
    register Bar *barPtr;

    barPtr = (Bar *)calloc(1, sizeof(Bar));
    assert(barPtr);
    barPtr->normalPenPtr = &(barPtr->normalPen);
    barPtr->infoPtr = &barClassInfo;
    barPtr->labelRelief = TK_RELIEF_FLAT;
    InitPen(barPtr->normalPenPtr);
    return ((Element *)barPtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * Blt_InitFreqTable --
 *
 *	Generate a table of abscissa frequencies.  Duplicate
 *	x-coordinates (depending upon the bar drawing mode) indicate
 *	that something special should be done with each bar segment
 *	mapped to the same abscissa (i.e. it should be stacked,
 *	aligned, or overlay-ed with other segments)
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Memory is allocated for the bar element structure.
 *
 * ----------------------------------------------------------------------
 */
void
Blt_InitFreqTable(graphPtr)
    Graph *graphPtr;
{
    register Element *elemPtr;
    Blt_ListItem item;
    Tcl_HashEntry *hPtr;
    Tcl_HashSearch cursor;
    Bar *barPtr;
    int isNew, count;
    int numStacks, numSegs;
    int numPoints;
    FreqKey key;
    Tcl_HashTable freqTable;
    register int i;
    double *xArr;
    /*
     * Free resources associated with a previous frequency table. This
     * includes the array of frequency information and the table itself
     */
    if (graphPtr->freqArr != NULL) {
	free((char *)graphPtr->freqArr);
	graphPtr->freqArr = NULL;
    }
    if (graphPtr->numStacks > 0) {
	Tcl_DeleteHashTable(&(graphPtr->freqTable));
	graphPtr->numStacks = 0;
    }
    if (graphPtr->mode == MODE_NORMAL) {
	return;			/* No frequency table is needed for
				 * normal mode */
    }
    Tcl_InitHashTable(&(graphPtr->freqTable), sizeof(FreqKey) / sizeof(int));

    /*
     * Initialize a hash table and fill it with unique abscissas.
     * Keep track of the frequency of each x-coordinate and how many
     * abscissas have duplicate mappings.
     */
    Tcl_InitHashTable(&freqTable, sizeof(FreqKey) / sizeof(int));
    numSegs = numStacks = 0;
    for (item = Blt_ListFirstItem(&(graphPtr->elemList));
	item != NULL; item = Blt_ListNextItem(item)) {
	elemPtr = (Element *)Blt_ListGetValue(item);
	if ((elemPtr->hidden) || (elemPtr->type != TYPE_ELEM_BAR)) {
	    continue;
	}
	numSegs++;
	barPtr = (Bar *)elemPtr;
	xArr = barPtr->x.valueArr;
	numPoints = NumberOfPoints(barPtr);
	for (i = 0; i < numPoints; i++) {
	    key.value = xArr[i];
	    key.axes = barPtr->axes;
	    hPtr = Tcl_CreateHashEntry(&freqTable, (char *)&key, &isNew);
	    if (hPtr == NULL) {
		Panic("can't allocate freqTable entry");
	    }
	    if (isNew) {
		count = 1;
	    } else {
		count = (int)Tcl_GetHashValue(hPtr);
		if (count == 1) {
		    numStacks++;
		}
		count++;
	    }
	    Tcl_SetHashValue(hPtr, (ClientData)count);
	}
    }
    if (numSegs == 0) {
	return;			/* No bar elements to be displayed */
    }
    if (numStacks > 0) {
	FreqInfo *infoPtr;
	FreqKey *keyPtr;
	Tcl_HashEntry *h2Ptr;

	graphPtr->freqArr = (FreqInfo *)calloc(numStacks, sizeof(FreqInfo));
	assert(graphPtr->freqArr);
	infoPtr = graphPtr->freqArr;
	for (hPtr = Tcl_FirstHashEntry(&freqTable, &cursor); hPtr != NULL;
	    hPtr = Tcl_NextHashEntry(&cursor)) {
	    count = (int)Tcl_GetHashValue(hPtr);
	    keyPtr = (FreqKey *)Tcl_GetHashKey(&freqTable, hPtr);
	    if (count > 1) {
		h2Ptr = Tcl_CreateHashEntry(&(graphPtr->freqTable),
		    (char *)keyPtr, &isNew);
		count = (int)Tcl_GetHashValue(hPtr);
		infoPtr->freq = count;
		infoPtr->axes = keyPtr->axes;
		Tcl_SetHashValue(h2Ptr, (ClientData)infoPtr);
		infoPtr++;
	    }
	}
    }
    Tcl_DeleteHashTable(&freqTable);
    graphPtr->numStacks = numStacks;
}

/*
 * ----------------------------------------------------------------------
 *
 * Blt_ComputeStacks --
 *
 *	Determine the height of each stack of bar segments.  A stack
 *	is created by designating two or more points with the same
 *	abscissa.  Each ordinate defines the height of a segment in
 *	the stack.  This procedure simply looks at all the data points
 *	summing the heights of each stacked segment. The sum is saved
 *	in the frequency information table.  This value will be used
 *	to calculate the y-axis limits (data limits aren't sufficient).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The heights of each stack is computed. CheckStacks will
 *	use this information to adjust the y-axis limits if necessary.
 *
 * ----------------------------------------------------------------------
 */
void
Blt_ComputeStacks(graphPtr)
    Graph *graphPtr;
{
    Element *elemPtr;
    Bar *barPtr;
    FreqKey key;
    Blt_ListItem item;
    Tcl_HashEntry *hPtr;
    int numPoints;
    register int i;
    register FreqInfo *infoPtr;
    double *xArr, *yArr;

    if ((graphPtr->mode != MODE_STACKED) || (graphPtr->numStacks == 0)) {
	return;
    }
    /* Reset the sums for all duplicate values to zero. */

    infoPtr = graphPtr->freqArr;
    for (i = 0; i < graphPtr->numStacks; i++) {
	infoPtr->sum = 0.0;
	infoPtr++;
    }

    /* Look at each bar point, adding the ordinates of duplicate abscissas */

    for (item = Blt_ListFirstItem(&(graphPtr->elemList));
	item != NULL; item = Blt_ListNextItem(item)) {
	elemPtr = (Element *)Blt_ListGetValue(item);
	if ((elemPtr->hidden) || (elemPtr->type != TYPE_ELEM_BAR)) {
	    continue;
	}
	barPtr = (Bar *)elemPtr;
	xArr = barPtr->x.valueArr;
	yArr = barPtr->y.valueArr;
	numPoints = NumberOfPoints(barPtr);
	for (i = 0; i < numPoints; i++) {
	    key.value = xArr[i];
	    key.axes = barPtr->axes;
	    hPtr = Tcl_FindHashEntry(&(graphPtr->freqTable), (char *)&key);
	    if (hPtr == NULL) {
		continue;
	    }
	    infoPtr = (FreqInfo *)Tcl_GetHashValue(hPtr);
	    infoPtr->sum += yArr[i];
	}
    }
}

void
Blt_ResetStacks(graphPtr)
    Graph *graphPtr;
{
    register FreqInfo *infoPtr;
    register int i;

    infoPtr = graphPtr->freqArr;
    for (i = 0; i < graphPtr->numStacks; i++) {
	infoPtr->lastY = 0.0;
	infoPtr->count = 0;
	infoPtr++;
    }
}
