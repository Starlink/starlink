/*
 * rtdPolyLine.c --
 *
 *      This file imlements line items for canvas widgets.
 *
 * Copyright (c) 1991-1994 The Regents of the University of California.
 * Copyright (c) 1994-1995 Sun Microsystems, Inc.
 * Copyright (c) 1999-2005 Central Laboratory of the Research Councils
 * Copyright (c) 2006 Particle Physics and Astronomy Research Council
 * Copyright (c) 2008 Science and Technology Facilities Council
 *
 * See the Tcl distribution file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * Modified:
 *    Peter W. Draper 07 May 1999: Was tkCanvLine in Tk8.0. Modified
 *    to allow the line coordinates to be supplied via the routine
 *    RtdSetLineCoords, which considerably speeds up coordinate
 *    passing (essential for contour drawing). The canvas type is
 *    renamed to rtd_polyline.
 *    16 March 2006: Now supports line breaks using the badValue. Only
 *    works for un-smoother lines and the badValue applies to the Y
 *    coordinates (data values).
 *    19 February 2008: Add support for a line style.
 */
#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <string.h>
#include <tk.h>
#include <rtdCanvas.h>

/*  Tk internal functions -- XXX may need changing in future releases */
extern void   TkIncludePoint _ANSI_ARGS_((Tk_Item *itemPtr, double *pointPtr));

extern void   TkFillPolygon _ANSI_ARGS_((Tk_Canvas canvas,
                                         double * coordPtr, int numPoints,
                                         Display * display, Drawable drawable, GC gc,
                                         GC outlineGC));

extern int    TkGetMiterPoints _ANSI_ARGS_((double p1[], double p2[],
                                            double p3[], double width, double m1[],
                                            double m2[]));
extern int    TkMakeBezierCurve _ANSI_ARGS_((Tk_Canvas canvas,
                                             double *pointPtr, int numPoints, int numSteps,
                                             XPoint xPoints[], double dblPoints[]));
extern void   TkGetButtPoints _ANSI_ARGS_((double p1[], double p2[],
                                           double width, int project, double m1[],
                                           double m2[]));
extern double TkPolygonToPoint _ANSI_ARGS_((double *polyPtr,
                                            int numPoints, double *pointPtr));
extern int    TkThickPolyLineToArea _ANSI_ARGS_((double *coordPtr,
                                                 int numPoints, double width, int capStyle,
                                                 int joinStyle, double *rectPtr));
extern int    TkPolygonToArea _ANSI_ARGS_((double *polyPtr,
                                           int numPoints, double *rectPtr));
extern void   TkMakeBezierPostscript _ANSI_ARGS_((Tcl_Interp *interp,
                                                  Tk_Canvas canvas, double *pointPtr,
                                                  int numPoints));
/*
 * The structure below defines the record for each line item.
 */

typedef struct PolyLineItem  {
    Tk_Item header;             /* Generic stuff that's the same for all
                                 * types.  MUST BE FIRST IN STRUCTURE. */
    Tk_Canvas canvas;           /* Canvas containing item.  Needed for
                                 * parsing arrow shapes. */
    int numPoints;              /* Number of points in line (always >= 2). */
    double *coordPtr;           /* Pointer to malloc-ed array containing
                                 * x- and y-coords of all points in line.
                                 * X-coords are even-valued indices, y-coords
                                 * are corresponding odd-valued indices. If
                                 * the line has arrowheads then the first
                                 * and last points have been adjusted to refer
                                 * to the necks of the arrowheads rather than
                                 * their tips.  The actual endpoints are
                                 * stored in the *firstArrowPtr and
                                 * *lastArrowPtr, if they exist. */
    double nullValue;           /* Value for missing points, not for smoothed
                                 * lines, set to -DBL_MAX by default, applies
                                 * to the Y coordinates only. */
    int width;                  /* Width of line. */
    XColor *fg;                 /* Foreground color for line. */
    Pixmap fillStipple;         /* Stipple bitmap for filling line. */
    int capStyle;               /* Cap style for line. */
    int joinStyle;              /* Join style for line. */
    GC gc;                      /* Graphics context for filling line. */
    GC arrowGC;                 /* Graphics context for drawing arrowheads. */
    Tk_Uid arrow;               /* Indicates whether or not to draw arrowheads:
                                 * "none", "first", "last", or "both". */
    float arrowShapeA;          /* Distance from tip of arrowhead to center. */
    float arrowShapeB;          /* Distance from tip of arrowhead to trailing
                                 * point, measured along shaft. */
    float arrowShapeC;          /* Distance of trailing points from outside
                                 * edge of shaft. */
    double *firstArrowPtr;      /* Points to array of PTS_IN_ARROW points
                                 * describing polygon for arrowhead at first
                                 * point in line.  First point of arrowhead
                                 * is tip.  Malloc'ed.  NULL means no arrowhead
                                 * at first point. */
    double *lastArrowPtr;       /* Points to polygon for arrowhead at last
                                 * point in line (PTS_IN_ARROW points, first
                                 * of which is tip).  Malloc'ed.  NULL means
                                 * no arrowhead at last point. */
    int smooth;                 /* Non-zero means draw line smoothed (i.e.
                                 * with Bezier splines). */
    int splineSteps;            /* Number of steps in each spline segment. */
    int style;                  /* Line drawing style, 0, 1, 2 or 3. */
    char dash[32];              /* Dash array, can be used for custom styles. */
} PolyLineItem;

/*
 * Number of points in an arrowHead:
 */
#define PTS_IN_ARROW 6

/*
 * Prototypes for procedures defined in this file:
 */

static int              ArrowheadPostscript _ANSI_ARGS_((Tcl_Interp *interp,
                            Tk_Canvas canvas, PolyLineItem *linePtr,
                            double *arrowPtr));
static void             ComputeLineBbox _ANSI_ARGS_((Tk_Canvas canvas,
                            PolyLineItem *linePtr));
static int              ConfigureLine _ANSI_ARGS_((Tcl_Interp *interp,
                            Tk_Canvas canvas, Tk_Item *itemPtr, int objc,
                            Tcl_Obj *CONST objv[], int flags));
static int              ConfigureArrows _ANSI_ARGS_((Tk_Canvas canvas,
                            PolyLineItem *linePtr));
static int              CreateLine _ANSI_ARGS_((Tcl_Interp *interp,
                            Tk_Canvas canvas, struct Tk_Item *itemPtr,
                            int objc, Tcl_Obj *CONST objv[]));
static int              DashParseProc _ANSI_ARGS_((ClientData clientData,
                            Tcl_Interp *interp, Tk_Window tkwin, char *value,
                            char *recordPtr, int offset));
static char *           DashPrintProc _ANSI_ARGS_((ClientData clientData,
                            Tk_Window tkwin, char *recordPtr, int offset,
                            Tcl_FreeProc **freeProcPtr));
static void             DeleteLine _ANSI_ARGS_((Tk_Canvas canvas,
                            Tk_Item *itemPtr, Display *display));
static int              LineCoords _ANSI_ARGS_((Tcl_Interp *interp,
                            Tk_Canvas canvas, Tk_Item *itemPtr,
                            int objc, Tcl_Obj *CONST objv[]));
static int              LineToArea _ANSI_ARGS_((Tk_Canvas canvas,
                            Tk_Item *itemPtr, double *rectPtr));
static double           LineToPoint _ANSI_ARGS_((Tk_Canvas canvas,
                            Tk_Item *itemPtr, double *coordPtr));
static int              ParseArrowShape _ANSI_ARGS_((ClientData clientData,
                            Tcl_Interp *interp, Tk_Window tkwin, char *value,
                            char *recordPtr, int offset));
static char *           PrintArrowShape _ANSI_ARGS_((ClientData clientData,
                            Tk_Window tkwin, char *recordPtr, int offset,
                            Tcl_FreeProc **freeProcPtr));
static void             ScaleLine _ANSI_ARGS_((Tk_Canvas canvas,
                            Tk_Item *itemPtr, double originX, double originY,
                            double scaleX, double scaleY));
static void             TranslateLine _ANSI_ARGS_((Tk_Canvas canvas,
                            Tk_Item *itemPtr, double deltaX, double deltaY));

/*
 * Information used for parsing configuration specs.  If you change any
 * of the default strings, be sure to change the corresponding default
 * values in CreateLine.
 */

static Tk_CustomOption arrowShapeOption = {
    ParseArrowShape, PrintArrowShape, (ClientData) NULL
};
static Tk_CustomOption tagsOption = {
    (Tk_OptionParseProc *) Tk_CanvasTagsParseProc, Tk_CanvasTagsPrintProc, (ClientData) NULL
};
static Tk_CustomOption dashOption = {
    DashParseProc, DashPrintProc, (ClientData) NULL
};

static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_UID, "-arrow", (char *) NULL, (char *) NULL,
        "none", Tk_Offset(PolyLineItem, arrow), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-arrowshape", (char *) NULL, (char *) NULL,
        "8 10 3", Tk_Offset(PolyLineItem, arrowShapeA),
        TK_CONFIG_DONT_SET_DEFAULT, &arrowShapeOption},
    {TK_CONFIG_CAP_STYLE, "-capstyle", (char *) NULL, (char *) NULL,
        "butt", Tk_Offset(PolyLineItem, capStyle), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-dash", (char *) NULL, (char *) NULL, (char *) NULL,
     Tk_Offset(PolyLineItem, dash), TK_CONFIG_NULL_OK, &dashOption},
    {TK_CONFIG_COLOR, "-fill", (char *) NULL, (char *) NULL,
        "black", Tk_Offset(PolyLineItem, fg), TK_CONFIG_NULL_OK},
    {TK_CONFIG_JOIN_STYLE, "-joinstyle", (char *) NULL, (char *) NULL,
        "round", Tk_Offset(PolyLineItem, joinStyle), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_DOUBLE, "-nullvalue", (char *) NULL, (char *) NULL,
        "1", Tk_Offset(PolyLineItem, nullValue), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BOOLEAN, "-smooth", (char *) NULL, (char *) NULL,
        "0", Tk_Offset(PolyLineItem, smooth), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_INT, "-splinesteps", (char *) NULL, (char *) NULL,
        "12", Tk_Offset(PolyLineItem, splineSteps), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BITMAP, "-stipple", (char *) NULL, (char *) NULL,
        (char *) NULL, Tk_Offset(PolyLineItem, fillStipple), TK_CONFIG_NULL_OK},
    {TK_CONFIG_INT, "-style", (char *) NULL, (char *) NULL,
        "0", Tk_Offset(PolyLineItem, style), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-tags", (char *) NULL, (char *) NULL,
        (char *) NULL, 0, TK_CONFIG_NULL_OK, &tagsOption},
    {TK_CONFIG_PIXELS, "-width", (char *) NULL, (char *) NULL,
        "1", Tk_Offset(PolyLineItem, width), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
        (char *) NULL, 0, 0}
};

/*
 * The structures below defines the line item type by means
 * of procedures that can be invoked by generic item code.
 */

Tk_ItemType tkPolyLineType = {
    "rtd_polyline",                     /* name */
    sizeof(PolyLineItem),               /* itemSize */
    CreateLine,                         /* createProc */
    configSpecs,                        /* configSpecs */
    ConfigureLine,                      /* configureProc */
    LineCoords,                         /* coordProc */
    DeleteLine,                         /* deleteProc */
    RtdLineDisplay,                     /* displayProc */
    TK_CONFIG_OBJS,                     /* alwaysRedraw & flags */
    LineToPoint,                        /* pointProc */
    LineToArea,                         /* areaProc */
    RtdLineToPostscript,                /* postscriptProc */
    ScaleLine,                          /* scaleProc */
    TranslateLine,                      /* translateProc */
    (Tk_ItemIndexProc *) NULL,          /* indexProc */
    (Tk_ItemCursorProc *) NULL,         /* icursorProc */
    (Tk_ItemSelectionProc *) NULL,      /* selectionProc */
    (Tk_ItemInsertProc *) NULL,         /* insertProc */
    (Tk_ItemDCharsProc *) NULL,         /* dTextProc */
    (Tk_ItemType *) NULL                /* nextPtr */
};

/*
 * The Tk_Uid's below refer to uids for the various arrow types:
 */

static Tk_Uid noneUid = NULL;
static Tk_Uid firstUid = NULL;
static Tk_Uid lastUid = NULL;
static Tk_Uid bothUid = NULL;

/*
 * The definition below determines how large are static arrays
 * used to hold spline points (splines larger than this have to
 * have their arrays malloc-ed).
 */

#define MAX_STATIC_POINTS 200

/*
 *  Dash patterns for the non-filled line styles, should give dot, dash and
 *  dot-dash.
 */
static char dot[] = { 6, 6, 0 };
static char dash[] = { 12, 6, 0 };
static char dotdash[] = { 18, 6, 6, 6, 0 };
static char *dashes[] = { dot, dash, dotdash };

/*  Definitions etc. for backdoor command which allows coordinates to
    be passed without conversion to string */

void RtdSetLineCoords( Tcl_Interp *interp, const double *x, const double *y,
                       int numPoints );
static Tk_Canvas lastCanvas_;
static PolyLineItem *lastItem_ = NULL;

/*
 *--------------------------------------------------------------
 * Polyline_Init --
 *
 *   This procedure initialises the rtd_polyline canvas item.
 *
 *--------------------------------------------------------------
 *
 */
int Polyline_Init()
{
    Tk_CreateItemType(&tkPolyLineType);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 * RtdLineCreate --
 *
 *    Create an "instance" for indirect use.
 *
 *    Returns a Tk_Item pointer, which can then be used in
 *    conjunction with another item that is directly managed
 *    by a canvas. All arguments except itemPtr are as passed to the
 *    CreateLine function.
 *
 *--------------------------------------------------------------
 *
 */
int RtdLineCreate( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item **itemPtr,
                   int objc, Tcl_Obj *CONST objv[] )
{
    *itemPtr = (Tk_Item *) ckalloc( sizeof(PolyLineItem) );
    return CreateLine( interp, canvas, *itemPtr, objc, objv );
}

/*
 *--------------------------------------------------------------
 * RtdLineDelete --
 *
 *    Delete an "instance" created for indirect use.
 *
 *--------------------------------------------------------------
 */
int RtdLineDelete( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display )
{
    DeleteLine( canvas, itemPtr, display );
    if ( itemPtr != NULL ) {
        ckfree( (char *) itemPtr );
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * CreateLine --
 *
 *      This procedure is invoked to create a new line item in
 *      a canvas.
 *
 * Results:
 *      A standard Tcl return value.  If an error occurred in
 *      creating the item, then an error message is left in
 *      interp->result;  in this case itemPtr is left uninitialized,
 *      so it can be safely freed by the caller.
 *
 * Side effects:
 *      A new line item is created.
 *
 *--------------------------------------------------------------
 */
static int CreateLine( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
                       int objc, Tcl_Obj *CONST objv[] )
{
    PolyLineItem *linePtr = (PolyLineItem *) itemPtr;
    int i;

    if (objc < 4) {
        Tcl_AppendResult(interp, "wrong # args: should be \"",
                         Tk_PathName(Tk_CanvasTkwin(canvas)), " create ",
                         itemPtr->typePtr->name,
                         " x1 y1 x2 y2 ?x3 y3 ...? ?options?\"",
                         (char *) NULL);
        return TCL_ERROR;
    }

    /*
     * Carry out initialization that is needed to set defaults and to
     * allow proper cleanup after errors during the the remainder of
     * this procedure.
     */

    linePtr->canvas = canvas;
    linePtr->numPoints = 0;
    linePtr->coordPtr = NULL;
    linePtr->nullValue = -DBL_MAX;
    linePtr->width = 1;
    linePtr->style = 0;
    linePtr->dash[0] = '\0';
    linePtr->fg = None;
    linePtr->fillStipple = None;
    linePtr->capStyle = CapButt;
    linePtr->joinStyle = JoinRound;
    linePtr->gc = None;
    linePtr->arrowGC = None;
    if (noneUid == NULL) {
        noneUid = Tk_GetUid("none");
        firstUid = Tk_GetUid("first");
        lastUid = Tk_GetUid("last");
        bothUid = Tk_GetUid("both");
    }
    linePtr->arrow = noneUid;
    linePtr->arrowShapeA = 8.0f;
    linePtr->arrowShapeB = 10.0f;
    linePtr->arrowShapeC = 3.0f;
    linePtr->firstArrowPtr = NULL;
    linePtr->lastArrowPtr = NULL;
    linePtr->smooth = 0;
    linePtr->splineSteps = 12;

    /*
     * Count the number of points and then parse them into a point
     * array.  Leading arguments are assumed to be points if they
     * start with a digit or a minus sign followed by a digit.
     */
    for (i = 1; i < objc; i++) {
	char *arg = Tcl_GetString(objv[i]);
	if ((arg[0] == '-') && (arg[1] >= 'a') && (arg[1] <= 'z')) {
	    break;
	}
    }

    if (LineCoords(interp, canvas, itemPtr, i, objv) != TCL_OK) {
        goto error;
    }
    if (ConfigureLine(interp, canvas, itemPtr, objc-i, objv+i, 0) == TCL_OK) {
        return TCL_OK;
    }

    error:
    DeleteLine(canvas, itemPtr, Tk_Display(Tk_CanvasTkwin(canvas)));
    return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * LineCoords --
 *
 *      This procedure is invoked to process the "coords" widget
 *      command on lines.  See the user documentation for details
 *      on what it does.
 *
 * Results:
 *      Returns TCL_OK or TCL_ERROR, and sets interp->result.
 *
 * Side effects:
 *      The coordinates for the given item may be changed.
 *
 *--------------------------------------------------------------
 */

static int LineCoords( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
                       int objc, Tcl_Obj *CONST objv[] )
{
    PolyLineItem *linePtr = (PolyLineItem *) itemPtr;
    double *coordPtr;
    int i, numPoints;

    /*  Record canvas and item for backdoor updates to coordinates. */
    lastCanvas_ = canvas;
    lastItem_ = linePtr;

    if (objc == 0) {
        double *coordPtr;
        int numCoords;
	Tcl_Obj *subobj, *obj = Tcl_NewObj();

        numCoords = 2*linePtr->numPoints;
        if (linePtr->firstArrowPtr != NULL) {
            coordPtr = linePtr->firstArrowPtr;
        }
        else {
            coordPtr = linePtr->coordPtr;
        }
        for (i = 0; i < numCoords; i++, coordPtr++) {
            if (i == 2) {
                coordPtr = linePtr->coordPtr+2;
            }
            if ((linePtr->lastArrowPtr != NULL) && (i == (numCoords-2))) {
                coordPtr = linePtr->lastArrowPtr;
            }
	    subobj = Tcl_NewDoubleObj(*coordPtr);
	    Tcl_ListObjAppendElement(interp, obj, subobj);
        }
        Tcl_SetObjResult(interp, obj);
        return TCL_OK;
    }
    else if (objc < 4) {
        Tcl_AppendResult(interp,
                "too few coordinates for line: must have at least 4",
                (char *) NULL);
        return TCL_ERROR;
    }
    else if (objc & 1) {
        Tcl_AppendResult(interp,
                "odd number of coordinates specified for line",
                (char *) NULL);
        return TCL_ERROR;
    }
    else {
	numPoints = objc/2;
	if (linePtr->numPoints != numPoints) {
	    coordPtr = (double *) ckalloc((unsigned)
                                          (sizeof(double) * objc));
	    if (linePtr->coordPtr != NULL) {
		ckfree((char *) linePtr->coordPtr);
	    }
	    linePtr->coordPtr = coordPtr;

	    linePtr->numPoints = numPoints;
	}
	coordPtr = linePtr->coordPtr;
	for (i = 0; i <objc; i++) {
	    if (Tk_CanvasGetCoordFromObj(interp, canvas, objv[i],
		    coordPtr++) != TCL_OK) {
  		return TCL_ERROR;
  	    }
  	}

        /*
         * Update arrowheads by throwing away any existing arrow-head
         * information and calling ConfigureArrows to recompute it.
         */

        if (linePtr->firstArrowPtr != NULL) {
            ckfree((char *) linePtr->firstArrowPtr);
            linePtr->firstArrowPtr = NULL;
        }
        if (linePtr->lastArrowPtr != NULL) {
            ckfree((char *) linePtr->lastArrowPtr);
            linePtr->lastArrowPtr = NULL;
        }
        if (linePtr->arrow != noneUid) {
            ConfigureArrows(canvas, linePtr);
        }
        ComputeLineBbox(canvas, linePtr);
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * ConfigureLine --
 *
 *      This procedure is invoked to configure various aspects
 *      of a line item such as its background color.
 *
 * Results:
 *      A standard Tcl result code.  If an error occurs, then
 *      an error message is left in interp->result.
 *
 * Side effects:
 *      Configuration information, such as colors and stipple
 *      patterns, may be set for itemPtr.
 *
 *--------------------------------------------------------------
 */

static int
ConfigureLine(Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
              int objc, Tcl_Obj *CONST objv[], int flags )
{
    PolyLineItem *linePtr = (PolyLineItem *) itemPtr;
    XGCValues gcValues;
    GC newGC, arrowGC;
    unsigned long mask;
    Tk_Window tkwin;

    tkwin = Tk_CanvasTkwin(canvas);

    if (Tk_ConfigureWidget(interp, tkwin, configSpecs, objc,
                           (CONST84 char **) objv, (char *) linePtr,
                           flags|TK_CONFIG_OBJS) != TCL_OK ) {
        return TCL_ERROR;
    }

    /*
     * A few of the options require additional processing, such as
     * graphics contexts.
     */

    if (linePtr->fg == NULL) {
        newGC = arrowGC = None;
    }
    else {
        gcValues.foreground = linePtr->fg->pixel;
        gcValues.join_style = linePtr->joinStyle;
        if (linePtr->width < 0) {
            linePtr->width = 1;
        }
        gcValues.line_width = linePtr->width;
        mask = GCForeground|GCJoinStyle|GCLineWidth;
        if (linePtr->fillStipple != None) {
            gcValues.stipple = linePtr->fillStipple;
            gcValues.fill_style = FillStippled;
            mask |= GCStipple|GCFillStyle;
        }

        /* Dashes if needed (offset is zero). Use style if dash is NULL. */
        if ( linePtr->style > 0 && linePtr->dash[0] == '\0' ) {
            linePtr->style = ( linePtr->style > 3 ) ? 3 : linePtr->style;
            strncpy( linePtr->dash, dashes[linePtr->style-1], 31 );
            linePtr->dash[31] = '\0';
        }

        if ( linePtr->dash[0] != '\0' ) {
            gcValues.line_style = LineOnOffDash;
            gcValues.dashes = *linePtr->dash;
            mask |= GCLineStyle|GCDashList;
        }

        if (linePtr->arrow == noneUid) {
            gcValues.cap_style = linePtr->capStyle;
            mask |= GCCapStyle;
        }
        newGC = Tk_GetGC(tkwin, mask, &gcValues);
        gcValues.line_width = 0;
        arrowGC = Tk_GetGC(tkwin, mask, &gcValues);
    }
    if (linePtr->gc != None) {
        Tk_FreeGC(Tk_Display(tkwin), linePtr->gc);
    }
    if (linePtr->arrowGC != None) {
        Tk_FreeGC(Tk_Display(tkwin), linePtr->arrowGC);
    }
    linePtr->gc = newGC;
    linePtr->arrowGC = arrowGC;

    /*
     * Keep spline parameters within reasonable limits.
     */

    if (linePtr->splineSteps < 1) {
        linePtr->splineSteps = 1;
    }
    else if (linePtr->splineSteps > 100) {
        linePtr->splineSteps = 100;
    }

    /*
     * Setup arrowheads, if needed.  If arrowheads are turned off,
     * restore the line's endpoints (they were shortened when the
     * arrowheads were added).
     */

    if ((linePtr->firstArrowPtr != NULL) && (linePtr->arrow != firstUid)
            && (linePtr->arrow != bothUid)) {
        linePtr->coordPtr[0] = linePtr->firstArrowPtr[0];
        linePtr->coordPtr[1] = linePtr->firstArrowPtr[1];
        ckfree((char *) linePtr->firstArrowPtr);
        linePtr->firstArrowPtr = NULL;
    }
    if ((linePtr->lastArrowPtr != NULL) && (linePtr->arrow != lastUid)
            && (linePtr->arrow != bothUid)) {
        int i;

        i = 2*(linePtr->numPoints-1);
        linePtr->coordPtr[i] = linePtr->lastArrowPtr[0];
        linePtr->coordPtr[i+1] = linePtr->lastArrowPtr[1];
        ckfree((char *) linePtr->lastArrowPtr);
        linePtr->lastArrowPtr = NULL;
    }
    if (linePtr->arrow != noneUid) {
        if ((linePtr->arrow != firstUid) && (linePtr->arrow != lastUid)
                && (linePtr->arrow != bothUid)) {
            Tcl_AppendResult(interp, "bad arrow spec \"",
                    linePtr->arrow, "\": must be none, first, last, or both",
                    (char *) NULL);
            linePtr->arrow = noneUid;
            return TCL_ERROR;
        }
        ConfigureArrows(canvas, linePtr);
    }

    /*
     * Recompute bounding box for line.
     */

    ComputeLineBbox(canvas, linePtr);

    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * DeleteLine --
 *
 *      This procedure is called to clean up the data structure
 *      associated with a line item.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      Resources associated with itemPtr are released.
 *
 *--------------------------------------------------------------
 */
static void
DeleteLine(Tk_Canvas canvas, Tk_Item *itemPtr, Display *display )
{
    PolyLineItem *linePtr = (PolyLineItem *) itemPtr;

    if (linePtr->coordPtr != NULL) {
        ckfree((char *) linePtr->coordPtr);
    }
    if (linePtr->fg != NULL) {
        Tk_FreeColor(linePtr->fg);
    }
    if (linePtr->fillStipple != None) {
        Tk_FreeBitmap(display, linePtr->fillStipple);
    }
    if (linePtr->gc != None) {
        Tk_FreeGC(display, linePtr->gc);
    }
    if (linePtr->arrowGC != None) {
        Tk_FreeGC(display, linePtr->arrowGC);
    }
    if (linePtr->firstArrowPtr != NULL) {
        ckfree((char *) linePtr->firstArrowPtr);
    }
    if (linePtr->lastArrowPtr != NULL) {
        ckfree((char *) linePtr->lastArrowPtr);
    }
}

/*
 *--------------------------------------------------------------
 *
 * ComputeLineBbox --
 *
 *      This procedure is invoked to compute the bounding box of
 *      all the pixels that may be drawn as part of a line.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The fields x1, y1, x2, and y2 are updated in the header
 *      for itemPtr.
 *
 *--------------------------------------------------------------
 */

static void
ComputeLineBbox(Tk_Canvas canvas, PolyLineItem *linePtr)
{
    double *coordPtr;
    int i, width;

    coordPtr = linePtr->coordPtr;
    linePtr->header.x1 = linePtr->header.x2 = (int) *coordPtr;
    linePtr->header.y1 = linePtr->header.y2 = (int) coordPtr[1];

    /*
     * Compute the bounding box of all the points in the line,
     * then expand in all directions by the line's width to take
     * care of butting or rounded corners and projecting or
     * rounded caps.  This expansion is an overestimate (worst-case
     * is square root of two over two) but it's simple.  Don't do
     * anything special for curves.  This causes an additional
     * overestimate in the bounding box, but is faster.
     */

    for (i = 1, coordPtr = linePtr->coordPtr+2; i < linePtr->numPoints;
            i++, coordPtr += 2) {
        TkIncludePoint((Tk_Item *) linePtr, coordPtr);
    }
    width = linePtr->width;
    if (width < 1) {
        width = 1;
    }
    linePtr->header.x1 -= width;
    linePtr->header.x2 += width;
    linePtr->header.y1 -= width;
    linePtr->header.y2 += width;

    /*
     * For mitered lines, make a second pass through all the points.
     * Compute the locations of the two miter vertex points and add
     * those into the bounding box.
     */

    if (linePtr->joinStyle == JoinMiter) {
        for (i = linePtr->numPoints, coordPtr = linePtr->coordPtr; i >= 3;
                i--, coordPtr += 2) {
            double miter[4];
            int j;

            if (TkGetMiterPoints(coordPtr, coordPtr+2, coordPtr+4,
                    (double) width, miter, miter+2)) {
                for (j = 0; j < 4; j += 2) {
                    TkIncludePoint((Tk_Item *) linePtr, miter+j);
                }
            }
        }
    }

    /*
     * Add in the sizes of arrowheads, if any.
     */

    if (linePtr->arrow != noneUid) {
        if (linePtr->arrow != lastUid) {
            for (i = 0, coordPtr = linePtr->firstArrowPtr; i < PTS_IN_ARROW;
                    i++, coordPtr += 2) {
                TkIncludePoint((Tk_Item *) linePtr, coordPtr);
            }
        }
        if (linePtr->arrow != firstUid) {
            for (i = 0, coordPtr = linePtr->lastArrowPtr; i < PTS_IN_ARROW;
                    i++, coordPtr += 2) {
                TkIncludePoint((Tk_Item *) linePtr, coordPtr);
            }
        }
    }

    /*
     * Add one more pixel of fudge factor just to be safe (e.g.
     * X may round differently than we do).
     */

    linePtr->header.x1 -= 1;
    linePtr->header.x2 += 1;
    linePtr->header.y1 -= 1;
    linePtr->header.y2 += 1;
}

/*
 *--------------------------------------------------------------
 *
 * RtdLineDisplay --
 *
 *      This procedure is invoked to draw a line item in a given
 *      drawable.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      ItemPtr is drawn in drawable using the transformation
 *      information in canvas.
 *
 *--------------------------------------------------------------
 */

EXTERN void
RtdLineDisplay( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display,
                Drawable drawable, int x, int y, int width, int height )
{
    PolyLineItem *linePtr = (PolyLineItem *) itemPtr;
    XPoint staticPoints[MAX_STATIC_POINTS];
    XPoint *pointPtr;
    XPoint *pPtr;
    double *coordPtr;
    int i, numPoints;

    if (linePtr->gc == None) {
        return;
    }

    /*
     * Build up an array of points in screen coordinates.  Use a
     * static array unless the line has an enormous number of points;
     * in this case, dynamically allocate an array.  For smoothed lines,
     * generate the curve points on each redisplay.
     */

    if ((linePtr->smooth) && (linePtr->numPoints > 2)) {
        numPoints = 1 + linePtr->numPoints*linePtr->splineSteps;
    }
    else {
        numPoints = linePtr->numPoints;
    }

    if (numPoints <= MAX_STATIC_POINTS) {
        pointPtr = staticPoints;
    }
    else {
        pointPtr = (XPoint *) ckalloc((unsigned) (numPoints * sizeof(XPoint)));
    }

    /*
     * If we're stippling, then modify the stipple offset in the GC (do this
     * now so that it applies to all the lines we're drawing below). Be sure
     * to reset the offset when done, since the GC is supposed to be
     * read-only.
     */
    if (linePtr->fillStipple != None) {
        Tk_CanvasSetStippleOrigin(canvas, linePtr->gc);
        Tk_CanvasSetStippleOrigin(canvas, linePtr->arrowGC);
    }

    /*
     * Same for dashing.
     */
    if ( linePtr->dash[0] != '\0' ) {
        XSetDashes( display, linePtr->gc, 0, linePtr->dash,
                    strlen( linePtr->dash ) );
    }

    /*
     * If there are breaks in the line, indicated by our null value in the Y
     * coordinate, then we draw the polyline piece-by-piece. Note breaks are
     * not possible for lines that are smoothed.
     */
    if ( ( linePtr->smooth) && ( linePtr->numPoints > 2 ) ) {
        numPoints = TkMakeBezierCurve( canvas, linePtr->coordPtr,
                                       linePtr->numPoints,
                                       linePtr->splineSteps, pointPtr,
                                       (double *) NULL );

        XDrawLines( display, drawable, linePtr->gc, pointPtr, numPoints,
                    CoordModeOrigin );
    }
    else {
        int count = 0;
        pPtr = pointPtr;
        coordPtr = linePtr->coordPtr;
        for ( i = 0; i < linePtr->numPoints; i++ ) {

            if ( coordPtr[1] == linePtr->nullValue ) {
                /* Break, draw what we have so far, ignore isolated points. */
                if ( count > 1 ) {
                    XDrawLines( display, drawable, linePtr->gc, pointPtr,
                                count, CoordModeOrigin );
                }
                pPtr = pointPtr;
                count = 0;
            }
            else {
                Tk_CanvasDrawableCoords( canvas, coordPtr[0], coordPtr[1],
                                         &pPtr->x, &pPtr->y );
                count++;
                pPtr++;
            }
            coordPtr += 2;
        }

        /* Draw what we have left */
        if ( count > 1 ) {
            XDrawLines( display, drawable, linePtr->gc, pointPtr, count,
                        CoordModeOrigin );
        }
    }

    /*
     * Free up line storage if it was dynamically allocated.
     */
    if (pointPtr != staticPoints) {
        ckfree((char *) pointPtr);
    }

    /*
     * Display arrowheads, if they are wanted.
     */
    if (linePtr->firstArrowPtr != NULL) {
        TkFillPolygon(canvas, linePtr->firstArrowPtr, PTS_IN_ARROW,
                      display, drawable, linePtr->gc, NULL);
    }
    if (linePtr->lastArrowPtr != NULL) {
        TkFillPolygon(canvas, linePtr->lastArrowPtr, PTS_IN_ARROW,
                      display, drawable, linePtr->gc, NULL);
    }

    if (linePtr->fillStipple != None) {
        XSetTSOrigin(display, linePtr->gc, 0, 0);
        XSetTSOrigin(display, linePtr->arrowGC, 0, 0);
    }
}

/*
 *--------------------------------------------------------------
 *
 * LineToPoint --
 *
 *      Computes the distance from a given point to a given
 *      line, in canvas units.
 *
 * Results:
 *      The return value is 0 if the point whose x and y coordinates
 *      are pointPtr[0] and pointPtr[1] is inside the line.  If the
 *      point isn't inside the line then the return value is the
 *      distance from the point to the line.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */
static double
LineToPoint(Tk_Canvas canvas, Tk_Item *itemPtr, double *pointPtr )
{
    PolyLineItem *linePtr = (PolyLineItem *) itemPtr;
    double *coordPtr, *linePoints;
    double staticSpace[2*MAX_STATIC_POINTS];
    double poly[10];
    double bestDist, dist;
    int numPoints, count;
    int changedMiterToBevel;    /* Non-zero means that a mitered corner
                                 * had to be treated as beveled after all
                                 * because the angle was < 11 degrees. */

    bestDist = 1.0e36;

    /*
     * Handle smoothed lines by generating an expanded set of points
     * against which to do the check.
     */

    if ((linePtr->smooth) && (linePtr->numPoints > 2)) {
        numPoints = 1 + linePtr->numPoints*linePtr->splineSteps;
        if (numPoints <= MAX_STATIC_POINTS) {
            linePoints = staticSpace;
        }
        else {
            linePoints = (double *) ckalloc((unsigned)
                    (2*numPoints*sizeof(double)));
        }
        numPoints = TkMakeBezierCurve(canvas, linePtr->coordPtr,
                linePtr->numPoints, linePtr->splineSteps, (XPoint *) NULL,
                linePoints);
    }
    else {
        numPoints = linePtr->numPoints;
        linePoints = linePtr->coordPtr;
    }

    /*
     * The overall idea is to iterate through all of the edges of
     * the line, computing a polygon for each edge and testing the
     * point against that polygon.  In addition, there are additional
     * tests to deal with rounded joints and caps.
     */

    changedMiterToBevel = 0;
    for (count = numPoints, coordPtr = linePoints; count >= 2;
            count--, coordPtr += 2) {

        /*
         * If rounding is done around the first point then compute
         * the distance between the point and the point.
         */

        if (((linePtr->capStyle == CapRound) && (count == numPoints))
                || ((linePtr->joinStyle == JoinRound)
                        && (count != numPoints))) {
            dist = hypot(coordPtr[0] - pointPtr[0], coordPtr[1] - pointPtr[1])
                    - linePtr->width/2.0;
            if (dist <= 0.0) {
                bestDist = 0.0;
                goto done;
            }
            else if (dist < bestDist) {
                bestDist = dist;
            }
        }

        /*
         * Compute the polygonal shape corresponding to this edge,
         * consisting of two points for the first point of the edge
         * and two points for the last point of the edge.
         */

        if (count == numPoints) {
            TkGetButtPoints(coordPtr+2, coordPtr, (double) linePtr->width,
                    linePtr->capStyle == CapProjecting, poly, poly+2);
        }
        else if ((linePtr->joinStyle == JoinMiter) && !changedMiterToBevel) {
            poly[0] = poly[6];
            poly[1] = poly[7];
            poly[2] = poly[4];
            poly[3] = poly[5];
        }
        else {
            TkGetButtPoints(coordPtr+2, coordPtr, (double) linePtr->width, 0,
                    poly, poly+2);

            /*
             * If this line uses beveled joints, then check the distance
             * to a polygon comprising the last two points of the previous
             * polygon and the first two from this polygon;  this checks
             * the wedges that fill the mitered joint.
             */

            if ((linePtr->joinStyle == JoinBevel) || changedMiterToBevel) {
                poly[8] = poly[0];
                poly[9] = poly[1];
                dist = TkPolygonToPoint(poly, 5, pointPtr);
                if (dist <= 0.0) {
                    bestDist = 0.0;
                    goto done;
                }
                else if (dist < bestDist) {
                    bestDist = dist;
                }
                changedMiterToBevel = 0;
            }
        }
        if (count == 2) {
            TkGetButtPoints(coordPtr, coordPtr+2, (double) linePtr->width,
                    linePtr->capStyle == CapProjecting, poly+4, poly+6);
        }
        else if (linePtr->joinStyle == JoinMiter) {
            if (TkGetMiterPoints(coordPtr, coordPtr+2, coordPtr+4,
                    (double) linePtr->width, poly+4, poly+6) == 0) {
                changedMiterToBevel = 1;
                TkGetButtPoints(coordPtr, coordPtr+2, (double) linePtr->width,
                        0, poly+4, poly+6);
            }
        }
        else {
            TkGetButtPoints(coordPtr, coordPtr+2, (double) linePtr->width, 0,
                    poly+4, poly+6);
        }
        poly[8] = poly[0];
        poly[9] = poly[1];
        dist = TkPolygonToPoint(poly, 5, pointPtr);
        if (dist <= 0.0) {
            bestDist = 0.0;
            goto done;
        }
        else if (dist < bestDist) {
            bestDist = dist;
        }
    }

    /*
     * If caps are rounded, check the distance to the cap around the
     * final end point of the line.
     */

    if (linePtr->capStyle == CapRound) {
        dist = hypot(coordPtr[0] - pointPtr[0], coordPtr[1] - pointPtr[1])
                - linePtr->width/2.0;
        if (dist <= 0.0) {
            bestDist = 0.0;
            goto done;
        }
        else if (dist < bestDist) {
            bestDist = dist;
        }
    }

    /*
     * If there are arrowheads, check the distance to the arrowheads.
     */

    if (linePtr->arrow != noneUid) {
        if (linePtr->arrow != lastUid) {
            dist = TkPolygonToPoint(linePtr->firstArrowPtr, PTS_IN_ARROW,
                    pointPtr);
            if (dist <= 0.0) {
                bestDist = 0.0;
                goto done;
            }
            else if (dist < bestDist) {
                bestDist = dist;
            }
        }
        if (linePtr->arrow != firstUid) {
            dist = TkPolygonToPoint(linePtr->lastArrowPtr, PTS_IN_ARROW,
                    pointPtr);
            if (dist <= 0.0) {
                bestDist = 0.0;
                goto done;
            }
            else if (dist < bestDist) {
                bestDist = dist;
            }
        }
    }

    done:
    if ((linePoints != staticSpace) && (linePoints != linePtr->coordPtr)) {
        ckfree((char *) linePoints);
    }
    return bestDist;
}

/*
 *--------------------------------------------------------------
 *
 * LineToArea --
 *
 *      This procedure is called to determine whether an item
 *      lies entirely inside, entirely outside, or overlapping
 *      a given rectangular area.
 *
 * Results:
 *      -1 is returned if the item is entirely outside the
 *      area, 0 if it overlaps, and 1 if it is entirely
 *      inside the given area.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */
static int
LineToArea( Tk_Canvas canvas, Tk_Item *itemPtr, double *rectPtr )
{
    PolyLineItem *linePtr = (PolyLineItem *) itemPtr;
    double staticSpace[2*MAX_STATIC_POINTS];
    double *linePoints;
    int numPoints, result;

    /*
     * Handle smoothed lines by generating an expanded set of points
     * against which to do the check.
     */

    if ((linePtr->smooth) && (linePtr->numPoints > 2)) {
        numPoints = 1 + linePtr->numPoints*linePtr->splineSteps;
        if (numPoints <= MAX_STATIC_POINTS) {
            linePoints = staticSpace;
        }
        else {
            linePoints = (double *) ckalloc((unsigned)
                    (2*numPoints*sizeof(double)));
        }
        numPoints = TkMakeBezierCurve(canvas, linePtr->coordPtr,
                linePtr->numPoints, linePtr->splineSteps, (XPoint *) NULL,
                linePoints);
    }
    else {
        numPoints = linePtr->numPoints;
        linePoints = linePtr->coordPtr;
    }

    /*
     * Check the segments of the line.
     */

    result = TkThickPolyLineToArea(linePoints, numPoints,
            (double) linePtr->width, linePtr->capStyle, linePtr->joinStyle,
            rectPtr);
    if (result == 0) {
        goto done;
    }

    /*
     * Check arrowheads, if any.
     */

    if (linePtr->arrow != noneUid) {
        if (linePtr->arrow != lastUid) {
            if (TkPolygonToArea(linePtr->firstArrowPtr, PTS_IN_ARROW,
                    rectPtr) != result) {
                result = 0;
                goto done;
            }
        }
        if (linePtr->arrow != firstUid) {
            if (TkPolygonToArea(linePtr->lastArrowPtr, PTS_IN_ARROW,
                    rectPtr) != result) {
                result = 0;
                goto done;
            }
        }
    }

    done:
    if ((linePoints != staticSpace) && (linePoints != linePtr->coordPtr)) {
        ckfree((char *) linePoints);
    }
    return result;
}

/*
 *--------------------------------------------------------------
 *
 * ScaleLine --
 *
 *      This procedure is invoked to rescale a line item.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The line referred to by itemPtr is rescaled so that the
 *      following transformation is applied to all point
 *      coordinates:
 *              x' = originX + scaleX*(x-originX)
 *              y' = originY + scaleY*(y-originY)
 *
 *--------------------------------------------------------------
 */

static void
ScaleLine( Tk_Canvas canvas, Tk_Item *itemPtr, double originX, double originY,
           double scaleX, double scaleY )
{
    PolyLineItem *linePtr = (PolyLineItem *) itemPtr;
    double *coordPtr;
    int i;

    /*
     * Delete any arrowheads before scaling all the points (so that
     * the end-points of the line get restored).
     */

    if (linePtr->firstArrowPtr != NULL) {
        linePtr->coordPtr[0] = linePtr->firstArrowPtr[0];
        linePtr->coordPtr[1] = linePtr->firstArrowPtr[1];
        ckfree((char *) linePtr->firstArrowPtr);
        linePtr->firstArrowPtr = NULL;
    }
    if (linePtr->lastArrowPtr != NULL) {
        int i;

        i = 2*(linePtr->numPoints-1);
        linePtr->coordPtr[i] = linePtr->lastArrowPtr[0];
        linePtr->coordPtr[i+1] = linePtr->lastArrowPtr[1];
        ckfree((char *) linePtr->lastArrowPtr);
        linePtr->lastArrowPtr = NULL;
    }
    for (i = 0, coordPtr = linePtr->coordPtr; i < linePtr->numPoints;
            i++, coordPtr += 2) {
        coordPtr[0] = originX + scaleX*(*coordPtr - originX);
        coordPtr[1] = originY + scaleY*(coordPtr[1] - originY);
    }
    if (linePtr->arrow != noneUid) {
        ConfigureArrows(canvas, linePtr);
    }
    ComputeLineBbox(canvas, linePtr);
}

/*
 *--------------------------------------------------------------
 *
 * TranslateLine --
 *
 *      This procedure is called to move a line by a given amount.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The position of the line is offset by (xDelta, yDelta), and
 *      the bounding box is updated in the generic part of the item
 *      structure.
 *
 *--------------------------------------------------------------
 */

static void
TranslateLine( Tk_Canvas canvas, Tk_Item *itemPtr, double deltaX,
               double deltaY )
{
    PolyLineItem *linePtr = (PolyLineItem *) itemPtr;
    double *coordPtr;
    int i;

    for (i = 0, coordPtr = linePtr->coordPtr; i < linePtr->numPoints;
            i++, coordPtr += 2) {
        coordPtr[0] += deltaX;
        coordPtr[1] += deltaY;
    }
    if (linePtr->firstArrowPtr != NULL) {
        for (i = 0, coordPtr = linePtr->firstArrowPtr; i < PTS_IN_ARROW;
                i++, coordPtr += 2) {
            coordPtr[0] += deltaX;
            coordPtr[1] += deltaY;
        }
    }
    if (linePtr->lastArrowPtr != NULL) {
        for (i = 0, coordPtr = linePtr->lastArrowPtr; i < PTS_IN_ARROW;
                i++, coordPtr += 2) {
            coordPtr[0] += deltaX;
            coordPtr[1] += deltaY;
        }
    }
    ComputeLineBbox(canvas, linePtr);
}

/*
 *--------------------------------------------------------------
 *
 * ParseArrowShape --
 *
 *      This procedure is called back during option parsing to
 *      parse arrow shape information.
 *
 * Results:
 *      The return value is a standard Tcl result:  TCL_OK means
 *      that the arrow shape information was parsed ok, and
 *      TCL_ERROR means it couldn't be parsed.
 *
 * Side effects:
 *      Arrow information in recordPtr is updated.
 *
 *--------------------------------------------------------------
 */
static int
ParseArrowShape( ClientData clientData, Tcl_Interp *interp, Tk_Window tkwin,
                 char *value, char *recordPtr, int offset )
{
    PolyLineItem *linePtr = (PolyLineItem *) recordPtr;
    double a, b, c;
    int argc;
    char **argv = NULL;

    if (offset != Tk_Offset(PolyLineItem, arrowShapeA)) {
        Tcl_Panic( "ParseArrowShape received bogus offset" );
    }

    if (Tcl_SplitList(interp, value, &argc, &argv) != TCL_OK) {
        syntaxError:
        Tcl_ResetResult(interp);
        Tcl_AppendResult(interp, "bad arrow shape \"", value,
                "\": must be list with three numbers", (char *) NULL);
        if (argv != NULL) {
            ckfree((char *) argv);
        }
        return TCL_ERROR;
    }
    if (argc != 3) {
        goto syntaxError;
    }
    if ((Tk_CanvasGetCoord(interp, linePtr->canvas, argv[0], &a) != TCL_OK)
            || (Tk_CanvasGetCoord(interp, linePtr->canvas, argv[1], &b)
                != TCL_OK)
            || (Tk_CanvasGetCoord(interp, linePtr->canvas, argv[2], &c)
                != TCL_OK)) {
        goto syntaxError;
    }
    linePtr->arrowShapeA = (float)a;
    linePtr->arrowShapeB = (float)b;
    linePtr->arrowShapeC = (float)c;
    ckfree((char *) argv);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * PrintArrowShape --
 *
 *      This procedure is a callback invoked by the configuration
 *      code to return a printable value describing an arrow shape.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */
static char *
PrintArrowShape(ClientData clientData, Tk_Window tkwin, char *recordPtr,
                int offset, Tcl_FreeProc **freeProcPtr )
{
    PolyLineItem *linePtr = (PolyLineItem *) recordPtr;
    char *buffer;

    buffer = (char *) ckalloc(120);
    sprintf(buffer, "%.5g %.5g %.5g", linePtr->arrowShapeA,
            linePtr->arrowShapeB, linePtr->arrowShapeC);
    *freeProcPtr = TCL_DYNAMIC;
    return buffer;
}

/*
 *--------------------------------------------------------------
 *
 * ConfigureArrows --
 *
 *      If arrowheads have been requested for a line, this
 *      procedure makes arrangements for the arrowheads.
 *
 * Results:
 *      Always returns TCL_OK.
 *
 * Side effects:
 *      Information in linePtr is set up for one or two arrowheads.
 *      the firstArrowPtr and lastArrowPtr polygons are allocated
 *      and initialized, if need be, and the end points of the line
 *      are adjusted so that a thick line doesn't stick out past
 *      the arrowheads.
 *
 *--------------------------------------------------------------
 */
static int
ConfigureArrows( Tk_Canvas canvas, PolyLineItem *linePtr )
{
    double *poly, *coordPtr;
    double dx, dy, length, sinTheta, cosTheta, temp;
    double fracHeight;                  /* Line width as fraction of
                                         * arrowhead width. */
    double backup;                      /* Distance to backup end points
                                         * so the line ends in the middle
                                         * of the arrowhead. */
    double vertX, vertY;                /* Position of arrowhead vertex. */
    double shapeA, shapeB, shapeC;      /* Adjusted coordinates (see
                                         * explanation below). */

    /*
     * The code below makes a tiny increase in the shape parameters
     * for the line.  This is a bit of a hack, but it seems to result
     * in displays that more closely approximate the specified parameters.
     * Without the adjustment, the arrows come out smaller than expected.
     */

    shapeA = linePtr->arrowShapeA + 0.001;
    shapeB = linePtr->arrowShapeB + 0.001;
    shapeC = linePtr->arrowShapeC + linePtr->width/2.0 + 0.001;

    /*
     * If there's an arrowhead on the first point of the line, compute
     * its polygon and adjust the first point of the line so that the
     * line doesn't stick out past the leading edge of the arrowhead.
     */

    fracHeight = (linePtr->width/2.0)/shapeC;
    backup = fracHeight*shapeB + shapeA*(1.0 - fracHeight)/2.0;
    if (linePtr->arrow != lastUid) {
        poly = linePtr->firstArrowPtr;
        if (poly == NULL) {
            poly = (double *) ckalloc((unsigned)
                    (2*PTS_IN_ARROW*sizeof(double)));
            poly[0] = poly[10] = linePtr->coordPtr[0];
            poly[1] = poly[11] = linePtr->coordPtr[1];
            linePtr->firstArrowPtr = poly;
        }
        dx = poly[0] - linePtr->coordPtr[2];
        dy = poly[1] - linePtr->coordPtr[3];
        length = hypot(dx, dy);
        if (length == 0) {
            sinTheta = cosTheta = 0.0;
        }
        else {
            sinTheta = dy/length;
            cosTheta = dx/length;
        }
        vertX = poly[0] - shapeA*cosTheta;
        vertY = poly[1] - shapeA*sinTheta;
        temp = shapeC*sinTheta;
        poly[2] = poly[0] - shapeB*cosTheta + temp;
        poly[8] = poly[2] - 2*temp;
        temp = shapeC*cosTheta;
        poly[3] = poly[1] - shapeB*sinTheta - temp;
        poly[9] = poly[3] + 2*temp;
        poly[4] = poly[2]*fracHeight + vertX*(1.0-fracHeight);
        poly[5] = poly[3]*fracHeight + vertY*(1.0-fracHeight);
        poly[6] = poly[8]*fracHeight + vertX*(1.0-fracHeight);
        poly[7] = poly[9]*fracHeight + vertY*(1.0-fracHeight);

        /*
         * Polygon done.  Now move the first point towards the second so
         * that the corners at the end of the line are inside the
         * arrowhead.
         */

        linePtr->coordPtr[0] = poly[0] - backup*cosTheta;
        linePtr->coordPtr[1] = poly[1] - backup*sinTheta;
    }

    /*
     * Similar arrowhead calculation for the last point of the line.
     */

    if (linePtr->arrow != firstUid) {
        coordPtr = linePtr->coordPtr + 2*(linePtr->numPoints-2);
        poly = linePtr->lastArrowPtr;
        if (poly == NULL) {
            poly = (double *) ckalloc((unsigned)
                    (2*PTS_IN_ARROW*sizeof(double)));
            poly[0] = poly[10] = coordPtr[2];
            poly[1] = poly[11] = coordPtr[3];
            linePtr->lastArrowPtr = poly;
        }
        dx = poly[0] - coordPtr[0];
        dy = poly[1] - coordPtr[1];
        length = hypot(dx, dy);
        if (length == 0) {
            sinTheta = cosTheta = 0.0;
        }
        else {
            sinTheta = dy/length;
            cosTheta = dx/length;
        }
        vertX = poly[0] - shapeA*cosTheta;
        vertY = poly[1] - shapeA*sinTheta;
        temp = shapeC*sinTheta;
        poly[2] = poly[0] - shapeB*cosTheta + temp;
        poly[8] = poly[2] - 2*temp;
        temp = shapeC*cosTheta;
        poly[3] = poly[1] - shapeB*sinTheta - temp;
        poly[9] = poly[3] + 2*temp;
        poly[4] = poly[2]*fracHeight + vertX*(1.0-fracHeight);
        poly[5] = poly[3]*fracHeight + vertY*(1.0-fracHeight);
        poly[6] = poly[8]*fracHeight + vertX*(1.0-fracHeight);
        poly[7] = poly[9]*fracHeight + vertY*(1.0-fracHeight);
        coordPtr[2] = poly[0] - backup*cosTheta;
        coordPtr[3] = poly[1] - backup*sinTheta;
    }

    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * RtdLineToPostscript --
 *
 *      This procedure is called to generate Postscript for
 *      line items.
 *
 * Results:
 *      The return value is a standard Tcl result.  If an error
 *      occurs in generating Postscript then an error message is
 *      left in interp->result, replacing whatever used
 *      to be there.  If no error occurs, then Postscript for the
 *      item is appended to the result.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

int
RtdLineToPostscript( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
                     int prepass )
{
    PolyLineItem *linePtr = (PolyLineItem *) itemPtr;
    char buffer[200];
    char *style;
    int i;

    if ( linePtr->fg == NULL ) {
        return TCL_OK;
    }

    /*
     * Generate a path for the line's center-line (do this differently
     * for straight lines and smoothed lines).
     */

    if ((!linePtr->smooth) || (linePtr->numPoints <= 2)) {
        Tk_CanvasPsPath(interp, canvas, linePtr->coordPtr, linePtr->numPoints);
    }
    else {
        if (linePtr->fillStipple == None) {
            TkMakeBezierPostscript(interp, canvas, linePtr->coordPtr,
                    linePtr->numPoints);
        }
        else {
            /*
             * Special hack: Postscript printers don't appear to be able
             * to turn a path drawn with "curveto"s into a clipping path
             * without exceeding resource limits, so TkMakeBezierPostscript
             * won't work for stippled curves.  Instead, generate all of
             * the intermediate points here and output them into the
             * Postscript file with "lineto"s instead.
             */

            double staticPoints[2*MAX_STATIC_POINTS];
            double *pointPtr;
            int numPoints;

            numPoints = 1 + linePtr->numPoints*linePtr->splineSteps;
            pointPtr = staticPoints;
            if (numPoints > MAX_STATIC_POINTS) {
                pointPtr = (double *) ckalloc((unsigned)
                        (numPoints * 2 * sizeof(double)));
            }
            numPoints = TkMakeBezierCurve(canvas, linePtr->coordPtr,
                    linePtr->numPoints, linePtr->splineSteps, (XPoint *) NULL,
                    pointPtr);
            Tk_CanvasPsPath(interp, canvas, pointPtr, numPoints);
            if (pointPtr != staticPoints) {
                ckfree((char *) pointPtr);
            }
        }
    }

    /*
     * Set other line-drawing parameters and stroke out the line.
     */
    sprintf(buffer, "%d setlinewidth\n", linePtr->width);
    Tcl_AppendResult(interp, buffer, (char *) NULL);
    style = "0 setlinecap\n";
    if (linePtr->capStyle == CapRound) {
        style = "1 setlinecap\n";
    }
    else if (linePtr->capStyle == CapProjecting) {
        style = "2 setlinecap\n";
    }
    Tcl_AppendResult(interp, style, (char *) NULL);
    style = "0 setlinejoin\n";
    if (linePtr->joinStyle == JoinRound) {
        style = "1 setlinejoin\n";
    }
    else if (linePtr->joinStyle == JoinBevel) {
        style = "2 setlinejoin\n";
    }
    Tcl_AppendResult(interp, style, (char *) NULL);

    if ( linePtr->dash[0] != '\0' ) {
        char *str = buffer;
        char *ptr = linePtr->dash;
        str += sprintf( str, "[");
        i = strlen( linePtr->dash );
        while ( i-- > 1 ) {
            str += sprintf( str, "%d ", *ptr++ & 0xff );
        }
        str += sprintf(str, "%d] 0 setdash\n", *ptr++ & 0xff);
        Tcl_AppendResult( interp, buffer, (char *) NULL );
    }

    if (Tk_CanvasPsColor(interp, canvas, linePtr->fg) != TCL_OK) {
        return TCL_ERROR;
    };
    if (linePtr->fillStipple != None) {
        Tcl_AppendResult(interp, "StrokeClip ", (char *) NULL);
        if (Tk_CanvasPsStipple(interp, canvas, linePtr->fillStipple)
                != TCL_OK) {
            return TCL_ERROR;
        }
    }
    else {
        Tcl_AppendResult(interp, "stroke\n", (char *) NULL);
    }

    /*
     * Output polygons for the arrowheads, if there are any.
     */

    if (linePtr->firstArrowPtr != NULL) {
        if (linePtr->fillStipple != None) {
            Tcl_AppendResult(interp, "grestore gsave\n",
                    (char *) NULL);
        }
        if (ArrowheadPostscript(interp, canvas, linePtr,
                linePtr->firstArrowPtr) != TCL_OK) {
            return TCL_ERROR;
        }
    }
    if (linePtr->lastArrowPtr != NULL) {
        if (linePtr->fillStipple != None) {
            Tcl_AppendResult(interp, "grestore gsave\n", (char *) NULL);
        }
        if (ArrowheadPostscript(interp, canvas, linePtr,
                linePtr->lastArrowPtr) != TCL_OK) {
            return TCL_ERROR;
        }
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * ArrowheadPostscript --
 *
 *      This procedure is called to generate Postscript for
 *      an arrowhead for a line item.
 *
 * Results:
 *      The return value is a standard Tcl result.  If an error
 *      occurs in generating Postscript then an error message is
 *      left in interp->result, replacing whatever used
 *      to be there.  If no error occurs, then Postscript for the
 *      arrowhead is appended to the result.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static int
ArrowheadPostscript( Tcl_Interp *interp, Tk_Canvas canvas,
                     PolyLineItem *linePtr, double *arrowPtr )
{
    Tk_CanvasPsPath(interp, canvas, arrowPtr, PTS_IN_ARROW);
    /* Modified by P.W. Draper to add colour to arrowheads with
       stipple */

    if (Tk_CanvasPsColor(interp, canvas, linePtr->fg) != TCL_OK) {
        return TCL_ERROR;
    }
    if (linePtr->fillStipple != None) {
        Tcl_AppendResult(interp, "clip ", (char *) NULL);
        if (Tk_CanvasPsStipple(interp, canvas, linePtr->fillStipple)
                != TCL_OK) {
            return TCL_ERROR;
        }
    }
    else {
        Tcl_AppendResult(interp, "fill\n", (char *) NULL);
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * RtdLineSetLastCoords --
 *
 *      This procedure is called to reset the item coordinates,
 *      without the need to parse a string.
 *
 * Side effects:
 *      The coordinates for the given item will be changed.
 *
 * Notes:
 *      This procedure is non-standard. It provides a speedy way to
 *      set the coordinates of this item. The problems are that the
 *      canvas name and item are not available and are recorded from
 *      the last create command, so this should be called immediately
 *      after the create command and at no other time.
 *
 *--------------------------------------------------------------
 */
void RtdLineSetLastCoords( Tcl_Interp *interp, const double *x,
                           const double *y, int numPoints )
{
    RtdLineQuickSetCoords( interp, lastCanvas_, (Tk_Item *) lastItem_,
                           x, y, numPoints );
}

/*
 *--------------------------------------------------------------
 *
 * RtdQuickSetLineCoords --
 *
 *      This procedure is called to set the item coordinates,
 *      without the need to parse a string.
 *
 * Side effects:
 *      The coordinates for the given item will be changed.
 *
 * Notes:
 *      Version of RtdLineSetLastCoords, if you have the item
 *      and canvas (see Polyline_Create()).
 *
 *--------------------------------------------------------------
 */
void RtdLineQuickSetCoords( Tcl_Interp *interp, Tk_Canvas canvas,
                            Tk_Item *itemPtr, const double *x,
                            const double *y, int numPoints )
{
    PolyLineItem *linePtr = (PolyLineItem *)itemPtr;
    int i, j;

    if ( linePtr->numPoints != numPoints ) {
        if (linePtr->coordPtr != NULL) {
            ckfree((char *) linePtr->coordPtr);
        }
        linePtr->coordPtr = (double *)
            ckalloc((unsigned) (sizeof(double) * numPoints * 2 ));
        linePtr->numPoints = numPoints;
    }
    for ( i = 0, j = 0; j < numPoints; j++, i+=2 ) {
        linePtr->coordPtr[i]   = x[j];
        linePtr->coordPtr[i+1] = y[j];
    }

    /*
     * Update arrowheads by throwing away any existing arrow-head
     * information and calling ConfigureArrows to recompute it.
     */

    if (linePtr->firstArrowPtr != NULL) {
        ckfree((char *) linePtr->firstArrowPtr);
        linePtr->firstArrowPtr = NULL;
    }
    if (linePtr->lastArrowPtr != NULL) {
        ckfree((char *) linePtr->lastArrowPtr);
        linePtr->lastArrowPtr = NULL;
    }
    if (linePtr->arrow != noneUid) {
        ConfigureArrows(canvas, linePtr);
    }
    ComputeLineBbox(canvas, linePtr);

    /* Request canvas redraw */
    Tk_CanvasEventuallyRedraw( canvas,
                               linePtr->header.x1, linePtr->header.y1,
                               linePtr->header.x2, linePtr->header.y2 );
}

/*  Quick configuration routines. */

EXTERN void RtdLineSetColour( Tk_Window tkwin, Display *display,
                              Tk_Item *itemPtr, XColor *colour )
{
    XColor *tkColor;
    PolyLineItem *linePtr = (PolyLineItem *) itemPtr;

    /*  Look up the colour and register with Tk. */
    tkColor = Tk_GetColorByValue( tkwin, colour );

    /*  Release the existing colour. */
    if ( linePtr->fg != None ) {
        Tk_FreeColor( linePtr->fg );
    }

    linePtr->fg = tkColor;
    XSetForeground( display, linePtr->gc, linePtr->fg->pixel );
}

EXTERN void RtdLineSetWidth( Display *display, Tk_Item *itemPtr, int width )
{
    PolyLineItem *linePtr = (PolyLineItem *) itemPtr;

    linePtr->width = width;
    XSetLineAttributes( display, linePtr->gc, linePtr->width, LineSolid,
                        linePtr->capStyle, linePtr->joinStyle );
}

/*
 *--------------------------------------------------------------
 *
 * DashParseProc --
 *
 *      This procedure is called back during option parsing to
 *      parse the -dash option.
 *
 *--------------------------------------------------------------
 */
static int
DashParseProc( ClientData clientData, Tcl_Interp *interp, Tk_Window tkwin,
               char *value, char *recordPtr, int offset )
{
    PolyLineItem *linePtr = (PolyLineItem *) recordPtr;
    char **argv = NULL;
    int a;
    int argc;
    int i;

    if ( offset != Tk_Offset( PolyLineItem, dash ) ) {
        Tcl_Panic( "ParseDashProc received bogus offset" );
    }

    /* -dash is a list of integers which we code to encode into
     * a character array. Allow an empty string to clear value.
     */
    if ( value[0] == '\0' ) {
        linePtr->dash[0] = '\0';
        return TCL_OK;
    }
    if ( Tcl_SplitList( interp, value, &argc, &argv ) == TCL_OK && argc > 0 ) {
        for ( i = 0; i < argc; i++ ) {
            if ( Tcl_GetInt( interp, argv[i], &a ) != TCL_OK ) {
                ckfree( (char *) argv );
                return TCL_ERROR;
            }
            linePtr->dash[i] = (char) a;
        }
        linePtr->dash[argc] = '\0';
        ckfree( (char *) argv );
        return TCL_OK;
    }

    Tcl_ResetResult( interp );
    Tcl_AppendResult( interp, "bad dash value \"", value, (char *) NULL);
    if ( argv != NULL ) {
        ckfree( (char *) argv );
    }
    return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * DashPrintProc --
 *
 *      This procedure is a callback invoked by the configuration
 *      code to return a printable value describing a dash.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */
static char *
DashPrintProc( ClientData clientData, Tk_Window tkwin, char *recordPtr,
               int offset, Tcl_FreeProc **freeProcPtr )
{
    PolyLineItem *linePtr = (PolyLineItem *) recordPtr;
    char *buffer;
    char *ptr1;
    char *ptr2;

    ptr1 = linePtr->dash;
    ptr2 = buffer = ckalloc( 120 );
    *ptr2 = '\0';

    while ( *ptr1 ) {
        ptr2 += sprintf( ptr2, "%d ", *ptr1 );
        ptr1++;
    }

    *freeProcPtr = TCL_DYNAMIC;
    return buffer;
}
