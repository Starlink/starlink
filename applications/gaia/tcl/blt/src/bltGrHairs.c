
/*
 * bltGrHairs.c --
 *
 *	This module implements crosshairs for the BLT graph widget.
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
 *
 * Graph widget created by Sani Nassif and George Howlett.
*/

#include "bltGraph.h"

extern Tk_CustomOption bltPositionOption;
extern Tk_CustomOption bltLengthOption;
extern Tk_CustomOption bltDashesOption;

typedef enum CrosshairsState {
    HAIRS_OFF, HAIRS_ON
} CrosshairsState;

/*
 * -------------------------------------------------------------------
 *
 * Crosshairs
 *
 *	Contains the line segments positions and graphics context used
 *	to simulate crosshairs (by XORing) on the graph.
 *
 * -------------------------------------------------------------------
 */

struct Crosshairs {

    XPoint hotSpot;		/* Hot spot for crosshairs */
    CrosshairsState state;	/* Internal state of crosshairs. If non-zero,
				 * crosshairs are displayed. */
    int hidden;			/* If non-zero, crosshairs are not displayed.
				 * This is not necessarily consistent with the
				 * internal state variable.  This is true when
				 * the hot spot is off the graph.  */
    Dashes dashes;		/* Dashstyle of the crosshairs. This represents
				 * an array of alternatingly drawn pixel
				 * values. If NULL, the hairs are drawn as a
				 * solid line */
    int lineWidth;		/* Width of the simulated crosshair lines */
    XSegment segArr[2];		/* Positions of line segments representing the
				 * simulated crosshairs. */
    XColor *colorPtr;		/* Foreground color of crosshairs */
    GC gc;			/* Graphics context for crosshairs. Set to
				 * GXxor to not require redraws of graph */
};

#define DEF_HAIRS_DASHES	(char *)NULL
#define DEF_HAIRS_FG_COLOR	RGB_COLOR_BLACK
#define DEF_HAIRS_FG_MONO	RGB_COLOR_BLACK
#define DEF_HAIRS_LINE_WIDTH	"0"
#define DEF_HAIRS_HIDE		"yes"
#define DEF_HAIRS_POSITION	(char *)NULL

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_COLOR, "-color", "color", "Color",
	DEF_HAIRS_FG_COLOR, Tk_Offset(Crosshairs, colorPtr), TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-color", "color", "Color",
	DEF_HAIRS_FG_MONO, Tk_Offset(Crosshairs, colorPtr), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_CUSTOM, "-dashes", "dashes", "Dashes",
	DEF_HAIRS_DASHES, Tk_Offset(Crosshairs, dashes),
	TK_CONFIG_NULL_OK, &bltDashesOption},
    {TK_CONFIG_BOOLEAN, "-hide", "hide", "Hide",
	DEF_HAIRS_HIDE, Tk_Offset(Crosshairs, hidden),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-linewidth", "lineWidth", "Linewidth",
	DEF_HAIRS_LINE_WIDTH, Tk_Offset(Crosshairs, lineWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-position", "position", "Position",
	DEF_HAIRS_POSITION, Tk_Offset(Crosshairs, hotSpot),
	0, &bltPositionOption},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

/*
 *----------------------------------------------------------------------
 *
 * TurnOffHairs --
 *
 *	XOR's the existing line segments (representing the crosshairs),
 *	thereby erasing them.  The internal state of the crosshairs is
 *	tracked.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Crosshairs are erased.
 *
 *----------------------------------------------------------------------
 */
static void
TurnOffHairs(tkwin, hairsPtr)
    Tk_Window tkwin;
    Crosshairs *hairsPtr;
{
    if (Tk_IsMapped(tkwin) && (hairsPtr->state == HAIRS_ON)) {
	XDrawSegments(Tk_Display(tkwin), Tk_WindowId(tkwin), hairsPtr->gc,
	    hairsPtr->segArr, 2);
	hairsPtr->state = HAIRS_OFF;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TurnOnHairs --
 *
 *	Draws (by XORing) new line segments, creating the effect of
 *	crosshairs. The internal state of the crosshairs is tracked.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Crosshairs are displayed.
 *
 *----------------------------------------------------------------------
 */
static void
TurnOnHairs(graphPtr, hairsPtr)
    Graph *graphPtr;
    Crosshairs *hairsPtr;
{
    if (Tk_IsMapped(graphPtr->tkwin) && (hairsPtr->state == HAIRS_OFF)) {
	if (!PointInGraph(graphPtr, hairsPtr->hotSpot.x, hairsPtr->hotSpot.y)) {
	    return;		/* Coordinates are off the graph */
	}
	XDrawSegments(graphPtr->display, Tk_WindowId(graphPtr->tkwin),
	    hairsPtr->gc, hairsPtr->segArr, 2);
	hairsPtr->state = HAIRS_ON;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureCrosshairs --
 *
 *	Configures attributes of the crosshairs such as line width,
 *	dashes, and position.  The crosshairs are first turned off
 *	before any of the attributes changes.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Crosshair GC is allocated.
 *
 *----------------------------------------------------------------------
 */
void
Blt_ConfigureCrosshairs(graphPtr)
    Graph *graphPtr;
{
    XGCValues gcValues;
    unsigned long gcMask;
    GC newGC;
    long colorValue;
    Crosshairs *hairsPtr = (Crosshairs *)graphPtr->crosshairs;

    /*
     * Turn off the crosshairs temporarily. This is in case the new
     * configuration changes the size, style, or position of the lines.
     */
    TurnOffHairs(graphPtr->tkwin, hairsPtr);

    gcValues.function = GXxor;

    if (graphPtr->plotBg == NULL) {
	/* The graph's color option may not have been set yet */
	colorValue = WhitePixelOfScreen(Tk_Screen(graphPtr->tkwin));
    } else {
	colorValue = graphPtr->plotBg->pixel;
    }
    gcValues.background = colorValue;
    gcValues.foreground = (colorValue ^ hairsPtr->colorPtr->pixel);

    gcValues.line_width = LineWidth(hairsPtr->lineWidth);
    gcMask = (GCForeground | GCBackground | GCFunction | GCLineWidth);
    if (hairsPtr->dashes.numValues > 0) {
	gcValues.line_style = LineOnOffDash;
	gcMask |= GCLineStyle;
    }
    newGC = Blt_GetPrivateGC(graphPtr->tkwin, gcMask, &gcValues);
    if (hairsPtr->dashes.numValues > 0) {
	Blt_SetDashes(graphPtr->display, newGC, &(hairsPtr->dashes));
    }
    if (hairsPtr->gc != NULL) {
	Blt_FreePrivateGC(graphPtr->display, hairsPtr->gc);
    }
    hairsPtr->gc = newGC;

    /*
     * Are the new coordinates on the graph?
     */
    hairsPtr->segArr[0].x2 = hairsPtr->segArr[0].x1 = hairsPtr->hotSpot.x;
    hairsPtr->segArr[0].y1 = graphPtr->yMax;
    hairsPtr->segArr[0].y2 = graphPtr->yMin;
    hairsPtr->segArr[1].y2 = hairsPtr->segArr[1].y1 = hairsPtr->hotSpot.y;
    hairsPtr->segArr[1].x1 = graphPtr->xMin;
    hairsPtr->segArr[1].x2 = graphPtr->xMax;

    if (!hairsPtr->hidden) {
	TurnOnHairs(graphPtr, hairsPtr);
    }
}

void
Blt_EnableCrosshairs(graphPtr)
    Graph *graphPtr;
{
    if (!graphPtr->crosshairs->hidden) {
	TurnOnHairs(graphPtr, graphPtr->crosshairs);
    }
}

void
Blt_DisableCrosshairs(graphPtr)
    Graph *graphPtr;
{
    TurnOffHairs(graphPtr->tkwin, graphPtr->crosshairs);
}

/*
 *----------------------------------------------------------------------
 *
 * UpdateCrosshairs --
 *
 *	Update the length of the hairs (not the hot spot).
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_UpdateCrosshairs(graphPtr)
    Graph *graphPtr;
{
    Crosshairs *hairsPtr = (Crosshairs *)graphPtr->crosshairs;

    hairsPtr->segArr[0].y1 = graphPtr->yMax;
    hairsPtr->segArr[0].y2 = graphPtr->yMin;
    hairsPtr->segArr[1].x1 = graphPtr->xMin;
    hairsPtr->segArr[1].x2 = graphPtr->xMax;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_DestroyCrosshairs --
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Crosshair GC is allocated.
 *
 *----------------------------------------------------------------------
 */
void
Blt_DestroyCrosshairs(graphPtr)
    Graph *graphPtr;
{
    Crosshairs *hairsPtr = (Crosshairs *)graphPtr->crosshairs;

    Tk_FreeOptions(configSpecs, (char *)hairsPtr, graphPtr->display, 0);
    if (hairsPtr->gc != NULL) {
	Blt_FreePrivateGC(graphPtr->display, hairsPtr->gc);
    }
    free((char *)hairsPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_CreateCrosshairs --
 *
 *	Creates and initializes a new crosshair structure.
 *
 * Results:
 *	Returns TCL_ERROR if the crosshair structure cannot be created,
 *	otherwise TCL_OK.
 *
 * Side Effects:
 *	Crosshair GC is allocated.
 *
 *----------------------------------------------------------------------
 */
int
Blt_CreateCrosshairs(graphPtr)
    Graph *graphPtr;
{
    Crosshairs *hairsPtr;

    hairsPtr = (Crosshairs *)calloc(1, sizeof(Crosshairs));
    assert(hairsPtr);
    hairsPtr->hidden = TRUE;
    hairsPtr->state = HAIRS_OFF;
    hairsPtr->hotSpot.x = hairsPtr->hotSpot.y = -1;
    graphPtr->crosshairs = hairsPtr;

    if (Blt_ConfigureWidgetComponent(graphPtr->interp, graphPtr->tkwin,
	    "crosshairs", "Crosshairs", configSpecs, 0, (char **)NULL,
	    (char *)hairsPtr, 0) != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * CgetOp --
 *
 *	Queries configuration attributes of the crosshairs such as
 *	line width, dashes, and position.
 *
 * Results:
 *	A standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
/* ARGSUSED */
static int
CgetOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;			/* unused */
    char **argv;
{
    Crosshairs *hairsPtr = (Crosshairs *)graphPtr->crosshairs;

    return (Tk_ConfigureValue(interp, graphPtr->tkwin, configSpecs,
	    (char *)hairsPtr, argv[3], 0));
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureOp --
 *
 *	Queries or resets configuration attributes of the crosshairs
 * 	such as line width, dashes, and position.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side Effects:
 *	Crosshairs are reset.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Crosshairs *hairsPtr = (Crosshairs *)graphPtr->crosshairs;

    if (argc == 3) {
	return (Tk_ConfigureInfo(interp, graphPtr->tkwin, configSpecs,
		(char *)hairsPtr, (char *)NULL, 0));
    } else if (argc == 4) {
	return (Tk_ConfigureInfo(interp, graphPtr->tkwin, configSpecs,
		(char *)hairsPtr, argv[3], 0));
    }
    if (Tk_ConfigureWidget(interp, graphPtr->tkwin, configSpecs, argc - 3,
	    argv + 3, (char *)hairsPtr, TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	return TCL_ERROR;
    }
    Blt_ConfigureCrosshairs(graphPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * MapOp --
 *
 *	Maps the crosshairs.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side Effects:
 *	Crosshairs are reset if necessary.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
MapOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Crosshairs *hairsPtr = (Crosshairs *)graphPtr->crosshairs;

    if (hairsPtr->hidden) {
	TurnOnHairs(graphPtr, hairsPtr);
	hairsPtr->hidden = FALSE;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * UnmapOp --
 *
 *	Unmaps the crosshairs.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side Effects:
 *	Crosshairs are reset if necessary.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
UnmapOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Crosshairs *hairsPtr = (Crosshairs *)graphPtr->crosshairs;

    if (!hairsPtr->hidden) {
	TurnOffHairs(graphPtr->tkwin, hairsPtr);
	hairsPtr->hidden = TRUE;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ToggleOp --
 *
 *	Toggles the state of the crosshairs.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side Effects:
 *	Crosshairs are reset.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ToggleOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Crosshairs *hairsPtr = (Crosshairs *)graphPtr->crosshairs;

    hairsPtr->hidden = (hairsPtr->hidden == 0);
    if (hairsPtr->hidden) {
	TurnOffHairs(graphPtr->tkwin, hairsPtr);
    } else {
	TurnOnHairs(graphPtr, hairsPtr);
    }
    return TCL_OK;
}


static Blt_OpSpec xhairOps[] =
{
    {"cget", 2, (Blt_Operation)CgetOp, 4, 4, "option",},
    {"configure", 2, (Blt_Operation)ConfigureOp, 3, 0, "?options...?",},
    {"off", 2, (Blt_Operation)UnmapOp, 3, 3, "",},
    {"on", 2, (Blt_Operation)MapOp, 3, 3, "",},
    {"toggle", 1, (Blt_Operation)ToggleOp, 3, 3, "",},
};
static int numXhairOps = sizeof(xhairOps) / sizeof(Blt_OpSpec);

/*
 *----------------------------------------------------------------------
 *
 * Blt_CrosshairsOp --
 *
 *	User routine to configure crosshair simulation.  Crosshairs
 *	are simulated by drawing line segments parallel to both axes
 *	using the XOR drawing function. The allows the lines to be
 *	erased (by drawing them again) without redrawing the entire
 *	graph.  Care must be taken to erase crosshairs before redrawing
 *	the graph and redraw them after the graph is redraw.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 * Side Effects:
 *	Crosshairs may be drawn in the plotting area.
 *
 *----------------------------------------------------------------------
 */
int
Blt_CrosshairsOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Blt_Operation proc;

    proc = Blt_GetOperation(interp, numXhairOps, xhairOps, BLT_OPER_ARG2,
	argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    return ((*proc) (graphPtr, interp, argc, argv));
}
