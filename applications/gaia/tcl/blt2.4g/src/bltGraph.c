
/*
 * bltGraph.c --
 *
 *	This module implements a graph widget for the Tk toolkit.
 *
 * Copyright 1991-1998 Lucent Technologies, Inc.
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

/*
 * To do:
 *
 * 2) Update manual pages.
 *
 * 3) Update comments.
 *
 * 5) Surface, contour, and flow graphs
 *
 * 7) Arrows for line markers
 *
 */

#include "bltGraph.h"
#include "bltGrElem.h"
#include <X11/Xutil.h>
#include <X11/Xatom.h>

static char *classNames[] =
{
    "Graph", "Stripchart", "Barchart", (char *)NULL
};

static Tcl_HashTable graphTable;
static int initialized = 0;

extern Tk_CustomOption bltLengthOption;
extern Tk_CustomOption bltBarModeOption;
extern Tk_CustomOption bltPadOption;
extern Tk_CustomOption bltTileOption;
extern Tk_CustomOption bltShadowOption;

#define DEF_GRAPH_ASPECT_RATIO	"0.0"
#define DEF_GRAPH_BAR_BASELINE	"0.0"
#define DEF_GRAPH_BAR_MODE	"normal"
#define DEF_GRAPH_BAR_WIDTH	"0.8"
#define DEF_GRAPH_BG_COLOR	STD_COLOR_NORMAL_BG
#define DEF_GRAPH_BG_MONO	STD_MONO_NORMAL_BG
#define DEF_GRAPH_BORDER_WIDTH	STD_BORDERWIDTH
#define DEF_GRAPH_BUFFERED	"1"
#define DEF_GRAPH_CURSOR  	"crosshair"
#define DEF_GRAPH_FONT		STD_FONT_LARGE
#define DEF_GRAPH_HALO		"2m"
#define DEF_GRAPH_HALO_BAR	"0.1i"
#define DEF_GRAPH_HEIGHT	"4i"
#define DEF_GRAPH_INVERT_XY	"0"
#define DEF_GRAPH_JUSTIFY	"center"
#define DEF_GRAPH_MARGIN	"0"
#define DEF_GRAPH_MARGIN_VAR	(char *)NULL
#define DEF_GRAPH_PLOT_BG_COLOR	RGB_COLOR_WHITE
#define DEF_GRAPH_PLOT_BG_MONO	RGB_COLOR_WHITE
#define DEF_GRAPH_PLOT_BW_COLOR STD_BORDERWIDTH
#define DEF_GRAPH_PLOT_BW_MONO  "0"
#define DEF_GRAPH_PLOT_PADX	"8"
#define DEF_GRAPH_PLOT_PADY	"8"
#define DEF_GRAPH_PLOT_RELIEF	"sunken"
#define DEF_GRAPH_RELIEF	"flat"
#define DEF_GRAPH_SHADOW_COLOR	(char *)NULL
#define DEF_GRAPH_SHADOW_MONO	(char *)NULL
#define DEF_GRAPH_TAKE_FOCUS	(char *)NULL
#define DEF_GRAPH_TITLE		"Graph Title"
#define DEF_GRAPH_TITLE_COLOR	STD_COLOR_NORMAL_FG
#define DEF_GRAPH_TITLE_MONO	STD_MONO_NORMAL_FG
#define DEF_GRAPH_WIDTH		"5i"

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_DOUBLE, "-aspect", "aspect", "Aspect",
	DEF_GRAPH_ASPECT_RATIO, Tk_Offset(Graph, aspectRatio),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_GRAPH_BG_COLOR, Tk_Offset(Graph, border),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
	DEF_GRAPH_BG_MONO, Tk_Offset(Graph, border),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_CUSTOM, "-barmode", "barMode", "BarMode",
	DEF_GRAPH_BAR_MODE, Tk_Offset(Graph, mode),
	TK_CONFIG_DONT_SET_DEFAULT, &bltBarModeOption},
    {TK_CONFIG_DOUBLE, "-barwidth", "barWidth", "BarWidth",
	DEF_GRAPH_BAR_WIDTH, Tk_Offset(Graph, barWidth), 0},
    {TK_CONFIG_DOUBLE, "-baseline", "baseline", "Baseline",
	DEF_GRAPH_BAR_BASELINE, Tk_Offset(Graph, baseline), 0},
    {TK_CONFIG_SYNONYM, "-bd", "borderWidth", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_SYNONYM, "-bg", "background", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_SYNONYM, "-bm", "bottomMargin",
	(char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_CUSTOM, "-borderwidth", "borderWidth", "BorderWidth",
	DEF_GRAPH_BORDER_WIDTH, Tk_Offset(Graph, borderWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-bottommargin", "bottomMargin", "Margin",
	DEF_GRAPH_MARGIN, Tk_Offset(Graph, reqBottomMargin), 0,
	&bltLengthOption},
    {TK_CONFIG_STRING, "-bottomvariable", "bottomVariable", "BottomVariable",
	DEF_GRAPH_MARGIN_VAR, Tk_Offset(Graph, bottomVar), TK_CONFIG_NULL_OK},
    {TK_CONFIG_BOOLEAN, "-bufferelements", "bufferElements", "BufferElements",
	DEF_GRAPH_BUFFERED, Tk_Offset(Graph, backingStore),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_ACTIVE_CURSOR, "-cursor", "cursor", "Cursor",
	DEF_GRAPH_CURSOR, Tk_Offset(Graph, cursor), TK_CONFIG_NULL_OK},
    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_FONT, "-font", "font", "Font",
	DEF_GRAPH_FONT, Tk_Offset(Graph, titleAttr.font), 0},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_GRAPH_TITLE_COLOR, Tk_Offset(Graph, titleAttr.color), TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_GRAPH_TITLE_MONO, Tk_Offset(Graph, titleAttr.color), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_CUSTOM, "-halo", "halo", "Halo",
	DEF_GRAPH_HALO, Tk_Offset(Graph, halo), 0, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-height", "height", "Height",
	DEF_GRAPH_HEIGHT, Tk_Offset(Graph, reqHeight), 0, &bltLengthOption},
    {TK_CONFIG_BOOLEAN, "-invertxy", "invertXY", "InvertXY",
	DEF_GRAPH_INVERT_XY, Tk_Offset(Graph, inverted),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_JUSTIFY, "-justify", "justify", "Justify",
	DEF_GRAPH_JUSTIFY, Tk_Offset(Graph, titleAttr.justify),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-leftmargin", "leftMargin", "Margin",
	DEF_GRAPH_MARGIN, Tk_Offset(Graph, reqLeftMargin),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_STRING, "-leftvariable", "leftVariable", "LeftVariable",
	DEF_GRAPH_MARGIN_VAR, Tk_Offset(Graph, leftVar), TK_CONFIG_NULL_OK},
    {TK_CONFIG_SYNONYM, "-lm", "leftMargin", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_COLOR, "-plotbackground", "plotBackground", "Background",
	DEF_GRAPH_PLOT_BG_MONO, Tk_Offset(Graph, plotBg),
	TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_COLOR, "-plotbackground", "plotBackground", "Background",
	DEF_GRAPH_PLOT_BG_COLOR, Tk_Offset(Graph, plotBg),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_CUSTOM, "-plotborderwidth", "plotBorderWidth", "BorderWidth",
	DEF_GRAPH_PLOT_BW_COLOR, Tk_Offset(Graph, plotBW),
	TK_CONFIG_COLOR_ONLY, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-plotborderwidth", "plotBorderWidth", "BorderWidth",
	DEF_GRAPH_PLOT_BW_MONO, Tk_Offset(Graph, plotBW),
	TK_CONFIG_MONO_ONLY, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-plotpadx", "plotPadX", "PlotPad",
	DEF_GRAPH_PLOT_PADX, Tk_Offset(Graph, padX),
	TK_CONFIG_DONT_SET_DEFAULT, &bltPadOption},
    {TK_CONFIG_CUSTOM, "-plotpady", "plotPadY", "PlotPad",
	DEF_GRAPH_PLOT_PADY, Tk_Offset(Graph, padY),
	TK_CONFIG_DONT_SET_DEFAULT, &bltPadOption},
    {TK_CONFIG_RELIEF, "-plotrelief", "plotRelief", "Relief",
	DEF_GRAPH_PLOT_RELIEF, Tk_Offset(Graph, plotRelief),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_RELIEF, "-relief", "relief", "Relief",
	DEF_GRAPH_RELIEF, Tk_Offset(Graph, relief), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-rightmargin", "rightMargin", "Margin",
	DEF_GRAPH_MARGIN, Tk_Offset(Graph, reqRightMargin),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_STRING, "-rightvariable", "rightVariable", "RightVariable",
	DEF_GRAPH_MARGIN_VAR, Tk_Offset(Graph, rightVar), TK_CONFIG_NULL_OK},
    {TK_CONFIG_SYNONYM, "-rm", "rightMargin", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_CUSTOM, "-shadow", "shadow", "Shadow",
	DEF_GRAPH_SHADOW_COLOR, Tk_Offset(Graph, titleAttr.shadow),
	TK_CONFIG_COLOR_ONLY, &bltShadowOption},
    {TK_CONFIG_CUSTOM, "-shadow", "shadow", "Shadow",
	DEF_GRAPH_SHADOW_MONO, Tk_Offset(Graph, titleAttr.shadow),
	TK_CONFIG_MONO_ONLY, &bltShadowOption},
    {TK_CONFIG_COLOR, "-foreground", "foreground", "Foreground",
	DEF_GRAPH_TITLE_MONO, Tk_Offset(Graph, titleAttr.color), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_STRING, "-takefocus", "takeFocus", "TakeFocus",
	DEF_GRAPH_TAKE_FOCUS, Tk_Offset(Graph, takeFocus), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-tile", "tile", "Tile",
	(char *)NULL, Tk_Offset(Graph, tile), TK_CONFIG_NULL_OK, &bltTileOption},
    {TK_CONFIG_STRING, "-title", "title", "Title",
	DEF_GRAPH_TITLE, Tk_Offset(Graph, titleText), TK_CONFIG_NULL_OK},
    {TK_CONFIG_SYNONYM, "-tm", "topMargin", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_CUSTOM, "-topmargin", "topMargin", "Margin",
	DEF_GRAPH_MARGIN, Tk_Offset(Graph, reqTopMargin),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_STRING, "-topvariable", "topVariable", "TopVariable",
	DEF_GRAPH_MARGIN_VAR, Tk_Offset(Graph, topVar), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-width", "width", "Width",
	DEF_GRAPH_WIDTH, Tk_Offset(Graph, reqWidth),
	0, &bltLengthOption},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

double bltPosInfinity = (double)DBL_MAX;
double bltNegInfinity = (double)-(DBL_MAX);

static void DisplayGraph _ANSI_ARGS_((ClientData clientData));
static void DestroyGraph _ANSI_ARGS_((DestroyData dataPtr));
static void GraphEventProc _ANSI_ARGS_((ClientData clientData,
	XEvent *eventPtr));
static int GraphWidgetCmd _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, int argc, char **argv));

#ifdef __STDC__
static BindPickProc PickEntry;
static Tcl_CmdProc StripchartCmd;
static Tcl_CmdProc BarchartCmd;
static Tcl_CmdProc GraphCmd;
static Tcl_CmdDeleteProc GraphDeleteCmdProc;
static Blt_TileChangedProc TileChangedProc;
static Tcl_CmdProc GraphWidgetCmd;
#endif

/*
 *--------------------------------------------------------------
 *
 * Blt_EventuallyRedrawGraph --
 *
 *	Tell the Tk dispatcher to call the graph display routine at
 *	the next idle point.  This request is made only if the window
 *	is displayed and no other redraw request is pending.
 *
 * Results: None.
 *
 * Side effects:
 *	The window is eventually redisplayed.
 *
 *--------------------------------------------------------------
 */
void
Blt_EventuallyRedrawGraph(graphPtr)
    Graph *graphPtr;		/* Graph widget record */
{
    if ((graphPtr->tkwin != NULL) && !(graphPtr->flags & REDRAW_PENDING)) {
	Tk_DoWhenIdle(DisplayGraph, (ClientData)graphPtr);
	graphPtr->flags |= REDRAW_PENDING;
    }
}

/*
 *--------------------------------------------------------------
 *
 * GraphEventProc --
 *
 *	This procedure is invoked by the Tk dispatcher for various
 *	events on graphs.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	When the window gets deleted, internal structures get
 *	cleaned up.  When it gets exposed, the graph is eventually
 *	redisplayed.
 *
 *--------------------------------------------------------------
 */
static void
GraphEventProc(clientData, eventPtr)
    ClientData clientData;	/* Graph widget record */
    register XEvent *eventPtr;	/* Event which triggered call to routine */
{
    Graph *graphPtr = (Graph *)clientData;

    if (eventPtr->type == Expose) {
	if (eventPtr->xexpose.count == 0) {
	    graphPtr->flags |= REDRAW_WORLD;
	    Blt_EventuallyRedrawGraph(graphPtr);
	}
    } else if (eventPtr->type == DestroyNotify) {
	if (graphPtr->tkwin != NULL) {
	    char *cmdName;

	    cmdName = Tcl_GetCommandName(graphPtr->interp, graphPtr->cmdToken);
#ifdef ITCL_NAMESPACES
	    Itk_SetWidgetCommand(graphPtr->tkwin, (Tcl_Command) NULL);
#endif /* ITCL_NAMESPACES */
	    graphPtr->tkwin = NULL;
	    Tcl_DeleteCommand(graphPtr->interp, cmdName);
	}
	if (graphPtr->flags & REDRAW_PENDING) {
	    Tk_CancelIdleCall(DisplayGraph, (ClientData)graphPtr);
	}
	Tk_EventuallyFree((ClientData)graphPtr, DestroyGraph);
    } else if (eventPtr->type == ConfigureNotify) {
	graphPtr->flags |= (COORDS_WORLD | REDRAW_WORLD);
	Blt_EventuallyRedrawGraph(graphPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * GraphDeleteCmdProc --
 *
 *	This procedure is invoked when a widget command is deleted.  If
 *	the widget isn't already in the process of being destroyed,
 *	this command destroys it.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The widget is destroyed.
 *
 *---------------------------------------------------------------------- */
static void
GraphDeleteCmdProc(clientData)
    ClientData clientData;	/* Pointer to widget record. */
{
    Graph *graphPtr = (Graph *)clientData;

    if (graphPtr->tkwin != NULL) {	/* NULL indicates window has
				    * already been destroyed. */
	Tk_Window tkwin;

	tkwin = graphPtr->tkwin;
	graphPtr->tkwin = NULL;
#ifdef ITCL_NAMESPACES
	Itk_SetWidgetCommand(tkwin, (Tcl_Command) NULL);
#endif /* ITCL_NAMESPACES */
	Tk_DestroyWindow(tkwin);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TileChangedProc
 *
 *	Rebuilds the designated GC with the new tile pixmap.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
TileChangedProc(clientData, tile)
    ClientData clientData;
    Blt_Tile tile;
{
    Graph *graphPtr = (Graph *)clientData;

    if (graphPtr->tkwin != NULL) {
	unsigned long gcMask;
	XGCValues gcValues;
	Pixmap pixmap;
	GC newGC;

	gcMask = (GCForeground | GCBackground);
	gcValues.foreground = Tk_3DBorderColor(graphPtr->border)->pixel;
	gcValues.background = graphPtr->titleAttr.color->pixel;
	pixmap = Blt_PixmapOfTile(tile);
	if (pixmap != None) {
	    gcMask |= (GCTile | GCFillStyle);
	    gcValues.fill_style = FillTiled;
	    gcValues.tile = pixmap;
	}
	newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
	if (graphPtr->fillGC != NULL) {
	    Tk_FreeGC(graphPtr->display, graphPtr->fillGC);
	}
	graphPtr->fillGC = newGC;
	graphPtr->flags |= REDRAW_WORLD;
	Blt_EventuallyRedrawGraph(graphPtr);
    }
}

/*
 *--------------------------------------------------------------
 *
 * Blt_AdjustAxisPointers --
 *
 *	Sets the axis pointers according to whether the axis is
 *	inverted on not.  The axis sites are also reset.
 *
 * Results:
 *	None.
 *
 *--------------------------------------------------------------
 */

void
Blt_AdjustAxisPointers(graphPtr)
    Graph *graphPtr;		/* Graph widget record */
{
    if (graphPtr->inverted) {
	graphPtr->leftAxis = &(graphPtr->axisArr[0]);
	graphPtr->bottomAxis = &(graphPtr->axisArr[1]);
	graphPtr->rightAxis = &(graphPtr->axisArr[2]);
	graphPtr->topAxis = &(graphPtr->axisArr[3]);
    } else {
	graphPtr->leftAxis = &(graphPtr->axisArr[1]);
	graphPtr->bottomAxis = &(graphPtr->axisArr[0]);
	graphPtr->rightAxis = &(graphPtr->axisArr[3]);
	graphPtr->topAxis = &(graphPtr->axisArr[2]);
    }
    graphPtr->bottomAxis->virtAxisPtr->site = AXIS_BOTTOM;
    graphPtr->leftAxis->virtAxisPtr->site = AXIS_LEFT;
    graphPtr->topAxis->virtAxisPtr->site = AXIS_TOP;
    graphPtr->rightAxis->virtAxisPtr->site = AXIS_RIGHT;
}

static int
InitPens(graphPtr)
    Graph *graphPtr;
{
    Pen *penPtr;

    Tcl_InitHashTable(&(graphPtr->penTable), TCL_STRING_KEYS);
    penPtr = Blt_CreatePen(graphPtr, "activeLine", TYPE_ELEM_LINE, 0,
	(char **)NULL);
    if (penPtr == NULL) {
	return TCL_ERROR;
    }
    penPtr = Blt_CreatePen(graphPtr, "activeBar", TYPE_ELEM_BAR, 0,
	(char **)NULL);
    if (penPtr == NULL) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_GraphTags --
 *
 *	Sets the binding tags for a graph object. This routine is
 *	called by Tk when an event occurs in the graph.  It fills
 *	an array of pointers with bind tag addresses.
 *
 *	The addresses are hashed strings in one of two tag tables: one
 *	for elements and the other for markers.  There's only one
 *	binding table for elements and markers.  This is so that when
 *	an event occurs, we can't trigger both a marker and element
 *	bind command.  But this also means they share tags and if they
 *	have the same tag, they can both be picked for the same type
 *	of event.  Which is not what we want. A tag of "all" for
 *	markers should mean all markers, not all markers and
 *	elements. So we keep separate hash tables for element and
 *	marker tags, which means we can't generate the same tag for
 *	both elements and markers, even if they have the same name.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	This information will be used by the binding code in bltUtil.c
 *	to determine what graph objects match the current event.  The
 *	tags are placed in tagArr and *numTagsPtr is set with the
 *	number of tags found.
 *
 *----------------------------------------------------------------------
 */
void
Blt_GraphTags(table, object, tagArr, numTagsPtr)
    BindTable table;
    ClientData object;
    ClientData tagArr[];
    int *numTagsPtr;
{
    Element *elemPtr;
    int numTags;
    MakeTagProc *tagProc;
    Graph *graphPtr;

    graphPtr = (Graph *)Blt_GetBindingData(table);
    /*
     * Trick:  Both markers and elements have the same first few fields in their
     *	      structures, such as "type", "name", or "tags".  This is so we can
     *	      look at both graph objects interchangably.  Therefore it doesn't
     *	      matter if we cast the object to either a marker or element.
     */
    elemPtr = (Element *)object;

    if ((elemPtr->type >= TYPE_ELEM_LINE) && (elemPtr->type <= TYPE_ELEM_BAR)) {
	tagProc = Blt_MakeElementTag;
    } else {
	tagProc = Blt_MakeMarkerTag;
    }
    /*
     * Always add the name of the object to the tag array.
     */
    tagArr[0] = (*tagProc) (graphPtr, elemPtr->name);
    numTags = 1;
    if (elemPtr->tags != NULL) {
	register char **p;
	register int i;

	p = elemPtr->tags;
	for (i = numTags; (*p != NULL) && (i < 10); p++, i++) {
	    tagArr[i] = (*tagProc) (graphPtr, *p);
	}
	numTags = i;
    }
    *numTagsPtr = numTags;
}

/*
 * Find the closest point from the set of displayed elements,
 * searching the display list from back to front.  That way, if
 * the points from two different elements overlay each other exactly,
 * the one that's on top (visible) is picked.
 */
static ClientData
PickEntry(clientData, x, y)
    ClientData clientData;
    int x, y;
{
    Graph *graphPtr = (Graph *)clientData;
    register Blt_ListItem item;
    ClosestSearch search;
    Element *elemPtr;
    Marker *markerPtr;

    markerPtr = (Marker *)Blt_NearestMarker(graphPtr, x, y, FALSE);
    if (markerPtr != NULL) {
	return (ClientData) markerPtr;
    }
    search.interpolate = TRUE;
    search.halo = graphPtr->halo + 1;
    search.index = -1;
    search.dist = (double)(search.halo + 1);
    search.x = x;
    search.y = y;

    for (item = Blt_ListLastItem(&(graphPtr->elemList)); item != NULL;
	item = Blt_ListPrevItem(item)) {
	elemPtr = (Element *)Blt_ListGetValue(item);
	if (!elemPtr->hidden) {
	    (*elemPtr->infoPtr->closestProc) (graphPtr, elemPtr, &search);
	}
    }
    if (search.dist <= (double)search.halo) {
	return (ClientData) search.elemPtr;
    }
    markerPtr = (Marker *)Blt_NearestMarker(graphPtr, x, y, TRUE);
    if (markerPtr != NULL) {
	return (ClientData) markerPtr;
    }
    return NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureGraph --
 *
 *	Allocates resources for the graph.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Configuration information, such as text string, colors, font,
 *	etc. get set for graphPtr;  old resources get freed, if there
 *	were any.  The graph is redisplayed.
 *
 *----------------------------------------------------------------------
 */
static void
ConfigureGraph(graphPtr)
    Graph *graphPtr;		/* Graph widget record */
{
    XColor *colorPtr;
    GC newGC;
    XGCValues gcValues;
    unsigned long gcMask;

    /* Don't allow negative bar widths. Reset to an arbitrary value (0.1) */
    if (graphPtr->barWidth <= 0.0) {
	graphPtr->barWidth = 0.1;
    }
    if ((graphPtr->reqHeight != Tk_ReqHeight(graphPtr->tkwin)) ||
	(graphPtr->reqWidth != Tk_ReqWidth(graphPtr->tkwin))) {
	Tk_GeometryRequest(graphPtr->tkwin, graphPtr->reqWidth,
	    graphPtr->reqHeight);
    }
    Tk_SetInternalBorder(graphPtr->tkwin, graphPtr->borderWidth);
    colorPtr = Tk_3DBorderColor(graphPtr->border);

    if (graphPtr->titleText != NULL) {
	int textWidth, textHeight;

	Blt_GetTextExtents(&(graphPtr->titleAttr), graphPtr->titleText,
	    &textWidth, &textHeight);
	graphPtr->titleAttr.height = textHeight + 10;
    } else {
	graphPtr->titleAttr.width = graphPtr->titleAttr.height = 0;
    }

    /*
     * Create GCs for interior and exterior regions, and a background
     * GC for clearing the margins with XFillRectangle
     */

    /* Margin GC */

    gcValues.foreground = graphPtr->titleAttr.color->pixel;
    gcValues.background = colorPtr->pixel;
    gcMask = (GCForeground | GCBackground);
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (graphPtr->drawGC != NULL) {
	Tk_FreeGC(graphPtr->display, graphPtr->drawGC);
    }
    graphPtr->drawGC = newGC;

    /* Plot fill GC (Background = Foreground) */

    gcValues.foreground = graphPtr->plotBg->pixel;
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (graphPtr->plotFillGC != NULL) {
	Tk_FreeGC(graphPtr->display, graphPtr->plotFillGC);
    }
    graphPtr->plotFillGC = newGC;

    /* Margin fill GC (Background = Foreground) */

    gcValues.foreground = colorPtr->pixel;
    gcValues.background = graphPtr->titleAttr.color->pixel;
    if (graphPtr->tile != NULL) {
	Pixmap pixmap;

	Blt_SetTileChangedProc(graphPtr->tile, TileChangedProc,
	    (ClientData)graphPtr);

	pixmap = Blt_PixmapOfTile(graphPtr->tile);
	if (pixmap != None) {
	    gcMask |= GCTile | GCFillStyle;
	    gcValues.fill_style = FillTiled;
	    gcValues.tile = pixmap;
	}
    }
    newGC = Tk_GetGC(graphPtr->tkwin, gcMask, &gcValues);
    if (graphPtr->fillGC != NULL) {
	Tk_FreeGC(graphPtr->display, graphPtr->fillGC);
    }
    graphPtr->fillGC = newGC;

    Blt_ResetTextAttributes(graphPtr->tkwin, &(graphPtr->titleAttr));

    if (Blt_ConfigModified(configSpecs, "-invertxy", (char *)NULL)) {

	/*
	 * If the -inverted option changed, we need to readjust the pointers
	 * to the axes and recompute the their scales.
	 */

	Blt_AdjustAxisPointers(graphPtr);
	graphPtr->flags |= RESET_AXES;
    }
    if ((!graphPtr->backingStore) && (graphPtr->backPixmap != None)) {

	/*
	 * Free the pixmap if we're not buffering the display of elements
	 * anymore.
	 */

	Tk_FreePixmap(graphPtr->display, graphPtr->backPixmap);
	graphPtr->backPixmap = None;
    }
    /*
     * Reconfigure the crosshairs in case the plotting area's
     * background color changed
     */

    Blt_ConfigureCrosshairs(graphPtr);

    /*
     *  Update the layout of the graph (and redraw the elements) if
     *  any of the following graph options which affect the size of
     *	the plotting area has changed.
     *
     *	    -aspect
     *      -borderwidth, -plotborderwidth
     *	    -font, -title
     *	    -width, -height
     *	    -invertxy
     *	    -bottommargin, -leftmargin, -rightmargin, -topmargin,
     *	    -barmode, -barwidth
     */
    if (Blt_ConfigModified(configSpecs, "-invertxy", "-title", "-font",
	    "-*margin", "-*width", "-height", "-barmode", "-*pad*", "-aspect",
	    (char *)NULL)) {
	graphPtr->flags |= SET_ALL_FLAGS;
    }
    if (Blt_ConfigModified(configSpecs, "-plotbackground", (char *)NULL)) {
	graphPtr->flags |= REDRAW_BACKING_STORE;
    }
    graphPtr->flags |= REDRAW_WORLD;
    Blt_EventuallyRedrawGraph(graphPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyGraph --
 *
 *	This procedure is invoked by Tk_EventuallyFree or Tk_Release
 *	to clean up the internal structure of a graph at a safe time
 *	(when no-one is using it anymore).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Everything associated with the widget is freed up.
 *
 *----------------------------------------------------------------------
 */
static void
DestroyGraph(dataPtr)
    DestroyData dataPtr;
{
    Graph *graphPtr = (Graph *)dataPtr;

    /*
     * Destroy the individual components of the graph: elements, markers,
     * X and Y axes, legend, display lists etc.
     */
    Blt_DestroyMarkers(graphPtr);
    Blt_DestroyElements(graphPtr);

    Blt_DestroyAxes(graphPtr);
    Blt_DestroyPens(graphPtr);

    Blt_DestroyLegend(graphPtr);
    Blt_DestroyPostScript(graphPtr);
    Blt_DestroyCrosshairs(graphPtr);
    Blt_DestroyGrid(graphPtr);
    Blt_DestroyBindingTable(graphPtr->bindTable);

    /* Release allocated X resources and memory. */
    if (graphPtr->drawGC != NULL) {
	Tk_FreeGC(graphPtr->display, graphPtr->drawGC);
    }
    if (graphPtr->fillGC != NULL) {
	Tk_FreeGC(graphPtr->display, graphPtr->fillGC);
    }
    if (graphPtr->plotFillGC != NULL) {
	Tk_FreeGC(graphPtr->display, graphPtr->plotFillGC);
    }
    Blt_FreeTextAttributes(graphPtr->display, &(graphPtr->titleAttr));
    if (graphPtr->backPixmap != None) {
	Tk_FreePixmap(graphPtr->display, graphPtr->backPixmap);
    }
    if (graphPtr->freqArr != NULL) {
	free((char *)graphPtr->freqArr);
    }
    if (graphPtr->numStacks > 0) {
	Tcl_DeleteHashTable(&(graphPtr->freqTable));
    }
    if (graphPtr->tile != NULL) {
	Blt_FreeTile(graphPtr->tile);
    }
    Tk_FreeOptions(configSpecs, (char *)graphPtr, graphPtr->display, 0);
    Tcl_DeleteHashEntry(graphPtr->hashPtr);
    free((char *)graphPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * CreateGraph --
 *
 *	This procedure creates and initializes a new widget.
 *
 * Results:
 *	The return value is a pointer to a structure describing
 *	the new widget.  If an error occurred, then the return
 *	value is NULL and an error message is left in interp->result.
 *
 * Side effects:
 *	Memory is allocated, a Tk_Window is created, etc.
 *
 *----------------------------------------------------------------------
 */

static Graph *
CreateGraph(interp, parent, argc, argv, type)
    Tcl_Interp *interp;
    Tk_Window parent;
    int argc;
    char **argv;
    ObjectType type;
{
    Graph *graphPtr;
    Tk_Window tkwin;
    int dummy;

    if (!initialized) {
	Tcl_InitHashTable(&(graphTable), TCL_ONE_WORD_KEYS);
	initialized = 1;
    }
    tkwin = Tk_CreateWindowFromPath(interp, parent, argv[1], (char *)NULL);
    if (tkwin == (Tk_Window)NULL) {
	return (Graph *) NULL;
    }
    graphPtr = (Graph *)calloc(1, sizeof(Graph));
    assert(graphPtr);

    Tk_SetClass(tkwin, classNames[type - 1]);

    /* Initialize the data structure for the graph. */

    graphPtr->tkwin = tkwin;
    graphPtr->display = Tk_Display(tkwin);
    graphPtr->interp = interp;
    graphPtr->type = type;
    graphPtr->backingStore = 1;
    graphPtr->plotRelief = TK_RELIEF_SUNKEN;
    graphPtr->relief = TK_RELIEF_FLAT;
    graphPtr->flags = (SET_ALL_FLAGS);
    graphPtr->nextMarkerId = 1;
    graphPtr->padLeft = graphPtr->padRight = 8;
    graphPtr->padTop = graphPtr->padBottom = 8;
    Blt_InitTextAttributes(&(graphPtr->titleAttr));

    /* Add graph to hash table of graphs */
    graphPtr->hashPtr = Tcl_CreateHashEntry(&graphTable, (char *)tkwin, &dummy);
    Tcl_SetHashValue(graphPtr->hashPtr, (ClientData)graphPtr);

    Tcl_InitHashTable(&(graphPtr->axisTable), TCL_STRING_KEYS);
    Tcl_InitHashTable(&(graphPtr->elemTable), TCL_STRING_KEYS);
    Tcl_InitHashTable(&(graphPtr->markerTable), TCL_STRING_KEYS);
    Tcl_InitHashTable(&(graphPtr->markerTagTable), TCL_STRING_KEYS);
    Tcl_InitHashTable(&(graphPtr->elemTagTable), TCL_STRING_KEYS);
    Blt_InitList(&(graphPtr->elemList), TCL_STRING_KEYS);
    Blt_InitList(&(graphPtr->markerList), TCL_STRING_KEYS);
    if (InitPens(graphPtr) != TCL_OK) {
	goto error;
    }
    if (Tk_ConfigureWidget(interp, tkwin, configSpecs, argc - 2, argv + 2,
	    (char *)graphPtr, 0) != TCL_OK) {
	goto error;
    }
    if (Blt_DefaultAxes(graphPtr) != TCL_OK) {
	goto error;
    }
    Blt_AdjustAxisPointers(graphPtr);

    if (Blt_CreatePostScript(graphPtr) != TCL_OK) {
	goto error;
    }
    if (Blt_CreateCrosshairs(graphPtr) != TCL_OK) {
	goto error;
    }
    if (Blt_CreateLegend(graphPtr) != TCL_OK) {
	goto error;
    }
    if (Blt_CreateGrid(graphPtr) != TCL_OK) {
	goto error;
    }
    Tk_CreateEventHandler(graphPtr->tkwin, ExposureMask | StructureNotifyMask,
	GraphEventProc, (ClientData)graphPtr);

    graphPtr->cmdToken = Tcl_CreateCommand(interp, argv[1], GraphWidgetCmd,
	(ClientData)graphPtr, GraphDeleteCmdProc);
#ifdef ITCL_NAMESPACES
    Itk_SetWidgetCommand(graphPtr->tkwin, graphPtr->cmdToken);
#endif
    ConfigureGraph(graphPtr);

    graphPtr->bindTable = Blt_CreateBindingTable(interp, tkwin,
	(ClientData)graphPtr, PickEntry, Blt_GraphTags);

    return graphPtr;

  error:
    if (tkwin != (Tk_Window)NULL) {
	Tk_DestroyWindow(tkwin);
    }
    return (Graph *) NULL;
}

/* Widget sub-commands */

/*ARGSUSED*/
static int
XAxisOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;		/* Not used */
    int argc;
    char **argv;
{
    return Blt_AxisOp(graphPtr, &(graphPtr->axisArr[0]), argc, argv);
}

/*ARGSUSED*/
static int
X2AxisOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;		/* Not used */
    int argc;
    char **argv;
{
    return Blt_AxisOp(graphPtr, &(graphPtr->axisArr[2]), argc, argv);
}

/*ARGSUSED*/
static int
YAxisOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;		/* Not used */
    int argc;
    char **argv;
{
    return Blt_AxisOp(graphPtr, &(graphPtr->axisArr[1]), argc, argv);
}

/*ARGSUSED*/
static int
Y2AxisOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;		/* Not used */
    int argc;
    char **argv;
{
    return Blt_AxisOp(graphPtr, &(graphPtr->axisArr[3]), argc, argv);
}

/*ARGSUSED*/
static int
BarOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;		/* Not used */
    int argc;
    char **argv;
{
    return Blt_ElementOp(graphPtr, interp, argc, argv, TYPE_ELEM_BAR);
}

/*ARGSUSED*/
static int
LineOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;		/* Not used */
    int argc;
    char **argv;
{
    return Blt_ElementOp(graphPtr, interp, argc, argv, TYPE_ELEM_LINE);
}

/*ARGSUSED*/
static int
ElementOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;		/* Not used */
    int argc;
    char **argv;
{
    return Blt_ElementOp(graphPtr, interp, argc, argv, graphPtr->type);
}

static int
ConfigureOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int flags;

    flags = TK_CONFIG_ARGV_ONLY;
    if (argc == 2) {
	return Tk_ConfigureInfo(interp, graphPtr->tkwin, configSpecs,
	    (char *)graphPtr, (char *)NULL, flags);
    } else if (argc == 3) {
	return Tk_ConfigureInfo(interp, graphPtr->tkwin, configSpecs,
	    (char *)graphPtr, argv[2], flags);
    } else {
	if (Tk_ConfigureWidget(interp, graphPtr->tkwin, configSpecs, argc - 2,
		argv + 2, (char *)graphPtr, flags) != TCL_OK) {
	    return TCL_ERROR;
	}
	ConfigureGraph(graphPtr);
	return TCL_OK;
    }
}

/* ARGSUSED*/
static int
CgetOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    return Tk_ConfigureValue(interp, graphPtr->tkwin, configSpecs,
	(char *)graphPtr, argv[2], 0);
}

/*
 *--------------------------------------------------------------
 *
 * ExtentsOp --
 *
 *	Reports the size of one of several items within the graph.
 *	The following are valid items:
 *
 *	  "bottommargin"	Height of the bottom margin
 *	  "leftmargin"		Width of the left margin
 *	  "legend"		x y w h of the legend
 *	  "plotarea"		x y w h of the plotarea
 *	  "plotheight"		Height of the plot area
 *	  "rightmargin"		Width of the right margin
 *	  "topmargin"		Height of the top margin
 *        "plotwidth"		Width of the plot area
 *
 * Results:
 *	Always returns TCL_OK.
 *
 *--------------------------------------------------------------
 */
/* ARGSUSED*/
static int
ExtentsOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    char c;
    unsigned int length;
    char string[200];

    c = argv[2][0];
    length = strlen(argv[2]);
    if ((c == 'p') && (length > 4) &&
	(strncmp("plotheight", argv[2], length) == 0)) {
	Tcl_SetResult(interp, Blt_Int(graphPtr->yMax - graphPtr->yMin + 1),
	    TCL_VOLATILE);
    } else if ((c == 'p') && (length > 4) &&
	(strncmp("plotwidth", argv[2], length) == 0)) {
	Tcl_SetResult(interp, Blt_Int(graphPtr->xMax - graphPtr->xMin + 1),
	    TCL_VOLATILE);
    } else if ((c == 'p') && (length > 4) &&
	(strncmp("plotarea", argv[2], length) == 0)) {
	sprintf(string, "%d %d %d %d", graphPtr->xMin, graphPtr->yMin,
	    graphPtr->xMax - graphPtr->xMin + 1,
	    graphPtr->yMax - graphPtr->yMin + 1);
	Tcl_SetResult(interp, string, TCL_VOLATILE);
    } else if ((c == 'l') && (length > 2) &&
	(strncmp("legend", argv[2], length) == 0)) {
	Legend *legendPtr = graphPtr->legendPtr;

	sprintf(string, "%d %d %d %d", legendPtr->x, legendPtr->y,
	    legendPtr->width, legendPtr->height);
	Tcl_SetResult(interp, string, TCL_VOLATILE);
    } else if ((c == 'l') && (length > 2) &&
	(strncmp("leftmargin", argv[2], length) == 0)) {
	Tcl_SetResult(interp, Blt_Int(graphPtr->leftMargin), TCL_VOLATILE);
    } else if ((c == 'r') && (length > 1) &&
	(strncmp("rightmargin", argv[2], length) == 0)) {
	Tcl_SetResult(interp, Blt_Int(graphPtr->rightMargin), TCL_VOLATILE);
    } else if ((c == 't') && (length > 1) &&
	(strncmp("topmargin", argv[2], length) == 0)) {
	Tcl_SetResult(interp, Blt_Int(graphPtr->topMargin), TCL_VOLATILE);
    } else if ((c == 'b') && (length > 1) &&
	(strncmp("bottommargin", argv[2], length) == 0)) {
	Tcl_SetResult(interp, Blt_Int(graphPtr->bottomMargin), TCL_VOLATILE);
    } else {
	Tcl_AppendResult(interp, "bad extent item \"", argv[2],
	    "\": should be plotheight, plotwidth, leftmargin, rightmargin, \
topmargin, bottommargin, plotarea, or legend", (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * InsideOp --
 *
 *	Returns true of false whether the given point is inside
 *	the plotting area (defined by xMin,yMax xMax, yMin).
 *
 * Results:
 *	Always returns TCL_OK.  interp->result will contain
 *	the boolean string representation.
 *
 *--------------------------------------------------------------
 */
/* ARGSUSED*/
static int
InsideOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    int x, y;

    if (Tk_GetPixels(interp, graphPtr->tkwin, argv[2], &x) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Tk_GetPixels(interp, graphPtr->tkwin, argv[3], &y) != TCL_OK) {
	return TCL_ERROR;
    }
    Tcl_SetResult(interp, PointInGraph(graphPtr, x, y) ? "1" : "0", TCL_STATIC);
    return TCL_OK;
}

/*
 * -------------------------------------------------------------------------
 *
 * InvtransformOp --
 *
 *	This procedure returns a list of the graph coordinate
 *	values corresponding with the given window X and Y
 *	coordinate positions.
 *
 * Results:
 *	Returns a standard Tcl result.  If an error occurred while
 *	parsing the window positions, TCL_ERROR is returned, and
 *	interp->result will contain the error message.  Otherwise
 *	interp->result will contain a Tcl list of the x and y
 *	coordinates.
 *
 * ------------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
InvtransformOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;		/* Graph widget record */
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    double x, y;
    Point2D point;
    Axis2D axes;

    if (Tcl_ExprDouble(interp, argv[2], &x) != TCL_OK ||
	Tcl_ExprDouble(interp, argv[3], &y) != TCL_OK) {
	return TCL_ERROR;
    }
    if (graphPtr->flags & RESET_AXES) {
	Blt_ResetAxes(graphPtr);
    }
    /* Perform the reverse transformation, converting from window
     * coordinates to graph data coordinates.  Note that the point is
     * always mapped to the bottom and left axes (which may not be
     * what the user wants).
     */
    /*  Pick the first pair of axes */
    axes.x = graphPtr->axisArr[0].virtAxisPtr;
    axes.y = graphPtr->axisArr[1].virtAxisPtr;
    point = Blt_InvTransform2DPt(graphPtr, x, y, &axes);

    Tcl_AppendElement(interp, Blt_Double(interp, point.x));
    Tcl_AppendElement(interp, Blt_Double(interp, point.y));
    return TCL_OK;
}

/*
 * --------------------------------------------------------------------------
 *
 * TransformOp --
 *
 *	This procedure returns a list of the window coordinates
 *	corresponding with the given graph x and y coordinates.
 *
 * Results:
 *	Returns a standard Tcl result.  interp->result contains
 *	the list of the graph coordinates. If an error occurred
 *	while parsing the window positions, TCL_ERROR is returned,
 *	then interp->result will contain an error message.
 *
 * -------------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
TransformOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;		/* Graph widget record */
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    double x, y;
    Point2D point;
    Axis2D axes;

    if (Blt_GetCoordinate(interp, argv[2], &x) != TCL_OK ||
	Blt_GetCoordinate(interp, argv[3], &y) != TCL_OK) {
	return TCL_ERROR;
    }
    if (graphPtr->flags & RESET_AXES) {
	Blt_ResetAxes(graphPtr);
    }
    /*
     * Perform the transformation from window to graph coordinates.
     * Note that the points are always mapped onto the bottom and left
     * axes (which may not be the what the user wants).
     */
    axes.x = graphPtr->axisArr[0].virtAxisPtr;
    axes.y = graphPtr->axisArr[1].virtAxisPtr;

    point = Blt_Transform2DPt(graphPtr, x, y, &axes);
    Tcl_AppendElement(interp, Blt_Int(ROUND(point.x)));
    Tcl_AppendElement(interp, Blt_Int(ROUND(point.y)));
    return TCL_OK;
}

#ifdef _MSC_VER
/*
 * --------------------------------------------------------------------------
 *
 * PrintOp --
 *
 *	Snaps a picture of the graph and stores it in the specified image
 *
 * Results:
 *	Returns a standard Tcl result.  interp->result contains
 *	the list of the graph coordinates. If an error occurred
 *	while parsing the window positions, TCL_ERROR is returned,
 *	then interp->result will contain an error message.
 *
 * -------------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
PrintOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;		/* Graph widget record */
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    Drawable drawable;
    int noBackingStore = 0;
    int width, height;

    graphPtr->width = Tk_Width(graphPtr->tkwin);
    graphPtr->height = Tk_Height(graphPtr->tkwin);
    if ((graphPtr->width < 2) && (graphPtr->reqWidth > 0)) {
	graphPtr->width = graphPtr->reqWidth;
    }
    if ((graphPtr->height < 2) && (graphPtr->reqHeight > 0)) {
	graphPtr->height = graphPtr->reqHeight;
    }
    if (Blt_GetOpenPrinter(interp, argv[2], &drawable) != TCL_OK) {
	return TCL_ERROR;
    }
    Blt_StartPrintJob(interp, argv[2]);
    graphPtr->flags |= SET_ALL_FLAGS;
    Blt_DrawGraph(graphPtr, drawable, noBackingStore);
    width = graphPtr->width, height = graphPtr->height;
    Blt_EndPrintJob(interp, argv[2]);
    return TCL_OK;
}
#endif /*WIN32*/

/*
 * --------------------------------------------------------------------------
 *
 * SnapOp --
 *
 *	Snaps a picture of the graph and stores it in the specified image
 *
 * Results:
 *	Returns a standard Tcl result.  interp->result contains
 *	the list of the graph coordinates. If an error occurred
 *	while parsing the window positions, TCL_ERROR is returned,
 *	then interp->result will contain an error message.
 *
 * -------------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
SnapOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;		/* Graph widget record */
    Tcl_Interp *interp;
    int argc;			/* not used */
    char **argv;
{
    int result;
    Pixmap drawable;
    int noBackingStore = 0;
    int width, height;

    graphPtr->width = Tk_Width(graphPtr->tkwin);
    graphPtr->height = Tk_Height(graphPtr->tkwin);
    if ((graphPtr->width < 2) && (graphPtr->reqWidth > 0)) {
	graphPtr->width = graphPtr->reqWidth;
    }
    if ((graphPtr->height < 2) && (graphPtr->reqHeight > 0)) {
	graphPtr->height = graphPtr->reqHeight;
    }
    drawable = Tk_GetPixmap(graphPtr->display, Tk_WindowId(graphPtr->tkwin),
	graphPtr->width, graphPtr->height, Tk_Depth(graphPtr->tkwin));
    graphPtr->flags |= SET_ALL_FLAGS;
    Blt_DrawGraph(graphPtr, drawable, noBackingStore);
    width = graphPtr->width, height = graphPtr->height;

    if ((argc > 3) &&
	(Blt_GetLength(interp, graphPtr->tkwin, argv[3], &width) != TCL_OK)) {
	return TCL_ERROR;
    }
    if ((argc > 4) &&
	(Blt_GetLength(interp, graphPtr->tkwin, argv[4], &height) != TCL_OK)) {
	return TCL_ERROR;
    }
    result = Blt_SnapPhoto(interp, graphPtr->tkwin, drawable, graphPtr->width,
	graphPtr->height, width, height, argv[2]);
    Tk_FreePixmap(graphPtr->display, drawable);
    return result;
}

/*
 * --------------------------------------------------------------------------
 *
 * GraphWidgetCmd --
 *
 *	This procedure is invoked to process the Tcl command that
 *	corresponds to a widget managed by this module.  See the user
 *	documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 * --------------------------------------------------------------------------
 */
static Blt_OpSpec graphOps[] =
{
    {"axis", 1, (Blt_Operation)Blt_VirtualAxisOp, 2, 0, "oper ?args?",},
    {"bar", 2, (Blt_Operation)BarOp, 2, 0, "oper ?args?",},
    {"cget", 2, (Blt_Operation)CgetOp, 3, 3, "option",},
    {"configure", 2, (Blt_Operation)ConfigureOp, 2, 0, "?option value?...",},
    {"crosshairs", 2, (Blt_Operation)Blt_CrosshairsOp, 2, 0, "oper ?args?",},
    {"element", 2, (Blt_Operation)ElementOp, 2, 0, "oper ?args?",},
    {"extents", 2, (Blt_Operation)ExtentsOp, 3, 3, "item",},
    {"grid", 1, (Blt_Operation)Blt_GridOp, 2, 0, "oper ?args?",},
    {"inside", 3, (Blt_Operation)InsideOp, 4, 4, "winX winY",},
    {"invtransform", 3, (Blt_Operation)InvtransformOp, 4, 4, "winX winY",},
    {"legend", 2, (Blt_Operation)Blt_LegendOp, 2, 0, "oper ?args?",},
    {"line", 2, (Blt_Operation)LineOp, 2, 0, "oper ?args?",},
    {"marker", 1, (Blt_Operation)Blt_MarkerOp, 2, 0, "oper ?args?",},
    {"pen", 2, (Blt_Operation)Blt_PenOp, 2, 0, "oper ?args?",},
    {"postscript", 2, (Blt_Operation)Blt_PostScriptOp, 2, 0, "oper ?args?",},
#ifdef _MSC_VER
    {"print", 2, (Blt_Operation)PrintOp, 3, 3, "printerId",},
#endif
    {"snap", 1, (Blt_Operation)SnapOp, 3, 5, "photoName ?width height?",},
    {"transform", 1, (Blt_Operation)TransformOp, 4, 4, "x y",},
    {"x2axis", 2, (Blt_Operation)X2AxisOp, 2, 0, "oper ?args?",},
    {"xaxis", 2, (Blt_Operation)XAxisOp, 2, 0, "oper ?args?",},
    {"y2axis", 2, (Blt_Operation)Y2AxisOp, 2, 0, "oper ?args?",},
    {"yaxis", 2, (Blt_Operation)YAxisOp, 2, 0, "oper ?args?",},
};
static int numGraphOps = sizeof(graphOps) / sizeof(Blt_OpSpec);

static int
GraphWidgetCmd(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Blt_Operation proc;
    int result;
    Graph *graphPtr = (Graph *)clientData;

    proc = Blt_GetOperation(interp, numGraphOps, graphOps, BLT_OPER_ARG1,
	argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    Tk_Preserve((ClientData)graphPtr);
    result = (*proc) (graphPtr, interp, argc, argv);
    Tk_Release((ClientData)graphPtr);
    return result;
}

/*
 * --------------------------------------------------------------------------
 *
 * NewGraph --
 *
 *	Creates a new window and Tcl command representing an
 *	instance of a graph widget.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 * --------------------------------------------------------------------------
 */
static int
NewGraph(clientData, interp, argc, argv, type)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
    ObjectType type;
{
    Tk_Window tkwin = (Tk_Window)clientData;
    Graph *graphPtr;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " pathName ?option value?...\"", (char *)NULL);
	return TCL_ERROR;
    }
    graphPtr = CreateGraph(interp, tkwin, argc, argv, type);
    if (graphPtr == NULL) {
	return TCL_ERROR;
    }
    Tcl_SetResult(interp, Tk_PathName(graphPtr->tkwin), TCL_STATIC);
    return TCL_OK;
}

/*
 * --------------------------------------------------------------------------
 *
 * GraphCmd --
 *
 *	Creates a new window and Tcl command representing an
 *	instance of a graph widget.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 * --------------------------------------------------------------------------
 */
static int
GraphCmd(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    return NewGraph(clientData, interp, argc, argv, TYPE_ELEM_LINE);
}

/*
 *--------------------------------------------------------------
 *
 * BarchartCmd --
 *
 *	Creates a new window and Tcl command representing an
 *	instance of a barchart widget.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */
/*ARGSUSED*/
static int
BarchartCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    return NewGraph(clientData, interp, argc, argv, TYPE_ELEM_BAR);
}

/*
 *--------------------------------------------------------------
 *
 * StripchartCmd --
 *
 *	Creates a new window and Tcl command representing an
 *	instance of a barchart widget.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StripchartCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    return NewGraph(clientData, interp, argc, argv, TYPE_ELEM_STRIP);
}

/*
 * -----------------------------------------------------------------------
 *
 * DrawMargins --
 *
 * 	Draws the exterior region of the graph (axes, ticks, titles, etc)
 *	onto a pixmap. The interior region is defined by the given
 *	rectangle structure.
 *
 *		X coordinate axis
 *		Y coordinate axis
 *		legend
 *		interior border
 *		exterior border
 *		titles (X and Y axis, graph)
 *
 * Returns:
 *	None.
 *
 * Side Effects:
 *	Exterior of graph is displayed in its window.
 *
 * -----------------------------------------------------------------------
 */
static void
DrawMargins(graphPtr, drawable)
    Graph *graphPtr;
    Drawable drawable;		/* Pixmap or window to draw into */
{
    XRectangle rectArr[4];
    LegendSite site;

#ifdef notdef
    fprintf(stderr, "Calling DrawMargins(%s)\n", Tk_PathName(graphPtr->tkwin));
#endif
    /*
     * Draw the four outer rectangles which encompass the plotting
     * surface. This clears the surrounding area and clips the plot.
     */
    rectArr[0].x = rectArr[0].y = rectArr[3].x = rectArr[1].x = 0;
    rectArr[0].width = rectArr[3].width = graphPtr->width;
    rectArr[0].height = graphPtr->topMargin;
    rectArr[3].y = graphPtr->yMax + 1;
    rectArr[3].height = graphPtr->bottomMargin;
    rectArr[2].y = rectArr[1].y = graphPtr->yMin;
    rectArr[1].width = graphPtr->leftMargin;
    rectArr[2].height = rectArr[1].height = 
	graphPtr->yMax - graphPtr->yMin + 1;
    rectArr[2].x = graphPtr->xMax + 1;
    rectArr[2].width = graphPtr->rightMargin;

    if (graphPtr->tile != NULL) {
	Blt_SetTileOrigin(graphPtr->tkwin, graphPtr->fillGC, 0, 0);
    }
    XFillRectangles(graphPtr->display, drawable, graphPtr->fillGC, rectArr, 4);

    /* Draw 3D border around the plotting area */

    if (graphPtr->plotBW > 0) {
	int x, y, width, height;

	x = graphPtr->xMin - graphPtr->plotBW;
	y = graphPtr->yMin - graphPtr->plotBW;
	width = (graphPtr->xMax - graphPtr->xMin) + (2 * graphPtr->plotBW);
	height = (graphPtr->yMax - graphPtr->yMin) + (2 * graphPtr->plotBW);
	Tk_Draw3DRectangle(graphPtr->tkwin, drawable, graphPtr->border,
	    x, y, width, height, graphPtr->plotBW, graphPtr->plotRelief);
    }
    site = GetLegendSite(graphPtr);
    if (site < LEGEND_SITE_PLOT) {
	/* Legend is drawn on one of the graph margins */
	Blt_DrawLegend(graphPtr, drawable);
    }
    if (graphPtr->titleText != NULL) {
	Blt_DrawText(graphPtr->tkwin, drawable, graphPtr->titleText,
	    &(graphPtr->titleAttr), graphPtr->titleX, graphPtr->titleY);
    }
    Blt_DrawAxes(graphPtr, drawable);

    if ((graphPtr->relief != TK_RELIEF_FLAT) && (graphPtr->borderWidth > 0)) {
	/* Exterior 3D border */
	Tk_Draw3DRectangle(graphPtr->tkwin, drawable, graphPtr->border, 0, 0,
	    graphPtr->width, graphPtr->height, graphPtr->borderWidth,
	    graphPtr->relief);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DrawPlotRegion --
 *
 *	Draws the contents of the plotting area.  This consists of
 *	the elements, markers (draw under elements), axis limits,
 *	grid lines, and possibly the legend.  Typically, the output
 *	will be cached into a backing store pixmap, so that redraws
 *	can occur quickly.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
DrawPlotRegion(graphPtr, drawable)
    Graph *graphPtr;
    Drawable drawable;		/* Pixmap or window to draw into */
{
    LegendSite site;
#ifdef notdef
    fprintf(stderr, "Calling DrawPlotRegion(%s)\n",
	Tk_PathName(graphPtr->tkwin));
#endif
    /* Clear the background of the plotting area. */
    XFillRectangle(graphPtr->display, drawable, graphPtr->plotFillGC,
	graphPtr->xMin, graphPtr->yMin, graphPtr->xMax - graphPtr->xMin + 1,
	graphPtr->yMax - graphPtr->yMin + 1);

    /* Draw the elements, markers, legend, and axis limits. */

    if (!graphPtr->gridPtr->hidden) {
	Blt_DrawGrid(graphPtr, drawable);
    }
    site = GetLegendSite(graphPtr);
    Blt_DrawMarkers(graphPtr, drawable, MARKER_UNDER);
    if (((site == LEGEND_SITE_XY) || (site == LEGEND_SITE_PLOT)) &&
	(!graphPtr->legendPtr->raised)) {
	Blt_DrawLegend(graphPtr, drawable);
    }
    Blt_DrawAxisLimits(graphPtr, drawable);
    Blt_DrawElements(graphPtr, drawable);
}

void
Blt_LayoutGraph(graphPtr)
    Graph *graphPtr;
{
    if (graphPtr->flags & RESET_AXES) {
	Blt_ResetAxes(graphPtr);
    }
    if (graphPtr->flags & LAYOUT_NEEDED) {
	Blt_LayoutMargins(graphPtr);
	graphPtr->flags &= ~LAYOUT_NEEDED;
    }
    /* Compute coordinate transformations for graph components */

    if (graphPtr->flags & COORDS_WORLD) {
	register Axis *axisPtr;
	register int i;

	for (axisPtr = graphPtr->axisArr, i = 0; i < 4; i++, axisPtr++) {
	    if (!axisPtr->virtAxisPtr->hidden) {
		Blt_TransformAxis(graphPtr, axisPtr);
	    }
	}
    }
    Blt_TransformElements(graphPtr);
    Blt_TransformMarkers(graphPtr);
    Blt_TransformGrid(graphPtr);
    graphPtr->flags &= ~(COORDS_ALL_PARTS);
}

void
Blt_DrawGraph(graphPtr, drawable, backingStore)
    Graph *graphPtr;
    Drawable drawable;		/* Pixmap or window to draw into */
    int backingStore;		/* If non-zero, use backing store for
				 * plotting area. */
{
    LegendSite site;
#ifdef notdef
    fprintf(stderr, "Calling Blt_DrawGraph(%s)\n", Tk_PathName(graphPtr->tkwin));
#endif
    if (backingStore) {
	if ((graphPtr->backPixmap == None) ||
	    (graphPtr->backWidth != graphPtr->width) ||
	    (graphPtr->backHeight != graphPtr->height)) {

	    /*
	     * Create another pixmap to save elements if one doesn't
	     * already exist or the size of the window has changed.
	     */
	    if (graphPtr->backPixmap != None) {
		Tk_FreePixmap(graphPtr->display, graphPtr->backPixmap);
	    }
	    graphPtr->backPixmap = Tk_GetPixmap(graphPtr->display,
		Tk_WindowId(graphPtr->tkwin), graphPtr->width, graphPtr->height,
		Tk_Depth(graphPtr->tkwin));
	    graphPtr->backWidth = graphPtr->width;
	    graphPtr->backHeight = graphPtr->height;
	    graphPtr->flags |= REDRAW_BACKING_STORE;
	}
	if (graphPtr->flags & REDRAW_BACKING_STORE) {

	    /* The backing store is new or out-of-date. */

	    DrawPlotRegion(graphPtr, graphPtr->backPixmap);
	    graphPtr->flags &= ~REDRAW_BACKING_STORE;
	}
	/* Copy the pixmap to the one used for drawing the entire graph. */
	XCopyArea(graphPtr->display, graphPtr->backPixmap, drawable,
	    graphPtr->drawGC, graphPtr->xMin, graphPtr->yMin,
	    (graphPtr->xMax - graphPtr->xMin + 1),
	    (graphPtr->yMax - graphPtr->yMin + 1),
	    graphPtr->xMin, graphPtr->yMin);
    } else {
	DrawPlotRegion(graphPtr, drawable);
    }

    /* Draw markers above elements */
    Blt_DrawMarkers(graphPtr, drawable, MARKER_ABOVE);
    Blt_DrawActiveElements(graphPtr, drawable);

    if (graphPtr->flags & REDRAW_MARGINS) {
	DrawMargins(graphPtr, drawable);
    }
    site = GetLegendSite(graphPtr);
    if (((site == LEGEND_SITE_XY) || (site == LEGEND_SITE_PLOT)) &&
	(graphPtr->legendPtr->raised)) {
	Blt_DrawLegend(graphPtr, drawable);
    }
}

static void
UpdateMarginTraces(graphPtr)
    Graph *graphPtr;
{
    char string[200];

    /* Trigger variable traces */
    if (graphPtr->topVar != NULL) {
	sprintf(string, "%d", graphPtr->topMargin);
	Tcl_SetVar(graphPtr->interp, graphPtr->topVar, string,
	    TCL_GLOBAL_ONLY);
    }
    if (graphPtr->bottomVar != NULL) {
	sprintf(string, "%d", graphPtr->bottomMargin);
	Tcl_SetVar(graphPtr->interp, graphPtr->bottomVar, string,
	    TCL_GLOBAL_ONLY);
    }
    if (graphPtr->leftVar != NULL) {
	sprintf(string, "%d", graphPtr->leftMargin);
	Tcl_SetVar(graphPtr->interp, graphPtr->leftVar, string,
	    TCL_GLOBAL_ONLY);
    }
    if (graphPtr->rightVar != NULL) {
	sprintf(string, "%d", graphPtr->rightMargin);
	Tcl_SetVar(graphPtr->interp, graphPtr->rightVar, string,
	    TCL_GLOBAL_ONLY);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DisplayGraph --
 *
 *	This procedure is invoked to display a graph widget.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Commands are output to X to display the graph in its
 *	current mode.
 *
 *----------------------------------------------------------------------
 */
static void
DisplayGraph(clientData)
    ClientData clientData;
{
    Graph *graphPtr = (Graph *)clientData;
    Pixmap drawable;

#ifdef notdef
    fprintf(stderr, "Calling DisplayGraph(%s)\n", Tk_PathName(graphPtr->tkwin));
#endif
    graphPtr->flags &= ~REDRAW_PENDING;
    if (graphPtr->tkwin == NULL) {
	return;			/* Window was destroyed (should not get here) */
    }
    if (Blt_GraphUpdateNeeded(graphPtr)) {
	/*
	 * One of the elements of the graph has a vector notification pending.
	 * This means that the vector will eventually notify the graph that its
	 * data has changed.  Since the graph uses the actual vector (not a copy)
	 * we need to keep in-sync.  Therefore don't draw right now but wait
	 * until we've been notified, before redrawing.
	 */
	return;
    }
    graphPtr->width = Tk_Width(graphPtr->tkwin);
    graphPtr->height = Tk_Height(graphPtr->tkwin);
    if (graphPtr->flags & RESET_AXES) {
	Blt_ResetAxes(graphPtr);
    }
    Blt_LayoutGraph(graphPtr);

    if (!Tk_IsMapped(graphPtr->tkwin)) {
	/*
	 * The graph's window isn't displayed, so don't bother drawing
	 * anything.  By getting this far, we've at least computed
	 * the coordinates of the graph's new layout.
	 */
	return;
    }
    /*
     * Create a pixmap the size of the window for double buffering.
     */
    drawable = Tk_GetPixmap(graphPtr->display, Tk_WindowId(graphPtr->tkwin),
	graphPtr->width, graphPtr->height, Tk_Depth(graphPtr->tkwin));

    Blt_DrawGraph(graphPtr, drawable, graphPtr->backingStore);

    /* Disable crosshairs before redisplaying to the screen */
    Blt_DisableCrosshairs(graphPtr);
    Blt_UpdateCrosshairs(graphPtr);

    if (graphPtr->flags & REDRAW_MARGINS) {
	XCopyArea(graphPtr->display, drawable, Tk_WindowId(graphPtr->tkwin),
	    graphPtr->drawGC, 0, 0, graphPtr->width, graphPtr->height, 0, 0);
    } else {
	XCopyArea(graphPtr->display, drawable, Tk_WindowId(graphPtr->tkwin),
	    graphPtr->drawGC, graphPtr->xMin, graphPtr->yMin,
	    (graphPtr->xMax - graphPtr->xMin + 1),
	    (graphPtr->yMax - graphPtr->yMin + 1),
	    graphPtr->xMin, graphPtr->yMin);
    }
    Blt_EnableCrosshairs(graphPtr);

    Tk_FreePixmap(graphPtr->display, drawable);

    graphPtr->flags &= ~SET_ALL_FLAGS;
    UpdateMarginTraces(graphPtr);
}

int
Blt_GraphInit(interp)
    Tcl_Interp *interp;
{
    static Blt_CmdSpec cmdSpecs[] =
    {
	{"graph", GraphCmd,},
	{"barchart", BarchartCmd,},
	{"stripchart", StripchartCmd,},
    };

    return Blt_InitCmds(interp, "blt", cmdSpecs, 3);
}

Graph *
Blt_FindGraph(tkwin)
    Tk_Window tkwin;
{
    Tcl_HashEntry *hPtr;

    if (!initialized) {
	Tcl_InitHashTable(&graphTable, TCL_ONE_WORD_KEYS);
	initialized = 1;
    }
    while (tkwin != NULL) {
	hPtr = Tcl_FindHashEntry(&graphTable, (char *)tkwin);
	if (hPtr != NULL) {
	    return (Graph *) Tcl_GetHashValue(hPtr);
	}
	tkwin = Tk_Parent(tkwin);
    }
    return NULL;
}
