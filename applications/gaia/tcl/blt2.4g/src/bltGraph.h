/*
 * bltGraph.h --
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
 */

#ifndef _GRAPH_H
#define _GRAPH_H

#include "bltInt.h"
#include "bltPs.h"
#include "bltGrAxis.h"

#define MARKER_UNDER	1	/* Draw markers designated to lie underneath
				 * elements, grids, legend, etc. */
#define MARKER_ABOVE	0	/* Draw markers designated to rest above
				 * elements, grids, legend, etc. */

#define PADX		2	/* Padding between labels/titles */
#define PADY    	2	/* Padding between labels */

#define TITLE_PAD	5	/* Padding between titles (axis and graph) */
#define MINIMUM_MARGIN	20	/* Minimum margin size */


#define BOUND(x, lo, hi)	 \
	(((x) > (hi)) ? (hi) : ((x) < (lo)) ? (lo) : (x))

/*
 * -------------------------------------------------------------------
 *
 * 	Graph component structure definitions
 *
 * -------------------------------------------------------------------
 */
typedef struct Graph Graph;
typedef struct Element Element;

typedef struct Extents2D {
    double xMin, xMax, yMin, yMax;
} Extents2D;

typedef struct Extents3D {
    double xMin, xMax, yMin, yMax, zMin, zMax;
} Extents3D;

#define PointInRegion(e,x,y) \
	(((x) <= (e)->xMax) && ((x) >= (e)->xMin) && \
	 ((y) <= (e)->yMax) && ((y) >= (e)->yMin))

#define PointInGraph(g,x,y) \
	(((x) <= (g)->xMax) && ((x) >= (g)->xMin) && \
	 ((y) <= (g)->yMax) && ((y) >= (g)->yMin))

#define PointInRectangle(r,x0,y0) \
	(((x0) <= (int)((r)->x + (r)->width - 1)) && ((x0) >= (int)(r)->x) && \
	 ((y0) <= (int)((r)->y + (r)->height - 1)) && ((y0) >= (int)(r)->y))


/*
 * -------------------------------------------------------------------
 *
 * ObjectType --
 *
 *	Enumerates the different types of graph elements this program
 *	produces.  An element can be either a line or a bar.
 *
 * -------------------------------------------------------------------
 */
typedef enum {
    TYPE_UNKNOWN,
    TYPE_ELEM_LINE,	
    TYPE_ELEM_STRIP,	
    TYPE_ELEM_BAR,	
    TYPE_MARKER_BITMAP,
    TYPE_MARKER_IMAGE,
    TYPE_MARKER_LINE,
    TYPE_MARKER_POLYGON,
    TYPE_MARKER_TEXT,
    TYPE_MARKER_WINDOW

} ObjectType;


/*
 * Mask values used to selectively enable GRAPH or BARCHART entries in
 * the various configuration specs.
 */
#define GraphType(g)	(TK_CONFIG_USER_BIT << (g)->type)
#define GRAPH		(TK_CONFIG_USER_BIT << TYPE_ELEM_LINE)
#define STRIPCHART	(TK_CONFIG_USER_BIT << TYPE_ELEM_STRIP)
#define BARCHART	(TK_CONFIG_USER_BIT << TYPE_ELEM_BAR)
#define LINE_GRAPHS	(GRAPH | STRIPCHART)
#define ALL_GRAPHS	(GRAPH | BARCHART | STRIPCHART)

#define PEN_DELETE_PENDING	(1<<0)
#define ACTIVE_PEN		(TK_CONFIG_USER_BIT << 6)
#define NORMAL_PEN		(TK_CONFIG_USER_BIT << 7)
#define ALL_PENS		(NORMAL_PEN | ACTIVE_PEN)

/*
 * -------------------------------------------------------------------
 *
 * FreqInfo --
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    int freq;			/* Number of occurrences of x-coordinate */
    Axis2D axes;		/* Indicates which x and y axis are mapped to
				 * the x-value */
    double sum;			/* Sum of the ordinates of each duplicate
				 * abscissa */
    int count;
    double lastY;
} FreqInfo;

/*
 * -------------------------------------------------------------------
 *
 * FreqKey --
 *
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    double value;		/* Duplicated abscissa */
    Axis2D axes;		/* Axis mapping of element */
} FreqKey;

typedef enum  BarModes {
    MODE_NORMAL,		/* Bars are displayed according to their
				 * x,y coordinates. If two bars have the
				 * same abscissa, the bars will overlay
				 * each other. */
    MODE_STACKED,		/* Coordinates with the same abscissa are
				 * displayed as bar segments stacked upon
				 * each other. The order of stacking is the
				 * order of the element display list and
				 * the order of the data values. */
    MODE_ALIGNED,		/* Coordinates with the same abscissa are
				 * displayed as thinner bar segments aligned
				 * one next to the other. The order of the
				 * alignment is the order of the element
				 * display list and the order of the data
				 * values. */
    MODE_OVERLAP		/* Coordinates with the same abscissa are
				 * displayed as thinner bar segments aligned
				 * one next to the other. The order of the
				 * alignment is the order of the element
				 * display list and the order of the data
				 * values. */
} BarMode;

typedef struct Pen Pen;
typedef struct Marker Marker;

typedef Pen *(PenCreateProc) _ANSI_ARGS_((void));
typedef int (PenConfigureProc) _ANSI_ARGS_((Graph *graphPtr, Pen *penPtr));
typedef void (PenDestroyProc) _ANSI_ARGS_((Graph *graphPtr, Pen *penPtr));

struct Pen {
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

};

typedef enum {
    PS_MONO_BACKGROUND, 
    PS_MONO_FOREGROUND
} MonoAttribute;

/*
 * PostScript --
 *
 * 	Structure contains information specific to the outputting of
 *	PostScript commands to print the graph.
 *
 */
typedef struct {
    int decorations;		/* If non-zero, print graph with
				 * color background and 3D borders */

    /* User configurable fields */

    int reqWidth, reqHeight;	/* If greater than zero, represents the
				 * requested dimensions of the printed graph */
    int reqPaperWidth;
    int reqPaperHeight;		/* Requested dimensions for the PostScript
				 * page. Can constrain the size of the graph
				 * if the graph (plus padding) is larger than
				 * the size of the page. */
    Pad padX, padY;		/* Requested padding on the exterior of the
				 * graph. This forms the bounding box for
				 * the page. */
    PsColorMode colorMode;	/* Selects the color mode for PostScript page
				 * (0=monochrome, 1=greyscale, 2=color) */
    char *colorVarName;		/* If non-NULL, is the name of a Tcl array
				 * variable containing X to PostScript color
				 * translations */
    char *fontVarName;		/* If non-NULL, is the name of a Tcl array
				 * variable containing X to PostScript font
				 * translations */
    int landscape;		/* If non-zero, rotate page 90 degrees */
    int center;			/* If non-zero, center the graph on the page */
    int maxpect;		/* If non-zero, indicates to scale the graph
				 * so that it fills the page (maintaining the
				 * aspect ratio of the graph) */
    int addPreview;		/* If non-zero, generate a preview image and
				 * add it to the PostScript output */

    /* Computed fields */
    
    int bbox[4];		/* Bounding box of PostScript plot. */
    float pageScale;		/* Scale of page. Set if "-maxpect" option 
				 * is set, otherwise 1.0. */
    int pageHeight;		/* Page height, used to translate to
				 * X11 coordinates. */

} PostScript;


/*
 * -------------------------------------------------------------------
 *
 * Grid
 *
 *	Contains attributes of describing how to draw grids (at major
 *	ticks) in the graph.  Grids may be mapped to either/both x and
 *	y axis.
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    GC gc;			/* Graphics context for the grid. */
    Axis2D axes;
    int hidden;			/* If non-zero, grid isn't displayed. */
    int minorGrid;		/* If non-zero, draw grid line for minor
				 * axis ticks too */
    Dashes dashes;		/* Dashstyle of the grid. This represents
				 * an array of alternatingly drawn pixel
				 * values. */
    int lineWidth;		/* Width of the grid lines */
    XColor *colorPtr;		/* Color of the grid lines */

    XSegment *xSegArr;		/* X-axis grid lines */
    int numX;			/* # of segment in the X-axis array */
    XSegment *ySegArr;		/* Y-axis grid lines */
    int numY;			/* # of segments in the Y-axis array */

} Grid;

/*
 * -------------------------------------------------------------------
 *
 * Crosshairs
 *
 *	Contains the line segments positions and graphics context used
 *	to simulate crosshairs (by XOR-ing) on the graph.
 *
 * -------------------------------------------------------------------
 */
typedef struct Crosshairs Crosshairs;

typedef enum LegendSites {
    LEGEND_SITE_BOTTOM,		/* Legend is drawn in the bottom margin */
    LEGEND_SITE_LEFT,		/* Legend is drawn in the left margin */
    LEGEND_SITE_RIGHT,		/* Legend is drawn in the right margin */
    LEGEND_SITE_TOP,		/* Legend is drawn in the top margin, below
				 * the title of the graph */
    LEGEND_SITE_PLOT,		/* Legend is drawn in the plot area */
    LEGEND_SITE_XY		/* Legend is drawn at a specified x,y
				 * window coordinate */
} LegendSite;

typedef struct {
    LegendSite site;
    int x, y;
} LegendPosition;

/*
 *
 * Legend --
 *
 * 	Contains information specific to how the legend will be
 *	displayed.
 *
 */
typedef struct {
    int flags;
    ObjectType type;		/* Type of marker. */
    int hidden;			/* If non-zero, don't display the legend. */
    int width, height;		/* Dimensions of the legend */
    LegendPosition anchorPos;	/* Window coordinates of legend positioning
				 * point. Used in conjunction with the anchor
				 * to determine the location of the legend. */
    int raised;			/* If non-zero, draw the legend last, above
				 * everything else. */

    Pad ipadX, ipadY;		/* # of pixels padding around legend entries */
    Pad padX, padY;		/* # of pixels padding to exterior of legend */
    TextAttributes entryAttr;
    int numEntries;		/* Number of labels (and symbols) to display */

    int numCols, numRows;	/* Number of columns and rows in legend */

    int maxSymSize;		/* Size of largest symbol to be displayed.
				 * Used to calculate size of legend */
    Tk_Anchor anchor;		/* Anchor of legend. Used to interpret the
				 * positioning point of the legend in the
				 * graph*/

    int x, y;			/* Origin of legend */

    int borderWidth;		/* Width of legend 3-D border */
    int relief;			/* 3-d effect of border around the legend:
				 * TK_RELIEF_RAISED etc. */
    Tk_3DBorder activeBorder;	/* Background color for active legend
				 * entries. */
    int activeRelief;		/* 3-d effect: TK_RELIEF_RAISED etc. */
    int entryBorderWidth;	/* Width of border around each entry in the
				 * legend */
    XColor *fillColor;
    GC fillGC;			/* Fill color background GC */

    BindTable bindTable;

} Legend;

#define GetLegendSite(g)  ((g)->legendPtr->anchorPos.site)

/*
 * -------------------------------------------------------------------
 *
 * Graph --
 *
 *	Top level structure containing everything pertaining to
 *	the graph.
 *
 * -------------------------------------------------------------------
 */
struct Graph {
    unsigned int flags;		/* Flags;  see below for definitions. */
    Tcl_Interp *interp;		/* Interpreter associated with graph */
    Tk_Window tkwin;		/* Window that embodies the graph.  NULL
				 * means that the window has been
				 * destroyed but the data structures
				 * haven't yet been cleaned up. */
    Display *display;		/* Display containing widget; needed,
				 * among other things, to release
				 * resources after tkwin has already gone
				 * away. */
    Tcl_HashEntry *hashPtr;	/* Entry of graph in global hash table
				 * used to track down graphs from windows. */

    Tcl_Command cmdToken;	/* Token for graph's widget command. */

    Tk_Cursor cursor;

    int borderWidth;		/* Width of the exterior border */
    int relief;			/* Relief of the exterior border */
    Tk_3DBorder border;		/* 3-D border used to delineate the plot
				 * surface and outer edge of window */

    TextAttributes titleAttr; /* Graph title */
    char *titleText;
    int titleX, titleY;

    char *takeFocus;

    int reqWidth, reqHeight;	/* Requested size of graph window */
    int width, height;		/* Size of graph window or PostScript
				 * page */

    Tcl_HashTable penTable;	/* Table of pens */
    Tcl_HashTable elemTable;	/* Hash table of data elements. */
    Tcl_HashTable markerTable;	/* Hash table of markers */
    Tcl_HashTable axisTable;	/* Table of axes */
    Tcl_HashTable elemTagTable;	/* Hash table of element binding tags. */
    Tcl_HashTable markerTagTable;/* Hash table of marker binding tags. */

    Blt_List elemList;		/* List describing order to draw elements */
    ObjectType type;		/* Default element type: either TYPE_ELEM_LINE,
				 * TYPE_ELEM_STRIP, or TYPE_ELEM_BAR */

    Blt_List markerList;	/* Display list of markers */
    int nextMarkerId;		/* Tracks next marker identifier available */

    BindTable bindTable;

    Axis axisArr[4];		/* Coordinate axis info: see bltGrAxis.c */
    Axis *bottomAxis, *topAxis, *leftAxis, *rightAxis;

    PostScript *postscript;	/* PostScript options: see bltGrPS.c */
    Legend *legendPtr;		/* Legend information: see bltGrLegd.c */
    Crosshairs *crosshairs;	/* Crosshairs information: see bltGrHairs.c */
    Grid *gridPtr;		/* Grid attribute information */

    int halo;			/* Maximum distance allowed between points
				 * when searching for a point */
    int inverted;		/* If non-zero, indicates the x and y axis
				 * positions should be inverted. */
    Blt_Tile tile;
    GC drawGC;			/* Used for drawing on the margins. This
				 * includes the axis lines */
    GC fillGC;			/* Used to fill the background of the
				 * margins. The fill is governed by
				 * the background color or the tiled
				 * pixmap. */
    int plotBW;			/* Width of interior 3-D border. */
    int plotRelief;		/* 3-d effect: TK_RELIEF_RAISED etc. */
    XColor *plotBg;		/* Color of plotting surface */

    GC plotFillGC;		/* Used to fill the plotting area with a
				 * solid background color. The fill color
				 * is stored in "plotBg". */
    /*
     * The following are the requested sizes for the margins surrounding the 
     * plotting area. If non-zero, the requested margin will override the
     * computed size.
     */
    int reqLeftMargin, reqRightMargin, reqTopMargin, reqBottomMargin;

    int leftMargin, rightMargin, topMargin, bottomMargin;

    /* If non-zero, force plot to conform to aspect ratio W/H */
    double aspectRatio;

    /* Variables to be updated when the margins are recalculated. */
    char *leftVar, *rightVar, *topVar, *bottomVar;

    int xMin, yMax;		/* Lower left coordinate of plot bbox */
    int xMax, yMin;		/* Upper right coordinate of plot bbox */

    Pad padX;			/* Vertical padding for plotarea */
    int vRange, vOffset;	/* Vertical axis range and offset from the
				 * left side of the graph window. Used to
				 * transform coordinates to vertical
				 * axes. */
    Pad padY;			/* Horizontal padding for plotarea */
    int hRange, hOffset;	/* Horizontal axis range and offset from
				 * the top of the graph window. Used to
				 * transform horizontal axes */

    int backingStore;		/* If non-zero, cache elements by drawing
				 * them into a pixmap */
    Pixmap backPixmap;		/* Pixmap used to cache elements
				 * displayed.  If *backingStore* is
				 * non-zero, each element is drawn
				 * into this pixmap before it is
				 * copied onto the screen.  The pixmap
				 * then acts as a cache (only the
				 * pixmap is redisplayed if the none
				 * of elements have changed). This is
				 * done so that markers can be redrawn
				 * quickly over elements without
				 * redrawing each element. */
    int backWidth, backHeight;	/* Size of element backing store pixmap. */

    /*
     * barchart specific information
     */
    double baseline;		/* Baseline from bar chart.  */
    double barWidth;		/* Default width of each bar in graph units.
				 * The default width is 1.0 units. */
    BarMode mode;		/* Mode describing how to display bars
				 * with the same x-coordinates. Mode can
				 * be "stack", "align", or "normal" */
    FreqInfo *freqArr;		/* Contains information about duplicate
				 * x-values in bar elements (malloc-ed).
				 * This information can also be accessed
				 * by the frequency hash table */
    Tcl_HashTable freqTable;	/* */
    int numStacks;		/* Number of entries in frequency array.
				 * If zero, indicates nothing special needs
				 * to be done for "stack" or "align" modes */

};

/*
 * Flag bits for graphs:
 * 	All kinds of state information kept here.  All these
 *	things happen when the window is available to draw into
 *	(DisplayGraph). Need the window width and height before
 *	we can calculate graph layout (i.e. the screen coordinates
 *	of the axes, elements, titles, etc). But we want to do this
 *	only when we have to, not every time the graph is redrawn.
 *
 *	Same goes for maintaining a pixmap to double buffer graph
 *	elements.  Need to mark when the pixmap needs to updated.
 *
 */
#define	COORDS_ALL_PARTS (1<<1)	/* Indicates that the layout
				 * of the axes and all elements and
				 * markers and the graph need to be
				 * recalculated. Otherwise, the layout
				 * of only those markers and elements that
				 * have changed will be reset. */


#define	GET_AXIS_GEOMETRY (1<<2)/* Indicates that the extents of the
				  * axes needs to be recalculated. */

#define RESET_AXES	  (1<<3)/* Flag to call to Blt_ResetAxes
				 * routine.  This routine recalculates the
				 * scale offset (used for mapping
				 * coordinates) of each axis.  If an axis
				 * min or max limit has changed, then it
				 * sets flags to relayout and redraw the
				 * entire graph.  This routine also needs
				 * to be called before the axis can be
				 * used to compute transformations between
				 * graph and screen coordinates. */

#define LAYOUT_NEEDED	(1<<4)
#define	REDRAW_BACKING_STORE	(1<<5)	/* If set, redraw all elements into the
				 * pixmap used for buffering elements. */

#define REDRAW_PENDING 	(1<<6)	/* Non-zero means a DoWhenIdle
				 * handler has already been queued to
				 * redraw this window. */

#define DRAW_LEGEND     (1<<7)	/* Non-zero means redraw the legend.
				 * If this is the only DRAW_ flag, the
				 * legend display routine is called instead
				 * of the graph display routine. */

#define DRAW_PLOT_AREA	(1<<8)
#define REDRAW_MARGINS	(1<<9)	/* Non-zero means that the region that
				 * borders the plotting surface of the
				 * graph needs to be redrawn. The
				 * possible reasons are:
				 *
				 * 1) an axis configuration changed
				 *
				 * 2) an axis limit changed
				 *
				 * 3) titles have changed
				 *
				 * 4) window was resized. */

#define	COORDS_WORLD 	(COORDS_ALL_PARTS|RESET_AXES|GET_AXIS_GEOMETRY|REDRAW_BACKING_STORE)
#define REDRAW_WORLD	(DRAW_PLOT_AREA | REDRAW_MARGINS | DRAW_LEGEND)

#define SET_ALL_FLAGS	(REDRAW_WORLD | COORDS_WORLD | RESET_AXES)

/*
 * ---------------------- Forward declarations ------------------------
 */

extern int Blt_CreateLegend _ANSI_ARGS_((Graph *graphPtr));
extern int Blt_CreatePostScript _ANSI_ARGS_((Graph *graphPtr));
extern int Blt_CreateCrosshairs _ANSI_ARGS_((Graph *graphPtr));
extern int Blt_CreateGrid _ANSI_ARGS_((Graph *graphPtr));
extern Point2D Blt_InvTransform2DPt _ANSI_ARGS_((Graph *graphPtr, double x,
	double y, Axis2D * pairPtr));
extern Point2D Blt_Transform2DPt _ANSI_ARGS_((Graph *graphPtr, double x,
	double y, Axis2D * pairPtr));
extern Graph *Blt_FindGraph _ANSI_ARGS_((Tk_Window tkwin));
extern void Blt_AdjustAxisPointers _ANSI_ARGS_((Graph *graphPtr));
extern int Blt_ClipSegment _ANSI_ARGS_((Extents2D * extentsPtr, Point2D * p1Ptr,
	Point2D * p2Ptr, XSegment *segPtr));
extern void Blt_ComputeStacks _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_ConfigureCrosshairs _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_DestroyAxes _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_DestroyCrosshairs _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_DestroyGrid _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_DestroyElements _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_DestroyLegend _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_DestroyMarkers _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_DestroyPostScript _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_DrawAxes _ANSI_ARGS_((Graph *graphPtr, Drawable drawable));
extern void Blt_DrawAxisLimits _ANSI_ARGS_((Graph *graphPtr, 
	Drawable drawable));
extern void Blt_DrawElements _ANSI_ARGS_((Graph *graphPtr, Drawable drawable));
extern void Blt_DrawActiveElements _ANSI_ARGS_((Graph *graphPtr, 
	Drawable drawable));
extern void Blt_DrawGraph _ANSI_ARGS_((Graph *graphPtr, Drawable drawable, 
	int backingStore));
extern void Blt_DrawGrid _ANSI_ARGS_((Graph *graphPtr, Drawable drawable));
extern void Blt_DrawLegend _ANSI_ARGS_((Graph *graphPtr, Drawable drawable));
extern void Blt_DrawMarkers _ANSI_ARGS_((Graph *graphPtr, Drawable drawable, 
	int under));
extern int Blt_GetAxisMargin _ANSI_ARGS_((Axis *axisPtr));
extern int Blt_GetCoordinate _ANSI_ARGS_((Tcl_Interp *interp,
	char *expr, double *valuePtr));
extern int Blt_GetElement _ANSI_ARGS_((Graph *graphPtr, char *name,
	Element **elemPtrPtr));
extern void Blt_InitFreqTable _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_LayoutGraph _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_LayoutMargins _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_LayoutLegend _ANSI_ARGS_((Graph *graphPtr, int width,
	int height));
extern void Blt_EventuallyRedrawGraph _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_ResetAxes _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_ResetStacks _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_SetClipRegion _ANSI_ARGS_((Graph *graphPtr, Extents2D * extentsPtr));
extern void Blt_DisableCrosshairs _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_EnableCrosshairs _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_TransformAxis _ANSI_ARGS_((Graph *graphPtr,
	Axis *axisPtr));
extern void Blt_TransformElements _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_TransformGraph _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_TransformMarkers _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_TransformGrid _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_UpdateCrosshairs _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_DestroyPens _ANSI_ARGS_((Graph *graphPtr));
extern int Blt_GetPen _ANSI_ARGS_((Graph *graphPtr, char *name, ObjectType type,
	Pen **penPtrPtr));
extern Pen *Blt_BarPen _ANSI_ARGS_((char *penName));
extern Pen *Blt_LinePen _ANSI_ARGS_((char *penName));
extern Pen *Blt_CreatePen _ANSI_ARGS_((Graph *graphPtr, char *penName, 
	ObjectType type, int numOpts, char **options));
extern int Blt_InitLinePens _ANSI_ARGS_((Graph *graphPtr));
extern int Blt_InitBarPens _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_FreePen _ANSI_ARGS_((Graph *graphPtr, Pen *penPtr));

extern int Blt_VirtualAxisOp _ANSI_ARGS_((Graph *graphPtr, Tcl_Interp *interp,
	int argc, char **argv));
extern int Blt_AxisOp _ANSI_ARGS_((Graph *graphPtr, Axis *axisPtr, int argc,
	char **argv));
extern int Blt_ElementOp _ANSI_ARGS_((Graph *graphPtr, Tcl_Interp *interp,
	int argc, char **argv, ObjectType type));
extern int Blt_GridOp _ANSI_ARGS_((Graph *graphPtr, Tcl_Interp *interp,
	int argc, char **argv));
extern int Blt_CrosshairsOp _ANSI_ARGS_((Graph *graphPtr, Tcl_Interp *interp,
	int argc, char **argv));
extern int Blt_LegendOp _ANSI_ARGS_((Graph *graphPtr, Tcl_Interp *interp,
	int argc, char **argv));
extern int Blt_MarkerOp _ANSI_ARGS_((Graph *graphPtr, Tcl_Interp *interp,
	int argc, char **argv));
extern int Blt_PenOp _ANSI_ARGS_((Graph *graphPtr, Tcl_Interp *interp,
	int argc, char **argv));
extern int Blt_PostScriptOp _ANSI_ARGS_((Graph *graphPtr, Tcl_Interp *interp,
	int argc, char **argv));
extern int Blt_GraphUpdateNeeded _ANSI_ARGS_((Graph *graphPtr));
extern int Blt_DefaultAxes _ANSI_ARGS_((Graph *graphPtr));
extern void Blt_UpdateAxisBackgrounds _ANSI_ARGS_((Graph *graphPtr));
extern int Blt_GetAxisSegments _ANSI_ARGS_((Graph *graphPtr,
	VirtualAxis * axisPtr, XSegment **segPtrPtr));

extern ObjectType Blt_GetElementType _ANSI_ARGS_((char *string));
extern char *Blt_NameOfElementType _ANSI_ARGS_((ObjectType type));
extern int Blt_NameToElement _ANSI_ARGS_((Graph *graphPtr, char *name, 
	Element **elemPtrPtr));
extern Marker *Blt_NearestMarker _ANSI_ARGS_((Graph *graphPtr, int x, int y, 
	int under));
extern int Blt_NameToMarker _ANSI_ARGS_((Graph *graphPtr, char *name, 
	Marker **markerPtrPtr));

typedef ClientData (MakeTagProc) _ANSI_ARGS_((Graph *graphPtr, char *tagName));
extern ClientData Blt_MakeElementTag _ANSI_ARGS_((Graph *graphPtr, 
	char *tagName));
extern ClientData Blt_MakeMarkerTag _ANSI_ARGS_((Graph *graphPtr, 
	char *tagName));


/* ---------------------- Global declarations ------------------------ */

extern double bltNegInfinity, bltPosInfinity;

#endif /* _GRAPH_H */
