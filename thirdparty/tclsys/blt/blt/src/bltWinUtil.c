/*
 * bltWinUtil.c --
 *
 *	This module contains WIN32 routines not included in the Tcl/Tk
 *	libraries.
 *
 * Copyright 1998 by Bell Labs Innovations for Lucent Technologies.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that the
 * copyright notice and warranty disclaimer appear in supporting documentation,
 * and that the names of Lucent Technologies any of their entities not be used
 * in advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 *
 * Lucent Technologies disclaims all warranties with regard to this software,
 * including all implied warranties of merchantability and fitness.  In no event
 * shall Lucent Technologies be liable for any special, indirect or
 * consequential damages or any damages whatsoever resulting from loss of use,
 * data or profits, whether in an action of contract, negligence or other
 * tortuous action, arising out of or in connection with the use or performance
 * of this software.
 *
 */

#include <bltInt.h>
#include <X11/Xutil.h>
#include <X11/Xlib.h>

#include <windowsx.h>


/*
 * Data structure for setting graphics context.
 */
typedef struct {
    int function;		/* logical operation */
    unsigned long plane_mask;	/* plane mask */
    unsigned long foreground;	/* foreground pixel */
    unsigned long background;	/* background pixel */
    int line_width;		/* line width */
    int line_style;	 	/* LineSolid, LineOnOffDash, LineDoubleDash */
    int cap_style;	  	/* CapNotLast, CapButt, 
				   CapRound, CapProjecting */
    int join_style;	 	/* JoinMiter, JoinRound, JoinBevel */
    int fill_style;	 	/* FillSolid, FillTiled, 
				   FillStippled, FillOpaeueStippled */
    int fill_rule;	  	/* EvenOddRule, WindingRule */
    int arc_mode;		/* ArcChord, ArcPieSlice */
    Pixmap tile;		/* tile pixmap for tiling operations */
    Pixmap stipple;		/* stipple 1 plane pixmap for stipping */
    int ts_x_origin;		/* offset for tile or stipple operations */
    int ts_y_origin;
    Font font;			/* default text font for text operations */
    int subwindow_mode;		/* ClipByChildren, IncludeInferiors */
    Bool graphics_exposures;	/* boolean, should exposures be generated */
    int clip_x_origin;		/* origin for clipping */
    int clip_y_origin;
    Pixmap clip_mask;		/* bitmap clipping; other calls for rects */
    int dash_offset;		/* patterned/dashed line information */
    char dashes;		/* Should be -1 to flag extended information */
    int numDashValues;
    char dashValues[12];
} XGCValuesExt, *GCExt;

static int tkpWinRopModes[] =
{
    R2_BLACK,			/* GXclear */
    R2_MASKPEN,			/* GXand */
    R2_MASKPENNOT,		/* GXandReverse */
    R2_COPYPEN,			/* GXcopy */
    R2_MASKNOTPEN,		/* GXandInverted */
    R2_NOT,			/* GXnoop */
    R2_XORPEN,			/* GXxor */
    R2_MERGEPEN,		/* GXor */
    R2_NOTMERGEPEN,		/* GXnor */
    R2_NOTXORPEN,		/* GXequiv */
    R2_NOT,			/* GXinvert */
    R2_MERGEPENNOT,		/* GXorReverse */
    R2_NOTCOPYPEN,		/* GXcopyInverted */
    R2_MERGENOTPEN,		/* GXorInverted */
    R2_NOTMASKPEN,		/* GXnand */
    R2_WHITE			/* GXset */
};

static HPEN GCToPen _ANSI_ARGS_((HDC dc, GC gc));

void *
Blt_Calloc(unsigned int numElems, unsigned int sizeOfElem)
{
    void *array;
    unsigned int numBytes;

    numBytes = numElems * sizeOfElem;
    array = Tcl_Alloc(numBytes);
    memset(array, 0, numBytes);
    return array;
}

char *
Blt_StrDup(const char *string)
{
    unsigned int numBytes;
    char *array;

    numBytes = strlen(string) + 1;
    array = (char *)Tcl_Alloc(numBytes * sizeof(char));
    strcpy(array, string);
    return array;
}

double
drand48()
{
    return (double) rand() / (double)RAND_MAX;
}

void
srand48(seed)
    unsigned int seed;
{
    srand(seed);
}

int
Blt_GetPlatformId(void)
{
    static int platformId = 0;

    if (platformId == 0) {
	OSVERSIONINFO opsysInfo;

	opsysInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	if (GetVersionEx(&opsysInfo)) {
	    platformId = opsysInfo.dwPlatformId;
	}
    }
    return platformId;
}

char *
Blt_Win32Error(void)
{
    static char buffer[1024];
    int length;

    FormatMessage(
	FORMAT_MESSAGE_FROM_SYSTEM,
	NULL, 
	GetLastError(), 
	MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), /* Default language */
	buffer,
	1024,
	NULL);
    length = strlen(buffer);
    if (buffer[length - 2] == '\r') {
	buffer[length - 2] = '\0';
    }
    return buffer;
}

/*
 *----------------------------------------------------------------------
 *
 * XFree --
 *
 *----------------------------------------------------------------------
 */
void
XFree(void *ptr)
{
    free(ptr);
}

/*
 *----------------------------------------------------------------------
 *
 * XMaxRequestSize --
 *
 *----------------------------------------------------------------------
 */
long
XMaxRequestSize(display)
    Display *display;
{
    return USHRT_MAX;
}

/*
 *----------------------------------------------------------------------
 *
 * XGetWindowAttributes --
 *
 *----------------------------------------------------------------------
 */
int
XGetWindowAttributes(display, window, attrPtr)
    Display *display;
    Window window;
    XWindowAttributes *attrPtr;
{
    HWND hWnd;

    hWnd = (HWND) (window);
    attrPtr->map_state = IsUnviewable;
    if (IsWindowVisible(hWnd)) {
	RECT region;

	attrPtr->map_state = IsViewable;
	if (GetWindowRect(hWnd, &region)) {
	    attrPtr->x = region.left;
	    attrPtr->y = region.top;
	    attrPtr->width = region.right - region.left;
	    attrPtr->height = region.bottom - region.top;
	    return TRUE;
	}
    }
    return FALSE;
}

/*
 *----------------------------------------------------------------------
 *
 * XLowerWindow --
 *
 *----------------------------------------------------------------------
 */
void
XLowerWindow(display, window)
    Display *display;
    Window window;
{
    HWND hWnd;

    hWnd = Tk_GetHWND(window);
    display->request++;
    SetWindowPos(hWnd, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
}

/*
 *----------------------------------------------------------------------
 *
 * XWarpPointer --
 *
 *	If destWindow is None, moves the pointer by the offsets (destX,
 *	destY) relative to the current position of the pointer.
 *	If destWindow is a window, moves the pointer to the offsets
 *	(destX, destY) relative to the origin of destWindow.  However,
 *	if srcWindow is a window, the move only takes place if the window
 *	srcWindow contains the pointer and if the specified rectangle of
 *	srcWindow contains the pointer.
 *
 *	The srcX and srcY coordinates are relative to the origin of
 *	srcWindow.  If srcHeight is zero, it is replaced with the current
 *	height of srcWindow minus srcY.  If srcWidth is zero, it is
 *	replaced with the current width of srcWindow minus srcX.
 *
 *----------------------------------------------------------------------
 */
void
XWarpPointer(display, srcWindow, destWindow, srcX, srcY, srcWidth, srcHeight,
    destX, destY)
    Display *display;
    Window srcWindow, destWindow;
    int srcX, srcY;
    unsigned int srcWidth, srcHeight;
    int destX, destY;
{
    HWND hWnd;
    POINT point;

    hWnd = Tk_GetHWND(destWindow);
    point.x = destX, point.y = destY;
    if (ClientToScreen(hWnd, &point)) {
	SetCursorPos(point.x, point.y);
    }
}

static Tcl_HashTable gcTable;
static int gcInitialized = FALSE;

typedef struct DashInfo {
    HDC dc;
    int count;
    COLORREF color;
    int offset, numBits;
} DashInfo;

void
Blt_SetDashes(display, gc, dashesPtr)
    Display *display;
    GC gc;
    Dashes *dashesPtr;
{
    register int i;
    GCExt gcExt = (GCExt)gc;

    /* This must be used only with a privately created GC */ 
    assert((int)gcExt->dashes == -1);

    gcExt->numDashValues = dashesPtr->numValues;
    gcExt->dash_offset = dashesPtr->offset;
    for(i = 0; i < dashesPtr->numValues; i++) {
	gcExt->dashValues[i] = dashesPtr->valueArr[i];
    }
}

static int
GetDashInfo(HDC dc, GC gc, DashInfo *infoPtr)
{
    int dashOffset, dashValue;
    
    dashValue = 0;
    dashOffset = gc->dash_offset;
    if ((int)gc->dashes == -1) {
	GCExt gcExt = (GCExt)gc;

	if (gcExt->numDashValues == 1) {
	    dashValue = gcExt->dashValues[0];
	}
    } else if (gc->dashes > 0) {
	dashValue = (int)gc->dashes;
    } 
    if (dashValue == 0) {
	return FALSE;
    }
    infoPtr->dc = dc;
    infoPtr->numBits = dashValue;
    infoPtr->offset = dashOffset;
    infoPtr->count = 0;
    infoPtr->color = gc->foreground;
    PurifyPrintf("offset is %d, numBits = %d\n", infoPtr->offset, 
	infoPtr->numBits);
    return TRUE;
}


/*
 *----------------------------------------------------------------------
 *
 * Blt_CreateGC --
 *
 *	Allocate a new extended GC, and initialize the specified fields.
 *
 * Results:
 *	Returns a newly allocated GC. 
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

GC
Blt_CreateGC(display, d, mask, valuesPtr)
    Display* display;
    Drawable d;
    unsigned long mask;
    XGCValues* valuesPtr;
{
    GCExt gcExt;

    gcExt = (GCExt)malloc(sizeof(XGCValuesExt));
    if (gcExt == NULL) {
	return None;
    }
    gcExt->function = (mask & GCFunction) 
	? valuesPtr->function : GXcopy;
    gcExt->plane_mask = (mask & GCPlaneMask) 
	? valuesPtr->plane_mask : ~0;
    gcExt->foreground = (mask & GCForeground) 
	? valuesPtr->foreground	: 0;
    gcExt->background = (mask & GCBackground) 	
	? valuesPtr->background : 0xffffff;
    gcExt->line_width = (mask & GCLineWidth) 
	? valuesPtr->line_width	: 0;	
    gcExt->line_style = (mask & GCLineStyle)
	? valuesPtr->line_style	: LineSolid;
    gcExt->cap_style = (mask & GCCapStyle) 
	? valuesPtr->cap_style : CapNotLast;
    gcExt->join_style = (mask & GCJoinStyle) 
	? valuesPtr->join_style : JoinMiter;
    gcExt->fill_style = (mask & GCFillStyle)	
	? valuesPtr->fill_style	: FillSolid;
    gcExt->fill_rule = (mask & GCFillRule)	
	? valuesPtr->fill_rule : WindingRule;
    gcExt->arc_mode = (mask & GCArcMode)	
	? valuesPtr->arc_mode : ArcPieSlice;
    gcExt->tile = (mask & GCTile) 
	? valuesPtr->tile : None;
    gcExt->stipple = (mask & GCStipple)	
	? valuesPtr->stipple : None;
    gcExt->ts_x_origin = (mask & GCTileStipXOrigin)
	? valuesPtr->ts_x_origin : 0;
    gcExt->ts_y_origin = (mask & GCTileStipYOrigin)
	? valuesPtr->ts_y_origin : 0;
    gcExt->font = (mask & GCFont) 
	? valuesPtr->font : None;
    gcExt->subwindow_mode = (mask & GCSubwindowMode)
	? valuesPtr->subwindow_mode : ClipByChildren;
    gcExt->graphics_exposures = (mask & GCGraphicsExposures)
	? valuesPtr->graphics_exposures : True;
    gcExt->clip_x_origin = (mask & GCClipXOrigin)	
	? valuesPtr->clip_x_origin : 0;
    gcExt->clip_y_origin = (mask & GCClipYOrigin)	
	? valuesPtr->clip_y_origin : 0;
    gcExt->dash_offset = (mask & GCDashOffset)	
	? valuesPtr->dash_offset : 0;
    gcExt->dashes = (mask & GCDashList)	
	? valuesPtr->dashes : -1;   /* Mark that this an extended GC */
    gcExt->numDashValues = 0;
    if (mask & GCClipMask) {
	struct ClipMask {
	    int type;		/* One of TKP_CLIP_PIXMAP or TKP_CLIP_REGION */
	    Pixmap pixmap;
	} *clipPtr;
	clipPtr = (struct ClipMask *)malloc(sizeof(struct ClipMask));
#define TKP_CLIP_PIXMAP 0
	clipPtr->type = TKP_CLIP_PIXMAP;
	clipPtr->pixmap = valuesPtr->clip_mask;
	gcExt->clip_mask = (Pixmap)clipPtr;
    } else {
	gcExt->clip_mask = None;
    }
    return (GC)gcExt;
}

/*
 *----------------------------------------------------------------------
 *
 * GCToPen --
 *
 *	Set up the graphics port from the given GC.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The current port is adjusted.
 *
 *----------------------------------------------------------------------
 */
static HPEN
GCToPen(dc, gc)
    HDC dc;
    GC gc;
{
    DWORD lineAttrs, lineStyle;
    DWORD dashArr[12];
    DWORD *dashPtr;
    int numValues, lineWidth;
    LOGBRUSH lBrush;
    HPEN pen;

    numValues = 0;
    lineWidth = (gc->line_width < 1) ? 1 : gc->line_width;
    if ((gc->line_style == LineOnOffDash) ||
	(gc->line_style == LineDoubleDash)) {
	if ((int)gc->dashes == -1) {
	    GCExt gcExt = (GCExt)gc;
	    register int i;

	    numValues = gcExt->numDashValues;
	    for (i = 0; i < numValues; i++) {
		dashArr[i] = (DWORD)gcExt->dashValues[i];
	    }
	} else {
	    dashArr[0] = (int)gc->dashes;
	    numValues = 1;
	}
    }
    switch (numValues) {
    case 0:
	lineStyle = PS_SOLID;
	break;
    case 3:
	lineStyle = PS_DASHDOT;
	break;
    case 4:
	lineStyle = PS_DASHDOTDOT;
	break;
    case 2:
    default:
	/* PS_DASH style dash length is too long. */
	lineStyle = PS_DOT;
	break;
    }

    lBrush.lbStyle = BS_SOLID;
    lBrush.lbColor = gc->foreground;
    lBrush.lbHatch = 0;		/* Value is ignored with style is BS_SOLID */

    lineAttrs = 0;
    switch (gc->cap_style) {
    case CapNotLast:
    case CapButt:
	lineAttrs |= PS_ENDCAP_FLAT;
	break;
    case CapRound:
	lineAttrs |= PS_ENDCAP_ROUND;
	break;
    default:
	lineAttrs |= PS_ENDCAP_SQUARE;
	break;
    }
    switch (gc->join_style) {
    case JoinMiter:
	lineAttrs |= PS_JOIN_MITER;
	break;
    case JoinRound:
	lineAttrs |= PS_JOIN_ROUND;
	break;
    default:
	lineAttrs |= PS_JOIN_BEVEL;
	break;
    }
    pen = NULL;
    SetBkMode(dc, TRANSPARENT);
    if ((lineStyle == PS_SOLID) ||
	(Blt_GetPlatformId() == VER_PLATFORM_WIN32_NT)) {
	/*
	 * If the line style is solid or we're running on NT, first
	 * try to use a geometric pen.
	 */
	if (numValues > 0) {
	    lineStyle = PS_USERSTYLE;
	    dashPtr = dashArr;
	} else {
	    dashPtr = NULL;
	}
	pen = ExtCreatePen(PS_GEOMETRIC | lineAttrs | lineStyle, lineWidth,
	    &lBrush, numValues, dashPtr);
    } else {
	/* Windows 95: we'll sacrifice thick lines for dashes. */
	pen = ExtCreatePen(PS_COSMETIC | lineAttrs | lineStyle, 1, &lBrush, 
		0, NULL);
    }
    if (pen == NULL) {
	pen = CreatePen(PS_COSMETIC | PS_SOLID, 1, gc->foreground);
    }
    assert(pen != NULL);
    return pen;
}

/*
 *----------------------------------------------------------------------
 *
 * XDrawRectangles --
 *
 *       Draws the outlines of the specified rectangles as if a
 *       five-point PolyLine protocol request were specified for each
 *       rectangle:
 *
 *             [x,y] [x+width,y] [x+width,y+height] [x,y+height]
 *             [x,y]
 *
 *      For the specified rectangles, these functions do not draw a
 *      pixel more than once.  XDrawRectangles draws the rectangles in
 *      the order listed in the array.  If rectangles intersect, the
 *      intersecting pixels are drawn multiple times.  Draws a
 *      rectangle.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Draws rectangles on the specified drawable.
 *
 *----------------------------------------------------------------------
 */
void
XDrawRectangles(display, drawable, gc, rectArr, numRects)
    Display *display;
    Drawable drawable;
    GC gc;
    XRectangle *rectArr;
    int numRects;
{
    HPEN pen, oldPen;
    TkWinDCState state;
    HBRUSH brush, oldBrush;
    HDC dc;
    register XRectangle *rectPtr;
    register int i;

    if (drawable == None) {
	return;
    }
    dc = TkWinGetDrawableDC(display, drawable, &state);
    pen = GCToPen(dc, gc);
    brush = GetStockObject(NULL_BRUSH);
    oldPen = SelectPen(dc, pen);
    oldBrush = SelectBrush(dc, brush);
    SetROP2(dc, tkpWinRopModes[gc->function]);
    rectPtr = rectArr;
    for (i = 0; i < numRects; i++, rectPtr++) {
	Rectangle(dc, (int)rectPtr->x, (int)rectPtr->y,
	    (int)(rectPtr->x + rectPtr->width + 1),
	    (int)(rectPtr->y + rectPtr->height + 1));
    }
    DeletePen(SelectPen(dc, oldPen));
    DeleteBrush(SelectBrush(dc, oldBrush));
    TkWinReleaseDrawableDC(drawable, dc, &state);
}

#ifdef notdef
/*
 * Implements the "pixeling" of small arcs, because GDI-performance
 * for this is awful
 * was made especially for BLT, graph4 demo now runs 4x faster
 *
 */
/* O-outer , I-inner, B-both */
#define NEITHER_ 0
#define OUTLINE 1
#define FILL 2
#define BOTH (OUTLINE|FILL)
#define MINIARCS 5
static int arcus0[1]={
    BOTH
};
static int arcus1[4]={
    BOTH,BOTH,
    BOTH,BOTH
};
 
static int arcus2[9] ={
    NEITHER,OUTLINE,NEITHER,
    OUTLINE,FILL,OUTLINE,
    NEITHER,OUTLINE,NEITHER
};

static int arcus3[16]={
    NEITHER,OUTLINE,OUTLINE,NEITHER,
    OUTLINE,FILL,FILL,OUTLINE,
    OUTLINE,FILL,FILL,OUTLINE,
    NEITHER,OUTLINE,OUTLINE,NEITHER
};

static int arcus4[25]={
    NEITHER,OUTLINE,OUTLINE,OUTLINE,NEITHER,
    OUTLINE,FILL,FILL,FILL,OUTLINE,
    OUTLINE,FILL,FILL,FILL,OUTLINE,
    OUTLINE,FILL,FILL,FILL,OUTLINE,
    NEITHER,OUTLINE,OUTLINE,OUTLINE,NEITHER
};

static int* arcis[MINIARCS]={
    arcus0, arcus1, arcus2, arcus3, arcus4
};

static void 
DrawMiniArc(
    HDC dc, 
    int width, 
    int x, 
    int y, 
    int mask,
    COLORREF inner,
    COLORREF outer) 
{
    int *arc;
    int i, j;

    if (width > MINIARCS) {
	return;
    }
    arc = arcis[width];
    for (i=0; i <= width; i++) {
	for(j=0;j <= width; j++) {
	    bit = (mask & *arc);
	    if (bit & OUTLINE) {
		SetPixelV(dc, x + i, y + j, outer);
	    } else if (bit & FILL) {
		SetPixelV(dc, x + i, y + j, inner);
	    }
	    arc++;
	}
    }
}
#endif

/*
 *----------------------------------------------------------------------
 *
 * DrawArc --
 *
 *	This procedure handles the rendering of drawn or filled
 *	arcs and chords.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Renders the requested arcs.
 *
 *----------------------------------------------------------------------
 */
static void
DrawArc(dc, arcMode, arcPtr, pen, brush)
    HDC dc;
    int arcMode;		/* Mode: either ArcChord or ArcPieSlice */
    XArc *arcPtr;
    HPEN pen;
    HBRUSH brush;
{
    int start, extent, clockwise;
    int xstart, ystart, xend, yend;
    double radian_start, radian_end, xr, yr;
    double dx, dy;

    if ((arcPtr->angle1 == 0) && (arcPtr->angle2 == 23040)) {
	/* Handle special case of circle or ellipse */ 
	Ellipse(dc, arcPtr->x, arcPtr->y, arcPtr->x + arcPtr->width + 1,
		    arcPtr->y + arcPtr->height + 1);
	return;
    }
    start = arcPtr->angle1, extent = arcPtr->angle2;
    clockwise = (extent < 0);	/* Non-zero if clockwise */

    /*
     * Compute the absolute starting and ending angles in normalized radians.
     * Swap the start and end if drawing clockwise.
     */
    start = start % (64 * 360);
    if (start < 0) {
	start += (64 * 360);
    }
    extent = (start + extent) % (64 * 360);
    if (extent < 0) {
	extent += (64 * 360);
    }
    if (clockwise) {
	int tmp = start;
	start = extent;
	extent = tmp;
    }
#define XAngleToRadians(a) ((double)(a) / 64 * M_PI / 180);
    radian_start = XAngleToRadians(start);
    radian_end = XAngleToRadians(extent);

    /*
     * Now compute points on the radial lines that define the starting and
     * ending angles.  Be sure to take into account that the y-coordinate
     * system is inverted.
     */
    dx = arcPtr->width * 0.5;
    dy = arcPtr->height * 0.5;

    xr = arcPtr->x + dx;
    yr = arcPtr->y + dy;
    xstart = (int)((xr + cos(radian_start) * dx) + 0.5);
    ystart = (int)((yr + sin(-radian_start) * dy) + 0.5);
    xend = (int)((xr + cos(radian_end) * dx) + 0.5);
    yend = (int)((yr + sin(-radian_end) * dy) + 0.5);

    /*
     * Now draw a filled or open figure.  Note that we have to
     * increase the size of the bounding box by one to account for the
     * difference in pixel definitions between X and Windows.
     */

    if (brush == 0) {
	/*
	 * Note that this call will leave a gap of one pixel at the
	 * end of the arc for thin arcs.  We can't use ArcTo because
	 * it's only supported under Windows NT.
	 */
	Arc(dc, arcPtr->x, arcPtr->y, arcPtr->x + arcPtr->width + 1,
	    arcPtr->y + arcPtr->height + 1, xstart, ystart, xend, yend);
	/* FIXME: */
    } else {
	if (arcMode == ArcChord) {
	    Chord(dc, arcPtr->x, arcPtr->y, arcPtr->x + arcPtr->width + 1,
		arcPtr->y + arcPtr->height + 1, xstart, ystart, xend, yend);
	} else if (arcMode == ArcPieSlice) {
	    Pie(dc, arcPtr->x, arcPtr->y, arcPtr->x + arcPtr->width + 1,
		arcPtr->y + arcPtr->height + 1, xstart, ystart, xend, yend);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * XDrawArcs --
 *
 *	Draws multiple circular or elliptical arcs.  Each arc is
 *	specified by a rectangle and two angles.  The center of the
 *	circle or ellipse is the center of the rect- angle, and the
 *	major and minor axes are specified by the width and height.
 *	Positive angles indicate counterclock- wise motion, and
 *	negative angles indicate clockwise motion.  If the magnitude
 *	of angle2 is greater than 360 degrees, XDrawArcs truncates it
 *	to 360 degrees.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Draws an arc for each array element on the specified drawable.
 *
 *----------------------------------------------------------------------
 */
void
XDrawArcs(display, drawable, gc, arcArr, numArcs)
    Display *display;
    Drawable drawable;
    GC gc;
    XArc *arcArr;
    int numArcs;
{
    HPEN pen, oldPen;
    HBRUSH brush, oldBrush;
    register int i;
    HDC dc;
    TkWinDCState state;
    register XArc *arcPtr;

    display->request++;
    if (drawable == None) {
	return;
    }

    dc = TkWinGetDrawableDC(display, drawable, &state);
    SetROP2(dc, tkpWinRopModes[gc->function]);
    pen = GCToPen(dc, gc);
    oldPen = SelectPen(dc, pen);
    brush = GetStockBrush(NULL_BRUSH);
    oldBrush = SelectBrush(dc, brush);
    for (arcPtr = arcArr, i = 0; i < numArcs; i++, arcPtr++) {
	DrawArc(dc, gc->arc_mode, arcPtr, pen, 0);
    }
    DeleteBrush(SelectBrush(dc, oldBrush));
    DeletePen(SelectPen(dc, oldPen));
    TkWinReleaseDrawableDC(drawable, dc, &state);
}

/*
 *----------------------------------------------------------------------
 *
 * XFillArcs --
 *
 *	Draw a filled arc.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Draws a filled arc for each array element on the specified drawable.
 *
 *----------------------------------------------------------------------
 */
void
XFillArcs(display, drawable, gc, arcArr, numArcs)
    Display *display;
    Drawable drawable;
    GC gc;
    XArc *arcArr;
    int numArcs;
{
    HBRUSH brush, oldBrush;
    HPEN pen, oldPen;
    register int i;
    HDC dc;
    register XArc *arcPtr;
    TkWinDCState state;

    display->request++;
    if (drawable == None) {
	return;
    }
    dc = TkWinGetDrawableDC(display, drawable, &state);
    SetROP2(dc, tkpWinRopModes[gc->function]);
    pen = GCToPen(dc, gc);
    oldPen = SelectPen(dc, pen);
    brush = CreateSolidBrush(gc->foreground);
    oldBrush = SelectBrush(dc, brush);
    for (arcPtr = arcArr, i = 0; i < numArcs; i++, arcPtr++) {
	DrawArc(dc, gc->arc_mode, arcPtr, pen, brush);
    }
    DeleteBrush(SelectBrush(dc, oldBrush));
    DeletePen(SelectPen(dc, oldPen));
    TkWinReleaseDrawableDC(drawable, dc, &state);
}

/*
 *----------------------------------------------------------------------
 *
 * XDrawLines --
 *
 *	Draw connected lines.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Renders a series of connected lines.
 *
 *----------------------------------------------------------------------
 */

/*
 *----------------------------------------------------------------------
 *
 * ConvertPoints --
 *
 *	Convert an array of X points to an array of Win32 points.
 *
 * Results:
 *	Returns the converted array of POINTs.
 *
 * Side effects:
 *	Allocates a block of memory that should not be freed.
 *
 *----------------------------------------------------------------------
 */

static POINT *
ConvertPoints(points, npoints, mode, bbox)
    XPoint *points;
    int npoints;
    int mode;			/* CoordModeOrigin or CoordModePrevious. */
    RECT *bbox;			/* Bounding box of points. */
{
    static POINT *winPoints = NULL;	/* Array of points that is reused. */
    static int nWinPoints = -1;	/* Current size of point array. */
    int i;

    /*
     * To avoid paying the cost of a malloc on every drawing routine,
     * we reuse the last array if it is large enough.
     */

    if (npoints > nWinPoints) {
	if (winPoints != NULL) {
	    ckfree((char *)winPoints);
	}
	winPoints = (POINT *)malloc(sizeof(POINT) * npoints);
	if (winPoints == NULL) {
	    nWinPoints = -1;
	    return NULL;
	}
	nWinPoints = npoints;
    }
    bbox->left = bbox->right = points[0].x;
    bbox->top = bbox->bottom = points[0].y;

    if (mode == CoordModeOrigin) {
	for (i = 0; i < npoints; i++) {
	    winPoints[i].x = points[i].x;
	    winPoints[i].y = points[i].y;
	    bbox->left = MIN(bbox->left, winPoints[i].x);
	    bbox->right = MAX(bbox->right, winPoints[i].x);
	    bbox->top = MIN(bbox->top, winPoints[i].y);
	    bbox->bottom = MAX(bbox->bottom, winPoints[i].y);
	}
    } else {
	winPoints[0].x = points[0].x;
	winPoints[0].y = points[0].y;
	for (i = 1; i < npoints; i++) {
	    winPoints[i].x = winPoints[i - 1].x + points[i].x;
	    winPoints[i].y = winPoints[i - 1].y + points[i].y;
	    bbox->left = MIN(bbox->left, winPoints[i].x);
	    bbox->right = MAX(bbox->right, winPoints[i].x);
	    bbox->top = MIN(bbox->top, winPoints[i].y);
	    bbox->bottom = MAX(bbox->bottom, winPoints[i].y);
	}
    }
    return winPoints;
}


static
void CALLBACK
DrawDot(
    int x, int y,		/* Coordinates of point */
    LPARAM clientData)
{				/* Line information */
    DashInfo *infoPtr = (DashInfo *)clientData;
    int count;

    infoPtr->count++;
    count = (infoPtr->count + infoPtr->offset) / infoPtr->numBits;
    if (count & 0x1) {
	SetPixelV(infoPtr->dc, x, y, infoPtr->color);
    }
}


void
XDrawLines(display, drawable, gc, pointArr, numPoints, mode)
    Display *display;
    Drawable drawable;
    GC gc;
    XPoint *pointArr;
    int numPoints;
    int mode;
{
    TkWinDCState state;
    HDC dc;

    if (drawable == None) {
	return;
    }
    dc = TkWinGetDrawableDC(display, drawable, &state);
    if (gc->line_style != LineSolid) {
	/* Handle dotted lines specially */
	register XPoint *p1, *p2;
	register int i;
	DashInfo info;

	if (!GetDashInfo(dc, gc, &info)) {
	    goto solidLine;
	}
	p1 = pointArr; p2 = p1 + 1;
	for (i = 1; i < numPoints; i++, p1++, p2++) {
	    LineDDA(p1->x, p1->y, p2->x, p2->y, DrawDot, (LPARAM)&info);
	}  
    } else {
	RECT rect;
	HPEN pen, oldPen;
	HBRUSH brush, oldBrush;
	POINT *pts;

    solidLine:
	pen = GCToPen(dc, gc);
	oldPen = SelectPen(dc, pen);
	brush = CreateSolidBrush(gc->foreground);
	oldBrush = SelectBrush(dc, brush);
	SetROP2(dc, tkpWinRopModes[gc->function]);
	pts = ConvertPoints(pointArr, numPoints, mode, &rect);
	Polyline(dc, pts, numPoints);
	DeletePen(SelectPen(dc, oldPen));
	DeleteBrush(SelectBrush(dc, oldBrush));
    }
    TkWinReleaseDrawableDC(drawable, dc, &state);
}

void
XDrawLine(Display *display,
    Drawable drawable,
    GC gc,
    int x1, int y1,
    int x2, int y2)
{
    TkWinDCState state;
    HDC dc;

    if (drawable == None) {
	return;
    }
    dc = TkWinGetDrawableDC(display, drawable, &state);
    if (gc->line_style != LineSolid) {
	/* Handle dotted lines specially */
	DashInfo info;

	if (!GetDashInfo(dc, gc, &info)) {
	    goto solidLine;
	}
	LineDDA(x1, y1, x2, y2, DrawDot, (LPARAM)&info);
    } else {
	HPEN pen, oldPen;
	HBRUSH brush, oldBrush;

    solidLine:
	pen = GCToPen(dc, gc);
	oldPen = SelectPen(dc, pen);
	brush = CreateSolidBrush(gc->foreground);
	oldBrush = SelectBrush(dc, brush);
	SetROP2(dc, tkpWinRopModes[gc->function]);
	MoveToEx(dc, x1, y1, (LPPOINT)NULL);
	LineTo(dc, x2, y2);
	DeletePen(SelectPen(dc, oldPen));
	DeleteBrush(SelectBrush(dc, oldBrush));
    }
    TkWinReleaseDrawableDC(drawable, dc, &state);
}

/*
 *----------------------------------------------------------------------
 *
 * XDrawSegments --
 *
 *	Draws multiple, unconnected lines. For each segment, draws a
 *	line between (x1, y1) and (x2, y2).  It draws the lines in the
 *	order listed in the array of XSegment structures and does not
 *	perform joining at coincident endpoints.  For any given line,
 *	does not draw a pixel more than once. If lines intersect, the
 *	intersecting pixels are drawn multiple times.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Draws unconnected line segments on the specified drawable.
 *
 *----------------------------------------------------------------------
 */
void
XDrawSegments(display, drawable, gc, segArr, numSegments)
    Display *display;
    Drawable drawable;
    GC gc;
    XSegment *segArr;
    int numSegments;
{
    register int i;
    HDC dc;
    register XSegment *segPtr;
    TkWinDCState state;

    display->request++;
    if (drawable == None) {
	return;
    }
    dc = TkWinGetDrawableDC(display, drawable, &state);
    SetROP2(dc, tkpWinRopModes[gc->function]);
    if (gc->line_style != LineSolid) {
	/* Handle dotted lines specially */
	DashInfo info;

	if (!GetDashInfo(dc, gc, &info)) {
	    goto solidLine;
	}
	for (segPtr = segArr, i = 0; i < numSegments; i++, segPtr++) {
	    info.count = 0;
	    LineDDA(segPtr->x1, segPtr->y1, segPtr->x2, segPtr->y2,
		DrawDot, (LPARAM)&info);
	}
    } else {
	HPEN pen, oldPen;

    solidLine:
	pen = GCToPen(dc, gc);
	oldPen = SelectPen(dc, pen);
	for (segPtr = segArr, i = 0; i < numSegments; i++, segPtr++) {
	    MoveToEx(dc, segPtr->x1, segPtr->y1, (LPPOINT) NULL);
	    LineTo(dc, segPtr->x2, segPtr->y2);
	}
	DeletePen(SelectPen(dc, oldPen));
    }
    TkWinReleaseDrawableDC(drawable, dc, &state);
}

/*
 *----------------------------------------------------------------------
 *
 * XDrawRectangle --
 *
 *       Draws the outlines of the specified rectangle as if a
 *       five-point PolyLine protocol request were specified for each
 *       rectangle:
 *
 *             [x,y] [x+width,y] [x+width,y+height] [x,y+height]
 *             [x,y]
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Draws a rectangle on the specified drawable.
 *
 *----------------------------------------------------------------------
 */
void
XDrawRectangle(Display *display,
    Drawable drawable,
    GC gc,
    int x, int y,
    unsigned int width,
    unsigned int height)
{
    TkWinDCState state;
    HPEN pen, oldPen;
    HBRUSH brush, oldBrush;
    HDC dc;

    if (drawable == None) {
	return;
    }
    dc = TkWinGetDrawableDC(display, drawable, &state);
    pen = GCToPen(dc, gc);
    brush = GetStockObject(NULL_BRUSH);
    oldPen = SelectPen(dc, pen);
    oldBrush = SelectBrush(dc, brush);
    SetROP2(dc, tkpWinRopModes[gc->function]);
    Rectangle(dc, x, y, x + width + 1, y + height + 1);
    DeletePen(SelectPen(dc, oldPen));
    DeleteBrush(SelectBrush(dc, oldBrush));
    TkWinReleaseDrawableDC(drawable, dc, &state);
}

/*
 *----------------------------------------------------------------------
 *
 * XDrawPoints --
 *
 *	Uses the foreground pixel and function components of the GC to
 *	draw a multiple points into the specified drawable.
 *      CoordModeOrigin treats all coordinates as relative to the
 *	origin, and CoordModePrevious treats all coordinates after
 *	the first as relative to the previous point.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Draws points on the specified drawable.
 *
 *----------------------------------------------------------------------
 */
void
XDrawPoints(display, drawable, gc, pointArr, numPoints, mode)
    Display *display;
    Drawable drawable;
    GC gc;
    XPoint *pointArr;
    int numPoints;
    int mode;			/* Ignored. CoordModeOrigin is assumed. */
{
    register int i;
    HDC dc;
    register XPoint *pointPtr;
    TkWinDCState state;

    display->request++;
    if (drawable == None) {
	return;
    }
    dc = TkWinGetDrawableDC(display, drawable, &state);
    SetROP2(dc, tkpWinRopModes[gc->function]);
    for (pointPtr = pointArr, i = 0; i < numPoints; i++, pointPtr++) {
	SetPixelV(dc, pointPtr->x, pointPtr->y, gc->foreground);
    }
    TkWinReleaseDrawableDC(drawable, dc, &state);
}

/*
 *----------------------------------------------------------------------
 *
 * XReparentWindow --
 *
 *	If the specified window is mapped, automatically performs an
 *	UnmapWindow request on it, removes it from its current
 *	position in the hierarchy, and inserts it as the child of the
 *	specified parent.  The window is placed in the stacking order
 *	on top with respect to sibling windows.
 *
 *	Note: In WIN32 you can't reparent to/from another application.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Reparents the specified window.
 *
 *----------------------------------------------------------------------
 */
void
XReparentWindow(display, window, parent, x, y)
    Display *display;
    Window window, parent;
    int x, y;
{
    HWND child, newParent;

    child = Tk_GetHWND(window);
    newParent = Tk_GetHWND(parent);
    SetParent(child, newParent);
    SetWindowLong(child, GWL_STYLE, WS_CHILD | WS_CLIPCHILDREN |
	WS_CLIPSIBLINGS);

    XMoveWindow(display, window, x, y);
    XRaiseWindow(display, window);
    XMapWindow(display, window);
}

void
XSetDashes(display, gc, dashOffset, dashList, n)
    Display *display;
    GC gc;
    int dashOffset;
    _Xconst char *dashList;
    int n;
{
    gc->dashes = (unsigned char)strlen(dashList);
    gc->dash_offset = (int)dashList;
}

/*
 *----------------------------------------------------------------------
 *
 * XDrawString --
 *
 *	Draw a single string in the current font.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Renders the specified string in the drawable.
 *
 *----------------------------------------------------------------------
 */

void
XDrawString(display, drawable, gc, x, y, string, numChars)
    Display *display;
    Drawable drawable;
    GC gc;
    int x, y;
    _Xconst char *string;
    int numChars;
{
    if (drawable == None) {
	return;
    }
    Tk_DrawChars(display, drawable, gc, (Tk_Font)gc->font, string,
	numChars, x, y);
}

#ifndef notdef
/*
 *----------------------------------------------------------------------
 *
 * XFillRectangles --
 *
 *	Fill multiple rectangular areas in the given drawable.
 *	Handles tiling.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Draws onto the specified drawable.
 *
 *----------------------------------------------------------------------
 */

void
XFillRectangles(display, d, gc, rectArr, numRectangles)
    Display *display;
    Drawable d;
    GC gc;
    XRectangle *rectArr;
    int numRectangles;
{
    HDC dc;
    int i;
    RECT rect;
    TkWinDCState state;
    HBRUSH brush;

    if (d == None) {
	return;
    }
    dc = TkWinGetDrawableDC(display, d, &state);
    SetROP2(dc, tkpWinRopModes[gc->function]);
    brush = CreateSolidBrush(gc->foreground);

    if (((gc->fill_style == FillStippled) ||
	    (gc->fill_style == FillOpaqueStippled)) &&
	(gc->stipple != None)) {
	TkWinDrawable *twdPtr = (TkWinDrawable *) gc->stipple;
	HBRUSH oldBrush, stipple;
	HBITMAP oldBitmap, bitmap;
	HDC dcMem;
	HBRUSH bgBrush = CreateSolidBrush(gc->background);

	if (twdPtr->type != TWD_BITMAP) {
	    panic("unexpected drawable type in stipple");
	}
	/*
	 * Select stipple pattern into destination dc.
	 */
	stipple = CreatePatternBrush(twdPtr->bitmap.handle);
	SetBrushOrgEx(dc, gc->ts_x_origin, gc->ts_y_origin, NULL);
	oldBrush = SelectBrush(dc, stipple);
	dcMem = CreateCompatibleDC(dc);

	/*
	 * For each rectangle, create a drawing surface which is the size of
	 * the rectangle and fill it with the background color.  Then merge the
	 * result with the stipple pattern.
	 */
/*
 * The following two raster ops are used to copy the foreground and background
 * bits of a source pattern as defined by a stipple used as the pattern.
 */

#define COPYFG		0x00CA0749	/* dest = (pat & src) | (!pat & dst) */
#define COPYBG		0x00AC0744	/* dest = (!pat & src) | (pat & dst) */

	for (i = 0; i < numRectangles; i++) {
	    bitmap = CreateCompatibleBitmap(dc, rectArr[i].width,
		rectArr[i].height);
	    oldBitmap = SelectBitmap(dcMem, bitmap);
	    rect.left = 0;
	    rect.top = 0;
	    rect.right = rectArr[i].width;
	    rect.bottom = rectArr[i].height;
	    FillRect(dcMem, &rect, brush);
	    BitBlt(dc, rectArr[i].x, rectArr[i].y, rectArr[i].width,
		rectArr[i].height, dcMem, 0, 0, COPYFG);
	    if (gc->fill_style == FillOpaqueStippled) {
		FillRect(dcMem, &rect, bgBrush);
		BitBlt(dc, rectArr[i].x, rectArr[i].y,
		    rectArr[i].width, rectArr[i].height, dcMem,
		    0, 0, COPYBG);
	    }
	    DeleteBitmap(SelectBitmap(dcMem, oldBitmap));
	}
	DeleteBrush(stipple);
    } else if (gc->fill_style == FillTiled) {
	TkWinDrawable *twdPtr = (TkWinDrawable *) gc->tile;
	HBRUSH oldBrush;
	HBRUSH tileBrush;
	/*
	 * Tile pattern into destination DC.
	 */
	tileBrush = CreatePatternBrush(twdPtr->bitmap.handle);
	SetBrushOrgEx(dc, gc->ts_x_origin, gc->ts_y_origin, NULL);
	oldBrush = SelectBrush(dc, tileBrush);
	for (i = 0; i < numRectangles; i++) {
	    PatBlt(dc, rectArr[i].x, rectArr[i].y, rectArr[i].width,
		rectArr[i].height, PATCOPY);
	}
	SelectBrush(dc, oldBrush);
	DeleteBrush(tileBrush);
    } else {
	for (i = 0; i < numRectangles; i++) {
	    TkWinFillRect(dc, rectArr[i].x, rectArr[i].y,
		rectArr[i].width, rectArr[i].height, gc->foreground);
	}
    }
    DeleteBrush(brush);
    TkWinReleaseDrawableDC(d, dc, &state);
}

void
XFillRectangle(display, d, gc, x, y, width, height)
    Display *display;
    Drawable d;
    GC gc;
    int x, y;
    unsigned int width, height;
{
    HDC dc;
    RECT rect;
    TkWinDCState state;
    HBRUSH brush;

    if (d == None) {
	return;
    }
    dc = TkWinGetDrawableDC(display, d, &state);
    SetROP2(dc, tkpWinRopModes[gc->function]);
    brush = CreateSolidBrush(gc->foreground);

    if (((gc->fill_style == FillStippled) ||
	    (gc->fill_style == FillOpaqueStippled)) &&
	(gc->stipple != None)) {
	TkWinDrawable *twdPtr = (TkWinDrawable *) gc->stipple;
	HBRUSH oldBrush, stipple;
	HBITMAP oldBitmap, bitmap;
	HDC dcMem;
	HBRUSH bgBrush = CreateSolidBrush(gc->background);

	if (twdPtr->type != TWD_BITMAP) {
	    panic("unexpected drawable type in stipple");
	}
	/*
	 * Select stipple pattern into destination dc.
	 */
	stipple = CreatePatternBrush(twdPtr->bitmap.handle);
	SetBrushOrgEx(dc, gc->ts_x_origin, gc->ts_y_origin, NULL);
	oldBrush = SelectBrush(dc, stipple);
	dcMem = CreateCompatibleDC(dc);

	/*
	 * For each rectangle, create a drawing surface which is the size of
	 * the rectangle and fill it with the background color.  Then merge the
	 * result with the stipple pattern.
	 */
/*
 * The following two raster ops are used to copy the foreground and background
 * bits of a source pattern as defined by a stipple used as the pattern.
 */

#define COPYFG		0x00CA0749	/* dest = (pat & src) | (!pat & dst) */
#define COPYBG		0x00AC0744	/* dest = (!pat & src) | (pat & dst) */

	bitmap = CreateCompatibleBitmap(dc, width, height);
	oldBitmap = SelectBitmap(dcMem, bitmap);
	rect.left = 0;
	rect.top = 0;
	rect.right = width;
	rect.bottom = height;
	FillRect(dcMem, &rect, brush);
	BitBlt(dc, x, y, width, height, dcMem, 0, 0, COPYFG);
	if (gc->fill_style == FillOpaqueStippled) {
	    FillRect(dcMem, &rect, bgBrush);
	    BitBlt(dc, x, y, width, height, dcMem, 0, 0, COPYBG);
	}
	DeleteBitmap(SelectBitmap(dcMem, oldBitmap));
	DeleteBrush(stipple);
    } else if (gc->fill_style == FillTiled) {
	TkWinDrawable *twdPtr = (TkWinDrawable *) gc->tile;
	HBRUSH oldBrush;
	HBRUSH tileBrush;
	/*
	 * Tile pattern into destination DC.
	 */
	tileBrush = CreatePatternBrush(twdPtr->bitmap.handle);
	SetBrushOrgEx(dc, gc->ts_x_origin, gc->ts_y_origin, NULL);
	oldBrush = SelectBrush(dc, tileBrush);
	PatBlt(dc, x, y, width, height, PATCOPY);
	SelectBrush(dc, oldBrush);
	DeleteBrush(tileBrush);
    } else {
	TkWinFillRect(dc, x, y, width, height, gc->foreground);
    }
    DeleteBrush(brush);
    TkWinReleaseDrawableDC(d, dc, &state);
}

#endif
