/*
 * tixUnixDraw.c --
 *
 *	Implement the Unix specific function calls for drawing.
 *
 * Copyright (c) 1996, Expert Interface Technologies
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include <tixPort.h>
#include <tixUnixInt.h>


/*
 *----------------------------------------------------------------------
 * TixpDrawTmpLine --
 *
 *	Draws a "temporary" line between the two points. The line can be
 *	removed by calling the function again with the same parameters.
 *
 * Results:
 *	Standard Tcl result.
 *
 * Side effects:
 *	A line is XOR'ed onto the screen.
 *----------------------------------------------------------------------
 */
void
TixpDrawTmpLine(x1, y1, x2, y2, tkwin)
    int x1;
    int y1;
    int x2;
    int y2;
    Tk_Window tkwin;
{
    GC gc;
    XGCValues values;
    unsigned long valuemask = GCForeground | GCSubwindowMode | GCFunction;
    Window winId;		/* The Window to draw into. */
    Tk_Window toplevel;		/* Toplevel containing the tkwin. */
    int rootx1, rooty1;		/* Root x and y of the toplevel window. */
    int rootx2, rooty2;

    for (toplevel=tkwin; !Tk_IsTopLevel(toplevel);
	    toplevel=Tk_Parent(toplevel)) {
	;
    }

    Tk_GetRootCoords(toplevel, &rootx1, &rooty1);
    rootx2 = rootx1 + Tk_Width(toplevel)  - 1;
    rooty2 = rooty1 + Tk_Height(toplevel) - 1;

    if (x1 >= rootx1 && x2 <= rootx2 &&	y1 >= rooty1 && y2 <= rooty2) {
	/*
	 * The line is completely inside the toplevel containing
	 * tkwin. It's better to draw into this window because on some
	 * X servers, especially PC X Servers running on Windows,
	 * drawing into the root window shows no effect.
	 */
	winId = Tk_WindowId(toplevel);
	x1 -= rootx1;
	y1 -= rooty1;
	x2 -= rootx1;
	y2 -= rooty1;
    } else {
	winId = XRootWindow(Tk_Display(tkwin), Tk_ScreenNumber(tkwin));
    }

    values.foreground	  = 0xff;
    values.subwindow_mode = IncludeInferiors;
    values.function	  = GXxor;

    gc = XCreateGC(Tk_Display(tkwin), winId, valuemask, &values);
    XDrawLine(Tk_Display(tkwin), winId, gc, x1, y1, x2, y2);
    XFreeGC(Tk_Display(tkwin), gc);
}

/*----------------------------------------------------------------------
 * TixpDrawAnchorLines --
 *
 *	See comments near Tix_DrawAnchorLines.
 *----------------------------------------------------------------------
 */

void TixpDrawAnchorLines(display, drawable, gc, x, y, w, h)
    Display *display;
    Drawable drawable;
    GC gc;
    int x;
    int y;
    int w;
    int h;
{
    XPoint points[4];

    if (w < 1) {
	w = 1;
    }
    if (h < 1) {
	h = 1;
    }

    XDrawRectangle(display, drawable, gc, x, y, w-1, h-1);

    /*
     * Draw these points so that the corners will not be rounded
     */
    points[0].x = x;
    points[0].y = y;
    points[1].x = x + w - 1;
    points[1].y = y;
    points[2].x = x;
    points[2].y = y + h - 1;
    points[3].x = x + w - 1;
    points[3].y = y + h - 1;

    XDrawPoints(display, drawable, gc, points, 4, CoordModeOrigin);
}

/*----------------------------------------------------------------------
 * TixpStartSubRegionDraw --
 *
 *	Limits the subsequent drawing operations into the prescribed
 *	rectangle region. This takes effect up to a matching
 *	TixEndSubRegionDraw() call.
 *
 * Return value:
 *	none.
 *----------------------------------------------------------------------
 */

void
TixpStartSubRegionDraw(display, drawable, gc, subRegPtr, origX, origY,
	x, y, width, height, needWidth, needHeight)
    Display *display;
    Drawable drawable;
    GC gc;
    TixpSubRegion * subRegPtr;
    int origX;
    int origY;
    int x;
    int y;
    int width;
    int height;
    int needWidth;
    int needHeight;
{
    if ((width < needWidth) || (height < needHeight)) {
	subRegPtr->rectUsed    = 1;
	subRegPtr->rect.x      = (short)x;
	subRegPtr->rect.y      = (short)y;
	subRegPtr->rect.width  = (short)width;
	subRegPtr->rect.height = (short)height;

	XSetClipRectangles(display, gc, origX, origY, &subRegPtr->rect,
		1, Unsorted);
    } else {
	subRegPtr->rectUsed    = 0;
    }
}

/*----------------------------------------------------------------------
 * TixpEndSubRegionDraw --
 *
 *
 *----------------------------------------------------------------------
 */
void
TixpEndSubRegionDraw(display, drawable, gc, subRegPtr)
    Display *display;
    Drawable drawable;
    GC gc;
    TixpSubRegion * subRegPtr;
{
    if (subRegPtr->rectUsed) {
	subRegPtr->rect.x      = (short)0;
	subRegPtr->rect.y      = (short)0;
	subRegPtr->rect.width  = (short)20000;
	subRegPtr->rect.height = (short)20000;
	XSetClipRectangles(display, gc, 0, 0, &subRegPtr->rect, 1, Unsorted);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TixpSubRegDisplayText --
 *
 *	Display a text string on one or more lines in a sub region.
 *
 * Results:
 *	See TkDisplayText
 *
 * Side effects:
 *	See TkDisplayText
 *
 *----------------------------------------------------------------------
 */

void
TixpSubRegDisplayText(display, drawable, gc, subRegPtr, font, string,
	numChars, x, y,	length, justify, underline)
    Display *display;		/* X display to use for drawing text. */
    Drawable drawable;		/* Window or pixmap in which to draw the
				 * text. */
    GC gc;			/* Graphics context to use for drawing text. */
    TixpSubRegion * subRegPtr;	/* Information about the subregion */
    TixFont font;		/* Font that determines geometry of text
				 * (should be same as font in gc). */
    char *string;		/* String to display;  may contain embedded
				 * newlines. */
    int numChars;		/* Number of characters to use from string. */
    int x, y;			/* Pixel coordinates within drawable of
				 * upper left corner of display area. */
    int length;			/* Line length in pixels;  used to compute
				 * word wrap points and also for
				 * justification.   Must be > 0. */
    Tk_Justify justify;		/* How to justify lines. */
    int underline;		/* Index of character to underline, or < 0
				 * for no underlining. */
{
    TixDisplayText(display, drawable, font, string,
	numChars, x, y,	length, justify, underline, gc);
}

/*----------------------------------------------------------------------
 * TixpSubRegFillRectangle --
 *
 *
 *----------------------------------------------------------------------
 */
void
TixpSubRegFillRectangle(display, drawable, gc, subRegPtr, x, y, width, height)
    Display *display;		/* X display to use for drawing rectangle. */
    Drawable drawable;		/* Window or pixmap in which to draw the
				 * rectangle. */
    GC gc;			/* Graphics context to use for drawing. */
    TixpSubRegion * subRegPtr;	/* Information about the subregion */
    int x, y;			/* Pixel coordinates within drawable of
				 * upper left corner of display area. */
    int width, height;		/* Size of the rectangle. */
{
    XFillRectangle(display, drawable, gc, x, y, width, height);
}

/*----------------------------------------------------------------------
 * TixpSubRegDrawImage	--
 *
 *	Draws a Tk image in a subregion.
 *----------------------------------------------------------------------
 */
void
TixpSubRegDrawImage(subRegPtr, image, imageX, imageY, width, height,
	drawable, drawableX, drawableY)
    TixpSubRegion * subRegPtr;
    Tk_Image image;
    int imageX;
    int imageY;
    int width;
    int height;
    Drawable drawable;
    int drawableX;
    int drawableY;
{
    if (subRegPtr->rectUsed) {
	if (drawableX < subRegPtr->rect.x) {
	    width  -= subRegPtr->rect.x - drawableX;
	    imageX += subRegPtr->rect.x - drawableX;
	    drawableX = subRegPtr->rect.x;
	}
	if (drawableX + width > subRegPtr->rect.x + subRegPtr->rect.width) {
	    width = subRegPtr->rect.x - drawableX + subRegPtr->rect.width;
	}

	if (drawableY < subRegPtr->rect.y) {
	    height -= subRegPtr->rect.y - drawableY;
	    imageY += subRegPtr->rect.y - drawableY;
	    drawableY = subRegPtr->rect.y;
	}
	if (drawableY + height > subRegPtr->rect.y + subRegPtr->rect.height) {
	    height = subRegPtr->rect.y - drawableY + subRegPtr->rect.height;
	}
    }

    Tk_RedrawImage(image, imageX, imageY, width, height, drawable,
	    drawableX, drawableY);
}

void
TixpSubRegDrawBitmap(display, drawable, gc, subRegPtr, bitmap, src_x, src_y,
	width, height, dest_x, dest_y, plane)
    Display *display;
    Drawable drawable;
    GC gc;
    TixpSubRegion * subRegPtr;
    Pixmap bitmap;
    int src_x, src_y;
    int width, height;
    int dest_x, dest_y;
    unsigned long plane;
{
    XCopyPlane(display, bitmap, drawable, gc, src_x, src_y, width, height,
	    dest_x, dest_y, plane);
}
