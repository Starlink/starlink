/*
 * bltText.c --
 *
 *	This module implements multi-line, rotate-able text for the BLT toolkit.
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

#include "bltInt.h"
#include <X11/Xutil.h>

#define ROTATE_0	0
#define ROTATE_90	1
#define ROTATE_180	2
#define ROTATE_270	3

static GC bitmapGC;

static void
DrawCompoundText(display, drawable, gc, x, y, textPtr)
    Display *display;
    Drawable drawable;
    GC gc;
    register int x, y;		/* Origin of text */
    CompoundText *textPtr;
{
    register TextSegment *segPtr = textPtr->segArr;
    register int i;

    for (i = 0; i < textPtr->numSegments; i++, segPtr++) {
	XDrawString(display, drawable, gc, x + segPtr->x, y + segPtr->y,
	    segPtr->text, segPtr->numChars);
    }
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_GetCompoundText --
 *
 *	Get the extents of a possibly multiple-lined text string.
 *
 * Results:
 *	Returns via *widthPtr* and *heightPtr* the dimensions of
 *	the text string.
 *
 * -----------------------------------------------------------------
 */
CompoundText *
Blt_GetCompoundText(string, attrPtr)
    char string[];
    TextAttributes *attrPtr;
{
    int maxHeight, maxWidth;
    int count;			/* Count # of characters on each line */
    int numSegments;
    int width;			/* Running dimensions of the text */
    TextSegment *segPtr;
    CompoundText *textPtr;
    int lineHeight;
    int size;
    register char *p;
    register int i;
    Tk_FontMetrics fontMetrics;

    Tk_GetFontMetrics(attrPtr->font, &fontMetrics);
    lineHeight = fontMetrics.linespace + attrPtr->leader +
	attrPtr->shadow.offset;
    numSegments = 0;
    for (p = string; *p != '\0'; p++) {
	if (*p == '\n') {
	    numSegments++;
	}
    }
    if (*(p - 1) != '\n') {
	numSegments++;
    }
    size = sizeof(CompoundText) + (sizeof(TextSegment) * (numSegments - 1));
    textPtr = (CompoundText *) calloc(1, size);
    textPtr->numSegments = numSegments;

    numSegments = count = 0;
    width = maxWidth = 0;
    maxHeight = attrPtr->padTop;
    segPtr = textPtr->segArr;
    for (p = string; *p != '\0'; p++) {
	if (*p == '\n') {
	    if (count > 0) {
		width = Tk_TextWidth(attrPtr->font, string, count) +
		    attrPtr->shadow.offset;
		if (width > maxWidth) {
		    maxWidth = width;
		}
	    }
	    segPtr->width = width;
	    segPtr->numChars = count;
	    segPtr->y = maxHeight + fontMetrics.ascent;
	    segPtr->text = string;
	    segPtr++;
	    numSegments++;
	    maxHeight += lineHeight;
	    string = p + 1;	/* Start the string on the next line */
	    count = 0;		/* Reset to indicate the start of a new line */
	    continue;
	}
	count++;
    }
    if (numSegments < textPtr->numSegments) {
	width = Tk_TextWidth(attrPtr->font, string, count) +
	    attrPtr->shadow.offset;
	if (width > maxWidth) {
	    maxWidth = width;
	}
	segPtr->width = width;
	segPtr->numChars = count;
	segPtr->y = maxHeight + fontMetrics.ascent;
	segPtr->text = string;
	maxHeight += lineHeight;
	numSegments++;
    }
    maxHeight += attrPtr->padBottom;
    maxWidth += PADDING(attrPtr->padX);
    segPtr = textPtr->segArr;
    for (i = 0; i < numSegments; i++, segPtr++) {
	switch (attrPtr->justify) {
	default:
	case TK_JUSTIFY_LEFT:
	    /* No offset for left justified text strings */
	    segPtr->x = attrPtr->padLeft;
	    break;
	case TK_JUSTIFY_RIGHT:
	    segPtr->x = (maxWidth - segPtr->width) - attrPtr->padRight;
	    break;
	case TK_JUSTIFY_CENTER:
	    segPtr->x = (maxWidth - segPtr->width) / 2;
	    break;
	}
    }
    textPtr->width = maxWidth;
    textPtr->height = maxHeight - attrPtr->leader;
    return textPtr;
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_GetTextExtents --
 *
 *	Get the extents of a possibly multiple-lined text string.
 *
 * Results:
 *	Returns via *widthPtr* and *heightPtr* the dimensions of
 *	the text string.
 *
 * -----------------------------------------------------------------
 */
void
Blt_GetTextExtents(attrPtr, string, widthPtr, heightPtr)
    TextAttributes *attrPtr;
    char string[];
    int *widthPtr, *heightPtr;
{
    int count;			/* Count # of characters on each line */
    int width, height;
    int w, lineHeight;
    register char *p;
    Tk_FontMetrics fontMetrics;

    if (string == NULL) {
	return;			/* NULL string? */
    }
    Tk_GetFontMetrics(attrPtr->font, &fontMetrics);
    lineHeight = fontMetrics.linespace + attrPtr->leader +
	attrPtr->shadow.offset;
    count = 0;
    width = height = 0;
    for (p = string; *p != '\0'; p++) {
	if (*p == '\n') {
	    if (count > 0) {
		w = Tk_TextWidth(attrPtr->font, string, count) +
		    attrPtr->shadow.offset;
		if (w > width) {
		    width = w;
		}
	    }
	    height += lineHeight;
	    string = p + 1;	/* Start the string on the next line */
	    count = 0;		/* Reset to indicate the start of a new line */
	    continue;
	}
	count++;
    }
    if ((count > 0) && (*(p - 1) != '\n')) {
	height += lineHeight;
	w = Tk_TextWidth(attrPtr->font, string, count) +
	    attrPtr->shadow.offset;
	if (w > width) {
	    width = w;
	}
    }
    *widthPtr = width + PADDING(attrPtr->padX);
    *heightPtr = height + PADDING(attrPtr->padY);
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_GetBoundingBox
 *
 *	Computes the dimensions of the bounding box surrounding a
 *	rectangle rotated about its center.  If pointArr isn't NULL,
 *	the coordinates of the rotated rectangle are also returned.
 *
 *	The dimensions are determined by rotating the rectangle, and
 *	doubling the maximum x-coordinate and y-coordinate.
 *
 *		w = 2 * maxX,  h = 2 * maxY
 *
 *	Since the rectangle is centered at 0,0, the coordinates of
 *	the bounding box are (-w/2,-h/2 w/2,-h/2, w/2,h/2 -w/2,h/2).
 *
 *  		0 ------- 1
 *  		|         |
 *  		|    x    |
 *  		|         |
 *  		3 ------- 2
 *
 * Results:
 *	The width and height of the bounding box containing the
 *	rotated rectangle are returned.
 *
 * -----------------------------------------------------------------
 */
void
Blt_GetBoundingBox(width, height, theta, rotWidthPtr, rotHeightPtr, pointArr)
    int width, height;		/* Unrotated region */
    double theta;		/* Rotation of box */
    int *rotWidthPtr, *rotHeightPtr;	/* (out) Bounding box region */
    XPoint *pointArr;		/* (out) Points of the rotated box */
{
    register int i;
    double sinTheta, cosTheta;
    double maxX, maxY;
    register double x, y;
    Point2D corner[4];

    theta = FMOD(theta, 360.0);
    if (FMOD(theta, (double)90.0) == 0.0) {
	int rotWidth, rotHeight;
	int quadrant;

	/* Handle right-angle rotations specifically */

	quadrant = (int)(theta / 90.0);
	switch (quadrant) {
	case ROTATE_270:	/* 270 degrees */
	case ROTATE_90:	/* 90 degrees */
	    rotWidth = height;
	    rotHeight = width;
	    break;
	case ROTATE_180:	/* 180 degrees */
	case ROTATE_0:		/* 0 degrees */
	default:
	    rotWidth = width;
	    rotHeight = height;
	    break;
	}
	if (pointArr != NULL) {
	    int sx, sy;

	    x = (double)rotWidth *0.5;
	    y = (double)rotHeight *0.5;
	    sx = ROUND(x);
	    sy = ROUND(y);
	    pointArr[3].x = pointArr[0].x = -sx;
	    pointArr[1].y = pointArr[0].y = -sy;
	    pointArr[2].x = pointArr[1].x = sx;
	    pointArr[3].y = pointArr[2].y = sy;
	}
	*rotWidthPtr = rotWidth;
	*rotHeightPtr = rotHeight;
	return;
    }
    /* Set the four corners of the rectangle whose center is the origin */

    corner[1].x = corner[2].x = (double)width *0.5;
    corner[0].x = corner[3].x = -corner[1].x;
    corner[2].y = corner[3].y = (double)height *0.5;
    corner[0].y = corner[1].y = -corner[2].y;

    theta = (-theta / 180.0) * M_PI;
    sinTheta = sin(theta), cosTheta = cos(theta);
    maxX = maxY = 0.0;

    /* Rotate the four corners and find the maximum X and Y coordinates */

    for (i = 0; i < 4; i++) {
	x = (corner[i].x * cosTheta) - (corner[i].y * sinTheta);
	y = (corner[i].x * sinTheta) + (corner[i].y * cosTheta);
	if (x > maxX) {
	    maxX = x;
	}
	if (y > maxY) {
	    maxY = y;
	}
	if (pointArr != NULL) {
	    pointArr[i].x = ROUND(x);
	    pointArr[i].y = ROUND(y);
	}
    }

    /*
     * By symmetry, the width and height of the bounding box are
     * twice the maximum x and y coordinates.
     */
    *rotWidthPtr = (int)((maxX + maxX) + 0.5);
    *rotHeightPtr = (int)((maxY + maxY) + 0.5);
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_TranslateAnchor --
 *
 * 	Translate the coordinates of a given bounding box based
 *	upon the anchor specified.  The anchor indicates where
 *	the given xy position is in relation to the bounding box.
 *
 *  		nw --- n --- ne
 *  		|            |
 *  		w   center   e
 *  		|            |
 *  		sw --- s --- se
 *
 * 	The coordinates returned are translated to the origin of the
 * 	bounding box (suitable for giving to XCopyArea, XCopyPlane, etc.)
 *
 * Results:
 *	The translated coordinates of the bounding box are returned.
 *
 * -----------------------------------------------------------------
 */
void
Blt_TranslateAnchor(x, y, width, height, anchor, transXPtr, transYPtr)
    int x, y;			/* Window coordinates of anchor */
    int width, height;		/* Extents of the bounding box */
    Tk_Anchor anchor;		/* Direction of the anchor */
    int *transXPtr, *transYPtr;
{
    switch (anchor) {
    case TK_ANCHOR_NW:		/* Upper left corner */
	break;
    case TK_ANCHOR_W:		/* Left center */
	y -= (height / 2);
	break;
    case TK_ANCHOR_SW:		/* Lower left corner */
	y -= height;
	break;
    case TK_ANCHOR_N:		/* Top center */
	x -= (width / 2);
	break;
    case TK_ANCHOR_CENTER:	/* Centered */
	x -= (width / 2);
	y -= (height / 2);
	break;
    case TK_ANCHOR_S:		/* Bottom center */
	x -= (width / 2);
	y -= height;
	break;
    case TK_ANCHOR_NE:		/* Upper right corner */
	x -= width;
	break;
    case TK_ANCHOR_E:		/* Right center */
	x -= width;
	y -= (height / 2);
	break;
    case TK_ANCHOR_SE:		/* Lower right corner */
	x -= width;
	y -= height;
	break;
    }
    *transXPtr = x;
    *transYPtr = y;
}

#ifdef	WIN32
/*
 * -----------------------------------------------------------------
 *
 * Blt_RotateBitmap --
 *
 *	Creates a new bitmap containing the rotated image of the given
 *	bitmap.  We also need a special GC of depth 1, so that we do
 *	not need to rotate more than one plane of the bitmap.
 *
 * Results:
 *	Returns a new bitmap containing the rotated image.
 *
 * -----------------------------------------------------------------
 */
Pixmap
Blt_RotateBitmap(tkwin, srcBitmap, srcWidth, srcHeight, theta,
    destWidthPtr, destHeightPtr)
    Tk_Window tkwin;
    Pixmap srcBitmap;		/* Source bitmap to be rotated */
    int srcWidth, srcHeight;	/* Width and height of the source bitmap */
    double theta;		/* Right angle rotation to perform */
    int *destWidthPtr, *destHeightPtr;
{
    Display *display;		/* X display */
    Window root;		/* Root window drawable */
    Pixmap destBitmap;
    int destWidth, destHeight;
    HDC src, dest;
    TkWinDCState srcState, destState;
    register int dx, dy;	/* Destination bitmap coordinates */
    register int sx, sy;	/* Source bitmap coordinates */
    unsigned long pixel;

    /* Create a bitmap and image big enough to contain the rotated text */

    display = Tk_Display(tkwin);
    root = RootWindow(Tk_Display(tkwin), Tk_ScreenNumber(tkwin));
    Blt_GetBoundingBox(srcWidth, srcHeight, theta, &destWidth, &destHeight,
	(XPoint *)NULL);
    destBitmap = Tk_GetPixmap(display, root, destWidth, destHeight, 1);
    if (destBitmap == None) {
	return None;		/* Can't allocate pixmap. */
    }
    XSetForeground(display, bitmapGC, 0x0);
    XFillRectangle(display, destBitmap, bitmapGC, 0, 0, destWidth, destHeight);

    src = TkWinGetDrawableDC(display, srcBitmap, &srcState);
    dest = TkWinGetDrawableDC(display, destBitmap, &destState);

    theta = FMOD(theta, 360.0);
    if (FMOD(theta, (double)90.0) == 0.0) {
	int quadrant;

	/* Handle right-angle rotations specifically */

	quadrant = (int)(theta / 90.0);
	switch (quadrant) {
	case ROTATE_270:	/* 270 degrees */
	    for (dx = 0; dx < destWidth; dx++) {
		for (dy = 0; dy < destHeight; dy++) {
		    sx = dy, sy = destWidth - dx - 1;
		    pixel = GetPixel(src, sx, sy);
		    if (pixel) {
			SetPixelV(dest, dx, dy, pixel);
		    }
		}
	    }
	    break;

	case ROTATE_180:	/* 180 degrees */
	    for (dx = 0; dx < destWidth; dx++) {
		for (dy = 0; dy < destHeight; dy++) {
		    sx = destWidth - dx - 1, sy = destHeight - dy - 1;
		    pixel = GetPixel(src, sx, sy);
		    if (pixel) {
			SetPixelV(dest, dx, dy, pixel);
		    }
		}
	    }
	    break;

	case ROTATE_90:	/* 90 degrees */
	    for (dx = 0; dx < destWidth; dx++) {
		for (dy = 0; dy < destHeight; dy++) {
		    sx = destHeight - dy - 1, sy = dx;
		    pixel = GetPixel(src, sx, sy);
		    if (pixel) {
			SetPixelV(dest, dx, dy, pixel);
		    }
		}
	    }
	    break;

	case ROTATE_0:		/* 0 degrees */
	    for (dx = 0; dx < destWidth; dx++) {
		for (dy = 0; dy < destHeight; dy++) {
		    pixel = GetPixel(src, sx, sy);
		    if (pixel) {
			SetPixelV(dest, dx, dy, pixel);
		    }
		}
	    }
	    break;

	default:
	    /* The calling routine should never let this happen. */
	    break;
	}
    } else {
	double radians, sinTheta, cosTheta;
	double srcX, srcY;	/* Center of source rectangle */
	double destX, destY;	/* Center of destination rectangle */
	double transX, transY;
	double rx, ry;		/* Angle of rotation for x and y coordinates */

	radians = (theta / 180.0) * M_PI;
	sinTheta = sin(radians), cosTheta = cos(radians);

	/*
	 * Coordinates of the centers of the source and destination rectangles
	 */
	srcX = srcWidth * 0.5;
	srcY = srcHeight * 0.5;
	destX = destWidth * 0.5;
	destY = destHeight * 0.5;

	/* Rotate each pixel of dest image, placing results in source image */

	for (dx = 0; dx < destWidth; dx++) {
	    for (dy = 0; dy < destHeight; dy++) {

		/* Translate origin to center of destination image */
		transX = dx - destX;
		transY = dy - destY;

		/* Rotate the coordinates about the origin */
		rx = (transX * cosTheta) - (transY * sinTheta);
		ry = (transX * sinTheta) + (transY * cosTheta);

		/* Translate back to the center of the source image */
		rx += srcX;
		ry += srcY;

		sx = ROUND(rx);
		sy = ROUND(ry);

		/*
		 * Verify the coordinates, since the destination image can be
		 * bigger than the source
		 */

		if ((sx >= srcWidth) || (sx < 0) || (sy >= srcHeight) ||
		    (sy < 0)) {
		    continue;
		}
		pixel = GetPixel(src, sx, sy);
		if (pixel) {
		    SetPixelV(dest, dx, dy, pixel);
		}
	    }
	}
    }
    TkWinReleaseDrawableDC(srcBitmap, src, &srcState);
    TkWinReleaseDrawableDC(destBitmap, dest, &destState);

    *destWidthPtr = destWidth;
    *destHeightPtr = destHeight;
    return destBitmap;
}

#else
/*
 * -----------------------------------------------------------------
 *
 * Blt_RotateBitmap --
 *
 *	Creates a new bitmap containing the rotated image of the given
 *	bitmap.  We also need a special GC of depth 1, so that we do
 *	not need to rotate more than one plane of the bitmap.
 *
 * Results:
 *	Returns a new bitmap containing the rotated image.
 *
 * -----------------------------------------------------------------
 */
Pixmap
Blt_RotateBitmap(tkwin, srcBitmap, srcWidth, srcHeight, theta,
    destWidthPtr, destHeightPtr)
    Tk_Window tkwin;
    Pixmap srcBitmap;		/* Source bitmap to be rotated */
    int srcWidth, srcHeight;	/* Width and height of the source bitmap */
    double theta;		/* Right angle rotation to perform */
    int *destWidthPtr, *destHeightPtr;
{
    Display *display;		/* X display */
    Window root;		/* Root window drawable */
    Pixmap destBitmap;
    int destWidth, destHeight;
    XImage *src, *dest;
    register int dx, dy;	/* Destination bitmap coordinates */
    register int sx, sy;	/* Source bitmap coordinates */
    unsigned long pixel;

    display = Tk_Display(tkwin);
    root = RootWindow(Tk_Display(tkwin), Tk_ScreenNumber(tkwin));

    /* Create a bitmap and image big enough to contain the rotated text */
    Blt_GetBoundingBox(srcWidth, srcHeight, theta, &destWidth, &destHeight,
	(XPoint *)NULL);
    destBitmap = Tk_GetPixmap(display, root, destWidth, destHeight, 1);
    XSetForeground(display, bitmapGC, 0x0);
    XFillRectangle(display, destBitmap, bitmapGC, 0, 0, destWidth, destHeight);

    src = XGetImage(display, srcBitmap, 0, 0, srcWidth, srcHeight, 1, ZPixmap);
    dest = XGetImage(display, destBitmap, 0, 0, destWidth, destHeight, 1,
	ZPixmap);
    theta = FMOD(theta, 360.0);
    if (FMOD(theta, (double)90.0) == 0.0) {
	int quadrant;

	/* Handle right-angle rotations specifically */

	quadrant = (int)(theta / 90.0);
	switch (quadrant) {
	case ROTATE_270:	/* 270 degrees */
	    for (dx = 0; dx < destWidth; dx++) {
		for (dy = 0; dy < destHeight; dy++) {
		    sx = dy, sy = destWidth - dx - 1;
		    pixel = XGetPixel(src, sx, sy);
		    if (pixel) {
			XPutPixel(dest, dx, dy, pixel);
		    }
		}
	    }
	    break;

	case ROTATE_180:	/* 180 degrees */
	    for (dx = 0; dx < destWidth; dx++) {
		for (dy = 0; dy < destHeight; dy++) {
		    sx = destWidth - dx - 1, sy = destHeight - dy - 1;
		    pixel = XGetPixel(src, sx, sy);
		    if (pixel) {
			XPutPixel(dest, dx, dy, pixel);
		    }
		}
	    }
	    break;

	case ROTATE_90:	/* 90 degrees */
	    for (dx = 0; dx < destWidth; dx++) {
		for (dy = 0; dy < destHeight; dy++) {
		    sx = destHeight - dy - 1, sy = dx;
		    pixel = XGetPixel(src, sx, sy);
		    if (pixel) {
			XPutPixel(dest, dx, dy, pixel);
		    }
		}
	    }
	    break;

	case ROTATE_0:		/* 0 degrees */
	    for (dx = 0; dx < destWidth; dx++) {
		for (dy = 0; dy < destHeight; dy++) {
		    pixel = XGetPixel(src, dx, dy);
		    if (pixel) {
			XPutPixel(dest, dx, dy, pixel);
		    }
		}
	    }
	    break;

	default:
	    /* The calling routine should never let this happen. */
	    break;
	}
    } else {
	double radians, sinTheta, cosTheta;
	double srcX, srcY;	/* Center of source rectangle */
	double destX, destY;	/* Center of destination rectangle */
	double transX, transY;
	double rx, ry;		/* Angle of rotation for x and y coordinates */

	radians = (theta / 180.0) * M_PI;
	sinTheta = sin(radians), cosTheta = cos(radians);

	/*
	 * Coordinates of the centers of the source and destination rectangles
	 */
	srcX = srcWidth * 0.5;
	srcY = srcHeight * 0.5;
	destX = destWidth * 0.5;
	destY = destHeight * 0.5;

	/* Rotate each pixel of dest image, placing results in source image */

	for (dx = 0; dx < destWidth; dx++) {
	    for (dy = 0; dy < destHeight; dy++) {

		/* Translate origin to center of destination image */
		transX = dx - destX;
		transY = dy - destY;

		/* Rotate the coordinates about the origin */
		rx = (transX * cosTheta) - (transY * sinTheta);
		ry = (transX * sinTheta) + (transY * cosTheta);

		/* Translate back to the center of the source image */
		rx += srcX;
		ry += srcY;

		sx = ROUND(rx);
		sy = ROUND(ry);

		/*
		 * Verify the coordinates, since the destination image can be
		 * bigger than the source
		 */

		if ((sx >= srcWidth) || (sx < 0) || (sy >= srcHeight) ||
		    (sy < 0)) {
		    continue;
		}
		pixel = XGetPixel(src, sx, sy);
		if (pixel) {
		    XPutPixel(dest, dx, dy, pixel);
		}
	    }
	}
    }
    /* Write the rotated image into the destination bitmap */
    XPutImage(display, destBitmap, bitmapGC, dest, 0, 0, 0, 0, destWidth,
	destHeight);

    /* Clean up temporary resources used */
    XDestroyImage(src), XDestroyImage(dest);
    *destWidthPtr = destWidth;
    *destHeightPtr = destHeight;
    return destBitmap;
}

#endif /* WIN32 */

/*
 * -----------------------------------------------------------------
 *
 * Blt_CreateTextBitmap --
 *
 *	Draw a bitmap, using the the given window coordinates
 *	as an anchor for the text bounding box.
 *
 * Results:
 *	Returns the bitmap representing the text string.
 *
 * Side Effects:
 *	Bitmap is drawn using the given font and GC in the
 *	drawable at the given coordinates, anchor, and rotation.
 *
 * -----------------------------------------------------------------
 */
Pixmap
Blt_CreateTextBitmap(tkwin, textPtr, attrPtr, bmWidthPtr, bmHeightPtr, isMask)
    Tk_Window tkwin;
    CompoundText *textPtr;	/* Text string to draw */
    TextAttributes *attrPtr;	/* Text attributes: rotation, color, font,
				 * linespacing, justification, etc. */
    int *bmWidthPtr;
    int *bmHeightPtr;		/* Extents of rotated text string */
    int isMask;
{
    int width, height;
    Pixmap bitmap;
    Display *display;
    Window root;
#ifdef WIN32
    HDC src;
    TkWinDCState state;
#endif
    display = Tk_Display(tkwin);

    width = textPtr->width + 2 * isMask;
    height = textPtr->height + 2 * isMask;

    /* Create a temporary bitmap to contain the text string */
    root = RootWindow(Tk_Display(tkwin), Tk_ScreenNumber(tkwin));
    bitmap = Tk_GetPixmap(display, root, width, height, 1);
    assert(bitmap != None);
    if (bitmap == None) {
	return None;		/* Can't allocate pixmap. */
    }
    /* Clear the pixmap and draw the text string into it */
#ifdef WIN32
    src = TkWinGetDrawableDC(display, bitmap, &state);
    PatBlt(src, 0, 0, width, height, WHITENESS);
    TkWinReleaseDrawableDC(bitmap, src, &state);
#else
    XSetForeground(display, bitmapGC, 0);
    XFillRectangle(display, bitmap, bitmapGC, 0, 0, width, height);
#endif /* WIN32 */

    XSetFont(display, bitmapGC, Tk_FontId(attrPtr->font));
    XSetForeground(display, bitmapGC, 1);
    if (isMask) {
	DrawCompoundText(display, bitmap, bitmapGC, 0, 0, textPtr);
	DrawCompoundText(display, bitmap, bitmapGC, 1, 1, textPtr);
	DrawCompoundText(display, bitmap, bitmapGC, 2, 2, textPtr);
    } else {
	DrawCompoundText(display, bitmap, bitmapGC, 0, 0, textPtr);
    }
#ifdef WIN32
    /*  
     * Under Win32 when drawing into a bitmap, the bits are
     * reversed. Which is why we are inverting the bitmap here.  
     */
    src = TkWinGetDrawableDC(display, bitmap, &state);
    PatBlt(src, 0, 0, textPtr->width, textPtr->height, DSTINVERT);
    TkWinReleaseDrawableDC(bitmap, src, &state);
#endif
    if (attrPtr->theta != 0.0) {
	Pixmap rotBitmap;

	/* Replace the text pixmap with a rotated one */

	rotBitmap = Blt_RotateBitmap(tkwin, bitmap, textPtr->width,
	    textPtr->height, attrPtr->theta, bmWidthPtr, bmHeightPtr);
	if (rotBitmap != None) {
	    Tk_FreePixmap(display, bitmap);
	    return rotBitmap;
	}
	panic("could not allocate rotated bitmap");
    }
    *bmWidthPtr = textPtr->width, *bmHeightPtr = textPtr->height;
    return bitmap;
}

#ifdef WIN32
/*
 * -----------------------------------------------------------------------
 *
 * Blt_ScaleBitmapRegion --
 *
 *	Creates a new scaled bitmap from another bitmap. The new bitmap
 *	is bounded by a specified region. Only this portion of the bitmap
 *	is scaled from the original bitmap.
 *
 *	By bounding scaling to a region we can generate a new bitmap
 *	which is no bigger than the specified viewport.
 *
 * Results:
 *	The new scaled bitmap is returned.
 *
 * Side Effects:
 *	A new pixmap is allocated. The caller must release this.
 *
 * -----------------------------------------------------------------------
 */
Pixmap
Blt_ScaleBitmapRegion(tkwin, srcBitmap, srcWidth, srcHeight,
    destWidth, destHeight, regionPtr)
    Tk_Window tkwin;
    Pixmap srcBitmap;
    int srcWidth, srcHeight, destWidth, destHeight;
    ImageRegion *regionPtr;
{
    TkWinDCState srcState, destState;
    HDC src, dest;
    Pixmap destBitmap;
    double xScale, yScale;
    register int dx, dy;	/* Destination bitmap coordinates */
    register int sx, sy;	/* Source bitmap coordinates */
    unsigned long pixel;
    Window root;
    Display *display;
    double tmp;

    /* Create a new bitmap the size of the region and clear it */

    display = Tk_Display(tkwin);
    root = RootWindow(Tk_Display(tkwin), Tk_ScreenNumber(tkwin));
    destBitmap = Tk_GetPixmap(display, root, regionPtr->width, 
	regionPtr->height, 1);
    if (destBitmap == None) {
	return None;
    }
    XSetForeground(display, bitmapGC, 0x0);
    XFillRectangle(display, destBitmap, bitmapGC, 0, 0, regionPtr->width,
	regionPtr->height);
    src = TkWinGetDrawableDC(display, srcBitmap, &srcState);
    dest = TkWinGetDrawableDC(display, destBitmap, &destState);

    /*
     * Scale each pixel of destination image from results of source
     * image. Verify the coordinates, since the destination image can
     * be bigger than the source
     */
    xScale = (double)srcWidth / (double)destWidth;
    yScale = (double)srcHeight / (double)destHeight;

    for (dy = 0; dy < regionPtr->height; dy++) {
	tmp = (double)(dy + regionPtr->y) * yScale;
	sy = ROUND(tmp);
	if (sy >= srcHeight) {
	    continue;
	}
	for (dx = 0; dx < regionPtr->width; dx++) {
	    tmp = (double)(dx + regionPtr->x) * xScale;
	    sx = ROUND(tmp);
	    if (sx >= srcWidth) {
		continue;
	    }
	    pixel = GetPixel(src, sx, sy);
	    if (pixel) {
		SetPixelV(dest, dx, dy, pixel);
	    }
	}
    }
    TkWinReleaseDrawableDC(srcBitmap, src, &srcState);
    TkWinReleaseDrawableDC(destBitmap, dest, &destState);
    return destBitmap;
}

#else

/*
 * -----------------------------------------------------------------------
 *
 * Blt_ScaleBitmapRegion --
 *
 *	Creates a new scaled bitmap from another bitmap. The new bitmap
 *	is bounded by a specified region. Only this portion of the bitmap
 *	is scaled from the original bitmap.
 *
 *	By bounding scaling to a region we can generate a new bitmap
 *	which is no bigger than the specified viewport.
 *
 * Results:
 *	The new scaled bitmap is returned.
 *
 * Side Effects:
 *	A new pixmap is allocated. The caller must release this.
 *
 * -----------------------------------------------------------------------
 */
Pixmap
Blt_ScaleBitmapRegion(tkwin, srcBitmap, srcWidth, srcHeight,
    destWidth, destHeight, regionPtr)
    Tk_Window tkwin;
    Pixmap srcBitmap;
    int srcWidth, srcHeight, destWidth, destHeight;
    ImageRegion *regionPtr;
{
    Display *display;
    Window root;
    XImage *src, *dest;
    Pixmap destBitmap;
    double xScale, yScale;
    register int dx, dy;	/* Destination bitmap coordinates */
    register int sx, sy;	/* Source bitmap coordinates */
    unsigned long pixel;
    double tmp;

    /* Create a new bitmap the size of the region and clear it */

    display = Tk_Display(tkwin);
    root = RootWindow(Tk_Display(tkwin), Tk_ScreenNumber(tkwin));
    destBitmap = Tk_GetPixmap(display, root, regionPtr->width, 
	regionPtr->height, 1);

    XSetForeground(display, bitmapGC, 0x0);
    XFillRectangle(display, destBitmap, bitmapGC, 0, 0, regionPtr->width,
	regionPtr->height);

    src = XGetImage(display, srcBitmap, 0, 0, srcWidth, srcHeight, 1, ZPixmap);
    dest = XGetImage(display, destBitmap, 0, 0, regionPtr->width, 
	regionPtr->height, 1, ZPixmap);

    /*
     * Scale each pixel of destination image from results of source
     * image. Verify the coordinates, since the destination image can
     * be bigger than the source
     */
    xScale = (double)srcWidth / (double)destWidth;
    yScale = (double)srcHeight / (double)destHeight;

    for (dy = 0; dy < regionPtr->height; dy++) {
	tmp = (double)(dy + regionPtr->y) * yScale;
	sy = ROUND(tmp);
	if (sy >= srcHeight) {
	    continue;
	}
	for (dx = 0; dx < regionPtr->width; dx++) {
	    tmp = (double)(dx + regionPtr->x) * xScale;
	    sx = ROUND(tmp);
	    if (sx >= srcWidth) {
		continue;
	    }
	    pixel = XGetPixel(src, sx, sy);
	    if (pixel) {
		XPutPixel(dest, dx, dy, pixel);
	    }
	}
    }

    /* Write the rotated image into the destination bitmap */

    XPutImage(display, destBitmap, bitmapGC, dest, 0, 0, 0, 0, 
	regionPtr->width, regionPtr->height);
    XDestroyImage(src), XDestroyImage(dest);
    return destBitmap;
}

#endif /* WIN32 */

/*
 * -----------------------------------------------------------------------
 *
 * Blt_ScaleBitmap --
 *
 *	Same as Blt_ScaleBitmapRegion, except that the region is unbounded.
 *	The scaled bitmap will be a fully scaled version of the original,
 *	not a portion of it.
 *
 * Results:
 *	The new scaled bitmap is returned.
 *
 * Side Effects:
 *	A new pixmap is allocated. The caller must release this.
 *
 * -----------------------------------------------------------------------
 */
Pixmap
Blt_ScaleBitmap(tkwin, srcBitmap, srcWidth, srcHeight, scaledWidth, 
	scaledHeight)
    Tk_Window tkwin;
    Pixmap srcBitmap;
    int srcWidth, srcHeight, scaledWidth, scaledHeight;
{
    ImageRegion region;

    region.x = region.y = 0;
    region.width = scaledWidth;
    region.height = scaledHeight;
    return Blt_ScaleBitmapRegion(tkwin, srcBitmap, srcWidth, srcHeight,
	scaledWidth, scaledHeight, &region);
}

/*LINTLIBRARY*/
void
Blt_InitTextAttributes(attrPtr)
    TextAttributes *attrPtr;
{
    /* Initialize these attributes to zero */
    attrPtr->leader = 0;
    attrPtr->shadow.offset = 0;
    attrPtr->padLeft = attrPtr->padRight = 0;
    attrPtr->padTop = attrPtr->padBottom = 0;
    attrPtr->shadow.color = attrPtr->activeColor =
	attrPtr->color = (XColor *)NULL;
    attrPtr->theta = 0.0;
    attrPtr->state = 0;
    attrPtr->anchor = TK_ANCHOR_CENTER;
    attrPtr->justify = TK_JUSTIFY_CENTER;
    attrPtr->font = NULL;
}

void
Blt_SetTextDrawAttributes(attrPtr, font, gc, normalColor, activeColor,
    shadowColor, theta, anchor, justify, leader, shadowOffset)
    TextAttributes *attrPtr;
    Tk_Font font;
    GC gc;
    XColor *normalColor, *activeColor, *shadowColor;
    double theta;
    Tk_Anchor anchor;
    Tk_Justify justify;
    int leader, shadowOffset;
{
    Blt_InitTextAttributes(attrPtr);
    attrPtr->gc = gc;
    attrPtr->color = normalColor;
    attrPtr->activeColor = activeColor;
    attrPtr->shadow.color = shadowColor;
    attrPtr->font = font;
    attrPtr->theta = theta;
    attrPtr->anchor = anchor;
    attrPtr->justify = justify;
    attrPtr->leader = leader;
    attrPtr->shadow.offset = shadowOffset;
}

void
Blt_SetTextPrintAttributes(attrPtr, font, fgColor, activeColor, shadowColor,
    theta, anchor, justify, leader, shadowOffset)
    TextAttributes *attrPtr;
    Tk_Font font;
    XColor *fgColor, *activeColor, *shadowColor;
    double theta;
    Tk_Anchor anchor;
    Tk_Justify justify;
    int leader, shadowOffset;
{
    Blt_InitTextAttributes(attrPtr);
    attrPtr->color = fgColor;
    attrPtr->activeColor = activeColor;
    attrPtr->shadow.color = shadowColor;
    attrPtr->font = font;
    attrPtr->theta = theta;
    attrPtr->anchor = anchor;
    attrPtr->justify = justify;
    attrPtr->leader = leader;
    attrPtr->shadow.offset = shadowOffset;
}

/*
 * -----------------------------------------------------------------
 *
 * DrawText --
 *
 *	Draw a text string, possibly rotated, using the the given
 *	window coordinates as an anchor for the text bounding box.
 *	If the text is not rotated, simply use the X text drawing
 *	routines. Otherwise, generate a bitmap of the rotated text.
 *
 * Results:
 *	Returns the x-coordinate to the right of the text.
 *
 * Side Effects:
 *	Text string is drawn using the given font and GC at the
 *	the given window coordinates.
 *
 *      The Stipple, FillStyle, and TSOrigin fields of the GC are
 *      modified for rotated text.  This assumes the GC is private,
 *      *not* shared (via Tk_GetGC)
 *
 * -----------------------------------------------------------------
 */
void
Blt_DrawCompoundText(tkwin, drawable, textPtr, attrPtr, x, y)
    Tk_Window tkwin;
    Drawable drawable;
    CompoundText *textPtr;
    TextAttributes *attrPtr;	/* Text attribute information */
    int x, y;			/* Window coordinates to draw text */
{
    int width, height;
    double theta;
    Display *display;
    Pixmap bitmap;
    int active;

    display = Tk_Display(tkwin);
    theta = FMOD(attrPtr->theta, (double)360.0);
    if (theta < 0.0) {
	theta += 360.0;
    }
    active = attrPtr->state & STATE_ACTIVE;
    if (theta == 0.0) {

	/*
	 * This is the easy case of no rotation. Simply draw the text
	 * using the standard drawing routines.  Handle offset printing
	 * for engraved (disabled) and shadowed text.
	 */
	width = textPtr->width, height = textPtr->height;
	Blt_TranslateAnchor(x, y, width, height, attrPtr->anchor, &x, &y);
	if (attrPtr->state & (STATE_DISABLED | STATE_EMPHASIS)) {
	    TkBorder *borderPtr = (TkBorder *) attrPtr->border;
	    XColor *color1, *color2;

	    color1 = borderPtr->lightColor, color2 = borderPtr->darkColor;
	    if (attrPtr->state & STATE_EMPHASIS) {
		XColor *hold;

		hold = color1, color1 = color2, color2 = hold;
	    }
	    if (color1 != NULL) {
		XSetForeground(display, attrPtr->gc, color1->pixel);
	    }
	    DrawCompoundText(display, drawable, attrPtr->gc, x + 1, y + 1,
		textPtr);
	    if (color2 != NULL) {
		XSetForeground(display, attrPtr->gc, color2->pixel);
	    }
	    DrawCompoundText(display, drawable, attrPtr->gc, x, y, textPtr);

	    /* Reset the foreground color back to its original setting,
	     * so not to invalidate the GC cache. */
	    XSetForeground(display, attrPtr->gc, attrPtr->color->pixel);

	    return;		/* Done */
	}
	if ((attrPtr->shadow.offset > 0) && (attrPtr->shadow.color != NULL)) {
	    XSetForeground(display, attrPtr->gc, attrPtr->shadow.color->pixel);
	    DrawCompoundText(display, drawable, attrPtr->gc,
		x + attrPtr->shadow.offset, y + attrPtr->shadow.offset, textPtr);
	    XSetForeground(display, attrPtr->gc, attrPtr->color->pixel);
	}
	if (active) {
	    XSetForeground(display, attrPtr->gc, attrPtr->activeColor->pixel);
	}
	DrawCompoundText(display, drawable, attrPtr->gc, x, y, textPtr);
	if (active) {
	    XSetForeground(display, attrPtr->gc, attrPtr->color->pixel);
	}
	return;			/* Done */
    }
    /*
     * Rotate the text by writing the text into a bitmap and rotating
     * the bitmap.  Set the clip mask and origin in the GC first.  And
     * make sure we restore the GC because it may be shared.
     */

    attrPtr->theta = theta;
    bitmap = Blt_CreateTextBitmap(tkwin, textPtr, attrPtr, &width, &height, FALSE);
    if (bitmap == None) {
	return;
    }
    Blt_TranslateAnchor(x, y, width, height, attrPtr->anchor, &x, &y);
    theta = FMOD(theta, (double)90.0);
    XSetClipMask(display, attrPtr->gc, bitmap);

    if (attrPtr->state & (STATE_DISABLED | STATE_EMPHASIS)) {
	TkBorder *borderPtr = (TkBorder *) attrPtr->border;
	XColor *color1, *color2;

	color1 = borderPtr->lightColor, color2 = borderPtr->darkColor;
	if (attrPtr->state & STATE_EMPHASIS) {
	    XColor *hold;

	    hold = color1, color1 = color2, color2 = hold;
	}
	if (color1 != NULL) {
	    XSetForeground(display, attrPtr->gc, color1->pixel);
	}
	XSetClipOrigin(display, attrPtr->gc, x + 1, y + 1);
	XCopyPlane(display, bitmap, drawable, attrPtr->gc, 0, 0, width, height,
	    x + 1, y + 1, 1);
	if (color2 != NULL) {
	    XSetForeground(display, attrPtr->gc, color2->pixel);
	}
	XSetClipOrigin(display, attrPtr->gc, x, y);
	XCopyPlane(display, bitmap, drawable, attrPtr->gc, 0, 0, width, height,
	    x, y, 1);
	XSetForeground(display, attrPtr->gc, attrPtr->color->pixel);
    } else {
	if ((attrPtr->shadow.offset > 0) && (attrPtr->shadow.color != NULL)) {
	    XSetClipOrigin(display, attrPtr->gc, x + attrPtr->shadow.offset,
		y + attrPtr->shadow.offset);
	    XSetForeground(display, attrPtr->gc, attrPtr->shadow.color->pixel);
	    XCopyPlane(display, bitmap, drawable, attrPtr->gc, 0, 0, width,
		height, x + attrPtr->shadow.offset, y + attrPtr->shadow.offset,
		1);
	    XSetForeground(display, attrPtr->gc, attrPtr->color->pixel);
	}
	if (active) {
	    XSetForeground(display, attrPtr->gc, attrPtr->activeColor->pixel);
	}
	XSetClipOrigin(display, attrPtr->gc, x, y);
	XCopyPlane(display, bitmap, drawable, attrPtr->gc, 0, 0, width, height,
	    x, y, 1);
	if (active) {
	    XSetForeground(display, attrPtr->gc, attrPtr->color->pixel);
	}
    }
    XSetClipMask(display, attrPtr->gc, None);
    Tk_FreePixmap(display, bitmap);
}

void
Blt_DrawText2(tkwin, drawable, string, attrPtr, x, y, areaPtr)
    Tk_Window tkwin;
    Drawable drawable;
    char string[];
    TextAttributes *attrPtr;	/* Text attribute information */
    int x, y;			/* Window coordinates to draw text */
    Dimension *areaPtr;
{
    CompoundText *textPtr;
    int width, height;
    double theta;

    if ((string == NULL) || (*string == '\0')) {
	return;			/* Empty string, do nothing */
    }
    textPtr = Blt_GetCompoundText(string, attrPtr);
    Blt_DrawCompoundText(tkwin, drawable, textPtr, attrPtr, x, y);
    theta = FMOD(attrPtr->theta, (double)360.0);
    if (theta < 0.0) {
	theta += 360.0;
    }
    width = textPtr->width;
    height = textPtr->height;
    if (theta != 0.0) {
	Blt_GetBoundingBox(width, height, theta, &width, &height,
	    (XPoint *)NULL);
    }
    free((char *)textPtr);
    areaPtr->width = width;
    areaPtr->height = height;
}

void
Blt_DrawText(tkwin, drawable, string, attrPtr, x, y)
    Tk_Window tkwin;
    Drawable drawable;
    char string[];
    TextAttributes *attrPtr;	/* Text attribute information */
    int x, y;			/* Window coordinates to draw text */
{
    CompoundText *textPtr;

    if ((string == NULL) || (*string == '\0')) {
	return;			/* Empty string, do nothing */
    }
    textPtr = Blt_GetCompoundText(string, attrPtr);
    Blt_DrawCompoundText(tkwin, drawable, textPtr, attrPtr, x, y);
    free((char *)textPtr);
}

/*ARGSUSED*/
void
Blt_InitBitmapGC(interp, tkwin)
    Tcl_Interp *interp;
    Tk_Window tkwin;
{
    Pixmap bitmap;
    XGCValues gcValues;
    unsigned int gcMask;
    Window root;

    root = RootWindow(Tk_Display(tkwin), Tk_ScreenNumber(tkwin));
    bitmap = Tk_GetPixmap(Tk_Display(tkwin), root, 1, 1, 1);
    gcValues.foreground = gcValues.background = 0;
    gcMask = (GCForeground | GCBackground);
    bitmapGC = Blt_GetPrivateGCFromDrawable(tkwin, bitmap, gcMask, &gcValues);
    Tk_FreePixmap(Tk_Display(tkwin), bitmap);
}

void
Blt_ResetTextAttributes(tkwin, attrPtr)
    Tk_Window tkwin;
    TextAttributes *attrPtr;
{
    GC newGC;
    XGCValues gcValues;
    unsigned long gcMask;

    gcMask = GCFont;
    gcValues.font = Tk_FontId(attrPtr->font);
    if (attrPtr->color != NULL) {
	gcMask |= GCForeground;
	gcValues.foreground = attrPtr->color->pixel;
    }
    newGC = Tk_GetGC(tkwin, gcMask, &gcValues);
    if (attrPtr->gc != NULL) {
	Tk_FreeGC(Tk_Display(tkwin), attrPtr->gc);
    }
    attrPtr->gc = newGC;
}

void
Blt_FreeTextAttributes(display, attrPtr)
    Display *display;
    TextAttributes *attrPtr;
{
    if (attrPtr->gc != NULL) {
	Tk_FreeGC(display, attrPtr->gc);
    }
}
