
/*
 * bltGrMisc.c --
 *
 *	This module implements miscellaneous routine for the BLT graph widget.
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
#include <X11/Xutil.h>

static int StringToPosition _ANSI_ARGS_((ClientData, Tcl_Interp *, Tk_Window,
	char *, char *, int));
static char *PositionToString _ANSI_ARGS_((ClientData, Tk_Window, char *, int,
	Tcl_FreeProc **));

Tk_CustomOption bltPositionOption =
{
    StringToPosition, PositionToString, (ClientData)0
};

static int StringToColorPair _ANSI_ARGS_((ClientData, Tcl_Interp *, Tk_Window,
	char *, char *, int));
static char *ColorPairToString _ANSI_ARGS_((ClientData, Tk_Window, char *, int,
	Tcl_FreeProc **));

Tk_CustomOption bltColorPairOption =
{
    StringToColorPair, ColorPairToString, (ClientData)0
};

#ifdef notdef
Tk_CustomOption bltDefColorPairOption =
{
    StringToColorPair, ColorPairToString, (ClientData)COLOR_ALLOW_DEFAULTS
};

static int StringToMapped _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, char *string, char *widgRec, int offset));
static char *MappedToString _ANSI_ARGS_((ClientData clientData, Tk_Window tkwin,
	char *widgRec, int offset, Tcl_FreeProc **freeProcPtrPtr));

Tk_CustomOption bltMappedOption =
{
    StringToMapped, MappedToString, (ClientData)0
};
#endif

/* ----------------------------------------------------------------------
 * Custom option parse and print procedures
 * ----------------------------------------------------------------------
 */

/*
 *----------------------------------------------------------------------
 *
 * Blt_GetXYPosition --
 *
 *	Converts a string in the form "@x,y" into an XPoint structure
 *	of the x and y coordinates.
 *
 * Results:
 *	A standard Tcl result. If the string represents a valid position
 *	*pointPtr* will contain the converted x and y coordinates and
 *	TCL_OK is returned.  Otherwise,	TCL_ERROR is returned and
 *	interp->result will contain an error message.
 *
 *----------------------------------------------------------------------
 */
int
Blt_GetXYPosition(interp, tkwin, string, xPtr, yPtr)
    Tcl_Interp *interp;
    Tk_Window tkwin;
    char *string;
    int *xPtr, *yPtr;
{
    char *comma;
    int result;
    int x, y;

    if ((string == NULL) || (*string == '\0')) {
	*xPtr = *yPtr = -SHRT_MAX;
	return TCL_OK;
    }
    if (*string != '@') {
	goto badFormat;
    }
    comma = strchr(string + 1, ',');
    if (comma == NULL) {
	goto badFormat;
    }
    *comma = '\0';
    result = ((Tk_GetPixels(interp, tkwin, string + 1, &x) == TCL_OK) &&
	(Tk_GetPixels(interp, tkwin, comma + 1, &y) == TCL_OK));
    *comma = ',';
    if (!result) {
	Tcl_AppendResult(interp, ": can't parse position \"", string, "\"",
	    (char *)NULL);
	return TCL_ERROR;
    }
    *xPtr = x, *yPtr = y;
    return TCL_OK;

  badFormat:
    Tcl_AppendResult(interp, "bad position \"", string, "\": should be \"@x,y\"",
	(char *)NULL);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * StringToPosition --
 *
 *	Convert the string representation of a legend XY position into
 *	window coordinates.  The form of the string must be "@x,y" or
 *	none.
 *
 * Results:
 *	A standard Tcl result.  The symbol type is written into the
 *	widget record.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToPosition(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* New legend position string */
    char *widgRec;		/* Widget record */
    int offset;			/* offset to XPoint structure */
{
    XPoint *pointPtr = (XPoint *)(widgRec + offset);
    int x, y;

    if (Blt_GetXYPosition(interp, tkwin, string, &x, &y) != TCL_OK) {
	return TCL_ERROR;
    }
    pointPtr->x = x, pointPtr->y = y;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * PositionToString --
 *
 *	Convert the window coordinates into a string.
 *
 * Results:
 *	The string representing the coordinate position is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
PositionToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Widget record */
    int offset;			/* offset of XPoint in record */
    Tcl_FreeProc **freeProcPtr;	/* Memory deallocation scheme to use */
{
    char *result;
    XPoint *pointPtr = (XPoint *)(widgRec + offset);

    result = "";
    if ((pointPtr->x != -SHRT_MAX) && (pointPtr->y != -SHRT_MAX)) {
	char string[200];

	sprintf(string, "@%d,%d", pointPtr->x, pointPtr->y);
	result = strdup(string);
	assert(result);
	*freeProcPtr = (Tcl_FreeProc *)free;
    }
    return (result);
}

/*LINTLIBRARY*/
static int
GetColorPair(interp, tkwin, fgStr, bgStr, pairPtr, allowDefault)
    Tcl_Interp *interp;
    Tk_Window tkwin;
    char *fgStr, *bgStr;
    ColorPair *pairPtr;
    int allowDefault;
{
    unsigned int length;
    XColor *fgColor, *bgColor;
    
    fgColor = bgColor = NULL;
    length = strlen(fgStr);
    if (fgStr[0] == '\0') {
	fgColor = NULL;
    } else if ((allowDefault) && (fgStr[0] == 'd') &&
	(strncmp(fgStr, "defcolor", length) == 0)) {
	fgColor = COLOR_DEFAULT;
    } else {
	fgColor = Tk_GetColor(interp, tkwin, Tk_GetUid(fgStr));
	if (fgColor == NULL) {
	    return TCL_ERROR;
	}
    }
    length = strlen(bgStr);
    if (bgStr[0] == '\0') {
	bgColor = NULL;
    } else if ((allowDefault) && (bgStr[0] == 'd') && 
	(strncmp(bgStr, "defcolor", length) == 0)) {
	bgColor = COLOR_DEFAULT;
    } else {
	bgColor = Tk_GetColor(interp, tkwin, Tk_GetUid(bgStr));
	if (bgColor == NULL) {
	    return TCL_ERROR;
	}
    }
    pairPtr->fgColor = fgColor;
    pairPtr->bgColor = bgColor;
    return TCL_OK;
}

void
Blt_FreeColorPair(pairPtr)
    ColorPair *pairPtr;
{
    if ((pairPtr->bgColor != NULL) && (pairPtr->bgColor != COLOR_DEFAULT)) {
	Tk_FreeColor(pairPtr->bgColor);
    }
    if ((pairPtr->fgColor != NULL) && (pairPtr->fgColor != COLOR_DEFAULT)) {
	Tk_FreeColor(pairPtr->fgColor);
    }
    pairPtr->bgColor = pairPtr->fgColor = NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * StringToColorPair --
 *
 *	Convert the color names into pair of XColor pointers.
 *
 * Results:
 *	A standard Tcl result.  The color pointer is written into the
 *	widget record.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToColorPair(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* String representing color */
    char *widgRec;		/* Widget record */
    int offset;			/* Offset of color field in record */
{
    ColorPair *pairPtr = (ColorPair *)(widgRec + offset);
    ColorPair sample;
    int allowDefault = (int)clientData;

    sample.fgColor = sample.bgColor = NULL;
    if ((string != NULL) && (*string != '\0')) {
	int numColors;
	char **colors;
	int result;

	if (Tcl_SplitList(interp, string, &numColors, &colors) != TCL_OK) {
	    return TCL_ERROR;
	}
	result = TCL_ERROR;
	switch (numColors) {
	case 0:
	    result = TCL_OK;
	    break;
	case 1:
	    result = GetColorPair(interp, tkwin, colors[0], "", &sample,
		allowDefault);
	    break;
	case 2:
	    result = GetColorPair(interp, tkwin, colors[0], colors[1],
		&sample, allowDefault);
	    break;
	default:
	    result = TCL_ERROR;
	    Tcl_AppendResult(interp, "too many names in colors list",
		(char *)NULL);
	}
	free((char *)colors);
	if (result != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    Blt_FreeColorPair(pairPtr);
    *pairPtr = sample;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * NameOfColor --
 *
 *	Convert the color option value into a string.
 *
 * Results:
 *	The static string representing the color option is returned.
 *
 *----------------------------------------------------------------------
 */
static char *
NameOfColor(colorPtr)
    XColor *colorPtr;
{
    if (colorPtr == NULL) {
	return "";
    } else if (colorPtr == COLOR_DEFAULT) {
	return "defcolor";
    } else {
	return Tk_NameOfColor(colorPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ColorPairToString --
 *
 *	Convert the color pairs into color names.
 *
 * Results:
 *	The string representing the symbol color is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
ColorPairToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Element information record */
    int offset;			/* Offset of symbol type field in record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    ColorPair *pairPtr = (ColorPair *)(widgRec + offset);
    Tcl_DString dStr;
    char *result;

    Tcl_DStringInit(&dStr);
    Tcl_DStringAppendElement(&dStr, NameOfColor(pairPtr->fgColor));
    Tcl_DStringAppendElement(&dStr, NameOfColor(pairPtr->bgColor));
    result = Tcl_DStringValue(&dStr);
    if (result == dStr.staticSpace) {
	result = strdup(result);
    }
    *freeProcPtr = (Tcl_FreeProc *)free;
    return (result);
}

#ifdef notdef
/*
 *----------------------------------------------------------------------
 *
 * StringToMapped --
 *
 *	Convert the boolean into an integer, reversing its value.
 *
 * Results:
 *	The return value is a standard Tcl result.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToMapped(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* String representing color */
    char *widgRec;		/* Widget record */
    int offset;			/* Offset of color field in record */
{
    int *hiddenPtr = (int *)(widgRec + offset);
    int bool;

    if (Tcl_GetBoolean(interp, string, &bool) != TCL_OK) {
	return TCL_ERROR;
    }
    *hiddenPtr = (bool == 0);	/* Reverse the meaning of the flag*/
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * MappedToString --
 *
 *	Generate a string representing the reverse value the boolean.
 *
 * Results:
 *	The string representing the boolean is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
MappedToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Element information record */
    int offset;			/* Offset of symbol type field in record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    int *hiddenPtr = (int *)(widgRec + offset);

    return (*hiddenPtr) ? "0" : "1";
}

#endif

/*
 * ----------------------------------------------------------------------
 *
 * Blt_GetCoordinate --
 *
 * 	Convert the expression string into a floating point value. The
 *	only reason we use this routine instead of Blt_ExprDouble is to
 *	handle "elastic" bounds.  That is, convert the strings "-Inf",
 *	"Inf" into -(DBL_MAX) and DBL_MAX respectively.
 *
 * Results:
 *	The return value is a standard Tcl result.  The value of the
 * 	expression is passed back via valuePtr.
 *
 * ----------------------------------------------------------------------
 */
int
Blt_GetCoordinate(interp, expr, valuePtr)
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    char *expr;			/* Numeric expression string to parse */
    double *valuePtr;		/* Real-valued result of expression */
{
    char c;

    c = expr[0];
    if ((c == 'I') && (strcmp(expr, "Inf") == 0)) {
	*valuePtr = bltPosInfinity;	/* Elastic upper bound */
    } else if ((c == '-') && (expr[1] == 'I') && (strcmp(expr, "-Inf") == 0)) {
	*valuePtr = bltNegInfinity;	/* Elastic lower bound */
    } else if ((c == '+') && (expr[1] == 'I') && (strcmp(expr, "+Inf") == 0)) {
	*valuePtr = bltPosInfinity;	/* Elastic upper bound */
    } else if (Tcl_ExprDouble(interp, expr, valuePtr) != TCL_OK) {
	Tcl_AppendResult(interp, "bad expression \"", expr, "\"", (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_SetClipRegion --
 *
 *	Generates a bounding box representing the plotting area of
 *	the graph. This data structure is used to clip the points and
 *	line segments of the line element.
 *
 *	The clip region is the plotting area plus such arbitrary extra
 *	space.  The reason we clip with a bounding box larger than the
 *	plot area is so that symbols will be drawn even if their center
 *	point isn't in the plotting area.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	The bounding box is filled with the dimensions of the plotting
 *	area.
 *
 *----------------------------------------------------------------------
 */
void
Blt_SetClipRegion(graphPtr, extentsPtr)
    Graph *graphPtr;
    Extents2D *extentsPtr;
{
    extentsPtr->xMin = (double)(graphPtr->hOffset - 50);
    extentsPtr->yMin = (double)(graphPtr->vOffset - 50);
    extentsPtr->xMax = (double)(graphPtr->hOffset + graphPtr->hRange + 50);
    extentsPtr->yMax = (double)(graphPtr->vOffset + graphPtr->vRange + 50);
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

int
Blt_ClipSegment(extentsPtr, p1Ptr, p2Ptr, segPtr)
    Extents2D *extentsPtr;
    Point2D *p1Ptr, *p2Ptr;	/* Points of line segment to be clipped */
    XSegment *segPtr;
{
    int code1, code2;
    int inside, outside;
    Point2D p, q;		/* Points of the clipped line segment */

    code1 = OutCode(extentsPtr, p1Ptr->x, p1Ptr->y);
    code2 = OutCode(extentsPtr, p2Ptr->x, p2Ptr->y);

    inside = ((code1 | code2) == 0);
    outside = ((code1 & code2) != 0);

    p.x = p1Ptr->x, p.y = p1Ptr->y;
    q.x = p2Ptr->x, q.y = p2Ptr->y;

    /*
     * In the worst case, we'll clip the line segment against each of
     * the four sides of the bounding rectangle.
     */
    while ((!outside) && (!inside)) {
	if (code1 == 0) {
	    double tmp;
	    int code;

	    tmp = p.x, p.x = q.x, q.x = tmp;
	    tmp = p.y, p.y = q.y, q.y = tmp;
	    code = code1, code1 = code2, code2 = code;
	}
	if (code1 & CLIP_LEFT) {
	    p.y += (q.y - p.y) * (extentsPtr->xMin - p.x) / (q.x - p.x);
	    p.x = extentsPtr->xMin;
	} else if (code1 & CLIP_RIGHT) {
	    p.y += (q.y - p.y) * (extentsPtr->xMax - p.x) / (q.x - p.x);
	    p.x = extentsPtr->xMax;
	} else if (code1 & CLIP_BOTTOM) {
	    p.x += (q.x - p.x) * (extentsPtr->yMax - p.y) / (q.y - p.y);
	    p.y = extentsPtr->yMax;
	} else if (code1 & CLIP_TOP) {
	    p.x += (q.x - p.x) * (extentsPtr->yMin - p.y) / (q.y - p.y);
	    p.y = extentsPtr->yMin;
	}
	code1 = OutCode(extentsPtr, p.x, p.y);

	inside = ((code1 | code2) == 0);
	outside = ((code1 & code2) != 0);
    }
    if ((inside) && (segPtr != NULL)) {
	segPtr->x1 = ROUND(p.x);
	segPtr->y1 = ROUND(p.y);
	segPtr->x2 = ROUND(q.x);
	segPtr->y2 = ROUND(q.y);
    }
    return inside;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_GetProjection --
 *
 *	Computes the projection of a point on a line.  The line (given
 *	by two points), is assumed the be infinite.
 *
 *	Compute the slope (angle) of the line and rotate it 90 degrees.
 *	Using the slope-intercept method (we know the second line from
 *	the sample test point and the computed slope), then find the
 *	intersection of both lines. This will be the projection of the
 *	sample point on the first line.
 *
 * Results:
 *	Returns the coordinates of the projection on the line.
 *
 *----------------------------------------------------------------------
 */
Point2D
Blt_GetProjection(x, y, x1, y1, x2, y2)
    int x, y;			/* Screen coordinates of the sample point. */
    int x1, y1, x2, y2;		/* Line segment to project point onto */
{
    double dx, dy;
    Point2D proj;

    dx = (double)(x1 - x2);
    dy = (double)(y1 - y2);

    /* Find the slope and y-intercept of the line segment */

    /* Make a quick test for horizontal and vertical lines */
    if (FABS(dx) < DBL_EPSILON) {
	proj.x = (double)x1, proj.y = (double)y;
    } else if (FABS(dy) < DBL_EPSILON) {
	proj.x = (double)x, proj.y = (double)y1;
    } else {
	double m1, m2;		/* Slope of both lines */
	double b1, b2;		/* y-intercepts */
	Point2D mid, p, q;

	m1 = (dy / dx);
	b1 = y1 - (x1 * m1);

	/* Rotate the line segment 90 degrees and compute its slope */

	mid.x = (x1 + x2) * 0.5;/* Find midpoint of segment */
	mid.y = (y1 + y2) * 0.5;

	p.x = mid.x - (0.5 * dy);
	p.y = mid.y - (0.5 * -dx);
	q.x = mid.x + (0.5 * dy);
	q.y = mid.y + (0.5 * -dx);

	m2 = (p.y - q.y) / (p.x - q.x);
	b2 = y - (x * m2);

	/*
	 * Given the equations of two lines which contain the same point,
	 *
	 *    y = m1 * x + b1
	 *    y = m2 * x + b2
	 *
	 * solve for the intersection.
	 *
	 *    x = (b2 - b1) / (m1 - m2)
	 *    y = m1 * x + b1
	 *
	 */

	proj.x = (b2 - b1) / (m1 - m2);
	proj.y = m1 * proj.x + b1;
    }
    return proj;
}

#define SetColor(c,r,g,b) ((c)->red = (int)((r) * 65535.0), \
			   (c)->green = (int)((g) * 65535.0), \
			   (c)->blue = (int)((b) * 65535.0))

void
Blt_HSV(colorPtr, huePtr, valPtr, satPtr)
    XColor *colorPtr;
    double *huePtr, *valPtr, *satPtr;
{
    unsigned short maxInt, minInt;
    double range;
    unsigned short *primArr;
    register int i;
    double hue, sat, val;

    /* Compute the minimum and maximum intensities */
    primArr = (unsigned short *)&colorPtr->red;
    maxInt = minInt = primArr[0];
    for (i = 1; i < 3; i++) {
	if (maxInt < primArr[i]) {
	    maxInt = primArr[i];
	} else if (minInt > primArr[i]) {
	    minInt = primArr[i];
	}
    }

    val = (double)maxInt / 65535.0;
    hue = 0.0, sat = 0.0;

    range = (double)maxInt - (double)minInt;
    if (maxInt != minInt) {
	sat = range / (double)maxInt;
    }
    if (sat > 0.0) {
	double r, g, b;

	/* Normalize the RGB values */
	r = ((double)maxInt - (double)colorPtr->red) / range;
	g = ((double)maxInt - (double)colorPtr->green) / range;
	b = ((double)maxInt - (double)colorPtr->blue) / range;

	if (colorPtr->red == maxInt) {
	    hue = (double)(b - g);
	} else if (colorPtr->green == maxInt) {
	    hue = (double)(2 + (r - b));
	} else if (colorPtr->blue == maxInt) {
	    hue = (double)(4 + (g - r));
	}
	hue *= 60.0;
    } else {
	sat = 0.5;
    }
    if (hue < 0.0) {
	hue += 360.0;
    }
    *huePtr = hue;
    *valPtr = val;
    *satPtr = sat;
}

void
Blt_RGB(hue, sat, val, colorPtr)
    double hue, sat, val;
    XColor *colorPtr;
{
    double p, q, t;
    double frac;
    int ihue;

    if (val < 0.0) {
	val = 0.0;
    } else if (val > 1.0) {
	val = 1.0;
    }
    if (sat == 0.0) {
	SetColor(colorPtr, val, val, val);
	return;
    }
    hue = FMOD(hue, 360.0) / 60.0;
    ihue = (int)floor(hue);
    frac = hue - ihue;
    p = val * (1 - sat);
    q = val * (1 - (sat * frac));
    t = val * (1 - (sat * (1 - frac)));

    switch (ihue) {
    case 0:
	SetColor(colorPtr, val, t, p);
	break;
    case 1:
	SetColor(colorPtr, q, val, p);
	break;
    case 2:
	SetColor(colorPtr, p, val, t);
	break;
    case 3:
	SetColor(colorPtr, p, q, val);
	break;
    case 4:
	SetColor(colorPtr, t, p, val);
	break;
    case 5:
	SetColor(colorPtr, val, p, q);
	break;
    }
}
