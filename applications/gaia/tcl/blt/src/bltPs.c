
/*
 * bltPs.c --
 *
 *      This module implements general PostScript conversion routines.
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

#include "bltInt.h"
#include "bltPs.h"

#include <X11/Xutil.h>
#include <X11/Xatom.h>
#if defined(__STDC__)
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#define PS_MAXPATH	1500	/* Maximum number of components in a PostScript
				 * (level 1) path. */

Printable
Blt_PrintObject(interp, tkwin, dStrPtr)
    Tcl_Interp *interp;
    Tk_Window tkwin;
    Tcl_DString *dStrPtr;
{
    struct Printable *printPtr;

    printPtr = (struct Printable *)malloc(sizeof(struct Printable));
    assert(printPtr);
    printPtr->fontVarName = printPtr->colorVarName = NULL;
    printPtr->interp = interp;
    printPtr->tkwin = tkwin;
    printPtr->dStrPtr = dStrPtr;
    printPtr->colorMode = PS_MODE_COLOR;
    return printPtr;
}

void
     Blt_PrintAppend
TCL_VARARGS_DEF(Printable, arg1)
{
    va_list argList;
    Printable printable;
    char *string;

    printable = TCL_VARARGS_START(Printable, arg1, argList);
    for (;;) {
	string = va_arg(argList, char *);
	if (string == NULL) {
	    break;
	}
	Tcl_DStringAppend(printable->dStrPtr, string, -1);
    }
}

void
     Blt_PrintFormat
TCL_VARARGS_DEF(Printable, arg1)
{
    va_list argList;
    Printable printable;
    char *fmt;

    printable = TCL_VARARGS_START(Printable, arg1, argList);
    fmt = va_arg(argList, char *);
    vsprintf(printable->scratchArr, fmt, argList);
    va_end(argList);
    Tcl_DStringAppend(printable->dStrPtr, printable->scratchArr, -1);
}

int
Blt_FileToPostScript(printable, fileName)
    Printable printable;
    char *fileName;
{
    FILE *f;
    char *libDir;
    char *pathName;
    Tcl_DString filePath;
    Tcl_Interp *interp = printable->interp;

    /*
     * Read a standard prolog file from file and append it to the
     * generated PostScript output stored in interp->result.
     */
    libDir = Tcl_GetVar(interp, "blt_library", TCL_GLOBAL_ONLY);
    if (libDir == NULL) {
	Tcl_AppendResult(interp, "couldn't find BLT script library:",
	    "global variable \"blt_library\" doesn't exist", (char *)NULL);
	return TCL_ERROR;
    }
    Tcl_DStringInit(&filePath);
    Tcl_DStringAppend(&filePath, libDir, -1);
    Tcl_DStringAppend(&filePath, "/", -1);
    Tcl_DStringAppend(&filePath, fileName, -1);
    pathName = Tcl_DStringValue(&filePath);

    f = fopen(pathName, "r");
    if (f == NULL) {
	Tcl_AppendResult(interp, "couldn't open prologue file \"", pathName,
	    "\": ", Tcl_PosixError(interp), (char *)NULL);
	return TCL_ERROR;
    }
    Blt_PrintAppend(printable, "\n% including file \"", pathName, "\"\n\n",
	(char *)NULL);

    while (fgets(printable->scratchArr, PRINTABLE_SCRATCH_LENGTH, f) != NULL) {
	Tcl_DStringAppend(printable->dStrPtr, printable->scratchArr, -1);
    }
    if (ferror(f)) {
	Tcl_AppendResult(interp, "error reading prologue file \"", pathName,
	    "\": ", Tcl_PosixError(interp), (char *)NULL);
	fclose(f);
	Tcl_DStringFree(&filePath);
	return TCL_ERROR;
    }
    fclose(f);
    Tcl_DStringFree(&filePath);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * XColorToPostScript --
 *
 *	Convert the a XColor (from its RGB values) to a PostScript
 *	command.  If a Tcl color map variable exists, it will be
 *	consulted for a PostScript translation based upon the color
 *	name.
 *
 * Results:
 *	The string representing the color mode is returned.
 *
 *----------------------------------------------------------------------
 */
static void
XColorToPostScript(printable, colorPtr)
    Printable printable;
    XColor *colorPtr;		/* Color value to be converted */
{
    /*
     * Remember when mapping the intergral color intensity to
     * the interval [0..1], color values run from 0 to 65535,
     * not 1 to 65536.
     */
    sprintf(printable->scratchArr, "%g %g %g",
	((double)colorPtr->red / 65535.0),
	((double)colorPtr->green / 65535.0),
	((double)colorPtr->blue / 65535.0));
    Tcl_DStringAppend(printable->dStrPtr, printable->scratchArr, -1);
}

void
Blt_BackgroundToPostScript(printable, colorPtr)
    Printable printable;
    XColor *colorPtr;
{
    /* If the color name exists in Tcl array variable, use that translation */
    if (printable->colorVarName != NULL) {
	char *colorDesc;

	colorDesc = Tcl_GetVar2(printable->interp, printable->colorVarName,
	    Tk_NameOfColor(colorPtr), 0);
	if (colorDesc != NULL) {
	    Blt_PrintAppend(printable, colorDesc, " ", (char *)NULL);
	    return;
	}
    } else {
	XColorToPostScript(printable, colorPtr);
    }
    Tcl_DStringAppend(printable->dStrPtr, " SetBgColor\n", -1);
}

void
Blt_ForegroundToPostScript(printable, colorPtr)
    Printable printable;
    XColor *colorPtr;
{
    /* If the color name exists in Tcl array variable, use that translation */
    if (printable->colorVarName != NULL) {
	char *colorDesc;

	colorDesc = Tcl_GetVar2(printable->interp, printable->colorVarName,
	    Tk_NameOfColor(colorPtr), 0);
	if (colorDesc != NULL) {
	    Blt_PrintAppend(printable, colorDesc, " ", (char *)NULL);
	    return;
	}
    } else {
	XColorToPostScript(printable, colorPtr);
    }
    Tcl_DStringAppend(printable->dStrPtr, " SetFgColor\n", -1);
}

/*
 *----------------------------------------------------------------------
 *
 * ReverseBits --
 *
 *	Convert a byte from a X image into PostScript image order.
 *	This requires not only the nybbles to be reversed but also
 *	their bit values.
 *
 * Results:
 *	The converted byte is returned.
 *
 *----------------------------------------------------------------------
 */
INLINE static unsigned char
ReverseBits(byte)
    register unsigned char byte;
{
    byte = ((byte >> 1) & 0x55) | ((byte << 1) & 0xaa);
    byte = ((byte >> 2) & 0x33) | ((byte << 2) & 0xcc);
    byte = ((byte >> 4) & 0x0f) | ((byte << 4) & 0xf0);
    return byte;
}

/*
 *----------------------------------------------------------------------
 *
 * ByteToHex --
 *
 *	Convert a byte to its ASCII hexidecimal equivalent.
 *
 * Results:
 *	The converted 2 ASCII character string is returned.
 *
 *----------------------------------------------------------------------
 */
INLINE static void
ByteToHex(byte, string)
    register unsigned char byte;
    char *string;
{
    static char hexDigits[] = "0123456789ABCDEF";

    string[0] = hexDigits[byte >> 4];
    string[1] = hexDigits[byte & 0x0F];
}

/*
 * -------------------------------------------------------------------------
 *
 * Blt_BitmapToPostScript --
 *
 *      Output a PostScript image string of the given bitmap image.
 *      It is assumed the image is one bit deep and a zero value
 *      indicates an off-pixel.  To convert to PostScript, the bits
 *      need to be reversed from the X11 image order.
 *
 * Results:
 *      None.
 *
 * Side Effects:
 *      The PostScript image string is appended to interp->result.
 *
 * -------------------------------------------------------------------------
 */
void
Blt_BitmapToPostScript(printable, display, bitmap, width, height)
    Printable printable;
    Display *display;
    Pixmap bitmap;
    int width, height;
{
    register unsigned char byte = 0;
    register int x, y, bitPos;
    unsigned long pixel;
#ifdef WIN32
    TkWinDCState state;
    HDC image;
#else 
    XImage *imagePtr;
#endif /*WIN32*/
    int byteCount;
    char string[10];

#ifdef WIN32
    image = TkWinGetDrawableDC(display, bitmap, &state);
#else 
    imagePtr = XGetImage(display, bitmap, 0, 0, width, height, 1, ZPixmap);
#endif /*WIN32*/
    Tcl_DStringAppend(printable->dStrPtr, "\t<", -1);
    byteCount = bitPos = 0;	/* Suppress compiler warning */
    for (y = 0; y < height; y++) {
	byte = 0;
	for (x = 0; x < width; x++) {
#ifdef WIN32
	    pixel = GetPixel(image, x, y);
#else 
	    pixel = XGetPixel(imagePtr, x, y);
#endif /*WIN32*/
	    bitPos = x % 8;
	    byte |= (unsigned char)(pixel << bitPos);
	    if (bitPos == 7) {
		byte = ReverseBits(byte);
		ByteToHex(byte, string);
		string[2] = '\0';
		byteCount++;
		byte = 0;
		if (byteCount >= 30) {
		    string[2] = '\n';
		    string[3] = '\t';
		    string[4] = '\0';
		    byteCount = 0;
		}
		Tcl_DStringAppend(printable->dStrPtr, string, -1);
	    }
	}			/* x */
	if (bitPos != 7) {
	    byte = ReverseBits(byte);
	    ByteToHex(byte, string);
	    string[2] = '\0';
	    Tcl_DStringAppend(printable->dStrPtr, string, -1);
	    byteCount++;
	}
    }				/* y */
    Tcl_DStringAppend(printable->dStrPtr, ">\n", -1);
#ifdef WIN32
    TkWinReleaseDrawableDC(bitmap, image, &state);
#else 
    XDestroyImage(imagePtr);
#endif /*WIN32*/
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ColorImageToPsData --
 *
 *	Converts a color image to PostScript RGB (3 components)
 *	or Greyscale (1 component) output.  With 3 components, we
 *	assume the "colorimage" operator is available.
 *
 *	Note that the image converted from bottom to top, to conform
 *	to the PostScript coordinate system.
 *
 * Results:
 *	The PostScript data comprising the color image is written
 *	into the dynamic string.
 *
 *----------------------------------------------------------------------
 */
void
Blt_ColorImageToPsData(image, numComponents, dStrPtr, prefix)
    ColorImage image;
    int numComponents;
    Tcl_DString *dStrPtr;
    char *prefix;
{
    char string[10];
    register int count;
    register int x, y;
    register Pix32 *pixelPtr;
    int width, height;
    int offset;

    width = ColorImageWidth(image);
    height = ColorImageHeight(image);

    count = 0;
    Tcl_DStringAppend(dStrPtr, prefix, -1);
    offset = (height - 1) * width;
    if (numComponents == 3) {
	for (y = (height - 1); y >= 0; y--) {
	    pixelPtr = ColorImageData(image) + offset;
	    for (x = 0; x < width; x++, pixelPtr++) {
		count += 6;
		ByteToHex(pixelPtr->Red, string);
		ByteToHex(pixelPtr->Green, string + 2);
		ByteToHex(pixelPtr->Blue, string + 4);
		string[6] = '\0';
		if (count >= 60) {
		    string[6] = '\n';
		    string[7] = prefix[0];
		    string[8] = '\0';
		    count = 0;
		}
		Tcl_DStringAppend(dStrPtr, string, -1);
	    }
	    offset -= width;
	}
    } else if (numComponents == 1) {
	for (y = (height - 1); y >= 0; y--) {
	    pixelPtr = ColorImageData(image) + offset;
	    for (x = 0; x < width; x++, pixelPtr++) {
		count += 2;
		ByteToHex(pixelPtr->Red, string);
		string[2] = '\0';
		if (count >= 60) {
		    string[2] = '\n';
		    string[3] = prefix[0];
		    string[4] = '\0';
		    count = 0;
		}
		Tcl_DStringAppend(dStrPtr, string, -1);
	    }
	    offset -= width;
	}
    }
    Tcl_DStringAppend(dStrPtr, "\n", -1);
}

/*
 *----------------------------------------------------------------------
 *
 * NameOfAtom --
 *
 *	Wrapper routine for Tk_GetAtomName.  Returns NULL instead of
 *	"?bad atom?" if the atom cannot be found.
 *
 * Results:
 *	The name of the atom is returned if found. Otherwise NULL.
 *
 *----------------------------------------------------------------------
 */
static char *
NameOfAtom(tkwin, atom)
    Tk_Window tkwin;
    Atom atom;
{
    char *result;

    result = Tk_GetAtomName(tkwin, atom);
    if ((result[0] == '?') && (strcmp(result, "?bad atom?") == 0)) {
	return NULL;
    }
    return (result);
}

#ifndef  WIN32

typedef struct FontMap {
    char *alias;
    char *fontName;
} FontMap;

static FontMap psFontMap[] =
{
    {"Arial", "Helvetica",},
    {"AvantGarde", "AvantGarde",},
    {"Courier New", "Courier",},
    {"Courier", "Courier",},
    {"Geneva", "Helvetica",},
    {"Helvetica", "Helvetica",},
    {"Monaco", "Courier",},
    {"New Century Schoolbook", "NewCenturySchlbk",},
    {"New York", "Times",},
    {"Palatino", "Palatino",},
    {"Symbol", "Symbol",},
    {"Times New Roman", "Times",},
    {"Times Roman", "Times",},
    {"Times", "Times",},
    {"Utopia", "Utopia",},
    {"ZapfChancery", "ZapfChancery",},
    {"ZapfDingbats", "ZapfDingbats",},
};

static int numFontNames = (sizeof(psFontMap) / sizeof(FontMap));

/*
 * -----------------------------------------------------------------
 *
 * XFontStructToPostScript --
 *
 *      Map X11 font to a PostScript font. Currently, only fonts whose
 *      FOUNDRY property are "Adobe" are converted. Simply gets the
 *      XA_FULL_NAME and XA_FAMILY properties and pieces together a
 *      PostScript fontname.
 *
 * Results:
 *      Returns the mapped PostScript font name if one is possible.
 *	Otherwise returns NULL.
 *
 * -----------------------------------------------------------------
 */
static char *
XFontStructToPostScript(tkwin, fontPtr)
    Tk_Window tkwin;		/* Window to query for atoms */
    XFontStruct *fontPtr;	/* Font structure to map to name */
{
    Atom atom;
    char *fullName, *family, *foundry;
    register char *src, *dest;
    char *start;
    static char string[200];	/* What size? */

    if (XGetFontProperty(fontPtr, XA_FULL_NAME, &atom) == False) {
	return NULL;
    }
    fullName = NameOfAtom(tkwin, atom);
    if (fullName == NULL) {
	return NULL;
    }
    family = foundry = NULL;
    if (XGetFontProperty(fontPtr, Tk_InternAtom(tkwin, "FOUNDRY"), &atom)) {
	foundry = NameOfAtom(tkwin, atom);
    }
    if (XGetFontProperty(fontPtr, XA_FAMILY_NAME, &atom)) {
	family = NameOfAtom(tkwin, atom);
    }
    /*
     * Try to map the font only if the foundry is Adobe
     */
    if ((foundry == NULL) || (family == NULL)) {
	return NULL;
    }
    src = fullName + strlen(family);
    if (strcmp(foundry, "Adobe") != 0) {
	register int i;

	if (strncasecmp(family, "itc ", 4) == 0) {
	    family += 4;	/* Throw out the "itc" prefix */
	}
	for (i = 0; i < numFontNames; i++) {
	    if (strcasecmp(family, psFontMap[i].alias) == 0) {
		family = psFontMap[i].fontName;
	    }
	}
	if (i == numFontNames) {
	    family = "Helvetica";	/* Default to a known font */
	}
    }
    /*
     * PostScript font name is in the form <family>-<type face>
     */
    sprintf(string, "%s-", family);
    dest = start = string + strlen(string);

    /*
     * Append the type face (part of the full name trailing the family name)
     * to the the PostScript font name, removing any spaces or dashes
     *
     * ex. " Bold Italic" ==> "BoldItalic"
     */
    while (*src != '\0') {
	if ((*src != ' ') && (*src != '-')) {
	    *dest++ = *src;
	}
	src++;
    }
    if (dest == start) {
	--dest;			/* Remove '-' to leave just the family name */
    }
    *dest = '\0';		/* Make a valid string */
    return string;
}
#endif /* !WIN32 */


/*
 * -------------------------------------------------------------------
 * Routines to convert X drawing functions to PostScript commands.
 * -------------------------------------------------------------------
 */
void
Blt_ClearBackgroundToPostScript(printable)
    Printable printable;
{
    Tcl_DStringAppend(printable->dStrPtr, " 1.0 1.0 1.0 SetBgColor\n", -1);
}

void
Blt_CapStyleToPostScript(printable, capStyle)
    Printable printable;
    int capStyle;
{
    /*
     * X11:not last = 0, butt = 1, round = 2, projecting = 3
     * PS: butt = 0, round = 1, projecting = 2
     */
    if (capStyle > 0) {
	capStyle--;
    }
    sprintf(printable->scratchArr, "%d setlinecap\n", capStyle);
    Tcl_DStringAppend(printable->dStrPtr, printable->scratchArr, -1);
}

void
Blt_JoinStyleToPostScript(printable, joinStyle)
    Printable printable;
    int joinStyle;
{
    /*
     * miter = 0, round = 1, bevel = 2
     */
    sprintf(printable->scratchArr, "%d setlinejoin\n", joinStyle);
    Tcl_DStringAppend(printable->dStrPtr, printable->scratchArr, -1);
}

void
Blt_LineWidthToPostScript(printable, lineWidth)
    Printable printable;
    int lineWidth;
{
    if (lineWidth < 1) {
	lineWidth = 1;
    }
    sprintf(printable->scratchArr, "%d setlinewidth\n", lineWidth);
    Tcl_DStringAppend(printable->dStrPtr, printable->scratchArr, -1);
}

void
Blt_LineDashesToPostScript(printable, dashesPtr)
    Printable printable;
    Dashes *dashesPtr;
{

    Tcl_DStringAppend(printable->dStrPtr, "[ ", -1);
    if (dashesPtr != NULL) {
	register int i;

	for (i = 0; i < dashesPtr->numValues; i++) {
	    Blt_PrintFormat(printable, " %d", dashesPtr->valueArr[i]);
	}
    }
    Tcl_DStringAppend(printable->dStrPtr, "] 0 setdash\n", -1);
}

void
Blt_LineAttributesToPostScript(printable, colorPtr, lineWidth, dashesPtr,
    capStyle, joinStyle)
    Printable printable;
    XColor *colorPtr;
    int lineWidth;
    Dashes *dashesPtr;
    int capStyle, joinStyle;
{
    Blt_JoinStyleToPostScript(printable, joinStyle);
    Blt_CapStyleToPostScript(printable, capStyle);
    Blt_ForegroundToPostScript(printable, colorPtr);
    Blt_LineWidthToPostScript(printable, lineWidth);
    Blt_LineDashesToPostScript(printable, dashesPtr);
}

void
Blt_RectangleToPostScript(printable, x, y, width, height)
    Printable printable;
    int x, y;
    int width, height;
{
    Blt_PrintFormat(printable, "%d %d %d %d Box Fill\n\n", x, y, width - 1,
	height - 1);
}

void
Blt_LinesToPostScript(printable, pointPtr, numPoints)
    Printable printable;
    register XPoint *pointPtr;
    int numPoints;
{
    register int i;

    Blt_PrintFormat(printable, "newpath %d %d moveto\n", pointPtr->x,
	pointPtr->y);
    pointPtr++;
    for (i = 1; i < numPoints; i++, pointPtr++) {
	Blt_PrintFormat(printable, "%d %d lineto\n",
	    pointPtr->x, pointPtr->y);
    }
}

void
Blt_PolygonToPostScript(printable, pointArr, numPoints)
    Printable printable;
    XPoint *pointArr;
    int numPoints;
{
    Blt_LinesToPostScript(printable, pointArr, numPoints);
    Blt_PrintFormat(printable, "%d %d ", pointArr[0].x, pointArr[0].y);
    Blt_PrintAppend(printable, " lineto closepath Fill\n", (char *)NULL);
}

void
Blt_SegmentsToPostScript(printable, segPtr, numSegments)
    Printable printable;
    register XSegment *segPtr;
    int numSegments;
{
    register int i;

    for (i = 0; i < numSegments; i++, segPtr++) {
	Blt_PrintFormat(printable, "%d %d %d %d LS\n", segPtr->x1,
	    segPtr->y1, segPtr->x2, segPtr->y2);
    }
}


void
Blt_RectanglesToPostScript(printable, rectArr, numRects)
    Printable printable;
    XRectangle rectArr[];
    int numRects;
{
    register int i;

    for (i = 0; i < numRects; i++) {
	Blt_RectangleToPostScript(printable, rectArr[i].x, rectArr[i].y,
	    (int)rectArr[i].width, (int)rectArr[i].height);
    }
}

void
Blt_3DRectangleToPostScript(printable, border, x, y, width, height,
    borderWidth, relief)
    Printable printable;
    Tk_3DBorder border;		/* Token for border to draw. */
    int x, y;			/* Coordinates of rectangle */
    int width, height;		/* Region to be drawn. */
    int borderWidth;		/* Desired width for border, in pixels. */
    int relief;			/* Should be either TK_RELIEF_RAISED or
                                 * TK_RELIEF_SUNKEN;  indicates position of
                                 * interior of window relative to exterior. */
{
    TkBorder *borderPtr = (TkBorder *) border;
    XColor lightColor, darkColor;
    XColor *lightColorPtr, *darkColorPtr;
    XColor *topColor, *bottomColor;
    XPoint points[7];
    int twiceWidth = (borderWidth * 2);

    if ((width < twiceWidth) || (height < twiceWidth)) {
	return;
    }
    if ((borderPtr->lightColor == NULL) || (borderPtr->darkColor == NULL)) {
	Screen *screenPtr;

	lightColor = *borderPtr->bgColor;
	screenPtr = Tk_Screen(printable->tkwin);
	if (lightColor.pixel == WhitePixelOfScreen(screenPtr)) {
	    darkColor.red = darkColor.blue = darkColor.green = 0x00;
	} else {
	    darkColor.red = darkColor.blue = darkColor.green = 0xFF;
	}
	lightColorPtr = &lightColor;
	darkColorPtr = &darkColor;
    } else {
	lightColorPtr = borderPtr->lightColor;
	darkColorPtr = borderPtr->darkColor;
    }

    /*
     * Handle grooves and ridges with recursive calls.
     */

    if ((relief == TK_RELIEF_GROOVE) || (relief == TK_RELIEF_RIDGE)) {
	int halfWidth, insideOffset;

	halfWidth = borderWidth / 2;
	insideOffset = borderWidth - halfWidth;
	Blt_3DRectangleToPostScript(printable, border, x, y, width, height,
	    halfWidth, (relief == TK_RELIEF_GROOVE) ? TK_RELIEF_SUNKEN :
	    TK_RELIEF_RAISED);
	Blt_3DRectangleToPostScript(printable, border, x + insideOffset,
	    y + insideOffset, width - insideOffset * 2,
	    height - insideOffset * 2, halfWidth,
	    (relief == TK_RELIEF_GROOVE) ? TK_RELIEF_RAISED :
	    TK_RELIEF_SUNKEN);
	return;
    }
    if (relief == TK_RELIEF_RAISED) {
	topColor = lightColorPtr;
	bottomColor = darkColorPtr;
    } else if (relief == TK_RELIEF_SUNKEN) {
	topColor = darkColorPtr;
	bottomColor = lightColorPtr;
    } else {
	topColor = bottomColor = borderPtr->bgColor;
    }
    Blt_BackgroundToPostScript(printable, bottomColor);
    Blt_RectangleToPostScript(printable, x, y + height - borderWidth, width,
	borderWidth);
    Blt_RectangleToPostScript(printable, x + width - borderWidth, y,
	borderWidth, height);
    points[0].x = points[1].x = points[6].x = x;
    points[0].y = points[6].y = y + height;
    points[1].y = points[2].y = y;
    points[2].x = x + width;
    points[3].x = x + width - borderWidth;
    points[3].y = points[4].y = y + borderWidth;
    points[4].x = points[5].x = x + borderWidth;
    points[5].y = y + height - borderWidth;
    if (relief != TK_RELIEF_FLAT) {
	Blt_BackgroundToPostScript(printable, topColor);
    }
    Blt_PolygonToPostScript(printable, points, 7);
}

#ifdef notdef
void
Blt_Fill3DRectangleToPostScript(printable, border, x, y, width, height,
    borderWidth, relief)
    Printable printable;
    Tk_3DBorder border;		/* Token for border to draw. */
    int x, y;			/* Coordinates of top-left of border area */
    int width, height;		/* Dimension of border to be drawn. */
    int borderWidth;		/* Desired width for border, in pixels. */
    int relief;			/* Should be either TK_RELIEF_RAISED or
                                 * TK_RELIEF_SUNKEN;  indicates position of
                                 * interior of window relative to exterior. */
{
    TkBorder *borderPtr = (TkBorder *) border;

    /*
     * I'm assuming that the rectangle is to be drawn as a background.
     * Setting the pen color as foreground or background only affects
     * the plot when the colormode option is "monochrome".
     */
    Blt_BackgroundToPostScript(printable, borderPtr->bgColor);
    Blt_RectangleToPostScript(printable, x, y, width, height);
    Blt_3DRectangleToPostScript(printable, border, x, y, width, height,
	borderWidth, relief);
}

#endif

void
Blt_StippleToPostScript(printable, display, bitmap, width, height, fillOrStroke)
    Printable printable;
    Display *display;
    Pixmap bitmap;
    int width, height;
    int fillOrStroke;

{
    Blt_PrintFormat(printable, "    %d %d\n", width, height);
    Blt_BitmapToPostScript(printable, display, bitmap, width, height);
    Blt_PrintAppend(printable, "    ", (fillOrStroke) ? "true" : "false",
	" StippleFill\n", (char *)NULL);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ColorImageToPostScript --
 *
 *      Translates a color image into 3 component RGB PostScript output.
 *	Uses PS Language Level 2 operator "colorimage".
 *
 * Results:
 *      The dynamic string will contain the PostScript output.
 *
 *----------------------------------------------------------------------
 */
void
Blt_ColorImageToPostScript(printable, image, x, y)
    Printable printable;
    ColorImage image;
    int x, y;
{
    int width, height;
    int tmpSize;

    width = ColorImageWidth(image);
    height = ColorImageHeight(image);

    tmpSize = width;
    if (printable->colorMode == PS_MODE_COLOR) {
	tmpSize *= 3;
    }
    Blt_PrintFormat(printable, "\n/tmpStr %d string def\n", tmpSize);
    Blt_PrintAppend(printable, "gsave\n", (char *)NULL);
    Blt_PrintFormat(printable, "  %d %d translate\n", x, y);
    Blt_PrintFormat(printable, "  %d %d scale\n", width, height);
    Blt_PrintFormat(printable, "  %d %d 8\n", width, height);
    Blt_PrintFormat(printable, "  [%d 0 0 %d 0 %d] ", width, -height,
	height);
    Blt_PrintAppend(printable, "{\n    currentfile tmpStr readhexstring pop\n  } ",
	(char *)NULL);
    if (printable->colorMode != PS_MODE_COLOR) {
	Blt_PrintAppend(printable, "image\n", (char *)NULL);
	Blt_ColorImageToGreyscale(image, image);
	Blt_ColorImageToPsData(image, 1, printable->dStrPtr, " ");
    } else {
	Blt_PrintAppend(printable, "false 3 colorimage\n", (char *)NULL);
	Blt_ColorImageToPsData(image, 3, printable->dStrPtr, " ");
    }
    Blt_PrintAppend(printable, "\ngrestore\n\n", (char *)NULL);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_WindowToPostScript --
 *
 *      Converts a Tk window to PostScript.  If the window could not
 *	be "snapped", then a grey rectangle is drawn in its place.
 *
 * Results:
 *      None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_WindowToPostScript(printable, tkwin, x, y)
    Printable printable;
    Tk_Window tkwin;
    int x, y;
{
    ColorImage image;
    int width, height;
    ImageRegion region;

    width = Tk_Width(tkwin);
    height = Tk_Height(tkwin);
    region.x = region.y = region.width = region.height = 0;
    image = Blt_DrawableToColorImage(tkwin, Tk_WindowId(tkwin), width, height,
	&region);
    if (image == NULL) {
	/* Can't grab window image so paint the window area grey */
	Blt_PrintAppend(printable, "% Can't grab window \"",
	    Tk_PathName(tkwin), "\"\n", (char *)NULL);
	Blt_PrintAppend(printable, "0.5 0.5 0.5 SetBgColor\n",
	    (char *)NULL);
	Blt_RectangleToPostScript(printable, x, y, width, height);
	return;
    }
    Blt_ColorImageToPostScript(printable, image, x, y);
    Blt_FreeColorImage(image);
}

/*
 * -------------------------------------------------------------------------
 *
 * Blt_PhotoToPostScript --
 *
 *      Output a PostScript image string of the given photo image.
 *	The photo is first converted into a color image and then
 *	translated into PostScript.
 *
 * Results:
 *      None.
 *
 * Side Effects:
 *      The PostScript output representing the photo is appended to
 *	the printable's dynamic string.
 *
 * -------------------------------------------------------------------------
 */
void
Blt_PhotoToPostScript(printable, photo, x, y)
    Printable printable;
    Tk_PhotoHandle photo;
    register int x, y;		/* Origin of photo image */
{
    ImageRegion region;
    ColorImage image;

    region.x = region.y = region.width = region.height = 0;
    image = Blt_PhotoToColorImage(photo, &region);
    Blt_ColorImageToPostScript(printable, image, x, y);
    Blt_FreeColorImage(image);
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_FontToPostScript --
 *
 *      Map the Tk font to a PostScript font and point size.
 *
 *	If a Tcl array variable was specified, each element should be
 *	indexed by the X11 font name and contain a list of 1-2
 *	elements; the PostScript font name and the desired point size.
 *	The point size may be omitted and the X font point size will
 *	be used.
 *
 *	Otherwise, if the foundry is "Adobe", we try to do a plausible
 *	mapping looking at the full name of the font and building a
 *	string in the form of "Family-TypeFace".
 *
 * Returns:
 *      None.
 *
 * Side Effects:
 *      PostScript commands are output to change the type and the
 *      point size of the current font.
 *
 * -----------------------------------------------------------------
 */

void
Blt_FontToPostScript(printable, font)
    Printable printable;
    Tk_Font font;		/* Tk font to query about */
{
    XFontStruct *fontPtr = (XFontStruct *)font;
    Tcl_Interp *interp = printable->interp;
    char *family, *fontName;
    float pointSize;
    register char **p;

    static char *psFonts[] =
    {
	"Arial",
	"AvantGarde",
	"Bookman",
	"Courier",
	"Geneva",
	"Helvetica",
	"Monaco",
	"New Century Schoolbook",
	"NewCenturySchlbk",
	"New York",
	"Palatino",
	"Symbol",
	"Times",
	"ZapfChancery",
	"ZapfDingBats",
	(char *)NULL,
    };

    fontName = Tk_NameOfFont(font);
    pointSize = 12.0;

    /*
     * Use the font variable information if it exists.
     */
    if (printable->fontVarName != NULL) {
	char *fontInfo;

	fontInfo = Tcl_GetVar2(interp, printable->fontVarName, fontName, 0);
	if (fontInfo != NULL) {
	    int numProps;
	    char **propArr = NULL;

	    if (Tcl_SplitList(interp, fontInfo,
		    &numProps, &propArr) == TCL_OK) {
		int newSize;

		fontName = propArr[0];
		if ((numProps == 2) &&
		    (Tcl_GetInt(interp, propArr[1], &newSize) == TCL_OK)) {
		    pointSize = (float)newSize;
		}
	    }
	    Blt_PrintFormat(printable, "%g /%s SetFont\n", pointSize, fontName);
	    if (propArr != (char **)NULL) {
		free((char *)propArr);
	    }
	    return;
	}
    }
#if (TK_MAJOR_VERSION >= 8)

    /*
     * Otherwise do a quick test to see if it's a PostScript font.
     * Tk_PostScriptFontName will silently generate a bogus PostScript
     * font description, so we have to check to see if this is really a
     * PostScript font.
     */
    family = ((TkFont *) fontPtr)->fa.family;
    for (p = psFonts; *p != NULL; p++) {
	if (strncasecmp(*p, family, strlen(*p)) == 0) {
	    Tcl_DString dString;

	    Tcl_DStringInit(&dString);
	    pointSize = (float)Tk_PostscriptFontName(font, &dString);
	    fontName = Tcl_DStringValue(&dString);
	    Blt_PrintFormat(printable, "%g /%s SetFont\n", pointSize,
		fontName);
	    Tcl_DStringFree(&dString);
	    return;
	}
    }

#endif /* TK_MAJOR_VERSION >= 8 */

    /*
     * Can't find it. Try to use the current point size.
     */
    family = NULL;
    fontName = NULL;
    pointSize = 12.0;

#ifndef  WIN32
#if (TK_MAJOR_VERSION >= 8)
    /* Can you believe what I have to go through to get an XFontStruct? */
    fontPtr = XLoadQueryFont(Tk_Display(printable->tkwin), Tk_NameOfFont(font));
#endif
    if (fontPtr != NULL) {
	unsigned long fontProp;

	if (XGetFontProperty(fontPtr, XA_POINT_SIZE, &fontProp) != False) {
	    pointSize = (float)fontProp / 10.0;
	}
	fontName = XFontStructToPostScript(printable->tkwin, fontPtr);
#if (TK_MAJOR_VERSION >= 8)
	XFreeFont(Tk_Display(printable->tkwin), fontPtr);
#endif
    }
#endif /* !WIN32 */
    if ((fontName == NULL) || (fontName[0] == '\0')) {
	fontName = "Helvetica-Bold";	/* Defaulting to a known PS font */
    }
    Blt_PrintFormat(printable, "%g /%s SetFont\n", pointSize, fontName);
}

static void
PrintCompoundText(printable, x, y, textPtr)
    Printable printable;
    int x, y;
    CompoundText *textPtr;
{
    register char *src, *dest;
    register int count;
    TextSegment *segPtr;
    register int i, j;

    segPtr = textPtr->segArr;
    for (i = 0; i < textPtr->numSegments; i++, segPtr++) {
	if (segPtr->numChars < 1) {
	    continue;
	}
	Tcl_DStringAppend(printable->dStrPtr, "(", -1);
	count = 0;
	dest = printable->scratchArr;
	src = segPtr->text;
	for (j = 0; j < segPtr->numChars; j++) {
	    if (count > BUFSIZ) {
		/* Don't let the buffer overflow with a big text string */
		*dest = '\0', dest = printable->scratchArr;
		Tcl_DStringAppend(printable->dStrPtr, dest, -1);
		count = 0;
	    }
	    /*
	     * If special PostScript characters characters "\", "(",
	     * and ")" are contained in the text string, prepend
	     * backslashes to them.
	     */
	    if ((*src == '\\') || (*src == '(') || (*src == ')')) {
		*dest++ = '\\';
	    }
	    *dest++ = *src++;
	    count++;
	}
	*dest = '\0';
	Tcl_DStringAppend(printable->dStrPtr, printable->scratchArr, -1);
	Blt_PrintFormat(printable, ") %d %d %d DrawAdjText\n",
	    segPtr->width, x + segPtr->x, y + segPtr->y);
    }
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_PrintText --
 *
 *      Output PostScript commands to print a text string. The string
 *      may be rotated at any arbitrary angle, and placed according
 *      the anchor type given. The anchor indicates how to interpret
 *      the window coordinates as an anchor for the text bounding box.
 *
 * Results:
 *      None.
 *
 * Side Effects:
 *      Text string is drawn using the given font and GC on the graph
 *      window at the given coordinates, anchor, and rotation
 *
 * -----------------------------------------------------------------
 */
void
Blt_PrintText(printable, string, attrPtr, x, y)
    Printable printable;
    char *string;		/* String to convert to PostScript */
    TextAttributes *attrPtr;	/* Text attribute information */
    int x, y;			/* Window coordinates where to print text */
{
    double theta;
    int bbWidth, bbHeight;
    CompoundText *textPtr;

    if ((string == NULL) || (*string == '\0')) {	/* Empty string, do nothing */
	return;
    }
    theta = FMOD(attrPtr->theta, (double)360.0);
    textPtr = Blt_GetCompoundText(string, attrPtr);
    Blt_GetBoundingBox(textPtr->width, textPtr->height, theta,
	&bbWidth, &bbHeight, (XPoint *)NULL);
    /*
     * Find the center of the bounding box
     */
    Blt_TranslateAnchor(x, y, bbWidth, bbHeight, attrPtr->anchor, &x, &y);
    x += (bbWidth / 2);
    y += (bbHeight / 2);

    /* Initialize text (sets translation and rotation) */
    Blt_PrintFormat(printable, "%d %d %g %d %d BeginText\n", textPtr->width,
	textPtr->height, attrPtr->theta, x, y);

    Blt_FontToPostScript(printable, attrPtr->font);

    /* All coordinates are now relative to what was set by BeginText */
    if ((attrPtr->shadow.offset > 0) && (attrPtr->shadow.color != NULL)) {
	Blt_ForegroundToPostScript(printable, attrPtr->shadow.color);
	PrintCompoundText(printable, attrPtr->shadow.offset,
	    attrPtr->shadow.offset, textPtr);
    }
    Blt_ForegroundToPostScript(printable, (attrPtr->state & STATE_ACTIVE)
	? attrPtr->activeColor : attrPtr->color);
    PrintCompoundText(printable, 0, 0, textPtr);
    free((char *)textPtr);
    Blt_PrintAppend(printable, "EndText\n", (char *)NULL);
}

/*
 * -----------------------------------------------------------------
 *
 * Blt_PrintLine --
 *
 *      Outputs PostScript commands to print a multi-segmented line.
 *      It assumes a procedure DashesProc was previously defined.
 *
 * Results:
 *      None.
 *
 * Side Effects:
 *      Line is printed.
 *
 * -----------------------------------------------------------------
 */
void
Blt_PrintLine(printable, pointPtr, numPoints)
    Printable printable;
    register XPoint *pointPtr;
    int numPoints;
{
    register int i;

    if (numPoints <= 0) {
	return;
    }
    Blt_PrintFormat(printable, " newpath %d %d moveto\n", pointPtr->x,
	pointPtr->y);
    pointPtr++;
    for (i = 1; i < (numPoints - 1); i++, pointPtr++) {
	Blt_PrintFormat(printable, " %d %d lineto\n", pointPtr->x,
	    pointPtr->y);
	if ((i % PS_MAXPATH) == 0) {
	    Blt_PrintFormat(printable,
		"DashesProc stroke\n newpath  %d %d moveto\n", pointPtr->x,
		pointPtr->y);
	}
    }
    Blt_PrintFormat(printable, " %d %d lineto\n", pointPtr->x,
	pointPtr->y);
    Blt_PrintAppend(printable, "DashesProc stroke\n", (char *)NULL);
}

void
Blt_PrintBitmap(printable, display, bitmap, scaleX, scaleY)
    Printable printable;
    Display *display;
    Pixmap bitmap;		/* Bitmap to be converted to PostScript */
    double scaleX, scaleY;
{
    int width, height;
    float scaledWidth, scaledHeight;

    Tk_SizeOfBitmap(display, bitmap, &width, &height);
    scaledWidth = (float)(width * scaleX);
    scaledHeight = (float)(height * scaleY);
    Blt_PrintAppend(printable, "  gsave\n", (char *)NULL);
    Blt_PrintFormat(printable, "    %g %g translate\n", scaledWidth * -0.5,
	scaledHeight * 0.5);
    Blt_PrintFormat(printable, "    %g %g scale\n", scaledWidth,
	-scaledHeight);
    Blt_PrintFormat(printable, "    %d %d true [%d 0 0 %d 0 %d] {", width,
	height, width, -height, height);
    Blt_BitmapToPostScript(printable, display, bitmap, width, height);
    Blt_PrintAppend(printable, "    } imagemask\n  grestore\n",
	(char *)NULL);
}
