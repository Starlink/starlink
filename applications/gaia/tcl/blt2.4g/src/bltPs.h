/*
 * bltPs.h --
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

#ifndef _BLT_PS_H
#define _BLT_PS_H

#include "bltImage.h"

typedef enum {
    PS_MODE_MONOCHROME, 
    PS_MODE_GREYSCALE, 
    PS_MODE_COLOR
} PsColorMode;

typedef struct Printable {
    Tcl_Interp *interp;		/* Interpreter to report errors to. */
    Tk_Window tkwin;		/* Tk_Window used to get font and color 
				 * information */
    Tcl_DString *dStrPtr;	/* Dynamic string used to contain the 
				 * PostScript generated. */
    char *fontVarName;		/* Name of a Tcl array variable to convert
				 * X font names to PostScript fonts. */

    char *colorVarName;		/* Name of a Tcl array variable to convert
				 * X color names to PostScript. */

    PsColorMode colorMode;	/* Mode: color or greyscale */

#define PRINTABLE_SCRATCH_LENGTH  (BUFSIZ*2)
    /* 
     * Utility space for building strings.  Currently used to create
     * PostScript output for the "postscript" command.  
     */
    char scratchArr[PRINTABLE_SCRATCH_LENGTH];
} *Printable;

extern Printable Blt_PrintObject _ANSI_ARGS_((Tcl_Interp *interp, Tk_Window tkwin,
	Tcl_DString *dStrPtr));

extern void Blt_PrintAppend _ANSI_ARGS_(TCL_VARARGS(Printable, printable));

extern void Blt_PrintFormat _ANSI_ARGS_(TCL_VARARGS(Printable, printable));

extern void Blt_3DRectangleToPostScript _ANSI_ARGS_((Printable printable,
	Tk_3DBorder border, int x, int y, int width, int height,
	int borderWidth, int relief));

extern void Blt_3DFillRectangle _ANSI_ARGS_((Printable printable, 
	Tk_3DBorder border, int x, int y, int width, int height, 
	int borderWidth, int relief));

extern void Blt_BackgroundToPostScript _ANSI_ARGS_((Printable printable,
	XColor *colorPtr));

extern void Blt_BitmapToPostScript _ANSI_ARGS_((Printable printable, 
	Display *display, Pixmap bitmap, int width, int height));

extern void Blt_ClearBackgroundToPostScript _ANSI_ARGS_((Printable printable));

extern void Blt_ColorImageToPsData _ANSI_ARGS_((ColorImage image, int numComponents,
	Tcl_DString *dStrPtr, char *prefix));

extern int Blt_ColorImageToGreyscale _ANSI_ARGS_((ColorImage srcImage, 
	ColorImage destImage));

extern void Blt_ColorImageToPostScript _ANSI_ARGS_((Printable printable, 
	ColorImage image, int x, int y));

extern void Blt_ForegroundToPostScript _ANSI_ARGS_((Printable printable,
	XColor *colorPtr));

extern void Blt_FontToPostScript _ANSI_ARGS_((Printable printable, Tk_Font font));

extern void Blt_WindowToPostScript _ANSI_ARGS_((Printable printable, 
	Tk_Window tkwin, int x, int y));

extern void Blt_LineDashesToPostScript _ANSI_ARGS_((Printable printable, 
	Dashes *dashesPtr));

extern void Blt_LineWidthToPostScript _ANSI_ARGS_((Printable printable, 
	int lineWidth));

extern void Blt_LinesToPostScript _ANSI_ARGS_((Printable printable,
	XPoint *pointArr, int numPoints));

extern void Blt_PhotoToPostScript _ANSI_ARGS_((Printable printable,
	Tk_PhotoHandle photoToken, int x, int y));

extern void Blt_PolygonToPostScript _ANSI_ARGS_((Printable printable, 
	XPoint *pointArr, int numPoints));

extern void Blt_PrintLine _ANSI_ARGS_((Printable printable, XPoint *pointArr, 
	int numPoints));

extern void Blt_PrintText _ANSI_ARGS_((Printable printable, char *string, 
	TextAttributes *attrPtr, int x, int y));

extern void Blt_RectangleToPostScript _ANSI_ARGS_((Printable printable, int x, 
	int y, int width, int height));

extern void Blt_RectanglesToPostScript _ANSI_ARGS_((Printable printable, 
	XRectangle *rectArr, int numRects));

extern void Blt_PrintBitmap _ANSI_ARGS_((Printable printable, Display *display,
	Pixmap bitmap, double scaleX, double scaleY));

extern void Blt_SegmentsToPostScript _ANSI_ARGS_((Printable printable,
	XSegment *segArr, int numSegs));

extern void Blt_StippleToPostScript _ANSI_ARGS_((Printable printable, 
	Display *display, Pixmap bitmap, int width, int height, int fgOrBg));

extern void Blt_LineAttributesToPostScript _ANSI_ARGS_((Printable printable, 
	XColor *colorPtr, int lineWidth, Dashes *dashesPtr, int capStyle,
	int joinStyle));

extern int Blt_FileToPostScript _ANSI_ARGS_((Printable printable, 
	char *fileName));

#endif /* BLT_PS_H */
