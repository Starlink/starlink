
/*
 * bltGrPs.c --
 *
 *      This module implements the "postscript" operation for BLT graph widget.
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

/*
 * -----------------------------------------------------------------
 *
 * PostScript routines to print a graph
 *
 * -----------------------------------------------------------------
 */
#include "bltGraph.h"
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#if defined(__STDC__)
#include <stdarg.h>
#else
#include <varargs.h>
#endif

static int StringToColorMode _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *string, char *widgRec,
	int offset));
static char *ColorModeToString _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption colorModeOption =
{
    StringToColorMode, ColorModeToString, (ClientData)0,
};

extern Tk_CustomOption bltLengthOption;
extern Tk_CustomOption bltPadOption;

#define DEF_PS_CENTER		"yes"
#define DEF_PS_COLOR_MAP	(char *)NULL
#define DEF_PS_COLOR_MODE	"color"
#define DEF_PS_DECORATIONS	"yes"
#define DEF_PS_FONT_MAP		(char *)NULL
#define DEF_PS_HEIGHT		"0"
#define DEF_PS_LANDSCAPE	"no"
#define DEF_PS_MAXPECT		"no"
#define DEF_PS_PADX		"1.0i"
#define DEF_PS_PADY		"1.0i"
#define DEF_PS_PAPERHEIGHT	"11.0i"
#define DEF_PS_PAPERWIDTH	"8.5i"
#define DEF_PS_PREVIEW		"no"
#define DEF_PS_WIDTH		"0"

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_BOOLEAN, "-center", "psCenter", "PsCenter",
	DEF_PS_CENTER, Tk_Offset(PostScript, center),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_STRING, "-colormap", "psColorMap", "PsColorMap",
	DEF_PS_COLOR_MAP, Tk_Offset(PostScript, colorVarName),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-colormode", "psColorMode", "PsColorMode",
	DEF_PS_COLOR_MODE, Tk_Offset(PostScript, colorMode),
	TK_CONFIG_DONT_SET_DEFAULT, &colorModeOption},
    {TK_CONFIG_BOOLEAN, "-decorations", "psDecorations", "PsDecorations",
	DEF_PS_DECORATIONS, Tk_Offset(PostScript, decorations),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_STRING, "-fontmap", "psFontMap", "PsFontMap",
	DEF_PS_FONT_MAP, Tk_Offset(PostScript, fontVarName),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-height", "psHeight", "PsHeight",
	DEF_PS_HEIGHT, Tk_Offset(PostScript, reqHeight),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_BOOLEAN, "-landscape", "psLandscape", "PsLandscape",
	DEF_PS_LANDSCAPE, Tk_Offset(PostScript, landscape),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BOOLEAN, "-maxpect", "psMaxpect", "PsMaxpect",
	DEF_PS_MAXPECT, Tk_Offset(PostScript, maxpect),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-padx", "psPadX", "PsPadX",
	DEF_PS_PADX, Tk_Offset(PostScript, padX), 0, &bltPadOption},
    {TK_CONFIG_CUSTOM, "-pady", "psPadY", "PsPadY",
	DEF_PS_PADY, Tk_Offset(PostScript, padY), 0, &bltPadOption},
    {TK_CONFIG_CUSTOM, "-paperheight", "psPaperHeight", "PsPaperHeight",
	DEF_PS_PAPERHEIGHT, Tk_Offset(PostScript, reqPaperHeight),
	0, &bltLengthOption},
    {TK_CONFIG_CUSTOM, "-paperwidth", "psPaperWidth", "PsPaperWidth",
	DEF_PS_PAPERWIDTH, Tk_Offset(PostScript, reqPaperWidth),
	0, &bltLengthOption},
    {TK_CONFIG_BOOLEAN, "-preview", "psPreview", "PsPreview",
	DEF_PS_PREVIEW, Tk_Offset(PostScript, addPreview),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-width", "psWidth", "PsWidth",
	DEF_PS_WIDTH, Tk_Offset(PostScript, reqWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0}
};

extern void Blt_PrintMarkers _ANSI_ARGS_((Graph *graphPtr, Printable printable,
	int under));
extern void Blt_PrintElements _ANSI_ARGS_((Graph *graphPtr, Printable printable));
extern void Blt_PrintActiveElements _ANSI_ARGS_((Graph *graphPtr,
	Printable printable));
extern void Blt_PrintLegend _ANSI_ARGS_((Graph *graphPtr, Printable printable));
extern void Blt_PrintGrid _ANSI_ARGS_((Graph *graphPtr, Printable printable));
extern void Blt_PrintAxes _ANSI_ARGS_((Graph *graphPtr, Printable printable));
extern void Blt_PrintAxisLimits _ANSI_ARGS_((Graph *graphPtr,
	Printable printable));

/*
 *----------------------------------------------------------------------
 *
 * StringToColorMode --
 *
 *	Convert the string representation of a PostScript color mode
 *	into the enumerated type representing the color level:
 *
 *	    PS_MODE_COLOR 	- Full color
 *	    PS_MODE_GREYSCALE  	- Color converted to greyscale
 *	    PS_MODE_MONOCHROME 	- Only black and white
 *
 * Results:
 *	A standard Tcl result.  The color level is written into the
 *	page layout information structure.
 *
 * Side Effects:
 *	Future invocations of the "postscript" option will use this
 *	variable to determine how color information will be displayed
 *	in the PostScript output it produces.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
StringToColorMode(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* not used */
    char *string;		/* New legend position string */
    char *widgRec;		/* Graph widget record */
    int offset;			/* Offset of colorMode field in record */
{
    PsColorMode *modePtr = (PsColorMode *) (widgRec + offset);
    unsigned int length;
    char c;

    c = string[0];
    length = strlen(string);
    if ((c == 'c') && (strncmp(string, "color", length) == 0)) {
	*modePtr = PS_MODE_COLOR;
    } else if ((c == 'g') && (strncmp(string, "grayscale", length) == 0)) {
	*modePtr = PS_MODE_GREYSCALE;
    } else if ((c == 'g') && (strncmp(string, "greyscale", length) == 0)) {
	*modePtr = PS_MODE_GREYSCALE;
    } else if ((c == 'm') && (strncmp(string, "monochrome", length) == 0)) {
	*modePtr = PS_MODE_MONOCHROME;
    } else {
	Tcl_AppendResult(interp, "bad color mode \"", string, "\": should be \
\"color\", \"greyscale\", or \"monochrome\"", (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * NameOfColorMode --
 *
 *	Convert the PostScript mode value into the string representing
 *	a valid color mode.
 *
 * Results:
 *	The static string representing the color mode is returned.
 *
 *----------------------------------------------------------------------
 */
static char *
NameOfColorMode(colorMode)
    PsColorMode colorMode;
{
    switch (colorMode) {
    case PS_MODE_COLOR:
	return "color";
    case PS_MODE_GREYSCALE:
	return "greyscale";
    case PS_MODE_MONOCHROME:
	return "monochrome";
    default:
	return "unknown color mode";
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ColorModeToString --
 *
 *	Convert the current color mode into the string representing a
 *	valid color mode.
 *
 * Results:
 *	The string representing the color mode is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
ColorModeToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* PostScript structure record */
    int offset;			/* field of colorMode in record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    PsColorMode mode = *(PsColorMode *) (widgRec + offset);

    return (NameOfColorMode(mode));
}

void
Blt_DestroyPostScript(graphPtr)
    Graph *graphPtr;
{
    Tk_FreeOptions(configSpecs, (char *)graphPtr->postscript,
	graphPtr->display, 0);
    free((char *)graphPtr->postscript);
}

/*
 *----------------------------------------------------------------------
 *
 * CgetOp --
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
CgetOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;
    char *argv[];
{
    PostScript *psPtr = (PostScript *)graphPtr->postscript;

    if (Tk_ConfigureValue(interp, graphPtr->tkwin, configSpecs, (char *)psPtr,
	    argv[3], 0) != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 * ----------------------------------------------------------------------
 *
 * ConfigureOp --
 *
 *      This procedure is invoked to print the graph in a file.
 *
 * Results:
 *      A standard TCL result.
 *
 * Side effects:
 *      A new PostScript file is created.
 *
 * ----------------------------------------------------------------------
 */
static int
ConfigureOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;
    Tcl_Interp *interp;
    int argc;			/* Number of options in argv vector */
    char **argv;		/* Option vector */
{
    int flags = TK_CONFIG_ARGV_ONLY;
    PostScript *psPtr = (PostScript *)graphPtr->postscript;

    if (argc == 3) {
	return (Tk_ConfigureInfo(interp, graphPtr->tkwin, configSpecs,
		(char *)psPtr, (char *)NULL, flags));
    } else if (argc == 4) {
	return (Tk_ConfigureInfo(interp, graphPtr->tkwin, configSpecs,
		(char *)psPtr, argv[3], flags));
    }
    if (Tk_ConfigureWidget(interp, graphPtr->tkwin, configSpecs, argc - 3, argv + 3,
	    (char *)psPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 * --------------------------------------------------------------------------
 *
 * ComputeBoundingBox --
 *
 * 	Calculate the bounding box required for the plot and its
 * 	padding.  First get the size of the plot (by default, it's the
 * 	size of graph's X window).  If the plot is bigger than the
 * 	designated paper size, or if the "-maxpect" option is turned
 * 	on, make the bounding box same size as the page.  The bounding
 * 	box will still maintain its padding, therefore the plot area
 * 	will grow or shrink.
 *
 * 	Since the values set rely on the width and height of the
 *	graph, this must be called each time PostScript is generated.
 *
 * Results: None.
 *
 * Side Effects:
 *	graph->width and graph->height are set to the postscript plot
 *	Extents.
 *
 * --------------------------------------------------------------------------
 */
static void
ComputeBoundingBox(graphPtr, psPtr)
    Graph *graphPtr;
    PostScript *psPtr;
{
    int paperWidth, paperHeight, bboxWidth, bboxHeight;
    int plotX, plotY, plotWidth, plotHeight;
    int xBorder, yBorder;

    plotWidth = (psPtr->reqWidth > 0) ? psPtr->reqWidth : graphPtr->width;
    plotHeight = (psPtr->reqHeight > 0) ? psPtr->reqHeight : graphPtr->height;
    plotX = psPtr->padLeft;
    plotY = psPtr->padTop;
    xBorder = PADDING(psPtr->padX);
    yBorder = PADDING(psPtr->padY);

    if (psPtr->landscape) {
	int temp;
	/*
	 * The bounding box is always aligned with the page, so correct for
	 * orientation.
	 */
	temp = plotWidth, plotWidth = plotHeight, plotHeight = temp;
    }
    /*
     * Calculate the size of the page.  The paper size is needed
     * for both the "-maxpect" and "-center" options.
     *
     * If no requested size was made (i.e. request is zero), make the
     * paper size the size of the plot plus the padding.
     */
    paperWidth = plotWidth + xBorder;
    paperHeight = plotHeight + yBorder;

    if (psPtr->reqPaperWidth > 0) {
	paperWidth = psPtr->reqPaperWidth;
    }
    if (psPtr->reqPaperHeight > 0) {
	paperHeight = psPtr->reqPaperHeight;
    }
    psPtr->pageScale = 1.0;

    if (psPtr->maxpect) {
	float xScale, yScale, scale;

	/* Scale the plot to fit the page.  Graph dimensions don't change. */

	xScale = (float)(paperWidth - xBorder) / plotWidth;
	yScale = (float)(paperHeight - yBorder) / plotHeight;
	scale = MIN(xScale, yScale);
	bboxWidth = (int)((plotWidth * scale) + 0.5);
	bboxHeight = (int)((plotHeight * scale) + 0.5);
	psPtr->pageScale = scale;
    } else {
	/*
	 * Reset the size of the bounding box if it's bigger than the page.
	 */
	if ((plotWidth + xBorder) > paperWidth) {
	    plotWidth = paperWidth - xBorder;
	}
	if ((plotHeight + yBorder) > paperHeight) {
	    plotHeight = paperHeight - yBorder;
	}
	bboxWidth = plotWidth;
	bboxHeight = plotHeight;
    }
    if (psPtr->center) {
	if (paperWidth > bboxWidth) {
	    plotX = (paperWidth - bboxWidth) / 2;
	}
	if (paperHeight > bboxHeight) {
	    plotY = (paperHeight - bboxHeight) / 2;
	}
    }
    if (psPtr->landscape) {
	graphPtr->height = plotWidth;
	graphPtr->width = plotHeight;
    } else {
	graphPtr->width = plotWidth;
	graphPtr->height = plotHeight;
    }
    psPtr->bbox[0] = plotX;
    psPtr->bbox[1] = plotY;
    psPtr->bbox[2] = plotX + bboxWidth;
    psPtr->bbox[3] = plotY + bboxHeight;
    psPtr->pageHeight = paperHeight;
    graphPtr->flags |= LAYOUT_NEEDED | COORDS_WORLD;
    Blt_LayoutGraph(graphPtr);
}

/*
 * --------------------------------------------------------------------------
 *
 * PreviewImage --
 *
 * 	Generates a EPSI thumbnail of the graph.  The thumbnail is
 *	restricted to a certain size.  This is to keep the size of the
 *	PostScript file small and the processing time low.
 *
 *	The graph is drawn into a pixmap.  We then take a snapshot
 *	of that pixmap, and rescale it to a smaller image.  Finally,
 * 	the image is dumped to PostScript.
 *
 * Results:
 *	None.
 *
 * --------------------------------------------------------------------------
 */
static void
PreviewImage(graphPtr, printable)
    Graph *graphPtr;
    Printable printable;
{
    int noBackingStore = 0;
    Pixmap drawable;
    ColorImage image;
    int numLines, numPixels;
    ImageRegion region;

    /* Create a pixmap and draw the graph into it. */

    drawable = Tk_GetPixmap(graphPtr->display, Tk_WindowId(graphPtr->tkwin),
	graphPtr->width, graphPtr->height, Tk_Depth(graphPtr->tkwin));
    Blt_DrawGraph(graphPtr, drawable, noBackingStore);

    /* Get a color image from the pixmap */

    region.x = region.y = region.width = region.height = 0;
    image = Blt_DrawableToColorImage(graphPtr->tkwin, drawable, graphPtr->width,
	graphPtr->height, &region);
    Tk_FreePixmap(graphPtr->display, drawable);
    if (image == NULL) {
	return;			/* Can't grab pixmap? */
    }
#ifdef notdef
    {
	float scale, xScale, yScale;
	ColorImage destImage;
	ImageRegion destRegion;

	/* Scale the source image into a size appropriate for a thumbnail. */
#define PS_MAX_PREVIEW_WIDTH	300.0
#define PS_MAX_PREVIEW_HEIGHT	300.0
	xScale = PS_MAX_PREVIEW_WIDTH / (float)graphPtr->width;
	yScale = PS_MAX_PREVIEW_HEIGHT / (float)graphPtr->height;
	scale = MIN(xScale, yScale);

	destRegion.width = (int)(scale * region.width + 0.5);
	destRegion.height = (int)(scale * region.height + 0.5);
	destRegion.x = destRegion.y = 0;

	destImage = Blt_ResampleColorImage(image, &region, &destRegion,
	    bltBoxFilter, bltBoxFilter);
	Blt_FreeColorImage(image);
	image = destImage;
    }
#endif
    Blt_ColorImageToGreyscale(image, image);

    numPixels = ColorImageWidth(image) * ColorImageHeight(image);
    numLines = (numPixels + 29) / 30;

    /* Finally, we can generate PostScript for the image */

    Tcl_DStringAppend(printable->dStrPtr, "%%BeginPreview: ", -1);
    Blt_PrintFormat(printable, "%d %d 8 %d\n", ColorImageWidth(image),
	ColorImageHeight(image), numLines);
    Blt_ColorImageToPsData(image, 1, printable->dStrPtr, "%");
    Blt_FreeColorImage(image);
    Tcl_DStringAppend(printable->dStrPtr, "%%EndPreview\n\n", -1);
}

/*
 * --------------------------------------------------------------------------
 *
 * PrintPreamble
 *
 *    	The PostScript preamble calculates the needed translation and scaling
 *    	to make X11 coordinates compatible with PostScript.
 *
 * ---------------------------------------------------------------------
 */

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif /* HAVE_SYS_TIME_H */
#endif /* TIME_WITH_SYS_TIME */

static int
PostScriptPreamble(graphPtr, fileName, printable)
    Graph *graphPtr;
    char *fileName;
    Printable printable;
{
    PostScript *psPtr = (PostScript *)graphPtr->postscript;
    long date;
    char *version;
    float dpiX, dpiY;
    float yScale, xScale;	/* Scales to convert pixels to pica */
    Screen *screenPtr;
    char dateStr[80];		/* Hold the date string from ctime() */
    char *who;
    char *lastPtr;

    ComputeBoundingBox(graphPtr, psPtr);
    if (fileName == NULL) {
	fileName = Tk_PathName(graphPtr->tkwin);
    }
    Blt_PrintAppend(printable, "%!PS-Adobe-3.0 EPSF-3.0\n", (char *)NULL);

    /*
     * Compute the scale factors to convert PostScript to X11 coordinates.
     * Round the pixels per inch (dpi) to an integral value before computing
     * the scale.
     */
#define MM_INCH (float)25.4
#define PICA_INCH (float)72.0

    screenPtr = Tk_Screen(graphPtr->tkwin);
    dpiX = (WidthOfScreen(screenPtr) * MM_INCH) / WidthMMOfScreen(screenPtr);
    dpiY = (HeightOfScreen(screenPtr) * MM_INCH) / HeightMMOfScreen(screenPtr);
    xScale = PICA_INCH / ROUND(dpiX);
    yScale = PICA_INCH / ROUND(dpiY);

    /*
     * Compute the lower left and upper right coordinates of the
     * bounding box.  The bounding box contains the graph without any
     * designated padding (-padx or -pady options).
     */
    Blt_PrintFormat(printable, "%%%%BoundingBox: %d %d %d %d\n",
	(int)(psPtr->bbox[0] * xScale + 0.5),
	(int)(psPtr->bbox[1] * yScale + 0.5),
	(int)(psPtr->bbox[2] * xScale + 0.5),
	(int)(psPtr->bbox[3] * yScale + 0.5));

    Blt_PrintAppend(printable, "%%Pages: 0\n", (char *)NULL);

    version = Tcl_GetVar(graphPtr->interp, "blt_version", TCL_GLOBAL_ONLY);
    if (version == NULL) {
	version = "???";
    }
    Blt_PrintFormat(printable, "%%%%Creator: (BLT %s %s)\n", version,
	Tk_Class(graphPtr->tkwin));

    date = time((time_t *) NULL);
    strcpy(dateStr, ctime(&date));
    lastPtr = dateStr + (strlen(dateStr) - 1);
    if (*lastPtr == '\n') {
	*lastPtr = '\0';
    }
    Blt_PrintFormat(printable, "%%%%CreationDate: (%s)\n", dateStr);
    Blt_PrintFormat(printable, "%%%%Title: (%s)\n", fileName);

    Blt_PrintAppend(printable,
	"%%DocumentNeededResources: font Helvetica Courier\n", (char *)NULL);
    Blt_PrintAppend(printable, "%%EndComments\n", (char *)NULL);
    if (psPtr->addPreview) {
	PreviewImage(graphPtr, printable);
    }
    if (Blt_FileToPostScript(printable, "bltGraph.pro") != TCL_OK) {
	return TCL_ERROR;
    }
    who = getenv("LOGNAME");
    if (who == NULL) {
	who = "???";
    }
    Blt_PrintAppend(printable,
	"8 /Helvetica SetFont\n",
	"10 30 moveto\n",
	"(Date: ", dateStr, ") show\n",
	"10 20 moveto\n",
	"(File: ", fileName, ") show\n",
	"10 10 moveto\n",
	"(Created by: ", who, "@", Tcl_GetHostName(), ") show\n",
	"0 0 moveto\n",
	(char *)NULL);
    /*
     * Set the conversion from PostScript to X11 coordinates.  Scale
     * pica to pixels and flip the y-axis (the origin is the upperleft
     * corner).
     */
    Blt_PrintAppend(printable,
	"% Transform coordinate system to use X11 coordinates\n\n",
	"% Flip the y-axis by changing the origin and reversing the scale,\n",
	"% making the origin the upper left corner\n", (char *)NULL);
    Blt_PrintFormat(printable, "%f -%f scale\n", xScale, yScale);
    Blt_PrintFormat(printable, "0 %d translate\n\n", -psPtr->pageHeight);
    Blt_PrintAppend(printable, "% User defined page layout\n\n",
	"%% Set color level\n", (char *)NULL);
    Blt_PrintFormat(printable, "/CL %d def\n\n", psPtr->colorMode);
    Blt_PrintFormat(printable, "%% Set origin\n%d %d translate\n\n",
	psPtr->bbox[0], psPtr->bbox[1]);
    if (psPtr->landscape) {
	Blt_PrintFormat(printable,
	    "%% Landscape orientation\n0 %g translate\n-90 rotate\n",
	    ((float)graphPtr->width * psPtr->pageScale));
    }
    if (psPtr->maxpect) {
	Blt_PrintAppend(printable, "\n%% Set max aspect ratio\n", (char *)NULL);
	Blt_PrintFormat(printable, " %g %g scale\n", psPtr->pageScale,
	    psPtr->pageScale);
    }
    Blt_PrintAppend(printable, "\n%%EndSetup\n\n", (char *)NULL);
    return TCL_OK;
}


static void
PrintMargins(graphPtr, printable)
    Graph *graphPtr;
    Printable printable;
{
    PostScript *psPtr = (PostScript *)graphPtr->postscript;
    XRectangle margin[4];

    margin[0].x = margin[0].y = margin[3].x = margin[1].x = 0;
    margin[0].width = margin[3].width = graphPtr->width;
    margin[0].height = graphPtr->topMargin + 1;
    margin[3].y = (int)graphPtr->yMax + 1;
    margin[3].height = graphPtr->bottomMargin;
    margin[2].y = margin[1].y = graphPtr->yMin;
    margin[1].width = graphPtr->leftMargin;
    margin[2].height = margin[1].height = graphPtr->yMax - graphPtr->yMin + 2;
    margin[2].x = graphPtr->xMax + 1;
    margin[2].width = graphPtr->rightMargin;

    /* Clear the surrounding margins and clip the plotting surface */
    if (psPtr->decorations) {
	Blt_BackgroundToPostScript(printable,
	    Tk_3DBorderColor(graphPtr->border));
    } else {
	Blt_ClearBackgroundToPostScript(printable);
    }
    Blt_RectanglesToPostScript(printable, margin, 4);

    /* Interior 3D border */
    if ((psPtr->decorations) && (graphPtr->plotBW > 0)) {
	int x, y, width, height;

	x = graphPtr->xMin - graphPtr->plotBW;
	y = graphPtr->yMin - graphPtr->plotBW;
	width = (graphPtr->xMax - graphPtr->xMin) + (2 * graphPtr->plotBW);
	height = (graphPtr->yMax - graphPtr->yMin) + (2 * graphPtr->plotBW);
	Blt_3DRectangleToPostScript(printable, graphPtr->border, x, y,
	    width, height, graphPtr->plotBW, graphPtr->plotRelief);
    }
    if (GetLegendSite(graphPtr) < LEGEND_SITE_PLOT) {
	/*
	 * Print the legend if we're using a site which lies in one
	 * of the margins (left, right, top, or bottom) of the graph.
	 */
	Blt_PrintLegend(graphPtr, printable);
    }
    if (graphPtr->titleText != NULL) {
	Blt_PrintText(printable, graphPtr->titleText,
	    &(graphPtr->titleAttr), graphPtr->titleX, graphPtr->titleY);
    }
    Blt_PrintAxes(graphPtr, printable);
}


static int
GraphToPostScript(graphPtr, ident, printable)
    Graph *graphPtr;
    char *ident;		/* Identifier string (usually the filename) */
    Printable printable;
{
    Legend *legendPtr = graphPtr->legendPtr;
    int x, y, width, height;
    int result = TCL_ERROR;
    LegendSite site;

    Tcl_DStringInit(printable->dStrPtr);
    result = PostScriptPreamble(graphPtr, ident, printable);
    if (result != TCL_OK) {
	goto error;
    }
    /*
     * Determine rectangle of the plotting area for the graph window
     */
    x = graphPtr->xMin - graphPtr->plotBW;
    y = graphPtr->yMin - graphPtr->plotBW;

    width = (graphPtr->xMax - graphPtr->xMin + 1) + (2 * graphPtr->plotBW);
    height = (graphPtr->yMax - graphPtr->yMin + 1) + (2 * graphPtr->plotBW);

    Blt_FontToPostScript(printable, graphPtr->titleAttr.font);
    if (graphPtr->postscript->decorations) {
	Blt_BackgroundToPostScript(printable, graphPtr->plotBg);
    } else {
	Blt_ClearBackgroundToPostScript(printable);
    }
    Blt_RectangleToPostScript(printable, x, y, width, height);
    Blt_PrintAppend(printable, "gsave clip\n\n", (char *)NULL);
    /* Draw the grid, elements, and markers in the plotting area. */
    site = GetLegendSite(graphPtr);

    if (!graphPtr->gridPtr->hidden) {
	Blt_PrintGrid(graphPtr, printable);
    }
    Blt_PrintMarkers(graphPtr, printable, TRUE);
    if ((site >= LEGEND_SITE_PLOT) && (!legendPtr->raised)) {
	/* Print legend underneath elements and markers */
	Blt_PrintLegend(graphPtr, printable);
    }
    Blt_PrintAxisLimits(graphPtr, printable);
    Blt_PrintElements(graphPtr, printable);
    if ((site >= LEGEND_SITE_PLOT) && (legendPtr->raised)) {
	/* Print legend above elements (but not markers) */
	Blt_PrintLegend(graphPtr, printable);
    }
    Blt_PrintMarkers(graphPtr, printable, FALSE);
    Blt_PrintActiveElements(graphPtr, printable);
    Blt_PrintAppend(printable, "\n",
	"% Unset clipping\n",
	"grestore\n\n", (char *)NULL);
    PrintMargins(graphPtr, printable);
    Blt_PrintAppend(printable,
	"showpage\n",
	"%Trailer\n",
	"grestore\n",
	"end\n",
	"%EOF\n", (char *)NULL);
  error:
    /* Reset height and width of graph window */
    graphPtr->width = Tk_Width(graphPtr->tkwin);
    graphPtr->height = Tk_Height(graphPtr->tkwin);
    graphPtr->flags = COORDS_WORLD;

    /*
     * Redraw the graph in order to re-calculate the layout as soon as
     * possible. This is in the case the crosshairs are active.
     */
    Blt_EventuallyRedrawGraph(graphPtr);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * OutputOp --
 *
 *      This procedure is invoked to print the graph in a file.
 *
 * Results:
 *      Standard TCL result.  TCL_OK if plot was successfully printed,
 *	TCL_ERROR otherwise.
 *
 * Side effects:
 *      A new PostScript file is created.
 *
 *----------------------------------------------------------------------
 */
static int
OutputOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;		/* Graph widget record */
    Tcl_Interp *interp;
    int argc;			/* Number of options in argv vector */
    char **argv;		/* Option vector */
{
    PostScript *psPtr = (PostScript *)graphPtr->postscript;
    int result = TCL_ERROR;
    Tcl_DString dString;
    FILE *f = NULL;
    Printable printable;
    char *fileName;		/* Name of file to write PostScript output
                                 * If NULL, output is returned via
                                 * interp->result. */
    fileName = NULL;
    if (argc > 3) {
	if (argv[3][0] != '-') {
	    fileName = argv[3];	/* First argument is the file name. */
	    argv++, argc--;
	}
	if (Tk_ConfigureWidget(interp, graphPtr->tkwin, configSpecs, argc - 3,
		argv + 3, (char *)psPtr, TK_CONFIG_ARGV_ONLY) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (fileName != NULL) {
	    f = fopen(fileName, "w");
	    if (f == NULL) {
		Tcl_AppendResult(interp, "can't create \"", fileName, "\": ",
		    Tcl_PosixError(interp), (char *)NULL);
		return TCL_ERROR;
	    }
	}
    }
    Tcl_DStringInit(&dString);
    printable = Blt_PrintObject(graphPtr->interp, graphPtr->tkwin, &dString);
    printable->fontVarName = psPtr->fontVarName;
    printable->colorVarName = psPtr->colorVarName;
    printable->colorMode = psPtr->colorMode;

    if (GraphToPostScript(graphPtr, fileName, printable) != TCL_OK) {
	goto error;
    }
    /*
     * If a file name was given, write the results to that file
     */
    if (f != NULL) {
	fputs(Tcl_DStringValue(&dString), f);
	if (ferror(f)) {
	    Tcl_AppendResult(interp, "error writing file \"", fileName, "\": ",
		Tcl_PosixError(interp), (char *)NULL);
	    goto error;
	}
    } else {
	Tcl_DStringResult(interp, &dString);
    }
    result = TCL_OK;

  error:
    if (f != NULL) {
	fclose(f);
    }
    Tcl_DStringFree(&dString);
    free((char *)printable);

    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_CreatePostScript --
 *
 *      Creates a postscript structure.
 *
 * Results:
 *      Always TCL_OK.
 *
 * Side effects:
 *      A new PostScript structure is created.
 *
 *----------------------------------------------------------------------
 */
int
Blt_CreatePostScript(graphPtr)
    Graph *graphPtr;
{
    PostScript *psPtr;

    psPtr = (PostScript *)calloc(1, sizeof(PostScript));
    assert(psPtr);
    psPtr->colorMode = PS_MODE_COLOR;
    psPtr->center = TRUE;
    psPtr->decorations = TRUE;
    graphPtr->postscript = psPtr;

    if (Blt_ConfigureWidgetComponent(graphPtr->interp, graphPtr->tkwin,
	    "Postscript", "postscript", configSpecs, 0, (char **)NULL,
	    (char *)psPtr, 0) != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Blt_PostScriptOp --
 *
 *	This procedure is invoked to process the Tcl command
 *	that corresponds to a widget managed by this module.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */
static Blt_OpSpec psOps[] =
{
    {"cget", 2, (Blt_Operation)CgetOp, 4, 4, "option",},
    {"configure", 2, (Blt_Operation)ConfigureOp, 3, 0, "?option value?...",},
    {"output", 1, (Blt_Operation)OutputOp, 3, 0,
	"?fileName? ?option value?...",},
};

static int numPsOps = sizeof(psOps) / sizeof(Blt_OpSpec);

int
Blt_PostScriptOp(graphPtr, interp, argc, argv)
    Graph *graphPtr;		/* Graph widget record */
    Tcl_Interp *interp;
    int argc;			/* # arguments */
    char **argv;		/* Argument list */
{
    Blt_Operation proc;
    int result;

    proc = Blt_GetOperation(interp, numPsOps, psOps, BLT_OPER_ARG2,
	argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    result = (*proc) (graphPtr, interp, argc, argv);
    return (result);
}
