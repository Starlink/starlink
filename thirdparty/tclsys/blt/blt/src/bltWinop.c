/*
 * bltWinop.c --
 *
 *	This module implements simple window commands for the BLT toolkit.
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

#ifndef NO_WINOP

#include "bltImage.h"
#include <X11/Xutil.h>
#ifndef WIN32
#include <X11/Xproto.h>
#endif

#ifdef __STDC__
static Tcl_CmdProc WindowCmd;
#endif

static Tk_Window
NameToWindow(interp, pathName, mainWindow)
    Tcl_Interp *interp;
    char *pathName;
    Tk_Window mainWindow;
{
    Tk_Window tkwin;

    tkwin = Tk_NameToWindow(interp, pathName, mainWindow);
    if ((tkwin != NULL) && (Tk_WindowId(tkwin) == None)) {
	Tk_MakeWindowExist(tkwin);
    }
    return tkwin;
}

#ifndef WIN32
/*ARGSUSED*/
static int
ColormapOp(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;			/* Not used. */
    char **argv;
{
    register int i;
    Tk_Window tkwin;
#define MAXCOLORS 256
    register XColor *colorPtr;
    XColor colorArr[MAXCOLORS];
    unsigned long int pixelValues[MAXCOLORS];
    int inUse[MAXCOLORS];
    char string[20];
    unsigned long int *indexPtr;
    int numFree;

    tkwin = NameToWindow(interp, argv[2], (Tk_Window)clientData);
    if (tkwin == NULL) {
	return TCL_ERROR;
    }
    /* Initially, we assume all color cells are allocated. */
    memset((char *)inUse, 0, sizeof(int) * MAXCOLORS);

    /*
     * Start allocating color cells.  This will tell us which color cells
     * haven't already been allocated in the colormap.  We'll release the
     * cells as soon as we find out how many there are.
     */
    numFree = 0;
    for (indexPtr = pixelValues, i = 0; i < MAXCOLORS; i++, indexPtr++) {
	if (!XAllocColorCells(Tk_Display(tkwin), Tk_Colormap(tkwin),
		False, NULL, 0, indexPtr, 1)) {
	    break;
	}
	inUse[*indexPtr] = TRUE;/* Indicate the cell is unallocated */
	numFree++;
    }
    XFreeColors(Tk_Display(tkwin), Tk_Colormap(tkwin), pixelValues, numFree, 0);
    for (colorPtr = colorArr, i = 0; i < MAXCOLORS; i++, colorPtr++) {
	colorPtr->pixel = i;
    }
    XQueryColors(Tk_Display(tkwin), Tk_Colormap(tkwin), colorArr, MAXCOLORS);
    for (colorPtr = colorArr, i = 0; i < MAXCOLORS; i++, colorPtr++) {
	if (!inUse[colorPtr->pixel]) {
	    sprintf(string, "#%02x%02x%02x", (colorPtr->red >> 8),
		(colorPtr->green >> 8), (colorPtr->blue >> 8));
	    Tcl_AppendElement(interp, string);
	    sprintf(string, "%ld", colorPtr->pixel);
	    Tcl_AppendElement(interp, string);
	}
    }
    return TCL_OK;
}

#endif

static int
LowerOp(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    register int i;
    Tk_Window tkwin;

    for (i = 2; i < argc; i++) {
	tkwin = NameToWindow(interp, argv[i], (Tk_Window)clientData);
	if (tkwin == NULL) {
	    return TCL_ERROR;
	}
	XLowerWindow(Tk_Display(tkwin), Tk_WindowId(tkwin));
    }
    return TCL_OK;
}

static int
RaiseOp(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    register int i;
    Tk_Window tkwin;

    for (i = 2; i < argc; i++) {
	tkwin = NameToWindow(interp, argv[i], (Tk_Window)clientData);
	if (tkwin == NULL) {
	    return TCL_ERROR;
	}
	if (Tk_IsTopLevel(tkwin)) {
	    Blt_RaiseTopLevelWindow(tkwin);
	} else {
	    XRaiseWindow(Tk_Display(tkwin), Tk_WindowId(tkwin));
	}
    }
    return TCL_OK;
}

static int
MapOp(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    register int i;
    Tk_Window tkwin;
    Tk_FakeWin *fakePtr;

    for (i = 2; i < argc; i++) {
	tkwin = NameToWindow(interp, argv[i], (Tk_Window)clientData);
	if (tkwin == NULL) {
	    return TCL_ERROR;
	}
	if (Tk_IsTopLevel(tkwin)) {
	    Blt_MapTopLevelWindow(tkwin);
	} else {
	    Tk_MapWindow(tkwin);
	}
	fakePtr = (Tk_FakeWin *) tkwin;
	fakePtr->flags |= TK_MAPPED;
    }
    return TCL_OK;
}

/*ARGSUSED*/
static int
MoveToOp(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;			/* Unused */
    char **argv;
{
    int x, y;
    Tk_Window tkwin;

    tkwin = NameToWindow(interp, argv[2], (Tk_Window)clientData);
    if (tkwin == NULL) {
	return TCL_ERROR;
    }
    if (Tk_GetPixels(interp, tkwin, argv[3], &x) != TCL_OK) {
	Tcl_AppendResult(interp, ": bad window x-coordinate", (char *)NULL);
	return TCL_ERROR;
    }
    if (Tk_GetPixels(interp, tkwin, argv[4], &y) != TCL_OK) {
	Tcl_AppendResult(interp, ": bad window y-coordinate", (char *)NULL);
	return TCL_ERROR;
    }
    if (Tk_IsTopLevel(tkwin)) {
	Tk_MoveToplevelWindow(tkwin, x, y);
    } else {
	Tk_MoveWindow(tkwin, x, y);
    }
    return TCL_OK;
}

static int
UnmapOp(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    register int i;
    Tk_Window tkwin;
    Tk_FakeWin *fakePtr;

    for (i = 2; i < argc; i++) {
	tkwin = NameToWindow(interp, argv[i], (Tk_Window)clientData);
	if (tkwin == NULL) {
	    return TCL_ERROR;
	}
	if (Tk_IsTopLevel(tkwin)) {
	    Blt_UnmapTopLevelWindow(tkwin);
	} else {
	    Tk_UnmapWindow(tkwin);
	}
	fakePtr = (Tk_FakeWin *) tkwin;
	fakePtr->flags &= ~TK_MAPPED;
    }
    return TCL_OK;
}

/* ARGSUSED */
static int
QueryOp(interp, tkwin)
    Tcl_Interp *interp;
    Tk_Window tkwin;
{
    int rootX, rootY, childX, childY;
    Window root, child;
    unsigned int mask;

    /* GetCursorPos */
    if (XQueryPointer(Tk_Display(tkwin), Tk_WindowId(tkwin), &root,
	    &child, &rootX, &rootY, &childX, &childY, &mask)) {
	char string[200];

	sprintf(string, "@%d,%d", rootX, rootY);
	Tcl_SetResult(interp, string, TCL_VOLATILE);
    }
    return TCL_OK;
}

/*ARGSUSED*/
static int
WarpToOp(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;			/* Not used */
    char **argv;
{
    Tk_Window tkwin;

    if (argc == 3) {
	if (argv[2][0] == '@') {
	    int x, y;
	    Window root;

	    tkwin = (Tk_Window)clientData;
	    if (Blt_GetXYPosition(interp, tkwin, argv[2], &x, &y) != TCL_OK) {
		return TCL_ERROR;
	    }
	    root = RootWindow(Tk_Display(tkwin), Tk_ScreenNumber(tkwin));
	    XWarpPointer(Tk_Display(tkwin), None, root, 0, 0, 0, 0, x, y);
	} else {
	    tkwin = NameToWindow(interp, argv[2], (Tk_Window)clientData);
	    if (tkwin == NULL) {
		return TCL_ERROR;
	    }
	    if (!Tk_IsMapped(tkwin)) {
		Tcl_AppendResult(interp, "can't warp to unmapped window \"",
		    Tk_PathName(tkwin), "\"", (char *)NULL);
		return TCL_ERROR;
	    }
	    XWarpPointer(Tk_Display(tkwin), None, Tk_WindowId(tkwin),
		0, 0, 0, 0, Tk_Width(tkwin) / 2, Tk_Height(tkwin) / 2);
	}
    }
    return (QueryOp(interp, (Tk_Window)clientData));
}

/*ARGSUSED*/
static int
ReadJPEGOp(clientData, interp, argc, argv)
    ClientData clientData;	/* Not used */
    Tcl_Interp *interp;
    int argc;			/* Not used */
    char **argv;
{
#if HAVE_JPEGLIB_H
    return Blt_JPEGToPhoto(interp, argv[2], argv[3]);
#else
    Tcl_AppendResult(interp, "JPEG support not compiled", (char *)NULL);
    return TCL_ERROR;
#endif
}

/*
 * --------------------------------------------------------------------------
 *
 * SnapOp --
 *
 *	Snaps a picture of a window and stores it in named photo.
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
SnapOp(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;			/* Not used */
    char **argv;
{
    Tk_Window tkwin;
    int result;
    int width, height;

    tkwin = NameToWindow(interp, argv[2], (Tk_Window)clientData);
    if (tkwin == NULL) {
	return TCL_ERROR;
    }
    width = Tk_Width(tkwin), height = Tk_Height(tkwin);
    if ((argc > 4) &&
	(Blt_GetLength(interp, tkwin, argv[4], &width) != TCL_OK)) {
	return TCL_ERROR;
    }
    if ((argc > 5) &&
	(Blt_GetLength(interp, tkwin, argv[5], &height) != TCL_OK)) {
	return TCL_ERROR;
    }
    result = Blt_SnapPhoto(interp, tkwin, Tk_WindowId(tkwin),
	Tk_Width(tkwin), Tk_Height(tkwin), width, height, argv[3]);
    return result;
}

/*
 * This is a temporary home for these image routines.  They will be
 * moved when a new image type is created for them.
 */
/*ARGSUSED*/
static int
ConvolveOp(clientData, interp, argc, argv)
    ClientData clientData;	/* Not used */
    Tcl_Interp *interp;
    int argc;			/* Not used */
    char **argv;
{
    Tk_PhotoHandle srcPhoto, destPhoto;
    ColorImage srcImage, destImage;
    ImageRegion region;
    Image2DFilter filter;
    int numValues;
    char **valueArr;
    float *kernel;
    double value, sum;
    register int i;
    int dim;
    int result = TCL_ERROR;

    srcPhoto = Blt_FindPhoto(interp, argv[2]);
    if (srcPhoto == NULL) {
	Tcl_AppendResult(interp, "source image \"", argv[2], "\" doesn't",
	    " exist or is not a photo image", (char *)NULL);
	return TCL_ERROR;
    }
    destPhoto = Blt_FindPhoto(interp, argv[3]);
    if (destPhoto == NULL) {
	Tcl_AppendResult(interp, "destination image \"", argv[3], "\" doesn't",
	    " exist or is not a photo image", (char *)NULL);
	return TCL_ERROR;
    }
    if (Tcl_SplitList(interp, argv[4], &numValues, &valueArr) != TCL_OK) {
	return TCL_ERROR;
    }
    kernel = NULL;
    if (numValues == 0) {
	Tcl_AppendResult(interp, "empty kernel", (char *)NULL);
	goto error;
    }
    dim = (int)sqrt((double)numValues);
    if ((dim * dim) != numValues) {
	Tcl_AppendResult(interp, "kernel must be square", (char *)NULL);
	goto error;
    }
    kernel = (float *)malloc(sizeof(float) * numValues);
    sum = 0.0;
    for (i = 0; i < numValues; i++) {
	if (Tcl_GetDouble(interp, valueArr[i], &value) != TCL_OK) {
	    goto error;
	}
	kernel[i] = (float)value;
	sum += value;
    }
    filter.kernel = kernel;
    filter.support = dim / (float)2.0;
    filter.sum = (sum == 0) ? (float)1.0 : (float)sum;
    filter.scale = (float)1.0 / numValues;

    region.x = region.y = region.width = region.height = 0;
    srcImage = Blt_PhotoToColorImage(srcPhoto, &region);
    destImage = Blt_ConvolveColorImage(srcImage, &filter);
    Blt_FreeColorImage(srcImage);
    Blt_ColorImageToPhoto(destImage, destPhoto);
    Blt_FreeColorImage(destImage);
    result = TCL_OK;
  error:
    if (valueArr != NULL) {
	free((char *)valueArr);
    }
    if (kernel != NULL) {
	free((char *)kernel);
    }
    return result;
}

/*ARGSUSED*/
static int
ResampleOp(clientData, interp, argc, argv)
    ClientData clientData;	/* Not used */
    Tcl_Interp *interp;
    int argc;			/* Not used */
    char **argv;
{
    Tk_PhotoHandle srcPhoto, destPhoto;
    Tk_PhotoImageBlock src, dest;
    ImageRegion region;
    Image1DFilter filter, vertFilter, horzFilter;
    char *filterName;

    srcPhoto = Blt_FindPhoto(interp, argv[2]);
    if (srcPhoto == NULL) {
	Tcl_AppendResult(interp, "source image \"", argv[2], "\" doesn't",
	    " exist or is not a photo image", (char *)NULL);
	return TCL_ERROR;
    }
    destPhoto = Blt_FindPhoto(interp, argv[3]);
    if (destPhoto == NULL) {
	Tcl_AppendResult(interp, "destination image \"", argv[3], "\" doesn't",
	    " exist or is not a photo image", (char *)NULL);
	return TCL_ERROR;
    }
    filterName = (argc > 4) ? argv[4] : "none";
    if (Blt_FindImage1DFilter(interp, filterName, &filter) != TCL_OK) {
	return TCL_ERROR;
    }
    vertFilter = horzFilter = filter;
    if ((filter != NULL) && (argc > 5)) {
	if (Blt_FindImage1DFilter(interp, argv[5], &filter) != TCL_OK) {
	    return TCL_ERROR;
	}
	vertFilter = filter;
    }
    Tk_PhotoGetImage(srcPhoto, &src);
    Tk_PhotoGetImage(destPhoto, &dest);
    if ((src.width == dest.width) && (src.height == dest.height)) {
      copyImage:
	/* Make copy of image */
	dest.width = src.width;
	dest.height = src.height;
	dest.pixelPtr = src.pixelPtr;
	dest.pixelSize = src.pixelSize;
	dest.pitch = src.pitch;
	dest.offset[0] = src.offset[0];
	dest.offset[1] = src.offset[1];
	dest.offset[2] = src.offset[2];
	Tk_PhotoPutBlock(destPhoto, &dest, 0, 0, dest.width, dest.height);
	return TCL_OK;
    }
    if ((src.width <= 1) || (src.height <= 1)) {
	Tcl_AppendResult(interp, "source image \"", argv[2], "\" is empty",
	    (char *)NULL);
	return TCL_ERROR;
    }
    if ((dest.width <= 1) || (dest.height <= 1)) {
	Tk_PhotoSetSize(destPhoto, src.width, src.height);
	goto copyImage;
    }
    region.x = region.y = region.width = region.height = 0;
    if (filter == NULL) {
	Blt_ResizePhoto(srcPhoto, destPhoto, &region);
    } else {
	Blt_ResamplePhoto(srcPhoto, destPhoto, &region, horzFilter, vertFilter);
    }
    return TCL_OK;
}

/*ARGSUSED*/
static int
SubsampleOp(clientData, interp, argc, argv)
    ClientData clientData;	/* Not used */
    Tcl_Interp *interp;
    int argc;			/* Not used */
    char **argv;
{
    Tk_Window tkwin;
    Tk_PhotoHandle srcPhoto, destPhoto;
    Tk_PhotoImageBlock src, dest;
    ImageRegion region;
    Image1DFilter filter, vertFilter, horzFilter;
    char *filterName;

    srcPhoto = Blt_FindPhoto(interp, argv[2]);
    if (srcPhoto == NULL) {
	Tcl_AppendResult(interp, "source image \"", argv[2], "\" doesn't",
	    " exist or is not a photo image", (char *)NULL);
	return TCL_ERROR;
    }
    destPhoto = Blt_FindPhoto(interp, argv[3]);
    if (destPhoto == NULL) {
	Tcl_AppendResult(interp, "destination image \"", argv[3], "\" doesn't",
	    " exist or is not a photo image", (char *)NULL);
	return TCL_ERROR;
    }
    tkwin = (Tk_Window)clientData;
    if (Blt_GetLength(interp, tkwin, argv[4], &(region.x)) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Blt_GetLength(interp, tkwin, argv[5], &(region.y)) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Blt_GetLength(interp, tkwin, argv[6], &(region.width)) != TCL_OK) {
	return TCL_ERROR;
    }
    if (Blt_GetLength(interp, tkwin, argv[7], &(region.height)) != TCL_OK) {
	return TCL_ERROR;
    }
    filterName = (argc > 8) ? argv[8] : "box";
    if (Blt_FindImage1DFilter(interp, filterName, &filter) != TCL_OK) {
	return TCL_ERROR;
    }
    vertFilter = horzFilter = filter;
    if ((filter != NULL) && (argc > 9)) {
	if (Blt_FindImage1DFilter(interp, argv[9], &filter) != TCL_OK) {
	    return TCL_ERROR;
	}
	vertFilter = filter;
    }
    Tk_PhotoGetImage(srcPhoto, &src);
    Tk_PhotoGetImage(destPhoto, &dest);
    if ((src.width <= 1) || (src.height <= 1)) {
	Tcl_AppendResult(interp, "source image \"", argv[2], "\" is empty",
	    (char *)NULL);
	return TCL_ERROR;
    }
    if (((region.x + region.width) >= src.width) ||
	((region.y + region.height) >= src.height)) {
	Tcl_AppendResult(interp, "nonsensical dimensions for subregion: x=",
	    argv[5], " y=", argv[6], " width=", argv[7], " height=",
	    argv[8], (char *)NULL);
	return TCL_ERROR;
    }
    if ((dest.width <= 1) || (dest.height <= 1)) {
	Tk_PhotoSetSize(destPhoto, region.width, region.height);
    }
    if (filter == NULL) {
	Blt_ResizePhoto(srcPhoto, destPhoto, &region);
    } else {
	Blt_ResamplePhoto(srcPhoto, destPhoto, &region, horzFilter, vertFilter);
    }
    return TCL_OK;
}

static Blt_OpSpec winOps[] =
{
#ifndef WIN32
    {"colormap", 3, (Blt_Operation)ColormapOp, 3, 3, "window",},
#endif
    {"convolve", 3, (Blt_Operation)ConvolveOp, 5, 5,
	"srcPhoto destPhoto filter",},
    {"lower", 1, (Blt_Operation)LowerOp, 2, 0, "window ?window?...",},
    {"map", 1, (Blt_Operation)MapOp, 2, 0, "window ?window?...",},
    {"move", 1, (Blt_Operation)MoveToOp, 5, 5, "window x y",},
    {"raise", 2, (Blt_Operation)RaiseOp, 2, 0, "window ?window?...",},
    {"readjpeg", 3, (Blt_Operation)ReadJPEGOp, 4, 4, "fileName photoName",},
    {"resample", 3, (Blt_Operation)ResampleOp, 4, 6,
	"srcPhoto destPhoto ?horzFilter vertFilter?",},
    {"snap", 1, (Blt_Operation)SnapOp, 4, 6,
	"window photoName ?width height?",},
    {"subsample", 2, (Blt_Operation)SubsampleOp, 8, 10,
	"srcPhoto destPhoto x y width height ?horzFilter? ?vertFilter?",},
    {"unmap", 1, (Blt_Operation)UnmapOp, 2, 0, "window ?window?...",},
    {"warpto", 1, (Blt_Operation)WarpToOp, 2, 3, "?window?",},
};

static int numWinOps = sizeof(winOps) / sizeof(Blt_OpSpec);

/* ARGSUSED */
static int
WindowCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window of interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Blt_Operation proc;
    int result;

    proc = Blt_GetOperation(interp, numWinOps, winOps, BLT_OPER_ARG1,
	argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    result = (*proc) (clientData, interp, argc, argv);
    return (result);
}

int
Blt_WinopInit(interp)
    Tcl_Interp *interp;
{
    static Blt_CmdSpec cmdSpec =
    {"winop", WindowCmd,};

    if (Blt_InitCmd(interp, "blt", &cmdSpec) == NULL) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

#endif /* NO_WINOP */
