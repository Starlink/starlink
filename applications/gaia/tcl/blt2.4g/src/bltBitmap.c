
/*
 * bltBitmap.c --
 *
 *	This module implements Tcl bitmaps for the Tk toolkit.
 *
 *	Much of the code is taken from XRdBitF.c and XWrBitF.c
 *	from the MIT X11R5 distribution.
 *
 * Copyright, 1987, Massachusetts Institute of Technology Permission
 * to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation, and that the name of M.I.T. not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  M.I.T. makes
 * no representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.
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
 *
 * The "bitmap" command created by George Howlett.  */

#include "bltInt.h"

#ifndef NO_BITMAP

#include <ctype.h>
#include <X11/Xutil.h>

extern Tk_CustomOption bltPadOption;

#define MAX_SIZE 255

typedef struct {
    double rotate;		/* Rotation of text string */
    double scale;		/* Scaling factor */
    Tk_Font font;		/* Font pointer */
    Tk_Justify justify;		/* Justify text */
    Pad padX, padY;		/* Padding around the text */
} BitmapInfo;

typedef struct {
    int width, height;		/* Dimension of image */
    unsigned char *dataArr;	/* Data array for bitmap image */
    int arraySize;		/* Number of bytes in data array */

} BitmapData;

#define DEF_BITMAP_FONT		STD_FONT
#define DEF_BITMAP_PAD		"4"
#define DEF_BITMAP_ROTATE	"0.0"
#define DEF_BITMAP_SCALE	"1.0"
#define DEF_BITMAP_JUSTIFY	"center"

static Tk_ConfigSpec composeConfigSpecs[] =
{
    {TK_CONFIG_FONT, "-font", (char *)NULL, (char *)NULL,
	DEF_BITMAP_FONT, Tk_Offset(BitmapInfo, font), 0},
    {TK_CONFIG_JUSTIFY, "-justify", (char *)NULL, (char *)NULL,
	DEF_BITMAP_JUSTIFY, Tk_Offset(BitmapInfo, justify),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-padx", (char *)NULL, (char *)NULL,
	DEF_BITMAP_PAD, Tk_Offset(BitmapInfo, padX),
	TK_CONFIG_DONT_SET_DEFAULT, &bltPadOption},
    {TK_CONFIG_CUSTOM, "-pady", (char *)NULL, (char *)NULL,
	DEF_BITMAP_PAD, Tk_Offset(BitmapInfo, padY),
	TK_CONFIG_DONT_SET_DEFAULT, &bltPadOption},
    {TK_CONFIG_DOUBLE, "-rotate", (char *)NULL, (char *)NULL,
	DEF_BITMAP_ROTATE, Tk_Offset(BitmapInfo, rotate),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_DOUBLE, "-scale", (char *)NULL, (char *)NULL,
	DEF_BITMAP_SCALE, Tk_Offset(BitmapInfo, scale),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL,
	(char *)NULL, 0, 0}
};

static Tk_ConfigSpec defineConfigSpecs[] =
{
    {TK_CONFIG_DOUBLE, "-rotate", (char *)NULL, (char *)NULL,
	DEF_BITMAP_ROTATE, Tk_Offset(BitmapInfo, rotate),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_DOUBLE, "-scale", (char *)NULL, (char *)NULL,
	DEF_BITMAP_SCALE, Tk_Offset(BitmapInfo, scale),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL,
	(char *)NULL, 0, 0}
};

/* Shared data for the image read/parse logic */
static char hexTable[256];	/* conversion value */
static int initialized = 0;	/* easier to fill in at run time */

#define blt_width 40
#define blt_height 40
static unsigned char blt_bits[] =
{
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfc, 0xff, 0xff, 0x03, 0x00, 0x04,
    0x00, 0x00, 0x02, 0x00, 0x04, 0x00, 0x00, 0x02, 0x00, 0xe4, 0x33, 0x3f,
    0x01, 0x00, 0x64, 0x36, 0x0c, 0x01, 0x00, 0x64, 0x36, 0x8c, 0x00, 0x00,
    0xe4, 0x33, 0x8c, 0x00, 0x00, 0x64, 0x36, 0x8c, 0x00, 0x00, 0x64, 0x36,
    0x0c, 0x01, 0x00, 0xe4, 0xf3, 0x0d, 0x01, 0x00, 0x04, 0x00, 0x00, 0x02,
    0x00, 0x04, 0x00, 0x00, 0x02, 0x00, 0xfc, 0xff, 0xff, 0x03, 0x00, 0x0c,
    0x00, 0x00, 0x00, 0x00, 0x0c, 0x00, 0x00, 0x00, 0x00, 0x0c, 0xf8, 0xff,
    0x03, 0x80, 0xed, 0x07, 0x00, 0x04, 0xe0, 0x0c, 0x00, 0x20, 0x09, 0x10,
    0x0c, 0x00, 0x00, 0x12, 0x10, 0x0c, 0x00, 0x00, 0x10, 0x30, 0x00, 0x00,
    0x00, 0x19, 0xd0, 0x03, 0x00, 0x00, 0x14, 0xb0, 0xfe, 0xff, 0xff, 0x1b,
    0x50, 0x55, 0x55, 0x55, 0x0d, 0xe8, 0xaa, 0xaa, 0xaa, 0x16, 0xe4, 0xff,
    0xff, 0xff, 0x2f, 0xf4, 0xff, 0xff, 0xff, 0x27, 0xd8, 0xae, 0xaa, 0xbd,
    0x2d, 0x6c, 0x5f, 0xd5, 0x67, 0x1b, 0xbc, 0xf3, 0x7f, 0xd0, 0x36, 0xf8,
    0x01, 0x10, 0xcc, 0x1f, 0xe0, 0x45, 0x8e, 0x92, 0x0f, 0xb0, 0x32, 0x41,
    0x43, 0x0b, 0xd0, 0xcf, 0x3c, 0x7c, 0x0d, 0xb0, 0xaa, 0xc2, 0xab, 0x0a,
    0x60, 0x55, 0x55, 0x55, 0x05, 0xc0, 0xff, 0xab, 0xaa, 0x03, 0x00, 0x00,
    0xfe, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

#define bigblt_width 64
#define bigblt_height 64
static unsigned char bigblt_bits[] =
{
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0xff, 0xff, 0xff, 0x3f, 0x00,
    0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x02, 0x00,
    0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x10, 0x00,
    0x00, 0x00, 0xe2, 0x0f, 0xc7, 0xff, 0x10, 0x00, 0x00, 0x00, 0xe2, 0x1f,
    0xc7, 0xff, 0x10, 0x00, 0x00, 0x00, 0xe2, 0x38, 0x07, 0x1c, 0x08, 0x00,
    0x00, 0x00, 0xe2, 0x38, 0x07, 0x1c, 0x08, 0x00, 0x00, 0x00, 0xe2, 0x38,
    0x07, 0x1c, 0x08, 0x00, 0x00, 0x00, 0xe2, 0x1f, 0x07, 0x1c, 0x04, 0x00,
    0x00, 0x00, 0xe2, 0x1f, 0x07, 0x1c, 0x04, 0x00, 0x00, 0x00, 0xe2, 0x38,
    0x07, 0x1c, 0x08, 0x00, 0x00, 0x00, 0xe2, 0x38, 0x07, 0x1c, 0x08, 0x00,
    0x00, 0x00, 0xe2, 0x38, 0x07, 0x1c, 0x08, 0x00, 0x00, 0x00, 0xe2, 0x1f,
    0xff, 0x1c, 0x10, 0x00, 0x00, 0x00, 0xe2, 0x0f, 0xff, 0x1c, 0x10, 0x00,
    0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x02, 0x00,
    0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x20, 0x00,
    0x00, 0x00, 0xfe, 0xff, 0xff, 0xff, 0x3f, 0x00, 0x00, 0x00, 0x06, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0xc0, 0xff, 0xff, 0x07, 0x00,
    0x00, 0xe0, 0xf6, 0x3f, 0x00, 0x00, 0x38, 0x00, 0x00, 0x1c, 0x06, 0x00,
    0x00, 0x00, 0xc0, 0x00, 0x80, 0x03, 0x06, 0x00, 0x00, 0xc0, 0x08, 0x03,
    0x40, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00, 0x04, 0x40, 0x00, 0x06, 0x00,
    0x00, 0x00, 0x40, 0x04, 0x40, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00, 0x04,
    0x40, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00, 0x04, 0xc0, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x0c, 0x06, 0x40, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05,
    0xc0, 0xfe, 0x00, 0x00, 0x00, 0x00, 0xc0, 0x06, 0x40, 0x55, 0xff, 0xff,
    0xff, 0xff, 0x7f, 0x05, 0x80, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0x06,
    0x80, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x03, 0x40, 0xab, 0xaa, 0xaa,
    0xaa, 0xaa, 0xaa, 0x01, 0x70, 0x57, 0x55, 0x55, 0x55, 0x55, 0xd5, 0x04,
    0x28, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x0b, 0xd8, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0x14, 0xd0, 0xf7, 0xff, 0xff, 0xff, 0xff, 0xff, 0x13,
    0xf0, 0xda, 0xbf, 0xaa, 0xba, 0xfd, 0xd6, 0x0b, 0x70, 0xed, 0x77, 0x55,
    0x57, 0xe5, 0xad, 0x07, 0xb8, 0xf7, 0xab, 0xaa, 0xaa, 0xd2, 0x5b, 0x0f,
    0xf8, 0xfb, 0x54, 0x55, 0x75, 0x94, 0xf7, 0x1e, 0xf0, 0x7b, 0xfa, 0xff,
    0x9f, 0xa9, 0xef, 0x1f, 0xc0, 0xbf, 0x00, 0x20, 0x40, 0x54, 0xfe, 0x0f,
    0x00, 0x1f, 0x92, 0x00, 0x04, 0xa9, 0xfc, 0x01, 0xc0, 0x5f, 0x41, 0xf9,
    0x04, 0x21, 0xfd, 0x00, 0xc0, 0x9b, 0x28, 0x04, 0xd8, 0x0a, 0x9a, 0x03,
    0x40, 0x5d, 0x08, 0x40, 0x44, 0x44, 0x62, 0x03, 0xc0, 0xaa, 0x67, 0xe2,
    0x03, 0x64, 0xba, 0x02, 0x40, 0x55, 0xd5, 0x55, 0xfd, 0xdb, 0x55, 0x03,
    0x80, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0x01, 0x00, 0x57, 0x55, 0x55,
    0x55, 0x55, 0xd5, 0x00, 0x00, 0xac, 0xaa, 0xaa, 0xaa, 0xaa, 0x2a, 0x00,
    0x00, 0xf0, 0xff, 0x57, 0x55, 0x55, 0x1d, 0x00, 0x00, 0x00, 0x00, 0xf8,
    0xff, 0xff, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

#ifdef __STDC__
static Tcl_CmdProc BitmapCmd;
#endif

/*
 * -----------------------------------------------------------------------
 *
 * GetHexValue --
 *
 *	Converts the hexadecimal string into an unsigned integer
 *	value.  The hexadecimal string need not have a leading "0x".
 *
 * Results:
 *	Returns a standard TCL result. If the conversion was
 *	successful, TCL_OK is returned, otherwise TCL_ERROR.
 *
 * Side Effects:
 * 	If the conversion fails, interp->result is filled with an
 *	error message.
 *
 * -----------------------------------------------------------------------
 */
static int
GetHexValue(interp, string, valuePtr)
    Tcl_Interp *interp;
    char *string;
    int *valuePtr;
{
    register int c;
    register char *s;
    register int value;

    s = string;
    if ((s[0] == '0') && ((s[1] == 'x') || (s[1] == 'X'))) {
	s += 2;
    }
    if (s[0] == '\0') {
	Tcl_AppendResult(interp, "expecting hex value: got \"", string, "\"",
	    (char *)NULL);
	return TCL_ERROR;	/* Only found "0x"  */
    }
    value = 0;
    for ( /*empty*/ ; *s != '\0'; s++) {
	/* Trim high bits, check type and accumulate */
	c = *s & 0xff;
	if (!isxdigit(c)) {
	    Tcl_AppendResult(interp, "expecting hex value: got \"", string,
		"\"", (char *)NULL);
	    return TCL_ERROR;	/* Not a hexadecimal number */
	}
	value = (value << 4) + hexTable[c];
    }
    *valuePtr = value;
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * BitmapToData --
 *
 *	Converts a bitmap into an data array.
 *
 * Results:
 *	Returns the number of bytes in an data array representing the bitmap.
 *
 * Side Effects:
 *	Memory is allocated for the data array. Caller must free
 *	array later.
 *
 * -----------------------------------------------------------------------
 */
static int
BitmapToData(tkwin, bitmap, width, height, dataPtrPtr)
    Tk_Window tkwin;		/* Main window of interpreter */
    Pixmap bitmap;		/* Bitmap to be queried */
    int width, height;		/* Dimensions of the bitmap */
    unsigned char **dataPtrPtr;	/* Pointer to converted array of data */
{
    int value, bitMask;
    unsigned long pixel;
    register int x, y;
    int count;
    int arraySize, bytes_per_line;
    Display *display;
#ifdef WIN32
    TkWinDCState state;
    HDC image;
#else 
    XImage *imagePtr;
#endif /*WIN32*/
    unsigned char *dataPtr;

    display = Tk_Display(tkwin);
    /* Convert the bitmap to an image */
#ifdef WIN32
    image = TkWinGetDrawableDC(display, bitmap, &state);
#else 
    imagePtr = XGetImage(display, bitmap, 0, 0, width, height, 1L, XYPixmap);
#endif /*WIN32*/
    /*
     * The slow but robust brute force method of converting an image:
     */
    bytes_per_line = (width + 7) / 8;
    arraySize = height * bytes_per_line;
    dataPtr = (unsigned char *)malloc(sizeof(unsigned char) * arraySize);
    assert(dataPtr);
    count = 0;
    for (y = 0; y < height; y++) {
	value = 0, bitMask = 1;
	for (x = 0; x < width; /*empty*/ ) {
#ifdef WIN32
	    pixel = GetPixel(image, x, y);
#else 
	    pixel = XGetPixel(imagePtr, x, y);
#endif /*WIN32*/
	    if (pixel) {
		value |= bitMask;
	    }
	    bitMask <<= 1;
	    x++;
	    if (!(x & 7)) {
		dataPtr[count++] = (unsigned char)value;
		value = 0, bitMask = 1;
	    }
	}
	if (x & 7) {
	    dataPtr[count++] = (unsigned char)value;
	}
    }
#ifdef WIN32
    TkWinReleaseDrawableDC(bitmap, image, &state);
#else 
    XDestroyImage(imagePtr);
#endif /*WIN32*/
    *dataPtrPtr = dataPtr;
    return count;
}

/*
 * -----------------------------------------------------------------------
 *
 * AsciiToData --
 *
 *	Converts a Tcl list of ASCII values into a data array.
 *
 * Results:
 *	A standard TCL result.
 *
 * Side Effects:
 * 	If an error occurs while processing the data, interp->result 
 * 	is filled with a corresponding error message.
 *
 * ----------------------------------------------------------------------- 
 */
static int
AsciiToData(interp, elemList, width, height, dataPtrPtr)
    Tcl_Interp *interp;		/* Interpreter to report results to */
    char *elemList;		/* List of of hex numbers representing
				 * bitmap data */
    int width, height;		/* Height and width */
    unsigned char **dataPtrPtr;	/* data array (output) */
{
    int arraySize;		/* Number of bytes of data */
    int value;			/* from an input line */
    int padding;		/* to handle alignment */
    int bytesPerLine;		/* per scanline of data */
    unsigned char *dataPtr;
    register int count;
    enum Formats {
	V10, V11
    } format;
    register int i;		/*  */
    char **valueArr;
    int numValues;

    /* First time through initialize the ascii->hex translation table */
    if (!initialized) {
	Blt_InitHexTable(hexTable);
	initialized = 1;
    }
    if (Tcl_SplitList(interp, elemList, &numValues, &valueArr) != TCL_OK) {
	return -1;
    }
    bytesPerLine = (width + 7) / 8;
    arraySize = bytesPerLine * height;
    if (numValues == arraySize) {
	format = V11;
    } else if (numValues == (arraySize / 2)) {
	format = V10;
    } else {
	Tcl_AppendResult(interp, "bitmap has wrong # of data values", 
		(char *)NULL);
	goto error;
    }
    padding = 0;
    if (format == V10) {
	padding = ((width % 16) && ((width % 16) < 9));
	if (padding) {
	    bytesPerLine = (width + 7) / 8 + padding;
	    arraySize = bytesPerLine * height;
	}
    }
    dataPtr = (unsigned char *)calloc(sizeof(unsigned char), arraySize);
    if (dataPtr == NULL) {
	Tcl_AppendResult(interp, "can't allocate memory for bitmap", 
		(char *)NULL);
	goto error;
    }
    count = 0;
    for (i = 0; i < numValues; i++) {
	if (GetHexValue(interp, valueArr[i], &value) != TCL_OK) {
	    free((char *)dataPtr);
	    goto error;
	}
	dataPtr[count++] = (unsigned char)value;
	if (format == V10) {
	    if ((!padding) || (((i * 2) + 2) % bytesPerLine)) {
		dataPtr[count++] = value >> 8;
	    }
	}
    }
    free((char *)valueArr);
    *dataPtrPtr = dataPtr;
    return (count);
  error:
    free((char *)valueArr);
    return -1;
}

/*
 * -----------------------------------------------------------------------
 *
 * RotateData --
 *
 *	Creates a new data array of the rotated image.
 *
 * Results:
 *	A standard Tcl result. If the bitmap data is rotated
 *	successfully, TCL_OK is returned.  But if memory could not be
 *	allocated for the new data array, TCL_ERROR is returned and an
 *	error message is left in interp->result.
 *
 * Side Effects:
 *	Memory is allocated for rotated data array. Caller must
 *	free array later.
 *
 * ----------------------------------------------------------------------- 
 */
static int
RotateData(interp, srcPtr, theta, destPtr)
    Tcl_Interp *interp;		/* Interpreter to report results to */
    BitmapData *srcPtr;
    double theta;		/* Rotate bitmap this many degrees */
    BitmapData *destPtr;
{
    register int dx, dy, sx, sy;
    double srcX, srcY, destX, destY;	/* Origins of source and destination
					 * bitmaps */
    double sinTheta, cosTheta;
    double transX, transY, rotX, rotY;
    double radians;
    unsigned char *dataArr;
    int arraySize;
    int pixel, arrIndex;
    int srcBytesPerLine, destBytesPerLine;

    srcBytesPerLine = (srcPtr->width + 7) / 8;
    Blt_GetBoundingBox(srcPtr->width, srcPtr->height, theta,
	&(destPtr->width), &(destPtr->height), (XPoint *)NULL);

    destBytesPerLine = (destPtr->width + 7) / 8;
    arraySize = destPtr->height * destBytesPerLine;
    dataArr = (unsigned char *)calloc(arraySize, sizeof(unsigned char));
    if (dataArr == NULL) {
	Tcl_AppendResult(interp, "can't allocate bitmap data array", 
		(char *)NULL);
	return TCL_ERROR;
    }
    destPtr->dataArr = dataArr;
    destPtr->arraySize = arraySize;

    radians = (theta / 180.0) * M_PI;
    sinTheta = sin(radians);
    cosTheta = cos(radians);

    /*
     * Coordinates of the centers of the source and destination rectangles
     */
    srcX = srcPtr->width * 0.5;
    srcY = srcPtr->height * 0.5;
    destX = destPtr->width * 0.5;
    destY = destPtr->height * 0.5;

    /*
     * Rotate each pixel of dest image, placing results in source image
     */
    for (dx = 0; dx < destPtr->width; dx++) {
	for (dy = 0; dy < destPtr->height; dy++) {
	    if (theta == 270.0) {
		sx = dy, sy = destPtr->width - dx - 1;
	    } else if (theta == 180.0) {
		sx = destPtr->width - dx - 1, sy = destPtr->height - dy - 1;
	    } else if (theta == 90.0) {
		sx = destPtr->height - dy - 1, sy = dx;
	    } else if (theta == 0.0) {
		sx = dx, sy = dy;
	    } else {
		/* Translate origin to center of destination image */

		transX = dx - destX;
		transY = dy - destY;

		/* Rotate the coordinates about the origin */

		rotX = (transX * cosTheta) - (transY * sinTheta);
		rotY = (transX * sinTheta) + (transY * cosTheta);

		/* Translate back to the center of the source image */
		rotX += srcX;
		rotY += srcY;

		sx = ROUND(rotX);
		sy = ROUND(rotY);

		/* 
		 * Verify the coordinates, since the destination image
		 * can be bigger than the source.
		 */

		if ((sx >= srcPtr->width) || (sx < 0) ||
		    (sy >= srcPtr->height) || (sy < 0)) {
		    continue;
		}
	    }
	    arrIndex = (srcBytesPerLine * sy) + (sx / 8);
	    pixel = srcPtr->dataArr[arrIndex] & (1 << (sx % 8));
	    if (pixel) {
		arrIndex = (destBytesPerLine * dy) + (dx / 8);
		dataArr[arrIndex] |= (1 << (dx % 8));
	    }
	}
    }
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * ScaleData --
 *
 *	Scale the data of the bitmap, creating a new data array of the
 *	scaled bitmap.
 *
 * Results:
 *	A standard Tcl result. If the bitmap data is scaled
 *	successfully, TCL_OK is returned.  But if memory could not be
 *	allocated for the new data array, TCL_ERROR is returned and an
 *	error message is left in interp->result.
 *
 * Side Effects:
 *	Memory is allocated for scaled data array. Caller must
 *	dispose of this array later.
 *
 * ----------------------------------------------------------------------- 
 */
static int
ScaleData(interp, srcPtr, scale, destPtr)
    Tcl_Interp *interp;		/* Interpreter to report results to */
    BitmapData *srcPtr;
    double scale;		/* Scale bitmap by this factor */
    BitmapData *destPtr;
{
    register int dx, dy, sx, sy;
    double scaleX, scaleY;
    unsigned char *dataArr;
    int arraySize;
    int pixel, arrIndex;
    int srcBytesPerLine, destBytesPerLine;

    destPtr->width = (int)((srcPtr->width * scale) + 0.5);
    destPtr->height = (int)((srcPtr->height * scale) + 0.5);
    srcBytesPerLine = (srcPtr->width + 7) / 8;
    destBytesPerLine = (destPtr->width + 7) / 8;

    arraySize = destPtr->height * destBytesPerLine;
    dataArr = (unsigned char *)calloc(arraySize, sizeof(unsigned char));
    if (dataArr == NULL) {
	Tcl_AppendResult(interp, "can't allocate bitmap data array", (char *)NULL);
	return TCL_ERROR;
    }
    destPtr->dataArr = dataArr;
    destPtr->arraySize = arraySize;

    /*
     * Scale each pixel of destination image from results of source image
     */
    for (dy = 0; dy < destPtr->height; dy++) {
	scaleY = (dy / scale);
	sy = ROUND(scaleY);
	for (dx = 0; dx < destPtr->width; dx++) {
	    scaleX = (dx / scale);
	    sx = ROUND(scaleX);

	    /*
	     * Verify the coordinates, since the destination image can be
	     * bigger than the source
	     */
	    if ((sx >= srcPtr->width) || (sx < 0) ||
		(sy >= srcPtr->height) || (sy < 0)) {
		continue;
	    }
	    arrIndex = (srcBytesPerLine * sy) + (sx / 8);
	    pixel = srcPtr->dataArr[arrIndex] & (1 << (sx % 8));
	    if (pixel) {
		arrIndex = (destBytesPerLine * dy) + (dx / 8);
		dataArr[arrIndex] |= (1 << (dx % 8));
	    }
	}
    }
    return TCL_OK;
}

/*
 * -----------------------------------------------------------------------
 *
 * BitmapDataToString --
 *
 *	Returns a list of hex values corresponding to the data
 *	bits of the bitmap given.
 *
 *	Converts the unsigned character value into a two character
 *	hexadecimal string.  A separator is also added, which may
 *	either a newline or space according the the number of bytes
 *	already output.
 *
 * Results:
 *	Returns TCL_ERROR if a data array can't be generated
 *	from the bitmap (memory allocation failure), otherwise TCL_OK.
 *
 * ----------------------------------------------------------------------- 
 */
static void
BitmapDataToString(tkwin, bitmap, dStrPtr)
    Tk_Window tkwin;		/* Main window of interpreter */
    Pixmap bitmap;		/* Bitmap to be queried */
    Tcl_DString *dStrPtr;	/* Dynamic string to output results to */
{
    unsigned char *dataPtr;
    char *separator;
    int arraySize;
    register int i;
    char string[200];
    int width, height;

    /* Get the dimensions of the bitmap */
    Tk_SizeOfBitmap(Tk_Display(tkwin), bitmap, &width, &height);
    arraySize = BitmapToData(tkwin, bitmap, width, height, &dataPtr);
#define BYTES_PER_OUTPUT_LINE 24
    for (i = 0; i < arraySize; i++) {
	separator = (i % BYTES_PER_OUTPUT_LINE) ? " " : "\n    ";
	sprintf(string, "%s%02x", separator, dataPtr[i]);
	Tcl_DStringAppend(dStrPtr, string, -1);
    }
    free((char *)dataPtr);
}

/*
 *--------------------------------------------------------------
 *
 * ComposeOp --
 *
 *	Converts the text string into an internal bitmap.
 *
 *	There's a lot of extra (read unnecessary) work going on here,
 *	but I don't (right now) think that it matters much.  The
 *	rotated bitmap (formerly an image) is converted back to an
 *	image just so we can convert it to a data array for
 *	Tk_DefineBitmap.
 *
 * Results:
 *	A standard TCL result.
 *
 * Side Effects:
 * 	If an error occurs while processing the data, interp->result 
 * 	is filled with a corresponding error message.
 *
 *-------------------------------------------------------------- 
 */
static int
ComposeOp(tkwin, interp, argc, argv)
    Tk_Window tkwin;		/* Main window of interpreter */
    Tcl_Interp *interp;		/* Interpreter to report results to */
    int argc;			/* Number of arguments */
    char **argv;		/* Argument list */
{
    int bmWidth, bmHeight;	/* Dimensions of bitmap */
    Pixmap bitmap;		/* Text bitmap */
    unsigned char *dataPtr;	/* Data array derived from text bitmap */
    int arraySize;
    BitmapInfo info;		/* Text rotation and font information */
    int result;
    double theta;
    TextAttributes attr;
    CompoundText *textPtr;

    /* Initialize info and process flags */
    info.justify = TK_JUSTIFY_CENTER;
    info.rotate = 0.0;		/* No rotation or scaling by default */
    info.scale = 1.0;
    info.padLeft = info.padRight = 0;
    info.padTop = info.padBottom = 0;
    info.font = (Tk_Font)NULL;	/* Initialized by Tk_ConfigureWidget */
    if (Tk_ConfigureWidget(interp, tkwin, composeConfigSpecs,
	    argc - 4, argv + 4, (char *)&info, 0) != TCL_OK) {
	return TCL_ERROR;
    }
    theta = FMOD(info.rotate, (double)360.0);
    if (theta < 0.0) {
	theta += 360.0;
    }
    Blt_InitTextAttributes(&attr);
    attr.font = info.font;
    attr.theta = theta;
    attr.justify = info.justify;
    attr.padX = info.padX;
    attr.padY = info.padY;
    attr.leader = 0;
    attr.anchor = TK_ANCHOR_CENTER;

    textPtr = Blt_GetCompoundText(argv[3], &attr);
    bitmap = Blt_CreateTextBitmap(tkwin, textPtr, &attr, &bmWidth, &bmHeight, 
	FALSE);
    free((char *)textPtr);
    if (bitmap == None) {
	Tcl_AppendResult(interp, "can't create bitmap", (char *)NULL);
	return TCL_ERROR;
    }
    /* Free the font structure, since we don't need it anymore */
    Tk_FreeOptions(composeConfigSpecs, (char *)&info, Tk_Display(tkwin), 0);

    /* Convert bitmap back to a data array */
    arraySize = BitmapToData(tkwin, bitmap, bmWidth, bmHeight, &dataPtr);
    Tk_FreePixmap(Tk_Display(tkwin), bitmap);
    if (info.scale != 1.0) {
	BitmapData srcData, destData;

	srcData.dataArr = dataPtr;
	srcData.width = bmWidth;
	srcData.height = bmHeight;
	srcData.arraySize = arraySize;

	result = ScaleData(interp, &srcData, info.scale, &destData);
	free((char *)dataPtr);	/* Free the un-scaled data array */
	if (result != TCL_OK) {
	    return TCL_ERROR;
	}
	dataPtr = destData.dataArr;
	bmWidth = destData.width;
	bmHeight = destData.height;
    }
    /* Create the bitmap again, this time using Tk's bitmap facilities */
    result = Tk_DefineBitmap(interp, Tk_GetUid(argv[2]), (char *)dataPtr,
	bmWidth, bmHeight);
    if (result != TCL_OK) {
	free((char *)dataPtr);
    }
    return result;
}

/*
 *--------------------------------------------------------------
 *
 * DefineOp --
 *
 *	Converts the dataList into an internal bitmap.
 *
 * Results:
 *	A standard TCL result.
 *
 * Side Effects:
 * 	If an error occurs while processing the data, interp->result
 *	is filled with a corresponding error message.
 *
 *--------------------------------------------------------------
 */
/* ARGSUSED */
static int
DefineOp(tkwin, interp, argc, argv)
    Tk_Window tkwin;		/* Main window of interpreter */
    Tcl_Interp *interp;		/* Interpreter to report results to */
    int argc;			/* Number of arguments */
    char **argv;		/* Argument list */
{
    int width, height;		/* Dimensions of bitmap */
    char *string, **elemArr;
    int numElem;
    unsigned char *dataPtr;	/* working variable */
    register char *p;
    BitmapInfo info;		/* not used */
    int arraySize;
    int result;
    double theta;

    /* Initialize info and then process flags */
    info.rotate = 0.0;		/* No rotation by default */
    info.scale = 1.0;		/* No scaling by default */
    if (Tk_ConfigureWidget(interp, tkwin, defineConfigSpecs,
	    argc - 4, argv + 4, (char *)&info, 0) != TCL_OK) {
	return TCL_ERROR;
    }
    dataPtr = NULL;
    elemArr = NULL;
    if (Tcl_SplitList(interp, argv[3], &numElem, &elemArr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (numElem == 2) {
	char **dimArr;
	int numDim;

	if (Tcl_SplitList(interp, elemArr[0], &numDim, &dimArr) != TCL_OK) {
	    goto error;
	}
	if (numDim != 2) {
	    Tcl_AppendResult(interp, "wrong # of bitmap dimensions: ",
			     "should be \"width height\"", (char *)NULL);
	    goto error;
	}
	result = ((Tcl_GetInt(interp, dimArr[0], &width) == TCL_OK) &&
		  (Tcl_GetInt(interp, dimArr[1], &height) == TCL_OK));
	free((char *)dimArr);
	if (!result) {
	    goto error;
	}
	string = elemArr[1];
    } else if (numElem == 3) {
	if ((Tcl_GetInt(interp, elemArr[0], &width) != TCL_OK) ||
	    (Tcl_GetInt(interp, elemArr[1], &height) != TCL_OK)) {
	    goto error;
	}
	string = elemArr[2];
    } else {
	Tcl_AppendResult(interp, "wrong # of bitmap data components: ",
	    "should be \"dimensions sourceData\"", (char *)NULL);
	goto error;
    }
    if ((width < 1) || (height < 1)) {
	Tcl_AppendResult(interp, "bad bitmap dimensions", (char *)NULL);
	goto error;
    }

    /* Convert commas to blank spaces */

    for (p = string; *p != '\0'; p++) {
	if (*p == ',') {
	    *p = ' ';
	}
    }
    arraySize = AsciiToData(interp, string, width, height, &dataPtr);
    if (arraySize < 0) {
	goto error;
    }
    free((char *)elemArr);
    elemArr = NULL;

    theta = FMOD(info.rotate, 360.0);
    if (theta < 0.0) {
	theta += 360.0;
    }
    /* If bitmap is to be rotated or scale, do it here */
    if (theta != 0.0) {
	BitmapData srcData, destData;

	srcData.dataArr = dataPtr;
	srcData.width = width;
	srcData.height = height;
	srcData.arraySize = arraySize;

	result = RotateData(interp, &srcData, theta, &destData);
	free((char *)dataPtr);	/* Free the un-rotated data array */
	if (result != TCL_OK) {
	    return TCL_ERROR;
	}
	dataPtr = destData.dataArr;
	width = destData.width;
	height = destData.height;
    }
    if (info.scale != 1.0) {
	BitmapData srcData, destData;

	srcData.dataArr = dataPtr;
	srcData.width = width;
	srcData.height = height;
	srcData.arraySize = arraySize;

	result = ScaleData(interp, &srcData, info.scale, &destData);
	free((char *)dataPtr);	/* Free the un-scaled data array */
	if (result != TCL_OK) {
	    return TCL_ERROR;
	}
	dataPtr = destData.dataArr;
	width = destData.width;
	height = destData.height;
    }
    result = Tk_DefineBitmap(interp, Tk_GetUid(argv[2]), (char *)dataPtr,
	width, height);
    if (result != TCL_OK) {
	free((char *)dataPtr);
    }
    return result;

  error:
    if (dataPtr != NULL) {
	free((char *)dataPtr);
    }
    if (elemArr != NULL) {
	free((char *)elemArr);
    }
    return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * ExistOp --
 *
 *	Indicates if the named bitmap exists.
 *
 *--------------------------------------------------------------
 */
/*ARGSUSED*/
static int
ExistsOp(tkwin, interp, argc, argv)
    Tk_Window tkwin;		/* Main window of interpreter */
    Tcl_Interp *interp;		/* Interpreter to report results to */
    int argc;			/* not used */
    char **argv;		/* Argument list */
{
    Pixmap bitmap;

    bitmap = Tk_GetBitmap(interp, tkwin, Tk_GetUid(argv[2]));
    Tcl_ResetResult(interp);
    if (bitmap != None) {
	Tk_FreeBitmap(Tk_Display(tkwin), bitmap);
    }
    Tcl_SetResult(interp, (bitmap == None) ? "0" : "1", TCL_STATIC);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * HeightOp --
 *
 *	Returns the height of the named bitmap.
 *
 *--------------------------------------------------------------
 */
/*ARGSUSED*/
static int
HeightOp(tkwin, interp, argc, argv)
    Tk_Window tkwin;		/* Main window of interpreter */
    Tcl_Interp *interp;		/* Interpreter to report results to */
    int argc;			/* not used */
    char **argv;		/* Argument list */
{
    int width, height;
    Pixmap bitmap;

    bitmap = Tk_GetBitmap(interp, tkwin, Tk_GetUid(argv[2]));
    if (bitmap == None) {
	return TCL_ERROR;
    }
    Tk_SizeOfBitmap(Tk_Display(tkwin), bitmap, &width, &height);
    Tcl_SetResult(interp, Blt_Int(height), TCL_VOLATILE);
    Tk_FreeBitmap(Tk_Display(tkwin), bitmap);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * WidthOp --
 *
 *	Returns the width of the named bitmap.
 *
 *--------------------------------------------------------------
 */
/*ARGSUSED*/
static int
WidthOp(tkwin, interp, argc, argv)
    Tk_Window tkwin;		/* Main window of interpreter */
    Tcl_Interp *interp;		/* Interpreter to report results to */
    int argc;			/* not used */
    char **argv;		/* Argument list */
{
    int width, height;
    Pixmap bitmap;

    bitmap = Tk_GetBitmap(interp, tkwin, Tk_GetUid(argv[2]));
    if (bitmap == None) {
	return TCL_ERROR;
    }
    Tk_SizeOfBitmap(Tk_Display(tkwin), bitmap, &width, &height);
    Tcl_SetResult(interp, Blt_Int(width), TCL_VOLATILE);
    Tk_FreeBitmap(Tk_Display(tkwin), bitmap);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * SourceOp --
 *
 *	Returns the data array (excluding width and height)
 *	of the named bitmap.
 *
 *--------------------------------------------------------------
 */
/*ARGSUSED*/
static int
SourceOp(tkwin, interp, argc, argv)
    Tk_Window tkwin;		/* Main window of interpreter */
    Tcl_Interp *interp;		/* Interpreter to report results to */
    int argc;			/* not used */
    char **argv;		/* Argument list */
{
    Pixmap bitmap;
    Tcl_DString dStr;

    bitmap = Tk_GetBitmap(interp, tkwin, Tk_GetUid(argv[2]));
    if (bitmap == None) {
	return TCL_ERROR;
    }
    Tcl_DStringInit(&dStr);
    BitmapDataToString(tkwin, bitmap, &dStr);
    Tk_FreeBitmap(Tk_Display(tkwin), bitmap);
    Tcl_DStringResult(interp, &dStr);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * DataOp --
 *
 *	Returns the data array, including width and height,
 *	of the named bitmap.
 *
 *--------------------------------------------------------------
 */
/*ARGSUSED*/
static int
DataOp(tkwin, interp, argc, argv)
    Tk_Window tkwin;		/* Main window of interpreter */
    Tcl_Interp *interp;		/* Interpreter to report results to */
    int argc;			/* not used */
    char **argv;		/* Argument list */
{
    Pixmap bitmap;
    int width, height;
    Tcl_DString dString;

    bitmap = Tk_GetBitmap(interp, tkwin, Tk_GetUid(argv[2]));
    if (bitmap == None) {
	return TCL_ERROR;
    }
    Tk_SizeOfBitmap(Tk_Display(tkwin), bitmap, &width, &height);
    Tcl_DStringInit(&dString);
    Tcl_DStringAppendElement(&dString, Blt_Int(width));
    Tcl_DStringAppendElement(&dString, Blt_Int(height));
    Tcl_DStringStartSublist(&dString);
    BitmapDataToString(tkwin, bitmap, &dString);
    Tcl_DStringEndSublist(&dString);
    Tk_FreeBitmap(Tk_Display(tkwin), bitmap);
    Tcl_DStringResult(interp, &dString);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * BLT Sub-command specification:
 *
 *	- Name of the sub-command.
 *	- Minimum number of characters needed to unambiguously
 *        recognize the sub-command.
 *	- Pointer to the function to be called for the sub-command.
 *	- Minimum number of arguments accepted.
 *	- Maximum number of arguments accepted.
 *	- String to be displayed for usage.
 *
 *--------------------------------------------------------------
 */
static Blt_OpSpec bitmapOps[] =
{
    {"compose", 1, (Blt_Operation)ComposeOp, 4, 0,
	"bitmapName text ?flags?",},
    {"data", 2, (Blt_Operation)DataOp, 3, 3, "bitmapName",},
    {"define", 2, (Blt_Operation)DefineOp, 4, 0,
	"bitmapName data ?flags?",},
    {"exists", 1, (Blt_Operation)ExistsOp, 3, 3, "bitmapName",},
    {"height", 1, (Blt_Operation)HeightOp, 3, 3, "bitmapName",},
    {"source", 1, (Blt_Operation)SourceOp, 3, 3, "bitmapName",},
    {"width", 1, (Blt_Operation)WidthOp, 3, 3, "bitmapName",},
};
static int numBitmapOps = sizeof(bitmapOps) / sizeof(Blt_OpSpec);

/*
 *--------------------------------------------------------------
 *
 * BitmapCmd --
 *
 *	This procedure is invoked to process the Tcl command
 *	that corresponds to bitmaps managed by this module.
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

static int
BitmapCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window of interpreter */
    Tcl_Interp *interp;		/* Interpreter to report results to */
    int argc;
    char **argv;
{
    Tk_Window tkwin = (Tk_Window)clientData;
    Blt_Operation proc;
    int result;

    proc = Blt_GetOperation(interp, numBitmapOps, bitmapOps, BLT_OPER_ARG1,
	argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    result = (*proc) (tkwin, interp, argc, argv);
    return (result);
}

/*
 *--------------------------------------------------------------
 *
 * Blt_BitmapInit --
 *
 *	This procedure is invoked to initialize the bitmap command.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Adds the command to the interpreter and sets an array variable
 *	which its version number.
 *
 *--------------------------------------------------------------
 */
int
Blt_BitmapInit(interp)
    Tcl_Interp *interp;
{
    static Blt_CmdSpec cmdSpec =
    {"bitmap", BitmapCmd,};

    /* Define the BLT logo bitmaps */

    if (Blt_InitCmd(interp, "blt", &cmdSpec) == NULL) {
	return TCL_ERROR;
    }
    Tk_DefineBitmap(interp, Tk_GetUid("BLT"), (char *)blt_bits,
	blt_width, blt_height);
    Tk_DefineBitmap(interp, Tk_GetUid("bigBLT"), (char *)bigblt_bits,
	bigblt_width, bigblt_height);
    return TCL_OK;
}

#endif /* NO_BITMAP */
