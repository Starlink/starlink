/*
 * bltImage.c --
 *
 *	This module implements image processing procedures for the BLT
 *	toolkit.
 *
 * Copyright 1997-1998 Lucent Technologies, Inc.
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
#include "bltImage.h"
#include <ctype.h>
#include <X11/Xutil.h>
#ifndef WIN32
#include <X11/Xproto.h>
#endif

#define CLAMP(c)	((((c) < 0.0) ? 0.0 : ((c) > 255.0) ? 255.0 : (c)))


/*
 * Each call to Tk_GetImage returns a pointer to one of the following
 * structures, which is used as a token by clients (widgets) that
 * display images.
 */
typedef struct TkImage {
    Tk_Window tkwin;		/* Window passed to Tk_GetImage (needed to
				 * "re-get" the image later if the manager
				 * changes). */
    Display *display;		/* Display for tkwin.  Needed because when
				 * the image is eventually freed tkwin may
				 * not exist anymore. */
    struct TkImageMaster *masterPtr;
				/* Master for this image (identifiers image
				 * manager, for example). */
    ClientData instanceData;
				/* One word argument to pass to image manager
				 * when dealing with this image instance. */
    Tk_ImageChangedProc *changeProc;
				/* Code in widget to call when image changes
				 * in a way that affects redisplay. */
    ClientData widgetClientData;
				/* Argument to pass to changeProc. */
    struct Image *nextPtr;	/* Next in list of all image instances
				 * associated with the same name. */

} TkImage;

/*
 * For each image master there is one of the following structures,
 * which represents a name in the image table and all of the images
 * instantiated from it.  Entries in mainPtr->imageTable point to
 * these structures.
 */
typedef struct TkImageMaster {
    Tk_ImageType *typePtr;	/* Information about image type.  NULL means
				 * that no image manager owns this image:  the
				 * image was deleted. */
    ClientData masterData;	/* One-word argument to pass to image mgr
				 * when dealing with the master, as opposed
				 * to instances. */
    int width, height;		/* Last known dimensions for image. */
    Tcl_HashTable *tablePtr;	/* Pointer to hash table containing image
				 * (the imageTable field in some TkMainInfo
				 * structure). */
    Tcl_HashEntry *hPtr;	/* Hash entry in mainPtr->imageTable for
				 * this structure (used to delete the hash
				 * entry). */
    TkImage *instancePtr;	/* Pointer to first in list of instances
				 * derived from this name. */
} TkImageMaster;

/*
 * ----------------------------------------------------------------------
 *
 * Tk_ImageDeleted --
 *
 *	Is there any other way to determine if an image has been
 *	deleted?
 *
 * Results:
 *	Returns 1 if the image has been deleted, 0 otherwise.
 *
 * ----------------------------------------------------------------------
 */
/*LINTLIBRARY*/
int
Blt_TkImageDeleted(tkImage)
    Tk_Image tkImage;		/* Token for image. */
{
    TkImage *imagePtr = (TkImage *) tkImage;

    return (imagePtr->masterPtr->typePtr == NULL);
}

/*LINTLIBRARY*/
Tk_ImageMaster
Blt_TkImageMaster(tkImage)
    Tk_Image tkImage;		/* Token for image. */
{
    TkImage *imagePtr = (TkImage *) tkImage;

    return (Tk_ImageMaster) imagePtr->masterPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * ResizeImageData
 *
 *      Reallocates the data for a color image to a designated
 *	width and height.
 *
 * Results:
 *      None.
 *
 * Side Effects:
 *	The data allocated for the image is changed (if needed).
 *
 *----------------------------------------------------------------------
 */
static void
ResizeImageData(imagePtr, width, height)
    struct ColorImage *imagePtr;
    int width, height;		/* Dimensions of new image */
{
    Pix32 *dataPtr;
    unsigned int numPixels;

    if ((width == imagePtr->width) && (height == imagePtr->height)) {
	return;			/* No change. */
    }
    if (imagePtr->dataPtr != NULL) {
	free((char *)imagePtr->dataPtr);
    }
    numPixels = width * height;
    dataPtr = (Pix32 *) calloc(numPixels, sizeof(Pix32));
    assert(dataPtr);
    imagePtr->width = width;
    imagePtr->height = height;
    imagePtr->dataPtr = dataPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_CreateColorImage --
 *
 *      Allocates a color image of a designated height and width.
 *
 *	This routine will be augmented with other types of information
 *	such as a color table, etc.
 *
 * Results:
 *      Returns the new color image.
 *
 *----------------------------------------------------------------------
 */
ColorImage
Blt_CreateColorImage(width, height)
    int width, height;		/* Dimensions of new image */
{
    unsigned int numPixels;
    Pix32 *dataPtr;
    struct ColorImage *imagePtr;

    numPixels = width * height;
    dataPtr = (Pix32 *) calloc(numPixels, sizeof(Pix32));
    assert(dataPtr);
    imagePtr = (struct ColorImage *)malloc(sizeof(struct ColorImage));
    assert(imagePtr);
    imagePtr->width = width;
    imagePtr->height = height;
    imagePtr->dataPtr = dataPtr;
    return imagePtr;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_FreeColorImage --
 *
 *      Deallocates the given color image.
 *
 * Results:
 *      None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_FreeColorImage(image)
    ColorImage image;
{
    Pix32 *dataPtr;

    dataPtr = ColorImageData(image);
    free((char *)dataPtr);
    free((char *)image);
}

void
Blt_GammaCorrectColorImage(image, newGamma)
    ColorImage image;
    double newGamma;
{
    unsigned int numPixels;
    register Pix32 *dataPtr;
    register unsigned int i;
    double value;
    unsigned char table[256];
    double invGamma;

    invGamma = 1.0 / newGamma;
    for (i = 0; i < 256; i++) {
	value = 255.0 * pow((double)i / 255.0, invGamma);
	table[i] = (unsigned char)CLAMP(value);
    }
    numPixels = ColorImageWidth(image) * ColorImageHeight(image);
    dataPtr = ColorImageData(image);
    for (i = 0; i < numPixels; i++, dataPtr++) {
	dataPtr->Red = table[dataPtr->Red];
	dataPtr->Green = table[dataPtr->Green];
	dataPtr->Blue = table[dataPtr->Blue];
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ColorImageToPhoto --
 *
 *      Translates a color image into a Tk photo.
 *
 * Results:
 *      The photo is re-written with the new color image.
 *
 *----------------------------------------------------------------------
 */
void
Blt_ColorImageToPhoto(image, photo)
    ColorImage image;		/* Image to use as source */
    Tk_PhotoHandle photo;	/* Photo to write color image into */
{
    Tk_PhotoImageBlock dest;
    int width, height;

    width = ColorImageWidth(image);
    height = ColorImageHeight(image);

    Tk_PhotoGetImage(photo, &dest);
    dest.pixelSize = sizeof(Pix32);
    dest.pitch = sizeof(Pix32) * width;
    dest.width = width;
    dest.height = height;
    dest.offset[0] = Tk_Offset(Pix32, Red);
    dest.offset[1] = Tk_Offset(Pix32, Green);
    dest.offset[2] = Tk_Offset(Pix32, Blue);
    dest.pixelPtr = (unsigned char *)ColorImageData(image);
    Tk_PhotoSetSize(photo, width, height);
    Tk_PhotoPutBlock(photo, &dest, 0, 0, width, height);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ColorImageToPhoto --
 *
 *      Create a color image from a Tk photo.
 *
 * Results:
 *      The new color image is returned.
 *
 *----------------------------------------------------------------------
 */
ColorImage
Blt_PhotoToColorImage(photo, regionPtr)
    Tk_PhotoHandle photo;	/* Source photo image to scale */
    ImageRegion *regionPtr;	/* Region of source image to copy */
{
    Tk_PhotoImageBlock src;
    ColorImage image;
    register Pix32 *destPtr;
    register unsigned char *srcPtr;
    register int regionOffset;
    register int x, y;

    Tk_PhotoGetImage(photo, &src);
    if (regionPtr->width < 1) {
	regionPtr->width = src.width;
    }
    if (regionPtr->height < 1) {
	regionPtr->height = src.height;
    }
    image = Blt_CreateColorImage(regionPtr->width, regionPtr->height);
    regionOffset = (regionPtr->x * src.pixelSize) + (regionPtr->y * src.pitch);
    destPtr = ColorImageData(image);
    for (y = 0; y < regionPtr->height; y++) {
	srcPtr = src.pixelPtr + regionOffset;
	for (x = 0; x < regionPtr->width; x++, destPtr++) {
	    destPtr->Red = srcPtr[src.offset[0]];
	    destPtr->Green = srcPtr[src.offset[1]];
	    destPtr->Blue = srcPtr[src.offset[2]];
	    srcPtr += src.pixelSize;
	}
	regionOffset += src.pitch;
    }
    return image;
}

#ifdef WIN32

/*
 *----------------------------------------------------------------------
 *
 * Blt_ColorImageToPixmap --
 *
 *      Converts a color image into a pixmap.
 *
 *	Right now this only handles TrueColor visuals.
 *
 * Results:
 *      The new pixmap is returned.
 *
 *----------------------------------------------------------------------
 */
Pixmap
Blt_ColorImageToPixmap(interp, tkwin, image, colorTablePtr)
    Tcl_Interp *interp;
    Tk_Window tkwin;
    ColorImage image;
    ColorTable *colorTablePtr;	/* Points to array of colormap indices */
{
    HDC pixmapDC;
    TkWinDCState state;
    Display *display;
    int width, height, depth;
    Pixmap pixmap;
    register int x, y;
    register Pix32 *srcPtr;
    COLORREF rgb;

    *colorTablePtr = NULL;
    width = ColorImageWidth(image);
    height = ColorImageHeight(image);
    display = Tk_Display(tkwin);
    depth = Tk_Depth(tkwin);

    pixmap = Tk_GetPixmap(display, Tk_WindowId(tkwin), width, height, depth);
    pixmapDC = TkWinGetDrawableDC(display, pixmap, &state);

    srcPtr = ColorImageData(image);
    for (y = 0; y < height; y++) {
	for (x = 0; x < width; x++, srcPtr++) {
	    rgb = PALETTERGB(srcPtr->Red, srcPtr->Green, srcPtr->Blue);
	    SetPixelV(pixmapDC, x, y, rgb);
	}
    }
    TkWinReleaseDrawableDC(pixmap, pixmapDC, &state);
    return pixmap;
}

#else

int redAdjust, greenAdjust, blueAdjust;
int redMaskShift, greenMaskShift, blueMaskShift;

/*
 *----------------------------------------------------------------------
 *
 * ShiftCount --
 *
 *	Returns the position of the least significant (low) bit in
 *	the given mask.
 *
 *	For TrueColor and DirectColor visuals, a pixel value is
 *	formed by OR-ing the red, green, and blue colormap indices
 *	into a single 32-bit word.  The visual's color masks tell
 *	you where in the word the indices are supposed to be.  The
 *	masks contain bits only where the index is found.  By counting
 *	the leading zeros in the mask, we know how many bits to shift
 *	to the individual red, green, and blue values to form a pixel.
 *
 * Results:
 *      The number of the least significant bit.
 *
 *----------------------------------------------------------------------
 */
static int
ShiftCount(mask)
    register unsigned int mask;
{
    register int count;

    for (count = 0; count < 32; count++) {
	if (mask & 0x01) {
	    break;
	}
	mask >>= 1;
    }
    return count;
}

/*
 *----------------------------------------------------------------------
 *
 * CountBits --
 *
 *	Returns the number of bits set in the given mask.
 *
 * Results:
 *      The number of bits to set in the mask.
 *
 *----------------------------------------------------------------------
 */
static int
CountBits(mask)
    register unsigned int mask;
{
    register int count;

    count = 0;
    while (mask != 0L) {
	if (mask & 0x01) {
	    count++;
	}
	mask >>= 1;		/* Keep shifting off bits until there
				 * are no more left. */
    }
    return count;
}

static void
ComputeMasks(visualPtr)
    Visual *visualPtr;
{
    int count;

    redMaskShift = ShiftCount((unsigned int)visualPtr->red_mask);
    greenMaskShift = ShiftCount((unsigned int)visualPtr->green_mask);
    blueMaskShift = ShiftCount((unsigned int)visualPtr->blue_mask);

    redAdjust = greenAdjust = blueAdjust = 0;
    count = CountBits((unsigned int)visualPtr->red_mask);
    if (count < 8) {
	redAdjust = 8 - count;
    }
    count = CountBits((unsigned int)visualPtr->green_mask);
    if (count < 8) {
	greenAdjust = 8 - count;
    }
    count = CountBits((unsigned int)visualPtr->blue_mask);
    if (count < 8) {
	blueAdjust = 8 - count;
    }
#ifdef notdef
    fprintf(stderr, "adjust r=%d,g=%d,b=%d shift r=%d,g=%d,b=%d\n",
	redAdjust, greenAdjust, blueAdjust, redMaskShift, greenMaskShift,
	blueMaskShift);
#endif
}

/*
 *----------------------------------------------------------------------
 *
 * TrueColorPixel --
 *
 *      Computes a pixel index from the 3 component RGB values.
 *
 * Results:
 *      The pixel index is returned.
 *
 *----------------------------------------------------------------------
 */
static INLINE unsigned int
TrueColorPixel(visualPtr, pixelPtr)
    Visual *visualPtr;
    Pix32 *pixelPtr;
{
    unsigned int red, green, blue;

    /*
     * The number of bits per color may be less than eight. For example,
     * 15/16 bit displays (hi-color) use only 5 bits, 8-bit displays
     * use 2 or 3 bits (don't ask me why you'd have an 8-bit TrueColor
     * display). So shift off the least significant bits.
     */
    red = ((unsigned int)pixelPtr->Red >> redAdjust);
    green = ((unsigned int)pixelPtr->Green >> greenAdjust);
    blue = ((unsigned int)pixelPtr->Blue >> blueAdjust);

    /* Shift each color into the proper location of the pixel index. */
    red = (red << redMaskShift) & visualPtr->red_mask;
    green = (green << greenMaskShift) & visualPtr->green_mask;
    blue = (blue << blueMaskShift) & visualPtr->blue_mask;
    return (red | green | blue);
}

/*
 *----------------------------------------------------------------------
 *
 * DirectColorPixel --
 *
 *      Translates the 3 component RGB values into a pixel index.
 *      This differs from TrueColor only in that it first translates
 *	the RGB values through a color table.
 *
 * Results:
 *      The pixel index is returned.
 *
 *----------------------------------------------------------------------
 */
static INLINE unsigned int
DirectColorPixel(colorTabPtr, pixelPtr)
    struct ColorTable *colorTabPtr;
    Pix32 *pixelPtr;
{
    unsigned int red, green, blue;

    red = colorTabPtr->red[pixelPtr->Red];
    green = colorTabPtr->green[pixelPtr->Green];
    blue = colorTabPtr->blue[pixelPtr->Blue];
    return (red | green | blue);
}

/*
 *----------------------------------------------------------------------
 *
 * PseudoColorPixel --
 *
 *      Translates the 3 component RGB values into a pixel index.
 *      This differs from TrueColor only in that it first translates
 *	the RGB values through a color table.
 *
 * Results:
 *      The pixel index is returned.
 *
 *----------------------------------------------------------------------
 */
static INLINE unsigned int
PseudoColorPixel(pixelPtr, lut)
    Pix32 *pixelPtr;
    unsigned int *lut;
{
    int red, green, blue;
    int pixel;

    red = (pixelPtr->Red >> 3) + 1;
    green = (pixelPtr->Green >> 3) + 1;
    blue = (pixelPtr->Blue >> 3) + 1;
    pixel = RGBIndex(red, green, blue);
    return lut[pixel];
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ColorImageToPixmap --
 *
 *      Converts a color image into a pixmap.
 *
 *	Right now this only handles TrueColor visuals.
 *
 * Results:
 *      The new pixmap is returned.
 *
 *----------------------------------------------------------------------
 */
Pixmap
Blt_ColorImageToPixmap(interp, tkwin, image, colorTablePtr)
    Tcl_Interp *interp;
    Tk_Window tkwin;
    ColorImage image;
    ColorTable *colorTablePtr;	/* Points to array of colormap indices */
{
    Display *display;
    int width, height;
    Pixmap pixmap;
    GC pixmapGC;
    Visual *visualPtr;
    XImage *imagePtr;
    int numPixels;

    visualPtr = Tk_Visual(tkwin);
    width = ColorImageWidth(image);
    height = ColorImageHeight(image);
    display = Tk_Display(tkwin);

    ComputeMasks(visualPtr);

    *colorTablePtr = NULL;
    imagePtr = XCreateImage(Tk_Display(tkwin), visualPtr, Tk_Depth(tkwin),
	ZPixmap, 0, (char *)NULL, width, height, 32, 0);
    assert(imagePtr);

    numPixels = width * height;
    imagePtr->data = (char *)malloc(sizeof(Pix32) * numPixels);
    assert(imagePtr->data);

    imagePtr->byte_order = MSBFirst;	/* Force the byte order */
    imagePtr->bitmap_bit_order = imagePtr->byte_order;
    imagePtr->bytes_per_line = width * sizeof(Pix32);

    switch (visualPtr->class) {
    case TrueColor:
	{
	    register int x, y;
	    register Pix32 *srcPtr;
	    register char *destPtr;
	    unsigned int pixel;
	    int rowOffset;

	    /*
	     * Compute the colormap locations directly from pixel RGB values.
	     */
	    srcPtr = ColorImageData(image);
	    rowOffset = 0;
	    for (y = 0; y < height; y++) {
		destPtr = imagePtr->data + rowOffset;
		for (x = 0; x < width; x++, srcPtr++) {
		    pixel = TrueColorPixel(visualPtr, srcPtr);
		    switch (imagePtr->bits_per_pixel) {
		    case 32:
			*destPtr++ = (pixel >> 24) & 0xFF;
			/*FALLTHRU*/
		    case 24:
			*destPtr++ = (pixel >> 16) & 0xFF;
			/*FALLTHRU*/
		    case 16:
			*destPtr++ = (pixel >> 8) & 0xFF;
			/*FALLTHRU*/
		    case 8:
			*destPtr++ = pixel & 0xFF;
			/*FALLTHRU*/
		    }
		}
		rowOffset += imagePtr->bytes_per_line;
	    }
	}
	break;

    case DirectColor:
	{
	    register int x, y;
	    register Pix32 *srcPtr;
	    register char *destPtr;
	    unsigned int pixel;
	    int rowOffset;
	    struct ColorTable *colorTabPtr;

	    /* Build a color table first */
	    colorTabPtr = Blt_DirectColorTable(interp, tkwin, image);

	    /*
	     * Compute the colormap locations directly from pixel RGB values.
	     */
	    srcPtr = ColorImageData(image);
	    rowOffset = 0;
	    for (y = 0; y < height; y++) {
		destPtr = imagePtr->data + rowOffset;
		for (x = 0; x < width; x++, srcPtr++) {
		    pixel = DirectColorPixel(colorTabPtr, srcPtr);
		    switch (imagePtr->bits_per_pixel) {
		    case 32:
			*destPtr++ = (pixel >> 24) & 0xFF;
			/*FALLTHRU*/
		    case 24:
			*destPtr++ = (pixel >> 16) & 0xFF;
			/*FALLTHRU*/
		    case 16:
			*destPtr++ = (pixel >> 8) & 0xFF;
			/*FALLTHRU*/
		    case 8:
			*destPtr++ = pixel & 0xFF;
			/*FALLTHRU*/
		    }
		}
		rowOffset += imagePtr->bytes_per_line;
	    }
	    *colorTablePtr = colorTabPtr;
	}
	break;

    case GrayScale:
    case StaticGray:
    case PseudoColor:
    case StaticColor:
	{
	    register int x, y;
	    register Pix32 *srcPtr;
	    register char *destPtr;
	    unsigned int pixel;
	    int rowOffset;
	    struct ColorTable *colorTabPtr;

	    colorTabPtr = Blt_PseudoColorTable(interp, tkwin, image);

	    srcPtr = ColorImageData(image);
	    rowOffset = 0;
	    for (y = 0; y < height; y++) {
		destPtr = imagePtr->data + rowOffset;
		for (x = 0; x < width; x++, srcPtr++) {
		    pixel = PseudoColorPixel(srcPtr, colorTabPtr->lut);
		    switch (imagePtr->bits_per_pixel) {
		    case 32:
			*destPtr++ = (pixel >> 24) & 0xFF;
			/*FALLTHRU*/
		    case 24:
			*destPtr++ = (pixel >> 16) & 0xFF;
			/*FALLTHRU*/
		    case 16:
			*destPtr++ = (pixel >> 8) & 0xFF;
			/*FALLTHRU*/
		    case 8:
			*destPtr++ = pixel & 0xFF;
			/*FALLTHRU*/
		    }
		}
		rowOffset += imagePtr->bytes_per_line;
	    }
	    free((char *)colorTabPtr->lut);
	    *colorTablePtr = colorTabPtr;
	}
	break;
    default:
	return None;		/* Bad or unknown visual class. */
    }

    pixmapGC = Tk_GetGC(tkwin, 0L, (XGCValues *)NULL);
    pixmap = Tk_GetPixmap(display, Tk_WindowId(tkwin), width, height,
	Tk_Depth(tkwin));
    XPutImage(display, pixmap, pixmapGC, imagePtr, 0, 0, 0, 0, width, height);
    XDestroyImage(imagePtr);
    Tk_FreeGC(display, pixmapGC);
    return pixmap;
}

#endif /* WIN32 */
/*
 *	filter function definitions
 */

#ifdef __STDC__
static ImageFilterProc DefaultFilter;
static ImageFilterProc BellFilter;
static ImageFilterProc BesselFilter;
static ImageFilterProc BoxFilter;
static ImageFilterProc BSplineFilter;
static ImageFilterProc CatromFilter;
static ImageFilterProc DummyFilter;
static ImageFilterProc GaussianFilter;
static ImageFilterProc Lanczos3Filter;
static ImageFilterProc MitchellFilter;
static ImageFilterProc SincFilter;
static ImageFilterProc TriangleFilter;
#endif

static double
DefaultFilter(x)
    double x;
{
    if (x < 0.0) {
	x = -x;
    }
    if (x < 1.0) {
	/* f(x) = 2|x|^3 - 3|x|^2 + 1, -1 <= x <= 1 */
	return (2.0 * x - 3.0) * x * x + 1.0;
    }
    return 0.0;
}

/* Just for testing */
static double
DummyFilter(x)
    double x;
{
    return FABS(x);
}

/*
 *
 * Finite filters in increasing order:
 *	Box (constant)
 *	Triangle (linear)
 *	Bell
 *	BSpline (cubic)
 *
 */
static double
BoxFilter(x)
    double x;
{
    if (x < -0.5) {
	return 0.0;
    }
    if (x <= 0.5) {
	return 1.0;
    }
    return 0.0;
}

static double
TriangleFilter(x)
    double x;
{
    if (x < 0.0) {
	x = -x;
    }
    if (x < 1.0) {
	return (1.0 - x);
    }
    return 0.0;
}

static double
BellFilter(x)
    double x;
{
    if (x < 0) {
	x = -x;
    }
    if (x < 0.5) {
	return (0.75 - (x * x));
    }
    if (x < 1.5) {
	x = (x - 1.5);
	return (0.5 * (x * x));
    }
    return 0.0;
}

static double
BSplineFilter(x)
    double x;
{
    double x2;

    if (x < 0) {
	x = -x;
    }
    if (x < 1) {
	x2 = x * x;
	return ((.5 * x2 * x) - x2 + (2.0 / 3.0));
    } else if (x < 2) {
	x = 2 - x;
	return ((x * x * x) / 6.0);
    }
    return 0.0;
}

/*
 *
 * Infinite Filters:
 *	Sinc		perfect lowpass filter
 *	Bessel		circularly symmetric 2-D filter
 *	Gaussian
 *	Lanczos3
 *	Mitchell
 */

static double
SincFilter(x)
    double x;
{
    if (x == 0.0) {
	return 1.0;
    }
    x *= M_PI;
    return (sin(x) / x);
}

static double
BesselFilter(x)
    double x;
{
    extern double j1 _ANSI_ARGS_((double value));
    /*
     * See Pratt "Digital Image Processing" p. 97 for Bessel functions
     * zeros are at approx x=1.2197, 2.2331, 3.2383, 4.2411, 5.2428, 6.2439,
     * 7.2448, 8.2454
     */
    return (x == 0.0) ? M_PI / 4.0 : j1(M_PI * x) / (x + x);
}

#define SQRT_2PI		0.79788456080286541	/* sqrt(2.0 / M_PI) */

static double
GaussianFilter(x)
    double x;
{
    return exp(-2.0 * x * x) * SQRT_2PI;
}

static double
Lanczos3Filter(x)
    double x;
{
    if (x < 0) {
	x = -x;
    }
    if (x < 3.0) {
	return (SincFilter(x) * SincFilter(x / 3.0));
    }
    return 0.0;
}

#define	B	0.3333333333333333	/* (1.0 / 3.0) */
#define	C	0.3333333333333333	/* (1.0 / 3.0) */

static double
MitchellFilter(x)
    double x;
{
    double x2;

    x2 = x * x;
    if (x < 0) {
	x = -x;
    }
    if (x < 1.0) {
	x = (((12.0 - 9.0 * B - 6.0 * C) * (x * x2)) +
	    ((-18.0 + 12.0 * B + 6.0 * C) * x2) + (6.0 - 2 * B));
	return (x / 6.0);
    } else if (x < 2.0) {
	x = (((-1.0 * B - 6.0 * C) * (x * x2)) + ((6.0 * B + 30.0 * C) * x2) +
	    ((-12.0 * B - 48.0 * C) * x) + (8.0 * B + 24 * C));
	return (x / 6.0);
    }
    return 0.0;
}

/*
 * Catmull-Rom spline
 */
static double
CatromFilter(x)
    double x;
{
    if (x < -2.) {
	return 0.0;
    }
    if (x < -1.0) {
	return 0.5 * (4.0 + x * (8.0 + x * (5.0 + x)));
    }
    if (x < 0.0) {
	return 0.5 * (2.0 + x * x * (-5.0 + x * -3.0));
    }
    if (x < 1.0) {
	return 0.5 * (2.0 + x * x * (-5.0 + x * 3.0));
    }
    if (x < 2.0) {
	return 0.5 * (4.0 + x * (-8.0 + x * (5.0 - x)));
    }
    return 0.0;
}

typedef struct FilterInfo {
    char *name;
    int numChars;
    struct Image1DFilter filter;
} FilterInfo;

static FilterInfo filterTable[] =
{
 /*
     * Name          # chars   support  function
     */
    {"bell", 3,
	{1.5, BellFilter},},
    {"box", 2,
	{0.5, BoxFilter},},
    {"bessel", 3,
	{(float)3.2383, BesselFilter},},
    {"bspline", 2,
	{2.0, BSplineFilter},},
    {"catrom", 1,
	{2.0, CatromFilter},},
    {"default", 2,
	{1.0, DefaultFilter},},
    {"dummy", 2,
	{0.5, DummyFilter},},
    {"gaussian", 1,
	{4.0, GaussianFilter},},
    {"lanczos3", 1,
	{3.0, Lanczos3Filter},},
    {"mitchell", 1,
	{2.0, MitchellFilter},},
    {"none", 3,
	{0.0, (ImageFilterProc *)NULL},},
    {"sinc", 1,
	{2.0, SincFilter},},
    {"triangle", 1,
	{1.0, TriangleFilter},},
};

static int numFilters = sizeof(filterTable) / sizeof(FilterInfo);

Image1DFilter bltBoxFilter = &(filterTable[1].filter);


/*
 *----------------------------------------------------------------------
 *
 * Blt_FindImage1DFilter --
 *
 *      Finds a 1-D filter associated by the given filter name.
 *
 * Results:
 *      A standard Tcl result.  Returns TCL_OK is the filter was
 *	found.  The filter information (proc and support) is returned
 *	via filterPtr. Otherwise TCL_ERROR is returned and an error
 *	message is left in interp->result.
 *
 *----------------------------------------------------------------------
 */
int
Blt_FindImage1DFilter(interp, name, filterPtr)
    Tcl_Interp *interp;
    char *name;
    Image1DFilter *filterPtr;
{
    char c;
    int length;
    register int i;
    register FilterInfo *infoPtr;

    c = name[0];
    length = strlen(name);
    for (infoPtr = filterTable, i = 0; i < numFilters; i++, infoPtr++) {
	if ((c == infoPtr->name[0]) && (length >= infoPtr->numChars) &&
	    (strncmp(name, infoPtr->name, length) == 0)) {
	    if (infoPtr->filter.proc == NULL) {
		*filterPtr = NULL;
	    } else {
		*filterPtr = &(infoPtr->filter);
	    }
	    return TCL_OK;
	}
    }
    Tcl_AppendResult(interp, "can't find filter \"", name, "\"", (char *)NULL);
    return TCL_ERROR;
}


typedef struct {
    int pixel;
    float weight;
} Sample;

typedef struct {
    int count;			/* Number of contributors */
    Sample *sampleArr;		/* Array of contributors */
} Contribution;


static void
ComputeContributors(srcWidth, destWidth, filterPtr, contribPtrPtr,
    samplePtrPtr)
    int srcWidth, destWidth;
    struct Image1DFilter *filterPtr;
    Contribution **contribPtrPtr;
    Sample **samplePtrPtr;
{
    Contribution *contribArr;
    Sample *samplePool, *sampleArr;
    double scale;
    int sampleSize;
    double center;
    register Contribution *contribPtr;
    register Sample *samplePtr;
    register int x, i;
    register int left, right;	/* filter bounds */
    float factor, sum;

    /* Pre-calculate filter contributions for a row */
    contribArr = (Contribution *)calloc(destWidth, sizeof(Contribution));
    assert(contribArr);

    scale = (double)destWidth / (double)srcWidth;

    if (scale < 1.0) {
	double width, fscale;

	/* Downsample */

	width = filterPtr->support / scale;
	fscale = 1.0 / scale;
	sampleSize = (int)(width * 2 + 2);
	samplePool = (Sample *)malloc(sampleSize * destWidth * sizeof(Sample));
	sampleArr = samplePool;
	for (contribPtr = contribArr, x = 0; x < destWidth; x++, contribPtr++) {
	    center = (double)x *fscale;

	    /* Determine bounds of filter and its density */
	    left = (int)(center - width + 0.5);
	    if (left < 0) {
		left = 0;
	    }
	    right = (int)(center + width + 0.5);
	    if (right >= srcWidth) {
		right = srcWidth - 1;
	    }
	    sum = 0.0;
	    for (samplePtr = sampleArr, i = left; i <= right;
		i++, samplePtr++) {
		samplePtr->weight = (float)
		    (*filterPtr->proc) (((double)i + 0.5 - center) * scale);
		sum += samplePtr->weight;
	    }
	    factor = (float)((sum == 0.0) ? 1.0 : (1.0 / sum));
	    for (samplePtr = sampleArr, i = left; i <= right;
		i++, samplePtr++) {
		samplePtr->pixel = i;
		samplePtr->weight *= factor;
	    }
	    contribPtr->count = right - left + 1;
	    contribPtr->sampleArr = sampleArr;
	    sampleArr += sampleSize;
	}
    } else {
	/* Upsample */

	sampleSize = (int)(filterPtr->support * 2 + 2);
	samplePool = (Sample *)malloc(sampleSize * destWidth * sizeof(Sample));
	sampleArr = samplePool;

	for (contribPtr = contribArr, x = 0; x < destWidth; x++, contribPtr++) {
	    center = (double)x / scale;
	    left = (int)(center - filterPtr->support + 0.5);
	    if (left < 0) {
		left = 0;
	    }
	    right = (int)(center + filterPtr->support + 0.5);
	    if (right >= srcWidth) {
		right = srcWidth - 1;
	    }
	    sum = 0.0;
	    for (samplePtr = sampleArr, i = left; i <= right;
		i++, samplePtr++) {
		samplePtr->weight =
		    (float)(*filterPtr->proc) ((double)i + 0.5 - center);
		sum += samplePtr->weight;
	    }
	    factor = (float)((sum == 0.0) ? 1.0 : (1.0 / sum));
	    for (samplePtr = sampleArr, i = left; i <= right;
		i++, samplePtr++) {
		samplePtr->pixel = i;
		samplePtr->weight *= factor;
	    }
	    contribPtr->count = right - left + 1;
	    contribPtr->sampleArr = sampleArr;
	    sampleArr += sampleSize;
	}
    }
    *samplePtrPtr = samplePool;
    *contribPtrPtr = contribArr;
}

static ColorImage
ZoomImageVertically(srcImage, srcRegionPtr, destRegionPtr, filter)
    ColorImage srcImage;
    ImageRegion *srcRegionPtr;	/* Region of source image to resample */
    ImageRegion *destRegionPtr;	/* Region of destination image to redraw */
    Image1DFilter filter;
{
    Contribution *contribArr, *contribPtr;
    register Sample *samplePtr;
    Sample *sampleArr;
    register int x, y, i;
    ColorImage destImage;
    register Pix32 *srcPtr, *destPtr;
    float red, green, blue;
    Pix32 *pixelPtr;
    int destOffset;
    int srcWidth, srcHeight;
    int destWidth, destHeight;

    srcWidth = srcRegionPtr->width;
    srcHeight = srcRegionPtr->height;
    destWidth = destRegionPtr->width;
    destHeight = destRegionPtr->height;

    destImage = Blt_CreateColorImage(destWidth, destHeight);

    /* Pre-calculate filter contributions for a row */
    ComputeContributors(srcHeight, destHeight, filter, &contribArr, &sampleArr);

    /* Apply filter to zoom vertically from tmp to destination */
    for (x = 0; x < srcWidth; x++) {
	destOffset = x;
	srcPtr = ColorImageData(srcImage) + x;
	for (contribPtr = contribArr, y = 0; y < destHeight; y++,
	    contribPtr++) {
	    red = green = blue = 0.0;
	    for (samplePtr = contribPtr->sampleArr, i = 0;
		i < contribPtr->count; i++, samplePtr++) {
		pixelPtr = srcPtr + (samplePtr->pixel * srcWidth);
		red += (float)pixelPtr->Red * samplePtr->weight;
		green += (float)pixelPtr->Green * samplePtr->weight;
		blue += (float)pixelPtr->Blue * samplePtr->weight;
	    }
	    destPtr = ColorImageData(destImage) + destOffset;
	    destPtr->Red = (unsigned char)CLAMP(red);
	    destPtr->Green = (unsigned char)CLAMP(green);
	    destPtr->Blue = (unsigned char)CLAMP(blue);
	    destOffset += destWidth;
	}
    }

    /* Free the memory allocated for filter weights */
    free((char *)sampleArr);
    free((char *)contribArr);

    return destImage;
}

static ColorImage
ZoomImageHorizontally(srcImage, srcRegionPtr, destRegionPtr, filter)
    ColorImage srcImage;
    ImageRegion *srcRegionPtr;	/* Region of source image to resample */
    ImageRegion *destRegionPtr;	/* Region of destination image to redraw */
    Image1DFilter filter;
{
    Contribution *contribArr, *contribPtr;
    register Sample *samplePtr;
    Sample *sampleArr;
    register int x, y, i;
    ColorImage destImage;
    register Pix32 *srcPtr, *destPtr;
    float red, green, blue;
    Pix32 *pixelPtr;
    int srcOffset, destOffset;
    int srcWidth, srcHeight;
    int destWidth, destHeight;

    srcWidth = srcRegionPtr->width;
    srcHeight = srcRegionPtr->height;
    destWidth = destRegionPtr->width;
    destHeight = destRegionPtr->height;

    destImage = Blt_CreateColorImage(destWidth, destHeight);

    /* Pre-calculate filter contributions for a row */
    ComputeContributors(srcWidth, destWidth, filter, &contribArr, &sampleArr);

    /* Apply filter to zoom horizontally from srcPtr to tmpPixels */
    srcOffset = srcRegionPtr->x + (srcRegionPtr->y * srcWidth);
    destOffset = 0;
    for (y = 0; y < srcHeight; y++) {
	srcPtr = ColorImageData(srcImage) + srcOffset;
	destPtr = ColorImageData(destImage) + destOffset;
	for (contribPtr = contribArr, x = 0; x < destWidth;
	    x++, destPtr++, contribPtr++) {
	    red = green = blue = 0.0;
	    for (samplePtr = contribPtr->sampleArr, i = 0;
		i < contribPtr->count; i++, samplePtr++) {
		pixelPtr = srcPtr + samplePtr->pixel;
		red += (float)pixelPtr->Red * samplePtr->weight;
		green += (float)pixelPtr->Green * samplePtr->weight;
		blue += (float)pixelPtr->Blue * samplePtr->weight;
	    }
	    destPtr->Red = (unsigned char)CLAMP(red);
	    destPtr->Green = (unsigned char)CLAMP(green);
	    destPtr->Blue = (unsigned char)CLAMP(blue);
	}
	destOffset += destWidth;
	srcOffset += srcWidth;
    }
    /* free the memory allocated for horizontal filter weights */
    free((char *)sampleArr);
    free((char *)contribArr);

    return destImage;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ResampleImage --
 *
 *      Resamples a given color image using 1-D filters and returns
 *	a new color image of the designated size.
 *
 * Results:
 *      Returns the resampled color image. The original color image
 *	is left intact.
 *
 *----------------------------------------------------------------------
 */
ColorImage
Blt_ResampleColorImage(image, srcRegionPtr, destRegionPtr, horzFilter,
    vertFilter)
    ColorImage image;
    ImageRegion *srcRegionPtr;	/* Region of source image to resample */
    ImageRegion *destRegionPtr;	/* Region of destination image to redraw */
    Image1DFilter horzFilter, vertFilter;
{
    ColorImage tmpImage, destImage;
    ImageRegion tmpRegion;
    int count1, count2;
    int vertSupport, horzSupport;

    vertSupport = (int)ceil(vertFilter->support * 2.0);
    horzSupport = (int)ceil(horzFilter->support * 2.0);

    tmpRegion.x = tmpRegion.y = 0;

    /* Determine if it's cheaper to first zoom vertically or horizontally */
    count1 = destRegionPtr->width * (srcRegionPtr->height *
	horzSupport + destRegionPtr->height * vertSupport);
    count2 = destRegionPtr->height * (srcRegionPtr->width *
	vertSupport + destRegionPtr->width * horzSupport);
    if (count1 < count2) {
	tmpRegion.width = srcRegionPtr->width;
	tmpRegion.height = destRegionPtr->height;
	tmpImage = ZoomImageVertically(image, srcRegionPtr, &tmpRegion,
	    vertFilter);
	destImage = ZoomImageHorizontally(tmpImage, &tmpRegion, destRegionPtr,
	    horzFilter);
    } else {
	tmpRegion.height = srcRegionPtr->height;
	tmpRegion.width = destRegionPtr->width;
	tmpImage = ZoomImageHorizontally(image, srcRegionPtr, &tmpRegion,
	    horzFilter);
	destImage = ZoomImageVertically(tmpImage, &tmpRegion, destRegionPtr,
	    vertFilter);
    }
    Blt_FreeColorImage(tmpImage);
    return destImage;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ResamplePhoto --
 *
 *      Resamples a Tk photo image using 1-D filters and returns
 *	writes the new image into another Tk photo.  It is possible
 *	for the source and destination to be the same photos.
 *
 * Results:
 *      The designated destination photo will contain the resampled
 *	color image. The original photo is left intact.
 *
 *----------------------------------------------------------------------
 */
void
Blt_ResamplePhoto(srcPhoto, destPhoto, srcRegionPtr, horzFilter, vertFilter)
    Tk_PhotoHandle srcPhoto;	/* Source photo image to scale */
    Tk_PhotoHandle destPhoto;	/* Resulting scaled photo image */
    ImageRegion *srcRegionPtr;	/* Region of source image to resample */
    Image1DFilter horzFilter, vertFilter;
{
    ColorImage image, destImage;
    Tk_PhotoImageBlock src, dest;
    ImageRegion srcRegion, destRegion;

    Tk_PhotoGetImage(srcPhoto, &src);
    Tk_PhotoGetImage(destPhoto, &dest);

    if (srcRegionPtr->width < 1) {
	srcRegionPtr->width = src.width;
    }
    if (srcRegionPtr->height < 1) {
	srcRegionPtr->height = src.height;
    }
    image = Blt_PhotoToColorImage(srcPhoto, srcRegionPtr);
    srcRegion.x = srcRegion.y = 0;
    srcRegion.width = srcRegionPtr->width;
    srcRegion.height = srcRegionPtr->height;
    destRegion.x = destRegion.y = 0;
    destRegion.width = dest.width;
    destRegion.height = dest.height;
    if (destRegion.width < 1) {
	destRegion.width = srcRegionPtr->width;
    }
    if (destRegion.height < 1) {
	destRegion.height = srcRegionPtr->height;
    }
    destImage = Blt_ResampleColorImage(image, &srcRegion, &destRegion,
	horzFilter, vertFilter);
    Blt_FreeColorImage(image);
    Blt_ColorImageToPhoto(destImage, destPhoto);
    Blt_FreeColorImage(destImage);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ResizePhoto --
 *
 *	Scales the region of the source image to the size of the
 *	destination image.  This routine performs raw scaling of
 *	the image and unlike Blt_ResamplePhoto does not handle
 *	aliasing effects from subpixel sampling. It is possible
 *	for the source and destination to be the same photos.
 *
 * Results:
 *      The designated destination photo will contain the resampled
 *	color image. The original photo is left intact.
 *
 *----------------------------------------------------------------------
 */
void
Blt_ResizePhoto(srcPhoto, destPhoto, srcRegionPtr)
    Tk_PhotoHandle srcPhoto;	/* Source photo image to scale */
    Tk_PhotoHandle destPhoto;	/* Resulting scaled photo image */
    ImageRegion *srcRegionPtr;	/* Region of source image to be scaled */
{
    register int x, y;
    register int sx, sy;
    float scaleX, scaleY;
    ColorImage destImage;
    Pix32 *destPtr;
    Tk_PhotoImageBlock src, dest;
    unsigned char *srcPtr;
    int offset;

    Tk_PhotoGetImage(srcPhoto, &src);
    Tk_PhotoGetImage(destPhoto, &dest);

    if (srcRegionPtr->width < 1) {
	srcRegionPtr->width = src.width;
    }
    if (srcRegionPtr->height < 1) {
	srcRegionPtr->height = src.height;
    }
    destImage = Blt_CreateColorImage(dest.width, dest.height);

    scaleX = (float)srcRegionPtr->width / (float)dest.width;
    scaleY = (float)srcRegionPtr->height / (float)dest.height;

    destPtr = ColorImageData(destImage);
    for (y = 0; y < dest.height; y++) {
	sy = (int)(scaleY * (float)y + 0.5);
	if (sy > (int)(srcRegionPtr->height)) {
	    continue;
	}
	offset = ((sy + srcRegionPtr->y) * src.pitch) +
	    (srcRegionPtr->x * src.pixelSize);
	for (x = 0; x < dest.width; x++) {
	    sx = (int)(scaleX * (float)x + 0.5);
	    if (sx > (int)(srcRegionPtr->width)) {
		continue;
	    }
	    srcPtr = src.pixelPtr + offset + (sx * src.pixelSize);
	    destPtr->Red = srcPtr[src.offset[0]];
	    destPtr->Green = srcPtr[src.offset[1]];
	    destPtr->Blue = srcPtr[src.offset[2]];
	    destPtr++;
	}
    }
    Blt_ColorImageToPhoto(destImage, destPhoto);
    Blt_FreeColorImage(destImage);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ResizeColorImage --
 *
 *	Scales the region of the source image to the size of the
 *	destination image.  This routine performs raw scaling of
 *	the image and unlike Blt_ResamplePhoto does not compensate
 *	for aliasing effects from subpixel sampling.
 *
 * Results:
 *      Returns the new resized color image.  The original image
 *	is left intact.
 *
 *----------------------------------------------------------------------
 */
ColorImage
Blt_ResizeColorImage(image, srcRegionPtr, destRegionPtr)
    ColorImage image;		/* Source photo image to scale */
    ImageRegion *srcRegionPtr;	/* Region of source image to be scaled */
    ImageRegion *destRegionPtr;	/* Region of destination image */
{
    register int x, y;
    register int sx, sy;
    float scaleX, scaleY;
    ColorImage destImage;
    Pix32 *srcPtr, *destPtr;
    int offset;

    if (srcRegionPtr->width < 1) {
	srcRegionPtr->width = ColorImageWidth(image);
    }
    if (srcRegionPtr->height < 1) {
	srcRegionPtr->height = ColorImageHeight(image);
    }
    destImage = Blt_CreateColorImage(destRegionPtr->width,
	destRegionPtr->height);

    scaleX = (float)srcRegionPtr->width / (float)destRegionPtr->width;
    scaleY = (float)srcRegionPtr->height / (float)destRegionPtr->height;

    destPtr = ColorImageData(destImage);
    for (y = 0; y < destRegionPtr->height; y++) {
	sy = (int)(scaleY * (float)y + 0.5);
	if (sy > (int)(srcRegionPtr->height)) {
	    continue;
	}
	offset = ((sy + srcRegionPtr->y) * ColorImageWidth(image)) +
	    srcRegionPtr->x;
	for (x = 0; x < destRegionPtr->width; x++) {
	    sx = (int)(scaleX * (float)x + 0.5);
	    if (sx > (int)(srcRegionPtr->width)) {
		continue;
	    }
	    srcPtr = ColorImageData(image) + offset + sx;
	    destPtr->Red = srcPtr->Red;
	    destPtr->Green = srcPtr->Green;
	    destPtr->Blue = srcPtr->Blue;
	    /* Don't copy alpha-channel information. */
	    destPtr++;
	}
    }
    return destImage;
}

/*
 * FIXME: Boundary handling could be better (pixels are replicated).
 *	  It's slow. Take boundary tests out of inner loop.
 */
ColorImage
Blt_ConvolveColorImage(srcImage, filterPtr)
    ColorImage srcImage;
    Image2DFilter *filterPtr;
{
    ColorImage destImage;
    Pix32 *srcData, *destData;
    register Pix32 *srcPtr, *destPtr;
#define MAXROWS	24
    register int sx, sy, dx, dy;
    register int x, y;
    float red, green, blue;
    int width, height;
    int radius;
    register float *valuePtr;
    int filterWidth;

    width = ColorImageWidth(srcImage);
    height = ColorImageHeight(srcImage);

    destImage = Blt_CreateColorImage(width, height);
    srcData = ColorImageData(srcImage);
    destData = ColorImageData(destImage);

    radius = (int)filterPtr->support;
    if (radius < 1) {
	radius = 1;
    }
    filterWidth = radius * 2;
#define PIXELOFFSET(x,y) (((y) * width) + (x))
    destPtr = destData;
    for (dy = 0; dy < height; dy++) {
	for (dx = 0; dx < width; dx++) {
	    red = green = blue = 0.0;
	    valuePtr = filterPtr->kernel;
	    for (sy = (dy - radius); sy <= (dy + radius); sy++) {
		y = sy;
		if (y < 0) {
		    y = 0;
		}  else if (y >= height) {
		    y = height - 1;
		}
		for (sx = (dx - radius); sx <= (dx + radius); sx++) {
		    x = sx;
		    if (x < 0) {
			x = 0;
		    }  else if (sx >= width) {
			x = width - 1;
		    }
#ifdef notdef
		    fprintf(stderr, "%d,%d r=%d\n", x, y, radius);
#endif
		    srcPtr = srcData + PIXELOFFSET(x, y);
		    red += *valuePtr * (float)srcPtr->Red;
		    green += *valuePtr * (float)srcPtr->Green;
		    blue += *valuePtr * (float)srcPtr->Blue;
#ifdef notdef
		    fprintf(stderr, "%d,%d = r=%f,g=%f,b=%f\n", x, y, 
			    red, green, blue);
#endif
		    valuePtr++;
		}
	    }
	    red /= filterPtr->sum;
	    green /= filterPtr->sum;
	    blue /= filterPtr->sum;
#ifdef notdef
	    fprintf(stderr, "%d,%d = r=%f,g=%f,b=%f\n", x, y, red, green, blue);
#endif
	    destPtr->Red = (unsigned char)CLAMP(red);
	    destPtr->Green = (unsigned char)CLAMP(green);
	    destPtr->Blue = (unsigned char)CLAMP(blue);
	    destPtr++;
	}
    }
    return destImage;
}

#ifdef WIN32
/*
 *----------------------------------------------------------------------
 *
 * Blt_DrawableToColorImage --
 *
 *      Takes a snapshot of an X drawable (pixmap or window) and
 *	converts it to a color image.
 *
 * Results:
 *      Returns a color image of the drawable.  If an error occurred,
 *	NULL is returned.
 *
 *----------------------------------------------------------------------
 */
ColorImage
Blt_DrawableToColorImage(tkwin, drawable, width, height, regionPtr)
    Tk_Window tkwin;
    Drawable drawable;
    int width, height;		/* Dimension of the drawable. */
    ImageRegion *regionPtr;	/* Region to be snapped. */
{
    HDC drawDC;
    TkWinDCState state;
    register Pix32 *destPtr;
    ColorImage image;
    register int x, y;
    COLORREF rgb;


    if ((regionPtr->x > width) || (regionPtr->y > height)) {
	return NULL;		/* Bad region X-Y coordinates */
    }
    /* Fix region dimensions if necessary */

    if (regionPtr->width < 1) {
	regionPtr->width = width;
    } else if ((regionPtr->x + regionPtr->width) > width) {
	regionPtr->width = width - regionPtr->x;
    }
    if (regionPtr->height < 1) {
	regionPtr->height = height;
    } else if ((regionPtr->y + regionPtr->height) > height) {
	regionPtr->height = height - regionPtr->y;
    }
    drawDC = TkWinGetDrawableDC(Tk_Display(tkwin), drawable, &state);
    /*
     * Allocate an array of pixels big enough to hold the image.
     */
    image = Blt_CreateColorImage(regionPtr->width, regionPtr->height);
    destPtr = ColorImageData(image);
    for (y = 0; y < regionPtr->height; y++) {
	for (x = 0; x < regionPtr->width; x++) {
	    rgb = GetPixel(drawDC, x + regionPtr->x, y + regionPtr->y);
	    destPtr->Red = (unsigned char)GetRValue(rgb);
	    destPtr->Green = (unsigned char)GetGValue(rgb);
	    destPtr->Blue = (unsigned char)GetBValue(rgb);
	    destPtr++;
	}
    }
    TkWinReleaseDrawableDC(drawable, drawDC, &state);
    return image;
}

#else

/* ARGSUSED */
static int
XGetImageErrorProc(clientData, errEventPtr)
    ClientData clientData;
    XErrorEvent *errEventPtr;
{
    int *errorPtr = (int *)clientData;

    *errorPtr = TCL_ERROR;
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_DrawableToColorImage --
 *
 *      Takes a snapshot of an X drawable (pixmap or window) and
 *	converts it to a color image.
 *
 *	The trick here is to efficiently convert the pixel values
 *	(indices into the color table) into RGB color values.  In the
 *	days of 8-bit displays, it was simpler to get RGB values for
 *	all 256 indices into the colormap.  Instead we'll build a
 *	hashtable of unique pixels and from that an array of pixels to
 *	pass to XQueryColors.  For TrueColor visuals, we'll simple
 *	compute the colors from the pixel.
 *
 *	[I don't know how much faster it would be to take advantage
 *	of all the different visual types.  This pretty much depends
 *	on the size of the image and the number of colors it uses.]
 *
 * Results:
 *      Returns a color image of the drawable.  If an error occurred,
 *	NULL is returned.
 *
 *----------------------------------------------------------------------
 */
ColorImage
Blt_DrawableToColorImage(tkwin, drawable, width, height, regionPtr)
    Tk_Window tkwin;
    Drawable drawable;
    int width, height;		/* Dimension of the drawable. */
    ImageRegion *regionPtr;	/* Region to be snapped. */
{
    XImage *imagePtr;
    ColorImage image;
    register Pix32 *destPtr;
    unsigned long pixel;
    int result = TCL_OK;
    Tk_ErrorHandler errHandler;
    register int x, y;
    Visual *visualPtr;

    errHandler = Tk_CreateErrorHandler(Tk_Display(tkwin), BadMatch,
	X_GetImage, -1, XGetImageErrorProc, (ClientData)&result);
    imagePtr = XGetImage(Tk_Display(tkwin), drawable, 0, 0, width, height,
	AllPlanes, ZPixmap);
    Tk_DeleteErrorHandler(errHandler);
    XSync(Tk_Display(tkwin), False);
    if (result != TCL_OK) {
	return NULL;
    }
    if ((regionPtr->x > width) || (regionPtr->y > height)) {
	return NULL;		/* Bad region X-Y coordinates */
    }
    /* Fix region dimensions if necessary */
    if (regionPtr->width < 1) {
	regionPtr->width = width;
    } else if ((regionPtr->x + regionPtr->width) > width) {
	regionPtr->width = width - regionPtr->x;
    }
    if (regionPtr->height < 1) {
	regionPtr->height = height;
    } else if ((regionPtr->y + regionPtr->height) > height) {
	regionPtr->height = height - regionPtr->y;
    }
    /*
     * First allocate an array of pixels big enough to hold the image.
     */
    image = Blt_CreateColorImage(regionPtr->width, regionPtr->height);

    visualPtr = Tk_Visual(tkwin);
    if (visualPtr->class == TrueColor) {
	unsigned int red, green, blue;
	/*
	 * Directly compute the RGB color values from the pixel index
	 * rather than of going through XQueryColors.
	 */
	ComputeMasks(visualPtr);
	destPtr = ColorImageData(image);
	for (y = 0; y < regionPtr->height; y++) {
	    for (x = 0; x < regionPtr->width; x++) {
		pixel = XGetPixel(imagePtr, x + regionPtr->x, y + regionPtr->y);

		red = (pixel & visualPtr->red_mask) >> redMaskShift;
		green = (pixel & visualPtr->green_mask) >> greenMaskShift;
		blue = (pixel & visualPtr->blue_mask) >> blueMaskShift;
		/*
		 * The number of bits per color in the pixel may be
		 * less than eight. For example, 15/16 bit displays
		 * (hi-color) use only 5 bits, 8-bit displays use 2 or
		 * 3 bits (don't ask me why you'd have an 8-bit
		 * TrueColor display). So shift back the least
		 * significant bits.
		 */
		destPtr->Red = (unsigned char)(red << redAdjust);
		destPtr->Green = (unsigned char)(green << greenAdjust);
		destPtr->Blue = (unsigned char)(blue << blueAdjust);
		destPtr++;
	    }
	}
	XDestroyImage(imagePtr);
    } else {
	Tcl_HashEntry *hPtr;
	Tcl_HashSearch cursor;
	Tcl_HashTable colorFreqTable;
	XColor *colorPtr, *colorArr;
	int numPixels;
	int numColors;
	int isNew;
	register int i;

	/*
	 * Fill the array with each pixel of the image. At the same time, build
	 * up a hashtable of the pixels used.
	 */
	numPixels = regionPtr->width * regionPtr->height;
	Tcl_InitHashTable(&colorFreqTable, TCL_ONE_WORD_KEYS);
	destPtr = ColorImageData(image);
	for (y = 0; y < regionPtr->height; y++) {
	    for (x = 0; x < regionPtr->width; x++) {
		pixel = XGetPixel(imagePtr, x + regionPtr->x, y + regionPtr->y);
		hPtr = Tcl_CreateHashEntry(&colorFreqTable, (char *)pixel, &isNew);
		if (isNew) {
		    Tcl_SetHashValue(hPtr, (char *)pixel);
		}
		destPtr->value = pixel;
		destPtr++;
	    }
	}
	XDestroyImage(imagePtr);

	/*
	 * Convert the hashtable of pixels into an array of XColors so that we
	 * can call XQueryColors with it. XQueryColors will convert the pixels
	 * into their RGB values.
	 */
	numColors = colorFreqTable.numEntries;
	colorArr = (XColor *)malloc(sizeof(XColor) * numColors);
	assert(colorArr);

	colorPtr = colorArr;
	for (hPtr = Tcl_FirstHashEntry(&colorFreqTable, &cursor); hPtr != NULL;
	    hPtr = Tcl_NextHashEntry(&cursor)) {
	    colorPtr->pixel = (unsigned long)Tcl_GetHashValue(hPtr);
	    Tcl_SetHashValue(hPtr, (char *)colorPtr);
	    colorPtr++;
	}
	XQueryColors(Tk_Display(tkwin), Tk_Colormap(tkwin), colorArr, numColors);

	/*
	 * Go again through the array of pixels, replacing each pixel of the
	 * image with its RGB value.
	 */
	destPtr = ColorImageData(image);
	for (i = 0; i < numPixels; i++, destPtr++) {
	    hPtr = Tcl_FindHashEntry(&colorFreqTable, (char *)destPtr->value);
	    colorPtr = (XColor *)Tcl_GetHashValue(hPtr);
	    destPtr->Red = (unsigned char)(colorPtr->red >> 8);
	    destPtr->Green = (unsigned char)(colorPtr->green >> 8);
	    destPtr->Blue = (unsigned char)(colorPtr->blue >> 8);
	}
	free((char *)colorArr);
	Tcl_DeleteHashTable(&colorFreqTable);
    }
    return image;
}

#endif /* WIN32 */

/*
 *----------------------------------------------------------------------
 *
 * Blt_SnapPhoto --
 *
 *      Takes a snapshot of an X drawable (pixmap or window) and
 *	writes it to an existing Tk photo image.
 *
 * Results:
 *      A standard Tcl result.
 *
 * Side Effects:
 *	The named Tk photo is updated with the snapshot.
 *
 *----------------------------------------------------------------------
 */
int
Blt_SnapPhoto(interp, tkwin, drawable, width, height, destWidth, destHeight,
    photoName)
    Tcl_Interp *interp;		/* Interpreter to report errors back to */
    Tk_Window tkwin;
    Drawable drawable;		/* Window or pixmap to be snapped */
    int width, height;		/* Dimension of the drawable */
    int destWidth, destHeight;	/* Desired size of the Tk photo */
    char *photoName;		/* Name of an existing Tk photo image. */
{
    Tk_PhotoHandle photo;	/* The photo image to write into. */
    ColorImage image;
    ImageRegion region;

    photo = Blt_FindPhoto(interp, photoName);
    if (photo == NULL) {
	Tcl_AppendResult(interp, "image \"", photoName, "\" doesn't",
	    " exist or is not a photo image", (char *)NULL);
	return TCL_ERROR;
    }
    region.x = region.y = region.width = region.height = 0;
    image = Blt_DrawableToColorImage(tkwin, drawable, width, height, &region);
    if (image == NULL) {
	Tcl_AppendResult(interp,
	    "can't grab window or pixmap (possibly obscured?)", (char *)NULL);
	return TCL_ERROR;	/* Can't grab window image */
    }
    if ((destWidth != width) || (destHeight != height)) {
	ImageRegion destRegion;
	ColorImage destImage;

	/*
	 * The requested size for the destination image is different than
	 * that of the source snapshot.  Resample the image as necessary.
	 * We'll use a cheap box filter. I'm assuming that the destination
	 * image will usually be smaller than the original.
	 */
	destRegion.x = destRegion.y = 0;
	destRegion.width = destWidth, destRegion.height = destHeight;
	destImage = Blt_ResampleColorImage(image, &region, &destRegion,
	    bltBoxFilter, bltBoxFilter);
	Blt_FreeColorImage(image);
	image = destImage;
    }
    Blt_ColorImageToPhoto(image, photo);
    Blt_FreeColorImage(image);
    return TCL_OK;
}

#if HAVE_JPEGLIB_H
#undef HAVE_STDLIB_H
#undef EXTERN
#include "jpeglib.h"
#include <setjmp.h>

typedef struct ErrorInfo {
    struct jpeg_error_mgr pub;	/* "public" fields */
    jmp_buf jmpBuf;
} ErrorInfo;

static void ErrorReadingJPEG _ANSI_ARGS_((j_common_ptr jpegInfo));

/*
 * Here's the routine that will replace the standard error_exit method:
 */

static void
ErrorReadingJPEG(jpegInfo)
    j_common_ptr jpegInfo;
{
    struct ErrorInfo *jpegErrorPtr = (ErrorInfo *)jpegInfo->err;

    longjmp(jpegErrorPtr->jmpBuf, 1);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_JPEGToColorImage --
 *
 *      Reads a JPEG file and converts it into a color image.
 *
 * Results:
 *      The color image is returned.  If an error occured, such
 *	as the designated file could not be opened, NULL is returned.
 *
 *----------------------------------------------------------------------
 */
ColorImage
Blt_JPEGToColorImage(interp, fileName)
    Tcl_Interp *interp;
    char *fileName;
{
    struct jpeg_decompress_struct jpegInfo;
    ColorImage image;
    unsigned int imageWidth, imageHeight;
    register Pix32 *destPtr;
    ErrorInfo jpegError;
    FILE *f;
    JSAMPLE **readBuffer;
    int row_stride;
    register int i;
    register JSAMPLE *bufPtr;

    f = fopen(fileName, "rb");
    if (f == NULL) {
	Tcl_AppendResult(interp, "can't open \"", fileName, "\":",
	    Tcl_PosixError(interp), (char *)NULL);
	return NULL;
    }
    image = NULL;

    /* Step 1: allocate and initialize JPEG decompression object */

    /* We set up the normal JPEG error routines, then override error_exit. */
    jpegInfo.err = jpeg_std_error(&jpegError.pub);
    jpegError.pub.error_exit = ErrorReadingJPEG;

    if (setjmp(jpegError.jmpBuf)) {
	jpeg_destroy_decompress(&jpegInfo);
	fclose(f);
	return NULL;
    }
    jpeg_create_decompress(&jpegInfo);
    jpeg_stdio_src(&jpegInfo, f);

    jpeg_read_header(&jpegInfo, TRUE);	/* Step 3: read file parameters */

    jpeg_start_decompress(&jpegInfo);	/* Step 5: Start decompressor */
    imageWidth = jpegInfo.output_width;
    imageHeight = jpegInfo.output_height;
    if ((imageWidth < 1) || (imageHeight < 1)) {
	Tcl_AppendResult(interp, "bad JPEG image size", (char *)NULL);
	fclose(f);
	return NULL;
    }
    /* JSAMPLEs per row in output buffer */
    row_stride = imageWidth * jpegInfo.output_components;

    /* Make a one-row-high sample array that will go away when done
       with image */
    readBuffer = (*jpegInfo.mem->alloc_sarray)
	((j_common_ptr) & jpegInfo, JPOOL_IMAGE, row_stride, 1);
    image = Blt_CreateColorImage(imageWidth, imageHeight);
    destPtr = ColorImageData(image);
    while (jpegInfo.output_scanline < imageHeight) {
	jpeg_read_scanlines(&jpegInfo, readBuffer, 1);
	bufPtr = readBuffer[0];
	for (i = 0; i < (int)imageWidth; i++) {
	    if (jpegInfo.output_components == 1) {
		destPtr->Red = destPtr->Green = destPtr->Blue = *bufPtr++;
	    } else {
		destPtr->Red = *bufPtr++;
		destPtr->Green = *bufPtr++;
		destPtr->Blue = *bufPtr++;
	    }
	    destPtr++;
	}
    }
    jpeg_finish_decompress(&jpegInfo);	/* We can ignore the return value
					* since suspension is not
					* possible with the stdio data
					* source.  */
    jpeg_destroy_decompress(&jpegInfo);

    /* After finish_decompress, we can close the input file.
     * Here we postpone it until after no more JPEG errors are possible,
     * so as to simplify the setjmp error logic above.  (Actually, I don't
     * think that jpeg_destroy can do an error exit, but why assume anything...)
     */
    fclose(f);

    /* At this point you may want to check to see whether any corrupt-data
     * warnings occurred (test whether jerr.pub.num_warnings is nonzero).
     */

    /*
     * We're ready to call the Tk_Photo routines. They'll take the RGB
     * array we've processed to build the Tk image of the JPEG.
     */
    return image;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_JPEGToPhoto --
 *
 *      Reads a JPEG file and converts it into a Tk photo.
 *
 * Results:
 *      A standard Tcl result.  If successful, TCL_OK is returned
 *	and the designated photo is re-written with the image.
 *	Otherwise, TCL_ERROR is returned and interp->result will
 *	contain an error message.
 *
 *----------------------------------------------------------------------
 */
int
Blt_JPEGToPhoto(interp, fileName, photoName)
    Tcl_Interp *interp;
    char *fileName;
    char *photoName;
{
    Tk_PhotoHandle photo;	/* The photo image to write into. */
    ColorImage image;

    photo = Blt_FindPhoto(interp, photoName);
    if (photo == NULL) {
	Tcl_AppendResult(interp, "image \"", photoName, "\" doesn't",
	    " exist or is not a photo image", (char *)NULL);
	return TCL_ERROR;
    }
    image = Blt_JPEGToColorImage(interp, fileName);
    if (image == NULL) {
	return TCL_ERROR;
    }
    Blt_ColorImageToPhoto(image, photo);
    Blt_FreeColorImage(image);
    return TCL_OK;
}

#endif /* HAVE_JPEGLIB_H */


/*
 *----------------------------------------------------------------------
 *
 * Blt_ColorImageToGreyscale --
 *
 *	Converts a color image to PostScript grey scale (1 component)
 *	output.  Luminosity isn't computed using the old NTSC formula,
 *
 *	    Y = 0.299 * Red + 0.587 * Green + 0.114 * Blue
 *
 *      but the following
 *
 *	    Y = 0.212671 * Red + 0.715160 * Green + 0.072169 * Blue
 *
 *	which better represents contemporary monitors.
 *
 * Results:
 *	The color image is converted to greyscale.
 *
 *----------------------------------------------------------------------
 */
int
Blt_ColorImageToGreyscale(srcImage, destImage)
    ColorImage srcImage, destImage;
{
    register Pix32 *srcPtr, *destPtr;
    float y;
    register int i;
    int numPixels;
    int width, height;

    width = ColorImageWidth(srcImage);
    height = ColorImageHeight(srcImage);
    ResizeImageData(destImage, width, height);
    numPixels = width * height;
    srcPtr = ColorImageData(srcImage);
    destPtr = ColorImageData(destImage);
    for (i = 0; i < numPixels; i++, srcPtr++, destPtr++) {
	y = (float)((0.212671 * (float)srcPtr->Red) +
	    (0.715160 * (float)srcPtr->Green) +
	    (0.072169 * (float)srcPtr->Blue));
	destPtr->Red = destPtr->Green = destPtr->Blue = (unsigned char)CLAMP(y);
    }
    return TCL_OK;
}

#ifdef notdef
/*
 *----------------------------------------------------------------------
 *
 * Blt_RotateColorImage --
 *
 *	Rotates a color image.
 *
 * Results:
 *	The color image is converted to greyscale.
 *
 *----------------------------------------------------------------------
 */
ColorImage
Blt_RotateColorImage(srcImage, theta)
    ColorImage srcImage;
    double theta;
{
    register Pix32 *srcPtr, *destPtr;
    float y;
    register int i;
    int numPixels;
    int width, height;

    width = ColorImageWidth(srcImage);
    height = ColorImageHeight(srcImage);
    destImage = Blt_CreateColorImage(height, width);
    numPixels = width * height;
    srcDataPtr = ColorImageData(srcImage);
    destDataPtr = ColorImageData(destImage);
    quadrant = (int)(theta / 90.0);
    switch (quadrant) {
    case ROTATE_270:		/* 270 degrees */
	for (x = 0; x < width; x++) {
	    for (y = 0; y < height; y++) {
		dx = y, dy = destWidth - x - 1;

		destPtr = destDataPtr + (y * height) + x;
		srcPtr = srcDataPtr + (
		    *destPtr++ = pixel;
	    }
	}
	break;

    case ROTATE_180:		/* 180 degrees */
	for (dx = 0; dx < destWidth; dx++) {
	    for (dy = 0; dy < destHeight; dy++) {
		sx = destWidth - dx - 1, sy = destHeight - dy - 1;
		XPutPixel(dest, dx, dy, XGetPixel(src, sx, sy));
	    }
	}
	break;

    case ROTATE_90:		/* 90 degrees */
	for (dx = 0; dx < destWidth; dx++) {
	    for (dy = 0; dy < destHeight; dy++) {
		sx = destHeight - dy - 1, sy = dx;
		XPutPixel(dest, dx, dy, XGetPixel(src, sx, sy));
	    }
	}
	break;

    case ROTATE_0:		/* 0 degrees */
	for (dx = 0; dx < destWidth; dx++) {
	    for (dy = 0; dy < destHeight; dy++) {
		XPutPixel(dest, dx, dy, XGetPixel(src, dx, dy));
	    }
	}
	break;

    default:

	y = (0.212671 * (float)srcPtr->Red) +
	    (0.715160 * (float)srcPtr->Green) +
	    (0.072169 * (float)srcPtr->Blue);
	destPtr->Red = destPtr->Green = destPtr->Blue = (unsigned char)CLAMP(y);
    }
    return TCL_OK;
}

#endif

#ifdef notdef

/*
 *----------------------------------------------------------------------
 *
 * StringToColormap --
 *
 *	Convert a string into a colormap.
 *
 * Results:
 *	If the string is successfully converted, TCL_OK is returned.
 *	Otherwise, TCL_ERROR is returned and an error message is left in
 *	interp->result.
 *
 * Side Effects:
 *	The padding structure passed is updated with the new values.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
    StringToColormap(clientData, interp, tkwin, string, widgRec, offset)
    ClientData clientData;	/* not used */
    Tcl_Interp *interp;		/* Interpreter to send results back to */
    Tk_Window tkwin;		/* Window */
    char *string;		/* Pixel value string */
    char *widgRec;		/* Widget record */
    int offset;			/* Offset of pad in widget */
{
    Colormap *colorMapPtr = (Colormap *) (widgRec + offset);

    if (*colorMapPtr != None) {
	Tk_FreeColormap(Tk_Display(tkwin), *colorMapPtr);
	*colorMapPtr = None;
    }
    if ((string != NULL) && (string[0] != '\0')) {
	Colormap colorMap;

	colorMap = Tk_GetColormap(interp, tkwin, string);
	if (colorMap == None) {
	    return TCL_ERROR;
	}
	*colorMapPtr = colorMap;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ColormapToString --
 *
 *	Converts the two pad values into a Tcl list.  Each pad has two
 *	pixel values.  For vertical pads, they represent the top and bottom
 *	margins.  For horizontal pads, they're the left and right margins.
 *	All pad values are non-negative integers.
 *
 * Results:
 *	The padding list is returned.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static char *
ColormapToString(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;	/* not used */
    Tk_Window tkwin;		/* not used */
    char *widgRec;		/* Structure record */
    int offset;			/* Offset of pad in record */
    Tcl_FreeProc **freeProcPtr;	/* not used */
{
    TextAttributes *attrPtr = (TextAttributes *)(widgRec + offset);
    char *result;
    char string[200];

    sprintf(string, "%s %d", Tk_NameOfColor(attrPtr->shadowColor),
	attrPtr->shadowOffset);
    result = strdup(string);
    if (result == NULL) {
	return "out of memory";
    }
    *freeProcPtr = (Tcl_FreeProc *)free;
    return result;
}

/*
 * Definition of the structure associated with color image masters.
 */
typedef struct ColorImageMaster {
    Tk_ImageMaster tkMaster;	/* Tk's token for image master.  NULL means
				 * the image is being deleted. */
    Tcl_Interp *interp;		/* Interpreter associated with the
				 * application using this image. */
    Tcl_Command cmdToken;	/* Token for image command (used to delete
				 * it when the image goes away).  NULL means
				 * the image command has already been
				 * deleted. */
    ImageRegion reqRegion;	/* Requested region of source to be copied
				 * into the image */
    char *fileString;		/* Name of file to read into image. */
    char *dataString;		/* String represents the 4 component data
				 * of the image (RGBA). */
    char *colorMap;		/* String represents the 4 component data
				 * of the image (RGBA). */
    char *format;		/* User-specified format of data in image
				 * file or string value. */

    Blt_List instances;		/* List of instances of image displayed on the
				 * screen. In most cases this will be only
				 * one. */

    int flags;			/* Sundry flags, defined below. */
    double gamma;		/* Display gamma value to correct for. */

    ColorImage original;	/* The original image */

} ColorImageMaster;

/*
 * Bit definitions for the flags field of a ColorImageMaster.
 * COLOR_IMAGE:			1 means that the image has different color
 *				components.
 * IMAGE_CHANGED:		1 means that the instances of this image
 *				need to be redithered.
 */

#define COLOR_IMAGE		1
#define IMAGE_CHANGED		2

/*
 * The following data structure represents all of the instances of
 * a photo image in windows on a given screen that are using the
 * same colormap.
 */

typedef struct ColorPixmap {
    Pixmap pixmap;
    ColorTable *colorTabPtr;
} ColorPixmap;


typedef struct ColorImageInstance {
    ColorImageMaster *masterPtr;/* Pointer to master for image. */

    Display *display;		/* Display for windows using this instance. */

    Colormap colormap;		/* The image may only be used in windows with
				 * this particular colormap. */

    Tk_Uid palette;		/* Palette for these particular instances. */

    double gamma;		/* Gamma value for these instances. */

    int keepColors;		/* Number of colors to "keep" from the default
				 * colormap when using a private colormap. */

    int exactColors;		/* A boolean that indicates if we should try
				 * to allocate "exact" colors or "best" match
				 * the colors that are currently allocated.
				 */
    int quantize;

    ColorImage image;		/* If NULL, this indicates that we should get
				 * the image from the master. */

    ColorPixmap colorPixmap;	/* The color pixmap (pixmap + palette) */

} ColorImageInstance;

/*
 * The type record for color images:
 */
static int CreateImageProc _ANSI_ARGS_((Tcl_Interp *interp, char *name,
	int argc, char **argv, Tk_ImageType * typePtr, Tk_ImageMaster master,
	ClientData *clientDataPtr));
static ClientData GetImageProc _ANSI_ARGS_((Tk_Window tkwin,
	ClientData clientData));
static void DisplayImageProc _ANSI_ARGS_((ClientData clientData,
	Display *display, Drawable drawable, int imageX, int imageY,
	int width, int height, int drawableX, int drawableY));
static void FreeImageProc _ANSI_ARGS_((ClientData clientData,
	Display *display));
static void DeleteImageProc _ANSI_ARGS_((ClientData clientData));

static Tk_ImageType ColorImageType = {
    "color",			/* name */
    CreateImageProc,		/* createProc */
    GetImageProc,		/* getProc */
    DisplayImageProc,		/* displayProc */
    FreeImageProc,		/* freeProc */
    DeleteImageProc,		/* deleteProc */
    (Tk_ImageType *) NULL	/* nextPtr */
};

/*
 * Information used for parsing configuration specifications:
 */
static Tk_ConfigSpec configSpecs[] = {
    {
	TK_CONFIG_CUSTOM, "-colormap", (char *)NULL, (char *)NULL,
	DEF_IMAGE_COLORMAP, Tk_Offset(ColorImageMaster, palette),
	0, &bltColormapOption
    },
    {
	TK_CONFIG_STRING, "-data", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(ColorImageMaster, dataString),
	TK_CONFIG_NULL_OK
    },
    {
	TK_CONFIG_BOOLEAN, "-exact", (char *)NULL, (char *)NULL,
	DEF_IMAGE_EXACT_COLORS, Tk_Offset(ColorImageMaster, exactColors), 0
    },
    {
	TK_CONFIG_STRING, "-format", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(ColorImageMaster, format), TK_CONFIG_NULL_OK
    },
    {
	TK_CONFIG_STRING, "-file", (char *)NULL, (char *)NULL,
	(char *)NULL, Tk_Offset(ColorImageMaster, fileString),
	TK_CONFIG_NULL_OK
    },
    {
	TK_CONFIG_DOUBLE, "-gamma", (char *)NULL, (char *)NULL,
	DEF_PHOTO_GAMMA, Tk_Offset(ColorImageMaster, gamma), 0
    },
    {
	TK_CONFIG_BOOLEAN, "-greyscale", (char *)NULL, (char *)NULL,
	DEF_IMAGE_GREYSCALE, Tk_Offset(ColorImageMaster, greyscale), 0
    },
    {
	TK_CONFIG_INT, "-height", (char *)NULL, (char *)NULL,
	DEF_PHOTO_HEIGHT, Tk_Offset(ColorImageMaster, height), 0
    },
    {
	TK_CONFIG_INT, "-keep", (char *)NULL, (char *)NULL,
	DEF_IMAGE_KEEP_COLORS, Tk_Offset(ColorImageMaster, keepColors), 0
    },
    {
	TK_CONFIG_UID, "-palette", (char *)NULL, (char *)NULL,
	DEF_PHOTO_PALETTE, Tk_Offset(ColorImageMaster, palette), 0
    },
    {
	TK_CONFIG_CUSTOM, "-region", (char *)NULL, (char *)NULL,
	DEF_IMAGE_REGION, Tk_Offset(ColorImageMaster, reqRegion), 0
    },
    {
	TK_CONFIG_INT, "-width", (char *)NULL, (char *)NULL,
	DEF_PHOTO_WIDTH, Tk_Offset(ColorImageMaster, width), 0
    },
    {
	TK_CONFIG_WINDOW, "-window", (char *)NULL, (char *)NULL,
	DEF_IMAGE_WINDOW, Tk_Offset(ColorImageMaster, window), 0
    },
    {
	TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL,
	(char *)NULL, 0, 0
    }
};

/*
 *----------------------------------------------------------------------
 *
 * CreateImageProc --
 *
 *	This procedure is called by the Tk image code to create
 *	a new color image.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	The data structure for a new color image is allocated and
 *	initialized.
 *
 *----------------------------------------------------------------------
 */

static int
    CreateImageProc(interp, name, argc, argv, typePtr, master, clientDataPtr)
    Tcl_Interp *interp;		/* Interpreter for application containing
				 * image. */
    char *name;			/* Name to use for image. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings for options (doesn't
				 * include image name or type). */
    Tk_ImageType *typePtr;	/* Pointer to our type record (not used). */
    Tk_ImageMaster master;	/* Token for image, to be used by us in
				 * later callbacks. */
    ClientData *clientDataPtr;	/* Store manager's token for image here;
				 * it will be returned in later callbacks. */
{
    ImageMaster *serverPtr;

    /*
     * Allocate and initialize the photo image master record.
     */

    masterPtr = (ImageMaster *)calloc(sizeof(ImageMaster));
    masterPtr->tkMaster = master;
    masterPtr->interp = interp;
    if (name != NULL) {
	masterPtr->imageCmd = Tcl_CreateCommand(interp, name, ColorImageCmd,
	    (ClientData)masterPtr, ColorImageCmdDeletedProc);
    }
    masterPtr->palette = NULL;
    masterPtr->pix24 = NULL;
    masterPtr->instancePtr = NULL;
    masterPtr->validRegion = TkCreateRegion();

    Blt_InitList(&(masterPtr->clients), TCL_ONE_WORD_KEYS);

    /*
     * Process configuration options given in the image create command.
     */
    if (ConfigureColorImage(interp, masterPtr, argc, argv, 0) != TCL_OK) {
	DeleteImageProc((ClientData)masterPtr);
	return TCL_ERROR;
    }
    *clientDataPtr = (ClientData)masterPtr;
    return TCL_OK;
}

#endif
