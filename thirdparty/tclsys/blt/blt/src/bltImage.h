/*
 * bltImage.h --
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

#include <X11/Xutil.h>
#ifndef WIN32
#include <X11/Xproto.h>
#endif

#ifndef _BLT_IMAGE_H
#define _BLT_IMAGE_H


#define Red	rgba.red
#define Blue	rgba.blue
#define Green	rgba.green
#define Alpha	rgba.alpha

/*
 *----------------------------------------------------------------------
 *
 * Pix32 --
 *
 *      A union representing either a pixel as a RGB triplet or a 
 *	single word value.  
 *
 *----------------------------------------------------------------------
 */
typedef union Pix32 {
    unsigned int value;		/* Lookup table address */
    struct RGBA {
	unsigned char red;	/* Red intensity 0..255 */
	unsigned char green;	/* Green intensity 0.255 */
	unsigned char blue;	/* Blue intensity 0..255 */
	unsigned char alpha;	/* Alpha-channel for compositing. 0..255 */
    } rgba;
    unsigned char channel[4];
} Pix32;

typedef struct ColorInfo {
    XColor exact, best;
    float error;
    unsigned int freq;
    int allocated;
    int index;
} ColorInfo;

/*
 *----------------------------------------------------------------------
 *
 * ColorTable --
 *
 *	For colormap-ed visuals, this structure contains color lookup
 *	information needed to translate RGB triplets to pixel indices.
 * 
 *	This structure isn't needed for TrueColor or Monochrome visuals.
 *
 *	DirectColor:	
 *		Pixel values for each color channel 
 *	StaticColor, PsuedoColor, StaticGray, and GrayScale:
 *		Red represents the 8-bit color. Green and Blue pixel 
 *		values are unused. 
 *			
 *----------------------------------------------------------------------
 */
typedef struct ColorTable {
    Display *display;		/* Display of colortable. Used to free
				 * colors allocated. */
    XVisualInfo visualInfo;	/* Visual information for window displaying
				 * the image. */
    Colormap colorMap;		/* Colormap used.  This may be the default
				 * colormap, or an allocated private map. */
    double gamma;		/* Gamma correction value */
    int flags;
    unsigned int red[256], green[256], blue[256];

    /* Array of allocated pixels in colormap */
    ColorInfo colorInfo[256];
    ColorInfo *sortedColors[256];

    int numUsedColors, numFreeColors;
    int numPixels;		/* Number of colors in the quantized image */
    unsigned long int pixelValues[256];

    unsigned int *lut;		/* Color lookup table. Used to collect frequencies
				 * of colors and later colormap indices */
} *ColorTable;

#define PRIVATE_COLORMAP	1
#define RGBIndex(r,g,b) (((r)<<10) + ((r)<<6) + (r) + ((g) << 5) + (g) + (b))

/*
 *----------------------------------------------------------------------
 *
 * ColorImage --
 *
 *      The structure below represents a color image.  Each pixel 
 *	occupies a 32-bit word of memory: one byte for each of the
 *	red, green, and blue color intensities, and another for 
 *	alpha-channel image compositing (e.g. transparency).
 *
 *	Even though the alpha-channel is usually unused, the standard 
 *	packed format (a contiguous stream of 3-byte RGB values) like 
 *	that of the Tk photo doesn't perform especially well for image 
 *	processing operations, such as filtering or resampling, where 
 *	pixel addressing is needed.  It's especially woeful on RISC 
 *	architectures.
 *
 *----------------------------------------------------------------------
 */

typedef struct ColorImage {
    int width, height;		/* Dimensions of the image */

    Pix32 *dataPtr;		/* Array of pixels representing the image. */

} *ColorImage;

/* 
 * ColorImage is supposed to be an opaque type. 
 * Use the macros below to access its members.
 */
#define ColorImageHeight(c)	((c)->height)
#define ColorImageWidth(c)	((c)->width)
#define ColorImageData(c)	((c)->dataPtr)

/*
 *----------------------------------------------------------------------
 *
 * ImageFilterProc --
 *
 *      A function implementing a 1-D filter.  
 *
 *----------------------------------------------------------------------
 */
typedef double (ImageFilterProc) _ANSI_ARGS_((double value));

/*
 *----------------------------------------------------------------------
 *
 * Image1DFilter --
 *
 *      Contains information about a 1-D filter (its support and
 *	the procedure implementing the filter).  
 *
 *----------------------------------------------------------------------
 */
typedef struct Image1DFilter {
    float support;		/* Radius of 1D filter */

    ImageFilterProc *proc;	/* Procedure to filter an image */

} *Image1DFilter;

extern Image1DFilter bltBoxFilter; /* The ubiquitous 1-D Box filter */

/*
 *----------------------------------------------------------------------
 *
 * Image2DFilter --
 *
 *      Defines a convolution mask for a 2-D filter.  Used to smooth or 
 *	enhance images.
 *
 *----------------------------------------------------------------------
 */
typedef struct Image2DFilter {
    float support;		/* Radius of filter */
    float sum, scale;		/* Sum of kernel */
    float *kernel;		/* Array of values (malloc-ed) representing
				 * the discrete 2-D filter. */
} Image2DFilter;

/* Prototypes of image routines */

extern void Blt_ColorImageToPhoto _ANSI_ARGS_((ColorImage image, 
	Tk_PhotoHandle photo));

extern Pixmap Blt_ColorImageToPixmap _ANSI_ARGS_((Tcl_Interp *interp,
	Tk_Window tkwin, ColorImage image, ColorTable *colorTablePtr));

extern ColorImage Blt_ConvolveColorImage _ANSI_ARGS_((ColorImage srcImage, 
	Image2DFilter *filter));

extern ColorImage Blt_CreateColorImage _ANSI_ARGS_((int width, int height));

extern ColorImage Blt_DrawableToColorImage _ANSI_ARGS_((Tk_Window tkwin, 
	Drawable drawable, int width, int height, ImageRegion *regionPtr));

extern int Blt_FindImage1DFilter _ANSI_ARGS_((Tcl_Interp *interp, 
	char *filterName, Image1DFilter *filterPtr));

extern void Blt_FreeColorImage _ANSI_ARGS_((ColorImage image));

extern ColorImage Blt_PhotoToColorImage _ANSI_ARGS_((Tk_PhotoHandle photo, 
	ImageRegion *regionPtr));

extern ColorImage Blt_ResampleColorImage _ANSI_ARGS_((ColorImage image, 
	ImageRegion *srcRegionPtr, ImageRegion *destRegionPtr, 
	Image1DFilter horzFilter, Image1DFilter vertFilter));

extern void Blt_ResamplePhoto _ANSI_ARGS_((Tk_PhotoHandle srcPhoto, 
	Tk_PhotoHandle destPhoto, ImageRegion *srcRegionPtr, 
	Image1DFilter horzFilter, Image1DFilter vertFilter));

extern ColorImage Blt_ResizeColorImage _ANSI_ARGS_((ColorImage image, 
	ImageRegion *srcRegionPtr, ImageRegion *destRegionPtr));

extern void Blt_ResizePhoto _ANSI_ARGS_((Tk_PhotoHandle srcPhoto, 
	Tk_PhotoHandle destPhoto, ImageRegion *srcRegionPtr));

extern int Blt_SnapPhoto _ANSI_ARGS_((Tcl_Interp *interp, Tk_Window tkwin,
	Drawable drawable, int width, int height, int destWidth, 
	int destHeight, char *photoName));

extern ColorTable Blt_CreateColorTable _ANSI_ARGS_((Tk_Window tkwin));

extern ColorTable Blt_DirectColorTable _ANSI_ARGS_((Tcl_Interp *interp,
	Tk_Window tkwin, ColorImage image));

extern ColorTable Blt_PseudoColorTable _ANSI_ARGS_((Tcl_Interp *interp,
	Tk_Window tkwin, ColorImage image));

extern ColorTable Blt_CreateColorTable _ANSI_ARGS_((Tk_Window tkwin));

extern void Blt_FreeColorTable _ANSI_ARGS_((ColorTable colorTable));

/* Routines missing from the Tk photo C API */

extern int Blt_TkImageDeleted _ANSI_ARGS_((Tk_Image tkImage));

extern Tk_ImageMaster Blt_TkImageMaster _ANSI_ARGS_((Tk_Image tkImage));

#endif /*_BLT_IMAGE_H*/
