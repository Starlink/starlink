/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id: tkCanvasPsImage.c,v 1.4 1999/03/22 21:42:24 abrighto Exp $" 
 *
 * TkCanvasPsImage.C -  Implement Tk postscript output for images
 *
 * The code here was taken from a version of the Tk canvasps patch, which
 * was modified by Peter Draper of Starlink to only access the visible
 * portion of an image (required, to avoid problems with large or zoomed
 * in images).
 *
 * To avoid having to patch the Tk sources, I gathered all the necessary
 * code in this file, including some private struct typedefs from the Tk
 * source files, which are not available in any include files.
 *
 * NOTE: This code is Tk4.2 specific. If you upgrade to a newer version,
 * you will have to redo it, based on the latest Tk sources and/or Tk
 * canvasps patch.
 *
 * To enable this extension, call TkCanvasPsImage_Init().
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  19/06/98  Created
 */
static char* rcsId="@(#) $Id: tkCanvasPsImage.c,v 1.4 1999/03/22 21:42:24 abrighto Exp $";


#include <stdio.h>
#include "tkInt.h"
#include "tkPort.h"
#include "tkCanvas.h"


/*
 * Struct used in generating postscript for images.
 */
typedef struct TkColormapData {   /* Hold color information for a window */
    int separated;                /* Whether to use separate color bands */
    int color;                    /* Whether window is color or black/white */
    int ncolors;                  /* Number of color values stored */
    XColor *colors;               /* Pixel value -> RGB mappings */
    int red_mask, green_mask, blue_mask;      /* Masks and sifts for each */
    int red_shift, green_shift, blue_shift;   /* color band */
} TkColormapData;


/* (from tkCanvPs.c)
 * One of the following structures is created to keep track of Postscript
 * output being generated.  It consists mostly of information provided on
 * the widget command line.
 */

typedef struct TkPostscriptInfo {
    int x, y, width, height;	/* Area to print, in canvas pixel
				 * coordinates. */
    int x2, y2;			/* x+width and y+height. */
    char *pageXString;		/* String value of "-pagex" option or NULL. */
    char *pageYString;		/* String value of "-pagey" option or NULL. */
    double pageX, pageY;	/* Postscript coordinates (in points)
				 * corresponding to pageXString and
				 * pageYString. Don't forget that y-values
				 * grow upwards for Postscript! */
    char *pageWidthString;	/* Printed width of output. */
    char *pageHeightString;	/* Printed height of output. */
    double scale;		/* Scale factor for conversion: each pixel
				 * maps into this many points. */
    Tk_Anchor pageAnchor;	/* How to anchor bbox on Postscript page. */
    int rotate;			/* Non-zero means output should be rotated
				 * on page (landscape mode). */
    char *fontVar;		/* If non-NULL, gives name of global variable
				 * containing font mapping information.
				 * Malloc'ed. */
    char *colorVar;		/* If non-NULL, give name of global variable
				 * containing color mapping information.
				 * Malloc'ed. */
    char *colorMode;		/* Mode for handling colors:  "monochrome",
				 * "gray", or "color".  Malloc'ed. */
    int colorLevel;		/* Numeric value corresponding to colorMode:
				 * 0 for mono, 1 for gray, 2 for color. */
    char *fileName;		/* Name of file in which to write Postscript;
				 * NULL means return Postscript info as
				 * result. Malloc'ed. */
    Tcl_Channel chan;		/* Open channel corresponding to fileName. */
    Tcl_HashTable fontTable;	/* Hash table containing names of all font
				 * families used in output.  The hash table
				 * values are not used. */
    int prepass;		/* Non-zero means that we're currently in
				 * the pre-pass that collects font information,
				 * so the Postscript generated isn't
				 * relevant. */
} TkPostscriptInfo;

/* (from tkCanvImg.c)
 * The structure below defines the record for each image item.
 */

typedef struct ImageItem  {
    Tk_Item header;		/* Generic stuff that's the same for all
				 * types.  MUST BE FIRST IN STRUCTURE. */
    Tk_Canvas canvas;		/* Canvas containing the image. */
    double x, y;		/* Coordinates of positioning point for
				 * image. */
    Tk_Anchor anchor;		/* Where to anchor image relative to
				 * (x,y). */
    char *imageString;		/* String describing -image option (malloc-ed).
				 * NULL means no image right now. */
    Tk_Image image;		/* Image to display in window, or NULL if
				 * no image at present. */
} ImageItem;


/*
 *--------------------------------------------------------------
 *
 * TkImageGetColor --
 *
 *	This procedure converts a pixel value to three floating
 *      point numbers, representing the amount of red, green, and 
 *      blue in that pixel on the screen.  It makes use of colormap
 *      data passed as an argument, and should work for all Visual
 *      types.
 *
 * Results:
 *	Returns red, green, and blue color values in the range 
 *      0 to 1.  There are no error returns.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
 
static void
TkImageGetColor(cdata, pixel, red, green, blue)
     TkColormapData *cdata;              /* Colormap data */
     unsigned long pixel;                /* Pixel value to look up */
     double *red, *green, *blue;         /* Color data to return */
{
    if (cdata->separated) {
        int r = (pixel & cdata->red_mask) >> cdata->red_shift;
        int g = (pixel & cdata->green_mask) >> cdata->green_shift;
        int b = (pixel & cdata->blue_mask) >> cdata->blue_shift;
        *red = cdata->colors[r].red / 65535.0;
        *green = cdata->colors[g].green / 65535.0;
        *blue = cdata->colors[b].blue / 65535.0;
    } else {
        *red = cdata->colors[pixel].red / 65535.0;
        *green = cdata->colors[pixel].green / 65535.0;
        *blue = cdata->colors[pixel].blue / 65535.0;
    }
}

 
/*
 *--------------------------------------------------------------
 *
 * TkCanvasPsImage --
 *
 *	This procedure is called to output the contents of an
 *	image in Postscript, using a format appropriate for the 
 *      current color mode (i.e. one bit per pixel in monochrome, 
 *      one byte per pixel in gray, and three bytes per pixel in
 *      color).
 *
 * Results:
 *	Returns a standard Tcl return value.  If an error occurs
 *	then an error message will be left in interp->result.
 *	If no error occurs, then additional Postscript will be
 *	appended to interp->result.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
 
int
TkCanvasPsImage(interp, canvas, ximage, cdata, width, height)
    Tcl_Interp *interp;         /* Interpreter for returning Postscript code
                                   or error message. */
    Tk_Canvas canvas;           /* Canvas containing image */
    XImage *ximage;             /* Image to draw */
    TkColormapData *cdata;      /* Colormap information */
    unsigned int width, height; /* Width and height of image */
{
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    TkPostscriptInfo *psInfoPtr = canvasPtr->psInfoPtr;
    char buffer[256];
    int x, y, band, maxRows;
    double red, green, blue;
    int bytesPerLine=0, maxWidth=0;
    int level = psInfoPtr->colorLevel;
 
    /*
     * Figure out which color level to use (possibly lower than the 
     * one specified by the user).  For example, if the user specifies
     * color with monochrome screen, use gray or monochrome mode instead. 
     */
 
    if (!cdata->color && level == 2) {
        level = 1;
    }

    if (!cdata->color && cdata->ncolors == 2) {
        level = 0;
    }
 
    /*
     * Check that at least one row of the image can be represented
     * with a string less than 64 KB long (this is a limit in the 
     * Postscript interpreter).
     */
     
    switch (level)
    {
        case 0: bytesPerLine = (width + 7) / 8;  maxWidth = 240000;  break;
        case 1: bytesPerLine = width;  maxWidth = 60000;  break;
        case 2: bytesPerLine = 3 * width;  maxWidth = 20000;  break;
    }
 
    if (bytesPerLine > 60000) {
        Tcl_ResetResult(interp);
        sprintf(buffer,
                "Can't generate Postscript for images"
                "more than %d pixxels wide",
                maxWidth);
        Tcl_AppendResult(interp, buffer, (char *) NULL);
        return TCL_ERROR;
    }
 
    maxRows = 60000 / bytesPerLine;
 
    for (band = height-1; band >= 0; band -= maxRows) {
        int rows = (band >= maxRows) ? maxRows : band + 1;
        int lineLen = 0;
        switch (level) {
            case 0:
                sprintf(buffer, "%d %d 1 matrix {\n<", width, rows);
                Tcl_AppendResult(interp, buffer, (char *) NULL);
                break;
            case 1:
                sprintf(buffer, "%d %d 8 matrix {\n<", width, rows);
                Tcl_AppendResult(interp, buffer, (char *) NULL);
                break;
            case 2:
                sprintf(buffer, "%d %d 8 matrix {\n<",
                        width, rows);
                Tcl_AppendResult(interp, buffer, (char *) NULL);
                break;
        }
        for (y = band; y > band - rows; y --) {
            switch (level) {
                case 0: {
                    /*
                     * Generate data for image in monochrome mode.
                     * No attempt at dithering is made--instead, just
                     * set a threshold.
                     */
                    unsigned char mask=0x80;
                    unsigned char data=0x00;
                    for (x = 0; x < width; x ++) {
                        TkImageGetColor(cdata, XGetPixel(ximage, x, y),
                                        &red, &green, &blue);
                        if (0.30 * red + 0.59 * green + 0.11 * blue > 0.5)
                            data |= mask;
                        mask >>= 1;
                        if (mask == 0) {
                            sprintf(buffer, "%02X", data);
                            Tcl_AppendResult(interp, buffer, (char *) NULL);
                            lineLen += 2;
                            if (lineLen > 60) {
                                lineLen = 0;
                                Tcl_AppendResult(interp, "\n", (char *) NULL);
                            }
                            mask=0x80;
                            data=0x00;
                        }
                    }
                    if ((width % 8) != 0) {
                        sprintf(buffer, "%02X", data);
                        Tcl_AppendResult(interp, buffer, (char *) NULL);
                        mask=0x80;
                        data=0x00;
                    }
                    break;
                }
                case 1: {
                    /*
                     * Generate data in gray mode--in this case, take a 
                     * weighted sum of the red, green, and blue values.
                     */
                    for (x = 0; x < width; x ++) {
                        TkImageGetColor(cdata, XGetPixel(ximage, x, y),
                                        &red, &green, &blue);
                        sprintf(buffer, "%02X", (int) floor(0.5 + 255.0 *
                                                            (0.30 * red +
                                                             0.59 * green +
                                                             0.11 * blue)));
                        Tcl_AppendResult(interp, buffer, (char *) NULL);
                        lineLen += 2;
                        if (lineLen > 60) {
                            lineLen = 0;
                            Tcl_AppendResult(interp, "\n", (char *) NULL);
                        }
                    }
                    break;
                }
                case 2: {
                    /*
                     * Finally, color mode.  Here, just output the red, green,
                     * and blue values directly.
                     */
                    for (x = 0; x < width; x ++) {
                        TkImageGetColor(cdata, XGetPixel(ximage, x, y),
                                        &red, &green, &blue);
                        sprintf(buffer, "%02X%02X%02X",
                                (int) floor(0.5 + 255.0 * red),
                                (int) floor(0.5 + 255.0 * green),
                                (int) floor(0.5 + 255.0 * blue));
                        Tcl_AppendResult(interp, buffer, (char *) NULL);
                        lineLen += 6;
                        if (lineLen > 60) {
                            lineLen = 0;
                            Tcl_AppendResult(interp, "\n", (char *) NULL);
                        }
                    }
                    break;
                }
            }
        }
        switch (level) {
            case 0: sprintf(buffer, ">\n} image\n"); break;
            case 1: sprintf(buffer, ">\n} image\n"); break;
            case 2: sprintf(buffer, ">\n} false 3 colorimage\n"); break;
        }
        Tcl_AppendResult(interp, buffer, (char *) NULL);
        sprintf(buffer, "0 %d translate\n", rows);
        Tcl_AppendResult(interp, buffer, (char *) NULL);
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * ImageToPostscript --
 *
 *	This procedure is called to generate Postscript for
 *	image items.
 *
 * Results:
 *	The return value is a standard Tcl result.  If an error
 *	occurs in generating Postscript then an error message is
 *	left in interp->result, replacing whatever used to be there.
 *	If no error occurs, then Postscript for the item is appended
 *	to the result.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static int
ImageToPostscript(interp, canvas, itemPtr, prepass)
     Tcl_Interp *interp;			/* Leave Postscript or error message
                                                 * here. */
     Tk_Canvas canvas;			/* Information about overall canvas. */
     Tk_Item *itemPtr;			/* Item for which Postscript is
 					 * wanted. */
     int prepass;			/* 1 means this is a prepass to
 					 * collect font information;  0 means
 					 * final Postscript is being created.*/
{
    ImageItem *imgPtr = (ImageItem *)itemPtr;
    Tk_Window canvasWin = Tk_CanvasTkwin(canvas);
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
   
    TkColormapData cdata;
  
    char buffer[256];
    
     int i, result, ncolors;
     double x, y;
     int width, height, depth;
     Pixmap pmap;
     Colormap cmap;
     XImage *ximage;
     Visual *visual;
     int screenX1, screenY1, screenX2, screenY2;
 
     if (prepass) {
         return TCL_OK;
     }
     
     /*  Determine region of image that needs to be drawn. Only the
         part visible on the display screen is done. */
     Tk_SizeOfImage(imgPtr->image, &width, &height);
     screenX1 = canvasPtr->xOrigin + canvasPtr->inset;
     screenY1 = canvasPtr->yOrigin + canvasPtr->inset;
     screenX2 = canvasPtr->xOrigin + Tk_Width(canvasWin) - canvasPtr->inset;
     screenY2 = canvasPtr->yOrigin + Tk_Height(canvasWin) - canvasPtr->inset;
     if ( width > screenX2 - screenX1 ) {
       width = screenX2 - screenX1;
     } else {
       screenX1 = 0;
     }
     if ( height > screenY2 - screenY1 ) {
       height = screenY2 - screenY1;
     } else {
       screenY1 = 0;
     }

     cmap = Tk_Colormap(canvasWin);
     depth = Tk_Depth(canvasWin);
     visual = Tk_Visual(canvasWin);
 
     /*
      * Create a Pixmap, tell the image to redraw itself there, and then
      * generate an XImage from the Pixmap.  We can then read pixel 
      * values out of the XImage.
      */
     pmap = Tk_GetPixmap(Tk_Display(canvasWin), Tk_WindowId(canvasWin),
                             width, height, depth);
     Tk_RedrawImage(imgPtr->image, screenX1, screenY1, width, height,
                    pmap, 0, 0); 
     ximage = XGetImage(Tk_Display(canvasWin), pmap, 0, 0, width, height,
                        AllPlanes, ZPixmap);
     Tk_FreePixmap(Tk_Display(canvasWin),pmap);
     
     /*
      * Compute the coordinates of the lower-left corner of the image,
      * taking into account the anchor position for the image.
      */
 
     x = imgPtr->x;
     y = Tk_CanvasPsY(canvas, imgPtr->y);
     
     switch (imgPtr->anchor) {
 	case TK_ANCHOR_NW:			y -= height;		break;
 	case TK_ANCHOR_N:	x -= width/2.0; y -= height;		break;
 	case TK_ANCHOR_NE:	x -= width;	y -= height;		break;
 	case TK_ANCHOR_E:	x -= width;	y -= height/2.0;	break;
 	case TK_ANCHOR_SE:	x -= width;				break;
 	case TK_ANCHOR_S:	x -= width/2.0;				break;
 	case TK_ANCHOR_SW:						break;
 	case TK_ANCHOR_W:			y -= height/2.0;	break;
 	case TK_ANCHOR_CENTER:	x -= width/2.0; y -= height/2.0;	break;
     }
 
     /*
      * Obtain information about the colormap, ie the mapping between
      * pixel values and RGB values.  The code below should work
      * for all Visual types.
      */
 
     ncolors = visual->map_entries;
     cdata.colors = (XColor *) ckalloc(sizeof(XColor) * ncolors);
     cdata.ncolors = ncolors;
 
     if (visual->class == DirectColor || visual->class == TrueColor)
     {
         cdata.separated = 1;
         cdata.red_mask = visual->red_mask;
         cdata.green_mask = visual->green_mask;
         cdata.blue_mask = visual->blue_mask;
         cdata.red_shift = 0;
         cdata.green_shift = 0;
         cdata.blue_shift = 0;
         while ((0x0001 & (cdata.red_mask >> cdata.red_shift)) == 0)
             cdata.red_shift ++;
         while ((0x0001 & (cdata.green_mask >> cdata.green_shift)) == 0)
             cdata.green_shift ++;
         while ((0x0001 & (cdata.blue_mask >> cdata.blue_shift)) == 0)
             cdata.blue_shift ++;
         for (i = 0; i < ncolors; i ++)
             cdata.colors[i].pixel =
                 ((i << cdata.red_shift) & cdata.red_mask) |
                 ((i << cdata.green_shift) & cdata.green_mask) |
                 ((i << cdata.blue_shift) & cdata.blue_mask);
     } else
     {
         cdata.separated=0;
         for (i = 0; i < ncolors; i ++)
             cdata.colors[i].pixel = i;
     }
     if (visual->class == StaticGray || visual->class == GrayScale)
         cdata.color = 0; else
             cdata.color = 1;
 
     XQueryColors(Tk_Display(canvasWin), cmap, cdata.colors, ncolors);
 
     /*
      * Finally, we have the pixel values and the colormap.  Call a
      * function that actually generates the postsript for the image. 
      */
 
     sprintf(buffer, "%.15g %.15g translate\n", x, y);
     Tcl_AppendResult(interp, buffer, (char *) NULL);
     result = TkCanvasPsImage(interp, canvas, ximage, &cdata, width, height);
 
     XDestroyImage(ximage);
     ckfree((char *) cdata.colors);
     return result;
}

 
/*
 * --------------------------------------------------------------
 *
 * TkCanvasPsImage_Init --
 *
 * 	This procedure is called to initialize the canvas image
 *  	postscript extension by setting a field in the canvas
 *      image type control struct, causing our postscript routine
 *      to be called for canvas images.
 *
 * Results:
 * 	None. 
 *
 * Side effects:
 * 	None.
 *
 * --------------------------------------------------------------
 */
 
void
TkCanvasPsImage_Init()
{
    extern Tk_ItemType tkImageType;
    tkImageType.postscriptProc = ImageToPostscript;
}


/*
 * XXX - allan: same as Tk_CanvasWindowCoords, but with no clipping
 */
void
Tk_CanvasWindowCoordsNoClip(canvas, x, y, screenXPtr, screenYPtr)
    Tk_Canvas canvas;			/* Token for the canvas. */
    double x, y;			/* Coordinates in canvas space. */
    int *screenXPtr, *screenYPtr;	/* Screen coordinates are stored
					 * here. */
{
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    double tmp;

    tmp = x - canvasPtr->xOrigin;
    if (tmp > 0) {
	tmp += 0.5;
    } else {
	tmp -= 0.5;
    }
    *screenXPtr = tmp;

    tmp = y  - canvasPtr->yOrigin;
    if (tmp > 0) {
	tmp += 0.5;
    } else {
	tmp -= 0.5;
    }
    *screenYPtr = tmp;
}
