/*
 * bltCanvEps.c --
 *
 *	This file implements Encapsulated PostScript items for canvas widgets.
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
 *
 * EPS canvas item created by George Howlett.
 */

#include "bltInt.h"
#include "bltPs.h"
#include "bltImage.h"
#include <ctype.h>

#define DEBUG_PREVIEW 0

#define xMin	header.x1
#define xMax	header.x2
#define yMin	header.y1
#define yMax	header.y2


#define MAX_EPS_LINE_LENGTH 255	/* Maximum line length for a PostScript EPS file */

/*
 * ParseInfo --
 *
 *	This structure is used to pass PostScript file information
 *	around to various routines while parsing the EPS file.
 */
struct ParseInfo {
    int lineNumber;		/* Current line number of EPS file */
    char line[MAX_EPS_LINE_LENGTH + 1];
    /* Buffer to contain a single line from
				 * the PostScript file. */
    char hexTable[256];		/* Table for converting ASCII hex digits to
				 * values */

    char *nextPtr;		/* Pointer to the next character to process on
				 * the current line.  If NULL (or if nextPtr
				 * points a NULL byte), this indicates the
				 * the next line needs to be read. */
    FILE *filePtr;		/*  */
};

#define DEF_EPS_ANCHOR		"nw"
#define DEF_EPS_OUTLINE_COLOR	RGB_COLOR_BLACK
#define DEF_EPS_OUTLINE_MONO    RGB_COLOR_BLACK
#define DEF_EPS_BORDER_WIDTH	STD_BORDERWIDTH
#define DEF_EPS_FILE_NAME	(char *)NULL
#define DEF_EPS_FONT		STD_FONT
#define DEF_EPS_FILL_COLOR     	STD_COLOR_NORMAL_FG
#define DEF_EPS_FILL_MONO	STD_MONO_NORMAL_FG
#define DEF_EPS_HEIGHT		"0"
#define DEF_EPS_IMAGE_NAME	(char *)NULL
#define DEF_EPS_JUSTIFY		"center"
#define DEF_EPS_QUICK_RESIZE	"no"
#define DEF_EPS_RELIEF		"sunken"
#define DEF_EPS_SHADOW_COLOR	(char *)NULL
#define DEF_EPS_SHADOW_MONO	(char *)NULL
#define DEF_EPS_SHOW_IMAGE	"yes"
#define DEF_EPS_STIPPLE		(char *)NULL
#define DEF_EPS_TAGS		(char *)NULL
#define DEF_EPS_TITLE		(char *)NULL
#define DEF_EPS_TITLE_ANCHOR	"center"
#define DEF_EPS_TITLE_COLOR	RGB_COLOR_BLACK
#define DEF_EPS_TITLE_ROTATE	"0"
#define DEF_EPS_WIDTH		"0"

/*
 * Information used for parsing configuration specs:
 */

static Tk_CustomOption tagsOption =
{
    Tk_CanvasTagsParseProc, Tk_CanvasTagsPrintProc, (ClientData)NULL
};

extern Tk_CustomOption bltLengthOption;
extern Tk_CustomOption bltShadowOption;

/*
 * The structure below defines the record for each EPS item.
 */
typedef struct EpsItem {
    Tk_Item header;		/* Generic stuff that's the same for all
				 * types.  MUST BE FIRST IN STRUCTURE. */
    Tk_Canvas canvas;		/* Canvas containing the EPS item. */

    int canvasX, canvasY;	/* Translated (by the anchor) canvas
				 * coordinates of the EPS item. */

    int lastWidth, lastHeight;	/* Last known dimensions of the EPS item.
				 * This is used to know if the color image
				 * preview needs to be resized. */

    Tcl_Interp *interp;

    FILE *filePtr;		/* File pointer to PostScript file. We'll
				 * hold this as long as the EPS item is
				 * using this file. */

    Tk_Image tkImage;		/* A Tk photo image provided to display in
				 * the canvas as the EPS contents. This
				 * will supersede any EPS preview embedded
				 * in the PostScript itself. */

    Tk_PhotoHandle photo;	/* Photo handle of above Tk image. */

    Pixmap pixmap;		/* Pixmap representing scaled preview. This
				 * isn't currently used.  For now we're
				 * overwriting the Tk image everytime the
				 * EPS item is resized. In the future
				 * we'll use our own image routines. */

    ColorTable colorTable;	/* Pointer to color table */

    ColorImage colorImage;	/* The original photo or PostScript
				 * preview image converted to a color
				 * image.  This is kept around for
				 * resampling or resizing the image. */

    int firstLine, lastLine;	/* First and last line numbers of the
				 * PostScript preview.  They are used
				 * to skip over the preview when
				 * encapsulating PostScript for the
				 * canvas item. */

    GC fillGC;			/* Graphics context to fill background
				 * of image outline if no preview image
				 * was present. */

    int llx, lly, urx, ury;	/* Lower left and upper right coordinates
				 * of PostScript bounding box, retrieved
				 * from file's "BoundingBox:" field. */

    char *title;		/* Title, retrieved from the file's "Title:"
				 * field, to be displayed over the top of
				 * the EPS preview (malloc-ed).  */

    /* User configurable fields */

    double x, y;		/* Canvas coordinates of the item */
    Tk_Anchor anchor;

    char *fileName;		/* Name of the encapsulated PostScript file.
				 * If NULL, indicates that no EPS file
				 * has be successfully loaded yet. */

    char *imageName;		/* Name of a Tk photo image to be used as
				 * the thumbnail for the EPS file. This
				 * supersedes a preview image found in the
				 * EPS file. */

    char *reqTitle;		/* Title to be displayed in the EPS item.
				 * Supersedes the title found in the EPS
				 * file. If NULL, indicates that the title
				 * found in the EPS file should be used. */

    int width, height;		/* Dimensions of EPS item. If set to zero,
				 * the dimension found in the "%%BoundingBox:"
				 * specification from the EPS file are
				 * used. */

    int showImage;		/* Indicates if the image or the outline
				 * rectangle should be displayed */

    int quick;

    XColor *fillColor;		/* Fill color of the image outline. */

    Tk_3DBorder border;		/* Outline color */

    int borderWidth;
    int relief;

    TextAttributes titleAttr;	/* Font, color, etc. for title */
    CompoundText *titleText;

    Pixmap stipple;		/* Stipple for image fill */

} EpsItem;

static Tk_ConfigSpec configSpecs[] =
{
    {TK_CONFIG_ANCHOR, "-anchor", (char *)NULL, (char *)NULL,
	DEF_EPS_ANCHOR, Tk_Offset(EpsItem, anchor),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_SYNONYM, "-bd", "borderWidth", (char *)NULL, (char *)NULL, 0, 0},
    {TK_CONFIG_CUSTOM, "-borderwidth", "borderWidth", (char *)NULL,
	DEF_EPS_BORDER_WIDTH, Tk_Offset(EpsItem, borderWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_STRING, "-file", (char *)NULL, (char *)NULL,
	DEF_EPS_FILE_NAME, Tk_Offset(EpsItem, fileName), TK_CONFIG_NULL_OK},
    {TK_CONFIG_FONT, "-font", "font", "Font",
	DEF_EPS_FONT, Tk_Offset(EpsItem, titleAttr.font), 0},
    {TK_CONFIG_COLOR, "-fill", "fill", (char *)NULL,
	DEF_EPS_FILL_COLOR, Tk_Offset(EpsItem, fillColor), TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_COLOR, "-fill", "fill", (char *)NULL,
	DEF_EPS_FILL_MONO, Tk_Offset(EpsItem, fillColor), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_CUSTOM, "-height", (char *)NULL, (char *)NULL,
	DEF_EPS_HEIGHT, Tk_Offset(EpsItem, height),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_STRING, "-image", (char *)NULL, (char *)NULL,
	DEF_EPS_IMAGE_NAME, Tk_Offset(EpsItem, imageName), TK_CONFIG_NULL_OK},
    {TK_CONFIG_JUSTIFY, "-justify", "justify", "Justify",
	DEF_EPS_JUSTIFY, Tk_Offset(EpsItem, titleAttr.justify),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BORDER, "-outline", "outline", (char *)NULL,
	DEF_EPS_OUTLINE_COLOR, Tk_Offset(EpsItem, border),
	TK_CONFIG_COLOR_ONLY | TK_CONFIG_NULL_OK},
    {TK_CONFIG_BORDER, "-outline", "outline", (char *)NULL,
	DEF_EPS_OUTLINE_MONO, Tk_Offset(EpsItem, border),
	TK_CONFIG_MONO_ONLY | TK_CONFIG_NULL_OK},
    {TK_CONFIG_BOOLEAN, "-quick", "quick", "Quick",
	DEF_EPS_QUICK_RESIZE, Tk_Offset(EpsItem, quick),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_RELIEF, "-relief", (char *)NULL, (char *)NULL,
	DEF_EPS_RELIEF, Tk_Offset(EpsItem, relief),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-shadow", "shadow", "Shadow",
	DEF_EPS_SHADOW_COLOR, Tk_Offset(EpsItem, titleAttr.shadow),
	TK_CONFIG_COLOR_ONLY, &bltShadowOption},
    {TK_CONFIG_CUSTOM, "-shadow", "shadow", "Shadow",
	DEF_EPS_SHADOW_MONO, Tk_Offset(EpsItem, titleAttr.shadow),
	TK_CONFIG_MONO_ONLY, &bltShadowOption},
    {TK_CONFIG_BOOLEAN, "-showimage", "showImage", "ShowImage",
	DEF_EPS_SHOW_IMAGE, Tk_Offset(EpsItem, showImage),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BITMAP, "-stipple", (char *)NULL, (char *)NULL,
	DEF_EPS_STIPPLE, Tk_Offset(EpsItem, stipple), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-tags", (char *)NULL, (char *)NULL,
	DEF_EPS_TAGS, 0, TK_CONFIG_NULL_OK, &tagsOption},
    {TK_CONFIG_STRING, "-title", (char *)NULL, (char *)NULL,
	DEF_EPS_TITLE, Tk_Offset(EpsItem, reqTitle), TK_CONFIG_NULL_OK},
    {TK_CONFIG_ANCHOR, "-titleanchor", (char *)NULL, (char *)NULL,
	DEF_EPS_TITLE_ANCHOR, Tk_Offset(EpsItem, titleAttr.anchor), 0},
    {TK_CONFIG_COLOR, "-titlecolor", (char *)NULL, (char *)NULL,
	DEF_EPS_TITLE_COLOR, Tk_Offset(EpsItem, titleAttr.color),
	TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_DOUBLE, "-titlerotate", "titleRotate", "TitleRotate",
	DEF_EPS_TITLE_ROTATE, Tk_Offset(EpsItem, titleAttr.theta),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-width", (char *)NULL, (char *)NULL,
	DEF_EPS_WIDTH, Tk_Offset(EpsItem, width),
	TK_CONFIG_DONT_SET_DEFAULT, &bltLengthOption},
    {TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL,
	(char *)NULL, 0, 0}
};

/*
 * Prototypes for procedures defined in this file:
 */

static void ImageChangedProc _ANSI_ARGS_((ClientData clientData, int x, int y,
	int width, int height, int imgWidth, int imgHeight));
static int EpsCoords _ANSI_ARGS_((Tcl_Interp *interp, Tk_Canvas canvas,
	Tk_Item * itemPtr, int argc, char **argv));
static int EpsToArea _ANSI_ARGS_((Tk_Canvas canvas, Tk_Item * itemPtr,
	double *rectPtr));
static double EpsToPoint _ANSI_ARGS_((Tk_Canvas canvas, Tk_Item * itemPtr,
	double *coordPtr));
static void ComputeEpsBbox _ANSI_ARGS_((Tk_Canvas canvas, EpsItem *imgPtr));
static int ConfigureEps _ANSI_ARGS_((Tcl_Interp *interp, Tk_Canvas canvas,
	Tk_Item * itemPtr, int argc, char **argv, int flags));
static int CreateEps _ANSI_ARGS_((Tcl_Interp *interp, Tk_Canvas canvas,
	struct Tk_Item * itemPtr, int argc, char **argv));
static void DeleteEps _ANSI_ARGS_((Tk_Canvas canvas, Tk_Item * itemPtr,
	Display *display));
static void DisplayEps _ANSI_ARGS_((Tk_Canvas canvas, Tk_Item * itemPtr,
	Display *display, Drawable dst, int x, int y, int width, int height));
static void ScaleEps _ANSI_ARGS_((Tk_Canvas canvas, Tk_Item * itemPtr,
	double originX, double originY, double scaleX, double scaleY));
static void TranslateEps _ANSI_ARGS_((Tk_Canvas canvas, Tk_Item * itemPtr,
	double deltaX, double deltaY));
static int EpsToPostScript _ANSI_ARGS_((Tcl_Interp *interp, Tk_Canvas canvas,
	Tk_Item * itemPtr, int prepass));
static int ParseEpsFile _ANSI_ARGS_((Tcl_Interp *interp, EpsItem *epsPtr));


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
 * GetHexValue --
 *
 *	Reads the next ASCII hex value from EPS preview image and
 *	converts it.
 *
 * Results:
 *	One of three Tcl return values is possible.
 *
 *	TCL_OK		the next byte was successfully parsed.
 *	TCL_ERROR	an error occurred processing the next hex value.
 *	TCL_RETURN	"%%EndPreview" line was detected.
 *
 *	The converted hex value is returned via "bytePtr".
 *
 *----------------------------------------------------------------------
 */
static int
GetHexValue(infoPtr, bytePtr)
    struct ParseInfo *infoPtr;
    unsigned char *bytePtr;
{
    register char *p;

    p = infoPtr->nextPtr;
    if (p == NULL) {
      nextLine:
	if (fgets(infoPtr->line, MAX_EPS_LINE_LENGTH, infoPtr->filePtr) == NULL) {
#if DEBUG_PREVIEW
	    fprintf(stderr, "short file\n");
#endif
	    return TCL_ERROR;	/* Short file */
	}
	infoPtr->lineNumber++;
	if (infoPtr->line[0] != '%') {
#if DEBUG_PREVIEW
	    fprintf(stderr, "line doesn't start with %% (%s)\n", infoPtr->line);
#endif
	    return TCL_ERROR;
	}
	if ((infoPtr->line[1] == '%') &&
	    (strncmp(infoPtr->line + 2, "EndPreview", 10) == 0)) {
#if DEBUG_PREVIEW
	    fprintf(stderr, "end of preview (%s)\n", infoPtr->line);
#endif
	    return TCL_RETURN;
	}
	p = infoPtr->line + 1;
    }
    while (isspace((int)*p)) {
	p++;			/* Skip spaces */
    }
    if (*p == '\0') {
	goto nextLine;
    }
    if ((!isxdigit((int)p[0])) || (!isxdigit((int)p[1]))) {
#if DEBUG_PREVIEW
	fprintf(stderr, "not a hex digit (%s)\n", infoPtr->line);
#endif
	return TCL_ERROR;
    }
    *bytePtr = (infoPtr->hexTable[(int)p[0]] << 4) |
	infoPtr->hexTable[(int)p[1]];
    p += 2;
    infoPtr->nextPtr = p;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ReadEPSPreview --
 *
 *	Reads the EPS preview image from the PostScript file, converting
 *	the image into a ColorImage.  If an error occurs when parsing
 *	the preview, the preview is silently ignored.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
ReadEPSPreview(epsPtr, infoPtr)
    EpsItem *epsPtr;
    struct ParseInfo *infoPtr;
{
    ColorImage image;
    int width, height, bitsPerPixel, numLines;
    char *field, *beginPreview;

    /* Search for a "%%BeginPreview:" image specification*/

    beginPreview = NULL;
    while (fgets(infoPtr->line, MAX_EPS_LINE_LENGTH, infoPtr->filePtr) != NULL) {
	infoPtr->lineNumber++;
	if ((infoPtr->line[0] != '%') || (infoPtr->line[1] != '%')) {
	    continue;
	}
	field = infoPtr->line + 2;
	if ((field[0] == 'B') && (strncmp(field, "BeginPreview:", 13) == 0)) {
	    beginPreview = field + 13;
	    break;
	} else if ((field[0] == 'E') && ((strncmp(field, "EndProlog", 9) == 0) ||
		(strncmp(field, "EndSetup", 8) == 0))) {
	    break;		/* Done */
	}
    }
    if (beginPreview == NULL) {
#if DEBUG_PREVIEW
	fprintf(stderr, "No beginpreview (%s)\n", infoPtr->line);
#endif
	return;			/* No "%%BeginPreview:" line */
    }
    if (sscanf(beginPreview, "%d %d %d %d", &width, &height, &bitsPerPixel,
	    &numLines) != 4) {
#if DEBUG_PREVIEW
	fprintf(stderr, "Bad beginpreview (%s)\n", beginPreview);
#endif
	return;
    }
    if (((bitsPerPixel != 1) && (bitsPerPixel != 8)) ||
	((width < 1) || (height < 1))) {
#if DEBUG_PREVIEW
	fprintf(stderr, "Bad beginpreview (%s)\n", beginPreview);
#endif
	return;			/* Bad "%%BeginPreview:" information */
    }
    epsPtr->firstLine = infoPtr->lineNumber;
    Blt_InitHexTable(infoPtr->hexTable);
    infoPtr->nextPtr = NULL;
    image = Blt_CreateColorImage(width, height);

    if (bitsPerPixel == 8) {
	int result;
	register Pix32 *pixelPtr;
	register int x, y;
	unsigned char byte;

	for (y = height - 1; y >= 0; y--) {
	    pixelPtr = ColorImageData(image) + (y * width);
	    for (x = 0; x < width; x++, pixelPtr++) {
		result = GetHexValue(infoPtr, &byte);
		if (result == TCL_ERROR) {
		    goto error;
		}
		if (result == TCL_RETURN) {
		    goto done;
		}
		pixelPtr->Red = pixelPtr->Green = pixelPtr->Blue = byte;
	    }
	}
    } else {
	int result;
	register Pix32 *pixelPtr;
	register int x, y;
	unsigned char byte;
	register int bit;

	pixelPtr = ColorImageData(image);
	for (y = 0; y < height; y++) {
	    bit = 8;
	    for (x = 0; x < width; x++, pixelPtr++) {
		if (bit == 8) {
		    result = GetHexValue(infoPtr, &byte);
		    if (result == TCL_ERROR) {
			goto error;
		    }
		    if (result == TCL_RETURN) {
			goto done;
		    }
		    byte = ReverseBits(byte);
		    bit = 0;
		}
		if (((byte >> bit) & 0x01) == 0) {
		    pixelPtr->Red = pixelPtr->Green = pixelPtr->Blue = 0xFF;
		}
		bit++;
	    }
	}
    }
  done:
    epsPtr->colorImage = image;
    epsPtr->lastLine = infoPtr->lineNumber + 1;
    return;

  error:
    epsPtr->firstLine = epsPtr->lastLine = -1;
    if (image != NULL) {
	Blt_FreeColorImage(image);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ParseEpsFile --
 *
 *	This routine parses the few fields we need out of an EPS file.
 *
 *	The EPS standards are outlined from Appendix H of the
 *	"PostScript Language Reference Manual" pp. 709-736.
 *
 *	Mandatory fields:
 *
 *	- Starts with "%!PS*"
 *	- Contains "%%BoundingBox: llx lly urx ury"
 *
 *	Optional fields for EPS item:
 *	- "%%BeginPreview: w h bpp #lines"
 *		Preview is in hexadecimal. Each line must start with "%"
 *      - "%%EndPreview"
 *	- "%%Title: (string)"
 *
 *----------------------------------------------------------------------
 */
static int
ParseEpsFile(interp, epsPtr)
    Tcl_Interp *interp;
    EpsItem *epsPtr;
{
    char *field, *title, *boundingBox;
    int readPreview;
    struct ParseInfo info;


    info.filePtr = epsPtr->filePtr;
    if (fgets(info.line, MAX_EPS_LINE_LENGTH, info.filePtr) == NULL) {
	Tcl_AppendResult(interp, "file \"", epsPtr->fileName, "\" is empty?",
	    (char *)NULL);
	return TCL_ERROR;
    }
    if (strncmp(info.line, "%!PS", 4) != 0) {
	Tcl_AppendResult(interp, "file \"", epsPtr->fileName,
	    "\" doesn't start with \"%!PS\"", (char *)NULL);
	return TCL_ERROR;
    }
    /*
     * Initial field flags to NULL. We want to  look only at the first
     * appearance of these comment fields.  The file itself may imbed
     * another EPS file.
     */
    boundingBox = title = NULL;
    readPreview = TRUE;

    info.lineNumber = 1;
    while (fgets(info.line, MAX_EPS_LINE_LENGTH, info.filePtr) != NULL) {
	info.lineNumber++;
	if ((info.line[0] == '%') &&
	    (info.line[1] == '%')) {	/* Header comment */
	    field = info.line + 2;
	    if ((field[0] == 'B') &&
		(strncmp(field, "BoundingBox:", 12) == 0)) {
		if (boundingBox == NULL) {
		    int numFields;

		    boundingBox = field + 12;
		    numFields = sscanf(boundingBox, "%d %d %d %d",
			&(epsPtr->llx), &(epsPtr->lly),
			&(epsPtr->urx), &(epsPtr->ury));
		    if (numFields != 4) {
			Tcl_AppendResult(interp,
			    "bad \"%%BoundingBox\" values: \"",
			    boundingBox, "\"", (char *)NULL);
			goto error;
		    }
		}
	    } else if ((field[0] == 'T') &&
		(strncmp(field, "Title:", 6) == 0)) {
		if (title == NULL) {
		    title = strdup(field + 6);
		}
	    } else if (field[0] == 'E') {
		if (strncmp(field, "EndComments", 11) == 0) {
		    break;	/* Done */
		}
		if (strncmp(field, "EndSetup", 8) == 0) {
		    readPreview = FALSE;
		    break;	/* Done */
		}
	    }
	}			/* %% */
    }
    if (boundingBox == NULL) {
	Tcl_AppendResult(interp, "\"BoundingBox:\" not found in file \"",
	    epsPtr->fileName, "\"", (char *)NULL);
      error:
	if (title != NULL) {
	    free(title);
	}
	return TCL_ERROR;	/* BoundingBox: is required. */
    }
    if (title != NULL) {
	epsPtr->title = title;
    }
    if (readPreview) {
#ifdef notdef
	if ((readPreview) && (epsPtr->photo == NULL)) {
	}
#endif
	/* Read the EPS preview image if no preview image was supplied */
	ReadEPSPreview(epsPtr, &info);
    }
    rewind(epsPtr->filePtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * DeleteEps --
 *
 *	This procedure is called to clean up the data structure
 *	associated with a EPS item.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Resources associated with itemPtr are released.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static void
DeleteEps(canvas, itemPtr, display)
    Tk_Canvas canvas;		/* Info about overall canvas widget. */
    Tk_Item *itemPtr;		/* Item that is being deleted. */
    Display *display;		/* Display containing window for
					 * canvas. */
{
    EpsItem *epsPtr = (EpsItem *)itemPtr;

    Tk_FreeOptions(configSpecs, (char *)epsPtr, display, 0);
    if (epsPtr->colorImage != NULL) {
	Blt_FreeColorImage(epsPtr->colorImage);
    }
    if (epsPtr->tkImage != NULL) {
	Tk_FreeImage(epsPtr->tkImage);
    }
    if (epsPtr->pixmap != None) {
#ifdef notdef
	Blt_FreeColorTable(epsPtr->colorTable);
#endif
	Tk_FreePixmap(display, epsPtr->pixmap);
    }
    if (epsPtr->stipple != None) {
	Tk_FreePixmap(display, epsPtr->stipple);
    }
    if (epsPtr->fillGC != NULL) {
	Tk_FreeGC(display, epsPtr->fillGC);
    }
    Blt_FreeTextAttributes(display, &(epsPtr->titleAttr));

    if (epsPtr->filePtr != NULL) {
	fclose(epsPtr->filePtr);
    }
    if (epsPtr->title != NULL) {
	free((char *)epsPtr->title);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * CreateEps --
 *
 *	This procedure is invoked to create a new EPS item
 *	in a canvas.
 *
 * Results:
 *	A standard Tcl return value.  If an error occurred in
 *	creating the item, then an error message is left in
 *	interp->result;  in this case itemPtr is left uninitialized,
 *	so it can be safely freed by the caller.
 *
 * Side effects:
 *	A new EPS item is created.
 *
 *----------------------------------------------------------------------
 */
static int
CreateEps(interp, canvas, itemPtr, argc, argv)
    Tcl_Interp *interp;		/* Interpreter for error reporting. */
    Tk_Canvas canvas;		/* Canvas to hold new item. */
    Tk_Item *itemPtr;		/* Record to hold new item;  header
					 * has been initialized by caller. */
    int argc;			/* Number of arguments in argv. */
    char **argv;		/* Arguments describing rectangle. */
{
    EpsItem *epsPtr = (EpsItem *)itemPtr;
    Tk_Window tkwin;

    tkwin = Tk_CanvasTkwin(canvas);
    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
	    Tk_PathName(tkwin), " create ", itemPtr->typePtr->name,
	    " x1 y1 ?options?\"", (char *)NULL);
	return TCL_ERROR;
    }
    /*
     * Initialize the item's record by hand (bleah).
     */
    epsPtr->anchor = TK_ANCHOR_NW;
    epsPtr->border = NULL;
    epsPtr->borderWidth = 2;
    epsPtr->canvas = canvas;
    epsPtr->fileName = NULL;
    epsPtr->filePtr = NULL;
    epsPtr->fillGC = NULL;
    epsPtr->fillColor = NULL;
    epsPtr->colorImage = NULL;
    epsPtr->imageName = NULL;
    epsPtr->interp = interp;
    epsPtr->photo = NULL;
    epsPtr->pixmap = None;
    epsPtr->firstLine = epsPtr->lastLine = -1;
    epsPtr->relief = TK_RELIEF_SUNKEN;
    epsPtr->reqTitle = NULL;
    epsPtr->stipple = None;
    epsPtr->showImage = TRUE;
    epsPtr->quick = FALSE;
    epsPtr->title = NULL;
    epsPtr->tkImage = NULL;
    epsPtr->lastWidth = epsPtr->lastHeight = 0;
    epsPtr->width = epsPtr->height = 0;
    epsPtr->x = epsPtr->y = 0.0;
    epsPtr->llx = epsPtr->lly = epsPtr->urx = epsPtr->ury = 0;
    epsPtr->canvasX = epsPtr->canvasY = 0;
    memset(&(epsPtr->titleAttr), 0, sizeof(TextAttributes));
#define PAD	8
    epsPtr->titleAttr.padLeft = epsPtr->titleAttr.padRight = PAD;
    epsPtr->titleAttr.padTop = epsPtr->titleAttr.padBottom = PAD;

    /*
     * Process the arguments to fill in the item record.
     */

    if ((Tk_CanvasGetCoord(interp, canvas, argv[0], &(epsPtr->x)) != TCL_OK) ||
	(Tk_CanvasGetCoord(interp, canvas, argv[1], &(epsPtr->y)) != TCL_OK)) {
	return TCL_ERROR;
    }
    if (ConfigureEps(interp, canvas, itemPtr, argc - 2, argv + 2, 0)
	!= TCL_OK) {
	DeleteEps(canvas, itemPtr, Tk_Display(tkwin));
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ImageChangedProc
 *
 *	The image is over-written each time the EPS item is resized.
 *	So we only worry if the image is deleted.
 *
 *	We always resample from the color image we saved when the
 *	photo image was specified (-image option).
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
/* ARGSUSED */
static void
ImageChangedProc(clientData, x, y, width, height, imageWidth, imageHeight)
    ClientData clientData;
    int x, y, width, height;	/* Not used */
    int imageWidth, imageHeight;/* Not used */
{
    EpsItem *epsPtr = (EpsItem *)clientData;

    if ((epsPtr->tkImage == NULL) || (Blt_TkImageDeleted(epsPtr->tkImage))) {
	epsPtr->tkImage = NULL;
	if (epsPtr->imageName != NULL) {
	    free((char *)epsPtr->imageName);
	    epsPtr->imageName = NULL;
	}
	Tk_CanvasEventuallyRedraw(epsPtr->canvas, epsPtr->xMin, epsPtr->yMin,
	    epsPtr->xMax, epsPtr->yMax);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ConfigureEps --
 *
 *	This procedure is invoked to configure various aspects
 *	of an EPS item, such as its background color.
 *
 * Results:
 *	A standard Tcl result code.  If an error occurs, then
 *	an error message is left in interp->result.
 *
 * Side effects:
 *	Configuration information may be set for itemPtr.
 *
 *----------------------------------------------------------------------
 */
static int
ConfigureEps(interp, canvas, itemPtr, argc, argv, flags)
    Tcl_Interp *interp;		/* Used for error reporting. */
    Tk_Canvas canvas;		/* Canvas containing itemPtr. */
    Tk_Item *itemPtr;		/* EPS item to reconfigure. */
    int argc;			/* Number of elements in argv.  */
    char **argv;		/* Arguments describing things to configure. */
    int flags;			/* Flags to pass to Tk_ConfigureWidget. */
{
    EpsItem *epsPtr = (EpsItem *)itemPtr;
    Tk_Window tkwin;
    XGCValues gcValues;
    unsigned long gcMask;
    GC newGC;
    int width, height;

    tkwin = Tk_CanvasTkwin(canvas);
    if (Tk_ConfigureWidget(interp, tkwin, configSpecs, argc,
	    argv, (char *)epsPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    /*
     * Check for a "-image" option specifying an image to be displayed
     * representing the EPS canvas item.
     */
    if (Blt_ConfigModified(configSpecs, "-image", (char *)NULL)) {
	if (epsPtr->tkImage != NULL) {
	    Tk_FreeImage(epsPtr->tkImage);	/* Release the old Tk image */
	    Blt_FreeColorImage(epsPtr->colorImage);
	    epsPtr->tkImage = NULL;
	    epsPtr->photo = NULL;
	    epsPtr->colorImage = NULL;
	}
	if (epsPtr->imageName != NULL) {
	    ImageRegion region;
	    /*
	     * Allocate a new image, if one was named.
	     */
	    epsPtr->photo = Blt_FindPhoto(interp, epsPtr->imageName);
	    if (epsPtr->photo == NULL) {
		Tcl_AppendResult(interp, "image \"", epsPtr->imageName,
		    "\" doesn't  exist or is not a photo image",
		    (char *)NULL);
		return TCL_ERROR;
	    }
	    epsPtr->tkImage = Tk_GetImage(interp, tkwin, epsPtr->imageName,
		ImageChangedProc, (ClientData)epsPtr);
	    if (epsPtr->tkImage == NULL) {
		Tcl_AppendResult(interp, "can't find an image \"",
		    epsPtr->imageName, "\"", (char *)NULL);
		free((char *)epsPtr->imageName);
		epsPtr->imageName = NULL;
		return TCL_ERROR;
	    }
	    epsPtr->lastWidth = epsPtr->lastHeight = 0;
	    Tk_SizeOfImage(epsPtr->tkImage, &(region.width), &(region.height));
	    region.x = region.y = 0;
	    epsPtr->colorImage = Blt_PhotoToColorImage(epsPtr->photo, &region);
	}
    }
    if (Blt_ConfigModified(configSpecs, "-file", (char *)NULL)) {
	if (epsPtr->filePtr != NULL) {
	    fclose(epsPtr->filePtr);
	    epsPtr->filePtr = NULL;
	}
	if (epsPtr->pixmap != None) {
#ifdef notdef
	    Blt_FreeColorTable(epsPtr->colorTable);
#endif
	    Tk_FreePixmap(Tk_Display(tkwin), epsPtr->pixmap);
	    epsPtr->pixmap = None;
	}
	if (epsPtr->colorImage != NULL) {
	    Blt_FreeColorImage(epsPtr->colorImage);
	    epsPtr->colorImage = NULL;
	}
	epsPtr->firstLine = epsPtr->lastLine = -1;
	if (epsPtr->fileName != NULL) {
	    epsPtr->filePtr = fopen(epsPtr->fileName, "r");
	    if (epsPtr->filePtr == NULL) {
		Tcl_AppendResult(interp, "can't open \"", epsPtr->fileName,
		    "\": ", Tcl_PosixError(interp), (char *)NULL);
		return TCL_ERROR;
	    }
	    ParseEpsFile(interp, epsPtr);
	}
    }
    /* Determine the size of the EPS item */
    width = height = 0;
    if (epsPtr->tkImage != NULL) {
	Tk_SizeOfImage(epsPtr->tkImage, &width, &height);
    }
    if (epsPtr->width == 0) {
	if (epsPtr->fileName != NULL) {
	    width = (epsPtr->urx - epsPtr->llx);
	}
	epsPtr->width = width;
    }
    if (epsPtr->height == 0) {
	if (epsPtr->fileName != NULL) {
	    height = (epsPtr->ury - epsPtr->lly);
	}
	epsPtr->height = height;
    }
    Blt_ResetTextAttributes(tkwin, &(epsPtr->titleAttr));

    if (Blt_ConfigModified(configSpecs, "-quick", (char *)NULL)) {
	epsPtr->lastWidth = epsPtr->lastHeight = 0;
    }
    /* Fill color GC */

    newGC = NULL;
    if (epsPtr->fillColor != NULL) {
	gcMask = GCForeground;
	gcValues.foreground = epsPtr->fillColor->pixel;
	if (epsPtr->stipple != None) {
	    gcMask |= (GCStipple | GCFillStyle);
	    gcValues.stipple = epsPtr->stipple;
	    if (epsPtr->border != NULL) {
		gcValues.foreground = Tk_3DBorderColor(epsPtr->border)->pixel;
		gcValues.background = epsPtr->fillColor->pixel;
		gcMask |= GCBackground;
		gcValues.fill_style = FillOpaqueStippled;
	    } else {
		gcValues.fill_style = FillStippled;
	    }
	}
	newGC = Tk_GetGC(tkwin, gcMask, &gcValues);
    }
    if (epsPtr->fillGC != NULL) {
	Tk_FreeGC(Tk_Display(tkwin), epsPtr->fillGC);
    }
    epsPtr->fillGC = newGC;

    ComputeEpsBbox(canvas, epsPtr);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * EpsCoords --
 *
 *	This procedure is invoked to process the "coords" widget
 *	command on EPS items.  See the user documentation for
 *	details on what it does.
 *
 * Results:
 *	Returns TCL_OK or TCL_ERROR, and sets interp->result.
 *
 * Side effects:
 *	The coordinates for the given item may be changed.
 *
 *----------------------------------------------------------------------
 */
static int
EpsCoords(interp, canvas, itemPtr, argc, argv)
    Tcl_Interp *interp;		/* Used for error reporting. */
    Tk_Canvas canvas;		/* Canvas containing item. */
    Tk_Item *itemPtr;		/* Item whose coordinates are to be
					 * read or modified. */
    int argc;			/* Number of coordinates supplied in
					 * argv. */
    char **argv;		/* Array of coordinates: x1, y1,
					 * x2, y2, ... */
{
    EpsItem *epsPtr = (EpsItem *)itemPtr;

    if ((argc != 0) && (argc != 2)) {
	Tcl_AppendResult(interp, "wrong # coordinates: expected 0 or 2, got ",
	    Blt_Int(argc), (char *)NULL);
	return TCL_ERROR;
    }
    if (argc == 2) {
	double x, y;		/* Don't overwrite old coordinates on errors */

	if ((Tk_CanvasGetCoord(interp, canvas, argv[0], &x) != TCL_OK) ||
	    (Tk_CanvasGetCoord(interp, canvas, argv[1], &y) != TCL_OK)) {
	    return TCL_ERROR;
	}
	epsPtr->x = x;
	epsPtr->y = y;
	ComputeEpsBbox(canvas, epsPtr);
	return TCL_OK;
    }
    Tcl_AppendElement(interp, Blt_Double(interp, epsPtr->x));
    Tcl_AppendElement(interp, Blt_Double(interp, epsPtr->y));
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ComputeEpsBbox --
 *
 *	This procedure is invoked to compute the bounding box of
 *	all the pixels that may be drawn as part of a EPS item.
 *	This procedure is where the preview image's placement is
 *	computed.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The fields x1, y1, x2, and y2 are updated in the header
 *	for itemPtr.
 *
 *----------------------------------------------------------------------
 */
 /* ARGSUSED */
static void
ComputeEpsBbox(canvas, epsPtr)
    Tk_Canvas canvas;		/* Canvas that contains item. */
    EpsItem *epsPtr;		/* Item whose bbox is to be recomputed. */
{
    int x, y;

    x = ROUND(epsPtr->x), y = ROUND(epsPtr->y);
    Blt_TranslateAnchor(x, y, epsPtr->width, epsPtr->height, epsPtr->anchor,
	&x, &y);
    epsPtr->xMin = epsPtr->canvasX = x;
    epsPtr->yMin = epsPtr->canvasY = y;

    /*
     * The xMax and yMax are (weirdly) exterior to the item.  Can't
     * complain much since it's documented in the Tk_CreateItemType
     * manual page.
     *
     * "These fields give a bounding box for the items using integer
     * canvas coordinates: the item should not cover any pixels with
     * x-coordinate lower than x1 or y-coordinate lower than y1, nor
     * should it cover any pixels with x-coordinate greater than or
     * equal to x2 or y-coordinate greater than or equal to y2."
     */
    epsPtr->xMax = x + epsPtr->width;
    epsPtr->yMax = y + epsPtr->height;
}

/*
 *----------------------------------------------------------------------
 *
 * DisplayEps --
 *
 *	This procedure is invoked to draw the EPS item in a
 *	given drawable.  The EPS item may be drawn as either
 *	a solid rectangle or a pixmap of the preview image.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	ItemPtr is drawn in drawable using the transformation
 *	information in canvas.
 *
 *----------------------------------------------------------------------
 */
static void
DisplayEps(canvas, itemPtr, display, drawable, x, y, width, height)
    Tk_Canvas canvas;		/* Canvas that contains item. */
    Tk_Item *itemPtr;		/* Item to be displayed. */
    Display *display;		/* Display on which to draw item. */
    Drawable drawable;		/* Pixmap or window in which to draw
				 * item. */
    int x, y, width, height;	/* Describes region of canvas that
				 * must be redisplayed (not used). */
{
    Tk_Window tkwin;
    EpsItem *epsPtr = (EpsItem *)itemPtr;
    short int drawableX, drawableY;
    char *title;
    int twiceBW;
    int noImage;

    if ((epsPtr->width < 1) || (epsPtr->height < 1)) {
	return;
    }
    tkwin = Tk_CanvasTkwin(canvas);
    epsPtr->showImage = TRUE;
    if ((epsPtr->showImage) && (epsPtr->colorImage != NULL) &&
	((epsPtr->lastWidth != epsPtr->width) ||
	    (epsPtr->lastHeight != epsPtr->height))) {
	ImageRegion srcRegion, destRegion;
	ColorImage image;

	srcRegion.x = srcRegion.y = 0;
	srcRegion.width = ColorImageWidth(epsPtr->colorImage);
	srcRegion.height = ColorImageHeight(epsPtr->colorImage);
	destRegion.x = destRegion.y = 0;
	destRegion.width = epsPtr->width;
	destRegion.height = epsPtr->height;
	if (epsPtr->quick) {
	    image = Blt_ResizeColorImage(epsPtr->colorImage, &srcRegion,
		&destRegion);
	} else {
	    image = Blt_ResampleColorImage(epsPtr->colorImage, &srcRegion,
		&destRegion, bltBoxFilter, bltBoxFilter);
	}
	if (epsPtr->photo != NULL) {
	    /*
	     * Resize the Tk photo image used to represent the EPS item.
	     *
	     * We will over-write the current image with a resampled one.
	     * This can be screwy, especially if someone is sharing the
	     * image.  But it's this or force the user to create another
	     * photo image for us.
	     */
	    Blt_ColorImageToPhoto(image, epsPtr->photo);
	} else {
#ifdef notdef
	    epsPtr->pixmap = Blt_ColorImageToPixmap(epsPtr->interp, tkwin,
		image, &(epsPtr->colorTable));
#endif
	}
	epsPtr->lastHeight = epsPtr->height;
	epsPtr->lastWidth = epsPtr->width;
	Blt_FreeColorImage(image);
    }
    /*
     * Translate the coordinates to those of the EPS item, then redisplay it.
     */
    Tk_CanvasDrawableCoords(canvas, (double)epsPtr->canvasX,
	(double)epsPtr->canvasY, &drawableX, &drawableY);
    x = (int)drawableX;
    y = (int)drawableY;

    twiceBW = epsPtr->borderWidth * 2;
    title = epsPtr->title;

    if (epsPtr->reqTitle != NULL) {
	title = epsPtr->reqTitle;
    }
    width = epsPtr->width;
    height = epsPtr->height;
    noImage = ((!epsPtr->showImage) || ((epsPtr->tkImage == NULL) &&
	    (epsPtr->pixmap == None)));
    if (noImage) {
	if ((twiceBW >= width) || (twiceBW >= height)) {
	    return;
	}
	width -= twiceBW;
	height -= twiceBW;
	if (epsPtr->fillGC != NULL) {
	    XSetTSOrigin(display, epsPtr->fillGC, x, y);
	    XFillRectangle(display, drawable, epsPtr->fillGC, x, y,
		epsPtr->width, epsPtr->height);
	    XSetTSOrigin(display, epsPtr->fillGC, 0, 0);
	}
    } else {
	if (epsPtr->pixmap != None) {
	    XCopyArea(Tk_Display(tkwin), epsPtr->pixmap, drawable,
		epsPtr->fillGC, 0, 0, width, height, x, y);
	} else {
	    Tk_RedrawImage(epsPtr->tkImage, 0, 0, width, height, drawable,
		x, y);
	}
    }

    if (title != NULL) {
	CompoundText *textPtr;
	int rotWidth, rotHeight;

	/* Translate the title to an anchor position within the EPS item */
	textPtr = Blt_GetCompoundText(title, &(epsPtr->titleAttr));
	Blt_GetBoundingBox(textPtr->width, textPtr->height,
	    epsPtr->titleAttr.theta, &rotWidth, &rotHeight, (XPoint *)NULL);
	if ((rotWidth <= width) && (rotHeight <= height)) {
	    int titleX, titleY;

	    Blt_TranslateAnchor(x, y, width, height, epsPtr->titleAttr.anchor,
		&titleX, &titleY);
	    if (noImage) {
		titleX += epsPtr->borderWidth;
		titleY += epsPtr->borderWidth;
	    }
	    Blt_DrawText(tkwin, drawable, title, &(epsPtr->titleAttr), titleX,
		titleY);
	}
    }
    if ((noImage) && (epsPtr->border != NULL)) {
	Tk_Draw3DRectangle(tkwin, drawable, epsPtr->border, x, y,
	    epsPtr->width, epsPtr->height, epsPtr->borderWidth, epsPtr->relief);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * EpsToPoint --
 *
 *	Computes the distance from a given point to a given
 *	rectangle, in canvas units.
 *
 * Results:
 *	The return value is 0 if the point whose x and y coordinates
 *	are coordPtr[0] and coordPtr[1] is inside the EPS item.  If the
 *	point isn't inside the item then the return value is the
 *	distance from the point to the EPS item.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static double
EpsToPoint(canvas, itemPtr, coordArr)
    Tk_Canvas canvas;		/* Canvas containing item. */
    Tk_Item *itemPtr;		/* Item to check against point. */
    double *coordArr;		/* Pointer to x and y coordinates. */
{
    EpsItem *epsPtr = (EpsItem *)itemPtr;
    double dx, dy;

    /*
     * Point is outside rectangle.
     */
    if (coordArr[0] < epsPtr->xMin) {
	dx = epsPtr->xMin - coordArr[0];
    } else if (coordArr[0] > epsPtr->xMax) {
	dx = coordArr[0] - epsPtr->xMax;
    } else {
	dx = 0;
    }
    if (coordArr[1] < epsPtr->yMin) {
	dy = epsPtr->yMin - coordArr[1];
    } else if (coordArr[1] > epsPtr->yMax) {
	dy = coordArr[1] - epsPtr->yMax;
    } else {
	dy = 0;
    }
    return hypot(dx, dy);
}

/*
 *----------------------------------------------------------------------
 *
 * EpsToArea --
 *
 *	This procedure is called to determine whether an item
 *	lies entirely inside, entirely outside, or overlapping
 *	a given rectangle.
 *
 * Results:
 *	-1 is returned if the item is entirely outside the area
 *	given by rectPtr, 0 if it overlaps, and 1 if it is entirely
 *	inside the given area.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
/*ARGSUSED*/
static int
EpsToArea(canvas, itemPtr, rectArr)
    Tk_Canvas canvas;		/* Canvas containing item. */
    Tk_Item *itemPtr;		/* Item to check against rectangle. */
    double *rectArr;		/* Pointer to array of four coordinates
				 * (x1, y1, x2, y2) describing rectangular
				 * area.  */
{
    EpsItem *epsPtr = (EpsItem *)itemPtr;

    if ((rectArr[2] <= epsPtr->xMin) || (rectArr[0] >= epsPtr->xMax) ||
	(rectArr[3] <= epsPtr->yMin) || (rectArr[1] >= epsPtr->yMax)) {
	return -1;
    }
    if ((rectArr[0] <= epsPtr->xMin) && (rectArr[1] <= epsPtr->yMin) &&
	(rectArr[2] >= epsPtr->xMax) && (rectArr[3] >= epsPtr->yMax)) {
	return 1;
    }
    return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * ScaleEps --
 *
 *	This procedure is invoked to rescale an item.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The item referred to by itemPtr is rescaled so that the
 *	following transformation is applied to all point coordinates:
 *		x' = originX + scaleX*(x-originX)
 *		y' = originY + scaleY*(y-originY)
 *
 *----------------------------------------------------------------------
 */
static void
ScaleEps(canvas, itemPtr, originX, originY, scaleX, scaleY)
    Tk_Canvas canvas;		/* Canvas containing rectangle. */
    Tk_Item *itemPtr;		/* Rectangle to be scaled. */
    double originX, originY;	/* Origin about which to scale rect. */
    double scaleX;		/* Amount to scale in X direction. */
    double scaleY;		/* Amount to scale in Y direction. */
{
    EpsItem *epsPtr = (EpsItem *)itemPtr;

    epsPtr->x = originX + scaleX * (epsPtr->x - originX);
    epsPtr->y = originY + scaleY * (epsPtr->y - originY);
    ComputeEpsBbox(canvas, epsPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * TranslateEps --
 *
 *	This procedure is called to move an item by a given amount.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The position of the item is offset by (xDelta, yDelta), and
 *	the bounding box is updated in the generic part of the item
 *	structure.
 *
 *----------------------------------------------------------------------
 */
static void
TranslateEps(canvas, itemPtr, deltaX, deltaY)
    Tk_Canvas canvas;		/* Canvas containing item. */
    Tk_Item *itemPtr;		/* Item that is being moved. */
    double deltaX, deltaY;	/* Amount by which item is to be
				 * moved. */
{
    EpsItem *epsPtr = (EpsItem *)itemPtr;

    epsPtr->x += deltaX;
    epsPtr->y += deltaY;
    ComputeEpsBbox(canvas, epsPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * EpsToPostscript --
 *
 *	This procedure is called to generate Postscript for EPS
 *	canvas items.
 *
 * Results:
 *	The return value is a standard Tcl result.  If an error
 *	occurs in generating Postscript then an error message is
 *	left in interp->result, replacing whatever used
 *	to be there.  If no error occurs, then Postscript for the
 *	item is appended to the result.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static int
EpsToPostScript(interp, canvas, itemPtr, prepass)
    Tcl_Interp *interp;		/* Leave Postscript or error message
				 * here. */
    Tk_Canvas canvas;		/* Information about overall canvas. */
    Tk_Item *itemPtr;		/* Item for which Postscript is
				 * wanted. */
    int prepass;		/* 1 means this is a prepass to
				 * collect font information;  0 means
				 * final Postscript is being created. */
{
    EpsItem *epsPtr = (EpsItem *)itemPtr;
    Tcl_DString dString;
    Printable printable;
    Tk_Window tkwin;
    float xScale, yScale;
    int x, y, width, height;
    int lineNum;

    if (prepass) {
	return TCL_OK;
    }
    tkwin = Tk_CanvasTkwin(epsPtr->canvas);
    Tcl_DStringInit(&dString);
    printable = Blt_PrintObject(interp, tkwin, &dString);
    x = epsPtr->canvasX;
    y = (int)Tk_CanvasPsY(canvas, (double)epsPtr->canvasY + epsPtr->height);

    if (epsPtr->fileName == NULL) {
	if (epsPtr->photo != NULL) {
	    /* No PostScript file, generate PostScript of image instead. */
	    Blt_PrintFormat(printable, "gsave\n");

	    /*
	     * First flip the PostScript y-coordinate axis so that the
	     * origin is the upper-left corner like our color image.
	     */
	    Blt_PrintFormat(printable, "  %d %d translate\n",
		x, y + epsPtr->height);
	    Blt_PrintFormat(printable, "  1 -1 scale\n");

	    Blt_PhotoToPostScript(printable, epsPtr->photo, 0, 0);
	    Blt_PrintFormat(printable, "grestore\n");

	    Tcl_AppendResult(interp, Tcl_DStringValue(&dString), (char *)NULL);
	    Tcl_DStringFree(&dString);
	    free((char *)printable);
	}
	return TCL_OK;
    }
    if (epsPtr->filePtr == NULL) {
	Tcl_AppendResult(interp, "can't get handle to EPS file", (char *)NULL);
	goto error;
    }
    /* Copy in the PostScript prolog for EPS encapsulation. */

    if (Blt_FileToPostScript(printable, "bltCanvEps.pro") != TCL_OK) {
	goto error;
    }
    Blt_PrintAppend(printable, "BeginEPSF\n", (char *)NULL);

    width = epsPtr->width;
    height = epsPtr->height;
    xScale = (float)width / (float)(epsPtr->urx - epsPtr->llx);
    yScale = (float)height / (float)(epsPtr->ury - epsPtr->lly);

    /* Set up scaling and translation transformations for the EPS item */

    Blt_PrintFormat(printable, "%d %d translate\n", x, y);
    Blt_PrintFormat(printable, "%g %g scale\n", xScale, yScale);
    Blt_PrintFormat(printable, "%d %d translate\n", -(epsPtr->llx),
	-(epsPtr->lly));
    Blt_PrintFormat(printable, "%d %d %d %d SetClipRegion\n", epsPtr->llx,
	epsPtr->lly, epsPtr->urx, epsPtr->ury);
    rewind(epsPtr->filePtr);
    Blt_PrintAppend(printable, "%% including \"", epsPtr->fileName, "\"\n\n",
	(char *)NULL);
    lineNum = 0;
    while (fgets(printable->scratchArr, PRINTABLE_SCRATCH_LENGTH,
	    epsPtr->filePtr) != NULL) {
	lineNum++;
	if ((lineNum > epsPtr->lastLine) || (lineNum < epsPtr->firstLine)) {
	    Tcl_DStringAppend(&dString, printable->scratchArr, -1);
	}
    }
    if (ferror(epsPtr->filePtr)) {
	Tcl_AppendResult(interp, "error reading EPS file \"", epsPtr->fileName,
	    "\": ", Tcl_PosixError(interp), (char *)NULL);
	fclose(epsPtr->filePtr);
	epsPtr->filePtr = NULL;
	goto error;
    }
    Blt_PrintAppend(printable, "EndEPSF\n", (char *)NULL);
    Tcl_AppendResult(interp, Tcl_DStringValue(&dString), (char *)NULL);
    Tcl_DStringFree(&dString);
    free((char *)printable);
    return TCL_OK;

  error:
    Tcl_DStringFree(&dString);
    free((char *)printable);
    return TCL_ERROR;
}

/*
 * The structures below defines the EPS item type in terms of
 * procedures that can be invoked by generic item code.
 */
static Tk_ItemType epsItemType =
{
    "eps",			/* name */
    sizeof(EpsItem),		/* itemSize */
    CreateEps,			/* createProc */
    configSpecs,		/* configSpecs */
    ConfigureEps,		/* configureProc */
    EpsCoords,			/* coordProc */
    DeleteEps,			/* deleteProc */
    DisplayEps,			/* displayProc */
    0,				/* alwaysRedraw */
    EpsToPoint,			/* pointProc */
    EpsToArea,			/* areaProc */
    EpsToPostScript,		/* postscriptProc */
    ScaleEps,			/* scaleProc */
    TranslateEps,		/* translateProc */
    (Tk_ItemIndexProc *) NULL,	/* indexProc */
    (Tk_ItemCursorProc *) NULL,	/* icursorProc */
    (Tk_ItemSelectionProc *) NULL,	/* selectionProc */
    (Tk_ItemInsertProc *) NULL,	/* insertProc */
    (Tk_ItemDCharsProc *) NULL,	/* dTextProc */
    (Tk_ItemType *) NULL	/* nextPtr */
};

/*ARGSUSED*/
void
Blt_InitEpsCanvasItem(interp)
    Tcl_Interp *interp;		/* Not used */
{
    Tk_CreateItemType(&epsItemType);
}
