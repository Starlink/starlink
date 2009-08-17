/*
 * Copyright (c) 1991-1994 The Regents of the University of California.
 * Copyright (c) 1994-1995 Sun Microsystems, Inc.
 * Copyright (c) 1997-2000 Central Laboratory of the Research Councils
 * Copyright (c) 2006      Particle Physics and Astronomy Reseach Council
 *
 *
 * See the Tcl distribution file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

/*+
 *  Name:
 *     rtdMark.c
 *
 *  Purpose:
 *     This file implements a set of simple markers that can be
 *     used on a canvas.
 *
 *  Notes:
 *     This item offers the following marker types of rtd_mark:
 *        dot
 *        plus
 *        cross
 *        square
 *        circle
 *        triangle
 *        diamond
 *
 *     All centered on the given position.
 *
 *  Authors:
 *     PWD: P.W. Draper (Durham University, U.K).
 *
 *  Changes:
 *     30-JUL-1997 (PWD):
 *        Original version, based on rtdWord.
 *     28-MAR-2000 (PWD):
 *        Added -scale as a configuration option. Fixing this
 *        allows creation at different "scales" to be harmonized.
 *     03-APR-2006 (PWD):
 *        Added -fixscale to switch off scaling of size by scale method.
 *     19-JUL-2006 (PWD):
 *        Added -minscale to limit the possible scale to some value.
 *-
 *.
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include "tk.h"
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "rotated.h"

#define MAX(a,b) ( (a) > (b) ? (a) : (b) )
#define MIN(a,b) ( (a) < (b) ? (a) : (b) )

/*  Define the possible marker types */
typedef enum {
    DOT, PLUS, CROSS, SQUARE, CIRCLE, TRIANGLE, DIAMOND
} Marker;

/*
 * Define a structure for containing all the necessary information
 * to describe an marker.
 */

typedef struct MarkItem  {
    Tk_Item header;                /* Mandatory Tk header information */
    GC fillGC;                     /* Graphics context */
    GC outlineGC;                  /* Graphics context */
    Marker shape;                  /* The type of marker */
    Pixmap fillStipple;            /* Stipple bitmap (if used) */
    Tk_Anchor tkanchor;            /* Where to anchor marker relative to (x,y). */
    XColor *fillColor;             /* Colour of fill (if used) */
    XColor *outlineColor;          /* Colour of lines*/
    char *type;                    /* The type of marker as a string */
    double scale;                  /* Current scale factor */
    double minscale;               /* Minimum possible scale factor */
    double realscale;              /* Current scale factor, regardless of
                                    * minscale setting */
    double x, y;                   /* Coordinates of reference point */
    int fixscale;                  /* Whether to scale the scale value */
    int size;                      /* Size of marker */
    int width;                     /* Width of marker */
} MarkItem;

/*
 * Set the configuration options.
 */

static Tk_CustomOption tagsOption = {
    Tk_CanvasTagsParseProc, Tk_CanvasTagsPrintProc, (ClientData) NULL
};

static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_ANCHOR, "-anchor", (char *) NULL, (char *) NULL, "e", 
                       Tk_Offset(MarkItem, tkanchor), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_BITMAP, "-stipple", (char *) NULL, (char *) NULL, (char *) NULL, 
                       Tk_Offset(MarkItem, fillStipple), TK_CONFIG_NULL_OK},
    {TK_CONFIG_BOOLEAN, "-fixscale", (char *) NULL, (char *) NULL, "0", 
                        Tk_Offset(MarkItem, fixscale), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_COLOR, "-fill", (char *) NULL, (char *) NULL, (char *) NULL, 
                      Tk_Offset(MarkItem, fillColor), TK_CONFIG_NULL_OK},
    {TK_CONFIG_DOUBLE, "-minscale", (char *) NULL, (char *) NULL, "-1", 
                       Tk_Offset(MarkItem, minscale), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_COLOR, "-outline", (char *) NULL, (char *) NULL, "black", 
                      Tk_Offset(MarkItem, outlineColor), 0},
    {TK_CONFIG_CUSTOM, "-tags", (char *) NULL, (char *) NULL, (char *) NULL, 
                       0, TK_CONFIG_NULL_OK, &tagsOption},
    {TK_CONFIG_DOUBLE, "-scale", (char *) NULL, (char *) NULL, "1", 
                       Tk_Offset(MarkItem, scale), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_INT, "-size", (char *) NULL, (char *) NULL, "1", 
                    Tk_Offset(MarkItem, size), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_INT, "-width", (char *) NULL, (char *) NULL, "1", 
                    Tk_Offset(MarkItem, width), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_STRING, "-type", (char *) NULL, (char *) NULL, "circle", 
                       Tk_Offset(MarkItem, type), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
                    (char *) NULL, 0, 0}
};

/*
 * Prototypes for procedures defined in this file:
 */

static void ComputeMarkBbox(Tk_Canvas canvas, MarkItem *markPtr);

static int ConfigureMark(Tcl_Interp *interp, Tk_Canvas canvas,
                         Tk_Item *itemPtr, int objc,
                         Tcl_Obj *CONST argv[], int flags);

static int CreateMark(Tcl_Interp *interp, Tk_Canvas canvas,
                      struct Tk_Item *itemPtr,
                      int objc, Tcl_Obj *CONST objv[]);

static void DeleteMark(Tk_Canvas canvas, Tk_Item *itemPtr, Display *display);

static void ScaleMark(Tk_Canvas canvas, Tk_Item *itemPtr, double originX,
                      double originY, double scaleX, double scaleY);

static int MarkCoords(Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
                      int objc, Tcl_Obj *CONST argv[]);

static void DisplayMark(Tk_Canvas canvas, Tk_Item *itemPtr, Display *display,
                        Drawable dst, int x, int y, int width, int height);

static int MarkToArea(Tk_Canvas canvas, Tk_Item *itemPtr, double *areaPtr);

static double MarkToPoint(Tk_Canvas canvas, Tk_Item *itemPtr,
                          double *pointPtr);

static void TranslateMark(Tk_Canvas canvas, Tk_Item *itemPtr, double deltaX,
                          double deltaY);

static int MarkToPostscript(Tcl_Interp *interp, Tk_Canvas canvas,
                            Tk_Item *itemPtr, int prepass);

static void MarkCorrectAnchor(MarkItem *markPtr, double *x, double *y);

/*
 * The structure below defines the marker item type, by means of
 * procedures that can be invoked by generic item code.
 */

static Tk_ItemType rtdMarkType = {
    "rtd_mark",                    /* name           */
    sizeof(MarkItem),              /* itemSize       */
    CreateMark,                    /* createProc     */
    configSpecs,                   /* configSpecs    */
    ConfigureMark,                 /* configureProc  */
    MarkCoords,                    /* coordProc      */
    DeleteMark,                    /* deleteProc     */
    DisplayMark,                   /* displayProc    */
    TK_CONFIG_OBJS,                /* alwaysRedraw & flags  */
    MarkToPoint,                   /* pointProc      */
    MarkToArea,                    /* areaProc       */
    MarkToPostscript,              /* postscriptProc */
    ScaleMark,                     /* scaleProc      */
    TranslateMark,                 /* translateProc  */
    (Tk_ItemIndexProc *) NULL,     /* indexProc      */
    (Tk_ItemCursorProc *) NULL,    /* cursorProc     */
    (Tk_ItemSelectionProc *) NULL, /* selectionProc  */
    (Tk_ItemInsertProc *) NULL,    /* insertProc     */
    (Tk_ItemDCharsProc *) NULL,    /* dTextProc      */
    (Tk_ItemType *) NULL           /* nextPtr        */
};

/*
 *--------------------------------------------------------------
 * Mark_Init --
 *
 *   This procedure initialises the rtd_mark canvas item.
 *
  *--------------------------------------------------------------
 *
 */
int Mark_Init()
{
    Tk_CreateItemType(&rtdMarkType);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * CreateMark --
 *
 *      This procedure is invoked to create a marker item.
 *
 * Results:
 *      A standard Tcl return value.  If an error occurred in
 *      creating the item, then an error message is left in
 *      interp->result;  in this case itemPtr is left uninitialized,
 *      so it can be safely freed by the caller.
 *
 * Side effects:
 *      A new marker item is created.
 *
 *--------------------------------------------------------------
 */

static int CreateMark( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
                       int objc, Tcl_Obj *CONST objv[] )
{
    MarkItem *markPtr = (MarkItem *) itemPtr;
    if (objc < 2) {
        Tcl_AppendResult(interp, "wrong # args:  should be \"",
                         Tk_PathName(Tk_CanvasTkwin(canvas)), "\" create ",
                         itemPtr->typePtr->name, " xcenter ycenter ?options?",
                         (char *) NULL);
        return TCL_ERROR;
    }

    /*
     * Carry out initialization that is needed in order to clean
     * up after errors during the the remainder of this procedure.
     */

    markPtr->fillColor = NULL;
    markPtr->fillGC = None;
    markPtr->fillStipple = None;
    markPtr->fixscale = 0;
    markPtr->outlineColor = NULL;
    markPtr->outlineGC = None;
    markPtr->scale = 1.0;
    markPtr->realscale = 1.0;
    markPtr->minscale = -1.0;
    markPtr->shape = CIRCLE;
    markPtr->size = 7;
    markPtr->tkanchor = TK_ANCHOR_CENTER;
    markPtr->type = NULL;
    markPtr->width = 1;

    /*
     * Process the arguments to fill in the item record.
     */
    if ((Tk_CanvasGetCoordFromObj(interp, canvas, objv[0], &markPtr->x) != TCL_OK) ||
        (Tk_CanvasGetCoordFromObj(interp, canvas, objv[1], &markPtr->y) != TCL_OK) ){
        return TCL_ERROR;
    }

    /*  And configure using any options */
    if (ConfigureMark(interp, canvas, itemPtr, objc-2, objv+2, 0) != TCL_OK) {
        DeleteMark(canvas, itemPtr, Tk_Display(Tk_CanvasTkwin(canvas)));
        return TCL_ERROR;
    }
    return TCL_OK;
}


/*
 *--------------------------------------------------------------
 *
 * MarkCoords --
 *
 *   This procedure is invoked to process the "coords" widget command.
 *   See the user documentation for details on what it does.
 *
 * Results:
 *      Returns TCL_OK or TCL_ERROR, and sets interp->result.
 *
 * Side effects:
 *      The coordinates for the given item may be changed.
 *
 *--------------------------------------------------------------
 */
static int MarkCoords( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
                       int objc, Tcl_Obj *CONST objv[] )
{
    MarkItem *markPtr = (MarkItem *) itemPtr;
    char x[TCL_DOUBLE_SPACE], y[TCL_DOUBLE_SPACE];

    if (objc == 0) {
        Tcl_PrintDouble(interp, markPtr->x, x);
        Tcl_PrintDouble(interp, markPtr->y, y);
        Tcl_AppendResult(interp, x, " ", y, " ", (char *) NULL);
    }
    else if (objc == 2) {
        if ((Tk_CanvasGetCoordFromObj(interp, canvas, objv[0], &markPtr->x) != TCL_OK) ||
            (Tk_CanvasGetCoordFromObj(interp, canvas, objv[1], &markPtr->y) != TCL_OK)) {
            return TCL_ERROR;
        }
        ComputeMarkBbox(canvas, markPtr);
    }
    else {
        char buffer[80];
        sprintf(buffer,
                "wrong # coordinates:  expected 0 or 2, got %d", objc);
        Tcl_SetResult( interp, buffer, TCL_VOLATILE );
        return TCL_ERROR;
    }
    return TCL_OK;
}


/*
 *--------------------------------------------------------------
 *
 * ConfigureMark --
 *
 *      This procedure is invoked to configure various aspects of a
 *      marker, such as its type, size, width and color.
 *
 * Results:
 *      A standard Tcl result code.  If an error occurs, then
 *      an error message is left in interp->result.
 *
 * Side effects:
 *      Configuration information, such as colors and stipple
 *      patterns, may be set for itemPtr.
 *
 *--------------------------------------------------------------
 */
static int ConfigureMark( Tcl_Interp *interp, Tk_Canvas canvas,
                          Tk_Item *itemPtr, int objc, Tcl_Obj *CONST objv[],
                          int flags )
{
    MarkItem *markPtr = (MarkItem *) itemPtr;
    XGCValues gcValues;
    GC newGC;
    unsigned long mask;
    Tk_Window tkwin;
    int length;

    tkwin = Tk_CanvasTkwin(canvas);
    if (Tk_ConfigureWidget(interp, tkwin, configSpecs, objc,
                           (char CONST **)objv, (char *) markPtr,
                           flags|TK_CONFIG_OBJS) != TCL_OK) {
        return TCL_ERROR;
    }

    /*
     * A few of the options require additional processing, such as
     * graphics contexts.
     */

    /* Outline GC */
    if (markPtr->outlineColor == NULL) {
        newGC = None;
    }
    else {
        gcValues.foreground = markPtr->outlineColor->pixel;
        gcValues.line_width = markPtr->width;
        mask = GCForeground|GCLineWidth;
        newGC = Tk_GetGC(tkwin, mask, &gcValues);
    }
    if (markPtr->outlineGC != None) {
        Tk_FreeGC(Tk_Display(tkwin), markPtr->outlineGC);
    }
    markPtr->outlineGC = newGC;

    /* fill GC */
    if (markPtr->fillColor == NULL) {
        newGC = None;
    }
    else {
        gcValues.foreground = markPtr->fillColor->pixel;
        if (markPtr->fillStipple != None) {
            gcValues.stipple = markPtr->fillStipple;
            gcValues.fill_style = FillStippled;
            mask = GCForeground|GCStipple|GCFillStyle;
        }
        else {
            mask = GCForeground;
        }
        newGC = Tk_GetGC(tkwin, mask, &gcValues);
    }
    if (markPtr->fillGC != None) {
        Tk_FreeGC(Tk_Display(tkwin), markPtr->fillGC);
    }
    markPtr->fillGC = newGC;

    /* Need to translate the type string into an internal marker type */

    length = strlen(markPtr->type);
    if ((markPtr->type[0] == 'd') &&
        (strncmp(markPtr->type, "dot", length) == 0)) {
        markPtr->shape = DOT;
    }
    else if ((markPtr->type[0] == 'c') &&
               (strncmp(markPtr->type, "circle", length) == 0)) {
        markPtr->shape = CIRCLE;
    }
    else if ((markPtr->type[0] == 's')) {
        markPtr->shape = SQUARE;
    }
    else if ((markPtr->type[0] == 'd') && (length > 1) &&
               (strncmp(markPtr->type, "diamond", length) == 0)) {
        markPtr->shape = DIAMOND;
    }
    else if ((markPtr->type[0] == 'p')) {
        markPtr->shape = PLUS;
    }
    else if ((markPtr->type[0] == 'c') && (length > 1) &&
             (strncmp(markPtr->type, "cross", length) == 0)) {
        markPtr->shape = CROSS;
    }
    else if ((markPtr->type[0] == 't') ) {
        markPtr->shape = TRIANGLE;
    }
    else {
        Tcl_AppendResult(interp, "bad marker type \"", markPtr->type,
                         "\": should be dot, circle, square, diamond, plus, cross, or triangle",
                         (char *)NULL);
        return TCL_ERROR;
    }

    ComputeMarkBbox(canvas, markPtr);
    return TCL_OK;
}


/*
 *--------------------------------------------------------------
 *
 * DeleteMark --
 *
 *      This procedure is called to clean up the data structure
 *      associated with a marker.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      Resources associated with itemPtr are released.
 *
 *--------------------------------------------------------------
 */

static void DeleteMark( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display )
{
    MarkItem *markPtr = (MarkItem *) itemPtr;

    if (markPtr->outlineColor != NULL) {
        Tk_FreeColor(markPtr->outlineColor);
    }
    if (markPtr->fillColor != NULL) {
        Tk_FreeColor(markPtr->fillColor);
    }
    if (markPtr->fillStipple != None) {
        Tk_FreeBitmap(display, markPtr->fillStipple);
    }
    if (markPtr->outlineGC != None) {
        Tk_FreeGC(display, markPtr->outlineGC);
    }
    if (markPtr->fillGC != None) {
        Tk_FreeGC(display, markPtr->fillGC);
    }
}

/*
 *--------------------------------------------------------------
 *
 * ComputeMarkBbox --
 *
 *      This procedure is invoked to compute the bounding box of
 *      all the pixels that may be drawn as part of a marker.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The fields x1, y1, x2, and y2 are updated in the header
 *      for itemPtr.
 *
 *--------------------------------------------------------------
 */
static void ComputeMarkBbox( Tk_Canvas canvas, MarkItem *markPtr )
{
    int sby2;
    double x, y;

    /*  Correct for anchor positioning */
    x = markPtr->x;
    y = markPtr->y;
    MarkCorrectAnchor( markPtr, &x, &y );

    /*  Work out scale corrected size for marker boxes */
    sby2 = (int) (markPtr->scale * (double) markPtr->size/ 2.0);

    /*  Update header, correcting for width plus a bit */
    markPtr->header.x1 = (int) x - sby2 - markPtr->width - 2;
    markPtr->header.y1 = (int) y - sby2 - markPtr->width - 2;
    markPtr->header.x2 = (int) x + sby2 + markPtr->width + 2;
    markPtr->header.y2 = (int) y + sby2 + markPtr->width + 2;
}


/*
 *--------------------------------------------------------------
 *
 * DisplayMark --
 *
 *      This procedure is invoked to draw a marker item in a given drawable.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      ItemPtr is drawn in drawable using the transformation
 *      information in canvas.
 *
 *--------------------------------------------------------------
 */
static void DisplayMark( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display,
                         Drawable drawable, int xcan, int ycan, int width,
                         int height )
{
    MarkItem *markPtr = (MarkItem *) itemPtr;
    short drawableX, drawableY;
    int size, sby2;
    double x, y;

    /* Use the current scale factor and size to derive a meaningful
       number to size the markers at */
    size = (int) (markPtr->scale * markPtr->size);
    if ( size < 1 ) {
        size = 1;
    }
    sby2 = size/2;

    /*
     * If we're stippling, then modify the stipple offset in the GC.  Be
     * sure to reset the offset when done, since the GC is supposed to be
     * read-only.
     */
    if (markPtr->fillStipple != None) {
        Tk_CanvasSetStippleOrigin(canvas, markPtr->fillGC);
    }

    /* Add correction to x and y for anchor positions. */
    x = markPtr->x;
    y = markPtr->y;
    MarkCorrectAnchor( markPtr, &x, &y );

    /* Transform coordinates into drawable from canvas */
    Tk_CanvasDrawableCoords(canvas, x, y, &drawableX, &drawableY);

    /* Draw the required marker. */
    switch (markPtr->shape) {
    case DOT:
        {
            /*  Draw a single point for a DOT no scale or size  allowed. */
            if ( markPtr->outlineGC != None ) {
                XDrawPoint( display, drawable, markPtr->outlineGC, drawableX,
                            drawableY );
            }
        }
        break;
    case SQUARE:
        {
            /*  Draw a single point for size == 1 otherwise draw a rectangle */
            if ( size == 1 ) {
                if ( markPtr->outlineGC != None ) {
                    XDrawPoint( display, drawable, markPtr->outlineGC, drawableX,
                                drawableY );
                }
            }
            else {
                int x, y;
                unsigned int width, height;
                height = width = size;
                x = drawableX - sby2;
                y = drawableY - sby2;
                if ( markPtr->fillGC != None ) {
                    XFillRectangle( display, drawable, markPtr->fillGC, x, y,
                                    width, height);
                }
                if ( markPtr->outlineGC != None ) {
                    XDrawRectangle( display, drawable, markPtr->outlineGC, x, y,
                                    width, height);
                }
            }
        }
        break;
    case PLUS:
        {
            XSegment segArr[2];
            segArr[0].x1 = segArr[0].x2 = (int) drawableX;
            segArr[0].y1 = drawableY - sby2;
            segArr[0].y2 = drawableY + sby2 + 1;

            segArr[1].x1 = drawableX - sby2;
            segArr[1].x2 = drawableX + sby2 + 1;
            segArr[1].y1 = segArr[1].y2 = (int) drawableY;

            if ( markPtr->outlineGC != None ) {
                XDrawSegments( display, drawable, markPtr->outlineGC, segArr, 2);
            }
        }
        break;
    case CROSS:
        {
            XSegment segArr[2];
            segArr[0].x1 = (int) drawableX - sby2;
            segArr[0].y1 = (int) drawableY - sby2;
            segArr[0].x2 = (int) drawableX + sby2 + 1;
            segArr[0].y2 = (int) drawableY + sby2 + 1;

            segArr[1].x1 = (int) drawableX + sby2 + 1;
            segArr[1].y1 = (int) drawableY - sby2 - 1;
            segArr[1].x2 = (int) drawableX - sby2;
            segArr[1].y2 = (int) drawableY + sby2;

            if ( markPtr->outlineGC != None ) {
                XDrawSegments( display, drawable, markPtr->outlineGC, segArr, 2);
            }
        }
        break;
    case CIRCLE:
        {
            XArc arc;
            arc.x = drawableX - sby2;
            arc.y = drawableY - sby2;
            arc.width = size;
            arc.height = size;
            arc.angle1 = 0;
            arc.angle2 = 64 * 360;
            if (markPtr->fillGC != None) {
                XFillArcs( display, drawable, markPtr->fillGC, &arc, 1 );
            }
            if ( markPtr->outlineGC != None ) {
                XDrawArcs( display, drawable, markPtr->outlineGC, &arc, 1 );
            }
        }
        break;
    case TRIANGLE:
        {
            XPoint corners[4];
            corners[0].x = drawableX - sby2;
            corners[0].y = drawableY - sby2;
            corners[1].x = drawableX + sby2;
            corners[1].y = drawableY - sby2;
            corners[2].x = drawableX;
            corners[2].y = drawableY + sby2;
            corners[3].x = drawableX - sby2;
            corners[3].y = drawableY - sby2;
            if (markPtr->fillGC != None) {
                XFillPolygon( display, drawable, markPtr->fillGC, corners, 3,
                              Convex, CoordModeOrigin );
            }
            if ( markPtr->outlineGC != None ) {
                XDrawLines( display, drawable, markPtr->outlineGC, corners, 4,
                            CoordModeOrigin);
            }
        }
        break;
    case DIAMOND:
        {
            XPoint corners[5];
            corners[0].x = drawableX;
            corners[0].y = drawableY - sby2;
            corners[1].x = drawableX + sby2;
            corners[1].y = drawableY;
            corners[2].x = drawableX;
            corners[2].y = drawableY + sby2;
            corners[3].x = drawableX - sby2;
            corners[3].y = drawableY;
            corners[4].x = drawableX;
            corners[4].y = drawableY - sby2;
            if (markPtr->fillGC != None) {
                XFillPolygon( display, drawable, markPtr->fillGC, corners, 4,
                              Convex, CoordModeOrigin );
            }
            if ( markPtr->outlineGC != None ) {
                XDrawLines( display, drawable, markPtr->outlineGC, corners, 5,
                            CoordModeOrigin);
            }
        }
        break;
    }

    /* And transform GC coordinates back to normal */
    if (markPtr->fillStipple != None) {
        XSetTSOrigin(display, markPtr->fillGC, 0, 0);
    }
    ComputeMarkBbox(canvas, markPtr);
}


/*
 *--------------------------------------------------------------
 *
 * MarkToPoint --
 *
 *      Computes the distance from a given point to the marker.
 *
 * Results:
 *      The return value is 0 if the point whose x and y coordinates
 *      are coordPtr[0] and coordPtr[1] is inside the marker.  If the
 *      point isn't inside the marker then the return value is the
 *      distance from the point to the item.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */
static double MarkToPoint( Tk_Canvas canvas, Tk_Item *itemPtr,
                           double *pointPtr )
{
    MarkItem *markPtr = (MarkItem *) itemPtr;
    double xDiff, yDiff;

    /*
     * If the point is inside the mark's bounding box, then can
     * return immediately.
     */

    if ((pointPtr[0] >= markPtr->header.x1)
        && (pointPtr[0] <= markPtr->header.x2)
        && (pointPtr[1] >= markPtr->header.y1)
        && (pointPtr[1] <= markPtr->header.y2)) {
        return 0.0;
    }

    /*
     * Point is outside mark's bounding box; compute distance to nearest
     * side.
     */

    if (pointPtr[0] < markPtr->header.x1) {
        xDiff = markPtr->header.x1 - pointPtr[0];
    }
    else if (pointPtr[0] > markPtr->header.x2)  {
        xDiff = pointPtr[0] - markPtr->header.x2;
    }
    else {
        xDiff = 0.0;
    }

    if (pointPtr[1] < markPtr->header.y1) {
        yDiff = markPtr->header.y1 - pointPtr[1];
    }
    else if (pointPtr[1] > markPtr->header.y2)  {
        yDiff = pointPtr[1] - markPtr->header.y2;
    }
    else {
        yDiff = 0.0;
    }
    return sqrt(xDiff*xDiff+yDiff*yDiff);
}


/*
 *--------------------------------------------------------------
 *
 * MarkToArea --
 *
 *      This procedure is called to determine whether an item
 *      lies entirely inside, entirely outside, or overlapping
 *      a given rectangular area.
 *
 * Results:
 *      -1 is returned if the item is entirely outside the area
 *      given by areaPtr, 0 if it overlaps, and 1 if it is entirely
 *      inside the given area.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static int MarkToArea( Tk_Canvas canvas, Tk_Item *itemPtr, double *areaPtr )
{
    MarkItem *markPtr = (MarkItem *) itemPtr;

    if ((areaPtr[2] > markPtr->header.x1) &&
        (areaPtr[0] < markPtr->header.x2) &&
        (areaPtr[3] > markPtr->header.y1) &&
        (areaPtr[1] < markPtr->header.y2)) {
        return 1;
    }
    if ((areaPtr[0] > markPtr->header.x1) &&
        (areaPtr[1] > markPtr->header.y1) &&
        (areaPtr[0] < markPtr->header.x2) &&
        (areaPtr[1] < markPtr->header.y2) ) {
        return 0;

    }
    if ((areaPtr[2] > markPtr->header.x1) &&
        (areaPtr[3] > markPtr->header.y1) &&
        (areaPtr[2] < markPtr->header.x2) &&
        (areaPtr[3] < markPtr->header.y2) ) {
        return 0;
    }
    return -1;
}

/*
 *--------------------------------------------------------------
 *
 * ScaleMark --
 *
 *      This procedure is invoked to rescale a marker. If fixscale
 *      is set then only the position is scaled. If minscale is set greater
 *      than zero then scaleX is limited to that value.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The item referred to by itemPtr is rescaled
 *      so that the following transformation is applied to all
 *      point coordinates:
 *              x' = originX + scaleX*(x-originX)
 *              y' = originY + scaleY*(y-originY)
 *
 *      The mark is also scaled to scaleX, if fixscale is set.
 *
 *--------------------------------------------------------------
 */
static void ScaleMark( Tk_Canvas canvas, Tk_Item *itemPtr, double originX,
                       double originY, double scaleX, double scaleY )
{
    MarkItem *markPtr = (MarkItem *) itemPtr;

    /* Scale all coordinates and set their related values */

    markPtr->x = originX + scaleX*(markPtr->x - originX);
    markPtr->y = originY + scaleY*(markPtr->y - originY);
    if ( ! markPtr->fixscale ) {

        /*  realscale is the scale we would have without a minscale, 
         *  when it is less than minscale the scale used is minscale.
         *  This allows an absolute sense of what the scale is. */
        markPtr->realscale *= scaleX;

        if ( markPtr->minscale > 0.0 && 
             markPtr->realscale < markPtr->minscale ) {
            markPtr->scale = markPtr->minscale;
        }
        else {
            markPtr->scale = markPtr->realscale;
        }
    }
    ComputeMarkBbox(canvas, markPtr);
    return;
}

/*
 *--------------------------------------------------------------
 *
 * TranslateMark --
 *
 *      This procedure is called to move marker by a given amount.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The position of the mark is offset by (xDelta, yDelta), and
 *      the bounding box is updated in the generic part of the item structure.
 *
 *--------------------------------------------------------------
 */
static void TranslateMark( Tk_Canvas canvas, Tk_Item *itemPtr,
                           double deltaX,  double deltaY )
{
    MarkItem *markPtr = (MarkItem *) itemPtr;

    markPtr->x += deltaX;
    markPtr->y += deltaY;
    ComputeMarkBbox(canvas, markPtr);
}


/*
 *--------------------------------------------------------------
 *
 * MarkToPostscript --
 *
 *      This procedure is called to generate Postscript for
 *      the marker.
 *
 * Results:
 *      The return value is a standard Tcl result.  If an error
 *      occurs in generating Postscript then an error message is
 *      left in interp->result, replacing whatever used to be there.
 *      If no error occurs, then Postscript for the marker is
 *      appended to the result.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static int MarkToPostscript( Tcl_Interp *interp, Tk_Canvas canvas,
                             Tk_Item *itemPtr, int prepass )
{
    MarkItem *markPtr = (MarkItem *) itemPtr;
    char buffer[500], string[100];
    double size, sby2;
    double x, y;

    /*  No GC's with colour then nothing to do */
    if (markPtr->outlineColor == NULL && markPtr->fillColor == NULL) {
        return TCL_OK;
    }

    /* Use the current scale factor and size to derive a meaningful
       number to size the markers at */
    size = markPtr->scale * markPtr->size;
    if ( size < 1.0 ) {
        size = 1.0;
    }
    sby2 = size/2.0;

    /*  Convert the X and Y positions to postscript coordinates and
     *  adjust them for the anchor position */
    x = markPtr->x;
    y = markPtr->y;
    MarkCorrectAnchor( markPtr, &x, &y );
    y = Tk_CanvasPsY(canvas, y);

    /* Draw the required marker. */
    switch (markPtr->shape) {
    case DOT:
        {
            /*  Draw a single point for a DOT no scale or size  allowed. */
            if (markPtr->outlineColor != NULL) {
                sprintf(buffer, "%.15g %.15g moveto 1.0 0.0 rlineto 0.0 1.0 \
rlineto -1.0 0.0 rlineto closepath\n",
                        x-0.5, y-0.5 );
                Tcl_AppendResult(interp, buffer, (char *) NULL);
                if (Tk_CanvasPsColor(interp, canvas, markPtr->outlineColor)
                    != TCL_OK) {
                    return TCL_ERROR;
                }
                Tcl_AppendResult(interp, "stroke\n", (char *) NULL);
            }
        }
        break;
    case SQUARE:
        {
            /*  Construct a postscript command to draw the region */
            sprintf(buffer, "%.15g %.15g moveto %.15g 0.0 rlineto 0.0 %.15g \
rlineto %.15g 0.0 rlineto closepath\n",
                    x-sby2, y-sby2, size, size, -size );

            /*  Now draw the region filled if needed */
            if (markPtr->fillColor != NULL) {
                Tcl_AppendResult(interp, buffer, (char *) NULL);
                if (Tk_CanvasPsColor(interp, canvas, markPtr->fillColor) != TCL_OK) {
                    return TCL_ERROR;
                }
                if (markPtr->fillStipple != None) {
                    Tcl_AppendResult(interp, "clip ", (char *) NULL);
                    if (Tk_CanvasPsStipple(interp, canvas, markPtr->fillStipple)
                        != TCL_OK) {
                        return TCL_ERROR;
                    }
                    if (markPtr->outlineColor != NULL) {
                        Tcl_AppendResult(interp, "grestore gsave\n", (char *) NULL);
                    }
                }
                else {
                    Tcl_AppendResult(interp, "fill\n", (char *) NULL);
                }
            }
            /*  Now the outline (using the same region command). */
            if (markPtr->outlineColor != NULL) {
                Tcl_AppendResult(interp, buffer, (char *) NULL);

                /*  Set the outline width */
                sprintf(string, "%d setlinewidth", MAX(1,markPtr->width/2));
                Tcl_AppendResult(interp, string,
                                 " 0 setlinejoin 2 setlinecap\n", (char *) NULL);
                if (Tk_CanvasPsColor(interp, canvas, markPtr->outlineColor)
                    != TCL_OK) {
                    return TCL_ERROR;
                }
                Tcl_AppendResult(interp, "stroke\n", (char *) NULL);
            }
        }
        break;
    case PLUS:
        {
            if (markPtr->outlineColor != NULL) {
                sprintf(buffer, "%.15g %.15g moveto 0.0 %.15g rlineto closepath\n",
                        x, y-sby2, size );
                Tcl_AppendResult(interp, buffer, (char *) NULL);
                sprintf(buffer, "%.15g %.15g moveto %.15g 0.0 rlineto closepath\n",
                        x-sby2, y, size );
                Tcl_AppendResult(interp, buffer, (char *) NULL);
                sprintf(string, "%d setlinewidth", MAX(1,markPtr->width/2));
                Tcl_AppendResult(interp, string,
                                 " 0 setlinejoin 2 setlinecap\n", (char *) NULL);
                if (Tk_CanvasPsColor(interp, canvas, markPtr->outlineColor)
                    != TCL_OK) {
                    return TCL_ERROR;
                }
                Tcl_AppendResult(interp, "stroke\n", (char *) NULL);
            }
        }
        break;
    case CROSS:
        {
            if (markPtr->outlineColor != NULL) {
                sprintf(buffer, "%.15g %.15g moveto %.15g %.15g rlineto closepath\n",
                        x-sby2, y-sby2, size, size );
                Tcl_AppendResult(interp, buffer, (char *) NULL);
                sprintf(buffer, "%.15g %.15g moveto %.15g %.15g rlineto closepath\n",
                        x-sby2, y+sby2, size, -size );
                Tcl_AppendResult(interp, buffer, (char *) NULL);
                sprintf(string, "%d setlinewidth", MAX(1,markPtr->width/2));
                Tcl_AppendResult(interp, string,
                                 " 0 setlinejoin 2 setlinecap\n", (char *) NULL);
                if (Tk_CanvasPsColor(interp, canvas, markPtr->outlineColor)
                    != TCL_OK) {
                    return TCL_ERROR;
                }
                Tcl_AppendResult(interp, "stroke\n", (char *) NULL);
            }
        }
        break;
    case CIRCLE:
        {
            sprintf(buffer, "matrix currentmatrix\n %.15g %.15g translate %.15g %.15g scale 1 0 moveto  0 0 1 0 360 arc\nsetmatrix\n",
                    x, y, sby2, sby2 );

            /*  Now draw the region filled if needed */
            if (markPtr->fillColor != NULL) {
                Tcl_AppendResult(interp, buffer, (char *) NULL);
                if (Tk_CanvasPsColor(interp, canvas, markPtr->fillColor) != TCL_OK) {
                    return TCL_ERROR;
                }
                if (markPtr->fillStipple != None) {
                    Tcl_AppendResult(interp, "clip ", (char *) NULL);
                    if (Tk_CanvasPsStipple(interp, canvas, markPtr->fillStipple)
                        != TCL_OK) {
                        return TCL_ERROR;
                    }
                    if (markPtr->outlineColor != NULL) {
                        Tcl_AppendResult(interp, "grestore gsave\n", (char *) NULL);
                    }
                }
                else {
                    Tcl_AppendResult(interp, "fill\n", (char *) NULL);
                }
            }
            /*  Now the outline (using the same region command). */
            if (markPtr->outlineColor != NULL) {
                Tcl_AppendResult(interp, buffer, (char *) NULL);

                /*  Set the outline width */
                sprintf(string, "%d setlinewidth", MAX(1,markPtr->width/2));
                Tcl_AppendResult(interp, string,
                                 " 0 setlinejoin 2 setlinecap\n", (char *) NULL);
                if (Tk_CanvasPsColor(interp, canvas, markPtr->outlineColor)
                    != TCL_OK) {
                    return TCL_ERROR;
                }
                Tcl_AppendResult(interp, "stroke\n", (char *) NULL);
            }
        }
        break;
    case TRIANGLE:
        {
            /*  Construct a postscript command to draw the region */
            sprintf(buffer, "%.15g %.15g moveto %.15g 0.0 rlineto %.15g %.15g \
rlineto closepath\n",
                    x-sby2, y+sby2, size, -sby2, -size );

            /*  Now draw the region filled if needed */
            if (markPtr->fillColor != NULL) {
                Tcl_AppendResult(interp, buffer, (char *) NULL);
                if (Tk_CanvasPsColor(interp, canvas, markPtr->fillColor) != TCL_OK) {
                    return TCL_ERROR;
                }
                if (markPtr->fillStipple != None) {
                    Tcl_AppendResult(interp, "clip ", (char *) NULL);
                    if (Tk_CanvasPsStipple(interp, canvas, markPtr->fillStipple)
                        != TCL_OK) {
                        return TCL_ERROR;
                    }
                    if (markPtr->outlineColor != NULL) {
                        Tcl_AppendResult(interp, "grestore gsave\n", (char *) NULL);
                    }
                }
                else {
                    Tcl_AppendResult(interp, "fill\n", (char *) NULL);
                }
            }
            /*  Now the outline (using the same region command). */
            if (markPtr->outlineColor != NULL) {
                Tcl_AppendResult(interp, buffer, (char *) NULL);

                /*  Set the outline width */
                sprintf(string, "%d setlinewidth", MAX(1,markPtr->width/2));
                Tcl_AppendResult(interp, string,
                                 " 0 setlinejoin 2 setlinecap\n", (char *) NULL);
                if (Tk_CanvasPsColor(interp, canvas, markPtr->outlineColor)
                    != TCL_OK) {
                    return TCL_ERROR;
                }
                Tcl_AppendResult(interp, "stroke\n", (char *) NULL);
            }
        }
        break;
    case DIAMOND:
        {
            /*  Construct a postscript command to draw the region */
            sprintf(buffer, "%.15g %.15g moveto %.15g %.15g rlineto %.15g %.15g \
rlineto %.15g %.15g rlineto closepath\n",
                    x-sby2, y, sby2, -sby2, sby2, sby2, -sby2, sby2);

            /*  Now draw the region filled if needed */
            if (markPtr->fillColor != NULL) {
                Tcl_AppendResult(interp, buffer, (char *) NULL);
                if (Tk_CanvasPsColor(interp, canvas, markPtr->fillColor) != TCL_OK) {
                    return TCL_ERROR;
                }
                if (markPtr->fillStipple != None) {
                    Tcl_AppendResult(interp, "clip ", (char *) NULL);
                    if (Tk_CanvasPsStipple(interp, canvas, markPtr->fillStipple)
                        != TCL_OK) {
                        return TCL_ERROR;
                    }
                    if (markPtr->outlineColor != NULL) {
                        Tcl_AppendResult(interp, "grestore gsave\n", (char *) NULL);
                    }
                }
                else {
                    Tcl_AppendResult(interp, "fill\n", (char *) NULL);
                }
            }
            /*  Now the outline (using the same region command). */
            if (markPtr->outlineColor != NULL) {
                Tcl_AppendResult(interp, buffer, (char *) NULL);
                
                /*  Set the outline width */
                sprintf(string, "%d setlinewidth", MAX(1,markPtr->width/2));
                Tcl_AppendResult(interp, string,
                                 " 0 setlinejoin 2 setlinecap\n", (char *) NULL);
                if (Tk_CanvasPsColor(interp, canvas, markPtr->outlineColor)
                    != TCL_OK) {
                    return TCL_ERROR;
                }
                Tcl_AppendResult(interp, "stroke\n", (char *) NULL);
            }
        }
        break;
    }
    
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * MarkCorrectAnchor--
 *
 *      This procedure is called to shift the "center" position
 *      of a marker to correct for any requested anchors. Note
 *      this doesn't correct for any scale factors.
 *
 *--------------------------------------------------------------
 */
static void MarkCorrectAnchor( MarkItem *markPtr, double *x, double *y )
{
    double sby2;
    sby2 = (double)markPtr->size/2.0;
    
    switch (markPtr->tkanchor) {
        case TK_ANCHOR_NW: 
            *x -= sby2;
            *y -= sby2;
        break;
        
        case TK_ANCHOR_N:
            *y -= sby2;
        break;
        
        case TK_ANCHOR_NE:
            *x += sby2;
            *y -= sby2;
        break;
        
        case TK_ANCHOR_W:
            *x -= sby2;
        break;
        
        case TK_ANCHOR_E:
            *x += sby2;
        break;
        
        case TK_ANCHOR_SW:
            *x -= sby2;
            *y += sby2;
        break;
        
        case TK_ANCHOR_S:
            *y += sby2;
        break;
        
        case TK_ANCHOR_SE:
            *x += sby2;
            *y += sby2;
        break;
        
        case TK_ANCHOR_CENTER:
        break;
    }
}
