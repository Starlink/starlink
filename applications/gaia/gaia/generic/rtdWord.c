/*
 * Copyright (c) 1991-1994 The Regents of the University of California.
 * Copyright (C) 1994-1995 Sun Microsystems, Inc.
 * Copyright (C) 1997-2005 Central Laboratory of the Research Councils
 * Copyright (C) 2006      Particle Physics and Astronomy Research Council
 *
 * See the Tcl distribution file "license.terms" for information on usage and
 * redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

/*+
 *  Name:
 *     rtdWord.c
 *
 *  Purpose:
 *     This file implements a very simple canvas text-like item that can
 *     be rotated and scaled.
 *
 *  Notes:
 *     This isn't a usual Tk text object as it doesn't use Tk's font
 *     caching, support selection or editing. It uses the xvertext
 *     extensions by Alan Richardson (mppa3@uk.ac.sussex.syma) to
 *     scale and rotate the text. This should be replaced by the
 *     X11R6 scheme at some time.

 *
 *  Authors:
 *     PWD: P.W. Draper (Durham University, U.K).
 *
 *  Changes:
 *     1-JAN-1997: (PWD)
 *        Original version, based on rtdBoxBoxEllipse and tkCanvText.
 *-
 *.
 */

#include <stdio.h>
#include "tk.h"
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "rotated.h"

#define MAX(a,b) ( (a) > (b) ? (a) : (b) )
#define MIN(a,b) ( (a) < (b) ? (a) : (b) )

/*
 * Define a structure for containing all the necessary information
 * to describe the "word".
 */

typedef struct WordItem  {
    Tk_Item header;             /* Mandatory Tk header information */
    char *word;                 /* Pointer to the word */
    int numChar;                /* Number of characters in word */
    double x, y;                /* Coordinates of word reference point */
    double angle;               /* Position angle of word */
    double scale;               /* Scale factor of word */
    int resize;                 /* Scale font on scale */
    Tk_Anchor tkanchor;         /* Where to anchor word relative to (x,y). */
    int anchor;                 /* Native anchor format */
    Tk_Font fontPtr;            /* Font used for drawing word */
    XColor *color;              /* Word colour */
    Pixmap stipple;             /* Stipple bitmap */
    GC GC;                      /* Graphics context for filling item. */
} WordItem;

/*
 * Set the configuration options.
 */

static Tk_CustomOption tagsOption = {Tk_CanvasTagsParseProc,
    Tk_CanvasTagsPrintProc, (ClientData) NULL
};

static Tk_ConfigSpec configSpecs[] = {

  {TK_CONFIG_STRING, "-word", (char *) NULL, (char *) NULL,
   "", Tk_Offset(WordItem, word), TK_CONFIG_DONT_SET_DEFAULT},

  {TK_CONFIG_FONT, "-font", (char *) NULL, (char *) NULL,
   "-Adobe-Helvetica-Bold-R-Normal--*-120-*-*-*-*-*-*",
   Tk_Offset(WordItem, fontPtr), 0},

  {TK_CONFIG_ANCHOR, "-anchor", (char *) NULL, (char *) NULL,
   "east", Tk_Offset(WordItem, tkanchor), TK_CONFIG_DONT_SET_DEFAULT},

  {TK_CONFIG_COLOR, "-fill", (char *) NULL, (char *) NULL,
   "black", Tk_Offset(WordItem, color), 0},

  {TK_CONFIG_BITMAP, "-stipple", (char *) NULL, (char *) NULL,
   (char *) NULL, Tk_Offset(WordItem, stipple), TK_CONFIG_NULL_OK},

  {TK_CONFIG_CUSTOM, "-tags", (char *) NULL, (char *) NULL,
   (char *) NULL, 0, TK_CONFIG_NULL_OK, &tagsOption},

  {TK_CONFIG_DOUBLE, "-angle", (char *) NULL, (char *) NULL,
   "0.0", Tk_Offset(WordItem, angle), TK_CONFIG_DONT_SET_DEFAULT},

  {TK_CONFIG_BOOLEAN, "-resize", (char *) NULL, (char *) NULL,
   "0", Tk_Offset(WordItem, resize), TK_CONFIG_DONT_SET_DEFAULT},

  {TK_CONFIG_DOUBLE, "-scale", (char *) NULL, (char *) NULL,
   "1.0", Tk_Offset(WordItem, scale), TK_CONFIG_DONT_SET_DEFAULT},

  {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
   (char *) NULL, 0, 0}
};

/*
 * Prototypes for procedures defined in this file:
 */

static void ComputeWordBbox( Tk_Canvas canvas, WordItem *wordPtr );

static int ConfigureWord( Tcl_Interp *interp, Tk_Canvas canvas,
                          Tk_Item *itemPtr, int objc, Tcl_Obj *CONST objv[],
                          int flags );

static int CreateWord( Tcl_Interp *interp, Tk_Canvas canvas,
                       struct Tk_Item *itemPtr, int objc,
                       Tcl_Obj *CONST objv[] );

static void DeleteWord( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display );

static void ScaleWord( Tk_Canvas canvas, Tk_Item *itemPtr, double originX,
                       double originY, double scaleX, double scaleY );

static int WordCoords( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
                       int objc, Tcl_Obj *CONST objv[] );

static void DisplayWord( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display,
                         Drawable dst, int x, int y, int width, int height );

static int WordToArea( Tk_Canvas canvas, Tk_Item *itemPtr, double *areaPtr );

static double WordToPoint( Tk_Canvas canvas, Tk_Item *itemPtr,
                           double *pointPtr );

static int WordToPostscript( Tcl_Interp *interp, Tk_Canvas canvas,
                             Tk_Item *itemPtr, int prepass );

static void TranslateWord( Tk_Canvas canvas, Tk_Item *itemPtr, double deltaX,
                           double deltaY );

static void LineToPostscript( Tcl_Interp *interp, char *string, int numChars );

/*
 * The structure below defines the word item type, by means of
 * procedures that can be invoked by generic item code.
 */

static Tk_ItemType rtdWordType = {
    "rtd_word",                    /* name           */
    sizeof(WordItem),              /* itemSize       */
    CreateWord,                    /* createProc     */
    configSpecs,                   /* configSpecs    */
    ConfigureWord,                 /* configureProc  */
    WordCoords,                    /* coordProc      */
    DeleteWord,                    /* deleteProc     */
    DisplayWord,                   /* displayProc    */
    TK_CONFIG_OBJS,                /* alwaysRedraw & flags  */
    WordToPoint,                   /* pointProc      */
    WordToArea,                    /* areaProc       */
    WordToPostscript,              /* postscriptProc */
    ScaleWord,                     /* scaleProc      */
    TranslateWord,                 /* translateProc  */
    (Tk_ItemIndexProc *) NULL,     /* indexProc      */
    (Tk_ItemCursorProc *) NULL,    /* cursorProc     */
    (Tk_ItemSelectionProc *) NULL, /* selectionProc  */
    (Tk_ItemInsertProc *) NULL,    /* insertProc     */
    (Tk_ItemDCharsProc *) NULL,    /* dTextProc      */
    (Tk_ItemType *) NULL           /* nextPtr        */
};


/*  Definitions of escape hatch function for real bounding box of the
    last string drawn. */
void RtdWordLastBBox( double *xb, double *yp );
static XPoint LastBBox[4];

/*
 *--------------------------------------------------------------
 * Word_Init --
 *
 *   This procedure initialises the rtd_word canvas item.
 *
  *--------------------------------------------------------------
 *
 */
int Word_Init()
{
    Tk_CreateItemType( &rtdWordType );
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * CreateWord --
 *
 *      This procedure is invoked to create a new word.
 *
 * Results:
 *      A standard Tcl return value.  If an error occurred in
 *      creating the item, then an error message is left in
 *      interp->result;  in this case itemPtr is left uninitialized,
 *      so it can be safely freed by the caller.
 *
 * Side effects:
 *      A new word item is created.
 *
 *--------------------------------------------------------------
 */
static int CreateWord( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
                       int objc, Tcl_Obj *CONST objv[] )
{
    WordItem *wordPtr = (WordItem *) itemPtr;
    if ( objc < 2 ) {
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

    wordPtr->word = NULL;
    wordPtr->numChar = 0;
    wordPtr->angle = 0.0;
    wordPtr->scale = 1.0;
    wordPtr->resize = 0;
    wordPtr->tkanchor = TK_ANCHOR_CENTER;
    wordPtr->anchor = MCENTRE;
    wordPtr->fontPtr = NULL;
    wordPtr->color = NULL;
    wordPtr->stipple = None;
    wordPtr->GC = None;

    /*
     * Process the arguments to fill in the item record.
     */
    if ( ( Tk_CanvasGetCoordFromObj( interp, canvas, objv[0], &wordPtr->x )
           != TCL_OK ) ||
         ( Tk_CanvasGetCoordFromObj( interp, canvas, objv[1], &wordPtr->y )
           != TCL_OK) )
    {
        return TCL_ERROR;
    }

    /*  And configure using any options */
    if ( ConfigureWord( interp, canvas, itemPtr, objc-2, objv+2, 0 )
         != TCL_OK )
    {
        DeleteWord( canvas, itemPtr, Tk_Display( Tk_CanvasTkwin( canvas ) ) );
        return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * WordCoords --
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

static int WordCoords( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
                       int objc, Tcl_Obj *CONST objv[] )
{
    WordItem *wordPtr = (WordItem *) itemPtr;
    char x[TCL_DOUBLE_SPACE], y[TCL_DOUBLE_SPACE];

    if ( objc == 0 ) {
        Tcl_PrintDouble( interp, wordPtr->x, x );

        Tcl_PrintDouble( interp, wordPtr->y, y );
        Tcl_AppendResult( interp, x, " ", y, " ", (char *) NULL );
    }
    else if ( objc == 2 ) {
        if ( ( Tk_CanvasGetCoordFromObj( interp, canvas, objv[0], &wordPtr->x )
               != TCL_OK ) ||
             ( Tk_CanvasGetCoordFromObj( interp, canvas, objv[1], &wordPtr->y )
               != TCL_OK ) )
        {
            return TCL_ERROR;
        }
        ComputeWordBbox( canvas, wordPtr );
    }
    else {
        char buffer[80];
        sprintf( buffer,
                 "wrong # coordinates:  expected 0 or 2, got %d", objc );
        Tcl_SetResult( interp, buffer, TCL_VOLATILE );
        return TCL_ERROR;
    }
    return TCL_OK;
}


/*
 *--------------------------------------------------------------
 *
 * ConfigureWord --
 *
 *      This procedure is invoked to configure various aspects of a
 *      word, such as its border and background colors.
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

static int ConfigureWord( Tcl_Interp *interp, Tk_Canvas canvas,
                          Tk_Item *itemPtr, int objc, Tcl_Obj *CONST objv[],
                          int flags )
{
    WordItem *wordPtr = (WordItem *) itemPtr;
    XGCValues gcValues;
    GC newGC;
    unsigned long mask;
    Tk_Window tkwin;

    tkwin = Tk_CanvasTkwin( canvas );
    if ( Tk_ConfigureWidget( interp, tkwin, configSpecs, objc,
                             (CONST char **) objv, (char *) wordPtr,
                             flags|TK_CONFIG_OBJS )
         != TCL_OK )
    {
        return TCL_ERROR;
    }

    /*
     * A few of the options require additional processing, such as
     * graphics contexts.
     */
    if ( wordPtr->word != NULL ) {
        wordPtr->numChar = strlen( wordPtr->word );
    }
    else {
        wordPtr->numChar = 0;
    }
    newGC = None;
    if ( ( wordPtr->color != NULL ) && ( wordPtr->fontPtr != NULL ) ) {
        gcValues.foreground = wordPtr->color->pixel;
        gcValues.font = Tk_FontId(wordPtr->fontPtr);

        mask = GCForeground|GCFont;
        if ( wordPtr->stipple != None ) {
            gcValues.stipple = wordPtr->stipple;
            gcValues.fill_style = FillStippled;
            mask |= GCForeground|GCStipple|GCFillStyle;
        }
        newGC = Tk_GetGC( tkwin, mask, &gcValues );
    }
    if ( wordPtr->GC != None ) {
        Tk_FreeGC( Tk_Display( tkwin ), wordPtr->GC );
    }
    wordPtr->GC = newGC;

    /* Convert Tk anchor into local version. */
    switch ( wordPtr->tkanchor ) {
        case TK_ANCHOR_NW:     wordPtr->anchor = BRIGHT;   break;
        case TK_ANCHOR_N:      wordPtr->anchor = BCENTRE;  break;
        case TK_ANCHOR_NE:     wordPtr->anchor = BLEFT;    break;
        case TK_ANCHOR_E:      wordPtr->anchor = MLEFT;    break;
        case TK_ANCHOR_SE:     wordPtr->anchor = TLEFT;    break;
        case TK_ANCHOR_S:      wordPtr->anchor = TCENTRE;  break;
        case TK_ANCHOR_SW:     wordPtr->anchor = TRIGHT;   break;
        case TK_ANCHOR_W:      wordPtr->anchor = MRIGHT;   break;
        case TK_ANCHOR_CENTER: wordPtr->anchor = MCENTRE;  break;
    }

    ComputeWordBbox( canvas, wordPtr );
    return TCL_OK;
}


/*
 *--------------------------------------------------------------
 *
 * DeleteWord --
 *
 *      This procedure is called to clean up the data structure
 *      associated with a word.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      Resources associated with itemPtr are released.
 *
 *--------------------------------------------------------------
 */

static void
DeleteWord( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display )
{
    WordItem *wordPtr = (WordItem *) itemPtr;

    if ( wordPtr->word != NULL ) {
        ckfree( wordPtr->word );
    }
    if ( wordPtr->fontPtr != NULL ) {
        /* Release font structure? Not sure that this a good idea when
         * dealing with the xvertext cache -- so don't, but do for tcl8,
         * as causes abort() if not done!*/
        Tk_FreeFont( wordPtr->fontPtr );
    }
    if ( wordPtr->color != NULL ) {
        Tk_FreeColor( wordPtr->color );
    }
    if ( wordPtr->stipple != None ) {
        Tk_FreeBitmap( display, wordPtr->stipple );
    }
    if ( wordPtr->GC != None ) {
        Tk_FreeGC( display, wordPtr->GC );
    }
}

/*
 *--------------------------------------------------------------
 *
 * ComputeWordBbox --
 *
 *      This procedure is invoked to compute the bounding box of
 *      all the pixels that may be drawn as part of a word.
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
static void ComputeWordBbox( Tk_Canvas canvas, WordItem *wordPtr )
{
    XPoint *bbox = NULL;
    XFontStruct *fontStruct = NULL;

    int x1, y1, x2, y2;
    if ( wordPtr->word != NULL ) {
        XRotSetMagnification( wordPtr->scale );

        fontStruct = XQueryFont( Tk_Display( Tk_CanvasTkwin( canvas ) ),
                                 Tk_FontId( wordPtr->fontPtr ) );

        if ( fontStruct != NULL ) {
            bbox = XRotTextExtents( Tk_Display( Tk_CanvasTkwin( canvas ) ),
                                    fontStruct,
                                    wordPtr->angle,
                                    (int) wordPtr->x,
                                    (int) wordPtr->y,
                                    wordPtr->word,
                                    wordPtr->anchor);
            XFreeFontInfo( NULL, fontStruct, 1 );

            if ( bbox != NULL ) {
                x1 = MIN( bbox[0].x, bbox[1].x );
                x1 = MIN( x1, bbox[2].x );
                x1 = MIN( x1, bbox[3].x );
                x2 = MAX( bbox[0].x, bbox[1].x );
                x2 = MAX( x2, bbox[2].x );
                x2 = MAX( x2, bbox[3].x );

                y1 = MIN( bbox[0].y, bbox[1].y );
                y1 = MIN( y1, bbox[2].y );
                y1 = MIN( y1, bbox[3].y );
                y2 = MAX( bbox[0].y, bbox[1].y );
                y2 = MAX( y2, bbox[2].y );
                y2 = MAX( y2, bbox[3].y );

                wordPtr->header.x1 = x1;
                wordPtr->header.y1 = y1;
                wordPtr->header.x2 = x2;
                wordPtr->header.y2 = y2;

                /*  Transfer this bounding box into local static space for
                    recovery as a true bounding box, not just an axis aligned
                    region. */
                LastBBox[0].x = bbox[0].x;
                LastBBox[0].y = bbox[0].y;
                LastBBox[1].x = bbox[1].x;
                LastBBox[1].y = bbox[1].y;
                LastBBox[2].x = bbox[2].x;
                LastBBox[2].y = bbox[2].y;
                LastBBox[3].x = bbox[3].x;
                LastBBox[3].y = bbox[3].y;

                free( (void *)bbox );
            }
        }
    }
}


/*
 *--------------------------------------------------------------
 *
 * DisplayWord --
 *
 *      This procedure is invoked to draw a word item in a given drawable.
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
static void DisplayWord( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display,
                         Drawable drawable, int x, int y, int width,
                         int height )
{
    WordItem *wordPtr = (WordItem *) itemPtr;
    short drawableX, drawableY;
    XFontStruct *fontStruct = NULL;

    if ( wordPtr->GC == None || wordPtr->word == NULL ) {
        return;
    }

    /*
     * If we're stippling, then modify the stipple offset in the GC.  Be
     * sure to reset the offset when done, since the GC is supposed to be
     * read-only.
     */

    if ( wordPtr->stipple != None ) {
        Tk_CanvasSetStippleOrigin( canvas, wordPtr->GC );
    }

    /* Transform coordinates into drawable from canvas */
    Tk_CanvasDrawableCoords( canvas, (double) wordPtr->x, (double) wordPtr->y,
                             &drawableX, &drawableY );

    /* Set the magnification factor */
    XRotSetMagnification( wordPtr->scale );

    /* And draw the string */
    fontStruct = XQueryFont( display, Tk_FontId( wordPtr->fontPtr ) );

    if ( fontStruct != NULL ) {
        XRotDrawAlignedString( display,
                               fontStruct,
                               wordPtr->angle,
                               drawable,
                               wordPtr->GC,
                               drawableX,
                               drawableY,
                               wordPtr->word,
                               wordPtr->anchor );
        XFreeFontInfo( NULL, fontStruct, 1 );
    }

    if ( wordPtr->stipple != None ) {
        XSetTSOrigin( display, wordPtr->GC, 0, 0 );
    }
}

/*
 *--------------------------------------------------------------
 *
 * WordToPoint --
 *
 *      Computes the distance from a given point to the word.
 *
 * Results:
 *      The return value is 0 if the point whose x and y coordinates
 *      are coordPtr[0] and coordPtr[1] is inside the word.  If the
 *      point isn't inside the word then the return value is the
 *      distance from the point to the item.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */
static double WordToPoint( Tk_Canvas canvas, Tk_Item *itemPtr,
                           double *pointPtr )
{
    WordItem *wordPtr = (WordItem *) itemPtr;
    double xDiff, yDiff;

    /*
     * If the point is inside the word's bounding box, then can
     * return immediately.
     */
    if ( ( pointPtr[0] >= wordPtr->header.x1 ) &&
         ( pointPtr[0] <= wordPtr->header.x2 ) &&
         ( pointPtr[1] >= wordPtr->header.y1 ) &&
         ( pointPtr[1] <= wordPtr->header.y2 ) )
    {
        return 0.0;
    }

    /*
     * Point is outside word's bounding box; compute distance to nearest
     * side.
     */
    if ( pointPtr[0] < wordPtr->header.x1 ) {
        xDiff = wordPtr->header.x1 - pointPtr[0];
    }
    else if ( pointPtr[0] > wordPtr->header.x2 )  {
        xDiff = pointPtr[0] - wordPtr->header.x2;
    }
    else {
        xDiff = 0.0;
    }

    if ( pointPtr[1] < wordPtr->header.y1 ) {
        yDiff = wordPtr->header.y1 - pointPtr[1];
    }
    else if ( pointPtr[1] > wordPtr->header.y2 )  {
        yDiff = pointPtr[1] - wordPtr->header.y2;
    }
    else {
        yDiff = 0.0;
    }
    return sqrt( ( xDiff * xDiff ) + ( yDiff * yDiff ) );
}

/*
 *--------------------------------------------------------------
 *
 * WordToArea --
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
static int WordToArea( Tk_Canvas canvas, Tk_Item *itemPtr, double *areaPtr )
{
    WordItem *wordPtr = (WordItem *) itemPtr;

    if ( ( areaPtr[2] > wordPtr->header.x1 ) &&
         ( areaPtr[0] < wordPtr->header.x2 ) &&
         ( areaPtr[3] > wordPtr->header.y1 ) &&
         ( areaPtr[1] < wordPtr->header.y2 ) )
    {
        return 1;
    }
    if ( ( areaPtr[0] > wordPtr->header.x1 ) &&
         ( areaPtr[1] > wordPtr->header.y1 ) &&
         ( areaPtr[0] < wordPtr->header.x2 ) &&
         ( areaPtr[1] < wordPtr->header.y2 ) )
    {
        return 0;
    }
    if ( ( areaPtr[2] > wordPtr->header.x1 ) &&
         ( areaPtr[3] > wordPtr->header.y1 ) &&
         ( areaPtr[2] < wordPtr->header.x2 ) &&
         ( areaPtr[3] < wordPtr->header.y2 ) )
    {
        return 0;

    }
    return -1;
}

/*
 *--------------------------------------------------------------
 *
 * ScaleWord --
 *
 *      This procedure is invoked to rescale a word.
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
 *      The word is also scaled to scaleX if resize is true.
 *
 *--------------------------------------------------------------
 */
static void ScaleWord( Tk_Canvas canvas, Tk_Item *itemPtr, double originX,
                       double originY, double scaleX, double scaleY )
{
    WordItem *wordPtr = (WordItem *) itemPtr;

    /* Scale all coordinates and set their related values */
    wordPtr->x = originX + scaleX * ( wordPtr->x - originX );
    wordPtr->y = originY + scaleY * ( wordPtr->y - originY );

    if ( wordPtr->resize ) {
        /*  If required also rescale the actual font. Otherwise just the
            position is rescaled. */
        wordPtr->scale *= scaleX;
    }
    ComputeWordBbox( canvas, wordPtr );
    return;
}

/*
 *--------------------------------------------------------------
 *
 * TranslateWord --
 *
 *      This procedure is called to move a word by a given amount.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The position of the word is offset by (xDelta, yDelta), and
 *      the bounding box is updated in the generic part of the item structure.
 *
 *--------------------------------------------------------------
 */
static void TranslateWord( Tk_Canvas canvas, Tk_Item *itemPtr, double deltaX,
                           double deltaY )
{
    WordItem *wordPtr = (WordItem *) itemPtr;

    wordPtr->x += deltaX;
    wordPtr->y += deltaY;
    ComputeWordBbox( canvas, wordPtr );
}


/*
 *--------------------------------------------------------------
 *
 * WordToPostscript --
 *
 *      This procedure is called to generate Postscript for
 *      the word item.
 *
 * Results:
 *      The return value is a standard Tcl result.  If an error
 *      occurs in generating Postscript then an error message is
 *      left in interp->result, replacing whatever used to be there.
 *      If no error occurs, then Postscript for the word is
 *      appended to the result.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */
static int WordToPostscript( Tcl_Interp *interp, Tk_Canvas canvas,
                             Tk_Item *itemPtr, int prepass )
{
    WordItem *wordPtr = (WordItem *) itemPtr;
    char buffer[500];
    double mag = 1.0;
    double xoffset = 0.0, yoffset = 0.0;

    if ( wordPtr->color == NULL ) {
        return TCL_OK;
    }

    if ( Tk_CanvasPsFont( interp, canvas, wordPtr->fontPtr ) != TCL_OK ) {
        return TCL_ERROR;
    }

    if ( Tk_CanvasPsColor( interp, canvas, wordPtr->color ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /*  Add the stipple text command if needed. */
    if ( wordPtr->stipple != None ) {
        Tcl_AppendResult( interp, "/StippleText {\n    ",
                          (char *) NULL );
        Tk_CanvasPsStipple( interp, canvas, wordPtr->stipple );
        Tcl_AppendResult( interp, "} bind def\n", (char *) NULL );
    }

    /*  Get the width and height of the word on paper. */
    sprintf( buffer, "0 0 moveto " );
    Tcl_AppendResult( interp, buffer, (char *) NULL );
    LineToPostscript( interp, wordPtr->word, wordPtr->numChar );
    sprintf( buffer, " false charpath\npathbbox /height exch def /width"
             " exch def pop pop newpath\n ");
    Tcl_AppendResult( interp, buffer, (char *) NULL );

    /*  Now change to the new coordinate system about which we scale
     *  and rotate. */
    sprintf( buffer, "%.15g %.15g translate \n", wordPtr->x,
             Tk_CanvasPsY( canvas, wordPtr->y ) );
    Tcl_AppendResult( interp, buffer, (char *) NULL );

    /*
     * Add the rotation and scale, then adjust the position to the correct
     * anchor point. We need to unscale the font size by the postscript to X
     * points conversion factor (the whole canvas has this scale applied at
     * the start, note mag is X points/PS point).
     */
    mag = WidthOfScreen( Tk_Screen( Tk_CanvasTkwin( canvas ) ) );
    mag /= (72.0/25.4)*WidthMMOfScreen(Tk_Screen(Tk_CanvasTkwin(canvas)));

    sprintf( buffer, "%.15g %.15g scale %.15g rotate \n",
             wordPtr->scale*mag, wordPtr->scale*mag, wordPtr->angle );
    Tcl_AppendResult( interp, buffer, (char *) NULL );
    switch ( wordPtr->tkanchor ) {
        case TK_ANCHOR_NW:     xoffset = -1.0; yoffset = 0.5;   break;
        case TK_ANCHOR_N:      xoffset = -0.5; yoffset = 0.5;   break;
        case TK_ANCHOR_NE:     xoffset = 0.0;  yoffset = 0.5;   break;
        case TK_ANCHOR_E:      xoffset = 0.0;  yoffset = -0.25; break;
        case TK_ANCHOR_SE:     xoffset = 0.0;  yoffset = -1.0;  break;
        case TK_ANCHOR_S:      xoffset = -0.5; yoffset = -1.0;  break;
        case TK_ANCHOR_SW:     xoffset = -1.0; yoffset = -1.0;  break;
        case TK_ANCHOR_W:      xoffset = -1.0; yoffset = -0.25; break;
        case TK_ANCHOR_CENTER: xoffset = -0.5; yoffset = -0.25; break;
    }
    sprintf( buffer, "width %f mul height %f mul moveto \n",
             xoffset, yoffset );
    Tcl_AppendResult( interp, buffer, (char *) NULL );

    /*  Finally add the text to draw. */
    LineToPostscript( interp, wordPtr->word, wordPtr->numChar );
    if ( wordPtr->stipple != None ) {
        Tcl_AppendResult( interp, " true charpath clip StippleText\n",
                          (char *) NULL );
    }
    else {
        Tcl_AppendResult( interp, " show\n", (char *) NULL );
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * LineToPostscript --
 *
 *      This procedure generates a parenthesized Postscript string
 *      describing one line of text from a text item.
 *
 * Results:
 *      None. The parenthesized string is appended to
 *      interp->result.  It generates proper backslash notation so
 *      that Postscript can interpret the string correctly.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */
static void LineToPostscript( Tcl_Interp *interp, char *string, int numChars )
{
#define BUFFER_SIZE 100
    char buffer[BUFFER_SIZE+5];
    int used, c;

    buffer[0] = '(';
    used = 1;
    for ( ; numChars > 0; string++, numChars--) {
        c = (*string) & 0xff;
        if ( ( c == '(') || ( c == ')' ) || ( c == '\\' ) || ( c < 0x20 )
             || ( c >= 0x7f ) ) {
            /*
             * Tricky point:  the "03" is necessary in the sprintf below,
             * so that a full three digits of octal are always generated.
             * Without the "03", a number following this sequence could
             * be interpreted by Postscript as part of this sequence.
             */
            sprintf( buffer+used, "\\%03o", c );
            used += strlen( buffer + used );
        }
        else {
            buffer[used] = c;
            used++;
        }
        if ( used >= BUFFER_SIZE ) {
            buffer[used] = 0;
            Tcl_AppendResult( interp, buffer, (char *) NULL );
            used = 0;
        }
    }
    buffer[used] = ')';
    buffer[used+1] = 0;
    Tcl_AppendResult( interp, buffer, (char *) NULL );
}

/*
 *--------------------------------------------------------------
 *
 * RtdWordLastBBox --
 *
 *    Returns the complete bounding box of the last text item whose
 *    canvas bounding box was requested.
 *
 *      - xb, X corners of the string bounding box
 *      - yb, Y corners of the string bounding box
 *
 *--------------------------------------------------------------
 */
void RtdWordLastBBox( double *xb, double *yb )
{
    /* The last call to WordBBox should have updated the contents of
     * the static space LastBBox, we just need to return it. */
    xb[0] = (double) LastBBox[0].x;
    xb[1] = (double) LastBBox[1].x;
    xb[2] = (double) LastBBox[2].x;
    xb[3] = (double) LastBBox[3].x;
    yb[0] = (double) LastBBox[0].y;
    yb[1] = (double) LastBBox[1].y;
    yb[2] = (double) LastBBox[2].y;
    yb[3] = (double) LastBBox[3].y;
}
