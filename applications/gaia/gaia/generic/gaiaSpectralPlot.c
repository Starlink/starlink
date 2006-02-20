/*+
 *  Name:
 *     gaiaSpectralPlot
 *
 *  Purpose:
 *     Defines a Tk canvas item "spectral_plot" that displays a spectrum in a
 *     2D axes plot.
 *
 *  Description:
 *     This item displays an array of data values as a polyline in
 *     a plot. The two axes are determined using an AST spectral coordinate
 *     system (wavelength, frequency, velocity etc.) and an optional set of
 *     data units.
 *
 * Copyright (c) 2006 Particle Physics and Astronomy Research Council
 *
 *  Authors:
 *     PWD: P.W. Draper (JAC, Durham University).
 *
 *  Changes:
 *     14-FEB-2006: (PWD)
 *        Original version.
 *-
 *.
 */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <tk.h>
#include <ast.h>

/*
 * Define a structure for containing all the necessary information
 * to describe an item.
 */
typedef struct SPItem  {
    Tk_Item header;             /* Mandatory Tk header information */
    double *dataPtr;            /* Pointer to the data values */
    int numPoints;              /* Number of data values */
    AstFrameSet *frameset;      /* The FrameSet associated with item */
    AstFrameSet *plotframeset;  /* The generated 2D plot FrameSet */
    char *dataunits;            /* The data units */
    char *datalabel;            /* The data units label */
    char *options;              /* AST options used when drawing item */
    double x, y;                /* Coordinates of item reference point */
    double scale;               /* Scale factor of item */
    Tk_Anchor tkanchor;         /* Where to anchor item relative to (x,y). */
} SPItem;

/* Item canvas tags */
static Tk_CustomOption tagsOption = { 
    Tk_CanvasTagsParseProc, Tk_CanvasTagsPrintProc, (ClientData) NULL
};

/* FrameSet handling */
static int FrameSetParseProc( ClientData clientData, Tcl_Interp *interp,
                              Tk_Window tkwin, CONST char *value,
                              char *widgRec, int offset );
static char *FrameSetPrintProc( ClientData clientData, Tk_Window tkwin,
                                char *widgRec, int offset,
                                Tcl_FreeProc **freeProcPtr );
static Tk_CustomOption framesetOption = { 
    FrameSetParseProc, FrameSetPrintProc, (ClientData) NULL
};

/* Configuration options */
static Tk_ConfigSpec configSpecs[] = {

  {TK_CONFIG_ANCHOR, "-anchor", (char *) NULL, (char *) NULL,
   "east", Tk_Offset(SPItem, tkanchor), TK_CONFIG_DONT_SET_DEFAULT},

  {TK_CONFIG_CUSTOM, "-tags", (char *) NULL, (char *) NULL,
   (char *) NULL, 0, TK_CONFIG_NULL_OK, &tagsOption},

  {TK_CONFIG_DOUBLE, "-scale", (char *) NULL, (char *) NULL,
   "1.0", Tk_Offset(SPItem, scale), TK_CONFIG_DONT_SET_DEFAULT},

  {TK_CONFIG_CUSTOM, "-frameset", (char *) NULL, (char *) NULL,
   (char *) NULL, Tk_Offset(SPItem, frameset), TK_CONFIG_NULL_OK, 
   &framesetOption},

  {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
   (char *) NULL, 0, 0}
};

/*
 * Prototypes for procedures defined in this file:
 */
static int    ReadDataList( Tcl_Interp *interp, SPItem *spPtr,
                            Tcl_Obj *CONST dataValues );

static void   ComputeSPBbox( Tk_Canvas canvas, SPItem *spPtr );

static int    ConfigureSP( Tcl_Interp *interp, Tk_Canvas canvas,
                           Tk_Item *itemPtr, int objc,
                           Tcl_Obj *CONST objv[], int flags );
static int    CreateSP( Tcl_Interp *interp, Tk_Canvas canvas,
                        struct Tk_Item *itemPtr, int objc,
                        Tcl_Obj *CONST objv[] );
static void   DeleteSP( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display );
static void   ScaleSP( Tk_Canvas canvas, Tk_Item *itemPtr, double originX,
                       double originY, double scaleX, double scaleY );
static int    SPCoords( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
                        int objc, Tcl_Obj *CONST objv[] );
static void   DisplaySP( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display,
                         Drawable dst, int x, int y, int width, int height );
static int    SPToArea( Tk_Canvas canvas, Tk_Item *itemPtr, double *areaPtr );
static double SPToPoint( Tk_Canvas canvas, Tk_Item *itemPtr,
                         double *pointPtr );
static int    SPToPostscript( Tcl_Interp *interp, Tk_Canvas canvas,
                              Tk_Item *itemPtr, int prepass );
static void   TranslateSP( Tk_Canvas canvas, Tk_Item *itemPtr, double deltaX,
                         double deltaY );

/*
 * The structure below defines the item type, by means of procedures that can
 * be invoked by generic item code.
 */
static Tk_ItemType spectralPlotType = {
    "spectral_plot",               /* name           */
    sizeof(SPItem),                /* itemSize       */
    CreateSP,                      /* createProc     */
    configSpecs,                   /* configSpecs    */
    ConfigureSP,                   /* configureProc  */
    SPCoords,                      /* coordProc      */
    DeleteSP,                      /* deleteProc     */
    DisplaySP,                     /* displayProc    */
    TK_CONFIG_OBJS,                /* flags          */
    SPToPoint,                     /* pointProc      */
    SPToArea,                      /* areaProc       */
    SPToPostscript,                /* postscriptProc */
    ScaleSP,                       /* scaleProc      */
    TranslateSP,                   /* translateProc  */
    (Tk_ItemIndexProc *) NULL,     /* indexProc      */
    (Tk_ItemCursorProc *) NULL,    /* cursorProc     */
    (Tk_ItemSelectionProc *) NULL, /* selectionProc  */
    (Tk_ItemInsertProc *) NULL,    /* insertProc     */
    (Tk_ItemDCharsProc *) NULL,    /* dTextProc      */
    (Tk_ItemType *) NULL           /* nextPtr        */
};


/*
 *--------------------------------------------------------------
 * SpectralPlot_Init --
 *
 *   This procedure initialises the spectral_plot canvas item.
 *
  *--------------------------------------------------------------
 *
 */
int SpectralPlot_Init() {
    Tk_CreateItemType(&spectralPlotType);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * CreateSP --
 *
 *      This procedure is invoked to create a spectral plot item.
 *
 * Results:
 *      A standard Tcl return value.  If an error occurred in
 *      creating the item, then an error message is left in
 *      interp->result;  in this case itemPtr is left uninitialized,
 *      so it can be safely freed by the caller.
 *
 * Side effects:
 *      A canvas item is created.
 *
 *--------------------------------------------------------------
 */
static int CreateSP( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
                     int objc, Tcl_Obj *CONST objv[] )
{
    long longResult;
    SPItem *spPtr = (SPItem *) itemPtr;
    if ( objc < 4 ) {
        Tcl_AppendResult( interp, "wrong # args:  should be \"",
                          Tk_PathName( Tk_CanvasTkwin( canvas ) ),
                          "\" create ", itemPtr->typePtr->name,
                          " x y {data1 data2 ...} frameset_item "
                          "?options?", (char *) NULL );
        return TCL_ERROR;
    }

    /*
     * Carry out initialization that is needed in order to clean
     * up after errors during the the remainder of this procedure.
     */
    spPtr->dataPtr = NULL;
    spPtr->numPoints = 0;
    spPtr->frameset = NULL;
    spPtr->plotframeset = NULL;
    spPtr->dataunits = NULL;
    spPtr->datalabel = NULL;
    spPtr->x = 0.0;
    spPtr->y = 0.0;
    spPtr->scale = 1.0;
    spPtr->tkanchor = TK_ANCHOR_CENTER;

    /*
     * Process the arguments to fill in the item record.
     */
    if ( (Tk_CanvasGetCoordFromObj( interp, canvas, objv[0], &spPtr->x )
          != TCL_OK) ||
         (Tk_CanvasGetCoordFromObj( interp, canvas, objv[1], &spPtr->y )
          != TCL_OK) ) {
        return TCL_ERROR;
    }

    /* Convert data values into doubles */
    if ( ReadDataList( interp, spPtr, objv[2] ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Access the AstFrameSet, this is a pointer reference */
    if ( Tcl_ExprLongObj( interp, objv[3], &longResult ) != TCL_OK ) {
        return TCL_ERROR;
    }
    spPtr->frameset = (AstFrameSet *) longResult;

    /*  And configure using any options */
    if ( ConfigureSP( interp, canvas, itemPtr, objc-4, objv+4, 0 ) != TCL_OK ){
        DeleteSP( canvas, itemPtr, Tk_Display( Tk_CanvasTkwin( canvas ) ) );
        return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * ReadDataList --
 *    Read a list of spectral data value from a list.
 *
 *--------------------------------------------------------------
 */
static int ReadDataList( Tcl_Interp *interp, SPItem *spPtr,
                         Tcl_Obj *CONST dataValues )
{
    Tcl_Obj *arg = NULL;
    int nargs = 0;
    int i = 0;

    if ( Tcl_ListObjLength( interp, dataValues, &nargs ) != TCL_OK ) {
        return TCL_ERROR;
    }

    spPtr->dataPtr = (double *) ckalloc( sizeof(double) * nargs );
    spPtr->numPoints = nargs;
    for ( i = 0; i < nargs; i++ ) {
        Tcl_ListObjIndex( interp, dataValues, i, &arg );
        if (Tcl_GetDoubleFromObj(interp, arg, &spPtr->dataPtr[i]) != TCL_OK) {
            ckfree( (char *) spPtr->dataPtr );
            spPtr->numPoints = 0;
            sprintf( interp->result, "%s is not a number: "
                     "failed accessing list of data values",
                     Tcl_GetString( arg ) );
            return TCL_ERROR;
        }
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * SPCoords --
 *
 *   This procedure is invoked to process the "coords" widget command.
 *   This just sets the x and y values that determine the item reference
 *   point.
 *
 * Results:
 *      Returns TCL_OK or TCL_ERROR, and sets interp->result.
 *
 * Side effects:
 *      The coordinates for the given item may be changed.
 *
 *--------------------------------------------------------------
 */
static int SPCoords( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
                     int objc, Tcl_Obj *CONST objv[] )
{
    SPItem *spPtr = (SPItem *) itemPtr;
    char x[TCL_DOUBLE_SPACE], y[TCL_DOUBLE_SPACE];

    if ( objc == 0 ) {
        Tcl_PrintDouble( interp, spPtr->x, x );
        Tcl_PrintDouble( interp, spPtr->y, y );
        Tcl_AppendResult( interp, x, " ", y, " ", (char *) NULL );
    }
    else if ( objc == 2 ) {
        if ( ( Tk_CanvasGetCoordFromObj( interp, canvas, objv[0], &spPtr->x )
               != TCL_OK ) ||
             ( Tk_CanvasGetCoordFromObj( interp, canvas, objv[1], &spPtr->y )
               != TCL_OK ) ) {
            return TCL_ERROR;
        }
        ComputeSPBbox( canvas, spPtr );
    }
    else {
        sprintf( interp->result,
                 "wrong # coordinates:  expected 0 or 2, got %d", objc );
        return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * ConfigureSP --
 *
 *      This procedure is invoked to configure the item.
 *
 * Results:
 *      A standard Tcl result code.  If an error occurs, then
 *      an error message is left in interp->result.
 *
 * Side effects:
 *      Configuration information may be set for itemPtr.
 *
 *--------------------------------------------------------------
 */
static int ConfigureSP( Tcl_Interp *interp, Tk_Canvas canvas,
                        Tk_Item *itemPtr, int objc,
                        Tcl_Obj *CONST objv[], int flags )
{
    SPItem *spPtr = (SPItem *) itemPtr;
    Tk_Window tkwin;

    tkwin = Tk_CanvasTkwin( canvas );
    /* Note using Tcl_Obj calls so need TK_CONFIG_OBJS flags. */
    if ( Tk_ConfigureWidget( interp, tkwin, configSpecs, objc,
                             (CONST char **) objv, (char *) spPtr, 
                             flags|TK_CONFIG_OBJS )
         != TCL_OK ) {
        return TCL_ERROR;
    }
    ComputeSPBbox( canvas, spPtr );
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * DeleteSP --
 *
 *      This procedure is called to clean up the data structure
 *      associated with an item.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      Resources associated with itemPtr are released.
 *
 *--------------------------------------------------------------
 */
static void DeleteSP( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display )
{
    SPItem *spPtr = (SPItem *) itemPtr;

    if ( spPtr->dataPtr != NULL) {
        ckfree( (char *) spPtr->dataPtr );
    }
    spPtr->numPoints = 0;
}

/*
 *--------------------------------------------------------------
 *
 * DisplaySP --
 *
 *      This procedure is invoked to draw an item in a given drawable.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      ItemPtr is drawn in drawable...
 *
 *--------------------------------------------------------------
 */
static void DisplaySP( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display,
                       Drawable drawable, int x, int y, int width,
                       int height )
{
    SPItem *spPtr = (SPItem *) itemPtr;
    if ( spPtr->dataPtr == NULL ) {
        return;
    }


    /** Need to do stuff here ... */

}

/*
 *--------------------------------------------------------------
 *
 * SPToPoint --
 *
 *      Computes the distance from a given point to the plot.
 *
 * Results:
 *      The return value is 0 if the point whose x and y coordinates
 *      are coordPtr[0] and coordPtr[1] is inside the plot.  If the
 *      point isn't inside the plot then the return value is the
 *      distance from the point to the item.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */
static double SPToPoint( Tk_Canvas canvas, Tk_Item *itemPtr,
                         double *pointPtr )
{
    SPItem *spPtr = (SPItem *) itemPtr;
    double xDiff, yDiff;

    /*
     * If the point is inside the bounding box of the plot, then we can
     * return immediately.
     */
    if ( ( pointPtr[0] >= spPtr->header.x1 ) &&
         ( pointPtr[0] <= spPtr->header.x2 ) &&
         ( pointPtr[1] >= spPtr->header.y1 ) &&
         ( pointPtr[1] <= spPtr->header.y2 ) ) {
        return 0.0;
    }

    /*
     * Point is outside plot's bounding box; compute distance to nearest
     * side.
     */
    if ( pointPtr[0] < spPtr->header.x1 ) {
        xDiff = spPtr->header.x1 - pointPtr[0];
    }
    else if ( pointPtr[0] > spPtr->header.x2 )  {
        xDiff = pointPtr[0] - spPtr->header.x2;
    }
    else {
        xDiff = 0.0;
    }

    if ( pointPtr[1] < spPtr->header.y1 ) {
        yDiff = spPtr->header.y1 - pointPtr[1];
    }
    else if ( pointPtr[1] > spPtr->header.y2 )  {
        yDiff = pointPtr[1] - spPtr->header.y2;
    }
    else {
        yDiff = 0.0;
    }
    return sqrt( ( xDiff * xDiff ) + ( yDiff * yDiff ) );
}

/*
 *--------------------------------------------------------------
 *
 * SPToArea --
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
static int SPToArea( Tk_Canvas canvas, Tk_Item *itemPtr, double *areaPtr )
{
    SPItem *spPtr = (SPItem *) itemPtr;

    if ( ( areaPtr[2] > spPtr->header.x1 ) &&
         ( areaPtr[0] < spPtr->header.x2 ) &&
         ( areaPtr[3] > spPtr->header.y1 ) &&
         ( areaPtr[1] < spPtr->header.y2 ) ) {
        return 1;
    }
    if ( ( areaPtr[0] > spPtr->header.x1 ) &&
         ( areaPtr[1] > spPtr->header.y1 ) &&
         ( areaPtr[0] < spPtr->header.x2 ) &&
         ( areaPtr[1] < spPtr->header.y2 ) ) {
        return 0;

    }
    if ( ( areaPtr[2] > spPtr->header.x1 ) &&
         ( areaPtr[3] > spPtr->header.y1 ) &&
         ( areaPtr[2] < spPtr->header.x2 ) &&
         ( areaPtr[3] < spPtr->header.y2 ) ) {
        return 0;

    }
    return -1;
}

/*
 *--------------------------------------------------------------
 *
 * ScaleSP --
 *
 *      This procedure is invoked to scale an item.
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
 *      This just moves the origin wrt to the scale.
 *
 *--------------------------------------------------------------
 */
static void ScaleSP( Tk_Canvas canvas, Tk_Item *itemPtr, double originX,
                     double originY, double scaleX, double scaleY )
{
    SPItem *spPtr = (SPItem *) itemPtr;

    /* Scale all coordinates and set their related values */
    spPtr->x = originX + scaleX * ( spPtr->x - originX );
    spPtr->y = originY + scaleY * ( spPtr->y - originY );

    ComputeSPBbox( canvas, spPtr );
    return;
}

/*
 *--------------------------------------------------------------
 *
 * TranslateSP --
 *
 *      This procedure is called to move an item by a given amount.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The position of the item is offset by (xDelta, yDelta), and
 *      the bounding box is updated in the generic part of the item structure.
 *
 *--------------------------------------------------------------
 */
static void TranslateSP( Tk_Canvas canvas, Tk_Item *itemPtr,
                         double deltaX,  double deltaY )
{
    SPItem *spPtr = (SPItem *) itemPtr;

    spPtr->x += deltaX;
    spPtr->y += deltaY;
    ComputeSPBbox( canvas, spPtr );
}

/*
 *--------------------------------------------------------------
 *
 * SPToPostscript --
 *
 *      This procedure is called to generate Postscript for the item.
 *
 * Results:
 *      Always returns TCL_OK. The drawing is done by the component items.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */
static int SPToPostscript( Tcl_Interp *interp, Tk_Canvas canvas,
                           Tk_Item *itemPtr, int prepass )
{
    return TCL_OK;
}


/*
 *--------------------------------------------------------------
 *
 * ComputeSPBbox --
 *
 *      This procedure is invoked to compute the bounding box of
 *      all the pixels that may be drawn.
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
static void ComputeSPBbox( Tk_Canvas canvas, SPItem *spPtr )
{
    /*  Ask AST about the Plot dimensions, use graphics bbox etc. */
}



/*
 *--------------------------------------------------------------
 *
 * FrameSetParseProc --
 *
 *      Provide parsing for -frameset option.
 *
 *--------------------------------------------------------------
 */
static int FrameSetParseProc( ClientData clientData, Tcl_Interp *interp,
                              Tk_Window tkwin, CONST char *value,
                              char *widgRec, int offset )
{
    AstFrameSet **ptr;
    *ptr = (AstFrameSet *)(widgRec + offset);
    long longResult;

    if ( Tcl_ExprLong( interp, value, &longResult ) != TCL_OK ) {
        return TCL_ERROR;
    }
    *ptr = (AstFrameSet *) longResult;
    return TCL_OK;
}


/*
 *--------------------------------------------------------------
 *
 * FrameSetPrintProc --
 *
 *      Return -frameset option as string.
 *
 *--------------------------------------------------------------
 */
static char *FrameSetPrintProc( ClientData clientData, Tk_Window tkwin,
                                char *widgRec, int offset,
                                Tcl_FreeProc **freeProcPtr )
{
    AstFrameSet **ptr;
    char *p;
    long longResult;
    *ptr = (AstFrameSet *)(widgRec + offset);
    p = (char *) ckalloc(24);
    Tcl_PrintDouble( (Tcl_Interp *) NULL, (long) *ptr, p );
    *freeProcPtr = TCL_DYNAMIC;
    return p;
}
