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
#include <string.h>
#include <float.h>
#include <tk.h>
#include <ast.h>
#include <grf_tkcan.h>

#define MAX(a,b) ( (a) > (b) ? (a) : (b) )
#define MIN(a,b) ( (a) < (b) ? (a) : (b) )

/*
 * Define a structure for containing all the necessary information
 * to describe an item.
 */
typedef struct SPItem  {
    Tk_Item header;             /* Mandatory Tk header information */
    double *dataPtr;            /* Data values */
    double *coordPtr;           /* Coordinates for each data value */
    int numPoints;              /* Number of data values */
    AstFrameSet *framesets[2];  /* The FrameSets associated with item */
    char *dataunits;            /* The data units */
    char *datalabel;            /* The data units label */
    char *options;              /* AST options used when drawing item */
    char utag[22];              /* Unique tag for AST graphics we create */
    double x, y;                /* Coordinates of item reference point */
    double width;               /* Width of item when not filling */
    double height;              /* Height of item when not filling */
    double scale;               /* Scale factor of item */
    int fill;                   /* Whether to fill canvas with item */
    Tk_Anchor tkanchor;         /* Where to anchor item relative to (x,y). */
    Tcl_Interp *interp;         /* The Tcl interpreter */
    double xmin;                /* Minimum physical X coordinate for plot */
    double xmax;                /* Maximum physical X coordinate for plot */
    double ymin;                /* Minimum physical Y coordinate for plot */
    double ymax;                /* Maximum physical Y coordinate for plot */
} SPItem;

static int tagCounter = 0;      /* Counter for creating unique tags */

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

  {TK_CONFIG_DOUBLE, "-width", (char *) NULL, (char *) NULL,
   "100.0", Tk_Offset(SPItem, width), TK_CONFIG_DONT_SET_DEFAULT},

  {TK_CONFIG_DOUBLE, "-height", (char *) NULL, (char *) NULL,
   "100.0", Tk_Offset(SPItem, height), TK_CONFIG_DONT_SET_DEFAULT},

  {TK_CONFIG_BOOLEAN, "-fill", (char *) NULL, (char *) NULL,
   "1", Tk_Offset(SPItem, fill), TK_CONFIG_DONT_SET_DEFAULT},

  {TK_CONFIG_STRING, "-astoptions", (char *) NULL, (char *) NULL,
   "Grid=1,DrawAxes=1", Tk_Offset(SPItem, options), TK_CONFIG_NULL_OK},

  {TK_CONFIG_STRING, "-dataunits", (char *) NULL, (char *) NULL,
   "", Tk_Offset(SPItem, dataunits), TK_CONFIG_NULL_OK},

  {TK_CONFIG_STRING, "-datalabel", (char *) NULL, (char *) NULL,
   "", Tk_Offset(SPItem, datalabel), TK_CONFIG_NULL_OK},

  {TK_CONFIG_CUSTOM, "-frameset", (char *) NULL, (char *) NULL,
   (char *) NULL, Tk_Offset(SPItem, framesets), TK_CONFIG_NULL_OK,
   &framesetOption},

  {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
   (char *) NULL, 0, 0}
};

/*
 * Prototypes for procedures defined in this file:
 */
static int ReadDataList( Tcl_Interp *interp, SPItem *spPtr,
                         Tcl_Obj *CONST dataValues );

static void ComputeSPBbox( Tk_Canvas canvas, SPItem *spPtr );

static int ConfigureSP( Tcl_Interp *interp, Tk_Canvas canvas,
                        Tk_Item *itemPtr, int objc,
                        Tcl_Obj *CONST objv[], int flags );

static int CreateSP( Tcl_Interp *interp, Tk_Canvas canvas,
                     struct Tk_Item *itemPtr, int objc,
                     Tcl_Obj *CONST objv[] );

static void DeleteSP( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display );

static void ScaleSP( Tk_Canvas canvas, Tk_Item *itemPtr, double originX,
                     double originY, double scaleX, double scaleY );

static int SPCoords( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
                     int objc, Tcl_Obj *CONST objv[] );

static void DisplaySP( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display,
                       Drawable dst, int x, int y, int width, int height );

static int SPToArea( Tk_Canvas canvas, Tk_Item *itemPtr, double *areaPtr );

static double SPToPoint( Tk_Canvas canvas, Tk_Item *itemPtr,
                         double *pointPtr );

static int SPToPostscript( Tcl_Interp *interp, Tk_Canvas canvas,
                           Tk_Item *itemPtr, int prepass );

static void TranslateSP( Tk_Canvas canvas, Tk_Item *itemPtr, double deltaX,
                         double deltaY );

static void ClearSPSubItems( Tk_Canvas canvas, SPItem *spPtr );

static void GenerateSPPlotFrameSet( SPItem *spPtr );

static void MakeSPSpectral( SPItem *spPtr );

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


/**
 * SpectralPlot_Init --
 *
 *   This procedure initialises the spectral_plot canvas item.
 */
int SpectralPlot_Init()
{
    Tk_CreateItemType( &spectralPlotType );
    return TCL_OK;
}

/**
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
 */
static int CreateSP( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
                     int objc, Tcl_Obj *CONST objv[] )
{
    long longResult;
    SPItem *spPtr = (SPItem *) itemPtr;

    fprintf( stderr, "CreateSP() \n" );

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
    spPtr->coordPtr = NULL;
    spPtr->numPoints = 0;
    spPtr->framesets[0] = NULL;
    spPtr->framesets[1] = NULL;
    spPtr->options = "Grid=1,DrawAxes=1";
    spPtr->dataunits = NULL;
    spPtr->datalabel = NULL;
    spPtr->fill = 1;
    spPtr->x = 0.0;
    spPtr->y = 0.0;
    spPtr->width = 100.0;
    spPtr->height = 100.0;
    spPtr->scale = 1.0;
    spPtr->tkanchor = TK_ANCHOR_CENTER;
    spPtr->interp = interp;

    /* Create a unique tag for AST graphic elements */
    sprintf( spPtr->utag, "gaiaSpectralPlot%d", tagCounter++ );

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
    spPtr->framesets[0] = (AstFrameSet *) longResult;
    spPtr->framesets[1] = NULL;

    /*  And configure using any options */
    if ( ConfigureSP( interp, canvas, itemPtr, objc-4, objv+4, 0 ) != TCL_OK ){
        DeleteSP( canvas, itemPtr, Tk_Display( Tk_CanvasTkwin( canvas ) ) );
        return TCL_ERROR;
    }

    /* Initialise the GRF interface, XXX are there any issues with interp
     * persistence? */
    astTk_Init( interp, Tk_PathName( Tk_CanvasTkwin( canvas ) ) );

    return TCL_OK;
}

/**
 * ReadDataList --
 *
 *    Read a list of spectral data value from a list.
 *
 *    Also sets the minimum and maximum data values in spPtr.
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

    spPtr->ymax = -DBL_MAX;
    spPtr->ymin =  DBL_MAX;

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
        spPtr->ymin = MIN( spPtr->ymin, spPtr->dataPtr[i] );
        spPtr->ymax = MAX( spPtr->ymax, spPtr->dataPtr[i] );
    }
    return TCL_OK;
}

/**
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
 */
static int SPCoords( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
                     int objc, Tcl_Obj *CONST objv[] )
{
    SPItem *spPtr = (SPItem *) itemPtr;
    char x[TCL_DOUBLE_SPACE], y[TCL_DOUBLE_SPACE];

    fprintf( stderr, "SPCoords() \n" );

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

/**
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
 */
static int ConfigureSP( Tcl_Interp *interp, Tk_Canvas canvas,
                        Tk_Item *itemPtr, int objc,
                        Tcl_Obj *CONST objv[], int flags )
{
    SPItem *spPtr = (SPItem *) itemPtr;
    Tk_Window tkwin;

    fprintf( stderr, "ConfigureSP() \n" );

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

/**
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
 */
static void DeleteSP( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display )
{
    SPItem *spPtr = (SPItem *) itemPtr;

    fprintf( stderr, "DeleteSP() \n" );

    if ( spPtr->dataPtr != NULL) {
        ckfree( (char *) spPtr->dataPtr );
    }
    if ( spPtr->coordPtr != NULL) {
        ckfree( (char *) spPtr->coordPtr );
    }
    spPtr->numPoints = 0;

    /* Delete the actual graphics */
    ClearSPSubItems( canvas, spPtr );
}

/**
 * DisplaySP --
 *
 *      This procedure is invoked to draw an item in a given drawable.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      ItemPtr is drawn in drawable...
 */
static void DisplaySP( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display,
                       Drawable drawable, int x, int y, int width,
                       int height )
{
    SPItem *spPtr = (SPItem *) itemPtr;
    Tk_Window tkwin;
    int i;
    char *tags;
    float graphbox[4];
    int ixo;
    int iyo;
    int iwidth;
    int iheight;
    int iborder;
    double basebox[4];
    AstPlot *plot;

    fprintf( stderr, "DisplaySP() \n" );

    if ( spPtr->dataPtr == NULL ) {
        return;
    }

    if ( spPtr->framesets[0] == NULL ) {
        return;
    }

    /* Generate the Plot frameset, if required (will be redone each time the 
     *  frameset changes). This also creates a new coordPtr array. */
    if ( spPtr->framesets[1] == NULL ) {
        GenerateSPPlotFrameSet( spPtr );
    }

    /* Set the tag used by the canvas items we create, need to be under the
       control of this item, so use a unique value */
    astTk_Tag( spPtr->utag );

    /* Clear existing items */
    ClearSPSubItems( canvas, spPtr );

    /* Set the draw area to the size of canvas, minus a 10% border */
    tkwin = Tk_CanvasTkwin( canvas );

    ixo = Tk_X( tkwin );
    iyo = Tk_Y( tkwin );
    iwidth = Tk_Width( tkwin );
    iheight = Tk_Height( tkwin );
    iborder = ( width / 10 );

    ixo += iborder;
    iyo += iborder;
    iwidth -= ( 2 * iborder );
    iheight -= ( 2 * iborder );

    graphbox[0] = graphbox[2] = ixo;
    graphbox[1] = graphbox[3] = iyo;
    graphbox[2] += iwidth;
    graphbox[1] += iheight;

    /* Set the limits of the drawing region in physical coordinates, AST__BASE
     * of the plot frameset. */
    basebox[0] = spPtr->xmin;
    basebox[1] = spPtr->ymin;
    basebox[2] = spPtr->xmin;
    basebox[3] = spPtr->ymax;

    fprintf( stderr, "basebox: %f,%f,%f,%f\n", basebox[0], basebox[1],
             basebox[2], basebox[3] );
    fprintf( stderr, "graphbox: %f,%f,%f,%f\n", graphbox[0], graphbox[1],
             graphbox[2], graphbox[3] );

    /* Create the AstPlot */
    plot = astPlot( spPtr->framesets[1], graphbox, basebox, spPtr->options );

    /* And plot the grid axes */
    astGrid( plot );

    /* Finally the spectrum */
    

    if ( !astOK ) {
        astClearStatus;
    }

    /*  Stop tagging canvas items */
    astTk_Tag( NULL );

}

/**
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
 */
static double SPToPoint( Tk_Canvas canvas, Tk_Item *itemPtr,
                         double *pointPtr )
{
    SPItem *spPtr = (SPItem *) itemPtr;
    double xDiff, yDiff;

    fprintf( stderr, "SPToPoint() \n" );

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

/**
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
 */
static int SPToArea( Tk_Canvas canvas, Tk_Item *itemPtr, double *areaPtr )
{
    SPItem *spPtr = (SPItem *) itemPtr;

    fprintf( stderr, "SPToArea() \n" );

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

/**
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
 */
static void ScaleSP( Tk_Canvas canvas, Tk_Item *itemPtr, double originX,
                     double originY, double scaleX, double scaleY )
{
    SPItem *spPtr = (SPItem *) itemPtr;

    fprintf( stderr, "ScaleSP() \n" );

    /* Scale all coordinates and set their related values */
    spPtr->x = originX + scaleX * ( spPtr->x - originX );
    spPtr->y = originY + scaleY * ( spPtr->y - originY );
    spPtr->width *= scaleX;
    spPtr->height *= scaleY;

    ComputeSPBbox( canvas, spPtr );
    return;
}

/**
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
 */
static void TranslateSP( Tk_Canvas canvas, Tk_Item *itemPtr,
                         double deltaX,  double deltaY )
{
    SPItem *spPtr = (SPItem *) itemPtr;
    spPtr->x += deltaX;
    spPtr->y += deltaY;
    ComputeSPBbox( canvas, spPtr );
}

/**
 * SPToPostscript --
 *
 *      This procedure is called to generate Postscript for the item.
 *
 * Results:
 *      Always returns TCL_OK. The drawing is done by the component items.
 *
 * Side effects:
 *      None.
 */
static int SPToPostscript( Tcl_Interp *interp, Tk_Canvas canvas,
                           Tk_Item *itemPtr, int prepass )
{
    return TCL_OK;
}

/**
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
 */
static void ComputeSPBbox( Tk_Canvas canvas, SPItem *spPtr )
{
    Tk_Window tkwin;
    tkwin = Tk_CanvasTkwin( canvas );

    if ( spPtr->fill ) {
        /*  The whole canvas. */
        spPtr->header.x1 = spPtr->header.x2 = Tk_X( tkwin );
        spPtr->header.y1 = spPtr->header.y2 = Tk_Y( tkwin );
        spPtr->header.x2 += Tk_Width( tkwin );
        spPtr->header.y2 += Tk_Height( tkwin );
    }
    else {
        spPtr->header.x1 = spPtr->header.x2 = spPtr->x;
        spPtr->header.y1 = spPtr->header.y2 = spPtr->y;
        spPtr->header.x2 += spPtr->width;
        spPtr->header.y2 += spPtr->height;
    }
}

/**
 * FrameSetParseProc --
 *
 *      Provide parsing for -frameset option.
 */
static int FrameSetParseProc( ClientData clientData, Tcl_Interp *interp,
                              Tk_Window tkwin, CONST char *value,
                              char *widgRec, int offset )
{
    long longResult;
    AstFrameSet ***ptr;
    ptr = (AstFrameSet ***)( widgRec + offset );

    if ( Tcl_ExprLong( interp, value, &longResult ) != TCL_OK ) {
        return TCL_ERROR;
    }
    *ptr[0] = (AstFrameSet *) longResult;
    *ptr[1] = (AstFrameSet *) NULL;
    return TCL_OK;
}


/**
 * FrameSetPrintProc --
 *
 *      Return -frameset option as string.
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

/**
 * ClearSPSubItems --
 *
 *     Clear any items we've already drawn (before refresh or when we're being
 *     deleted).
 */
static void ClearSPSubItems( Tk_Canvas canvas, SPItem *spPtr )
{
    /* Don't worry about errors, shouldn't be fatal, but cannot do this
     * if the whole canvas is being destroyed, so check for a NULL window. */
    Tk_Window tkwin = Tk_CanvasTkwin( canvas );
    if ( tkwin != NULL ) {
        Tcl_VarEval( spPtr->interp, Tk_PathName( tkwin ), " delete ",
                     spPtr->utag, (char *) NULL );
    }
}

/**
 * GenerateSPPlotFrameSet --
 *
 *     Generate a FrameSet suitable for drawing a 2D data plot using our data
 *     values and 1D frameset. Also creates the coordinate positions array.
 */
static void GenerateSPPlotFrameSet( SPItem *spPtr )
{
    int i;
    double *tmpPtr;

    /* Create the spectral FrameSet */
    MakeSPSpectral( spPtr );

    /* Get the centres of the pixel positions in current coordinates (so we
     * can eventually go from current coordinates through to graphics
     * coordinates when actually drawing the spectrum).
     */
    if ( spPtr->coordPtr != NULL ) {
        ckfree( (char *) spPtr->coordPtr );
    }
    spPtr->coordPtr = (double *)ckalloc( spPtr->numPoints * sizeof( double ) );
    tmpPtr = (double *) ckalloc( spPtr->numPoints * sizeof( double ) );
    for ( i = 0; i < spPtr->numPoints; i++ ) {
        tmpPtr[i] = (double) i + 1;
    }
    astTran1( spPtr->framesets[0], spPtr->numPoints, tmpPtr, 1, 
              spPtr->coordPtr );

    /* Set the axis range. */
    spPtr->xmin = DBL_MAX;
    spPtr->xmax = -DBL_MAX;
    for ( i = 0; i < spPtr->numPoints; i++ ) {
        spPtr->xmin = MIN( spPtr->xmin, spPtr->coordPtr[i] );
        spPtr->xmax = MAX( spPtr->xmax, spPtr->coordPtr[i] );
    }
}
 

/**
 * MakeSPSpectral --
 *
 *     Generate a FrameSet suitable for drawing a 2D data plot using our data
 *     values and 1D frameset.
 */
static void MakeSPSpectral( SPItem *spPtr )
{
    AstFrame *cfrm;
    AstFrame *f1;
    AstFrame *f2;
    AstFrame *frm1;
    AstFrame *frm2;
    AstMapping *map;
    AstMapping *smap;
    AstUnitMap *umap;
    char const *system;
    int havespecframe;
    int havefluxframe;
    int iaxes[2];

    astBegin;
    
    /* -------------------------------------------------------------
     * Create a mapping to transform from GRID positions to spectral
     * coordinates and data values.
     * -------------------------------------------------------------
     */
    
    /* Get a simplified mapping from the base to current frames. This will
     * be used to transform from GRID coordinates to spectral
     * coordinates. */
    map = astGetMapping( spPtr->framesets[0], AST__BASE, AST__CURRENT );
    smap = astSimplify( map );
    
    /* Save a pointer to the current frame. */
    cfrm = astGetFrame( spPtr->framesets[0], AST__CURRENT );

    /* The data units map onto themselves directly so use a unitmap. */
    umap = astUnitMap( 1, "" );

    /*  Create a compound of these two mappings. */
    map = (AstMapping *) astCmpMap( smap, umap, 0, "" );

    /* Get a Frame representing the input coordinates, uses a GRID axis of
     * the base frame and a default axis. */
    iaxes[0] = 1;
    iaxes[1] = 0;
    frm1 = astGetFrame( spPtr->framesets[0], AST__BASE );
    frm1 = astPickAxes( frm1, 2, iaxes, NULL );

    /* Set up label, symbol and units for axis 2 (the data values axis). */
    astSetC( frm1, "Symbol(2)", "Data" );
    if ( spPtr->datalabel != NULL ) {
        astSetC( frm1, "Label(2)", spPtr->datalabel );
    }
    else {
        astSetC( frm1, "Label(2)", "Data value" );
    }
    if ( spPtr->dataunits != NULL ) {
        astSetC( frm1, "Unit(2)", spPtr->dataunits );
    }

    /* Clear domain and title which are now incorrect. */
    astClear( frm1, "Domain" );
    astClear( frm1, "Title" );

    /* ---------------------------------
     * Create the coordinate data frame.
     * ---------------------------------
     */
    
    /* Use first axis from the current frame as the spectral axis. */
    iaxes[0] = 1;
    f1 = astPickAxes( spPtr->framesets[0], 1, iaxes, NULL );
    havespecframe = astIsASpecFrame( f1 );

    /* If we have data units that can be understood (as some kind of flux in
     * general), then create a FluxFrame using them, but only if we also have
     * a SpecFrame. Note that SpecFrames are not that useful as we don't use a
     * reference spectrum (so no alignments are used in this item).
     */
    if ( havespecframe && spPtr->dataunits != NULL && astOK ) {
        f2 = (AstFrame *) astFluxFrame( AST__BAD, NULL, "" );
        astSetC( f2, "unit(1)", spPtr->dataunits );

        /* Get the default System value from the FluxFrame. This will
         * depend on the units. If the units do not correspond to any of
         * the supported flux systems, then an error will be thrown
         */
        system = astGetC( f2, "System" );
        if ( ! astOK ) {
            astClearStatus;
            f2 = NULL;
            havefluxframe = 0;
        }
        else {
            havefluxframe = 1;
        }
    }
    if ( f2 == NULL ) {
        havefluxframe = 0;
        f2 = astFrame( 1, "" );
    }
    
    /* Create the full frame using the spectral frame and flux frame. */
    if ( havespecframe && havefluxframe ) {
        frm2 = (AstFrame *) astSpecFluxFrame( (AstSpecFrame *) f1, 
                                              (AstFluxFrame *) f2, "" );
    } 
    else {
        frm2 = (AstFrame *) astCmpFrame( f1, f2, "" );
    }

    /* Clear digits and format, unless set in the input frameset. */
    if ( ! astTest( cfrm, "Format(1)" ) ) {
        astClear( frm2, "Format(1)" );
    }
    if ( ! astTest( cfrm, "Format(1)" ) ) {
        astClear( frm2, "Format(1)" );
    }
    if ( ! astTest( cfrm, "Digits(1)" ) ) {
        astClear( frm2, "digits(1)" );
    }

    /* Set symbol, label and units for second axis, if not already done. */
    astSetC( frm2, "Symbol(2)", "Data" );
    if ( spPtr->datalabel != NULL ) {
        astSetC( frm2, "Label(2)", spPtr->datalabel );
    }
    else {
        astSetC( frm2, "Label(2)", "Data value" );
    }

    /* If this is a FluxFrame then the units are already set (and active). */
    if ( ! havefluxframe && spPtr->dataunits != NULL ) {
        astSetC( frm2, "Unit(2)", spPtr->dataunits );
    }

    /* The domain of this frame is "DATAPLOT" (as in KAPPA) */
    astSetC( frm2, "Domain", "DATAPLOT" );

    /* Now create the output frameset, which has frm2 as current
     * and frm1 as base. */
    spPtr->framesets[1] = astFrameSet( frm1, "" );
    astAddFrame( spPtr->framesets[1], AST__BASE, map, frm2 );

    /* Export the FrameSet and make sure everything else is released. */
    astExport( spPtr->framesets[1] );
    if ( !astOK ) {
        astClearStatus;
    }
    astEnd;
}
