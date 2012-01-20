/*+
 *  Name:
 *     gaiaSpectralPlot

 *  Purpose:
 *     Defines a Tk canvas item "spectral_plot" that displays a spectrum in a
 *     2D axes plot.

 *  Description:
 *     This item displays an array of data values as a polyline in
 *     a plot. The two axes are determined using an AST spectral coordinate
 *     system (wavelength, frequency, velocity etc.) and an optional set of
 *     data units.
 *
 *     An additional polyline can also be drawn. This is intended to serve
 *     as a reference spectrum and plays no part in determining the data
 *     limits used in the plot.
 *
 *     The spectra can have error bars plotted using associated standard
 *     deviations.

 *  Notes:
 *     The reference spectrum idea is primitive and should not be
 *     extended. Use a system that shares the AST plot (and uses several
 *     instances of this item) if additional spectra are required.

 *  Copyright:
 *     Copyright (C) 2006-2007 Particle Physics & Astronomy Research Council.
 *     Copyright (C) 2008-2009 Science and Technology Facilities Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  Authors:
 *     PWD: P.W. Draper (JAC, Durham University).

 *  Changes:
 *     14-FEB-2006: (PWD)
 *        Original version.
 *-
 *.
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <tk.h>
#include <ast.h>
#include "grf_tkcan.h"
#include "rtdCanvas.h"
#include "GaiaArray.h"

#define MAX(a,b) ( (a) > (b) ? (a) : (b) )
#define MIN(a,b) ( (a) < (b) ? (a) : (b) )

#define DEBUG 0

#define LONG_LENGTH 25

/*  Maximum number of dimensions that a WCS frame can have (AST/NDF). */
#define MAXDIM 7

/*  Length of the serifs on errors bars, pixels. */
#define SERIF_LENGTH 2.5

/*
 * Define a structure for containing all the necessary information
 * to describe an item.
 */
typedef struct SPItem  {
    Tk_Item header;             /* Mandatory Tk header information */
    AstFrameSet *framesets[2];  /* The FrameSets associated with item */
    AstMapping *mapping;        /* 1D mapping used */
    AstPlot *plot;              /* The AstPlot for drawing regions */
    Tcl_Interp *interp;         /* The Tcl interpreter */
    Tk_Anchor tkanchor;         /* Where to anchor relative to (x,y). */
    Tk_Item *polyline;          /* rtdPolyline for main line context */
    Tk_Item *refpolyline;       /* rtdPolyline for reference line context */
    Tk_Item *mainsegment;       /* Segmented line for main error bars */
    Tk_Item *refsegment;        /* Segmented line for reference error bars */
    XColor *linecolour;         /* Foreground color for polyline */
    XColor *reflinecolour;      /* Foreground color for reference polyline */
    XColor *errorcolour;        /* Colour for any error bars */
    char *datalabel;            /* The data units label */
    char *dataunits;            /* The data units */
    char *options;              /* AST options used when drawing item */
    char *tagPtr;               /* All tags in use for item */
    char utag[22];              /* Unique tag for AST graphics we create */
    double *coordPtr;           /* Coordinates of main spectrum */
    double *dataPtr;            /* Data values of main spectrum */
    double *refDataPtr;         /* Data values for a reference spectrum */
    double *refVarPtr;          /* Data variance values for a reference spectrum */
    double *tmpPtr[2];          /* Temporary arrays for transformed coordinates etc. */
    double *varPtr;             /* Data variance values of main spectrum */
    double badvalue;            /* The value to replace AST__BAD with */
    double height;              /* Height of item */
    double nsigma;              /* Number of std shown as error bar */
    double width;               /* Width of item */
    double x, y;                /* Coordinates of item reference point */
    double xborder;             /* Fractional border for X plot axes, only used when autoscaling using scale command */
    double xleft;               /* Right-hand physical X coordinate for plot */
    double xright;              /* Left-hand physical X coordinate for plot */
    double yborder;             /* Fractional border for Y plot axes, only used when autoscaling using scale command */
    double ybot;                /* Physical Y coordinate for plot top */
    double ymax;                /* Maximum physical Y coordinate */
    double ymin;                /* Minimum physical Y coordinate */
    double ytop;                /* Physical Y coordinate for plot bottom */
    int axis;                   /* Axis of supplied WCS that is spectral */
    int fixdatarange;           /* If set the Y range is fixed to ytop to ybot, otherwise the max and min values are used */
    int fixedscale;             /* If set do not scale item with canvas */
    int frequency;              /* Sampling rate for error bars, 1 = every point */
    int isDSB;                  /* Spectral coordinates are dual sideband */
    int linewidth;              /* Width of polyline */
    int newplot;                /* Whether to regenerate plot */
    int numPoints;              /* Number of data values */
    int offset;                 /* The offset of the given spectrum along the base frame of the FrameSet (NDF origin -1 ). */
    int showaxes;               /* Whether to show the grid */
    int showerrorbars;          /* Whether to show error bars */
    int xminmax;                /* If true then use min->max X coordinates */
    int xpositive;              /* If true only positive X coordinates will be used. */
    int ypositive;              /* If true only positive data values will be used. */
} SPItem;

static int tagCounter = 0;      /* Counter for creating unique tags */

/* Item canvas tags */
static int CanvasTagsParseProc( ClientData clientData, Tcl_Interp *interp,
                                Tk_Window tkwin, CONST char *value,
                                char *widgRec, int offset );
static Tk_CustomOption tagsOption = {
    (Tk_OptionParseProc *) CanvasTagsParseProc,
    (Tk_OptionPrintProc *) Tk_CanvasTagsPrintProc,
    (ClientData) NULL
};

/* FrameSet handling */
static int FrameSetParseProc( ClientData clientData, Tcl_Interp *interp,
                              Tk_Window tkwin, CONST char *value,
                              char *widgRec, int offset );
static char *FrameSetPrintProc( ClientData clientData, Tk_Window tkwin,
                                char *widgRec, int offset,
                                Tcl_FreeProc **freeProcPtr );
static Tk_CustomOption framesetOption = {
    (Tk_OptionParseProc *) FrameSetParseProc,
    (Tk_OptionPrintProc *) FrameSetPrintProc,
    (ClientData) NULL
};

static int MappingParseProc( ClientData clientData, Tcl_Interp *interp,
                             Tk_Window tkwin, CONST char *value,
                             char *widgRec, int offset );
static char *MappingPrintProc( ClientData clientData, Tk_Window tkwin,
                               char *widgRec, int offset,
                               Tcl_FreeProc **freeProcPtr );
static Tk_CustomOption mappingOption = {
    (Tk_OptionParseProc *) MappingParseProc,
    (Tk_OptionPrintProc *) MappingPrintProc,
    (ClientData) NULL
};

/* Configuration options */
static Tk_ConfigSpec configSpecs[] = {

    {TK_CONFIG_ANCHOR, "-anchor", (char *) NULL, (char *) NULL,
     "nw", Tk_Offset(SPItem, tkanchor), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_INT, "-axis", (char *) NULL, (char *) NULL,
     "1.0", Tk_Offset(SPItem, axis), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_DOUBLE, "-badvalue", (char *) NULL, (char *) NULL,
     "0.0", Tk_Offset(SPItem, badvalue), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_STRING, "-datalabel", (char *) NULL, (char *) NULL,
     "Data value", Tk_Offset(SPItem, datalabel), TK_CONFIG_NULL_OK, NULL},

    {TK_CONFIG_STRING, "-dataunits", (char *) NULL, (char *) NULL,
     "", Tk_Offset(SPItem, dataunits), TK_CONFIG_NULL_OK, NULL},

    {TK_CONFIG_BOOLEAN, "-fixdatarange", (char *) NULL, (char *) NULL,
     "0", Tk_Offset(SPItem, fixdatarange), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_BOOLEAN, "-fixedscale", (char *) NULL, (char *) NULL,
     "0", Tk_Offset(SPItem, fixedscale), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_CUSTOM, "-frameset", (char *) NULL, (char *) NULL,
     (char *) NULL, Tk_Offset(SPItem, framesets), TK_CONFIG_NULL_OK,
     &framesetOption},

    {TK_CONFIG_STRING, "-gridoptions", (char *) NULL, (char *) NULL,
     "DrawTitle=0", Tk_Offset(SPItem, options), TK_CONFIG_NULL_OK, NULL},

    {TK_CONFIG_DOUBLE, "-height", (char *) NULL, (char *) NULL,
     "100.0", Tk_Offset(SPItem, height), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_COLOR, "-linecolour", (char *) NULL, (char *) NULL,
     "blue", Tk_Offset(SPItem, linecolour), TK_CONFIG_NULL_OK, NULL},

    {TK_CONFIG_COLOR, "-errorcolour", (char *) NULL, (char *) NULL,
     "red", Tk_Offset(SPItem, errorcolour), TK_CONFIG_NULL_OK, NULL},

    {TK_CONFIG_DOUBLE, "-nsigma", (char *) NULL, (char *) NULL,
     "1.0", Tk_Offset(SPItem, nsigma), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_INT, "-frequency", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(SPItem, frequency), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_PIXELS, "-linewidth", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(SPItem, linewidth), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_CUSTOM, "-mapping", (char *) NULL, (char *) NULL,
     (char *) NULL, Tk_Offset(SPItem, mapping), TK_CONFIG_NULL_OK,
     &mappingOption},

    {TK_CONFIG_INT, "-offset", (char *) NULL, (char *) NULL,
     "0.0", Tk_Offset(SPItem, offset), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_COLOR, "-reflinecolour", (char *) NULL, (char *) NULL,
     "green", Tk_Offset(SPItem, reflinecolour), TK_CONFIG_NULL_OK, NULL},

    {TK_CONFIG_BOOLEAN, "-showaxes", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(SPItem, showaxes), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_BOOLEAN, "-showerrorbars", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(SPItem, showerrorbars), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_CUSTOM, "-tags", (char *) NULL, (char *) NULL,
     (char *) NULL, Tk_Offset(SPItem, tagPtr), TK_CONFIG_NULL_OK,
     &tagsOption},

    {TK_CONFIG_DOUBLE, "-width", (char *) NULL, (char *) NULL,
     "100.0", Tk_Offset(SPItem, width), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_DOUBLE, "-x", (char *) NULL, (char *) NULL,
     "0.0", Tk_Offset(SPItem, x), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_DOUBLE, "-xborder", (char *) NULL, (char *) NULL,
     "0.03", Tk_Offset(SPItem, xborder), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_BOOLEAN, "-xminmax", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(SPItem, xminmax), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_BOOLEAN, "-xpositive", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(SPItem, xpositive), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_DOUBLE, "-y", (char *) NULL, (char *) NULL,
     "0.0", Tk_Offset(SPItem, y), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_DOUBLE, "-yborder", (char *) NULL, (char *) NULL,
     "0.20", Tk_Offset(SPItem, yborder), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_DOUBLE, "-ybot", (char *) NULL, (char *) NULL,
     "0.0", Tk_Offset(SPItem, ybot), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_BOOLEAN, "-ypositive", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(SPItem, ypositive), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_DOUBLE, "-ytop", (char *) NULL, (char *) NULL,
     "0.0", Tk_Offset(SPItem, ytop), TK_CONFIG_DONT_SET_DEFAULT, NULL},

    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
     (char *) NULL, 0, 0, NULL}
};

/*
 * Prototypes for procedures defined in this file:
 */
static int    SPConfigure( Tcl_Interp *interp, Tk_Canvas canvas,
                           Tk_Item *itemPtr, int objc, Tcl_Obj *CONST objv[],
                           int flags );
static int    SPCoords( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
                        int objc, Tcl_Obj *CONST objv[] );
static int    SPCreate( Tcl_Interp *interp, Tk_Canvas canvas, struct
                        Tk_Item *itemPtr, int objc, Tcl_Obj *CONST objv[] );
static void   SPDelete( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display );
static void   SPDisplay( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display,
                         Drawable dst, int x, int y, int width, int height );
static int    SPToArea( Tk_Canvas canvas, Tk_Item *itemPtr, double *areaPtr );
static double SPToPoint( Tk_Canvas canvas, Tk_Item *itemPtr,
                         double *pointPtr );
static int    SPToPostscript( Tcl_Interp *interp, Tk_Canvas canvas,
                              Tk_Item *itemPtr, int prepass );
static void   SPScale( Tk_Canvas canvas, Tk_Item *itemPtr, double originX,
                       double originY, double scaleX, double scaleY );
static void   SPTranslate( Tk_Canvas canvas, Tk_Item *itemPtr, double deltaX,
                           double deltaY );

static void CorrectAnchor( SPItem *spPtr, double *x, double *y );
static void ClearSubItems( Tk_Canvas canvas, SPItem *spPtr );
static void ComputeBBox( Tk_Canvas canvas, SPItem *spPtr );
static void GeneratePlotFrameSet( SPItem *spPtr );
static void MakeSpectral( SPItem *spPtr );

/*
 * The structure below defines the item type, by means of procedures that can
 * be invoked by generic item code.
 */
static Tk_ItemType spectralPlotType = {
    "spectral_plot",               /* name           */
    sizeof(SPItem),                /* itemSize       */
    SPCreate,                      /* createProc     */
    configSpecs,                   /* configSpecs    */
    SPConfigure,                   /* configureProc  */
    SPCoords,                      /* coordProc      */
    SPDelete,                      /* deleteProc     */
    SPDisplay,                     /* displayProc    */
    TK_CONFIG_OBJS,                /* flags          */
    SPToPoint,                     /* pointProc      */
    SPToArea,                      /* areaProc       */
    SPToPostscript,                /* postscriptProc */
    SPScale,                       /* scaleProc      */
    SPTranslate,                   /* translateProc  */
    (Tk_ItemIndexProc *) NULL,     /* indexProc      */
    (Tk_ItemCursorProc *) NULL,    /* cursorProc     */
    (Tk_ItemSelectionProc *) NULL, /* selectionProc  */
    (Tk_ItemInsertProc *) NULL,    /* insertProc     */
    (Tk_ItemDCharsProc *) NULL,    /* dTextProc      */
    (Tk_ItemType *) NULL,          /* nextPtr        */
    (char *) NULL,                 /* Reserved for future extension. */
    0,                             /* Reserved for future extension. */
    (char *) NULL,                 /* Reserved for future extension. */
    (char *) NULL                  /* Reserved for future extension. */

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
 * SPCreate --
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
static int SPCreate( Tcl_Interp *interp, Tk_Canvas canvas, Tk_Item *itemPtr,
                     int objc, Tcl_Obj *CONST objv[] )
{
    SPItem *spPtr = (SPItem *) itemPtr;
    char *arg;
    int i;
#if DEBUG
    fprintf( stderr, "SPCreate() \n" );
#endif
    if ( objc < 2 ) {
        Tcl_AppendResult( interp, "wrong # args:  should be \"",
                          Tk_PathName( Tk_CanvasTkwin( canvas ) ),
                          "\" create ", itemPtr->typePtr->name,
                          "v1 v2 v3 ... ?options?", (char *) NULL );
        return TCL_ERROR;
    }
    /*
     * Carry out initialization that is needed in order to clean
     * up after errors during the the remainder of this procedure.
     */
    spPtr->axis = 1;
    spPtr->badvalue = -DBL_MAX;
    spPtr->coordPtr = NULL;
    spPtr->dataPtr = NULL;
    spPtr->datalabel = NULL;
    spPtr->dataunits = NULL;
    spPtr->errorcolour = None;
    spPtr->fixdatarange = 0;
    spPtr->fixedscale = 0;
    spPtr->framesets[0] = NULL;
    spPtr->framesets[1] = NULL;
    spPtr->frequency = 1;
    spPtr->height = 100.0;
    spPtr->interp = interp;
    spPtr->isDSB = 0;
    spPtr->linecolour = None;
    spPtr->linewidth = 1;
    spPtr->mainsegment = NULL;
    spPtr->mapping = NULL;
    spPtr->newplot = 1;
    spPtr->nsigma = 1.0;
    spPtr->numPoints = 0;
    spPtr->options = NULL;
    spPtr->plot = NULL;
    spPtr->polyline = NULL;
    spPtr->refDataPtr = NULL;
    spPtr->refVarPtr = NULL;
    spPtr->reflinecolour = None;
    spPtr->refpolyline = NULL;
    spPtr->refsegment = NULL;
    spPtr->showerrorbars = 0;
    spPtr->tagPtr = NULL;
    spPtr->tkanchor = TK_ANCHOR_NW;
    spPtr->tmpPtr[0] = NULL;
    spPtr->tmpPtr[1] = NULL;
    spPtr->varPtr = NULL;
    spPtr->width = 100.0;
    spPtr->x = 0.0;
    spPtr->xborder = 0.07;
    spPtr->xminmax = 1;
    spPtr->xpositive = 0;
    spPtr->y = 0.0;
    spPtr->yborder = 0.25;
    spPtr->ybot = DBL_MAX;
    spPtr->ypositive = 0;
    spPtr->ytop = -DBL_MAX;

    /* Create a unique tag for AST graphic elements */
    sprintf( spPtr->utag, "gaiaSpectralPlot%d", tagCounter++ );
    spPtr->tagPtr = strdup( spPtr->utag );

    /*
     * Count the number of values and then parse them into a point
     * array.  Leading arguments are assumed to be points if they
     * start with a digit or a minus sign followed by a digit.
     */
    for ( i = 0; i < objc; i++ ) {
        arg = Tcl_GetString( objv[i] );
        if ( ( arg[0] == '-' ) && ( arg[1] >= 'a' ) && ( arg[1] <= 'z' ) ) {
            break;
        }
    }
    if ( i && SPCoords( interp, canvas, itemPtr, i, objv ) != TCL_OK ) {
        SPDelete( canvas, itemPtr, Tk_Display( Tk_CanvasTkwin( canvas ) ) );
        return TCL_ERROR;
    }

    /* And configure using any options */
    if ( SPConfigure( interp, canvas, itemPtr, objc-i, objv+i, 0 ) != TCL_OK ){
        SPDelete( canvas, itemPtr, Tk_Display( Tk_CanvasTkwin( canvas ) ) );
        return TCL_ERROR;
    }

    /* Use default unit frameset when no other available */
    if ( spPtr->framesets[0] == NULL ) {
        AstFrame *f1, *f2;
        astBegin;
        f1 = astFrame( 1, " " );
        f2 = astFrame( 1, " " );
        spPtr->mapping = (AstMapping *) astUnitMap( 1, " " );
        spPtr->framesets[0] = astFrameSet( f1, " " );
        astAddFrame( spPtr->framesets[0], AST__BASE, spPtr->mapping, f2 );

        astExport( spPtr->framesets[0] );
        astExport( spPtr->mapping );

        spPtr->isDSB = 0;
        spPtr->axis = 1;

        astEnd;
    }

    /* Create the polylines we're going to use for drawing the spectrum,
     * these will be used directly for speed. */
    {
        Tcl_Obj *dummyObjs[4];
        Tcl_Obj *obj = Tcl_NewDoubleObj( 0.0 );
        dummyObjs[0] = obj;
        dummyObjs[1] = obj;
        dummyObjs[2] = obj;
        dummyObjs[3] = obj;

        if ( RtdLineCreate( interp, canvas, &spPtr->polyline, 4, dummyObjs )
             != TCL_OK ) {
            Tcl_AppendResult( interp, "\nInternal error: "
                              "Failed to create polyline object",
                              (char *) NULL );
            return TCL_ERROR;
        }

        if ( RtdLineCreate( interp, canvas, &spPtr->refpolyline, 4, dummyObjs )
             != TCL_OK ) {
            Tcl_AppendResult( interp, "\nInternal error: "
                              "Failed to create refpolyline object",
                              (char *) NULL );
            return TCL_ERROR;
        }
    }

    /* And the segments for any error bars. */
    {
        char *dummy[4] = { "0.0", "0.0", "0.0", "0.0" };

        if ( RtdSegmentCreate( interp, canvas, &spPtr->mainsegment, 4, dummy )
             != TCL_OK ) {
            Tcl_AppendResult( interp, "\nInternal error: "
                              "Failed to create segment object",
                              (char *) NULL );
            return TCL_ERROR;
        }

        if ( RtdSegmentCreate( interp, canvas, &spPtr->refsegment, 4, dummy )
             != TCL_OK ) {
            Tcl_AppendResult( interp, "\nInternal error: "
                              "Failed to create segment object",
                              (char *) NULL );
            return TCL_ERROR;
        }
    }

    /* Initialise the GRF interface. */
    astTk_Init( interp, Tk_PathName( Tk_CanvasTkwin( canvas ) ) );

    return TCL_OK;
}

/**
 * SPCoords --
 *
 *   This procedure is invoked to process the "coords" widget command.
 *
 *   This reads a list of doubles that are the spectral data values
 *   or accepts possibly two pointers to ARRAYinfo structures of already
 *   available values in one of the HDS types (_BYTE, _UBYTE, _WORD, _UWORD,
 *   _INTEGER, _REAL _DOUBLE). In the latter case you should use the format:
 *
 *      <canvas> coords <item> \
 *         "pointer" memory_address_of_ARRAYinfo memory_address_of_ARRAYinfo
 *
 *   The second pointer is to an array of variances. Set this to 0 if none
 *   are to be used (this is the only way to supply variances).
 *
 *   This is also extended to allow another set of data points to be
 *   supplied to support an additional spectrum (with the same coordinates, so
 *   must be from the same data set) to act as a "reference" spectrum. It is
 *   expected that this will be updated less frequently that the main
 *   spectrum and is not used to determine any data limits.
 *
 *      <canvas> coords <item> \
 *         "refpointer" memory_address_of_ARRAYinfo memory_address_of_ARRAYinfo
 *
 *   To remove the reference spectrum pass in a memory address of 0.
 *
 *   Special features (should be moved into some generic interface, but
 *   that's not possible for canvas items, so some method of exporting the
 *   plot would be required) are convert an X coordinate into a canvas
 *   coordinate and vice-versa, using the plot. This uses the format:
 *
 *      <canvas> coords <item> "convert" ?toworld? coordinate.
 *
 *   Where tocanvas is a boolean, and to get the data limits of the plot
 *   in world coordinates.
 *
 *      <canvas> coords <item> "limits"
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
    ARRAYinfo *dataArrayInfo = NULL;
    ARRAYinfo *varArrayInfo = NULL;
    SPItem *spPtr = (SPItem *) itemPtr;
    char *optionPtr;
    double *dataPtr;
    int i;
    int ispointer = 0;
    int isref = 0;
    int nel;
    long adr;

#if DEBUG
    fprintf( stderr, "SPCoords() \n" );
#endif

    optionPtr = Tcl_GetString( objv[0] );
    if ( objc == 0 ) {
        /*
	 * Print the coordinate and data values as pairs. BAD values are
         * just ignored.
	 */
        Tcl_Obj *subobj, *obj = Tcl_NewObj();
        for ( i = 0; i < spPtr->numPoints; i++ ) {
            if ( spPtr->dataPtr[i] != spPtr->badvalue ) {
                subobj = Tcl_NewDoubleObj( spPtr->coordPtr[i] );
                Tcl_ListObjAppendElement( interp, obj, subobj );
                subobj = Tcl_NewDoubleObj( spPtr->dataPtr[i] );
                Tcl_ListObjAppendElement( interp, obj, subobj );
            }
        }
        Tcl_SetObjResult( interp, obj );
        return TCL_OK;
    }
    else if ( strcmp( optionPtr, "convert" ) == 0 ) {
        /*  Look for a request to convert a coordinate or return the limits */
        if ( spPtr->plot != NULL ) {
            double xin[1];
            double yin[1];
            double xout[1];
            double yout[1];
            int toworld;
            if ( Tcl_GetBooleanFromObj( interp, objv[1], &toworld ) != TCL_OK ||
                 Tcl_GetDoubleFromObj( interp, objv[2], &xin[0] ) != TCL_OK ) {
                return TCL_ERROR;
            }
            yin[0] = 0.0;
            astTran2( spPtr->plot, 1, xin, yin, toworld, xout, yout );
            if ( !astOK || xout[0] == AST__BAD ) {
                Tcl_SetObjResult( interp, Tcl_NewDoubleObj( 0.0 ) );
                astClearStatus;
            }
            else {
                Tcl_SetObjResult( interp, Tcl_NewDoubleObj( xout[0] ) );
            }
        }
        else {
            Tcl_SetObjResult( interp, objv[1] );
        }
        return TCL_OK;
    }
    else if ( strcmp( optionPtr, "limits" ) == 0 ) {
        Tcl_Obj *subobj;
        Tcl_Obj *obj = Tcl_NewObj();

        subobj = Tcl_NewDoubleObj( spPtr->xleft );
        Tcl_ListObjAppendElement( interp, obj, subobj );
        subobj = Tcl_NewDoubleObj( spPtr->ybot );
        Tcl_ListObjAppendElement( interp, obj, subobj );
        subobj = Tcl_NewDoubleObj( spPtr->xright );
        Tcl_ListObjAppendElement( interp, obj, subobj );
        subobj = Tcl_NewDoubleObj( spPtr->ytop );
        Tcl_ListObjAppendElement( interp, obj, subobj );

        Tcl_SetObjResult( interp, obj );
        return TCL_OK;
    }

    /* Could be a list of doubles or a memory address. */
    nel = objc;
    adr = 0L;
    isref = 0;
    if ( strcmp( optionPtr, "refpointer" ) == 0 ) {
        isref = 1;
    }

    if ( strcmp( optionPtr, "pointer" ) == 0 || isref ) {

        /*  Memory address, sort this out. */
        if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
            Tcl_AppendResult( interp, "Failed to read ARRAYinfo pointer",
                              (char *) NULL );
            return TCL_ERROR;
        }

        /* If the address is 0 and this is a reference spectrum, that's a
           request to clear it, just do that and return. */
        if ( isref && adr == 0L ) {
            if ( spPtr->refDataPtr != NULL ) {
                ckfree( (char *) spPtr->refDataPtr );
                spPtr->refDataPtr = NULL;
                if ( spPtr->refVarPtr != NULL ) {
                    ckfree( (char *) spPtr->refVarPtr );
                    spPtr->refVarPtr = NULL;
                }
            }
            return TCL_OK;
        }

        if ( adr == 0L ) {
            Tcl_AppendResult( interp, "Given zero valued ARRAYinfo pointer",
                              (char *) NULL );
            return TCL_ERROR;
        }

        ispointer = 1;
        dataArrayInfo = (ARRAYinfo *) adr;
        nel = dataArrayInfo->el;

        /* Check for the second address, that is for any variances. */
        if ( Tcl_GetLongFromObj( interp, objv[2], &adr ) != TCL_OK ) {
            Tcl_AppendResult( interp,
                              "Failed to read variance ARRAYinfo pointer",
                              (char *) NULL );
            return TCL_ERROR;
        }

        /* If 0 we have none. Note must be the same length. */
        if ( adr == 0 ) {
            varArrayInfo = NULL;

            /* No variance, release any existing. */
            if ( spPtr->varPtr != NULL ) {
                ckfree( (char *) spPtr->varPtr );
                spPtr->varPtr = NULL;
            }
        }
        else {
            varArrayInfo = (ARRAYinfo *) adr;
            if ( varArrayInfo->el != nel ) {
                Tcl_AppendResult( interp, "Variance and data array must have"
                                  "the same number of elements",
                                  (char *) NULL );
                return TCL_ERROR;
            }
        }
    }

    /* Reference spectra must have the same number of points and we must
     * have a data spectrum already */
    if ( isref ) {
        if ( nel != spPtr->numPoints ) {
            Tcl_SetResult( interp, "Reference spectra are required to"
                           " have the same number of points as main"
                           " spectrum", TCL_VOLATILE );
            return TCL_ERROR;
        }
        if ( spPtr->dataPtr == NULL ) {
            Tcl_SetResult( interp, "A reference spectrum can only be added"
                           " after the main spectrum", TCL_VOLATILE );
            return TCL_ERROR;
        }
        if ( spPtr->refDataPtr == NULL ) {
            spPtr->refDataPtr =
                (double *) ckalloc( sizeof( double ) * nel );
        }
        if ( varArrayInfo != NULL && spPtr->refVarPtr == NULL ) {
            spPtr->refVarPtr =
                (double *) ckalloc( sizeof( double ) * nel );
        }
    }
    else if ( spPtr->dataPtr != NULL && nel > spPtr->numPoints ) {
        /* Re-create various workspaces, if needed */
        ckfree( (char *) spPtr->dataPtr );
        spPtr->dataPtr = NULL;
        if ( spPtr->varPtr != NULL ) {
            ckfree( (char *) spPtr->varPtr );
            spPtr->varPtr = NULL;
        }
        ckfree( (char *) spPtr->coordPtr );
        if ( spPtr->refDataPtr != NULL ) {
            ckfree( (char *) spPtr->refDataPtr );
            spPtr->refDataPtr = NULL;
            if ( spPtr->refVarPtr != NULL ) {
                ckfree( (char *) spPtr->refVarPtr );
                spPtr->refVarPtr = NULL;
            }
        }
        ckfree( (char *) spPtr->tmpPtr[0] );
        ckfree( (char *) spPtr->tmpPtr[1] );
    }
    else if ( spPtr->dataPtr != NULL && nel != spPtr->numPoints ) {
        /* When the number of elements changes, the reference spectrum
         * always becomes invalid . */
        if ( spPtr->refDataPtr != NULL ) {
            ckfree( (char *) spPtr->refDataPtr );
            spPtr->refDataPtr = NULL;
            if ( spPtr->refVarPtr != NULL ) {
                ckfree( (char *) spPtr->refVarPtr );
                spPtr->refVarPtr = NULL;
            }
        }
    }

    if ( spPtr->dataPtr == NULL ) {
        i = sizeof( double ) * nel;
        spPtr->dataPtr = (double *) ckalloc( i );
        spPtr->coordPtr = (double *) ckalloc( i );
        spPtr->tmpPtr[0] = (double *) ckalloc( i );
        spPtr->tmpPtr[1] = (double *) ckalloc( i );
    }
    if ( varArrayInfo != NULL && spPtr->varPtr == NULL ) {
        spPtr->varPtr = (double *) ckalloc( sizeof( double ) * nel );
    }
    spPtr->numPoints = nel;

    /* Read in data and work out limits */
    spPtr->ymax = -DBL_MAX;
    spPtr->ymin = DBL_MAX;

    if ( ! ispointer ) {
        /* Read doubles from each word, cannot be reference spectrum */
        for ( i = 0; i < nel; i++ ) {
            if ( Tcl_GetDoubleFromObj( interp, objv[i],
                                       &spPtr->dataPtr[i] ) != TCL_OK ) {
                return TCL_ERROR;
            }

            /* Ignore negative values, if requested. */
            if ( spPtr->ypositive ) {
                if ( spPtr->dataPtr[i] != spPtr->badvalue &&
                     spPtr->dataPtr[i] > 0.0 ) {
                    spPtr->ymin = MIN( spPtr->ymin, spPtr->dataPtr[i] );
                    spPtr->ymax = MAX( spPtr->ymax, spPtr->dataPtr[i] );
                }
            }
            else {
                if ( spPtr->dataPtr[i] != spPtr->badvalue ) {
                    spPtr->ymin = MIN( spPtr->ymin, spPtr->dataPtr[i] );
                    spPtr->ymax = MAX( spPtr->ymax, spPtr->dataPtr[i] );
                }
            }
        }
    }
    else {
        /* Given ARRAYinfo addresses, convert type into double precision and
         * check for BAD values which are replaced with our bad value,
         * -DBL_MAX. */
        if ( isref ) {
            gaiaArrayToDouble( dataArrayInfo, spPtr->badvalue,
                               spPtr->refDataPtr );
            if ( varArrayInfo != NULL ) {
                gaiaArrayToDouble( varArrayInfo, spPtr->badvalue,
                                   spPtr->refVarPtr );
            }
        }
        else {
            gaiaArrayToDouble( dataArrayInfo, spPtr->badvalue,
                               spPtr->dataPtr );

            /* Get range of data */
            dataPtr = spPtr->dataPtr;
            for ( i = 0; i < nel; i++ ) {

                /* Ignore negative values, if requested. */
                if ( spPtr->ypositive ) {
                    if ( *dataPtr != spPtr->badvalue &&
                         *dataPtr > 0.0 ) {
                        spPtr->ymin = MIN( spPtr->ymin, *dataPtr );
                        spPtr->ymax = MAX( spPtr->ymax, *dataPtr );
                    }
                }
                else {
                    if ( *dataPtr != spPtr->badvalue ) {
                        spPtr->ymin = MIN( spPtr->ymin, *dataPtr );
                        spPtr->ymax = MAX( spPtr->ymax, *dataPtr );
                    }
                }
                dataPtr++;
            }

            if ( varArrayInfo != NULL ) {
                gaiaArrayToDouble( varArrayInfo, spPtr->badvalue,
                                   spPtr->varPtr );
            }
        }
    }

    /* If autoscaling or there's no range use the min/max. */
    if ( ( ! spPtr->fixdatarange ) ||
         spPtr->ybot == DBL_MAX || spPtr->ytop == -DBL_MAX ) {
        spPtr->ytop = spPtr->ymax;
        spPtr->ybot = spPtr->ymin;
    }

    /* Check if data has no range, in that case just add +/- 1, so we can
     * plot something. During interactive updates that's better than
     * throwing an error. */
    if ( ! isref ) {
        if ( spPtr->ybot == spPtr->ytop ) {
            /* No range */
            spPtr->ybot -= 1.0;
            spPtr->ytop += 1.0;
        }
        else if ( spPtr->ybot == DBL_MAX ) {
            /* All bad */
            spPtr->ybot = -1.0;
            spPtr->ytop =  1.0;
        }
    }
    return TCL_OK;
}

/**
 * SPConfigure --
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
static int SPConfigure( Tcl_Interp *interp, Tk_Canvas canvas,
                        Tk_Item *itemPtr, int objc,
                        Tcl_Obj *CONST objv[], int flags )
{
    SPItem *spPtr = (SPItem *) itemPtr;
    Tk_Window tkwin;

#if DEBUG
    fprintf( stderr, "SPConfigure() \n" );
#endif
    tkwin = Tk_CanvasTkwin( canvas );
    /* Note using Tcl_Obj calls so need TK_CONFIG_OBJS flags. */
    if ( Tk_ConfigureWidget( interp, tkwin, configSpecs, objc,
                             (char **) objv, (char *) spPtr,
                             flags|TK_CONFIG_OBJS )
         != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Check if any options that require the plot to be regenerated
     * were set. */
    if ( objc > 1 ) {
        /*  Any new flags mean regenerate. */
        spPtr->newplot = 1;
    }
    ComputeBBox( canvas, spPtr );
    return TCL_OK;
}

/**
 * SPDelete --
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
static void SPDelete( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display )
{
    SPItem *spPtr = (SPItem *) itemPtr;
#if DEBUG
    fprintf( stderr, "SPDelete() \n" );
#endif
    if ( spPtr->dataPtr != NULL ) {
        ckfree( (char *) spPtr->dataPtr );
    }
    if ( spPtr->coordPtr != NULL ) {
        ckfree( (char *) spPtr->coordPtr );
    }
    if ( spPtr->refDataPtr != NULL ) {
        ckfree( (char *) spPtr->refDataPtr );
    }
    if ( spPtr->tmpPtr[0] != NULL ) {
        ckfree( (char *) spPtr->tmpPtr[0] );
    }
    if ( spPtr->tmpPtr[1] != NULL ) {
        ckfree( (char *) spPtr->tmpPtr[1] );
    }
    spPtr->numPoints = 0;

    if ( spPtr->framesets[0] != NULL ) {
        (void) astAnnul( spPtr->framesets[0] );
    }

    if ( spPtr->mapping != NULL ) {
        (void) astAnnul( spPtr->mapping );
    }

    if ( spPtr->polyline != NULL ) {
        RtdLineDelete( canvas, spPtr->polyline, display );
    }

    if ( spPtr->refpolyline != NULL ) {
        RtdLineDelete( canvas, spPtr->refpolyline, display );
    }

    if ( spPtr->mainsegment != NULL ) {
        RtdSegmentDelete( canvas, spPtr->mainsegment, display );
    }

    if ( spPtr->refsegment != NULL ) {
        RtdSegmentDelete( canvas, spPtr->refsegment, display );
    }

    /* Delete any grid items */
    ClearSubItems( canvas, spPtr );
    if ( spPtr->tagPtr != NULL ) {
        free( spPtr->tagPtr );
    }
}

/**
 * SPDisplay --
 *
 *      This procedure is invoked to draw an item in a given drawable.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      ItemPtr is drawn in drawable, or not if an error occurs.
 */
static void SPDisplay( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display,
                       Drawable drawable, int x, int y, int width,
                       int height )
{
    SPItem *spPtr = (SPItem *) itemPtr;
    Tk_Item *polyline;
    Tk_Item *segment;
    Tk_Window tkwin;
    XColor *linecolour;
    double *dataPtr;
    double *varPtr;
    double basebox[4];
    double half = 0.0;
    double xin[2];
    double xout[2];
    double yin[2];
    double yout[2];
    float graphbox[4];
    int base;
    int current;
    int i;
    int j;
    int xborder;
    int yborder;

#if DEBUG
    fprintf( stderr, "SPDisplay() \n" );
#endif

    if ( spPtr->dataPtr == NULL ) {
        return;
    }
    if ( spPtr->framesets[0] == NULL ) {
        return;
    }

    tkwin = Tk_CanvasTkwin( canvas );

    /* Generate the Plot frameset, if required */
    if ( spPtr->newplot || spPtr->framesets[1] == NULL ) {
        spPtr->newplot = 0;
#if DEBUG
        fprintf( stderr, "Regenerating plot\n" );
#endif

        if ( spPtr->framesets[1] != NULL ) {
            (void) astAnnul( spPtr->framesets[1] );
        }
        GeneratePlotFrameSet( spPtr );

        /* Graphics limits are bounding box, note these are corrected for the
         * anchor position and we reserve a little for making sure the
         * graphics around the axes are cleared (otherwise AST draws these
         * outside the limits and refresh is unpredicable). */
        xborder = (int) ( spPtr->width * spPtr->xborder );
        yborder = (int) ( spPtr->height * spPtr->yborder );

        graphbox[0] = spPtr->header.x1 + xborder;
        graphbox[1] = spPtr->header.y2 - yborder;
        graphbox[2] = spPtr->header.x2 - xborder;
        if ( spPtr->isDSB ) {
            graphbox[3] = spPtr->header.y1 + yborder; /* Room for upper axis */
        }
        else {
            graphbox[3] = spPtr->header.y1;           /* No room for title */
        }

        /* Physical limits */
        basebox[0] = spPtr->xleft;
        basebox[2] = spPtr->xright;
        basebox[1] = spPtr->ybot;
        basebox[3] = spPtr->ytop;

#if DEBUG
        fprintf( stderr, "basebox: %f,%f,%f,%f\n", basebox[0], basebox[1],
                 basebox[2], basebox[3] );
        fprintf( stderr, "graphbox: %f,%f,%f,%f\n", graphbox[0], graphbox[1],
                 graphbox[2], graphbox[3] );
#endif

        /* Create the Plot. So that we get linear axis 1 coordinates we must
         * choose the current frame as the base frame (so that the mapping
         * from graphics to physical is linear)
         */
        base = astGetI( spPtr->framesets[1], "Base" );
        current = astGetI( spPtr->framesets[1], "Current" );
        astSetI( spPtr->framesets[1], "Base", current );
        spPtr->plot = astPlot( spPtr->framesets[1], graphbox, basebox,
                               (spPtr->options == NULL ? " " : spPtr->options));
        astSetI( spPtr->framesets[1], "Base", base );

        if ( spPtr->plot == NULL ) {
            if ( !astOK ) {
                astClearStatus;
            }
            return;
        }

        /* And plot the grid axes, this is also only required when the
         * framesets change. */
        if ( spPtr->showaxes ) {

            astTk_SetCanvas( Tk_PathName( tkwin ) );
            astTk_Tag( spPtr->tagPtr );      /* Includes unique tag */
            ClearSubItems( canvas, spPtr );  /* Remove last grid */

            /* Axes should always be shown on the exterior */
            astSet( spPtr->plot, "Labelling=exterior,ForceExterior=1" );

            /* If the spectral axis is a DSBSpecFrame then we need to arrange
             * to show the lower and upper sidebands, this involves drawing
             * the grid twice, with different wavelength coordinates on the
             * bottom and top of grid. */
            if ( spPtr->isDSB ) {
                const char *sideband = astGetC( spPtr->framesets[0],
                                                "SideBand" );

                /* Stop plot showing Y axes for the first grid and draw X
                 * coordinates on bottom */
                astSet( spPtr->plot, "DrawAxes(2)=0" );
                astSet( spPtr->plot, "TextLab(2)=0" );
                astSet( spPtr->plot, "NumLab(2)=0" );
                astSet( spPtr->plot, "Edge(1)=bottom" );

                /* TickAll never works in this mode, so just switch it off */
                astSet( spPtr->plot, "TickAll=0" );

                /* We never have room for a title, ever so it goes too */
                astSet( spPtr->plot, "DrawTitle=0" );

                astGrid( spPtr->plot );          /* Draw first grid */

                /* Show Y axes this time and draw X coordinates on top */
                astSet( spPtr->plot, "DrawAxes(2)=1" );
                astSet( spPtr->plot, "TextLab(2)=1" );
                astSet( spPtr->plot, "NumLab(2)=1" );
                astSet( spPtr->plot, "Edge(1)=top" );

                /* Switch sideband */
                if ( strcmp( "USB", sideband ) == 0 ) {
                    astSetC( spPtr->plot, "SideBand", "LSB" );
                }
                else {
                    astSetC( spPtr->plot, "SideBand", "USB" );
                }
                astGrid( spPtr->plot );          /* Second grid */

                /* Restore */
                astSetC( spPtr->plot, "SideBand", sideband );
            }
            else {
                astGrid( spPtr->plot );          /* Draw grid */
            }

            astTk_Tag( NULL );               /* Stop tagging */
        }
        if ( ! astOK ) astClearStatus;
    }

    /* Always plot the spectra, for speed we use direct control of two
     * rtd_polyline and two rtd_segment instances.  First convert coordinates
     * into canvas from current. Use two loops in case have reference
     * spectrum, note reference spectrum is plotted first. */
    if ( spPtr->plot != NULL ) {

        for ( i = 0; i < 2; i++ ) {
            if ( i == 0 ) {
                if ( spPtr->refDataPtr == NULL ) {
                    /* No reference spectrum, skip to next loop */
                    continue;
                }
                dataPtr = spPtr->refDataPtr;
                varPtr = spPtr->refVarPtr;
                polyline = spPtr->refpolyline;
                linecolour = spPtr->reflinecolour;
                segment = spPtr->refsegment;
            }
            else {
                dataPtr = spPtr->dataPtr;
                varPtr = spPtr->varPtr;
                polyline = spPtr->polyline;
                linecolour = spPtr->linecolour;
                segment = spPtr->mainsegment;
            }

            astTran2( spPtr->plot, spPtr->numPoints, spPtr->coordPtr,
                      dataPtr, 0, spPtr->tmpPtr[0], spPtr->tmpPtr[1] );

            /* Set line coordinates */
            RtdLineQuickSetCoords( spPtr->interp, canvas, polyline,
                                   spPtr->tmpPtr[0], spPtr->tmpPtr[1],
                                   spPtr->numPoints );

            RtdLineSetColour( tkwin, display, polyline, linecolour );
            RtdLineSetWidth( display, polyline, spPtr->linewidth );

            /* Do the draw. */
            RtdLineDisplay( canvas, polyline, display, drawable, x, y,
                            width, height );

            /* Now for error bars, if needed. */
            if ( varPtr != NULL && spPtr->showerrorbars ) {

                /* Colour and width of errorbars */
                RtdSegmentSetColour( tkwin, display, segment,
                                     spPtr->errorcolour );
                RtdSegmentSetWidth( display, segment, spPtr->linewidth );

                for ( j = 0; j < spPtr->numPoints; j += spPtr->frequency ) {
                    if ( varPtr[j] != spPtr->badvalue ) {

                        half = sqrt( fabs( varPtr[j] ) ) * spPtr->nsigma;
                        xin[0] = spPtr->coordPtr[j];
                        yin[0] = dataPtr[j] - half;
                        xin[1] = spPtr->coordPtr[j];
                        yin[1] = dataPtr[j] + half;
                        astTran2( spPtr->plot, 2, xin, yin, 0, xout, yout );

                        RtdSegmentQuickSetCoords( spPtr->interp, canvas,
                                                  segment, ( j != 0 ),
                                                  xout, yout, 2 );
                        RtdSegmentDisplay( canvas, segment, display,
                                           drawable, x, y, width, height );

                        /* Add the serifs. */
                        xin[0] = xout[0] - SERIF_LENGTH;
                        xin[1] = xout[0] + SERIF_LENGTH;
                        yin[0] = yout[0];
                        yin[1] = yout[0];
                        RtdSegmentQuickSetCoords( spPtr->interp, canvas,
                                                  segment, 1, xin, yin, 2);
                        RtdSegmentDisplay( canvas, segment, display,
                                           drawable, x, y, width, height );

                        xin[0] = xout[1] - SERIF_LENGTH;
                        xin[1] = xout[1] + SERIF_LENGTH;
                        yin[0] = yout[1];
                        yin[1] = yout[1];
                        RtdSegmentQuickSetCoords( spPtr->interp, canvas,
                                                  segment, 1, xin, yin, 2);
                        RtdSegmentDisplay( canvas, segment, display,
                                           drawable, x, y, width, height );
                    }
                }
            }
        }
    }

    /* Reset AST if any errors occurred, don't want next draw to fail
     * as well. */
    if ( !astOK ) {
        astClearStatus;
    }
}

/**
 * SPToPoint --
 *
 *      Computes the distance from a given point to the plot.
 *
 * Results:
 *      The return value is 0 if the point whose x and y coordinates
 *      are pointPtr[0] and pointPtr[1] is inside the plot.  If the
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
#if DEBUG
    fprintf( stderr, "SPToPoint() \n" );
#endif
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
#if DEBUG
    fprintf( stderr, "SPToArea() \n" );
#endif
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
 * SPScale --
 *
 *      This procedure is invoked to scale an item. If the arguments are
 *      set to -1, then factors that fill the canvas will be used. Scaling
 *      can also be inhibited by setting the fixedscale option to 1.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The item referred to by itemPtr is rescaled
 *              x' = originX + scaleX*(x-originX)
 *              y' = originY + scaleY*(y-originY)
 *      This just moves the origin wrt to the scale.
 */
static void SPScale( Tk_Canvas canvas, Tk_Item *itemPtr, double originX,
                     double originY, double scaleX, double scaleY )
{
    SPItem *spPtr = (SPItem *) itemPtr;
#if DEBUG
    fprintf( stderr, "SPScale(%f,%f,%f,%f) \n",
             originX, originY, scaleX, scaleY );
#endif

    /* Do not scale, if we want to be fixed in size */
    if ( spPtr->fixedscale ) {
        return;
    }

    if ( scaleX < 0.0 ) {
        /* Autoscale, fit whole of canvas */
        Tk_Window tkwin;
        tkwin = Tk_CanvasTkwin( canvas );
        spPtr->x = Tk_X( tkwin );
        spPtr->y = Tk_Y( tkwin );
        spPtr->width = Tk_Width( tkwin );
        spPtr->height = Tk_Height( tkwin );
    }
    else {
        /* Scale all coordinates and set their related values */
        spPtr->x = originX + scaleX * ( spPtr->x - originX );
        spPtr->y = originY + scaleY * ( spPtr->y - originY );
        spPtr->width *= scaleX;
        spPtr->height *= scaleY;
    }

    /* Need to recompute plot to fit this change */
    spPtr->newplot = 1;

    ComputeBBox( canvas, spPtr );

    return;
}

/**
 * SPTranslate --
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
static void SPTranslate( Tk_Canvas canvas, Tk_Item *itemPtr,
                         double deltaX,  double deltaY )
{
    SPItem *spPtr = (SPItem *) itemPtr;
    spPtr->x += deltaX;
    spPtr->y += deltaY;
    ComputeBBox( canvas, spPtr );
}

/**
 * SPToPostscript --
 *
 *      This procedure is called to generate Postscript for the item.
 *
 * Results:
 *      Most of the drawing will be done by the AST components which the
 *      canvas should manage. We need to get the polylines to draw themselves.
 *
 * Side effects:
 *      None.
 */
static int SPToPostscript( Tcl_Interp *interp, Tk_Canvas canvas,
                           Tk_Item *itemPtr, int prepass )
{
    int result;
    SPItem *spPtr = (SPItem *) itemPtr;

    result = RtdLineToPostscript( interp, canvas, spPtr->polyline, prepass );
    if ( result == TCL_OK && spPtr->refDataPtr != NULL ) {
        result = RtdLineToPostscript( interp, canvas, spPtr->refpolyline,
                                      prepass );
    }
    if ( result == TCL_OK && spPtr->varPtr != NULL ) {
        result = RtdSegmentToPostscript( interp, canvas, spPtr->mainsegment,
                                         prepass );
    }
    if ( result == TCL_OK && spPtr->refVarPtr != NULL ) {
        result = RtdSegmentToPostscript( interp, canvas, spPtr->refsegment,
                                         prepass );
    }
    return result;
}

/**
 * ComputeBBox --
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
static void ComputeBBox( Tk_Canvas canvas, SPItem *spPtr )
{
    Tk_Window tkwin;
    double x, y;

    tkwin = Tk_CanvasTkwin( canvas );
#if DEBUG
    fprintf( stderr, "ComputeBBox() \n" );
#endif
    x = spPtr->x;
    y = spPtr->y;
    CorrectAnchor( spPtr, &x, &y );

    spPtr->header.x1 = spPtr->header.x2 = x;
    spPtr->header.y1 = spPtr->header.y2 = y;
    spPtr->header.x2 += spPtr->width;
    spPtr->header.y2 += spPtr->height;

#if DEBUG
    fprintf( stderr, "\t %d,%d,%d,%d \n", spPtr->header.x1, spPtr->header.y1,
                                          spPtr->header.x2, spPtr->header.y2 );
#endif
}


/*
 *--------------------------------------------------------------
 *
 * CorrectAnchor--
 *
 *      This procedure is called to shift the x,y position to
 *      correct for any requested anchors. Note this doesn't
 *      correct for any scale factors.
 *
 *--------------------------------------------------------------
 */
static void CorrectAnchor( SPItem *spPtr, double *x, double *y )
{
#if DEBUG
    fprintf( stderr, "CorrectAnchor: %d, %d \n", *x, *y );
#endif
    switch ( spPtr->tkanchor ) {
        case TK_ANCHOR_NW:
            /* Default for all anchors */
        break;

        case TK_ANCHOR_N:
            *x -= (double) spPtr->width * 0.5;
        break;

        case TK_ANCHOR_NE:
            *x -= (double) spPtr->width;
        break;

        case TK_ANCHOR_W:
            *y -= (double) spPtr->height * 0.5;
        break;

        case TK_ANCHOR_E:
            *x -= (double) spPtr->width;
            *y -= (double) spPtr->height * 0.5;
        break;

        case TK_ANCHOR_SW:
            *y -= (double) spPtr->height;
        break;

        case TK_ANCHOR_S:
            *x -= (double) spPtr->width * 0.5;
            *y -= (double) spPtr->height;
        break;

        case TK_ANCHOR_SE:
            *x -= (double) spPtr->width;
            *y -= (double) spPtr->height;
        break;

        case TK_ANCHOR_CENTER:
            *x -= (double) spPtr->width * 0.5;
            *y -= (double) spPtr->height * 0.5;
        break;
    }
#if DEBUG
    fprintf( stderr, "             : %d, %d \n", *x, *y );
#endif
}

/**
 * CanvasParseProc --
 *
 *      Provide parsing for -tags option, want to keep string copy so don't
 *      use standard Tk_CanvasParseProc directly.
 */
static int CanvasTagsParseProc( ClientData clientData, Tcl_Interp *interp,
                                Tk_Window tkwin, CONST char *value,
                                char *widgRec, int offset )
{
    SPItem *spPtr = (SPItem *) widgRec;
    if ( spPtr->tagPtr != NULL ) {
        free( spPtr->tagPtr );
    }
    spPtr->tagPtr = (char *) malloc( strlen(value) + strlen(spPtr->utag) + 2 );
    strcpy( spPtr->tagPtr, spPtr->utag );
    strcat( spPtr->tagPtr, " " );
    strcat( spPtr->tagPtr, value );
    Tk_CanvasTagsParseProc(clientData, interp, tkwin, value, widgRec, offset);
    return TCL_OK;
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
    AstFrame *picked;
    SPItem *spPtr = (SPItem *) widgRec;
    const char *sideband;
    int iaxes[1];
    long longResult;

    if ( Tcl_ExprLong( interp, value, &longResult ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Release old frameset, before accepting new */
    if ( spPtr->framesets[0] != NULL ) {
        (void) astAnnul( spPtr->framesets[0] );
        if ( spPtr->mapping != NULL ) {
            (void) astAnnul( spPtr->mapping );
        }
    }

    /* Make a clone of the frameset so that the original can be released */
    spPtr->framesets[0] = (AstFrameSet *) astClone((AstFrameSet *)longResult);
    spPtr->framesets[1] = (AstFrameSet *) NULL;
    spPtr->mapping = (AstMapping *) NULL;

    /* If this has a DSBSpecFrame the plotting is different, so find out.*/
    iaxes[0] = spPtr->axis;
    picked = astPickAxes( spPtr->framesets[0], 1, iaxes, NULL );
    spPtr->isDSB = astIsADSBSpecFrame( picked );
    if ( spPtr->isDSB ) {

        /* If the sideband is set to "LO", then in fact we still treat this as
         * a normal spectrum, check that. If this becomes switchable then this
         * test will need to be applied everytime isDSB is accessed. */
        sideband = astGetC( spPtr->framesets[0], "SideBand" );
        if ( strcmp( "LO", sideband ) == 0 ) {
            spPtr->isDSB = 0;
        }
    }
    (void) astAnnul( picked );

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
    SPItem *spPtr = (SPItem *) widgRec;
    char *p = (char *) ckalloc( LONG_LENGTH );

    sprintf( p, "%lu", (long) spPtr->framesets[0] );

    *freeProcPtr = TCL_DYNAMIC;
    return p;
}

/**
 * MappingParseProc --
 *
 *      Provide parsing for -mapping option, in fact this isn't
 *      really allowed as the mapping is a generated value. Allow
 *      only for debugging purposes.
 */
static int MappingParseProc( ClientData clientData, Tcl_Interp *interp,
                             Tk_Window tkwin, CONST char *value,
                             char *widgRec, int offset )
{
    SPItem *spPtr = (SPItem *) widgRec;
    long longResult;

    if ( Tcl_ExprLong( interp, value, &longResult ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Release old mapping, before accepting new */
    if ( spPtr->mapping != NULL ) {
        (void) astAnnul( spPtr->mapping );
    }

    spPtr->mapping = (AstMapping *) astClone((AstMapping *)longResult);

    return TCL_OK;
}


/**
 * MappingPrintProc --
 *
 *      Return -mapping option as string.
 */
static char *MappingPrintProc( ClientData clientData, Tk_Window tkwin,
                               char *widgRec, int offset,
                               Tcl_FreeProc **freeProcPtr )
{
    SPItem *spPtr = (SPItem *) widgRec;
    char *p = (char *) ckalloc( LONG_LENGTH );

    sprintf( p, "%lu", (long) spPtr->mapping );

    *freeProcPtr = TCL_DYNAMIC;
    return p;
}

/**
 * ClearSubItems --
 *
 *     Clear any items we've already drawn (before refresh or when we're being
 *     deleted).
 */
static void ClearSubItems( Tk_Canvas canvas, SPItem *spPtr )
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
 * GeneratePlotFrameSet --
 *
 *     Generate a FrameSet suitable for drawing a 2D data plot using our data
 *     values and 1D frameset. Also creates the coordinate positions array.
 */
static void GeneratePlotFrameSet( SPItem *spPtr )
{
    int i;
    double *tmpPtr;

    /* Create the spectral FrameSet */
    MakeSpectral( spPtr );

    /* Get the centres of the pixel positions in current coordinates (so we
     * can eventually go from current coordinates through to graphics
     * coordinates when actually drawing the spectrum).
     */
    tmpPtr = spPtr->tmpPtr[0];
    for ( i = 0; i < spPtr->numPoints; i++ ) {
        tmpPtr[i] = (double) ( i + 1 + spPtr->offset );
    }
    astTran1( spPtr->mapping, spPtr->numPoints, tmpPtr, 1,
              spPtr->coordPtr );

    /* Set the axis range, pick first non-BAD values and possibly only
     * positive values from ends (spectrum must be monotonic, so this is OK
     * and keeps the natural order which can be actually be min to max or max
     * to min). Then apply the xminmax preference.
     */
    spPtr->xleft = DBL_MAX;
    spPtr->xright = -DBL_MAX;

    if ( spPtr->xpositive ) {
        for ( i = 0; i < spPtr->numPoints; i++ ) {
            if ( spPtr->coordPtr[i] != spPtr->badvalue &&
                 spPtr->coordPtr[i] > 0.0 ) {
                spPtr->xleft = spPtr->coordPtr[i];
                break;
            }
        }
        for ( i = spPtr->numPoints - 1; i >= 0; i-- ) {
            if ( spPtr->coordPtr[i] != spPtr->badvalue &&
                 spPtr->coordPtr[i] > 0.0 ) {
                spPtr->xright = spPtr->coordPtr[i];
                break;
            }
        }
    }
    else {
        for ( i = 0; i < spPtr->numPoints; i++ ) {
            if ( spPtr->coordPtr[i] != spPtr->badvalue ) {
                spPtr->xleft = spPtr->coordPtr[i];
                break;
            }
        }
        for ( i = spPtr->numPoints - 1; i >= 0; i-- ) {
            if ( spPtr->coordPtr[i] != spPtr->badvalue ) {
                spPtr->xright = spPtr->coordPtr[i];
                break;
            }
        }
    }

    if ( spPtr->xminmax ) {
        double tmp;
        tmp = MIN( spPtr->xleft, spPtr->xright );
        spPtr->xright = MAX( spPtr->xleft, spPtr->xright );
        spPtr->xleft = tmp;
    }

    /* Check if coordinates have no range, in that case just add +/- 1, so we
     * can plot something. During interactive updates that's better than
     * throwing an error. */
    if ( spPtr->xleft == spPtr->xright ) {
        /* No range */
        spPtr->xleft -= 1.0;
        spPtr->xright += 1.0;
    }
    else if ( spPtr->xleft == DBL_MAX ) {
        /* All bad */
        spPtr->xleft = -1.0;
        spPtr->xright =  1.0;
    }
}


/**
 * MakeSpectral --
 *
 *     Generate a FrameSet suitable for drawing a 2D data plot.
 */
static void MakeSpectral( SPItem *spPtr )
{
    AstFrame *cfrm;
    AstFrame *f1;
    AstFrame *f2;
    AstFrame *frm1;
    AstFrame *frm2;
    AstMapping *map;
    AstUnitMap *umap;
    char buffer[20];
    char const *system;
    double *coords;
    double *grid;
    double *lutcoords;
    double work[MAXDIM];
    int havefluxframe;
    int havespecframe;
    int i;
    int iaxes[MAXDIM];
    int j;
    int nin;
    int nout;

    astBegin;

    /* -------------------------------------------------------------
     * Create a mapping to transform from GRID positions to spectral
     * coordinates and data values.
     * -------------------------------------------------------------
     */

    /* Get a simplified mapping from the base to current frames. This will be
     * used to transform from GRID coordinates to spectral coordinates.
     */
    map = astGetMapping( spPtr->framesets[0], AST__BASE, AST__CURRENT );
    spPtr->mapping = astSimplify( map );

    /* Save a pointer to the current frame. */
    cfrm = astGetFrame( spPtr->framesets[0], AST__CURRENT );

    /* Get number of input and output coordinates, these may not be 1. In that
     * case the relevant axis is spPtr->axis, which should be set before
     * defining the frameset. */
    nin = astGetI( spPtr->framesets[0], "Nin" );
    nout = astGetI( spPtr->framesets[0], "Nout" );

    /* When picking the current frame from a nD WCS we need to be careful to
     * handle dependent axes so that they do not introduce an non-invertable
     * mapping. Do this by creating a Lutmap based on the forward
     * transformation, which can then be inverted.
     */
    if ( nout != 1 ) {
        grid = malloc( sizeof(double) * spPtr->numPoints * nin );
        coords = malloc( sizeof(double) * spPtr->numPoints * nout );
        lutcoords = malloc( sizeof(double) * spPtr->numPoints );

        /*  Generate GRID positions along the WCS axis (assumes these are axes
         *  aligned to the GRID axes, otherwise the range of coordinates will
         *  vary for each extraction).
         */
        for ( i = 0; i < spPtr->numPoints; i++ ) {

            /*  Each other axis starts at pixel 1. XXX could use given values
             *  then axes alignment assumption is not as strong (at least then
             *  we have the full range of the WCS axes along this GRID
             *  axis). */
            for ( j = 0; j < nin; j++ ) {
                grid[spPtr->numPoints*j+i] = 1;
            }

            /*  Step along WCS/GRID axis. */
            grid[spPtr->numPoints * (spPtr->axis-1) + i] = i + 1;
        }

        /* Transform these GRID positions into the current frame. */
        astTranN( spPtr->framesets[0], spPtr->numPoints, nin,
                  spPtr->numPoints, grid, 1, nout, spPtr->numPoints, coords );

        /* Normalise the coordinates */
        for ( i = 0; i < spPtr->numPoints; i++ ) {
            for ( j = 0; j < nout; j++ ) {
                work[j] = coords[spPtr->numPoints*j+i];
            }
            astNorm( cfrm, work );

            /*  Store the selected coordinate, repeats WCS/GRID axes alignment
             *  assumption. */
            lutcoords[i] = work[spPtr->axis - 1];
        }
        spPtr->mapping = (AstMapping *) astLutMap( spPtr->numPoints,
                                                   lutcoords, 1.0, 1.0, " " );

        free( grid );
        free( coords );
        free( lutcoords );
    }

    /* The data units map onto themselves directly so use a unitmap. */
    umap = astUnitMap( 1, " " );

    /*  Create a compound of these two mappings. */
    map = (AstMapping *) astCmpMap( spPtr->mapping, umap, 0, " " );

    /* Get a Frame representing the input coordinates, uses a GRID axis of
     * the base frame (any old one) and a default axis. */
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

    /* Use selected axis from the current frame as the spectral axis. */
    iaxes[0] = spPtr->axis;
    f1 = astPickAxes( spPtr->framesets[0], 1, iaxes, NULL );
    havespecframe = astIsASpecFrame( f1 );

    /* If we have data units that can be understood (as some kind of flux in
     * general), then create a FluxFrame using them, but only if we also have
     * a SpecFrame. Note that SpecFrames are not that useful as we don't use a
     * reference spectrum (so no alignments are used in this item).
     */
    f2 = NULL;
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
        f2 = astFrame( 1, " " );
    }

    /* Create the full frame using the spectral frame and flux frame. */
    if ( havespecframe && havefluxframe ) {
        frm2 = (AstFrame *) astSpecFluxFrame( (AstSpecFrame *) f1,
                                              (AstFluxFrame *) f2, " " );
    }
    else {
        frm2 = (AstFrame *) astCmpFrame( f1, f2, " " );
    }

    /* Clear digits and format, unless set in the input frameset. */
    sprintf( buffer, "Format(%d)", spPtr->axis );
    if ( ! astTest( cfrm, buffer ) ) {
        astClear( frm2, "Format(1)" );
    }
    sprintf( buffer, "Digits(%d)", spPtr->axis );
    if ( ! astTest( cfrm, buffer ) ) {
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
    spPtr->framesets[1] = astFrameSet( frm1, " " );
    astAddFrame( spPtr->framesets[1], AST__BASE, map, frm2 );

    /* Export the FrameSet and 1D mapping, and make sure everything else is
     * released. */
    astExport( spPtr->framesets[1] );
    astExport( spPtr->mapping );
    if ( !astOK ) {
        astClearStatus;
    }
    astEnd;
}
