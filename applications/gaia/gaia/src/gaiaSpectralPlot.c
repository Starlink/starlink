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

 *  Notes:
 *     The reference spectrum idea is primitive and should not be
 *     extended. Use a system that shares the AST plot (and uses several
 *     instances of this item) if additional spectra are required.

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
 *     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
 *     02111-1307, USA

 *  Authors:
 *     PWD: P.W. Draper (JAC, Durham University).

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
#include "grf_tkcan.h"
#include "rtdCanvas.h"
#include "gaiaArray.h"

#define MAX(a,b) ( (a) > (b) ? (a) : (b) )
#define MIN(a,b) ( (a) < (b) ? (a) : (b) )

#define DEBUG 0

#define LONG_LENGTH 25

/*
 * Define a structure for containing all the necessary information
 * to describe an item.
 */
typedef struct SPItem  {
    Tk_Item header;             /* Mandatory Tk header information */
    AstFrameSet *framesets[2];  /* The FrameSets associated with item */
    AstPlot *plot;              /* The AstPlot for drawing regions */
    Tcl_Interp *interp;         /* The Tcl interpreter */
    Tk_Anchor tkanchor;         /* Where to anchor relative to (x,y). */
    Tk_Item *polyline;          /* rtdPolyline for main line context */
    Tk_Item *refpolyline;       /* rtdPolyline for reference line context */
    XColor *linecolour;         /* Foreground color for polyline */
    XColor *reflinecolour;      /* Foreground color for reference polyline */
    char *datalabel;            /* The data units label */
    char *dataunits;            /* The data units */
    char *options;              /* AST options used when drawing item */
    char *tagPtr;               /* All tags in use for item */
    char utag[22];              /* Unique tag for AST graphics we create */
    double *coordPtr;           /* Coordinates of main spectrum */
    double *dataPtr;            /* Data values of main spectrum */
    double *refDataPtr;         /* Data values for a reference spectrum */
    double *tmpPtr[2];          /* Temporary arrays for transformed coordinates etc. */
    double badvalue;            /* The value to replace AST__BAD with */
    double height;              /* Height of item */
    double width;               /* Width of item */
    double x, y;                /* Coordinates of item reference point */
    double xborder;             /* Fractional border for X plot axes, only used when autoscaling using scale command */
    double xleft;               /* Right-hand physical X coordinate for plot */
    double xright;              /* Left-hand physical X coordinate for plot */
    double yborder;             /* Fractional border for Y plot axes, only used when autoscaling using scale command */
    double ybot;                /* Physical Y coordinate for plot top */
    double ytop;                /* Physical Y coordinate for plot bottom */
    int fixedscale;             /* If set do not scale item with canvas */
    int isDSB;                  /* Spectral coordinates are dual sideband */
    int linewidth;              /* Width of polyline */
    int newplot;                /* Whether to regenerate plot */
    int numPoints;              /* Number of data values */
    int showaxes;               /* Whether to show the grid */
    int xminmax;                /* If true then use min->max X coordinates */
} SPItem;

static int tagCounter = 0;      /* Counter for creating unique tags */

/* Item canvas tags */
static int CanvasTagsParseProc( ClientData clientData, Tcl_Interp *interp,
                                Tk_Window tkwin, CONST char *value,
                                char *widgRec, int offset );
static Tk_CustomOption tagsOption = {
    CanvasTagsParseProc, Tk_CanvasTagsPrintProc, (ClientData) NULL
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
     "nw", Tk_Offset(SPItem, tkanchor), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_DOUBLE, "-badvalue", (char *) NULL, (char *) NULL,
     "0.0", Tk_Offset(SPItem, badvalue), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_STRING, "-datalabel", (char *) NULL, (char *) NULL,
     "Data value", Tk_Offset(SPItem, datalabel), TK_CONFIG_NULL_OK},

    {TK_CONFIG_STRING, "-dataunits", (char *) NULL, (char *) NULL,
     "", Tk_Offset(SPItem, dataunits), TK_CONFIG_NULL_OK},

    {TK_CONFIG_BOOLEAN, "-fixedscale", (char *) NULL, (char *) NULL,
     "0", Tk_Offset(SPItem, fixedscale), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_CUSTOM, "-frameset", (char *) NULL, (char *) NULL,
     (char *) NULL, Tk_Offset(SPItem, framesets), TK_CONFIG_NULL_OK,
     &framesetOption},

    {TK_CONFIG_STRING, "-gridoptions", (char *) NULL, (char *) NULL,
     "DrawTitle=0", Tk_Offset(SPItem, options), TK_CONFIG_NULL_OK},

    {TK_CONFIG_DOUBLE, "-height", (char *) NULL, (char *) NULL,
     "100.0", Tk_Offset(SPItem, height), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_COLOR, "-linecolour", (char *) NULL, (char *) NULL,
     "blue", Tk_Offset(SPItem, linecolour), TK_CONFIG_NULL_OK},

    {TK_CONFIG_COLOR, "-reflinecolour", (char *) NULL, (char *) NULL,
     "green", Tk_Offset(SPItem, reflinecolour), TK_CONFIG_NULL_OK},

    {TK_CONFIG_PIXELS, "-linewidth", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(SPItem, linewidth), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_BOOLEAN, "-showaxes", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(SPItem, showaxes), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_CUSTOM, "-tags", (char *) NULL, (char *) NULL,
     (char *) NULL, Tk_Offset(SPItem, tagPtr), TK_CONFIG_NULL_OK,
     &tagsOption},

    {TK_CONFIG_DOUBLE, "-width", (char *) NULL, (char *) NULL,
     "100.0", Tk_Offset(SPItem, width), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_DOUBLE, "-x", (char *) NULL, (char *) NULL,
     "0.0", Tk_Offset(SPItem, x), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_DOUBLE, "-xborder", (char *) NULL, (char *) NULL,
     "0.03", Tk_Offset(SPItem, xborder), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_BOOLEAN, "-xminmax", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(SPItem, xminmax), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_DOUBLE, "-y", (char *) NULL, (char *) NULL,
     "0.0", Tk_Offset(SPItem, y), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_DOUBLE, "-yborder", (char *) NULL, (char *) NULL,
     "0.20", Tk_Offset(SPItem, yborder), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
     (char *) NULL, 0, 0}
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
    spPtr->badvalue = -DBL_MAX;
    spPtr->coordPtr = NULL;
    spPtr->dataPtr = NULL;
    spPtr->datalabel = NULL;
    spPtr->dataunits = NULL;
    spPtr->fixedscale = 0;
    spPtr->framesets[0] = NULL;
    spPtr->framesets[1] = NULL;
    spPtr->height = 100.0;
    spPtr->interp = interp;
    spPtr->isDSB = 0;
    spPtr->linecolour = None;
    spPtr->linewidth = 1;
    spPtr->newplot = 1;
    spPtr->numPoints = 0;
    spPtr->options = NULL;
    spPtr->plot = NULL;
    spPtr->polyline = NULL;
    spPtr->refDataPtr = NULL;
    spPtr->reflinecolour = None;
    spPtr->refpolyline = NULL;
    spPtr->tagPtr = NULL;
    spPtr->tkanchor = TK_ANCHOR_NW;
    spPtr->tmpPtr[0] = NULL;
    spPtr->tmpPtr[1] = NULL;
    spPtr->width = 100.0;
    spPtr->x = 0.0;
    spPtr->xborder = 0.07;
    spPtr->xminmax = 1;
    spPtr->y = 0.0;
    spPtr->yborder = 0.17;

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
        AstUnitMap *map;
        astBegin;
        f1 = astFrame( 1, "" );
        f2 = astFrame( 1, "" );
        map = astUnitMap( 1, "" );
        spPtr->framesets[0] = astFrameSet( f1, "" );
        astAddFrame( spPtr->framesets[0], AST__BASE, map, f2 );
        astExport( spPtr->framesets[0] );
        spPtr->isDSB = 0;
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

    /* Initialise the GRF interface. */
    astTk_Init( interp, Tk_PathName( Tk_CanvasTkwin( canvas ) ) );

    return TCL_OK;
}

/**
 * SPCoords --
 *
 *   This procedure is invoked to process the "coords" widget command.
 *
 *   This reads a list of doubles that are the spectral coordinates,
 *   or accepts a pointer to an ARRAYinfo structure of already available
 *   values in one of the HDS types (_BYTE, _UBYTE, _WORD, _UWORD,
 *   _INTEGER, _REAL _DOUBLE). In the latter case you should use the format:
 *
 *      <canvas> coords <item> \
 *         "pointer" memory_address_of_ARRAYinfo
 *
 *   This is also extended to allow another set of data points to be
 *   supplied to support an additional spectrum (with the same coordinates, so
 *   must be from the same data set) to act as a "reference" spectrum. It is
 *   expected that this will be updated less frequently that the main
 *   spectrum and is not used to determine any data limits.
 *
 *      <canvas> coords <item> \
 *         "refpointer" memory_address_of_ARRAYinfo
 *
 *   To remove the reference spectrum pass in a memory address of 0. 
 *
 *   A special feature (should be moved into some generic interface, but
 *   that's not possible for canvas items, so some method of exporting the
 *   plot would be required) is to convert an X coordinate into a canvas
 *   coordinate and vice-versa, using the plot. This uses the format:
 *
 *      <canvas> coords <item> "convert" ?toworld? coordinate.
 *
 *   Where tocanvas is a boolean.
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
    ARRAYinfo *arrayInfo;
    char *optionPtr;
    double *dataPtr;
    int i;
    int nel;
    int isref = 0;
    long adr;

#if DEBUG
    fprintf( stderr, "SPCoords() \n" );
#endif

    if ( objc == 0 ) {
	/*
	 * Print the main data values.
	 */
	Tcl_Obj *subobj, *obj = Tcl_NewObj();
	for ( i = 0; i < spPtr->numPoints; i++ ) {
	    subobj = Tcl_NewDoubleObj( spPtr->dataPtr[i] );
	    Tcl_ListObjAppendElement( interp, obj, subobj );
	}
	Tcl_SetObjResult( interp, obj );
	return TCL_OK;
    }

    /*  Look for a request to convert a coordinate */
    optionPtr = Tcl_GetString( objv[0] );
    if ( strcmp( optionPtr, "convert" ) == 0 ) {
        if ( spPtr->plot != NULL ) {
            double xin[1];
            double yin[1];
            double xout[1];
            double yout[1];
            int toworld;

            if ( Tcl_GetBooleanFromObj( interp, objv[1], &toworld ) != TCL_OK
                 ||
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
    else {

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
                }
                return TCL_OK;
            }
            arrayInfo = (ARRAYinfo *) adr;
            nel = arrayInfo->el;
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
        }
        else if ( spPtr->dataPtr != NULL && nel > spPtr->numPoints ) {
            /* Re-create various workspaces, if needed */
            ckfree( (char *) spPtr->dataPtr );
            ckfree( (char *) spPtr->coordPtr );
            if ( spPtr->refDataPtr != NULL ) {
                ckfree( (char *) spPtr->refDataPtr );
                spPtr->refDataPtr = NULL;
            }
            ckfree( (char *) spPtr->tmpPtr[0] );
            ckfree( (char *) spPtr->tmpPtr[1] );
            spPtr->dataPtr = NULL;
        }
        else if ( spPtr->dataPtr != NULL && nel != spPtr->numPoints ) {
            /* When the number of elements changes, the reference spectrum
             * always becomes invalid . */
            if ( spPtr->refDataPtr != NULL ) {
                ckfree( (char *) spPtr->refDataPtr );
                spPtr->refDataPtr = NULL;
            }
        }

        if ( spPtr->dataPtr == NULL ) {
            i = sizeof( double ) * nel;
            spPtr->dataPtr = (double *) ckalloc( i );
            spPtr->coordPtr = (double *) ckalloc( i );
            spPtr->tmpPtr[0] = (double *) ckalloc( i );
            spPtr->tmpPtr[1] = (double *) ckalloc( i );
        }
        spPtr->numPoints = nel;

        /* Read in data and work out limits */
        spPtr->ytop = -DBL_MAX;
        spPtr->ybot =  DBL_MAX;

        if ( adr == 0 ) {
            /* Read doubles from each word, cannot be reference spectrum */
            for ( i = 0; i < nel; i++ ) {
                if ( Tcl_GetDoubleFromObj( interp, objv[i],
                                           &spPtr->dataPtr[i] ) != TCL_OK ) {
                    return TCL_ERROR;
                }
                if ( spPtr->dataPtr[i] != spPtr->badvalue ) {
                    spPtr->ybot = MIN( spPtr->ybot, spPtr->dataPtr[i] );
                    spPtr->ytop = MAX( spPtr->ytop, spPtr->dataPtr[i] );
                }
            }
        }
        else {
            /* Given ARRAYinfo address, convert type into double precision and
             * check for BAD values which are replaced with our bad value,
             * -DBL_MAX. */
            if ( isref ) { 
                gaiaArrayToDouble( arrayInfo, spPtr->badvalue, 
                                   spPtr->refDataPtr );
            }
            else {
                gaiaArrayToDouble( arrayInfo, spPtr->badvalue, 
                                   spPtr->dataPtr );

                /* Get range of data */
                dataPtr = spPtr->dataPtr;
                for ( i = 0; i < nel; i++ ) {
                    if ( *dataPtr != spPtr->badvalue ) {
                        spPtr->ybot = MIN( spPtr->ybot, *dataPtr );
                        spPtr->ytop = MAX( spPtr->ytop, *dataPtr );
                    }
                    dataPtr++;
                }
            }
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
    Tk_ConfigSpec *specPtr;

#if DEBUG
    fprintf( stderr, "SPConfigure() \n" );
#endif
    tkwin = Tk_CanvasTkwin( canvas );
    /* Note using Tcl_Obj calls so need TK_CONFIG_OBJS flags. */
    if ( Tk_ConfigureWidget( interp, tkwin, configSpecs, objc,
                             (CONST char **) objv, (char *) spPtr,
                             flags|TK_CONFIG_OBJS )
         != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Check if any options that require the plot to be regenerated
     * were set. */
    for ( specPtr = configSpecs; specPtr->type != TK_CONFIG_END; specPtr++ ) {
        if ( specPtr->specFlags & TK_CONFIG_OPTION_SPECIFIED ) {
            /* All will do for now! */
            spPtr->newplot = 1;
           break;
        }
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
       astAnnul( spPtr->framesets[0] );
    }

    if ( spPtr->polyline != NULL ) {
        RtdLineDelete( canvas, spPtr->polyline, display );
    }

    if ( spPtr->refpolyline != NULL ) {
        RtdLineDelete( canvas, spPtr->refpolyline, display );
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
 *      ItemPtr is drawn in drawable...
 */
static void SPDisplay( Tk_Canvas canvas, Tk_Item *itemPtr, Display *display,
                       Drawable drawable, int x, int y, int width,
                       int height )
{
    AstFrame *picked;
    SPItem *spPtr = (SPItem *) itemPtr;
    Tk_Item *polyline;
    XColor *linecolour;
    double basebox[4];
    double *dataPtr;
    double xin[2];
    double xout[2];
    double yin[2];
    double yout[2];
    float graphbox[4];
    int i;
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

    /* Generate the Plot frameset, if required */
    if ( spPtr->newplot || spPtr->framesets[1] == NULL ) {
        spPtr->newplot = 0;
#if DEBUG
        fprintf( stderr, "Regenerating plot\n" );
#endif

        if ( spPtr->framesets[1] != NULL ) {
            astAnnul( spPtr->framesets[1] );
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

        /* Set the limits of the drawing region in physical coordinates. Note
         * these are in the BASE frame of the frameset, not CURRENT
         * coordinates */
        xin[0] = spPtr->xleft;
        xin[1] = spPtr->xright;
        yin[0] = spPtr->ybot;
        yin[1] = spPtr->ytop;
#if DEBUG
        fprintf( stderr, "xleft etc: %f,%f,%f,%f\n", spPtr->xleft, spPtr->ybot,
                 spPtr->xright, spPtr->ytop );
#endif

        astTran2( spPtr->framesets[1], 2, xin, yin, 0, xout, yout );
        basebox[0] = xout[0];
        basebox[1] = yout[0];
        basebox[2] = xout[1];
        basebox[3] = yout[1];

        /* If the transformation fails, take a stab at some useful values. */
        if ( basebox[0] == AST__BAD ) basebox[0] = spPtr->xright;
        if ( basebox[1] == AST__BAD ) basebox[1] = spPtr->ybot;
        if ( basebox[2] == AST__BAD ) basebox[2] = spPtr->xleft;
        if ( basebox[3] == AST__BAD ) basebox[3] = spPtr->ytop;
        if ( ! astOK ) astClearStatus;

#if DEBUG
        fprintf( stderr, "basebox: %f,%f,%f,%f\n", basebox[0], basebox[1],
                 basebox[2], basebox[3] );
        fprintf( stderr, "graphbox: %f,%f,%f,%f\n", graphbox[0], graphbox[1],
                 graphbox[2], graphbox[3] );
#endif
        spPtr->plot = astPlot( spPtr->framesets[1], graphbox, basebox,
                               (spPtr->options == NULL ? "" : spPtr->options));

        /* And plot the grid axes, this is also only required when the
         * framesets change. */
        if ( spPtr->showaxes ) {

            astTk_SetCanvas( Tk_PathName( Tk_CanvasTkwin( canvas ) ) );
            astTk_Tag( spPtr->tagPtr );      /* Includes unique tag */
            ClearSubItems( canvas, spPtr );  /* Remove last grid */

            /* If the spectral axis is a DSBSpecFrame then we need to arrange
             * to show the lower and upper sidebands, this involves drawing
             * the grid twice, with different wavelength coordinates on the
             * bottom and top of grid. */
            if ( spPtr->isDSB ) {
                const char *sideband = astGetC(spPtr->framesets[0],"SideBand");

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

    /* Finally the spectra, for speed we use direct control of two
     * rtd_polyline instances.  First convert coordinates into canvas from
     * current. Use two loops in case have reference spectrum, note reference
     * spectrum is plotted first. */
    for ( i = 0; i < 2; i++ ) {
        if ( i == 0 ) {
            if ( spPtr->refDataPtr == NULL ) {
                /* No reference spectrum, skip to next loop */
                continue;
            }
            dataPtr = spPtr->refDataPtr;
            polyline = spPtr->refpolyline;
            linecolour = spPtr->reflinecolour;
        }
        else {
            dataPtr = spPtr->dataPtr;
            polyline = spPtr->polyline;
            linecolour = spPtr->linecolour;
        }

        astTran2( spPtr->plot, spPtr->numPoints, spPtr->coordPtr,
                  dataPtr, 0, spPtr->tmpPtr[0], spPtr->tmpPtr[1] );

        /* Set line coordinates */
        RtdLineQuickSetCoords( spPtr->interp, canvas, polyline,
                               spPtr->tmpPtr[0], spPtr->tmpPtr[1],
                               spPtr->numPoints );

        RtdLineSetColour( display, polyline, linecolour );
        RtdLineSetWidth( display, polyline, spPtr->linewidth );

        /* Do the draw. */
        RtdLineDisplay( canvas, polyline, display, drawable, x, y,
                        width, height );

        /* Reset AST if any errors occurred, don't want next draw to fail
         * as well. */
        if ( !astOK ) {
            astClearStatus;
        }
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

        /* Need to recompute plot to fit this change */
        spPtr->newplot = 1;
    }

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
    int iaxes[1];
    AstFrame *picked;
    long longResult;
    SPItem *spPtr = (SPItem *) widgRec;

    if ( Tcl_ExprLong( interp, value, &longResult ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Release old frameset, before accepting new */
    if ( spPtr->framesets[0] != NULL ) {
        astAnnul( spPtr->framesets[0] );
    }

    /* Make a clone of the frameset so that the original can be released */
    spPtr->framesets[0] = (AstFrameSet *) astClone((AstFrameSet *)longResult);
    spPtr->framesets[1] = (AstFrameSet *) NULL;

    /* If this has a DSBSpecFrame the plotting is different, so find out.*/
    iaxes[0] = 1;
    picked = astPickAxes( spPtr->framesets[0], 1, iaxes, NULL );
    spPtr->isDSB = astIsADSBSpecFrame( picked );
    astAnnul( picked );

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
        tmpPtr[i] = (double) ( i + 1 );
    }
    astTran1( spPtr->framesets[0], spPtr->numPoints, tmpPtr, 1,
              spPtr->coordPtr );

    /* Set the axis range, pick first non-BAD values from ends (spectrum must
     * be monotonic, so this is OK and keeps the natural order which can be
     * actually be min to max or max to min). Then apply the xminmax
     * preference. */
    spPtr->xleft = DBL_MAX;
    spPtr->xright = -DBL_MAX;
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
 *     Generate a FrameSet suitable for drawing a 2D data plot using our data
 *     values and 1D frameset.
 */
static void MakeSpectral( SPItem *spPtr )
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
