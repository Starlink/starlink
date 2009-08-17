 /*
  * rtdRotBoxEllipse.c --
  *
  *     This file implements rotating box and ellipse items for canvas
  *     widgets.
  *
  * Copyright (c) 1991-1994 The Regents of the University of California.
  * Copyright (c) 1994-1995 Sun Microsystems, Inc.
  * Copyright (c) 1996-2001 Central Laboratory of the Research Councils.
  * Copyright (c) 2009 Science and Technology Facilities Council.
  *
  * See the Tcl distribution file "license.terms" for information on usage and
  * redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
  */

/*
 *    Implements rotating ellipse and box items for a canvas.
 *
 * Authors:
 *    PWD: P.W. Draper (Starlink, Durham University, U.K).
 *
 * Changes:
 *    29-JAN-1996 (PWD):
 *       Original version, based on tkRectOval implementation.
 *    10-MAY-1996 (PWD):
 *       Added rotating rectangle code.
 *    22-OCT-1997 (PWD):
 *       Changed so that semi-major, semi-minor and angle are
 *       configuration options only. Not part of coords command.
 *       The end points of the semi-major and semi-minor axes now
 *       become optional coords values so that transformations of
 *       coords will reorient the object correctly (i.e. for
 *       reflections).
 *    17-AUG-2001 (PWD):
 *       Recoding the ellipe locus generating code to give smoother
 *       ellipses.
 *
 * $Id$
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include "tcl.h"
#include "tk.h"
#include <math.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define UCHAR(c) ((unsigned char) (c))

/* Prototypes for internal Tcl routines */
extern int    TkOvalToArea _ANSI_ARGS_((double *, double *));
extern double TkOvalToPoint _ANSI_ARGS_((double *,double, int, double *));

/* Useful constants */

#define SEGMENTS 60   /*  Number of steps around an ellipse */
#ifndef PI
#define PI 3.141592653589793238462643
#endif
#define D2R PI/180.0
#define R2D 180.0/PI


/*
 * The structure below defines the record for each ellipse or rotbox.
 */

typedef struct BoxEllItem  {
    Tk_Item header;             /* Generic stuff that's the same for all
                                 * types.  MUST BE FIRST IN STRUCTURE. */
    double bbox[4];             /* Bounding box of ellipse (approx) */
    double coords[6];           /* Coordinates of ellipse center and
                                 * possible end points of
                                 * semi-major / semi-minor axes */
    double angle;               /* Position angle of ellipse */
    double semiMajor;           /* SemiMajor axis length */
    double semiMinor;           /* SemiMinor axis length */
    double semiMajorX;          /* X position of end of semimajor axis*/
    double semiMajorY;          /* Y position of end of semimajor axis*/
    double semiMinorX;          /* X position of end of semiminor axis*/
    double semiMinorY;          /* Y position of end of semiminor axis*/
    double centerX;             /* X centre of ellipse */
    double centerY;             /* Y centre of ellipse */
    int width;                  /* Width of outline. */
    XColor *outlineColor;       /* Color for outline. */
    XColor *fillColor;          /* Color for filling ellipse */
    Pixmap fillStipple;         /* Stipple bitmap for filling ellipse. */
    GC outlineGC;               /* Graphics context for outline. */
    GC fillGC;                  /* Graphics context for filling item. */
    int newCoords;              /* If new coords values have been given */
} BoxEllItem;

/*
 * Information used for parsing configuration specs:
 */

static Tk_CustomOption tagsOption = {
     Tk_CanvasTagsParseProc, Tk_CanvasTagsPrintProc, (ClientData) NULL
};

static Tk_ConfigSpec configSpecs[] = {

    {TK_CONFIG_COLOR, "-fill", (char *) NULL, (char *) NULL,
     (char *) NULL, Tk_Offset(BoxEllItem, fillColor), TK_CONFIG_NULL_OK},

    {TK_CONFIG_COLOR, "-outline", (char *) NULL, (char *) NULL,
     "black", Tk_Offset(BoxEllItem, outlineColor), TK_CONFIG_NULL_OK},

    {TK_CONFIG_BITMAP, "-stipple", (char *) NULL, (char *) NULL,
     (char *) NULL, Tk_Offset(BoxEllItem, fillStipple), TK_CONFIG_NULL_OK},

    {TK_CONFIG_CUSTOM, "-tags", (char *) NULL, (char *) NULL,
     (char *) NULL, 0, TK_CONFIG_NULL_OK, &tagsOption},

    {TK_CONFIG_PIXELS, "-width", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(BoxEllItem, width), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_DOUBLE, "-angle", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(BoxEllItem, angle), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_DOUBLE, "-semimajor", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(BoxEllItem, semiMajor), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_DOUBLE, "-semiminor", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(BoxEllItem, semiMinor), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_DOUBLE, "-semimajorx", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(BoxEllItem, semiMajorX), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_DOUBLE, "-semiminorx", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(BoxEllItem, semiMinorX), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_DOUBLE, "-semimajory", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(BoxEllItem, semiMajorY), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_DOUBLE, "-semiminory", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(BoxEllItem, semiMinorY), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_DOUBLE, "-xcenter", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(BoxEllItem, centerX), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_DOUBLE, "-ycenter", (char *) NULL, (char *) NULL,
     "1", Tk_Offset(BoxEllItem, centerY), TK_CONFIG_DONT_SET_DEFAULT},

    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
     (char *) NULL, 0, 0}
};

/*
 * Prototypes for procedures defined in this file:
 */

static void   ComputeBoxEllBbox _ANSI_ARGS_((Tk_Canvas canvas,
                                             BoxEllItem *boxellPtr));
static int    ConfigureBoxEll _ANSI_ARGS_((Tcl_Interp *interp,
                                           Tk_Canvas canvas, Tk_Item *itemPtr,
                                           int argc, char **argv, int flags));
static int    CreateBoxEll _ANSI_ARGS_((Tcl_Interp *interp,
                                        Tk_Canvas canvas, struct Tk_Item *itemPtr,
                                        int argc, char **argv));
static void   DeleteBoxEll _ANSI_ARGS_((Tk_Canvas canvas,
                                        Tk_Item *itemPtr, Display *display));
static void   DisplayBoxEll _ANSI_ARGS_((Tk_Canvas canvas,
                                         Tk_Item *itemPtr, Display *display, Drawable dst,
                                         int x, int y, int width, int height));
static int    EllipseToArea _ANSI_ARGS_((Tk_Canvas canvas,
                                         Tk_Item *itemPtr, double *areaPtr));
static int    RotBoxToArea _ANSI_ARGS_((Tk_Canvas canvas,
                                        Tk_Item *itemPtr, double *areaPtr));
static double EllipseToPoint _ANSI_ARGS_((Tk_Canvas canvas,
                                          Tk_Item *itemPtr, double *pointPtr));
static double RotBoxToPoint _ANSI_ARGS_((Tk_Canvas canvas,
                                         Tk_Item *itemPtr, double *pointPtr));
static int    BoxEllCoords _ANSI_ARGS_((Tcl_Interp *interp,
                                        Tk_Canvas canvas, Tk_Item *itemPtr, int argc,
                                        char **argv));
static int    EllipseToPostscript _ANSI_ARGS_((Tcl_Interp *interp,
                                               Tk_Canvas canvas, Tk_Item *itemPtr, int prepass));
static int    RotBoxToPostscript _ANSI_ARGS_((Tcl_Interp *interp,
                                              Tk_Canvas canvas, Tk_Item *itemPtr, int prepass));
static void   ScaleBoxEll _ANSI_ARGS_((Tk_Canvas canvas,
                                       Tk_Item *itemPtr, double originX, double originY,
                                       double scaleX, double scaleY));
static void   TranslateBoxEll _ANSI_ARGS_((Tk_Canvas canvas,
                                           Tk_Item *itemPtr, double deltaX, double deltaY));
static int    GenEllipse _ANSI_ARGS_( ( XPoint *ellipse,
                                        double centerX,
                                        double centerY,
                                        double semiMajor,
                                        double semiMinor,
                                        double angle ) );
static int    GenRotBox _ANSI_ARGS_( ( XPoint *ellipse,
                                       double centerX,
                                       double centerY,
                                       double semiMajor,
                                       double semiMinor,
                                       double angle ) );
static void   ComputeBoxEllGeom _ANSI_ARGS_((Tk_Canvas canvas,
                                             BoxEllItem *boxellPtr));

/*
 * The structure below defines the ellipse item type, by means of
 * procedures that can be invoked by generic item code.
 */

Tk_ItemType rtdEllipseType = {
    "rtd_ellipse",                /* name */
    sizeof(BoxEllItem),           /* itemSize */
    CreateBoxEll,                 /* createProc */
    configSpecs,                  /* configSpecs */
    ConfigureBoxEll,              /* configureProc */
    BoxEllCoords,                 /* coordProc */
    DeleteBoxEll,                 /* deleteProc */
    DisplayBoxEll,                /* displayProc */
    0,                            /* alwaysRedraw */
    EllipseToPoint,               /* pointProc */
    EllipseToArea,                /* areaProc */
    EllipseToPostscript,          /* postscriptProc */
    ScaleBoxEll,                  /* scaleProc */
    TranslateBoxEll,              /* translateProc */
    (Tk_ItemIndexProc *) NULL,    /* indexProc */
    (Tk_ItemCursorProc *) NULL,   /* cursorProc */
    (Tk_ItemSelectionProc *) NULL,/* selectionProc */
    (Tk_ItemInsertProc *) NULL,   /* insertProc */
    (Tk_ItemDCharsProc *) NULL,   /* dTextProc */
    (Tk_ItemType *) NULL          /* nextPtr */
};

/*
 * The structure below defines the rotbox item type, by means of
 * procedures that can be invoked by generic item code.
 */

Tk_ItemType rtdRotBoxType = {
    "rtd_rotbox",                 /* name */
    sizeof(BoxEllItem),           /* itemSize */
    CreateBoxEll,                 /* createProc */
    configSpecs,                  /* configSpecs */
    ConfigureBoxEll,              /* configureProc */
    BoxEllCoords,                 /* coordProc */
    DeleteBoxEll,                 /* deleteProc */
    DisplayBoxEll,                /* displayProc */
    0,                            /* alwaysRedraw */
    RotBoxToPoint,                /* pointProc */
    RotBoxToArea,                 /* areaProc */
    RotBoxToPostscript,           /* postscriptProc */
    ScaleBoxEll,                  /* scaleProc */
    TranslateBoxEll,              /* translateProc */
    (Tk_ItemIndexProc *) NULL,    /* indexProc */
    (Tk_ItemCursorProc *) NULL,   /* cursorProc */
    (Tk_ItemSelectionProc *) NULL,/* selectionProc */
    (Tk_ItemInsertProc *) NULL,   /* insertProc */
    (Tk_ItemDCharsProc *) NULL,   /* dTextProc */
    (Tk_ItemType *) NULL          /* nextPtr */
};


/*
 *--------------------------------------------------------------
 * Ellipse_Init --
 *
 *   This procedure initialises the rtd_ellipse canvas item.
 *
  *--------------------------------------------------------------
 *
 */
int Ellipse_Init() {
    Tk_CreateItemType(&rtdEllipseType);
    return TCL_OK;
}


/*
 *--------------------------------------------------------------
 * RotBox_Init --
 *
 *   This procedure initialises the rtd_rotbox canvas item.
 *
 *--------------------------------------------------------------
 *
 */
int RotBox_Init() {
    Tk_CreateItemType(&rtdRotBoxType);
    return TCL_OK;
}


/*
 *--------------------------------------------------------------
 *
 * CreateBoxEll --
 *
 *      This procedure is invoked to create a new ellipse or rotbox.
 *
 * Results:
 *      A standard Tcl return value.  If an error occurred in
 *      creating the item, then an error message is left in
 *      interp->result;  in this case itemPtr is left uninitialized,
 *      so it can be safely freed by the caller.
 *
 * Side effects:
 *      A new ellipse or rotbox is created.
 *
 *--------------------------------------------------------------
 */

static int
CreateBoxEll(interp, canvas, itemPtr, argc, argv)
    Tcl_Interp *interp;                 /* For error reporting. */
    Tk_Canvas canvas;                   /* Canvas to hold new item. */
    Tk_Item *itemPtr;                   /* Record to hold new item;  header
                                         * has been initialized by caller. */
    int argc;                           /* Number of arguments in argv. */
    char **argv;                        /* Arguments describing item. */
{
    BoxEllItem *boxellPtr = (BoxEllItem *) itemPtr;
    int i;
    int npoint;

    if (argc < 2 ) {
        Tcl_AppendResult(interp, "wrong # args:  should be \"",
                         Tk_PathName(Tk_CanvasTkwin(canvas)), "\" create ",
                         itemPtr->typePtr->name,
                         " xcenter ycenter ?end-points? ?options?",
                         (char *) NULL);
        return TCL_ERROR;
    }

    /*
     * Carry out initialization that is needed in order to clean
     * up after errors during the the remainder of this procedure.
     */

    boxellPtr->width = 1;
    boxellPtr->outlineColor = NULL;
    boxellPtr->fillColor = NULL;
    boxellPtr->fillStipple = None;
    boxellPtr->outlineGC = None;
    boxellPtr->fillGC = None;

    boxellPtr->centerX = boxellPtr->coords[0] = 10.0;
    boxellPtr->centerY = boxellPtr->coords[1] = 10.0;
    boxellPtr->semiMajorX = boxellPtr->coords[2] = 10.0;
    boxellPtr->semiMajorY = boxellPtr->coords[3] = 0.0;
    boxellPtr->semiMinorX = boxellPtr->coords[4] = 0.0;
    boxellPtr->semiMinorY = boxellPtr->coords[5] = 10.0;
    boxellPtr->semiMajor = 10.0;
    boxellPtr->semiMinor = 10.0;
    boxellPtr->angle = 0;
    boxellPtr->newCoords = 0;


    /*
     * Count the number of points supplied. Leading arguments are
     * assumed to be points if they start with a digit or a minus sign
     * followed by a digit.
     */
    for (i = 2; i < 6; i+=2) {
        if ((!isdigit(UCHAR(argv[i][0]))) &&
            ((argv[i][0] != '-')
             || ((argv[i][1] != '.') && !isdigit(UCHAR(argv[i][1]))))) {
            break;
        }
    }
    npoint = i;
    for (i = 0; i < npoint; i++) {
        if (Tk_CanvasGetCoord(interp, canvas, argv[i], &boxellPtr->coords[i])
            != TCL_OK) {
            return TCL_ERROR;
        }
    }

    /*  And configure using any remaining options */
    boxellPtr->newCoords = npoint;
    if (ConfigureBoxEll(interp, canvas, itemPtr, argc-npoint,
                        argv+npoint, 0) != TCL_OK) {
        DeleteBoxEll(canvas, itemPtr, Tk_Display(Tk_CanvasTkwin(canvas)));
        return TCL_ERROR;
    }
    return TCL_OK;
}



/*
 *--------------------------------------------------------------
 *
 * BoxEllCoords --
 *
 *   This procedure is invoked to process the "coords" widget command
 *   for ellipses and rotboxes. See the user documentation for details
 *   on what it does.
 *
 * Results:
 *      Returns TCL_OK or TCL_ERROR, and sets interp->result.
 *
 * Side effects:
 *      The coordinates for the given item may be changed.
 *
 *--------------------------------------------------------------
 */

static int
BoxEllCoords(interp, canvas, itemPtr, argc, argv)
    Tcl_Interp *interp;                 /* Used for error reporting. */
    Tk_Canvas canvas;                   /* Canvas containing item. */
    Tk_Item *itemPtr;                   /* Item whose coordinates are to be
                                         * read or modified. */
    int argc;                           /* Number of coordinates supplied in
                                         * argv. */
    char **argv;                        /* Array of values to use */
{
    BoxEllItem *boxellPtr = (BoxEllItem *) itemPtr;
    int i;

    char c0[TCL_DOUBLE_SPACE], c1[TCL_DOUBLE_SPACE];
    char c2[TCL_DOUBLE_SPACE], c3[TCL_DOUBLE_SPACE];
    char c4[TCL_DOUBLE_SPACE], c5[TCL_DOUBLE_SPACE];

    if (argc == 0) {
        Tcl_PrintDouble(interp, boxellPtr->coords[0], c0);
        Tcl_PrintDouble(interp, boxellPtr->coords[1], c1);
        Tcl_PrintDouble(interp, boxellPtr->coords[2], c2);
        Tcl_PrintDouble(interp, boxellPtr->coords[3], c3);
        Tcl_PrintDouble(interp, boxellPtr->coords[4], c4);
        Tcl_PrintDouble(interp, boxellPtr->coords[5], c5);
        Tcl_AppendResult(interp, c0, " ", c1," ", c2," ", c3," ", c4," ",
                         c5, (char *) NULL);
    } else if (argc == 2 || argc == 6 ) {
        for ( i = 0; i < argc; i++ ) {
            if ((Tk_CanvasGetCoord(interp, canvas, argv[i],
                                   &boxellPtr->coords[i]) != TCL_OK) ) {
                return TCL_ERROR;
            }
        }
        boxellPtr->newCoords = argc;
        ComputeBoxEllGeom(canvas, boxellPtr);
    } else {
        char buffer[80];
        sprintf(buffer,
                "wrong # coordinates:  expected 0, 2 or 6, got %d", argc);
        Tcl_SetResult(interp, buffer, TCL_VOLATILE);
        return TCL_ERROR;
    }
    return TCL_OK;
}



/*
 *--------------------------------------------------------------
 *
 * ConfigureBoxEll --
 *
 *      This procedure is invoked to configure various aspects of an
 *      ellipse or rotbox, such as its border and background colors.
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

static int
ConfigureBoxEll(interp, canvas, itemPtr, argc, argv, flags)
    Tcl_Interp *interp;           /* Used for error reporting. */
    Tk_Canvas canvas;             /* Canvas containing itemPtr. */
    Tk_Item *itemPtr;             /* Rectangle item to reconfigure. */
    int argc;                     /* Number of elements in argv.  */
    char **argv;                  /* Arguments describing things to configure. */
    int flags;                    /* Flags to pass to Tk_ConfigureWidget. */
{
    BoxEllItem *boxellPtr = (BoxEllItem *) itemPtr;
    GC newGC;
    Tk_ConfigSpec *p;
    Tk_Window tkwin;
    XGCValues gcValues;
    int i;
    unsigned long mask;

    tkwin = Tk_CanvasTkwin(canvas);
    if (Tk_ConfigureWidget(interp, tkwin, configSpecs, argc, argv,
                           (char *) boxellPtr, flags) != TCL_OK) {
        return TCL_ERROR;
    }

    /* Set config flags (TK_CONFIG_OPTION_SPECIFIED not supported in Tk8.5) */
    for ( p = configSpecs; p->type != TK_CONFIG_END; p++ ) {

        /*  Default is cleared. */
        p->specFlags &= ~TK_CONFIG_OPTION_SPECIFIED;

        for ( i = 0; i < argc; i +=2 ) {
            if ( strcmp( argv[i], p->argvName ) == 0 ) {
                /*  Option in list, so set. */
                p->specFlags |= TK_CONFIG_OPTION_SPECIFIED;
                break;
            }
        }
    }

    /*
     * A few of the options require additional processing, such as
     * graphics contexts.
     */

    if (boxellPtr->width < 1) {
        boxellPtr->width = 1;
    }
    if (boxellPtr->outlineColor == NULL) {
        newGC = None;
    } else {
        gcValues.foreground = boxellPtr->outlineColor->pixel;
        gcValues.cap_style = CapProjecting;
        gcValues.line_width = boxellPtr->width;
        mask = GCForeground|GCCapStyle|GCLineWidth;
        newGC = Tk_GetGC(tkwin, mask, &gcValues);
    }
    if (boxellPtr->outlineGC != None) {
        Tk_FreeGC(Tk_Display(tkwin), boxellPtr->outlineGC);
    }
    boxellPtr->outlineGC = newGC;

    if (boxellPtr->fillColor == NULL) {
        newGC = None;
    } else {
        gcValues.foreground = boxellPtr->fillColor->pixel;
        if (boxellPtr->fillStipple != None) {
            gcValues.stipple = boxellPtr->fillStipple;
            gcValues.fill_style = FillStippled;
            mask = GCForeground|GCStipple|GCFillStyle;
        } else {
            mask = GCForeground;
        }
        newGC = Tk_GetGC(tkwin, mask, &gcValues);
    }
    if (boxellPtr->fillGC != None) {
        Tk_FreeGC(Tk_Display(tkwin), boxellPtr->fillGC);
    }
    boxellPtr->fillGC = newGC;

    /*  Now deal with the ellipse geometry changes. */
    ComputeBoxEllGeom(canvas, boxellPtr);
    return TCL_OK;
}



/*
 *--------------------------------------------------------------
 *
 * DeleteBoxEll --
 *
 *      This procedure is called to clean up the data structure
 *      associated with an ellipse or rotbox.
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
DeleteBoxEll(canvas, itemPtr, display)
    Tk_Canvas canvas;                   /* Info about overall widget. */
    Tk_Item *itemPtr;                   /* Item that is being deleted. */
    Display *display;                   /* Display containing window for
                                         * canvas. */
{
    BoxEllItem *boxellPtr = (BoxEllItem *) itemPtr;

    if (boxellPtr->outlineColor != NULL) {
        Tk_FreeColor(boxellPtr->outlineColor);
    }
    if (boxellPtr->fillColor != NULL) {
        Tk_FreeColor(boxellPtr->fillColor);
    }
    if (boxellPtr->fillStipple != None) {
        Tk_FreeBitmap(display, boxellPtr->fillStipple);
    }
    if (boxellPtr->outlineGC != None) {
        Tk_FreeGC(display, boxellPtr->outlineGC);
    }
    if (boxellPtr->fillGC != None) {
        Tk_FreeGC(display, boxellPtr->fillGC);
    }
}


/*
 *--------------------------------------------------------------
 *
 * ComputeBoxEllBbox --
 *
 *      This procedure is invoked to compute the bounding box of
 *      all the pixels that may be drawn as part of an ellipse
 *      or rotbox.
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

        /* ARGSUSED */
static void
ComputeBoxEllBbox(canvas, boxellPtr)
    Tk_Canvas canvas;                   /* Canvas that contains item. */
    BoxEllItem *boxellPtr;              /* Item whose bbox is to be
                                         * recomputed. */
{
    int bloat;
    double x[4], y[4];                  /* Positions of rectangle
                                         * corners of rotated bbox */
    double cosTheta, sinTheta;
    double cosThetaX, sinThetaX;
    double cosThetaY, sinThetaY;
    int i;

    /* The bounding box used is approximate and corresponds to a
     * rotation of the bounding box of an unrotated ellipse. Which is
     * correct for a rotbox, but slightly too much for an ellipse.
     */

    cosTheta = cos( boxellPtr->angle * D2R );
    sinTheta = sin( boxellPtr->angle * D2R );
    cosThetaX = boxellPtr->semiMajor * cosTheta;
    sinThetaX = boxellPtr->semiMajor * sinTheta;
    cosThetaY = boxellPtr->semiMinor * cosTheta;
    sinThetaY = boxellPtr->semiMinor * sinTheta;

    x[0] = cosThetaX - sinThetaY;
    y[0] = sinThetaX + cosThetaY;

    x[1] = cosThetaX + sinThetaY;
    y[1] = sinThetaX - cosThetaY;

    x[2] = -cosThetaX + sinThetaY;
    y[2] = -sinThetaX - cosThetaY;

    x[3] = -cosThetaX - sinThetaY;
    y[3] = -sinThetaX + cosThetaY;

    /*
     * Now pick out the bounding box.
     */

    boxellPtr->bbox[0] = x[0];
    boxellPtr->bbox[1] = y[0];
    boxellPtr->bbox[2] = x[1];
    boxellPtr->bbox[3] = y[1];
    for ( i = 0; i < 4; i++ ) {
        if ( x[i] < boxellPtr->bbox[0] ) {
            boxellPtr->bbox[0] = x[i];
        }
        if ( y[i] < boxellPtr->bbox[1] ) {
            boxellPtr->bbox[1] = y[i];
        }
        if ( x[i] > boxellPtr->bbox[2] ) {
            boxellPtr->bbox[2] = x[i];
        }
        if ( y[i] > boxellPtr->bbox[3] ) {
            boxellPtr->bbox[3] = y[i];
        }
    }

    /* And add origin */
    boxellPtr->bbox[0] += boxellPtr->centerX;
    boxellPtr->bbox[1] += boxellPtr->centerY;
    boxellPtr->bbox[2] += boxellPtr->centerX;
    boxellPtr->bbox[3] += boxellPtr->centerY;

    if (boxellPtr->outlineColor == NULL) {
        bloat = 0;
    } else {
        bloat = (boxellPtr->width+1)/2;
    }

    /*
     * Note:  add an extra pixel to the bounding box on all sides to
     * account for rounding error (e.g. negative coords will be rounded
     * differently than positive ones).
     */

    boxellPtr->header.x1 = boxellPtr->bbox[0] - bloat - 1;
    boxellPtr->header.y1 = boxellPtr->bbox[1] - bloat - 1;
    boxellPtr->header.x2 = boxellPtr->bbox[2] + bloat + 1;
    boxellPtr->header.y2 = boxellPtr->bbox[3] + bloat + 1;
}



/*
 *--------------------------------------------------------------
 *
 * DisplayBoxEll --
 *
 *      This procedure is invoked to draw a rotbox or ellipse
 *      item in a given drawable.
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

static void
DisplayBoxEll(canvas, itemPtr, display, drawable, x, y, width, height)
    Tk_Canvas canvas;                   /* Canvas that contains item. */
    Tk_Item *itemPtr;                   /* Item to be displayed. */
    Display *display;                   /* Display on which to draw item. */
    Drawable drawable;                  /* Pixmap or window in which to draw
                                         * item. */
    int x, y, width, height;            /* Describes region of canvas that
                                         * must be redisplayed (not used). */
{
    BoxEllItem *boxellPtr = (BoxEllItem *) itemPtr;
    short xc, yc;
    XPoint pointPtr[SEGMENTS];  /*  Actual space for ellipse points */
    int nPoints;

    /* Compute screen coordinates for the centre of the ellipse */

    Tk_CanvasDrawableCoords(canvas, boxellPtr->centerX, boxellPtr->centerY,
                            &xc, &yc);

    if (boxellPtr->header.typePtr == &rtdEllipseType) {

        /* Generate the ellipse */
        nPoints = GenEllipse( pointPtr, (double)xc, (double)yc,
                              boxellPtr->semiMajor, boxellPtr->semiMinor,
                              boxellPtr->angle );
    } else {

        /* Generate the points for the rotbox */
        nPoints = GenRotBox( pointPtr, (double)xc, (double)yc,
                             boxellPtr->semiMajor, boxellPtr->semiMinor,
                             boxellPtr->angle );
    }

    /*
     * Display filled part first (if wanted), then outline.  If we're
     * stippling, then modify the stipple offset in the GC.  Be sure to
     * reset the offset when done, since the GC is supposed to be
     * read-only.
     */

    if (boxellPtr->fillGC != None) {
        if (boxellPtr->fillStipple != None) {
            Tk_CanvasSetStippleOrigin(canvas, boxellPtr->fillGC);
        }
        XFillPolygon( display, drawable, boxellPtr->fillGC, pointPtr,
                      nPoints, Convex, CoordModeOrigin);
        if (boxellPtr->fillStipple != None) {
            XSetTSOrigin(display, boxellPtr->fillGC, 0, 0);
        }
    }
    if (boxellPtr->outlineGC != None) {
        XDrawLines( display, drawable, boxellPtr->outlineGC, pointPtr,
                    nPoints, CoordModeOrigin);
    }
}



/*
 *--------------------------------------------------------------
 *
 * EllipseToPoint --
 *
 *      Computes the distance from a given point to a given
 *      ellipse or rotbox, in canvas units.
 *
 * Results:
 *      The return value is 0 if the point whose x and y coordinates
 *      are coordPtr[0] and coordPtr[1] is inside the ellipse.  If the
 *      point isn't inside the ellipse then the return value is the
 *      distance from the point to the item.  If itemPtr is filled,
 *      then anywhere in the interior is considered "inside"; if
 *      itemPtr isn't filled, then "inside" means only the area
 *      occupied by the outline.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static double
EllipseToPoint(canvas, itemPtr, pointPtr)
    Tk_Canvas canvas;             /* Canvas containing item. */
    Tk_Item *itemPtr;             /* Item to check against point. */
    double *pointPtr;             /* Pointer to x and y coordinates. */
{
    BoxEllItem *boxellPtr = (BoxEllItem *) itemPtr;
    double width;
    double newpoint[2];
    double newbbox[4];
    double cosTheta, sinTheta;
    int filled;
    double  x, y;

    width = boxellPtr->width;
    filled = boxellPtr->fillGC != None;
    if (boxellPtr->outlineGC == None) {
        width = 0.0;
        filled = 1;
    }


    /*  This works by making use of the normal Tk Oval routines. The
     *  slight of hand is is to rotate our current coordinate system
     *  into a oval based one (all axes aligned) and then perform
     *  the test.
     */

    cosTheta = cos( boxellPtr->angle * D2R );
    sinTheta = sin( boxellPtr->angle * D2R );
    x = pointPtr[0] - boxellPtr->centerX;
    y = pointPtr[1] - boxellPtr->centerY;
    newpoint[0] = x * cosTheta + y * sinTheta;
    newpoint[1] = -x * sinTheta + y * cosTheta;

    newbbox[0] = -boxellPtr->semiMajor;
    newbbox[1] = -boxellPtr->semiMinor;
    newbbox[2] = boxellPtr->semiMajor;
    newbbox[3] = boxellPtr->semiMinor;

    return TkOvalToPoint( newbbox, width, filled, newpoint);
}



/*
 *--------------------------------------------------------------
 *
 * RotBoxToPoint --
 *
 *      Computes the distance from a given point to a given
 *      rectangle, in canvas units.
 *
 * Results:
 *      The return value is 0 if the point whose x and y coordinates
 *      are coordPtr[0] and coordPtr[1] is inside the rotbox.  If the
 *      point isn't inside the rotbox then the return value is the
 *      distance from the point to the rotbox.  If itemPtr is filled,
 *      then anywhere in the interior is considered "inside"; if
 *      itemPtr isn't filled, then "inside" means only the area
 *      occupied by the outline.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

        /* ARGSUSED */
static double
RotBoxToPoint(canvas, itemPtr, pointPtr)
    Tk_Canvas canvas;           /* Canvas containing item. */
    Tk_Item *itemPtr;           /* Item to check against point. */
    double *pointPtr;           /* Pointer to x and y coordinates. */
{
    BoxEllItem *boxellPtr = (BoxEllItem *) itemPtr;
    double xDiff, yDiff, x1, y1, x2, y2, inc, tmp;
    double newpoint[2];
    double newbbox[4];
    double cosTheta, sinTheta;
    double  x, y;

    /*  Rotate our current coordinate system into a rectangular based
     *  one (all axes aligned) and then perform the test.
     */

    cosTheta = cos( boxellPtr->angle * D2R );
    sinTheta = sin( boxellPtr->angle * D2R );
    x = pointPtr[0] - boxellPtr->centerX;
    y = pointPtr[1] - boxellPtr->centerY;
    newpoint[0] = x * cosTheta + y * sinTheta;
    newpoint[1] = -x * sinTheta + y * cosTheta;

    newbbox[0] = -boxellPtr->semiMajor;
    newbbox[1] = -boxellPtr->semiMinor;
    newbbox[2] = boxellPtr->semiMajor;
    newbbox[3] = boxellPtr->semiMinor;

    /*
     * Generate a new larger rectangle that includes the border
     * width, if there is one.
     */

    x1 = newbbox[0];
    y1 = newbbox[1];
    x2 = newbbox[2];
    y2 = newbbox[3];
    if (boxellPtr->outlineGC != None) {
        inc = boxellPtr->width/2.0;
        x1 -= inc;
        y1 -= inc;
        x2 += inc;
        y2 += inc;
    }

    /*
     * If the point is inside the rectangle, handle specially:
     * distance is 0 if rectangle is filled, otherwise compute
     * distance to nearest edge of rectangle and subtract width
     * of edge.
     */

    if ((newpoint[0] >= x1) && (newpoint[0] < x2)
        && (newpoint[1] >= y1) && (newpoint[1] < y2)) {
        if ((boxellPtr->fillGC != None) || (boxellPtr->outlineGC == None)) {
            return 0.0;
        }
        xDiff = newpoint[0] - x1;
        tmp = x2 - newpoint[0];
        if (tmp < xDiff) {
            xDiff = tmp;
        }
        yDiff = newpoint[1] - y1;
        tmp = y2 - newpoint[1];
        if (tmp < yDiff) {
            yDiff = tmp;
        }
        if (yDiff < xDiff) {
            xDiff = yDiff;
        }
        xDiff -= boxellPtr->width;
        if (xDiff < 0.0) {
            return 0.0;
        }
        return xDiff;
    }

    /*
     * Point is outside rectangle.
     */

    if (newpoint[0] < x1) {
        xDiff = x1 - newpoint[0];
    } else if (newpoint[0] > x2)  {
        xDiff = newpoint[0] - x2;
    } else {
        xDiff = 0;
    }

    if (newpoint[1] < y1) {
        yDiff = y1 - newpoint[1];
    } else if (newpoint[1] > y2)  {
        yDiff = newpoint[1] - y2;
    } else {
        yDiff = 0;
    }

    return hypot(xDiff, yDiff);
}


/*
 *--------------------------------------------------------------
 *
 * EllipseToArea --
 *
 *      This procedure is called to determine whether an item
 *      lies entirely inside, entirely outside, or overlapping
 *      a given rectangular area.
 *
 * Results:
 *      -1 is returned if the item is entirely outside the area
 *      given by rectPtr, 0 if it overlaps, and 1 if it is entirely
 *      inside the given area.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static int
EllipseToArea(canvas, itemPtr, areaPtr)
    Tk_Canvas canvas;             /* Canvas containing item. */
    Tk_Item *itemPtr;             /* Item to check against oval. */
    double *areaPtr;              /* Pointer to array of four coordinates
                                   * (x1, y1, x2, y2) describing rectangular
                                   * area.  */
{
    BoxEllItem *boxellPtr = (BoxEllItem *) itemPtr;
    double oval[4], halfWidth;
    double newBbox[4];
    double newArea[4];
    double cosTheta, sinTheta;
    double x, y;
    int result;

    /*  This works by making use of the normal Tk Oval routine. The
     *  slight of hand is is to rotate our current coordinate system
     *  into a oval based one (all axes aligned) and then perform the test.
     */

    cosTheta = cos( -boxellPtr->angle * D2R );
    sinTheta = sin( -boxellPtr->angle * D2R );

    x = areaPtr[0] - boxellPtr->centerX;
    y = areaPtr[1] - boxellPtr->centerY;
    newArea[0] = x * cosTheta + y * sinTheta;
    newArea[1] = -x * sinTheta + y * cosTheta;

    x = areaPtr[2] - boxellPtr->centerX;
    y = areaPtr[3] - boxellPtr->centerY;
    newArea[0] = x * cosTheta + y * sinTheta;
    newArea[1] = -x * sinTheta + y * cosTheta;

    newBbox[0] = - boxellPtr->semiMajor;
    newBbox[1] = - boxellPtr->semiMinor;
    newBbox[2] = boxellPtr->semiMajor;
    newBbox[3] = boxellPtr->semiMinor;

    /*
     * Expand the ellipse to include the width of the outline, if any.
     */

    halfWidth = boxellPtr->width/2.0;
    if (boxellPtr->outlineGC == None) {
        halfWidth = 0.0;
    }
    oval[0] = newBbox[0] - halfWidth;
    oval[1] = newBbox[1] - halfWidth;
    oval[2] = newBbox[2] + halfWidth;
    oval[3] = newBbox[3] + halfWidth;

    result = TkOvalToArea(oval, newArea);


    /*
     * If the rectangle appears to overlap the oval and the oval
     * isn't filled, do one more check to see if perhaps all four
     * of the rectangle's corners are totally inside the oval's
     * unfilled center, in which case we should return "outside".
     */

    if ((result == 0) && (boxellPtr->outlineGC != None)
        && (boxellPtr->fillGC == None)) {
        double centerX, centerY, width, height;
        double xDelta1, yDelta1, xDelta2, yDelta2;

        centerX = boxellPtr->centerX;
        centerY = boxellPtr->centerY;
        width = (newBbox[2] - newBbox[0])/2.0 - halfWidth;
        height = (newBbox[3] - newBbox[1])/2.0 - halfWidth;
        xDelta1 = (newArea[0] - centerX)/width;
        xDelta1 *= xDelta1;
        yDelta1 = (newArea[1] - centerY)/height;
        yDelta1 *= yDelta1;
        xDelta2 = (newArea[2] - centerX)/width;
        xDelta2 *= xDelta2;
        yDelta2 = (newArea[3] - centerY)/height;
        yDelta2 *= yDelta2;
        if (((xDelta1 + yDelta1) < 1.0)
            && ((xDelta1 + yDelta2) < 1.0)
            && ((xDelta2 + yDelta1) < 1.0)
            && ((xDelta2 + yDelta2) < 1.0)) {
            return -1;
        }
    }
    return result;
}


/*
 *--------------------------------------------------------------
 *
 * RotBoxToArea --
 *
 *      This procedure is called to determine whether an item
 *      lies entirely inside, entirely outside, or overlapping
 *      a given rotbox.
 *
 * Results:
 *      -1 is returned if the item is entirely outside the area
 *      given by boxellPtr, 0 if it overlaps, and 1 if it is entirely
 *      inside the given area.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

/* ARGSUSED */
static int
RotBoxToArea(canvas, itemPtr, areaPtr)
    Tk_Canvas canvas;            /* Canvas containing item. */
    Tk_Item *itemPtr;            /* Item to check against rotbox. */
    double *areaPtr;             /* Pointer to array of four coordinates
                                  * (x1, y1, x2, y2) describing rectangular
                                  * area.  */
{
    BoxEllItem *boxellPtr = (BoxEllItem *) itemPtr;
    double halfWidth;
    double newBbox[4];
    double newArea[4];
    double cosTheta, sinTheta;
    double x, y;

    /*  Rotate our current coordinate system into a rectangular based
     *  one (all axes aligned) and then perform the test.
     */

    cosTheta = cos( -boxellPtr->angle * D2R );
    sinTheta = sin( -boxellPtr->angle * D2R );

    x = areaPtr[0] - boxellPtr->centerX;
    y = areaPtr[1] - boxellPtr->centerY;
    newArea[0] = x * cosTheta + y * sinTheta;
    newArea[1] = -x * sinTheta + y * cosTheta;

    x = areaPtr[2] - boxellPtr->centerX;
    y = areaPtr[3] - boxellPtr->centerY;
    newArea[0] = x * cosTheta + y * sinTheta;
    newArea[1] = -x * sinTheta + y * cosTheta;

    newBbox[0] = - boxellPtr->semiMajor;
    newBbox[1] = - boxellPtr->semiMinor;
    newBbox[2] = boxellPtr->semiMajor;
    newBbox[3] = boxellPtr->semiMinor;

    halfWidth = boxellPtr->width/2.0;
    if (boxellPtr->outlineGC == None) {
        halfWidth = 0.0;
    }

    if ((newArea[2] <= (newBbox[0] - halfWidth))
        || (newArea[0] >= (newBbox[2] + halfWidth))
        || (newArea[3] <= (newBbox[1] - halfWidth))
        || (newArea[1] >= (newBbox[3] + halfWidth))) {
        return -1;
    }
    if ((boxellPtr->fillGC == None) && (boxellPtr->outlineGC != None)
        && (newArea[0] >= (newBbox[0] + halfWidth))
        && (newArea[1] >= (newBbox[1] + halfWidth))
        && (newArea[2] <= (newBbox[2] - halfWidth))
        && (newArea[3] <= (newBbox[3] - halfWidth))) {
        return -1;
    }
    if ((newArea[0] <= (newBbox[0] - halfWidth))
        && (newArea[1] <= (newBbox[1] - halfWidth))
        && (newArea[2] >= (newBbox[2] + halfWidth))
        && (newArea[3] >= (newBbox[3] + halfWidth))) {
        return 1;
    }
    return 0;
}


/*
 *--------------------------------------------------------------
 *
 * ScaleBoxEll --
 *
 *      This procedure is invoked to rescale an ellipse or rotbox.
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
 *--------------------------------------------------------------
 */

static void
ScaleBoxEll(canvas, itemPtr, originX, originY, scaleX, scaleY)
    Tk_Canvas canvas;                     /* Canvas containing rectangle. */
    Tk_Item *itemPtr;                     /* Rectangle to be scaled. */
    double originX, originY;              /* Origin about which to scale rect. */
    double scaleX;                        /* Amount to scale in X direction. */
    double scaleY;                        /* Amount to scale in Y direction. */
{
    double xc, yc;
    double xmaj, ymaj;
    double xmin, ymin;
    BoxEllItem *boxellPtr = (BoxEllItem *) itemPtr;

    /* Scale all coordinates and set their related values */

    xc = originX + scaleX*(boxellPtr->centerX - originX);
    yc = originY + scaleY*(boxellPtr->centerY - originY);
    boxellPtr->coords[0] = xc;
    boxellPtr->coords[1] = yc;

    xmaj = originX + scaleX*(boxellPtr->semiMajorX - originX);
    ymaj = originY + scaleY*(boxellPtr->semiMajorY - originY);
    boxellPtr->coords[2] = xmaj;
    boxellPtr->coords[3] = ymaj;

    xmin = originX + scaleX*(boxellPtr->semiMinorX - originX);
    ymin = originY + scaleY*(boxellPtr->semiMinorY - originY);
    boxellPtr->coords[4] = xmin;
    boxellPtr->coords[5] = ymin;

    boxellPtr->newCoords = 6;
    ComputeBoxEllGeom(canvas, boxellPtr);
}


/*
 *--------------------------------------------------------------
 *
 * TranslateBoxEll --
 *
 *      This procedure is called to move an ellipse by a given amount.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The position of the ellipse is offset by (xDelta, yDelta), and
 *      the bounding box is updated in the generic part of the item structure.
 *
 *--------------------------------------------------------------
 */

static void
TranslateBoxEll(canvas, itemPtr, deltaX, deltaY)
    Tk_Canvas canvas;                     /* Canvas containing item. */
    Tk_Item *itemPtr;                     /* Item that is being moved. */
    double deltaX, deltaY;                /* Amount by which item is to be
                                           * moved. */
{
    BoxEllItem *boxellPtr = (BoxEllItem *) itemPtr;

    boxellPtr->coords[0] += deltaX;
    boxellPtr->coords[1] += deltaY;
    boxellPtr->coords[2] += deltaX;
    boxellPtr->coords[3] += deltaY;
    boxellPtr->coords[4] += deltaX;
    boxellPtr->coords[5] += deltaY;
    boxellPtr->newCoords = 6;
    ComputeBoxEllGeom(canvas, boxellPtr);
}

/*
 *--------------------------------------------------------------
 *
 * EllipseToPostscript --
 *
 *      This procedure is called to generate Postscript for
 *      an ellipse item.
 *
 * Results:
 *      The return value is a standard Tcl result.  If an error
 *      occurs in generating Postscript then an error message is
 *      left in interp->result, replacing whatever used to be there.
 *      If no error occurs, then Postscript for the rectangle is
 *      appended to the result.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static int
EllipseToPostscript(interp, canvas, itemPtr, prepass)
    Tcl_Interp *interp;                   /* Interpreter for error reporting. */
    Tk_Canvas canvas;                     /* Information about overall canvas. */
    Tk_Item *itemPtr;                     /* Item for which Postscript is
                                           * wanted. */
    int prepass;                          /* 1 means this is a prepass to
                                           * collect font information;  0 means
                                           * final Postscript is being created. */
{
    char pathCmd[500], string[100];
    BoxEllItem *boxellPtr = (BoxEllItem *) itemPtr;
    double centerY;

    centerY = Tk_CanvasPsY(canvas, boxellPtr->centerY);

    /*
     * Generate a string that creates a path for the ellipse.
     * May need to check absolute position.
     */

    sprintf(pathCmd, "matrix currentmatrix\n"
            "   %.15g %.15g translate \n"
            "   %.15g rotate \n"
            "   %.15g %.15g scale \n"
            "   1 0  moveto \n"
            "   0 0 1 0 360 arc\n"
            "setmatrix\n",
            boxellPtr->centerX, centerY,
            -boxellPtr->angle,
            boxellPtr->semiMajor, boxellPtr->semiMinor );

    /*
     * First draw the filled area of the ellipse.
     */

    if (boxellPtr->fillColor != NULL) {
        Tcl_AppendResult(interp, pathCmd, (char *) NULL);
        if (Tk_CanvasPsColor(interp, canvas, boxellPtr->fillColor)
            != TCL_OK) {
            return TCL_ERROR;
        }
        if (boxellPtr->fillStipple != None) {
            Tcl_AppendResult(interp, "clip ", (char *) NULL);
            if (Tk_CanvasPsStipple(interp, canvas, boxellPtr->fillStipple)
                != TCL_OK) {
                return TCL_ERROR;
            }
            if (boxellPtr->outlineColor != NULL) {
                Tcl_AppendResult(interp, "grestore gsave\n", (char *) NULL);
            }
        } else {
            Tcl_AppendResult(interp, "fill\n", (char *) NULL);
        }
    }

    /*
     * Now draw the outline, if there is one.
     */

    if (boxellPtr->outlineColor != NULL) {
        Tcl_AppendResult(interp, pathCmd, (char *) NULL);
        sprintf(string, "%d setlinewidth", boxellPtr->width);
        Tcl_AppendResult(interp, string,
                         " 0 setlinejoin 2 setlinecap\n", (char *) NULL);
        if (Tk_CanvasPsColor(interp, canvas, boxellPtr->outlineColor)
            != TCL_OK) {
            return TCL_ERROR;
        }
        Tcl_AppendResult(interp, "stroke\n", (char *) NULL);
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * RotBoxToPostscript --
 *
 *      This procedure is called to generate Postscript for
 *      a rotbox items.
 *
 * Results:
 *      The return value is a standard Tcl result.  If an error
 *      occurs in generating Postscript then an error message is
 *      left in interp->result, replacing whatever used
 *      to be there.  If no error occurs, then Postscript for the
 *      item is appended to the result.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static int
RotBoxToPostscript(interp, canvas, itemPtr, prepass)
    Tcl_Interp *interp;                 /* Leave Postscript or error message
                                         * here. */
    Tk_Canvas canvas;                   /* Information about overall canvas. */
    Tk_Item *itemPtr;                   /* Item for which Postscript is
                                         * wanted. */
    int prepass;                        /* 1 means this is a prepass to
                                         * collect font information;  0 means
                                         * final Postscript is being created. */
{
    char string[100];
    BoxEllItem *boxellPtr = (BoxEllItem *) itemPtr;
    XPoint points[5];
    double coords [10];
    int nPoints;
    int i, j;
    double centerY;

    centerY = Tk_CanvasPsY(canvas, boxellPtr->centerY);

    /* Generate the polygon points */
    nPoints = GenRotBox( points, boxellPtr->centerX, boxellPtr->centerY,
                         boxellPtr->semiMajor, boxellPtr->semiMinor,
                         boxellPtr->angle );

    /* Convert these to (double) from (short) */
    j = 0;
    for (i=0; i<nPoints ; i++) {
        coords[j] = (double)points[i].x;
        j++;
        coords[j] = (double)points[i].y;
        j++;
    }

    /*
     * Fill the area of the polygon.
     */

    if (boxellPtr->fillColor != NULL) {
        Tk_CanvasPsPath(interp, canvas, coords, nPoints);
        if (Tk_CanvasPsColor(interp, canvas, boxellPtr->fillColor) != TCL_OK) {
            return TCL_ERROR;
        }
        if (boxellPtr->fillStipple != None) {
            Tcl_AppendResult(interp, "eoclip ", (char *) NULL);
            if (Tk_CanvasPsStipple(interp, canvas, boxellPtr->fillStipple)
                != TCL_OK) {
                return TCL_ERROR;
            }
            if (boxellPtr->outlineColor != NULL) {
                Tcl_AppendResult(interp, "grestore gsave\n", (char *) NULL);
            }
        } else {
            Tcl_AppendResult(interp, "eofill\n", (char *) NULL);
        }
    }

    /*
     * Now draw the outline, if there is one.
     */

    if (boxellPtr->outlineColor != NULL) {
        Tk_CanvasPsPath(interp, canvas, coords, nPoints);
        sprintf(string, "%d setlinewidth\n", boxellPtr->width);
        Tcl_AppendResult(interp, string,
                         "1 setlinecap\n1 setlinejoin\n", (char *) NULL);
        if (Tk_CanvasPsColor(interp, canvas, boxellPtr->outlineColor)
            != TCL_OK) {
            return TCL_ERROR;
        }
        Tcl_AppendResult(interp, "stroke\n", (char *) NULL);
    }
    return TCL_OK;
}



/*
 *--------------------------------------------------------------
 *
 * GenEllipse --
 *
 *    Generates a list of coordinates that describe an ellipse
 *
 * Results:
 *      Returns the number of points generated.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static int
GenEllipse( XPoint *ellipse, double centerX, double centerY,
            double semiMajor, double semiMinor, double angle ) {
    int i;
    double step;
    double rpa;
    double cosRpa, sinRpa;
    double cosTheta, sinTheta;
    double theta;


    /* Step is the increment in angle about the centre of the ellipse.
     * theta is angle subtended from the locus to the centre of the
     * ellipse and is defined to be 0 or pi radians on the major axis.
     */
    step = PI * 2.0 / (double)( SEGMENTS - 1 );

    /* Convert angle to radians */
    rpa = angle * D2R;
    cosRpa = cos( rpa );
    sinRpa = sin( rpa );

    /* Define the start of ellipse as the positive end of the major axis.*/
    ellipse[0].x = centerX + semiMajor * cos( rpa );
    ellipse[0].y = centerY + semiMajor * sin( rpa );

    for ( i = 1; i < SEGMENTS; i++ ) {

        /* Get the orientation of the current locus point. */
        theta = step * (double) i;
        cosTheta = cos( theta );
        sinTheta = sin( theta );

        /* Compute product of the two tranformation matrices : rotation
         * by angle radians and parametisation of x,y coordinates to
         * semiMajor, semiMinor, theta.
         */
        ellipse[i].x = centerX + semiMajor * cosRpa * cosTheta -
                                 semiMinor * sinRpa * sinTheta;
        ellipse[i].y = centerY + semiMajor * sinRpa * cosTheta +
                                 semiMinor * cosRpa * sinTheta;
    }
    return SEGMENTS;
}


/*
 *--------------------------------------------------------------
 *
 * GenRotBox --
 *
 *    Generates a list of coordinates that describe a rotbox.
 *
 * Results:
 *      Returns the number of points generated.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static int
GenRotBox( XPoint *rotbox, double centerX, double centerY,
           double semiMajor, double semiMinor, double angle )
{
    double cosTheta, sinTheta;
    double x[4], y[4];

    /* Generate useful constants */
    cosTheta = cos( angle * D2R );
    sinTheta = sin( angle * D2R );

    /* Generate coordinates of centered, unrotated box */
    x[0] = x[3] = semiMajor;
    x[1] = x[2] = -semiMajor;
    y[0] = y[1] = semiMinor;
    y[2] = y[3] = -semiMinor;

    /* Now Apply rotation and offset */
    rotbox[0].x = (short)( centerX + ( x[0]*cosTheta ) - ( y[0]*sinTheta ));
    rotbox[0].y = (short)( centerY + ( x[0]*sinTheta ) + ( y[0]*cosTheta ));

    rotbox[1].x = (short)( centerX + ( x[1]*cosTheta ) - ( y[1]*sinTheta ));
    rotbox[1].y = (short)( centerY + ( x[1]*sinTheta ) + ( y[1]*cosTheta ));

    rotbox[2].x = (short)( centerX + ( x[2]*cosTheta ) - ( y[2]*sinTheta ));
    rotbox[2].y = (short)( centerY + ( x[2]*sinTheta ) + ( y[2]*cosTheta ));

    rotbox[3].x = (short)( centerX + ( x[3]*cosTheta ) - ( y[3]*sinTheta ));
    rotbox[3].y = (short)( centerY + ( x[3]*sinTheta ) + ( y[3]*cosTheta ));

    rotbox[4].x = rotbox[0].x;
    rotbox[4].y = rotbox[0].y;

    /*  Return number of points generated */
    return 5;
}


/*
 *--------------------------------------------------------------
 *
 *  ComputeBoxEllGeom --
 *
 *    Computes the geometry given certain changes.
 *
 * Results:
 *      None
 *
 * Side effects:
 *      Changes the item description.
 *
 *--------------------------------------------------------------
 */

static void
ComputeBoxEllGeom (canvas, boxellPtr)
    Tk_Canvas canvas;
    BoxEllItem *boxellPtr;
{
    Tk_ConfigSpec *p;
    double cosTheta, sinTheta;
    double xdiff, ydiff;
    double x, y;
    double xc, yc;

    /* If the coords are changed then update the necessary configuration
     * items to keep these in sync. Process config changes separately.
     * If we're processing coords then any configs are ignored.
     */
    if ( boxellPtr->newCoords ) {
        xc = boxellPtr->centerX = boxellPtr->coords[0];
        yc = boxellPtr->centerY = boxellPtr->coords[1];
        if ( boxellPtr->newCoords == 2 ) {

            /* No end points supplied, so create them from the existing
               information. */
            cosTheta = cos( boxellPtr->angle * D2R );
            sinTheta = sin( boxellPtr->angle * D2R );
            boxellPtr->coords[2] = boxellPtr->semiMajorX = xc + boxellPtr->semiMajor * cosTheta;
            boxellPtr->coords[3] = boxellPtr->semiMajorY = yc + boxellPtr->semiMajor * sinTheta;
            boxellPtr->coords[4] = boxellPtr->semiMinorX = xc - boxellPtr->semiMinor * sinTheta;
            boxellPtr->coords[5] = boxellPtr->semiMinorY = yc + boxellPtr->semiMinor * cosTheta;

        } else {
            /* Given end-points, so derive all other information about the
               object */
            boxellPtr->semiMajorX = boxellPtr->coords[2];
            boxellPtr->semiMajorY = boxellPtr->coords[3];
            boxellPtr->semiMinorX = boxellPtr->coords[4];
            boxellPtr->semiMinorY = boxellPtr->coords[5];

            xdiff = boxellPtr->semiMajorX - xc;
            ydiff = boxellPtr->semiMajorY - yc;
            boxellPtr->semiMajor = sqrt( xdiff * xdiff + ydiff * ydiff );
            boxellPtr->angle = atan2( ydiff, xdiff ) * R2D;

            xdiff = xc - boxellPtr->semiMinorX;
            ydiff = boxellPtr->semiMinorY - yc;
            boxellPtr->semiMinor = sqrt( xdiff * xdiff + ydiff * ydiff );
        }
        boxellPtr->newCoords = 0;

    } else {

        /* Now compute the semiMajor axis. This can change by a process of
         * rotation or by setting the position of the end point. Process
         * these in order minor-major-angle */

        if ( ( configSpecs[9].specFlags & TK_CONFIG_OPTION_SPECIFIED ) ||
             ( configSpecs[11].specFlags & TK_CONFIG_OPTION_SPECIFIED ) ) {
            /* Minor axis end point changed so recompute angle and axis length
             * and major end point */
            xc = boxellPtr->centerX;
            yc = boxellPtr->centerY;
            x = boxellPtr->semiMinorX;
            y = boxellPtr->semiMinorY;

            xdiff = xc -x;
            ydiff = y - yc;
            boxellPtr->semiMinor = sqrt( xdiff * xdiff + ydiff * ydiff );
            boxellPtr->angle = atan2( xdiff, ydiff ) *R2D;

            cosTheta = cos( boxellPtr->angle * D2R );
            sinTheta = sin( boxellPtr->angle * D2R );
            boxellPtr->semiMajorX = xc + boxellPtr->semiMajor * cosTheta;
            boxellPtr->semiMajorY = yc + boxellPtr->semiMajor * sinTheta;
        }

        if ( ( configSpecs[8].specFlags & TK_CONFIG_OPTION_SPECIFIED ) ||
             ( configSpecs[10].specFlags & TK_CONFIG_OPTION_SPECIFIED ) ) {

            /* Major axis end point changed so recompute angle and axis length
             * and minor end point */
            xc = boxellPtr->centerX;
            yc = boxellPtr->centerY;
            x = boxellPtr->semiMajorX;
            y = boxellPtr->semiMajorY;

            xdiff = x - xc;
            ydiff = y - yc;
            boxellPtr->semiMajor = sqrt( xdiff * xdiff + ydiff * ydiff );
            boxellPtr->angle = atan2( ydiff, xdiff ) *R2D;

            cosTheta = cos( boxellPtr->angle * D2R );
            sinTheta = sin( boxellPtr->angle * D2R );
            boxellPtr->semiMinorX = xc - boxellPtr->semiMinor * sinTheta;
            boxellPtr->semiMinorY = yc + boxellPtr->semiMinor * cosTheta;
        }

        if ( configSpecs[6].specFlags & TK_CONFIG_OPTION_SPECIFIED ) {

            /* Major axis changed in length. Recompute the end points. */
            xc = boxellPtr->centerX;
            yc = boxellPtr->centerY;
            cosTheta = cos( boxellPtr->angle * D2R );
            sinTheta = sin( boxellPtr->angle * D2R );
            boxellPtr->semiMajorX = xc + boxellPtr->semiMajor * cosTheta;
            boxellPtr->semiMajorY = yc + boxellPtr->semiMajor * sinTheta;
        }

        if ( configSpecs[7].specFlags & TK_CONFIG_OPTION_SPECIFIED ) {

            /* Minor axis changed in length. Recompute the end points. */
            xc = boxellPtr->centerX;
            yc = boxellPtr->centerY;
            cosTheta = cos( boxellPtr->angle * D2R );
            sinTheta = sin( boxellPtr->angle * D2R );
            boxellPtr->semiMinorX = xc - boxellPtr->semiMinor * sinTheta;
            boxellPtr->semiMinorY = yc + boxellPtr->semiMinor * cosTheta;
        }

        if ( configSpecs[5].specFlags & TK_CONFIG_OPTION_SPECIFIED ) {

            /* Angle changed so recompute axes end points */
            xc = boxellPtr->centerX;
            yc = boxellPtr->centerY;
            cosTheta = cos( boxellPtr->angle * D2R );
            sinTheta = sin( boxellPtr->angle * D2R );
            boxellPtr->semiMajorX = xc + boxellPtr->semiMajor * cosTheta;
            boxellPtr->semiMajorY = yc + boxellPtr->semiMajor * sinTheta;
            boxellPtr->semiMinorX = xc - boxellPtr->semiMinor * sinTheta;
            boxellPtr->semiMinorY = yc + boxellPtr->semiMinor * cosTheta;
        }

        /* Set the coords to the current values */
        boxellPtr->coords[0] = boxellPtr->centerX;
        boxellPtr->coords[1] = boxellPtr->centerY;
        boxellPtr->coords[2] = boxellPtr->semiMajorX;
        boxellPtr->coords[3] = boxellPtr->semiMajorY;
        boxellPtr->coords[4] = boxellPtr->semiMinorX;
        boxellPtr->coords[5] = boxellPtr->semiMinorY;

        /* Clear config flags */
        for ( p = configSpecs; p->type != TK_CONFIG_END; p++ ) {
            p->specFlags &= ~TK_CONFIG_OPTION_SPECIFIED;
        }
    }

    /* And recompute the bounding box. */
    ComputeBoxEllBbox( canvas, boxellPtr);
}
