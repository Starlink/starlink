/*
 *+
 *  Name:
 *     grf_tkcan.c

 *  Purpose:
 *     Implements an AST graphics interface to a Tk canvas.

 *  Description:
 *     This module defines a series of low level graphics functions
 *     needed by AST when drawing to a Tk canvas. The primitives are
 *     created by evaluating Tcl/Tk commands consisting of suitably
 *     customized standard and GAIA extended Tk canvas items.

 *  Implementation notes:
 *     - This module requires that the "rtd_mark", "rtd_segment" and
 *       "rtd_polyline" canvas items are available in the Tcl interpreter
 *       (these are distributed as part of GAIA and built into its standard
 *       wish). Also requires a rotatable text item (Tk 8.6 or backport).
 *
 *     - Before using any AST plotting routines you must invoke the
 *       "astTk_Init" function. This establishes the names of the Tcl
 *       interpreter and the canvas to be drawn into.
 *
 *     - You can control the canvas items created by this module
 *       by using the astTk_Tag routine. This allows you to set a new
 *       tag to associate with any elements created after your call.
 *       A default tag "ast_element" is always also associated with
 *       any graphics (allowing global control of all items).
 *       Note that the tag cannot be greater than 32 characters and
 *       is reset at each astTk_Init invocation to "".

 *  Copyright:
 *     Copyright (C) 1997-2004 Central Laboratory of the Research Councils
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     Copyright (C) 2007-2011 Science and Technology Facilities Council.
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
 *     PWD: Peter W. Draper (STARLINK - Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     1-AUG-1997 (PWD):
 *        Original version, based on grp_pgplot.c.
 *     18-NOV-1997 (PWD):
 *        Updated module for AST version 0.9.
 *     20-JUL-1998 (PWD):
 *        Changed to use a global tag as well as the one set by
 *        astTk_Tag.
 *     03-APR-2001 (PWD):
 *        Added facility to change the colour list.
 *     09-FEB-2004 (PWD):
 *        Added GCap and renamed GAxScale to GScales for AST 3.2 Grf mods.
 *     11-JUL-2009 (PWD):
 *        Removed use of rtd_word canvas text item. Use standard text item
 *        instead (requires Tk 8.6 or backport of code).
 *     28-JUL-2009 (PWD):
 *        Change the way that font scaling works. Broken by previous change.
 *        Remove the resizing concept, that is no longer applicable.
 *     11-MAR-2011 (PWD):
 *        Add astGBBuf and astGEBuf dummy routines for grf 5.6.
 *     {enter_changes_here}
 *-
 */

/* Macros */
/* ====== */
#define astCLASS                 /* Make AST protected functions available */

/* Header files. */
/* ============= */

#if HAVE_CONFIG_H
#include "config.h"
#endif

/* AST Interface definitions. */
/* -------------------------- */
#include "ast.h"                 /* AST functions and macros */
#include "grf.h"                 /* GRF Interface to this module */
#include "grf_tkcan.h"           /* Local interface to this module */

/* Local includes. */
/* --------------- */
#include "rtdCanvas.h"           /* Public interface to local rtd canvas
                                  * items */

/* Standard include files.*/
/* ----------------------- */
#include <string.h>
#include <math.h>
#include <float.h>
#include <stdlib.h>
#include "tcl.h"                 /* Tcl functions and definitions */

/* Constants. */
/* ========== */
#ifdef R2D
#undef R2D
#endif
#define R2D 57.29578             /* Radians to degrees factor */
#ifdef D2R
#undef D2R
#endif
#define D2R (1.0/57.29578)       /* Degrees to radians factor */

#define CMDLEN 512               /* Maximum length of Tcl message string */
#define TAGLEN 32                /* Maximum length of a canvas tag */
#define WIDLEN 132               /* Maximum length of widget name */
#define MAXSEG 132               /* Maximum number of segments in one
                                    canvas item */
enum {SEGMENTS, POLYLINE};       /* Type of lines drawn */
#define MAXCOLOURS 64            /* Maximum number of colours */


/* Local Macros */
/* ============ */
#define MAX(a,b) ( (a) > (b) ? (a) : (b) )
#define MIN(a,b) ( (a) < (b) ? (a) : (b) )

/* Function Prototypes. */
/* ==================== */
static int textBBox( double x, double y, const char *text,
                     const char *anchor, double angle, float *xb,
                     float *yb );

static int textAnchor( const char *just, char *anchor );

/* Static variables. */
/* ================= */
static Tcl_Interp *Interp = NULL;  /* Name of the Tcl interpreter */
static char Canvas[WIDLEN];        /* Name of Tk canvas */
static char Segment[TAGLEN];       /* Tk tag of the current line
                                    * segment item */
static int NewSegment = 1;         /* True when a new line segment is
                                    * needed (at first call to create
                                    * a line and after the default tag
                                    * is changed) */
static int Plotted = 0;            /* Number of segments plotted */
static int LineType = SEGMENTS;    /* Type of line to draw */

/* Structure to contain the current graphics configuration. */
typedef struct configInfo {
    double style;
    double width;
    double size;
    int font;
    int colour;
    char tag[TAGLEN+14];
    int smooth;
} configInfo;

static configInfo ConfigInfo;

/*  Define a list of possible fonts. Keep in sync with script values in
 *  gaia_defaults.tcl. Note these must be defined in points, not pixels
 *  otherwise they will not scale.
 *
 *  Historical note: tried named fonts which would be a better method, but the
 *  propagation of configuration changes through the whole application caused
 *  unwanted side effects.
 */
typedef struct fontInfo {
    const char *family;
    int size;
    const char *style;
} fontInfo;

static const fontInfo Fonts[] = {
    {"Helvetica",   8, "normal"},
    {"Helvetica",   8, "italic"},
    {"Helvetica",   8, "bold"},
    {"Helvetica",   8, "bold italic"},
    {"Helvetica",  10, "normal"},
    {"Helvetica",  10, "italic"},
    {"Helvetica",  10, "bold"},
    {"Helvetica",  10, "bold italic"},
    {"Times",       8, "normal"},
    {"Times",       8, "italic"},
    {"Times",       8, "bold"},
    {"Times",       8, "bold italic"},
    {"Courier",     8, "normal"},
    {"Courier",     8, "italic"},
    {"Courier",     8, "bold"},
    {"Courier",     8, "bold italic"},
    {"Helvetica",  18, "bold"},
};

/*  Define a list of possible colours in Tcl format (this is more or
 *  less the PGPLOT standard colourset).
 *
 *                                  R     G     B
 *     0   White                  1.00, 1.00, 1.00
 *     1   Black                  0.00, 0.00, 0.00
 *     2   Red                    1.00, 0.00, 0.00
 *     3   Green                  0.00, 1.00, 0.00
 *     4   Blue                   0.00, 0.00, 1.00
 *     5   Cyan (Green + Blue)    0.00, 1.00, 1.00
 *     6   Magenta (Red + Blue)   1.00, 0.00, 1.00
 *     7   Yellow  (Red + Green)  1.00, 1.00, 0.00
 *     8   Red + Yellow (Orange)  1.00, 0.50, 0.00
 *     9   Green + Yellow         0.50, 1.00, 0.00
 *    10   Green + Cyan           0.00, 1.00, 0.50
 *    11   Blue + Cyan            0.00, 0.50, 1.00
 *    12   Blue + Magenta         0.50, 0.00, 1.00
 *    13   Red + Magenta          1.00, 0.00, 0.50
 *    14   Dark Gray              0.33, 0.33, 0.33
 *    15   Light Gray             0.66, 0.66, 0.66
 */
static char *StandardColours[] = {
    "#fffffffff", "#000", "#f00", "#0f0", "#00f", "#0ff", "#f0f",
    "#ff0", "#f80", "#8f0", "#0f8", "#08f", "#80f", "#f08",
    "#512751275127", "#a8b4a8b4a8b4"
};

/*
 *  Define an extensible (up to MAXCOLOURS) list of indexed colours.
 */
static char *Colours[MAXCOLOURS];

/*
 *  The number of indexed colours initialised.
 */
static int numColours = 0;


/* External interface functions. */
/* ============================= */
/* These implement the "grf" interface in terms of Tk canvas
   primitives and add the necessary initialisation functions. */

int astTk_Init( Tcl_Interp *theinterp, const char *thecanvas ) {
/*
 *+
 *  Name:
 *     astTk_Init

 *  Purpose:
 *     Establishes the Tcl interpreter and canvas that are used
 *     to draw the graphics.

 *  Synopsis:
 *     #include "grf_tkcan.h"
 *     int astTk_Init( Tcl_Interp *theinterp, const char *thecanvas )

 *  Description:
 *      This routine sets the name of the Tcl interpreter and the
 *      canvas that are used to draw and display the graphics. It
 *      should be invoked at least once before any attempts to draw
 *      graphics are made from AST.
 *
 *      This routine also sets the default graphics configuration.

 *  Parameters:
 *     Tcl_Interp *theinterp
 *        The Tcl interpreter that was used to create the canvas.
 *     const char *thecanvas
 *        The Tk name of the canvas (i.e. that returned by the
 *        "canvas" command). Note this can be changed later using the
 *        astTk_SetCanvas function.

 *  Returned Value:
 *     A value of 0 is returned if an error occurs, and 1 is returned
 *     otherwise.

 *-
 */
    /*  Record the names of the canvas and its interpreter */
    if ( ! astTk_SetCanvas( thecanvas ) ) {
        return 0;
    }

    if ( theinterp != NULL ) {
        Interp = theinterp;
    } else {
        astError( AST__GRFER, "NULL Tcl interpreter not allowed\n");
        return 0;
    }

    /*  Set the default configuration information. */
    ConfigInfo.style = 0.0;
    ConfigInfo.width = 1.0;
    ConfigInfo.size = 1.0;
    ConfigInfo.font = 0;
    ConfigInfo.colour = 0;
    ConfigInfo.smooth = 0;
    (void) strcpy( ConfigInfo.tag, "ast_element " );

    /*  Set the default colormap (this can be overwritten and
        extended, but is only initialised once) */
    astTk_InitColours();

    /*  Need a line segment item. */
    NewSegment = 1;
    Segment[0] = '\0';

    /*  Return here if all O.K. */
    return 1;
}

/*
 *+
 *  Name:
 *     astTk_SetCanvas

 *  Purpose:
 *     Re-establishes the canvas used to draw the graphics.

 *  Synopsis:
 *     #include "grf_tkcan.h"
 *     int astTk_SetCanvas( const char *thecanvas )

 *  Description:
 *      This routine sets the name of the canvas used to draw and
 *      display the graphics. It may be invoked after astTk_Init
 *      to change the canvas used to draw graphics.

 *  Parameters:
 *     const char *thecanvas
 *        The Tk name of the canvas (i.e. that returned by the
 *        "canvas" command).

 *  Returned Value:
 *     A value of 0 is returned if an error occurs, and 1 is returned
 *     otherwise.

 *-
 */
int astTk_SetCanvas( const char *thecanvas )
{
    if ( thecanvas == NULL ) {
        astError( AST__GRFER, "NULL canvas name not allowed\n");
        return 0;
    }
    (void) strncpy( Canvas, thecanvas, WIDLEN - 1 );
    return 1;
}


void astTk_Tag( const char *newtag ) {
/*
 *+
 *  Name:
 *     astTk_Tag

 *  Purpose:
 *     Sets the canvas tag to be associated with any new canvas items.

 *  Synopsis:
 *     include "grf_tkcan.h"
 *     void astTk_Tag( const char *newtag )

 *  Description:
 *     This routine modifies the string used to tag any new canvas
 *     items. This facility is intended to be used to identify any
 *     elements associated with specific calls to AST routines (such as
 *     astGrid) so that they can be subsequently modified (or deleted
 *     as a whole). It is therefore important that the tag is reset at
 *     appropriate times (by setting the tag to NULL) so that further
 *     canvas items are not mis-identified.
 *
 *     Note that in addition to this tag a default tag "ast_element"
 *     is always available.

 *  Parameters:
 *     const char *newtag
 *        The tag to be associated with any canvas items created from
 *        this point onwards. If set to NULL then the tag will be
 *        reset to the default value "ast_element" only.

 *-
 */
    char *str = ConfigInfo.tag;
    if ( newtag != NULL ) {
        str += 12;
        (void) strncpy( str, newtag, TAGLEN - 1 );
    } else {
        (void) strcpy( ConfigInfo.tag, "ast_element " );
    }

    /*  Need a line segment item. */
    NewSegment = 1;
}

void astTk_InitColours() {
/*
 *+
 *  Name:
 *     astTk_InitColours

 *  Purpose:
 *     Initialise the standard colours.

 *  Synopsis:
 *     include "grf_tkcan.h"
 *     int astTk_InitColours()

 *  Description:
 *     If not already done so initialise the standard colours and set the
 *     extra colours to white. Can be called safely at any time.

 *-
 */
    int i;
    if ( numColours == 0 ) {
        numColours = sizeof( StandardColours ) / sizeof( char * );
        for ( i = 0; i < numColours; i++ ) {
            Colours[i] = NULL;
            (void) astTk_AddColour( i, StandardColours[i] );
        }

        /*  Extra colours are white */
        for ( i = numColours; i < MAXCOLOURS; i++ ) {
            Colours[i] = NULL;
            (void) astTk_AddColour( i, StandardColours[0] );
        }
        numColours = MAXCOLOURS;
    }
}

void astTk_AddColour( const int index, const char *colour ) {
/*
 *+
 *  Name:
 *     astTk_AddColour

 *  Purpose:
 *     Add an indexed Tcl colour to the colour list.

 *  Synopsis:
 *     include "grf_tkcan.h"
 *     int astTk_AddColour( const int index, const char *colour )

 *  Description:
 *     This routine makes a new colour available to the GRF
 *     interface. The colour is specified by a Tcl string (acceptable
 *     to Tcl_Color, usually a hexidecimal string) and an index for
 *     the colour.
 *
 *     By default a list of 16 colours are made available. These may
 *     be overwritten by required. Gaps in an index sequence are set
 *     to the default colour.
 *
 *     The maximum number of colours is MAXCOLOURS.
 *
 *     Failure is silent.

 *  Parameters:
 *     const char *colour
 *        The Tcl colour to be added.

 *-
 */
    if ( index < MAXCOLOURS ) {
        if ( Colours[index] != NULL ) {
            free( Colours[index] );
        }
        Colours[index] = (char *) strdup( colour );
    }
}

void astTk_LineType( int segments, int smooth ) {
/*
 *+
 *  Name:
 *     astTk_LineType

 *  Purpose:
 *     Sets line type used to draw graphics.

 *  Synopsis:
 *     include "grf_tkcan.h"
 *     void astTk_LineType( int segments )

 *  Description:
 *     This routine sets the sort of canvas graphics to use when
 *     drawing lines. These are line segments by default (i.e. every
 *     polyline is made up of abutted straight-lines), but may be
 *     switched to polylines. Which to use depends on the type of line
 *     graphics being drawn, lots of short lines are best represented
 *     using line segments (which is optimised so handle many of these
 *     per canvas item) and long lines by polylines (which look better
 *     when scaled).
 *
 *     If a polyline is drawn then the extra argument smooth may be
 *     used to determine if bsplines are used to smooth out the lines.
 *     Normally this is set to false.

 *  Parameters:
 *     int segment
 *        Which type of canvas lines to draw, 1 for segments and 0 for
 *        polylines.
 *
 *     int smooth
 *        Whether polylines are to be drawn with smoothing or not. Has
 *        no effect for line-segments.
 *-
 */
    ConfigInfo.smooth = smooth;

    if ( segments ) {
        LineType = SEGMENTS;
    } else {
        LineType = POLYLINE;
    }
}

int astGFlush( void ){
/*
 *+
 *  Name:
 *     astGFlush

 *  Purpose:
 *     Flush all pending graphics to the output device.

 *  Synopsis:
 *     #include "grf.h"
 *     int astGFlush( void )

 *  Description:
 *     This function ensures that the display device is up-to-date,
 *     by flushing any pending graphics to the output device.

 *  Parameters:
 *     None.

 *  Returned Value:
 *     A value of 0 is returned if an error occurs, and 1 is returned
 *     otherwise.

 *-
 */
    if ( Interp == NULL ) {
        astError( AST__GRFER, "astGFlush: Tk graphics system not initialised\n");
        return 0;
    }
    if ( Tcl_Eval( Interp, "update idletasks\n" ) == TCL_OK ) {
        return 1;
    } else {
        astError( AST__GRFER, "astGFlush: Failed to flush graphics (%s)\n",
                  Tcl_GetStringResult( Interp ) );
        return 0;
    }
}

int astGLine( int n, const float *x, const float *y ) {
/*
 *+
 *  Name:
 *     astGLine

 *  Purpose:
 *     Draw a polyline.

 *  Synopsis:
 *     #include "grf.h"
 *     int astGLine( int n, const float *x, const float *y )

 * Description:
 *     This function displays a series of lines that join the
 *     given positions creating a single "polyline".
 *
 *     There are two different types of line that can be drawn, either
 *     line segments, or a single polyline.
 *
 *     The segments option allows the creation of the line graphics as
 *     part of the same canvas "rtd_segment" item (including lines
 *     from subsequent calls, until either the graphics configuration
 *     or canvas tag is changed, or a limit for the number of segments
 *     per item is exceeded). This is so that when global canvas
 *     orientation changes are necessary (such as when tracking image
 *     rescaling, flipping etc.) as many of the lines as possible are
 *     dealt with as one item, so much of the work is performed in C,
 *     rather than Tcl.
 *
 *     The polyline option is useful when drawing long continous lines
 *     as a segmented line shows joins when scaled.

 *  Parameters:
 *     int n
 *        The number of positions to be joined together.
 *     const float *x
 *        Pointer to an array of "n" x values.
 *     const float *y
 *        Pointer to an array of "n" y values.

 *  Returned Value:
 *     A value of 0 is returned if an error occurs, and 1 is returned
 *     otherwise.

 *  Notes:
 *     -  Nothing is done if "n" is less than 2, or if a NULL pointer is
 *        given for either "x" or "y".

 *- */

    /* Local variables. */
    static char coords[] = "0.0 0.0 0.0 0.0";  /*  Dummy coords for creation command */
    char buffer[CMDLEN];
    double *xlines;
    double *ylines;
    int npoints;
    int i;
    int j;

    if ( Interp == NULL ) {
        astError( AST__GRFER, "astGLine: Tk graphics system not initialised\n");
        return 0;
    }

    /*  If we have some data points. */
    if( n > 1 && x && y ) {
        if ( LineType == SEGMENTS ) {

            /*  Use line segments */
            /*  ================= */

            /*  Convert input coordinates into a line-segment based format for
                passing to RtdSegmentSetCoords. */
            npoints = ( 2 * n ) - 2;
            xlines = malloc( sizeof( double ) * npoints );
            ylines = malloc( sizeof( double ) * npoints );
            for ( i = 0, j = 0; i < n - 1; i++, j += 2 ) {
                xlines[j]   = x[i];
                xlines[j+1] = x[i+1];
                ylines[j]   = y[i];
                ylines[j+1] = y[i+1];
            }

            /*  If using an existing segment then add these new values,
                otherwise create a new segment and record its tag. */
            if ( NewSegment ) {
                Plotted = 0;

                /*  Configure the command by adding the canvas name and all the
                    required options. */
                (void) sprintf( buffer, " -fill %s -width %f -tag {%s} -style %d\n",
                                Colours[ConfigInfo.colour], ConfigInfo.width,
                                ConfigInfo.tag, (int) ConfigInfo.style );
                if ( Tcl_VarEval( Interp, Canvas, " create rtd_segment ",
                                  coords, buffer, (char *) NULL ) != TCL_OK ) {

                    /*  Failed in creation attempt, so issue an error,
                        release the workspace and make sure that a new
                        segment item is created next time. */
                    astError( AST__GRFER,
                              "astGLine: failed to create line (%s)",
                              Tcl_GetStringResult( Interp ) );
                    free( xlines );
                    free( ylines );
                    NewSegment = 1;
                    return 0;

                } else {

                    /*  Now send coordinates (which may be a very long
                        list making it worth avoiding the conversion to
                        a string and back again) */
                    RtdSegmentSetCoords( Interp, 0, xlines, ylines, npoints );

                    /*  Record the name of the item we have just created. */
                    (void) strncpy( Segment, Tcl_GetStringResult( Interp ),
                                    TAGLEN - 1 );
                    NewSegment = 0;
                }
            } else {

                /*  Use the existing segment. Do a dummy command to make sure
                    context for new positions is correct */
                if ( Tcl_VarEval( Interp, Canvas, " coords ", Segment,
                                  " null ", coords, (char *) NULL )
                     != TCL_OK ) {

                    /*  Failed in creation attempt, so issue an error,
                        release the workspace and make sure that a new
                        segment item is created next time. */
                    astError( AST__GRFER,
                              "astGLine: failed to append line segments (%s)",
                              Tcl_GetStringResult( Interp ) );
                    free( xlines );
                    free( ylines );
                    NewSegment = 1;
                    return 0;
                } else {

                    /*  Append new coordinates */
                    RtdSegmentSetCoords( Interp, 1, xlines, ylines, npoints );
                }
            }
            free( xlines );
            free( ylines );

            /*  If number of segments exceeds the maximum number allowed per
                item, then start a new Segment next time */
            Plotted += n;
            if ( Plotted > MAXSEG ) {
                NewSegment = 1;
            }
        } else {

            /*  Use a single polyline. */
            /*  =====================  */

            /*  Convert input coordinates into doubles */
            xlines = malloc( sizeof( double ) * n );
            ylines = malloc( sizeof( double ) * n );
            for ( i = 0 ; i < n; i++ ) {
                xlines[i] = x[i];
                ylines[i] = y[i];
            }

            /*  Configure the command by adding the canvas name and all the
                required options. */
            (void) sprintf( buffer,
                            " -fill %s -width %f -tag {%s} -smooth %d -style %d\n",
                            Colours[ConfigInfo.colour], ConfigInfo.width,
                            ConfigInfo.tag, ConfigInfo.smooth,
                            (int) ConfigInfo.style );
            if ( Tcl_VarEval( Interp, Canvas, " create rtd_polyline ",
                              coords, buffer, (char *) NULL ) != TCL_OK ) {

                /*  Failed in creation attempt, so issue an error,
                    release the workspace and make sure that a new
                    segment item is created next time. */
                astError( AST__GRFER, "astGLine: failed to create line (%s)",
                          Tcl_GetStringResult( Interp ) );
                free( xlines );
                free( ylines );
                return 0;

            } else {

                /*  Now send coordinates (which may be a very long
                    list making it worth avoiding the conversion to a
                    string and back again) */
                RtdLineSetLastCoords( Interp, xlines, ylines, n );
            }
            free( xlines );
            free( ylines );
        }
    }
    return 1;
}

int astGMark( int n, const float *x, const float *y, int type ) {
/*
 *+
 *  Name:
 *     astGMark

 *  Purpose:
 *     Draw a set of markers.

 *  Synopsis:
 *     #include "grf.h"
 *     int astGMark( int n, const float *x, const float *y, int type )

 *  Description:
 *     This function displays markers at the given positions. The
 *     markers are drawn using the types available in the "rtd_mark"
 *     canvas item.

 *  Parameters:
 *     int n
 *        The number of markers to draw.
 *     const float *x
 *        Pointer to an array of "n" x values.
 *     const float *y
 *        Pointer to an array of "n" y values.
 *     int type
 *        An integer indicating the type of marker symbol required:
 *
 *          - 1 "dot"
 *          - 2 "cross"
 *          - 3 "plus"
 *          - 4 "square"
 *          - 5 "circle"
 *          - 6 "diamond"
 *          - 7 "triangle"
 *
 *        Any other value results in a circle.

 *  Returned Value:
 *     A value of 0 is returned if an error occurs, and 1 is returned
 *     otherwise.

 *  Notes:
 *     -  Nothing is done if "n" is less than 1, or if a NULL pointer is
 *        given for either "x" or "y".

 *-
 */

    /* Local variables */
    char *shape;
    char buffer[CMDLEN];
    int i;

    if ( Interp == NULL ) {
        astError( AST__GRFER,
                  "astGMark: Tk graphics system not initialised\n");
        return 0;
    }

    if( n > 0 && x && y ) {

        /* Convert marker type into one known to the rtd_mark item. If
           the value is out of range then the user gets a circle. */
        switch (type) {
        case 1:
            shape = "dot";
            break;
        case 2:
            shape = "cross";
            break;
        case 3:
            shape = "plus";
            break;
        case 4:
            shape = "square";
            break;
        case 5:
            shape = "circle";
            break;
        case 6:
            shape = "diamond";
            break;
        case 7:
            shape = "triangle";
            break;
        default:
            shape = "circle";
        }

        /* Fill the string buffer with the rtd_mark creation and
           configuration command. The evaluate it to create the canvas
           item. */
        for ( i = 0; i < n; i++ ) {
            (void) sprintf( buffer,
                            "%s create rtd_mark %f %f -type %s -size %d "
                            " -width %d -outline %s -fill %s -tag {%s}\n",
                            Canvas, (double) x[i], (double) y[i], shape,
                            (int) ConfigInfo.size, (int) ConfigInfo.width,
                            Colours[ConfigInfo.colour],
                            Colours[ConfigInfo.colour],
                            ConfigInfo.tag );
            if ( Tcl_Eval( Interp, buffer ) != TCL_OK ) {
                astError( AST__GRFER, "astGLine: failed to create mark (%s)",
                          Tcl_GetStringResult( Interp ) );
                return 0;
            }
        }
    }
    return 1;
}

int astGText( const char *text, float x, float y, const char *just,
              float upx, float upy ){
/*
 *+
 *  Name:
 *     astGText

 *  Purpose:
 *     Draw a character string.

 *  Synopsis:
 *     #include "grf.h"
 *     int astGText( const char *text, float x, float y, const char *just,
 *                   float upx, float upy )

 *  Description:
 *     This function displays a character string at a given position
 *     using a specified justification and up-vector (orientation).
 *
 *     The "text" canvas item is used so that the strings may be
 *     drawn at any angle.

 *  Parameters:
 *     const char *text
 *        Pointer to the string to be displayed.
 *     float x
 *        The reference x coordinate.
 *     float y
 *        The reference y coordinate.
 *     const char *just
 *        A character string which specifies the location within the
 *        text string which is to be placed at the reference position
 *        given by x and y. The first character may be 'T' for "top",
 *        'C' for "centre", or 'B' for "bottom", and specifies the
 *        vertical location of the reference position. Note, "bottom"
 *        corresponds to the base-line of normal text. Some characters
 *        (eg "y", "g", "p", etc) descend below the base-line. The second
 *        character may be 'L' for "left", 'C' for "centre", or 'R'
 *        for "right", and specifies the horizontal location of the
 *        reference position. If the string has less than 2 characters
 *        then 'C' is used for the missing characters.
 *     float upx
 *        The x component of the up-vector for the text, in graphics world
 *        coordinates. If necessary the supplied value should be negated
 *        to ensure that positive values always refer to displacements from
 *        left to right on the screen.
 *     float upy
 *        The y component of the up-vector for the text, in graphics world
 *        coordinates. If necessary the supplied value should be negated
 *        to ensure that positive values always refer to displacements from
 *        bottom to top on the screen.

 *  Returned Value:
 *     A value of 0 is returned if an error occurs, and 1 is returned
 *     otherwise.

 *  Notes:
 *     -  A NULL value for "just" causes a value of "CC" to be used.
 *     -  Both "upx" and "upy" being zero causes an error.
 *     -  Any unrecognised character in "just" causes an error.

 *-
 */

    /* Local Variables: */
    char anchor[3];
    char buffer[CMDLEN];
    float angle;
    fontInfo finfo;
    int size;

    if ( Interp == NULL ) {
        astError( AST__GRFER,
                  "astGText: Tk graphics system not initialised\n");
        return 0;
    }


    /* Check that there is something to draw. */
    if( text && text[ 0 ] != 0 ){

        /* Translate the justification string into a Tk anchor string. */
        if ( ! textAnchor( just, anchor ) ) {
            return 0;
        }

        /* Get the angle between the text base-line and horizontal. */
        angle = atan2( -(double) upx, (double) upy ) * R2D;

        /* Now display the text at the current size. */
        finfo = Fonts[ConfigInfo.font];
        size = (int)( (double)finfo.size * ConfigInfo.size );
        (void) sprintf ( buffer,
                         "%s create text %f %f -text {%s} -angle %f "
                         "-anchor %s -font {%s %d %s} -fill %s -tag {%s} \n",
                         Canvas, (double) x, (double) y, text, angle, anchor,
                         finfo.family, size, finfo.style,
                         Colours[ConfigInfo.colour], ConfigInfo.tag );

        if ( Tcl_Eval( Interp, buffer ) != TCL_OK ) {
            astError( AST__GRFER, "astGText: Failed to draw text." );
            return 0;
        }
    }

    /* Return. */
    return 1;
}

int astGTxExt( const char *text, float x, float y, const char *just,
               float upx, float upy, float *xb, float *yb ){
/*
 *+
 *  Name:
 *     astGTxExt

 *  Purpose:
 *     Get the extent of a character string.

 *  Synopsis:
 *     #include "grf.h"
 *     int astGTxExt( const char *text, float x, float y, const char *just,
 *                   float upx, float upy, float *xb, float *yb )

 *  Description:
 *     This function returns the corners of a box which would enclose the
 *     supplied character string if it were displayed using astGText.

 *  Parameters:
 *     const char *text
 *        Pointer to a null-terminated character string to be displayed.
 *     float x
 *        The reference x coordinate.
 *     float y
 *        The reference y coordinate.
 *     const char *just
 *        A character string which specifies the location within the
 *        text string which is to be placed at the reference position
 *        given by x and y. The first character may be 'T' for "top",
 *        'C' for "centre", or 'B' for "bottom", and specifies the
 *        vertical location of the reference position. The second
 *        character may be 'L' for "left", 'C' for "centre", or 'R'
 *        for "right", and specifies the horizontal location of the
 *        reference position. If the string has less than 2 characters
 *        then 'C' is used for the missing characters.
 *     float upx
 *        The x component of the up-vector for the text.
 *     float upy
 *        The y component of the up-vector for the text.
 *     float *xb
 *        An array of 4 elements in which to return the x coordinate of
 *        each corner of the bounding box.
 *     float *yb
 *        An array of 4 elements in which to return the y coordinate of
 *        each corner of the bounding box.

 *  Returned Value:
 *     A value of 0 is returned if an error occurs, and 1 is returned
 *     otherwise.

 *  Notes:
 *     -  A NULL value for "just" causes a value of "CC" to be used.
 *     -  Both "upx" and "upy" being zero causes an error.
 *     -  Any unrecognised character in "just" causes an error.
 *     -  Zero is returned for all bounds of the box if an error occurs.

 *-
 */

    /* Local Variables: */
    char anchor[3];
    float angle;
    int i;

    if ( Interp == NULL ) {
        astError( AST__GRFER,
                  "astGTxExt: Tk graphics system not initialised\n");
        return 0;
    }

    /* Initialise the returned values to indicate no box available. */
    for( i = 0; i < 4; i++ ){
        xb[i] = 0.0f;
        yb[i] = 0.0f;
    }

    /* Check that there is something to draw. */
    if( text && text[ 0 ] != 0 ){

        /* Translate the justification string into a Tk anchor string. */
        if ( ! textAnchor( just, anchor ) ) {
            return 0;
        }

        /* Get the angle between the text base-line and horizontal. */
        angle = atan2( -(double) upx, (double) upy ) * R2D;

        /* Now get the bounding box. */
        if ( ! textBBox ( (double) x, (double) y, text, anchor,
                          (double) angle, xb, yb ) ) {
            return 0;
        }
    }

    /* Return. */
    return 1;
}

int astGQch( float *chv, float *chh ){
/*
 *+
 *  Name:
 *     astGQch

 *  Purpose:
 *     Return the character height in world coordinates.

 *  Synopsis:
 *     #include "grf.h"
 *     int astGQch( float *chv, float *chh )

 *  Description:
 *     This function returns the height of characters drawn vertically and
 *     horizontally in world coordinates.

 *  Parameters:
 *     float *chv
 *        A pointer to the float which is to receive the height of
 *        characters drawn vertically. This will be an increment in the X
 *        axis.
 *     float *chh
 *        A pointer to the float which is to receive the height of
 *        characters drawn horizontally. This will be an increment in the Y
 *        axis.

 *  Returned Value:
 *     A value of 0 is returned if an error occurs, and 1 is returned
 *     otherwise.

 *-
 */

    /* Local Variables: */
    float xbox[4], ybox[4];

    if ( Interp == NULL ) {
        astError( AST__GRFER, "astGQch: Tk graphics system not initialised\n");
        return 0;
    }

    /* Get the bounding box for a horizontal text string with good up
       and down span. */
    if ( ! textBBox ( 0.0, 0.0, "ABCD", "c", 0.0, xbox, ybox ) ) {
        return 0;
    } else {
        *chh = ybox[3] - ybox[0];
        if ( *chh < 0 ) *chh = -(*chh);
    }

    /* Same for vertical text. */
    if ( ! textBBox ( 0.0, 0.0, "ABCD", "c", 90.0, xbox, ybox ) ) {
        return 0;
    } else {
        *chv = xbox[3] - xbox[0];
        if ( *chv < 0 ) *chv = -(*chv);
    }

    /* Return. */
    return 1;
}

int astGAttr( int attr, double value, double *old_value, int prim ){
/*
 *+
 *  Name:
 *     astGAttr

 *  Purpose:
 *     Enquire or set a graphics attribute value.

 *  Synopsis:
 *     #include "grf.h"
 *     int  astGAttr( int attr, double value, double *old_value, int prim )

 *  Description:
 *     This function returns the current value of a specified graphics
 *     attribute, and optionally establishes a new value. The supplied
 *     value is converted to an integer value if necessary.

 *  Parameters:
 *     int attr
 *        An integer value identifying the required attribute. The
 *        following symbolic values are defined in grf.h:
 *
 *           GRF__STYLE  - Line style.
 *           GRF__WIDTH  - Line width.
 *           GRF__SIZE   - Character and marker size scale factor.
 *           GRF__FONT   - Character font.
 *           GRF__COLOUR - Colour index.
 *     double value
 *        A new value to store for the attribute. If this is AST__BAD
 *        no value is stored.
 *     double *old_value
 *        A pointer to a double in which to return the attribute value.
 *        If this is NULL, no value is returned.
 *     int prim
 *        The sort of graphics primative to be drawn with the new attribute.
 *        Identified by the following values defined in grf.h:
 *           GRF__LINE
 *           GRF__MARK
 *           GRF__TEXT

 *  Returned Value:
 *     A value of 0 is returned if an error occurs, and 1 is returned
 *     otherwise.

 *  Notes:

 *-
 */
    int ival;
    double dval;

    if ( Interp == NULL ) {
        astError( AST__GRFER,
                  "astGAttr: Tk graphics system not initialised\n" );
        return 0;
    }

    /* If required retrieve the current line style, and set a new line
       style. This can be 0, 1, 2 or 3. */
    if ( attr == GRF__STYLE ) {
        if ( old_value ) *old_value = ConfigInfo.style;

        if ( value != AST__BAD ) {

            /* Need a new segment item if the value has changed. */
            if ( value != ConfigInfo.style ) {
                NewSegment = 1;
            }
            ConfigInfo.style = MAX( 0.0, MIN( value, 3.0 ) );
        }
    }
    else if ( attr == GRF__WIDTH ) {

        /* If required retrieve the current line width, and set a new
           line width.  Line width is scaled betwen 0.0 (minimum
           thickness) and 1.0 (maximum thickness). */
        if ( old_value ) *old_value = ConfigInfo.width / 200.0;

        if( value != AST__BAD ){
            dval = MAX( 0.0, MIN( 200.0, ( value * 200.0 ) ) );

            /* Need a new segment item if the value has changed. */
            if ( dval != ConfigInfo.width ) {
                NewSegment = 1;
            }
            ConfigInfo.width = MAX( 0.0, MIN( 200.0, ( value * 200.0 ) ) );
        }

    } else if( attr == GRF__SIZE ){

        /* If required retrieve the current character size, and set a
           new size.  The attribute value should be a factor by which
           to multiply the default character size. */
        if( old_value ) *old_value = ConfigInfo.size;

        if( value != AST__BAD ){
            ConfigInfo.size = value;
        }

    } else if ( attr == GRF__FONT ){

        /* If required retrieve the current character font, and set a
           new font. */
        if ( old_value ) *old_value = (double) ConfigInfo.font;

        if ( value != AST__BAD ) {
            int nfonts = (int) ( sizeof( Fonts ) / sizeof( fontInfo ) - 1);
            ConfigInfo.font = MAX( 0, MIN( nfonts, (int) value ) );
        }

    } else if( attr == GRF__COLOUR ){

        /* If required retrieve the current colour index, and set a
           new colour index. */
        if( old_value ) *old_value = (double) ConfigInfo.colour;

        if( value != AST__BAD ){
            ival = MAX( 0, MIN( MAXCOLOURS - 1, (int) value ) );

            /* Need a new segment item if the value has changed. */
            if ( ival != ConfigInfo.colour ) {
                NewSegment = 1;
            }
            ConfigInfo.colour = ival;
        }
    } else {

        /* Give an error message for any other attribute value. */
        astError( AST__GRFER, "astGAttr: Unknown graphics attribute '%d' "
                  "requested.", attr );
        return 0;
    }

    /* Return. */
    return 1;
}

int astGScales( float *alpha, float *beta ){
/*
 *+
 *  Name:
 *     astGScales

 *  Purpose:
 *     Get the axis scales.

 *  Synopsis:
 *     #include "grf.h"
 *     int astGScales( float *alpha, float *beta )

 *  Description:
 *     This function returns two values (one for each axis) which scale
 *     increments on the corresponding axis into a "normal" coordinate
 *     system in which:
 *        1 - The axes have equal scale in terms of (for instance)
 *            millimetres per unit distance.
 *        2 - X values increase from left to right.
 *        3 - Y values increase from bottom to top.

 *  Parameters:
 *     float *alpha
 *        A pointer to the location at which to return the scale for the
 *        X axis (i.e. Xnorm = alpha*Xworld).
 *     float *beta
 *        A pointer to the location at which to return the scale for the
 *        Y axis (i.e. Ynorm = beta*Yworld).

 *  Returned Value:
 *     A value of 0 is returned if an error occurs, and 1 is returned
 *     otherwise.

 *-
 */
    /*  For a Tk canvas the scales are required to be square and fixed, so we
     *  cannot work out better values than 1, -1 */
    *alpha = 1.0f;
    *beta = -1.0f;
    return 1;
}

int astGCap( int cap, int value ){
/*
 *+
 *  Name:
 *     astGCap

 *  Purpose:
 *     Indicate if this grf module has a given capability.

 *  Synopsis:
 *     #include "grf.h"
 *     int astGCap( int cap, int value )

 *  Description:
 *     This function is called by the AST Plot class to determine if the
 *     grf module has a given capability, as indicated by the "cap"
 *     argument.

 *  Parameters:
 *     cap
 *        The capability being inquired about. This will be one of the
 *        following constants defined in grf.h:
 *
 *        GRF__SCALES: This function should return a non-zero value if
 *        it implements the astGScales function, and zero otherwise. The
 *        supplied "value" argument should be ignored.
 *
 *        GRF__MJUST: This function should return a non-zero value if
 *        the astGText and astGTxExt functions recognise "M" as a
 *        character in the justification string. If the first character of
 *        a justification string is "M", then the text should be justified
 *        with the given reference point at the bottom of the bounding box.
 *        This is different to "B" justification, which requests that the
 *        reference point be put on the baseline of the text, since some
 *        characters hang down below the baseline. If the astGText or
 *        astGTxExt function cannot differentiate between "M" and "B",
 *        then this function should return zero, in which case "M"
 *        justification will never be requested by Plot. The supplied
 *        "value" argument should be ignored.
 *
 *        GRF__ESC: This function should return a non-zero value if the
 *        astGText and astGTxExt functions can recognise and interpret
 *        graphics escape sequences within the supplied string. These
 *        escape sequences are described below. Zero should be returned
 *        if escape sequences cannot be interpreted (in which case the
 *        Plot class will interpret them itself if needed). The supplied
 *        "value" argument should be ignored only if escape sequences cannot
 *        be interpreted by astGText and astGTxExt. Otherwise, "value"
 *        indicates whether astGText and astGTxExt should interpret escape
 *        sequences in subsequent calls. If "value" is non-zero then
 *        escape sequences should be interpreted by astGText and
 *        astGTxExt. Otherwise, they should be drawn as literal text.

 *  Returned Value:
 *     The return value, as described above. Zero should be returned if
 *     the supplied capability is not recognised.

 *  The escape sequences etc. are documented in AST SUN/211.

 *-
 */
    if ( cap == GRF__SCALES ) {
        return 1;
    }
    return 0;
}

/*  Local functions. */
/*  ================ */
static int textBBox( double x, double y, const char *text,
                     const char *anchor, double angle, float *xb,
                     float *yb ) {
/*
 *+
 *  Name:
 *     textBBox

 *  Purpose:
 *     Return the bounding box of the given text.

 *  Synopsis:
 *     int textBBox( double x, double y, char *text, char *anchor,
 *                   double angle, float *xb, float *yb )

 *  Description:
 *     This routine returns a bounding box as used by astGTxExt.
 *     This is achieved by creating a word with the necessary
 *     parameters and then querying its bounding box. The text is then
 *     deleted from the canvas before proceeding.

 *  Parameters:
 *     double x
 *        The text x position.
 *     double y
 *        The text y position.
 *     const char *text
 *        The text string.
 *     const char *anchor
 *        The Tk text anchor.
 *     double angle
 *        Postion angle of text.
 *     float *xb
 *        Pointer to an array of four floats. These will be set to the
 *        x bounding box positions on exit.
 *     float *yb
 *        Pointer to an array of four floats. These will be set to the
 *        y bounding box positions on exit.

 *   Return value:
 *      1 for success, 0 otherwise.

 */

    /*  Local Variables. */
    Tcl_Obj **lbbox;
    Tcl_Obj *res;
    Tk_Anchor tkanchor;
    char buffer[CMDLEN];
    double cosa;
    double dx = 0.0;
    double dy = 0.0;
    double sina;
    double xbd[4];
    double ybd[4];
    fontInfo finfo;
    int i;
    int nbbox;
    int size;

    /*  First display the text using the current configuration
        options, as well as the position and angle. Note we use an
        unlikely canvas tag to arrange control of the item. */
    finfo = Fonts[ConfigInfo.font];
    size = (int)( (double)finfo.size * ConfigInfo.size );
    (void) sprintf ( buffer, "%s create text %f %f -text {%s} "
                     "-anchor %s -font {%s %d %s} -tag grf_word_temp",
                     Canvas, x, y, text, anchor, finfo.family, size,
                     finfo.style );

    if ( Tcl_Eval( Interp, buffer ) == TCL_OK ) {

        /*  Now get the bounding box of the text and then remove it. */
        (void) sprintf ( buffer, "%s bbox %s \n", Canvas,
                         Tcl_GetStringResult( Interp ) );
        if ( Tcl_Eval( Interp, buffer ) == TCL_OK ) {

            /*  Get the unrotated bounding box and create the other corner
             * positions. */
            res = Tcl_GetObjResult( Interp );
            Tcl_ListObjGetElements( Interp, res, &nbbox, &lbbox );
            Tcl_GetDoubleFromObj( Interp, lbbox[0], &xbd[0] );
            Tcl_GetDoubleFromObj( Interp, lbbox[1], &ybd[0] );
            Tcl_GetDoubleFromObj( Interp, lbbox[2], &xbd[2] );
            Tcl_GetDoubleFromObj( Interp, lbbox[3], &ybd[2] );

            xbd[1] = xbd[2];
            ybd[1] = ybd[0];
            xbd[3] = xbd[0];
            ybd[3] = ybd[2];

            /*  Rotate text bounding box to get the real corners. Need to
             *  move to anchor position, rotate and then translate back. */
            Tk_GetAnchor( Interp, anchor, &tkanchor );
            switch ( tkanchor ) {
                case TK_ANCHOR_NW:
                case TK_ANCHOR_N:
                case TK_ANCHOR_NE:
                    dy = ybd[0];
                    break;

                case TK_ANCHOR_W:
                case TK_ANCHOR_CENTER:
                case TK_ANCHOR_E:
                    dy = 0.5 * ( ybd[2] + ybd[0] );
                    break;

                case TK_ANCHOR_SW:
                case TK_ANCHOR_S:
                case TK_ANCHOR_SE:
                    dy = ybd[1];
                    break;
            }

            switch ( tkanchor ) {
                case TK_ANCHOR_NW:
                case TK_ANCHOR_W:
                case TK_ANCHOR_SW:
                    dx = xbd[0];
                    break;

                case TK_ANCHOR_N:
                case TK_ANCHOR_CENTER:
                case TK_ANCHOR_S:
                    dx = 0.5 * ( xbd[1] + xbd[0] );
                    break;

                case TK_ANCHOR_NE:
                case TK_ANCHOR_E:
                case TK_ANCHOR_SE:
                    dx = xbd[1];
                    break;
            }


            cosa = cos( angle * D2R );
            sina = sin( angle * D2R );

            for( i = 0; i < 4; i++ ) {
                xb[i] = dx +
                    ( ( xbd[i] - dx ) * cosa ) +
                    ( ( ybd[i] - dy ) * sina );
                yb[i] = dy +
                    ( ( xbd[i] - dx ) * -sina ) +
                    ( ( ybd[i] - dy ) *  cosa );
            }

            /*  Remove the word. */
            (void) sprintf ( buffer, "%s delete grf_word_temp \n", Canvas );
            (void) Tcl_Eval( Interp, buffer );
        } else {
            (void) sprintf ( buffer, "%s delete grf_word_temp \n", Canvas );
            (void) Tcl_Eval( Interp, buffer );
            return 0;
        }
    }
    return 1;
}


static int textAnchor ( const char *just, char *anchor ) {

/*
 *+
 *  Name:
 *     textAnchor

 *  Purpose:
 *     Convert an AST justification string into a Tk anchor position.

 *  Synopsis:
 *      textAnchor ( const char *just, char *anchor )

 *  Description:
 *     This routine takes an AST justification string and converts it
 *     into the equivalent text anchor string.

 *  Parameters:
 *     const char *just
 *        The AST standard justification string.
 *     char *anchor
 *        Pointer to a string for the text anchor postion.

 *  Return value:
 *     1 for success, 0 otherwise.

 *  Notes:
 *     The arrangement is actually top-bottom, left-right flipped.


 */

    anchor[0] = ' ', anchor[1] = '\0', anchor[2] = '\0';
    if( just ) {

        switch ( just[0] ) {
            case 'T':
                anchor[0] = 'n'; break;
            case 'C':
                anchor[0] = 'c'; break;
            case 'B':
                anchor[0] = 's'; break;
        }

        switch ( just[1] ) {
            case 'L':
                if ( anchor[0] == 'c' ) {
                    anchor[0] = 'w';
                } else {
                    anchor[1] = 'w';
                }
                break;
            case 'R':
                if ( anchor[0] == 'c' ) {
                    anchor[0] = 'e';
                } else {
                anchor[1] = 'e';
                }
                break;
            case 'C':
                break;
            default: {
                astError( AST__GRFER, "astGText: Justification string '%s' is "
                          "invalid.", just );
                return 0;
            }

        }
    } else {

        /*  NULL string equates to a center anchor. */
        anchor[0] = 'c';
    }

    return 1;
}


/*  Graphics buffering is not supported. */
int astGBBuf( void )
{
    astError( AST__GRFER, "astGBBuf: is not supported" );
    return 0;
}

int astGEBuf( void )
{
    astError( AST__GRFER, "astGEBuf: is not supported" );
    return 0;
}

