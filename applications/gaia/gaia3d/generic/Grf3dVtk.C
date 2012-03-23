/*
 *  Name:
 *     Grf3dVtk.C

 *  Purpose:
 *     Implement the grf3d_gaia interface for GAIA3D VTK.

 *  Description:

 *  Copyright:
 *     Copyright (C) 2007 Science & Technology Facilities Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public Licence as
 *     published by the Free Software Foundation; either version 2 of
 *     the Licence, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful,but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public Licence for more details.
 *
 *     You should have received a copy of the GNU General Public Licence
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  Authors:
 *     DSB: David S. Berry (JAC, UCLancs)
 *     PWD: Peter W. Draper (JAC, Durham University)

 *  History:
 *     13-JUL-2007 (PWD):
 *        Original version, based on grf3d_pgplot.c.
 */

/* Macros */
/* ====== */
#define MAX(a,b) ( (a) > (b) ? (a) : (b) )
#define MIN(a,b) ( (a) < (b) ? (a) : (b) )
#define TWOPI 6.28318530718      /* 2*PI */

#define MAXCOLOURS 64
#define MAXTYPES 10
#define MAXSTIPPLES 4
#define TEXTFUDGE 1.4

/* Header files. */
/* ============= */

#if HAVE_CONFIG_H
#include "config.h"
#endif

/* AST Interface definitions. */
/* -------------------------- */
extern "C" {
#include "ast.h"                 /* AST functions and macros */
#include "grf3d_gaia.h"          /* GAIA GRF3D Interface */
#include "grf.h"
}

/* System header files */
#include <iostream>
#include <cmath>
#include <cfloat>
#include <cstdlib>

#include <vtk/vtkActor.h>
#include <vtk/vtkCamera.h>
#include <vtk/vtkCellType.h>
#include <vtk/vtkDataSet.h>
#include <vtk/vtkDataSetMapper.h>
#include <vtk/vtkDoubleArray.h>
#include <vtk/vtkFollower.h>
#include <vtk/vtkIdList.h>
#include <vtk/vtkLookupTable.h>
#include <vtk/vtkMath.h>
#include <vtk/vtkMatrix4x4.h>
#include <vtk/vtkPointData.h>
#include <vtk/vtkPolyData.h>
#include <vtk/vtkPolyDataMapper.h>
#include <vtk/vtkProgrammableGlyphFilter.h>
#include <vtk/vtkPropAssembly.h>
#include <vtk/vtkPropCollection.h>
#include <vtk/vtkProperty.h>
#include <vtk/vtkRenderer.h>
#include <vtk/vtkSuperquadricSource.h>
#include <vtk/vtkTransform.h>
#include <vtk/vtkVectorText.h>

#include <vtk/vtkDataSetAlgorithm.h>

/* Function templates. */
/* =================== */

#include "Grf3dVtk.h"

static void ClearAllObjects();
static void ClearPolyData();
static void CreatePolyObjects();
static void CreateMarkerObjects();
static vtkFollower *CreateVectorText( const char *text, float ref[3],
                                      const char *just, float up[3],
                                      float norm[3] );
static void TextOrientation( float ref[3], float up[3], float norm[3],
                             const char*just, double orient[3],
                             char newjust[3], float rx[3], float ry[3],
                             float rz[3] );

static void Grf3dMarkerGlyphFactory( void *arg );

static int vtkG3DAttr( int, double, double *, int );
static int vtkG3DCap( int, int );
static int vtkG3DFlush( void );
static int vtkG3DLine( int, float *, float *, float * );
static int vtkG3DMark( int, float *, float *, float *, int, float[3] );
static int vtkG3DQch( float * );
static int vtkG3DText( const char *, float[3], const char *, float[3],
                       float[3] );
static int vtkG3DTxExt( const char *, float[3], const char *, float[3],
                        float[3], float *, float *, float *, float[3] );


/* Static variables. */
/* ================= */

/* Structure to contain the current graphics configuration. */
typedef struct ConfigInfo {
    int style;
    double width;
    double size;
    int font;
    int colour;
} ConfigInfo;

/* Graphics context structure for an AST Plot and renderer. */
typedef struct GC {
    /* These are the important parts for the context. The other items are
     * volatile. */
    vtkRenderer *renderer;     // The renderer
    vtkPropAssembly *assembly; // All the actors in this context

    /* AST context for lines, widths, colours. */
    ConfigInfo configInfo;     // AST config

    /* Polylines. These are kept in instance of vtkPolyData, new instances are
     * recreated as the properties (width & colour) change. */
    vtkPolyData *polyData;     // Polylines
    vtkPoints *polyPoints;     // The polyData points
    int currentPolyDataId;     // Id of the last point added

    int lookAtCamera;          // Text looks at camera. XXX doesn't work well
    double textScale;          // Scale of text

    /* Text. Only one font for vtkVectorText, so nothing defined. */

    /* Markers. These are based on a vtkProgrammableGlyphFilter, which has
     * various vtkSuperquadricSource shapes generated to create things with
     * differing shapes. The size and type of the shape are stored in one
     * tuple, and another is used to provide scalars that map the colours via
     * the lookup table. */
    vtkProgrammableGlyphFilter *markerGlyph; // Programmable glyph filter
    vtkPoints *markerPoints;                 // Positions of markers
    vtkIntArray *markerColours;              // Marker colours set using
                                             // scalars that map via lookup
                                             // table
    vtkDoubleArray *markerInfo;              // Marker state information, type
                                             // and size
    int currentMarkerId;                     // Id of the last marker point
    vtkSuperquadricSource *markerSquad;      // Source of marker shapes
} GC;

/*  The current graphics context (must use this method as GRF interface has no
 *  context support. */
static GC *currentGC = NULL;

/*  Define a list of preset stipples for line styles. These are 16-bit binary
 *  patterns and will only work for OpenGL. */
static int StandardStipples[] = {
    0xFFFF,                       /* none          */
    0x0101,                       /* dotted        */
    0x00FF,                       /* dashed        */
    0x1C47                        /* dash/dot/dash */
};

/*  Colours. Define a list of possible colours (this is more or less the
 *  PGPLOT standard colourset).
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
static double StandardColours[][3] = {
    {1.00, 1.00, 1.00},
    {0.00, 0.00, 0.00},
    {1.00, 0.00, 0.00},
    {0.00, 1.00, 0.00},
    {0.00, 0.00, 1.00},
    {0.00, 1.00, 1.00},
    {1.00, 0.00, 1.00},
    {1.00, 1.00, 0.00},
    {1.00, 0.50, 0.00},
    {0.50, 1.00, 0.00},
    {0.00, 1.00, 0.50},
    {0.00, 0.50, 1.00},
    {0.50, 0.00, 1.00},
    {1.00, 0.00, 0.50},
    {0.33, 0.33, 0.33},
    {0.66, 0.66, 0.66}
};

/*  The standard and extra colours as a lookup table. */
static vtkLookupTable *lookupTable = NULL;

/*  Configuration values for vtkSuperquadricSource to generate different
 *  shaped markers. First two are sphere and cube, the rest various until
 *  the repeats when they become toroidal. */
static double markerSettings[MAXTYPES] = {
    0.0, 1.0, 0.5, 2.0, 5.0, 0.0, 1.0, 0.5, 2.0, 5.0
};

/*  Set the local functions as the Grf3D interface, or not. */
static int grfRegistered = 0;

/* ================
 * Public interface
 * ================ */

/*
 *+
 *  Name:
 *     Grf3dVtk_Init

 *  Purpose:
 *     Initialize this interface with a renderer. This function must be
 *     called before any other function. Returns a context pointer that can be
 *     re-established with a call to Grf3dVtk_SetContext.
 *-
 */
void *Grf3dVtk_Init( vtkRenderer *r, int lac )
{
    /* Create a Graphics context. */
    GC *gc = new GC;

    /* Initialisation. */
    gc->renderer = r;
    gc->lookAtCamera = lac;
    gc->assembly = vtkPropAssembly::New();

    gc->polyData = NULL;
    gc->polyPoints = NULL;
    gc->currentPolyDataId = 0;

    gc->textScale = 5.0;

    gc->markerGlyph = NULL;
    gc->markerPoints = NULL;
    gc->markerColours = NULL;
    gc->markerInfo = NULL;
    gc->currentMarkerId = 0;
    gc->markerSquad = NULL;

    gc->configInfo.style = 0;
    gc->configInfo.width = 1.0;
    gc->configInfo.size = 1.0;
    gc->configInfo.font = 0;
    gc->configInfo.colour = 1;

    /*  This becomes the current GC. */
    Grf3dVtk_SetContext( gc );

    /*  Get GAIA to use the local functions as the Grf3D interface (done
     *  once). */
    if ( ! grfRegistered ) {
        Grf3d_Register( (Grf3DCapFun) &vtkG3DCap,
                        (Grf3DFlushFun) &vtkG3DFlush,
                        (Grf3DLineFun) &vtkG3DLine,
                        (GrfG3DQchFun) &vtkG3DQch,
                        (Grf3DMarkFun) &vtkG3DMark,
                        (Grf3DTextFun) &vtkG3DText,
                        (Grf3DTxExtFun) &vtkG3DTxExt,
                        (Grf3DAttrFun) &vtkG3DAttr );
        grfRegistered = 1;
    }

    /*  Set the default colormap (this can be overwritten and extended, but is
     *  only initialised once and shared with all contexts). */
    Grf3dVtk_InitColours();

    /*  Return the context. */
    return (void *) gc;
}

/*
 *+
 *  Name:
 *     Grf3dVtk_SetContext

 *  Purpose:
 *     Set the graphics context to one created by the Grf3dVtk_Init procedure.
 *     Used to switch between renderers that are concurrently drawing AST
 *     graphics.

 *  Synopsis:
 *     int Grf3dVtk_SetContext( void *gc )

 *-
 */
void Grf3dVtk_SetContext( void *gc )
{
    currentGC = (GC *) gc;
}

/*
 *+
 *  Name:
 *     Grf3dVtk_FreeContext

 *  Purpose:
 *     Free a graphics context created by the Grf3dVtk_Init procedure.
 *     Use to release all resources. Note just Clear if the context is
 *     to be reused.

 *  Synopsis:
 *     int Grf3dVtk_FreeContext( void *gc )

 *-
 */
void Grf3dVtk_FreeContext( void *gc )
{
    //  Switch contexts for clear then reset.
    GC *activeGC = currentGC;
    currentGC = (GC *) gc;
    ClearAllObjects();
    currentGC = activeGC;

    //  Release the context structure.
    delete (GC *) gc;
}

/*
 *+
 *  Name:
 *     Grf3dVtk_Clear

 *  Purpose:
 *     Clear all the props, mappers and data sources for the current context.
 *     and ready for new graphics.

 *  Synopsis:
 *     int Grf3dVtk_Clear()

 *-
 */
void Grf3dVtk_Clear()
{
    if ( currentGC == NULL ) {
        return;
    }
    ClearAllObjects();

    /*  Assembly gathers all props for lines, text and marker elements for new
     *  graphics. */
    currentGC->assembly = vtkPropAssembly::New();
}

/*
 *+
 *  Name:
 *     Grf3dVtk_InitColours

 *  Purpose:
 *     Initialise the default colourmap, if needed.

 *  Synopsis:
 *     void Grf3dVtk_InitColours()

 *  Description:
 *     Set the default colormap, which can be overwritten and extended, but is
 *     only initialised once and shared with all contexts.
 *
 *     Note also resets additional colours to white.

 *-
 */
void Grf3dVtk_InitColours()
{
    if ( lookupTable == NULL ) {

        /*  Create a lookup table for managing colours. */
        lookupTable = vtkLookupTable::New();
        lookupTable->SetNumberOfColors( MAXCOLOURS );
        lookupTable->Build();

        /*  Add standard colours. */
        int numColours = sizeof(StandardColours) / sizeof(*StandardColours);
        for ( int i = 0; i < numColours; i++ ) {
            Grf3dVtk_AddColour( i, StandardColours[i][0],
                                StandardColours[i][1],
                                StandardColours[i][2] );
        }

        /*  Extra colours are white */
        for ( int i = numColours; i < MAXCOLOURS; i++ ) {
            Grf3dVtk_AddColour( i, StandardColours[0][0],
                                StandardColours[0][1],
                                StandardColours[0][2] );
        }
    }
}

/*
 *+
 *  Name:
 *     Grf3dVtk_AddColour

 *  Purpose:
 *     Add an indexed colour to the global colour list.

 *  Synopsis:
 *     void Grf3dVtk_AddColour( const int index, double r, double g, double b )

 *  Description:
 *     This routine makes a new colour available to the GRF3D interface for
 *     use in all contexts. The colour is specified by three RGB values
 *     (0 to 1) and an index for the colour.
 *
 *     By default a list of 16 colours are made available. These may be
 *     overwritten by required. Gaps in an index sequence are set to the
 *     default colour.
 *
 *     The maximum number of colours is MAXCOLOURS.
 *
 *     Failure is silent.

 *-
 */
void Grf3dVtk_AddColour( int index, double r, double g, double b )
{
    if ( index < MAXCOLOURS ) {
        lookupTable->SetTableValue( index, r, g, b );
    }
}

/*
 *+
 *  Name:
 *     Grf3dVtk_SetTextScale

 *  Purpose:
 *     Set the scale used when drawing text in the current context.

 *  Synopsis:
 *     void Grf3dVtk_SetTextScale( double textScale )

 *  Description:
 *     Set the scale used for drawing text. By default the scale
 *     is 5.0 units.

 *-
 */
void Grf3dVtk_SetTextScale( double s )
{
    currentGC->textScale = MAX( fabs( s ), 0.0001 );
}

/* ====================================================
 * Public functions defined by the grf3d_gaia interface.
 * ==================================================== */

/*
 *+
 *  Name:
 *     vtkG3DAttr

 *  Purpose:
 *     Enquire or set a 3D graphics attribute value.

 *  Synopsis:
 *     #include "grf3d.h"
 *     int int vtkG3DAttr( int attr, double value, double *old_value,
 *                         int prim )

 *  Description:
 *     This function returns the current value of a specified 3D graphics
 *     attribute, and optionally establishes a new value. The supplied
 *     value is converted to an integer value if necessary before use.

 *  Parameters:
 *     attr
 *        An integer value identifying the required attribute. The
 *        following symbolic values are defined in grf3d.h:
 *
 *           GRF__STYLE  - Line style.
 *           GRF__WIDTH  - Line width.
 *           GRF__SIZE   - Character and marker size scale factor.
 *           GRF__FONT   - Character font.
 *           GRF__COLOUR - Colour index.
 *     value
 *        A new value to store for the attribute. If this is AST__BAD
 *        no value is stored.
 *     old_value
 *        A pointer to a double in which to return the attribute value.
 *        If this is NULL, no value is returned.
 *     prim
 *        The sort of graphics primitive to be drawn with the new attribute.
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
static int vtkG3DAttr( int attr, double value, double *old_value, int prim )
{
    int ival;
    double dval;

    /* If required retrieve the current line style, and set a new line
       style. */
    if ( attr == GRF__STYLE ) {
        if ( old_value ) *old_value = currentGC->configInfo.style;
        if ( value != AST__BAD ) {
            ival = (int) MAX( 0, MIN( value, MAXSTIPPLES ) );
            if ( ival != currentGC->configInfo.style ) {
                ClearPolyData();
            }
            currentGC->configInfo.style = ival;
        }
    }
    else if ( attr == GRF__WIDTH ) {

        /* If required retrieve the current line width, and set a new
           line width.  Line width is scaled betwen 0.0 (minimum
           thickness = 1) and 1.0 (maximum thickness = 200). */
        if ( old_value ) *old_value = currentGC->configInfo.width / 200.0;
        if ( value != AST__BAD ){
            dval = MAX( 0.0, MIN( 200.0, ( value * 200.0 ) ) );
            if ( dval != currentGC->configInfo.width ) {
                ClearPolyData();
            }
            currentGC->configInfo.width = dval;
        }
    }
    else if ( attr == GRF__SIZE ) {
        /* If required retrieve the current character size, and set a
           new size.  The attribute value should be a factor by which
           to multiply the default character size. */
        if ( old_value ) *old_value = currentGC->configInfo.size;
        if ( value != AST__BAD ){
            currentGC->configInfo.size = value;
        }
    }
    else if ( attr == GRF__FONT ) {
        /* If required retrieve the current character font, and set a
           new font. */
        if ( old_value ) *old_value = (double) currentGC->configInfo.font;
        if ( value != AST__BAD ) {
            currentGC->configInfo.font = MAX( 0, MIN( 18, (int) value ) );
        }
    }
    else if ( attr == GRF__COLOUR ) {
        /* If required retrieve the current colour index, and set a
           new colour index. */
        if ( old_value ) *old_value = (double) currentGC->configInfo.colour;
        if ( value != AST__BAD ) {
            ival = MAX( 0, MIN( MAXCOLOURS - 1, (int) value ) );
            if ( ival != currentGC->configInfo.colour ) {
                ClearPolyData();
            }
            currentGC->configInfo.colour = ival;
        }
    }
    else {
        /* Give an error message for any other attribute value. */
        astError( AST__GRFER, "vtkG3DAttr: Unknown graphics attribute '%d' "
                  "requested.", attr );
        return 0;
    }

    /* Return. */
    return 1;
}

/*
 *+
 *  Name:
 *     vtkG3DCap

 *  Purpose:
 *     Indicate if this grf3d module has a given capability.

 *  Synopsis:
 *     #include "grf3d.h"
 *     int vtkG3DCap( int cap, int value )

 *  Description:
 *     This function is called by the AST Plot class to determine if the
 *     grf3d module has a given capability, as indicated by the "cap"
 *     argument.

 *  Parameters:
 *     cap
 *        The capability being inquired about. This will be one of the
 *        following constants defined in grf3d.h:
 *
 *        GRF3D__ESC: This function should return a non-zero value if the
 *        vtkG3DText and vtkG3DTxExt functions can recognise and interpret
 *        graphics escape sequences within the supplied string. These
 *        escape sequences are described below. Zero should be returned
 *        if escape sequences cannot be interpreted (in which case the
 *        Plot class will interpret them itself if needed). The supplied
 *        "value" argument should be ignored only if escape sequences cannot
 *        be interpreted by vtkG3DText and vtkG3DTxExt. Otherwise, "value"
 *        indicates whether vtkG3DText and vtkG3DTxExt should interpret escape
 *        sequences in subsequent calls. If "value" is non-zero then
 *        escape sequences should be interpreted by vtkG3DText and
 *        vtkG3DTxExt. Otherwise, they should be drawn as literal text.

 *  Returned Value:
 *     The return value, as described above. Zero should be returned if
 *     the supplied capability is not recognised.
 *-
 */
static int vtkG3DCap( int cap, int value )
{
    /* That's simple we cannot do this. */
    return 0;
}

/*
 *+
 *  Name:
 *     vtkG3DFlush

 *  Purpose:
 *     Flush all pending graphics to the output device.

 *  Synopsis:
 *     #include "grf3d.h"
 *     int vtkG3DFlush( void )

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
static int vtkG3DFlush( void )
{
    /* Render the scene. */
    currentGC->renderer->Render();
    return 1;
}

/*
 *+
 *  Name:
 *     vtkG3DLine

 *  Purpose:
 *     Draw a polyline (i.e. a set of connected lines).

 *  Synopsis:
 *     #include "grf3d.h"
 *     int vtkG3DLine( int n, float *x, float *y, float *z )

 *  Description:
 *     This function displays lines joining the given positions.

 *  Parameters:
 *     n
 *        The number of positions to be joined together.
 *     x
 *        A pointer to an array holding the "n" x values.
 *     y
 *        A pointer to an array holding the "n" y values.
 *     z
 *        A pointer to an array holding the "n" z values.

 *  Returned Value:
 *     A value of 0 is returned if an error occurs, and 1 is returned
 *     otherwise.

 *-
 */
static int vtkG3DLine( int n, float *x, float *y, float *z )
{
    /* Do nothing if we have less than 2 points, but do not indicate an
     * error. */
    if ( n < 2 ) {
        return 1;
    }

    /* Create the vtk objects to manage this polyline. If not already done. */

    /* Create a vtkPolyData object for containing line segments, if needed.*/
    if ( currentGC->polyData == NULL ) {
        CreatePolyObjects();
    }

    /* The PolyLine is just a collection of points in the PolyData, with some
     * list of related identifiers. */
    vtkIdList *ids = vtkIdList::New();
    ids->SetNumberOfIds( n );

    /* Add points */
    for ( int i = 0; i < n; i++, currentGC->currentPolyDataId++ ) {
        currentGC->polyPoints->
            InsertPoint( currentGC->currentPolyDataId, x[i], y[i], z[i] );
        ids->SetId( i, currentGC->currentPolyDataId );
    }

    /* Add to data set */
    currentGC->polyData->InsertNextCell( VTK_POLY_LINE, ids );
    currentGC->polyData->SetPoints( currentGC->polyPoints );

    ids->Delete();
    return 1;
}

/*
 *+
 *  Name:
 *     vtkG3DMark

 *  Purpose:
 *     Draw a set of markers.

 *  Synopsis:
 *     #include "grf.h"
 *     int vtkG3DMark( int n, float *x, float *y, float *z, int type,
 *                     float norm[3] )

 *  Description:
 *     This function draws markers centred at the given positions.

 *  Parameters:
 *     n
 *        The number of markers to draw.
 *     x
 *        A pointer to an array holding the "n" x values.
 *     y
 *        A pointer to an array holding the "n" y values.
 *     z
 *        A pointer to an array holding the "n" z values.
 *     type
 *        An integer which can be used to indicate the type of marker symbol
 *        required. See the description of routine PGPT in the PGPLOT manual.
 *     norm
 *        This is not used in this implementation.

 *  Returned Value:
 *     A value of 0 is returned if an error occurs, and 1 is returned
 *     otherwise.

 *  Notes:
 *     -  Nothing is done if "n" is less than 1, or if a NULL pointer is
 *     given for "x", "y" or "z".
 *-
 */
static int vtkG3DMark( int n, float *x, float *y, float *z, int type,
                       float norm[3] )
{
    /*  Return if any of the coordinate pointers is NULL. */
    if ( !x || !y || !z ) return 1;

    /*  Initialise the controller objects, if not already done. */
    if ( currentGC->markerPoints == NULL ) {
        CreateMarkerObjects();
    }

    /*  Add the points and store the related configuration info. */
    for ( int i = 0; i < n; i++ ) {
        currentGC->markerPoints->
            InsertPoint( currentGC->currentMarkerId, x[i], y[i], z[i] );
        currentGC->markerColours->InsertTuple1( currentGC->currentMarkerId,
                                                currentGC->configInfo.colour );
        currentGC->markerInfo->InsertTuple2( currentGC->currentMarkerId,
                                             (double) type,
                                             currentGC->configInfo.size );
        currentGC->currentMarkerId++;
    }
    return 1;
}

/*
 *+
 *  Name:
 *     vtkG3DQch

 *  Purpose:
 *     Return the character height in world coordinates.

 *  Synopsis:
 *     #include "grf3d.h"
 *     int vtkG3DQch( float *ch )

 *  Description:
 *     This function returns the height of characters drawn using vtkG3DText.

 *  Parameters:
 *     ch
 *        A pointer to the double which is to receive the height of
 *        characters drawn with vtkG3DText.

 *  Returned Value:
 *     A value of 0 is returned if an error occurs, and 1 is returned
 *     otherwise.

 *  Notes:
 *     - Since the 3D world coordinate axes are assumed to be equally
 *     scaled, the height of text in world coordinate units is independent
 *     of the orientation of the text. Therefore, this function returns
 *     only one height value, unlike the equivalent 2D astGQch function
 *     that returns two heights.
 *-
 */
static int vtkG3DQch( float *ch )
{
    /* Measured for vtkVectorText. */
    *ch = (float) currentGC->textScale * TEXTFUDGE;
    return 1;
}

/*
 *+
 *  Name:
 *     vtkG3DText

 *  Purpose:
 *     Draw a character string.

 *  Synopsis:
 *     #include "grf3d.h"
 *     int vtkG3DText( const char *text, float ref[3], const char *just,
 *                     float up[3], float norm[3] )

 *  Description:
 *     This function displays a character string at a given position
 *     on a given plane in 3D world coords, using a specified
 *     justification and up-vector.

 *  Parameters:
 *     text
 *        Pointer to a null-terminated character string to be displayed.
 *     ref
 *        The reference (x,y,z) coordinates.
 *     just
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
 *     up
 *        The (x,y,z) up-vector for the text. The actual up vector used is
 *        the projection of the supplied vector onto the plane specified by
 *        "norm".
 *     norm
 *        The (x,y,z) components of a vector that is normal to the plane
 *        containing the text. The given vector passes through the text
 *        from the back to the front. If all components of this vector are
 *        zero, then a normal vector pointing towards the camera eye is used.

 *  Returned Value:
 *     A value of 0 is returned if an error occurs, and 1 is returned
 *     otherwise.

 *-
 */
static int vtkG3DText( const char *text, float ref[3], const char *just,
                       float up[3], float norm[3] )
{
    /*  Create the text prop. */
    vtkFollower *prop = CreateVectorText( text, ref, just, up, norm );

    /*  Record in assembly for tidying up later. */
    currentGC->assembly->AddPart( prop );

    /*  Add to the renderer. */
    currentGC->renderer->AddActor( prop );

    /* Follower points at camera when requested. */
    if ( currentGC->lookAtCamera ) {
        prop->SetCamera( currentGC->renderer->GetActiveCamera() );
    }

    /* Remove unnecessary references */
    prop->Delete();

    return 1;
}

/*
 *+
 *  Name:
 *     vtkG3DTxExt

 *  Purpose:
 *     Get the extent of a character string.

 *  Synopsis:
 *     #include "grf3d.h"
 *     int vtkG3DTxExt( const char *text, float ref[3], const char *just,
 *                      float up[3], float norm[3], float *xb, float *yb,
 *                      float *zb, float bl[3] )

 *  Description:
 *     This function returns the corners of a box which would enclose the
 *     supplied character string if it were displayed using vtkG3DText.
 *
 *     The returned box INCLUDES any leading or trailing spaces.

 *  Parameters:
 *     text
 *        Pointer to a null-terminated character string to be displayed.
 *     ref
 *        The reference (x,y,z) coordinates.
 *     just
 *        A character string which specifies the location within the
 *        text string which is to be placed at the reference position
 *        given by x and y. The first character may be 'T' for "top",
 *        'C' for "centre", 'B' for "baseline", or "M" for "bottom", and
 *        specifies the vertical location of the reference position. Note,
 *        "baseline" corresponds to the base-line of normal text. Some
 *        characters (eg "y", "g", "p", etc) descend below the base-line,
 *        and so "M" and "B" will produce different effects for such
 *        characters. The second character may be 'L' for "left", 'C' for
 *        "centre", or 'R' for "right", and specifies the horizontal
 *        location of the reference position. If the string has less than
 *        2 characters then 'C' is used for the missing characters.
 *     up
 *        The (x,y,z) up-vector for the text. The actual up vector used is
 *        the projection of the supplied vector onto the plane specified by
 *        "norm".
 *     norm
 *        The (x,y,z) components of a vector that is normal to the plane
 *        containing the text. The given vector passes through the text
 *        from the back to the front. If all components of this vector are
 *        zero, then a normal vector pointing towards the camera eye is used.
 *     xb
 *        An array of 4 elements in which to return the x coordinate of
 *        each corner of the bounding box.
 *     yb
 *        An array of 4 elements in which to return the y coordinate of
 *        each corner of the bounding box.
 *     zb
 *        An array of 4 elements in which to return the z coordinate of
 *        each corner of the bounding box.
 *     bl
 *        The 3D world coordinates at the left hand end of the text
 *        baseline.

 *  Returned Value:
 *     A value of 0 is returned if an error occurs, and 1 is returned
 *     otherwise.

 *  Notes:
 *     -  The order of the corners is anti-clockwise starting at the
 *        bottom left when viewing the text normally (i.e. face on).
 *     -  This routine does not recognise PGPLOT escape sequences.
 *     -  A NULL value for "just" causes a value of "CC" to be used.
 *-
 */
static int vtkG3DTxExt( const char *text, float ref[3], const char *just,
                        float up[3], float norm[3], float *xb, float *yb,
                        float *zb, float bl[3] )
{
    /* Initialise the returned values to indicate no box available. */
    for( int i = 0; i < 4; i++ ){
        xb[ i ] = 0.0;
        yb[ i ] = 0.0;
        zb[ i ] = 0.0;
    }

    /* Bounding box for none rotated, simple axis aligned string. */
    vtkVectorText *vtext = vtkVectorText::New();
    vtext->SetText( text );
    vtkPolyDataMapper *mapper = vtkPolyDataMapper::New();
    vtkFollower *prop = vtkFollower::New();
    prop->SetMapper( mapper );
    mapper->SetInput( vtext->GetOutput() );

    double *bbox = prop->GetBounds();
    vtext->Delete();
    mapper->Delete();
    prop->Delete();

    /* Get orientation of the text plane. */
    double orient[3];
    char newjust[3];
    float tx[3], ty[3], tz[3];
    TextOrientation( ref, up, norm, just, orient, newjust, tx, ty, tz );

    /* Set up the bounding box in text plane coordinates. */
    float txlo = bbox[0] * currentGC->textScale * TEXTFUDGE;
    float txhi = bbox[1] * currentGC->textScale * TEXTFUDGE;
    float tylo = bbox[2] * currentGC->textScale * TEXTFUDGE;
    float tyhi = bbox[3] * currentGC->textScale * TEXTFUDGE;
    float tyzero = 0.0f;

    /* Adjust the text plane bounding box to take account of the specified
     * text justification. The above process implicitly assumed a justification
     * of "BL". */
    float w;
    if ( newjust[0] == 'C' || newjust[0] == 0 ) {
        w = 0.5 * ( tyhi + tylo );
        tylo -= w;
        tyhi -= w;
        tyzero -= w;
    }
    else if ( newjust[0] == 'T' ) {
        w = tyhi;
        tylo -= w;
        tyhi -= w;
        tyzero -= w;
    }
    else if ( newjust[0] == 'M' ) {
        w = -tylo;
        tylo += w;
        tyhi += w;
        tyzero += w;
    }
    else if ( newjust[0] != 'B' ) {
        astError( AST__GRFER, "astG3DTxExt: Justification string '%s' "
                  "is invalid.", newjust );
        return 0;
    }

    if ( newjust[1] == 'C' || newjust[1] == 0 ) {
        w = 0.5 * ( txhi + txlo );
        txlo -= w;
        txhi -= w;
    }
    else if ( newjust[1] == 'R' ) {
        w = txhi;
        txlo -= w;
        txhi -= w;
    }
    else if ( newjust[1] == 'L' ) {
        w = txlo;
        txlo -= w;
        txhi -= w;
    }
    else {
        astError( AST__GRFER, "astG3DTxExt: Justification string '%s' "
                  "is invalid.", newjust );
        return 0;
    }

    /* Use the supplied text plane axis vectors to transform the corners of
     * the text plane bounding box into 3D world coordinates. */
    xb[0] = tx[0]*txlo + ty[0]*tylo + ref[0];
    yb[0] = tx[1]*txlo + ty[1]*tylo + ref[1];
    zb[0] = tx[2]*txlo + ty[2]*tylo + ref[2];

    xb[1] = tx[0]*txhi + ty[0]*tylo + ref[0];
    yb[1] = tx[1]*txhi + ty[1]*tylo + ref[1];
    zb[1] = tx[2]*txhi + ty[2]*tylo + ref[2];

    xb[2] = tx[0]*txhi + ty[0]*tyhi + ref[0];
    yb[2] = tx[1]*txhi + ty[1]*tyhi + ref[1];
    zb[2] = tx[2]*txhi + ty[2]*tyhi + ref[2];

    xb[3] = tx[0]*txlo + ty[0]*tyhi + ref[0];
    yb[3] = tx[1]*txlo + ty[1]*tyhi + ref[1];
    zb[3] = tx[2]*txlo + ty[2]*tyhi + ref[2];

    xb[0] -= 10.0;
    xb[1] -= 10.0;
    xb[2] -= 10.0;
    xb[3] -= 10.0;

    /* Also transform the text plane coordinates at the bottom left of the
     * text baseline into 3D world coordinates. */
    bl[0] = tx[0]*txlo + ty[0]*tyzero + ref[0];
    bl[1] = tx[1]*txlo + ty[1]*tyzero + ref[1];
    bl[2] = tx[2]*txlo + ty[2]*tyzero + ref[2];

    return 1;
}

/*========================*/
/*  Local Static Functions  */
/*========================*/

/*
 *+
 *  Name:
 *     ClearAllObjects

 *  Purpose:
 *     Clear all the props, mappers and data sources for the current context
 *     and remove everything from the render scene.

 *-
 */
void ClearAllObjects()
{
    /*  Access all stored actors and remove from the scene. */
    if ( currentGC->assembly != NULL ) {
        if ( currentGC->renderer != NULL ) {
            vtkPropCollection *collection = currentGC->assembly->GetParts();
            collection->InitTraversal();
            vtkProp *prop;
            while ( ( prop = collection->GetNextProp() ) ) {
                currentGC->renderer->RemoveViewProp( prop );
            }

            /*  Make sure modifications are acted on. */
            currentGC->renderer->Modified();
        }
        currentGC->assembly->Delete();
        currentGC->assembly = NULL;

        /*  Release any static references associated with props. */
        /*  Polylines */
        if ( currentGC->polyData ) {
            currentGC->polyData->Delete();
        }
        if ( currentGC->polyPoints ) {
            currentGC->polyPoints->Delete();
        }
        ClearPolyData();

        /*  Markers. */
        if ( currentGC->markerPoints != NULL ) {
            currentGC->markerColours->Delete();
            currentGC->markerGlyph->Delete();
            currentGC->markerInfo->Delete();
            currentGC->markerPoints->Delete();
            currentGC->markerSquad->Delete();
            currentGC->markerPoints = NULL;
            currentGC->currentMarkerId = 0;
        }
    }
}

/*
 *+
 *  Name:
 *     CreatePolyObjects

 *  Purpose:
 *     Create objects needed for drawing lines.

 *  Description:
 *     Creates new vtkPolyData and vtkPoints instances together with a mapper
 *     and actor to realise it. The current configuration values are set in
 *     the actor.

 *-
 */
static void CreatePolyObjects( void )
{
    if ( currentGC->polyData != NULL ) {
        ClearPolyData();
    }

    /*  New objects */
    currentGC->polyData = vtkPolyData::New();
    currentGC->polyPoints = vtkPoints::New();
    vtkDataSetMapper *mapper = vtkDataSetMapper::New();
    vtkActor *prop = vtkActor::New();

    /*  Set mapper and add polyData to it. */
    prop->SetMapper( mapper );
    mapper->SetInput( currentGC->polyData );

    /*  Record in assembly for tidying up later. */
    currentGC->assembly->AddPart( prop );

    /*  Add to the renderer. */
    currentGC->renderer->AddActor( prop );

    /*  Room for points, XXX how to manage this better */
    currentGC->polyData->Allocate( 1000, 1000 );

    /* Set the line properties. */
    vtkProperty *property = vtkProperty::New();
    property->SetLineWidth( currentGC->configInfo.width );
    property->
        SetColor( lookupTable->GetTableValue( currentGC->configInfo.colour ) );
    property->
        SetLineStipplePattern( StandardStipples[currentGC->configInfo.style] );
    prop->SetProperty( property );

    /* Remove unnecessary references */
    prop->Delete();
    mapper->Delete();
    property->Delete();
}

/*
 *+
 *  Name:
 *     ClearPolyData

 *  Purpose:
 *     Reset state so that a new vtkPolyData instance will be used.

 *  Description:
 *     Arranges so new lines will be drawn using a new vtkPolyData
 *     instance, mapper and actor. This is required when the properties
 *     of line rendering are changed (one actor per colour, style etc.).

 *-
 */
static void ClearPolyData( void )
{
    currentGC->polyData = NULL;
    currentGC->polyPoints = NULL;
    currentGC->currentPolyDataId = 0;
}

/*
 *+
 *  Name:
 *     CreateVectorText

 *  Purpose:
 *     Create, organise, orient and position a vtkVectorText instance.

 *  Description:
 *     Creates a vtkVector instance for a given text string. Arranges for the
 *     mapping and realisation using a vtkFollower instance. The follower
 *     is positioning using the given justification, position and orientation
 *     information (see ast3DGline).

 *-
 */
static vtkFollower *CreateVectorText( const char *text, float ref[3],
                                      const char *just, float up[3],
                                      float norm[3] )
{
    vtkVectorText *vtext = vtkVectorText::New();
    vtext->SetText( text );

    vtkPolyDataMapper *mapper = vtkPolyDataMapper::New();
    vtkFollower *prop = vtkFollower::New();

    /*  Set mapper and add text to it. */
    prop->SetMapper( mapper );
    mapper->SetInput( vtext->GetOutput() );

    /* Set the text properties. */
    vtkProperty *property = vtkProperty::New();
    property->
        SetColor( lookupTable->GetTableValue( currentGC->configInfo.colour ) );
    prop->SetProperty( property );

    /* Avoid shading, text should always be same colour from all directions,
     * so we increase the power for ambient lighting. */
    property->SetAmbient( 10.0 );

    /* Do the positioning... */

    /* Calculate the rotation of text plane. */
    double orient[3];
    char newjust[3];
    float tx[3], ty[3], tz[3];
    TextOrientation( ref, up, norm, just, orient, newjust, tx, ty, tz );

    /* Adjust initial position for justification. */
    double *bounds = prop->GetBounds();
    double *centre = prop->GetCenter();
    if ( newjust[0] == 'C' && newjust[1] == 'C' ) {
        prop->SetOrigin( centre[0], centre[1], centre[2] );
    }
    else {
        double origin[3];
        origin[2] = bounds[4];

        if ( newjust[0] == 'C' ) {
            origin[1] = centre[1];
        }
        else if ( newjust[0] == 'B' || newjust[0] == 'M' ) {
            origin[1] = bounds[2];
        }
        else {
            origin[1] = bounds[3];
        }

        if ( newjust[1] == 'C' ) {
            origin[0] = centre[0];
        }
        else if ( newjust[1] == 'L' ) {
            origin[0] = bounds[0];
        }
        else {
            origin[0] = bounds[1];
        }
        prop->SetOrigin( origin[0], origin[1], origin[2] );
    }

    /* Now apply rotation of text plane. */
    prop->SetOrientation( orient );

    /* Scale of text. */
    prop->SetScale( currentGC->textScale, currentGC->textScale,
                    currentGC->textScale );

    /* Final position of text. */
    prop->SetPosition( (double) ref[0], (double) ref[1], (double) ref[2] );

    /*  Release unneeded references. */
    vtext->Delete();
    mapper->Delete();
    property->Delete();

    return prop;
}


/* Get the angles and unit vectors representing a re-orientation into a
 * particular direction using the GRF defined up and norm vectors and
 * justification. */
static void TextOrientation( float ref[3], float up[3], float norm[3],
                             const char*just, double orient[3],
                             char newjust[3],
                             float rx[3], float ry[3], float rz[3] )
{
    /* Initialise the returned justification to equal the supplied
     * justification, supplying defaults if required. */
    if ( just ) {
        newjust[0] = just[0];
        newjust[1] = just[1];
        if ( !newjust[0] ) newjust[0] = 'C';
        if ( !newjust[1] ) newjust[1] = 'C';
        newjust[2] = '\0';
    }
    else {
        newjust[0] = 'C';
        newjust[1] = 'C';
        newjust[2] = '\0';
    }

    /* Get the camera, this is used as a default normal direction and for
     * checking that the text orientation gives readable text. */
    vtkCamera *camera = currentGC->renderer->GetActiveCamera();

    /* Get vector from the reference position to the camera by subtracting
     * the camera position from the reference position. Note requires that
     * the camera is setup fully. */
    double *cdir = camera->GetPosition();
    if ( cdir[0] == 0.0 && cdir[1] == 0.0 && cdir[2] == 1.0 ) {
        cdir[0] *= 1000.0; // Use false camera position, default is 0,0,1.
        cdir[1] *= 1000.0; // which is within the volume.
        cdir[2] *= 1000.0;
    }
    float eye[3];
    eye[0] = (float)cdir[0] - ref[0];
    eye[1] = (float)cdir[1] - ref[1];
    eye[2] = (float)cdir[2] - ref[2];

    /* Make eye into a unit vector. */
    vtkMath::Normalize( eye );

    /* Create unit vectors along the three axes of the text plane coordinate
     * system. These unit vectors are represented in terms of the 3D world
     * coordinate axes. The text Z axis is parallel to the supplied "norm"
     * vector. */
    rz[0] = norm[0];
    rz[1] = norm[1];
    rz[2] = norm[2];

    /* Attempt to normalise the "rz" vector. If it has zero length, use the
     * offset from the reference point to the camera as the normal direction
     * for text (points at the camera). */
    if ( vtkMath::Normalize( rz ) == 0.0 || currentGC->lookAtCamera ) {
        rz[0] = -eye[0];
        rz[1] = -eye[1];
        rz[2] = -eye[2];
    }

    /* Find vectors along the text plane x and y axes (projects up vector
     * onto text plane to unit vectors rx and ry in the text plane, rz is
     * normal to this). */
    vtkMath::Cross( up, rz, rx );
    vtkMath::Cross( rz, rx, ry );

    vtkMath::Normalize( rx );
    vtkMath::Normalize( ry );
    vtkMath::Normalize( rz );

    /* Tricks start, which are vital... */

    /* We now reverse text plane vectors if this will help their text to be
     * viewed "normally" on the screen. If the existing vectors cause the text
     * to be viewed from the back rather than the front, reverse the rx and rz
     * vectors so that the text is viewed from the front. */
    if ( vtkMath::Dot( rz, eye ) <= 0.0f ) {

         rz[0] = -rz[0];
         rz[1] = -rz[1];
         rz[2] = -rz[2];
         rx[0] = -rx[0];
         rx[1] = -rx[1];
         rx[2] = -rx[2];

         /* The text will have spun around the up vector (i.e. the ry axis), so
          * modify the horizontal justification so that the text occupies the
          * same area on the screen. */
         if ( newjust[1] == 'L' ) {
             newjust[1] = 'R';
         }
         else if( newjust[1] == 'R' ) {
             newjust[1] = 'L';
         }
    }

    /* If the existing vectors cause the text to be viewed upside down,
     * reverse the rx and ry vectors so that the text is viewed right way
     * up. */
    double *dvup = camera->GetViewUp();
    float vup[3];
    vup[0] = (float) dvup[0];
    vup[1] = (float) dvup[1];
    vup[2] = (float) dvup[2];
    if ( vtkMath::Dot( ry, vup ) <= 0.0 ) {

        ry[0] = -ry[0];
        ry[1] = -ry[1];
        ry[2] = -ry[2];
        rx[0] = -rx[0];
        rx[1] = -rx[1];
        rx[2] = -rx[2];

        /* The text will have spun around the rz vector (i.e. the viewing
         * vector), so modify both vertical and horizontal justification so
         * that the text occupies the same area on the screen. */
        if ( newjust[0] == 'B' || newjust[0] == 'M' ) {
            newjust[0] = 'T';
        }
        else if( newjust[0] == 'T' ) {
            newjust[0] = 'M';
        }

        if ( newjust[1] == 'L' ) {
            newjust[1] = 'R';
        }
        else if( newjust[1] == 'R' ) {
            newjust[1] = 'L';
        }
    }

    /* The above is much how the original PGPLOT implementation works, but for
     * VTK actors we need orientations, not vectors... */
    vtkMatrix4x4 *matrix = vtkMatrix4x4::New();

    matrix->Element[0][0] = rx[0];
    matrix->Element[1][0] = rx[1];
    matrix->Element[2][0] = rx[2];

    matrix->Element[0][1] = ry[0];
    matrix->Element[1][1] = ry[1];
    matrix->Element[2][1] = ry[2];

    matrix->Element[0][2] = rz[0];
    matrix->Element[1][2] = rz[1];
    matrix->Element[2][2] = rz[2];

    vtkTransform::GetOrientation( orient, matrix );
    matrix->Delete();
}

/*
 *+
 *  Name:
 *     CreateMarkerObjects

 *  Purpose:
 *     Create objects needed for drawing markers.

 *-
 */
static void CreateMarkerObjects( void )
{
    /*  Positions and related data */
    currentGC->markerPoints = vtkPoints::New();
    currentGC->markerInfo = vtkDoubleArray::New();
    currentGC->markerColours = vtkIntArray::New();
    currentGC->markerInfo->SetNumberOfComponents( 2 ); /* Type, size. */
    currentGC->markerColours->SetNumberOfComponents( 1 ); /* Colour */

    /*  Need info as a vtkDataSet so we can use it in a filter. */
    vtkPolyData *markerPolyData = vtkPolyData::New();
    markerPolyData->
        GetPointData()->SetScalars( currentGC->markerColours );
    markerPolyData->SetPoints( currentGC->markerPoints );

    /*  Marker shape creation. */
    currentGC->markerSquad = vtkSuperquadricSource::New();
    currentGC->markerGlyph = vtkProgrammableGlyphFilter::New();

    /*  Colour markers with markersInfo (see above). */
    currentGC->markerGlyph->SetInput( markerPolyData );

    /*  Define the marker source and the function used to generate the various
     *  sources (polydata). */
    currentGC->markerGlyph->SetSource( currentGC->markerSquad->GetOutput() );
    currentGC->markerGlyph->SetGlyphMethod( Grf3dMarkerGlyphFactory, NULL );

    /*  Map the glyphs. */
    vtkPolyDataMapper *mapper = vtkPolyDataMapper::New();
    mapper->SetInputConnection( currentGC->markerGlyph->GetOutputPort() );

    /*  Arrange colours to use our lookup table. */
    mapper->SetLookupTable( lookupTable );
    mapper->SetColorModeToMapScalars();
    mapper->SetScalarModeToUsePointData();
    mapper->SetScalarRange( 0, MAXCOLOURS );

    /*  Realise. */
    vtkActor *prop = vtkActor::New();
    prop->SetMapper( mapper );

    /*  Record in assembly for tidying up later. */
    currentGC->assembly->AddPart( prop );

    /*  Add to the renderer. */
    currentGC->renderer->AddActor( prop );

    /* Remove unnecessary references */
    markerPolyData->Delete();
    mapper->Delete();
    prop->Delete();
}

/*
 *  Function to configure the markerGlyph for a specific point. Called by the
 *  markerGlyph object when processing a point.
 */
static void Grf3dMarkerGlyphFactory( void *arg )
{
    int id = currentGC->markerGlyph->GetPointId();
    double *xyz = currentGC->markerGlyph->GetPoint();

    double values[2];
    currentGC->markerInfo->GetTupleValue( id, values );

    currentGC->markerSquad->SetScale( values[1], values[1], values[1] );
    currentGC->markerSquad->SetCenter( xyz );

    int type = (int) values[0];
    currentGC->markerSquad->SetPhiRoundness( markerSettings[type] );
    currentGC->markerSquad->SetThetaRoundness( markerSettings[type] );
    if ( type > 4 ) {
        currentGC->markerSquad->ToroidalOn();
    }
}
