/*+
 *   Name:
 *      Gaia3dVtkTcl

 *   Purpose:
 *      GAIA3D utility routines for interoperating with VTK.

 *   Language:
 *      C++

 *  Copyright:
 *     Copyright (C) 2007 Science and Technology Facilities Council
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

 *   Authors:
 *      PWD: Peter W. Draper, Starlink - University of Durham

 *   History:
 *      14-JUN-2007 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

using namespace std;

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdlib>
#include <cstdio>
#include <sstream>
#include <cstring>

#include <ColorMapInfo.h>
#include <tcl.h>
#include <GaiaArray.h>
#include <vtk/vtkColorTransferFunction.h>
#include <vtk/vtkImageData.h>
#include <vtk/vtkImageImport.h>
#include <vtk/vtkLookupTable.h>
#include <vtk/vtkRenderer.h>
#include <vtk/vtkTclUtil.h>
#include <vtk/vtkTransformPolyDataFilter.h>
#include <vtk/vtkUnsignedCharArray.h>

#include "vtkAstTransform.h"
#include "Grf3dVtk.h"

extern "C" {
#include <ast.h>
}

/* Local prototypes */
static int Gaia3dVtkAstGrid( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int Gaia3dVtkAstMark( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int Gaia3dVtkAstPlot( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int Gaia3dVtkCreateMask( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] );
static int Gaia3dVtkGrfAddColour( ClientData clientData, Tcl_Interp *interp,
                                  int objc, Tcl_Obj *CONST objv[] );
static int Gaia3dVtkGrfClear( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );
static int Gaia3dVtkGrfInit( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int Gaia3dVtkGrfSetContext( ClientData clientData, Tcl_Interp *interp,
                                   int objc, Tcl_Obj *CONST objv[] );
static int Gaia3dVtkGrfFreeContext( ClientData clientData, Tcl_Interp *interp,
                                    int objc, Tcl_Obj *CONST objv[] );
static int Gaia3dVtkSetArray( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );

static int Gaia3dVtkColourMap( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] );

static int Gaia3dVtkAstSetPolyDataFilterTransform( ClientData clientData,
                                                   Tcl_Interp *interp,
                                                   int objc,
                                                   Tcl_Obj *CONST objv[] );

/**
 * Register all the gvtk:: commands.
 */
int Gaia3dVtk_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "gvtk::cmap", Gaia3dVtkColourMap,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gvtk::astgrid", Gaia3dVtkAstGrid,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gvtk::astmark", Gaia3dVtkAstMark,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gvtk::astplot", Gaia3dVtkAstPlot,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gvtk::astsetpolydatafiltertransform",
                          Gaia3dVtkAstSetPolyDataFilterTransform,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gvtk::createmask", Gaia3dVtkCreateMask,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gvtk::grfaddcolour", Gaia3dVtkGrfAddColour,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gvtk::grfclear", Gaia3dVtkGrfClear,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gvtk::grfinit", Gaia3dVtkGrfInit,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gvtk::grffreecontext",
                          Gaia3dVtkGrfFreeContext, (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gvtk::grfsetcontext",
                          Gaia3dVtkGrfSetContext, (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gvtk::setarray", Gaia3dVtkSetArray,
                          (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

    return TCL_OK;
}

/**
 * Set the array component of an vtkImageImport object to all or part of a
 * given data-cube.
 *
 * There are six arguments, the ARRAYinfo struct containing the array data,
 * the vtkImageImport command reference, the current data dimensions and the
 * new axes limits along those dimensions (blank for none). The last two
 * determine if bad values are replaced with a null value (this is important
 * for volume rendering).
 *
 * Note you must create the vtkImageImport object, the ARRAYinfo struct
 * can be from an NDF or FITS file.
 *
 * The result is a single 1 or 0, which indicates that no BAD pixels were
 * definitely detected, or that the data may or may not contain BAD pixels.
 */
static int Gaia3dVtkSetArray( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    /* Check arguments, only allow six. */
    if ( objc != 7 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "array_handle vtkImageImport "
                          "dims limits check_null nullvalue" );
        return TCL_ERROR;
    }

    /* Get the ARRAYinfo struct */
    long adr;
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    ARRAYinfo *arrayInfo = (ARRAYinfo *) adr;

    /* Access the Tcl_Command for the vtkImageImport object */
    Tcl_CmdInfo cmdInfo;
    if ( Tcl_GetCommandInfo( interp, Tcl_GetString( objv[2] ), &cmdInfo ) ) {

        /* Get the vtkImageImport object */
        ClientData cd = cmdInfo.clientData;
        vtkImageImport *vii =
            (vtkImageImport *)(((vtkTclCommandArgStruct *)cd)->Pointer);

        /* Get the dimensions. */
        int dims[3];
        Tcl_Obj **dimObjv;
        int ndims;
        if ( Tcl_ListObjGetElements( interp, objv[3], &ndims, &dimObjv ) !=
             TCL_OK) {
            return TCL_ERROR;
        }
        for ( int i = 0; i < 3; i++ ) {
            if ( Tcl_GetIntFromObj(interp, dimObjv[i], &dims[i]) != TCL_OK ) {
                return TCL_ERROR;
            }
        }

        /* See if a section of the data is required. */
        int nvals;
        if ( Tcl_ListObjGetElements( interp, objv[4], &nvals, &dimObjv ) !=
             TCL_OK) {
            return TCL_ERROR;
        }
        int lbnd[3];
        int ubnd[3];
        if ( nvals > 1 ) {
            for ( int i = 0, j = 0; i < 3; i++, j += 2  ) {
                if ( Tcl_GetIntFromObj( interp, dimObjv[j], &lbnd[i] )
                     != TCL_OK ) {
                    return TCL_ERROR;
                }
                if ( Tcl_GetIntFromObj( interp, dimObjv[j+1], &ubnd[i] )
                     != TCL_OK) {
                    return TCL_ERROR;
                }
            }
        }

        /* Check for nulls? */
        int nullcheck = 0;
        if ( Tcl_GetBooleanFromObj( interp, objv[5], &nullcheck ) != TCL_OK ) {
            return TCL_ERROR;
        }
        double nullvalue = 0.0;
        if ( Tcl_GetDoubleFromObj(interp, objv[6], &nullvalue) != TCL_OK) {
            return TCL_ERROR;
        }

        /* Extract and normalise the data -- FITS & byte -- if
         * needed. Otherwise just normalise the data. */
        ARRAYinfo *normInfo = NULL;
        ARRAYinfo *currentInfo = arrayInfo;
        int nobad = 0;
        int savemem = 0;
        if ( nvals > 1 ) {
            gaiaArrayRawCubeFromCube( arrayInfo, dims, lbnd, ubnd,
                                      &currentInfo, GAIA_ARRAY_NEW );
        }
        gaiaArrayNormalisedFromArray( currentInfo, &normInfo, nullcheck,
                                      nullvalue, GAIA_ARRAY_NEW, &nobad );

        /* If new memory was allocated need to handle it */
        if ( normInfo == NULL ) {
            normInfo = currentInfo;

            /* Data not normalised, but that could be a from a section which
             * will be a copy. */
            if ( nvals > 1 ) {
                /* New memory. */
                savemem = 0;
            }
            else {
                savemem = 1;
            }
        }
        else {
            /* New memory */
            savemem = 0;

            /* If from a section we can free that memory now. */
            if ( nvals > 1 ) {
                gaiaArrayFree( currentInfo );
                gaiaArrayFreeInfo( currentInfo );
            }
        }

        /* Set related meta-data */
        switch ( normInfo->type ) {
        case HDS_BYTE:
            vii->SetDataScalarType( VTK_CHAR );
            break;
        case HDS_UWORD:
            vii->SetDataScalarTypeToUnsignedShort();
            break;
        case HDS_WORD:
            vii->SetDataScalarTypeToShort();
            break;
        case HDS_INTEGER:
            vii->SetDataScalarTypeToInt();
            break;
        case HDS_INT64:
            vii->SetDataScalarType( VTK_LONG );
            break;
        case HDS_REAL:
            vii->SetDataScalarTypeToFloat();
            break;
        case HDS_DOUBLE:
            vii->SetDataScalarTypeToDouble();
            break;
        default:
            //  Char fits all.
            vii->SetDataScalarTypeToUnsignedChar();
            break;
        }

        /* Assign shared data pointer, let VTK dispose if a memory copy */
        vii->SetImportVoidPointer( normInfo->ptr, savemem );

        /* Result is BAD pixel detection flag */
        Tcl_SetObjResult( interp, Tcl_NewBooleanObj( nobad ) );

        return TCL_OK;
    }
    return TCL_ERROR;
}

/**
 * Create an unsigned char mask representing the BAD pixels in
 * a data array.
 *
 * There are two arguments, the ARRAYinfo struct containing the array
 * data and the vtkUnsignedCharArray command reference. The input data are
 * checked for bad pixels and the array will not be populated if none
 * are found. The result will be 0 if that is true and 1 otherwise.
 *
 * Note you must create the vtkUnsignedCharArray object, the ARRAYinfo
 * struct can be from an NDF or FITS file.
 */
static int Gaia3dVtkCreateMask( ClientData clientData, Tcl_Interp *interp,
                                int objc, Tcl_Obj *CONST objv[] )
{
    ARRAYinfo *arrayInfo;
    Tcl_CmdInfo cmdInfo;
    long adr;
    vtkUnsignedCharArray *vuca;

    /* Check arguments, only allow two. */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv,
                          "array_handle vtkUnsignedCharArray" );
        return TCL_ERROR;
    }

    /* Get the ARRAYinfo struct */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    arrayInfo = (ARRAYinfo *) adr;

    /* Access the Tcl_Command for the vtkUnsignedCharArray object */
    if ( Tcl_GetCommandInfo( interp, Tcl_GetString( objv[2] ), &cmdInfo ) ) {

        /* Get the vtkImageImport object */
        ClientData cd = cmdInfo.clientData;
        vuca =
            (vtkUnsignedCharArray *)(((vtkTclCommandArgStruct *)cd)->Pointer);

        /* Create the mask, if needed */
        unsigned char *mask = gaiaArrayCreateUnsignedMask( arrayInfo,
                                                           GAIA_ARRAY_NEW );
        if ( mask != NULL ) {

            /* Assign shared data pointer */
            vuca->SetArray( mask, arrayInfo->el, 1 );
        }

        /* Result is OK */
        Tcl_SetObjResult( interp, Tcl_NewBooleanObj( 1 ) );
        return TCL_OK;
    }
    return TCL_ERROR;
}

/**
 *  Initialise the renderer used when drawing AST graphics using the Grf3d
 *  interface. One argument the vtkRenderer instance. Should be called before
 *  starting a sequence of AST graphics calls related to a particular
 *  renderer.
 *
 *  The result is a context for this renderer that should be re-established
 *  when drawing is expected (the context will be changed for other
 *  renderers).
 */
static int Gaia3dVtkGrfInit( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    /* Check arguments, need one, the renderer. */
    if ( objc != 2  ) {
        Tcl_WrongNumArgs( interp, 1, objv, "vtkRenderer" );
        return TCL_ERROR;
    }

    /* Access the Tcl_Command for the vtkRenderer object. */
    Tcl_CmdInfo cmdInfo;
    if ( ! Tcl_GetCommandInfo( interp, Tcl_GetString( objv[1] ), &cmdInfo ) ) {
        return TCL_ERROR;
    }

    /* Get the vtkRenderer object */
    ClientData cd = cmdInfo.clientData;
    vtkRenderer *vtkr =
        (vtkRenderer *)(((vtkTclCommandArgStruct *)cd)->Pointer);

    /* Do initialisation of the GRF3D interface. */
    void *gc = Grf3dVtk_Init( vtkr, 0 );

    /* Export the graphics context. */
    Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) gc ) );
    return TCL_OK;
}

/**
 *  Add a new VTK colour to the standard colourmap used by the GRF interface.
 *
 *  Four arguments are required, the index to insert the colour at (can
 *  overwrite the standard set, but that shouldn't normally be done) and
 *  the VTK RGB values.
 */
static int Gaia3dVtkGrfAddColour( ClientData clientData, Tcl_Interp *interp,
                                  int objc, Tcl_Obj *CONST objv[] )
{
    /* Check arguments, need four, the index and the colour. */
    if ( objc != 5  ) {
        Tcl_WrongNumArgs( interp, 1, objv, "index R G B" );
        return TCL_ERROR;
    }

    /* Index */
    int index;
    if ( Tcl_GetIntFromObj( interp, objv[1], &index ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Colour */
    double colour[3];
    for ( int i = 0; i < 3; i++ ) {
        if (Tcl_GetDoubleFromObj( interp, objv[i+2], &colour[i] ) != TCL_OK) {
            return TCL_ERROR;
        }
    }

    /* Add colour, but make sure we're initialised first. */
    Grf3dVtk_InitColours();
    Grf3dVtk_AddColour( index, colour[0], colour[1], colour[2] );
    return TCL_OK;
}

/**
 *  Set the context for the GRF graphics. A context is established by a call
 *  to gtvk::grfinit and will need re-establishing when it is possible that
 *  another renderer has taken the context. One argument the context.
 */
static int Gaia3dVtkGrfSetContext( ClientData clientData, Tcl_Interp *interp,
                                   int objc, Tcl_Obj *CONST objv[] )
{
    /* Check arguments, need one, the context. */
    if ( objc != 2  ) {
        Tcl_WrongNumArgs( interp, 1, objv, "grf_context" );
        return TCL_ERROR;
    }

    /* Import the context */
    long adr;
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    void *gc = (void *) adr;

    /* And set it */
    Grf3dVtk_SetContext( gc );
    return TCL_OK;
}

/**
 *  Free the context for the GRF graphics. A context is established by a call
 *  to gtvk::grfinit and will need freeing when the renderer is destroyed.
 *  One argument the context.
 */
static int Gaia3dVtkGrfFreeContext( ClientData clientData, Tcl_Interp *interp,
                                    int objc, Tcl_Obj *CONST objv[] )
{
    /* Check arguments, need one, the context. */
    if ( objc != 2  ) {
        Tcl_WrongNumArgs( interp, 1, objv, "grf_context" );
        return TCL_ERROR;
    }

    /* Import the context */
    long adr;
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    void *gc = (void *) adr;

    /* And free it */
    Grf3dVtk_FreeContext( gc );
    return TCL_OK;
}

/**
 * Create a Plot3D for the given 3D frameset using the current renderer (set
 * this using gvtk::grfinit command).  Requires the dimensions of the volume
 * to be used (usually the dimensions of the datacube associated with the
 * frameset). Any attributes of the plot can also be set (setup for drawing
 * grids, axes, markers etc.).
 *
 * The plot address is returned as the result unless an error occurs.
 * This should be annuled when no longer needed (gaiautils::astannul).
 */
static int Gaia3dVtkAstPlot( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    /* Check arguments, need three, the renderer, the WCS and cube
     * dimensions. */
    if ( objc != 3 && objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "AstFrameSet "
                          "{dim1 dim2 dim3} [plot_attributes]" );
        return TCL_ERROR;
    }

    /* Access the AstFrameSet. */
    long adr;
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    AstFrameSet *wcs = (AstFrameSet *) adr;

    /* Get dimensions. */
    int dims[3];
    Tcl_Obj **dimObjv;
    int ndims;
    if (Tcl_ListObjGetElements( interp, objv[2], &ndims, &dimObjv )!= TCL_OK) {
        return TCL_ERROR;
    }
    for ( int i = 0; i < 3; i++ ) {
        if ( Tcl_GetIntFromObj( interp, dimObjv[i], &dims[i] ) != TCL_OK ) {
            return TCL_ERROR;
        }
    }

    /* Get the bounds of the plotting region in base physical and graphics
     * coordinates. Both of these systems are GRID, except VTK uses a zero
     * based system and AST a one based system. */
    float graphbox[6];
    graphbox[0] = 0.0f;
    graphbox[1] = 0.0f;
    graphbox[2] = 0.0f;
    graphbox[3] = (float) (dims[0] - 1);
    graphbox[4] = (float) (dims[1] - 1);
    graphbox[5] = (float) (dims[2] - 1);

    double basebox[6];
    basebox[0] = (double) (graphbox[0] + 1.0f);
    basebox[1] = (double) (graphbox[1] + 1.0f);
    basebox[2] = (double) (graphbox[2] + 1.0f);
    basebox[3] = (double) (graphbox[3] + 1.0f);
    basebox[4] = (double) (graphbox[4] + 1.0f);
    basebox[5] = (double) (graphbox[5] + 1.0f);

    /* Use any attributes that have been given. */
    const char *atts = "";
    if ( objc == 4 ) {
        atts = Tcl_GetString( objv[3] );
    }

    /* Use a copy of the main AST FrameSet so we do not make any permanent
     * modifications (that cannot be undone).
     */
    AstFrameSet *copy = (AstFrameSet *) astCopy( wcs );
    AstPlot3D *plot = astPlot3D( copy, graphbox, basebox, atts );
    copy = (AstFrameSet *) astAnnul( copy );
    if ( !plot || ! astOK ) {
        if ( !astOK ) astClearStatus;
        Tcl_SetResult( interp, "Failed to create Plot3D", TCL_VOLATILE );
        return TCL_ERROR;
    }

    /* Export the plot address as the result. */
    Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) plot ) );
    return TCL_OK;
}

/**
 * Draw a grid in the given Plot3D instance. Applying the given text scale
 * and some AST attributes (for the Plot).
 */
static int Gaia3dVtkAstGrid( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    /* Check arguments. */
    if ( objc != 3 && objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "Plot3D text_scale [attributes]" );
        return TCL_ERROR;
    }

    /* Access the Plot3D address. */
    long adr;
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    AstPlot3D *plot = (AstPlot3D *) adr;

    /* Text scale. */
    double textScale = 5.0;
    if ( Tcl_GetDoubleFromObj( interp, objv[2], &textScale ) != TCL_OK ) {
        return TCL_ERROR;
    }
    Grf3dVtk_SetTextScale( textScale );

    /* Apply any new attributes to the plot. */
    if ( objc == 4 ) {
        astSet( plot, Tcl_GetString( objv[3] ) );
        if ( ! astOK ) {
            char buf[1024];
            sprintf( buf, "Failed to apply AST attributes to plot: %s",
                     Tcl_GetString( objv[3] ) );
            Tcl_SetResult( interp, buf, TCL_VOLATILE );
            astClearStatus;
            return TCL_ERROR;
        }
    }

    /* Draw the grid. */
    astGrid( plot );

    if ( ! astOK ) {
        Tcl_SetResult( interp, "Failed to draw 3D grid", TCL_VOLATILE );
        astClearStatus;
        return TCL_ERROR;
    }
    return TCL_OK;
}

/**
 * Draw markers using a Plot3D. Requires the address of a Plot3D instance,
 * a marker type and a list of positions to draw the markers at. The marker
 * types are defined in the Grf3dVtk.C file.
 */
static int Gaia3dVtkAstMark( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    /* Check arguments. */
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv,
                          "Plot3D marker_type {x1 y1 z2 x2 y2 z2...}" );
        return TCL_ERROR;
    }

    /* Access the Plot3D address. */
    long adr;
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    AstPlot3D *plot = (AstPlot3D *) adr;

    /* Mark type and coordinates. */
    int type;
    if ( Tcl_GetIntFromObj( interp, objv[2], &type ) != TCL_OK ) {
        return TCL_ERROR;
    }

    int nvals;
    Tcl_Obj **dimObjv;
    if (Tcl_ListObjGetElements( interp, objv[3], &nvals, &dimObjv )!= TCL_OK) {
        return TCL_ERROR;
    }
    double *coords = new double[nvals];
    for ( int i = 0; i < nvals; i++ ) {
        if (Tcl_GetDoubleFromObj( interp, dimObjv[i], &coords[i] ) != TCL_OK) {
            return TCL_ERROR;
        }
    }

    /* Draw the markers, for simplicity one at a time (otherwise we need to
     * create extra workspace to get the points in the correct order). */
    int nmarks = nvals / 3;
    double *in = coords;
    for ( int i = 0; i < nmarks; i++, in += 3 ) {
        astMark( plot, 1, 3, 1, in, type );
    }
    delete[] coords;

    if ( ! astOK ) {
        astClearStatus;
        Tcl_SetResult( interp, "Failed to draw 3D markers", TCL_VOLATILE );
        return TCL_ERROR;
    }
    return TCL_OK;
}

/**
 * Clear any GRF3D graphics that have been drawn.
 */
static int Gaia3dVtkGrfClear( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    /* Check arguments, none allowed. */
    if ( objc != 1 ) {
        Tcl_WrongNumArgs( interp, 1, objv,"" );
        return TCL_ERROR;
    }
    Grf3dVtk_Clear();
    return TCL_OK;
}

/**
 * Manage RTD colourmaps so that we can make these available as lookup tables
 * or colour transfer functions in VTK.
 *
 * This command implements two subcommands:
 *
 *    list                     -  returns a list of the available colourmaps.
 *    set name vtkLookupTable
 *    set name vtkColorTransferFunction
 *                             - sets a given vtkLookupTable or
 *                               vtkColorTransferFunction to use
 *                               the colours of the named colourmap.
 */
static int Gaia3dVtkColourMap( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] )
{
    const char *usage = "list | "
        "set name vtLookupTable | "
        "set name vtkColorTransferFunction minval maxval";

    /* Check arguments. */
    if ( objc != 2 && objc != 4 && objc != 6 ) {
        Tcl_WrongNumArgs( interp, 1, objv, usage );
        return TCL_ERROR;
    }

    const char* subcmd = Tcl_GetString( objv[1] );
    if ( strcmp( subcmd, "list" ) == 0 ) {

        /* Return a list of colourmaps in RTD. */
        ostringstream os;
        ColorMapInfo::list( os );
        Tcl_SetResult( interp, (char *) os.str().c_str(), TCL_VOLATILE );
        return TCL_OK;
    }
    else if ( objc != 4 && objc != 6 ) {
        Tcl_WrongNumArgs( interp, 1, objv, usage );
        return TCL_ERROR;
    }
    else if ( ( strcmp( subcmd, "set" ) != 0 ) ) {

        /* Not set subcommand, should be. */
        char buf[80];
        sprintf( buf, "Unknown gvtk::cmap subcommand: "
                 "\"%s\", should be set or list", subcmd );
        Tcl_SetResult( interp, buf, TCL_VOLATILE );
        return TCL_ERROR;
    }

    /* Set up a lookup table or colour function to mirror an RTD colourmap.
     * First get the colour map name. */
    char *name = Tcl_GetString( objv[2] );
    ColorMapInfo* cmapInfo = ColorMapInfo::get( name );
    if ( cmapInfo == NULL ) {
        char buf[80];
        sprintf( buf, "Unknown colourmap: \"%s\"", name );
        Tcl_SetResult( interp, buf, TCL_VOLATILE );
        return TCL_ERROR;
    }

    /* Access the Tcl_Command for the vtk object */
    Tcl_CmdInfo cmdInfo;
    if ( Tcl_GetCommandInfo( interp, Tcl_GetString( objv[3] ), &cmdInfo ) ) {

        ClientData cd = cmdInfo.clientData;
        vtkObject *obj =
            (vtkObject *)(((vtkTclCommandArgStruct *)cd)->Pointer);

        if ( obj->IsA( "vtkLookupTable" ) ) {
            vtkLookupTable *table = (vtkLookupTable *)
                (((vtkTclCommandArgStruct *)cd)->Pointer);

            /* Get each set of RGB values and add them to the table. */
            table->SetNumberOfColors( MAX_COLOR );
            table->Build();
            const RGBColor *rgb = cmapInfo->rgbcolor();
            for ( int i = 0; i < MAX_COLOR; i++ ) {
                table->SetTableValue( i, rgb[i].red, rgb[i].green,
                                      rgb[i].blue );
            }
        }
        else if ( obj->IsA( "vtkColorTransferFunction" ) ) {
            vtkColorTransferFunction *func = (vtkColorTransferFunction *)
                (((vtkTclCommandArgStruct *)cd)->Pointer);

            if ( objc != 6 ) {
                Tcl_WrongNumArgs( interp, 1, objv, usage );
                return TCL_ERROR;
            }

            /* Additional two arguments, the range in data values (no scalar
             * mapping for these). */
            double min;
            double max;
            if ( Tcl_GetDoubleFromObj( interp, objv[4], &min ) == TCL_OK &&
                 Tcl_GetDoubleFromObj( interp, objv[5], &max ) == TCL_OK ) {

                /* Get each set of RGB values and add them to the function. */
                /* The value associated with each point spans min to max
                 * linearly. */
                double inc = ( max - min ) / (double) MAX_COLOR;
                double v = min;
                const RGBColor *rgb = cmapInfo->rgbcolor();
                for ( int i = 0; i < MAX_COLOR; i++ ) {
                    func->AddRGBPoint( v, rgb[i].red, rgb[i].green,
                                       rgb[i].blue );
                    v += inc;
                }
            }
        }
        else {
            Tcl_SetResult( interp, "Unknown VTK object passed", TCL_VOLATILE );
            return TCL_ERROR;
        }
        return TCL_OK;
    }
    return TCL_ERROR;
}

/**
 * Set the transform of a vtkTransformPolyDataFilter to use an instance of
 * vtkAstTransform. The vtkAstTransform instance is created here and set to
 * use a given mapping (or frameset). This command avoids the need to export
 * vtkAstTransform as a Tcl Command, which would require working out how to
 * integrate that into VTK (easy when building VTK from scratch).
 */
static int Gaia3dVtkAstSetPolyDataFilterTransform( ClientData clientData,
                                                   Tcl_Interp *interp,
                                                   int objc,
                                                   Tcl_Obj *CONST objv[] )
{
    /* Check arguments. Need a vtkTransformPolyDataFilter and an AST mapping.*/
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv,
                          "vtkTransformPolyDataFilter AST-mapping" );
        return TCL_ERROR;
    }

    /* Access the vtkTransformPolyDataFilter. */
    Tcl_CmdInfo cmdInfo;
    if ( ! Tcl_GetCommandInfo( interp, Tcl_GetString( objv[1] ), &cmdInfo ) ) {
        return TCL_ERROR;
    }
    ClientData cd = cmdInfo.clientData;
    vtkTransformPolyDataFilter *tpdf =
       (vtkTransformPolyDataFilter *)(((vtkTclCommandArgStruct *)cd)->Pointer);

    /* Access the AST mapping (could be a FrameSet). */
    long adr;
    if ( Tcl_GetLongFromObj( interp, objv[2], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    AstMapping *mapping = (AstMapping *) adr;

    /* Create the vtkAstTransform to make the mapping available in VTK. */
    /* Note make a copy as vtkAstTransform annuls the pointer. */
    vtkAstTransform *transform = vtkAstTransform::New();
    transform->SetMapping( (AstMapping *) astCopy( mapping ) );

    /* Apply to the filter. */
    tpdf->SetTransform( transform );

    /* We're done. */
    transform->Delete();

    return TCL_OK;
}
