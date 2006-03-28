/*+
 *   Name:
 *      gaiaNDFTcl
 *
 *   Purpose:
 *      Simple, that's without 2D bias, access to NDFs from Tcl scripts.
 *
 *   Language:
 *      C
 *
 *   Authors:
 *      PWD: Peter W. Draper, JAC - University of Durham
 *
 *   History:
 *      2-MAR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#include <stdio.h>
#include <stdlib.h>

#include <tcl.h>
#include <ndf.h>
#include <gaiaNDF.h>

/* Local prototypes */
static int gaiaNdfBounds( ClientData clientData, Tcl_Interp *interp, 
                          int objc, Tcl_Obj *CONST objv[] );
static int gaiaNdfCGet( ClientData clientData, Tcl_Interp *interp, 
                        int objc, Tcl_Obj *CONST objv[] );
static int gaiaNdfClose( ClientData clientData, Tcl_Interp *interp, 
                         int objc, Tcl_Obj *CONST objv[] );
static int gaiaNdfCoord( ClientData clientData, Tcl_Interp *interp, 
                         int objc, Tcl_Obj *CONST objv[] );
static int gaiaNdfDims( ClientData clientData, Tcl_Interp *interp, 
                        int objc, Tcl_Obj *CONST objv[] );
static int gaiaNdfGtWcs( ClientData clientData, Tcl_Interp *interp, 
                         int objc, Tcl_Obj *CONST objv[] );
static int gaiaNdfMap( ClientData clientData, Tcl_Interp *interp, 
                       int objc, Tcl_Obj *CONST objv[] );
static int gaiaNdfOpen( ClientData clientData, Tcl_Interp *interp, 
                        int objc, Tcl_Obj *CONST objv[] );

static int importNdfIdentifier( Tcl_Interp *interp, Tcl_Obj *obj, int *indf );

/**
 * Register all the NDF access commands.
 */
int Ndf_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "ndf::bounds", gaiaNdfBounds,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::close", gaiaNdfClose,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::coord", gaiaNdfCoord,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::dims", gaiaNdfDims,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::getc", gaiaNdfCGet,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::getwcs", gaiaNdfGtWcs,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::map", gaiaNdfMap,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::open", gaiaNdfOpen,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    return TCL_OK;
}

/*
 * Import an NDF identifier from a Tcl_Obj. Returns TCL_ERROR if fails.
 */
static int importNdfIdentifier( Tcl_Interp *interp, Tcl_Obj *obj, int *indf )
{
    Tcl_Obj *resultObj;

    if ( Tcl_GetIntFromObj( interp, obj, indf ) != TCL_OK ) {
        resultObj = Tcl_GetObjResult( interp );
        Tcl_SetStringObj( resultObj, " not an NDF identifier" , -1 );
        return TCL_ERROR;
    }
    return TCL_OK;
}

/**
 * Open an NDF. The result is an NDF identifier (integer)
 */
static int gaiaNdfOpen( ClientData clientData, Tcl_Interp *interp,
                        int objc, Tcl_Obj *CONST objv[] )
{
    char *error_mess;
    char *name;
    int indf;
    Tcl_Obj *resultObj;

    /* Check arguments, only allow one, the filename */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_name" );
        return TCL_ERROR;
    }

    /* Get name of NDF */
    name = Tcl_GetString( objv[1] );

    /* And open it */
    resultObj = Tcl_GetObjResult( interp );
    if ( gaiaSimpleOpenNDF( name, &indf, &error_mess ) == TCL_OK ) {
        Tcl_SetIntObj( resultObj, indf );
        return TCL_OK;
    }
    Tcl_SetStringObj( resultObj, error_mess, -1 );
    free( error_mess );
    return TCL_ERROR;
}

/**
 * Close an NDF.
 */
static int gaiaNdfClose( ClientData clientData, Tcl_Interp *interp,
                         int objc, Tcl_Obj *CONST objv[] )
{
    int indf;
    int result;

    /* Check arguments, only allow one, the ndf identifier */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_identifier" );
        return TCL_ERROR;
    }

    /* Get the identifier */
    result = importNdfIdentifier( interp, objv[1], &indf );
    if ( result == TCL_OK ) {
        /* Close NDF */
        result = gaiaSimpleCloseNDF( &indf );
    }
    return result;
}

/**
 * Map the NDF data array in the NDF's data type. The result is a memory
 * address (long int), the number of elements mapped and the data type.
 */
static int gaiaNdfMap( ClientData clientData, Tcl_Interp *interp,
                       int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
    char *error_mess;
    char type[NDF__SZTYP+1];
    int el;
    int indf;
    int result;
    void *dataPtr;

    /* Check arguments, only allow one, the ndf identifier */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_identifier" );
        return TCL_ERROR;
    }

    /* Get the identifier */
    result = importNdfIdentifier( interp, objv[1], &indf );
    if ( result == TCL_OK ) {
        resultObj = Tcl_GetObjResult( interp );
        result = gaiaSimpleTypeNDF( indf, "DATA", type, NDF__SZTYP+1,
                                    &error_mess );
        if ( result == TCL_OK ) {
            result = gaiaSimpleMapNDF( indf, type, "DATA", &dataPtr,
                                       &el, &error_mess  );
            if ( result == TCL_OK ) {
                Tcl_ListObjAppendElement( interp, resultObj,
                                          Tcl_NewLongObj( (long) dataPtr ) );
                Tcl_ListObjAppendElement( interp, resultObj,
                                          Tcl_NewIntObj( el ) );
                Tcl_ListObjAppendElement( interp, resultObj,
                                          Tcl_NewStringObj( type, -1 ) );
            }
        }
        if ( result != TCL_OK ) {
            Tcl_SetStringObj( resultObj, error_mess, -1 );
            free( error_mess );
        }
    }
    return result;
}

/**
 * Return the address of the NDF WCS component. This is an AST FrameSet.
 *
 * Can return a WCS for a specific axis using optional second and third
 * arguments. The third argument is an offset for the GRID domain and is used
 * when a section of a WCS is required (0 when not needed).
 */
static int gaiaNdfGtWcs( ClientData clientData, Tcl_Interp *interp,
                         int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *iwcs;
    Tcl_Obj *resultObj;
    char *error_mess;
    int axis;
    int indf;
    int offset;
    int result;

    /* Check arguments, only allow one or three, the ndf identifier and an
     * axis number plus offset */
    if ( objc != 2 && objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_identifier [axis offset]" );
        return TCL_ERROR;
    }

    /* Get the identifier */
    result = importNdfIdentifier( interp, objv[1], &indf );
    if ( result != TCL_OK ) {
        return TCL_ERROR;
    }
    resultObj = Tcl_GetObjResult( interp );

    /* Get the axis and offset */
    axis = -1;
    offset = 0;
    if ( objc == 4 ) {
        if ( Tcl_GetIntFromObj( interp, objv[2], &axis ) != TCL_OK ) {
            Tcl_SetStringObj( resultObj, "not an integer" , -1 );
            return TCL_ERROR;
        }
        if ( Tcl_GetIntFromObj( interp, objv[3], &offset ) != TCL_OK ) {
            Tcl_SetStringObj( resultObj, "not an integer" , -1 );
            return TCL_ERROR;
        }
    }

    if ( axis == -1 ) {
        /* Full WCS */
        result = gaiaSimpleWCSNDF( indf, &iwcs, &error_mess );
    }
    else {
        /* WCS for one axis, plus grid offset */
        result = gaiaSimpleAxisWCSNDF( indf, axis, offset, &iwcs, 
                                       &error_mess );
    }

    /* Export the WCS as a long containing the address */
    if ( result == TCL_OK ) {
        Tcl_SetLongObj( resultObj, (long) iwcs );
    }
    else {
        Tcl_SetStringObj( resultObj, error_mess, -1 );
        free( error_mess );
    }
    return result;
}

/**
 * Return the value of an NDF character component.
 */
static int gaiaNdfCGet( ClientData clientData, Tcl_Interp *interp,
                        int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
    char *error_mess;
    char value[NDF__SZHIS+1];
    int indf;
    int result;

    /* Check arguments, need 2 the ndf identifier and the component */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_identifier component" );
        return TCL_ERROR;
    }

    /* Get the identifier */
    result = importNdfIdentifier( interp, objv[1], &indf );
    if ( result == TCL_OK ) {
        resultObj = Tcl_GetObjResult( interp );
        value[0] = '\0';
        result = gaiaSimpleCGetNDF( indf, Tcl_GetString( objv[2] ),
                                    value, NDF__SZHIS+1, &error_mess );
        if ( result == TCL_OK ) {
            Tcl_SetStringObj( resultObj, value, -1 );
        }
        else {
            Tcl_SetStringObj( resultObj, error_mess, -1 );
            free( error_mess );
        }
    }
    return result;
}

/**
 * Get the dimensions of an NDF. Returns a list of values, one for each NDF
 * dimension.
 */
static int gaiaNdfDims( ClientData clientData, Tcl_Interp *interp,
                        int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
    char *error_mess;
    int i;
    int indf;
    int dims[NDF__MXDIM];
    int ndim;
    int result;

    /* Check arguments, need the ndf identifier */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_identifier" );
        return TCL_ERROR;
    }

    /* Get the NDF identifier */
    result = importNdfIdentifier( interp, objv[1], &indf );
    if ( result == TCL_OK ) {
        resultObj = Tcl_GetObjResult( interp );
        result = gaiaSimpleQueryDims( indf, NDF__MXDIM, dims, &ndim,
                                      &error_mess );
        if ( result == TCL_OK ) {
            for ( i = 0; i < ndim; i++ ) {
                Tcl_ListObjAppendElement( interp, resultObj,
                                          Tcl_NewIntObj( dims[i] ) );
            }
        }
        if ( result != TCL_OK ) {
            Tcl_SetStringObj( resultObj, error_mess, -1 );
            free( error_mess );
        }
    }
    return result;
}

/**
 * Get the bounds of an NDF. Returns a list of lower and upper pixel indices.
 */
static int gaiaNdfBounds( ClientData clientData, Tcl_Interp *interp,
                          int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
    char *error_mess;
    int i;
    int indf;
    int lbnd[NDF__MXDIM];
    int ndim;
    int result;
    int ubnd[NDF__MXDIM];

    /* Check arguments, need the ndf identifier */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "ndf_identifier" );
        return TCL_ERROR;
    }

    /* Get the NDF identifier */
    result = importNdfIdentifier( interp, objv[1], &indf );
    if ( result == TCL_OK ) {
        resultObj = Tcl_GetObjResult( interp );
        result = gaiaSimpleQueryBounds( indf, NDF__MXDIM, lbnd, ubnd, &ndim,
                                        &error_mess );
        if ( result == TCL_OK ) {
            for ( i = 0; i < ndim; i++ ) {
                Tcl_ListObjAppendElement( interp, resultObj,
                                          Tcl_NewIntObj( lbnd[i] ) );
                Tcl_ListObjAppendElement( interp, resultObj,
                                          Tcl_NewIntObj( ubnd[i] ) );
            }
        }
        if ( result != TCL_OK ) {
            Tcl_SetStringObj( resultObj, error_mess, -1 );
            free( error_mess );
        }
    }
    return result;
}

/**
 * Return the formatted coordinate of a position along a given axis.
 *
 * The second argument is the NDF identifier, the third the index of the axis,
 * and the fourth a list of all the pixel indices needed to identify the
 * coordinate (so the list must have a number for each dimension of the NDF).
 * A boolean, optional, final argument can be used to switch on trailing label
 * and units strings to the value.
 */
static int gaiaNdfCoord( ClientData clientData, Tcl_Interp *interp,
                         int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj **listObjv;
    Tcl_Obj *resultObj;
    char *coord;
    char *error_mess;
    double coords[NDF__MXDIM];
    int axis;
    int i;
    int indf;
    int ncoords;
    int result;
    int trailed;

    /* Check arguments */
    if ( objc != 4 && objc != 5 ) {
        Tcl_WrongNumArgs( interp, 1, objv,
                          "ndf_identifier axis {c1 c2 .. cn} [boolean]" );
        return TCL_ERROR;
    }

    /* Import NDF identifier */
    result = importNdfIdentifier( interp, objv[1], &indf );
    if ( result == TCL_OK ) {

        /* Next arguments are the axis that the coordinate is required for
         * followed by a list of all the pixel indices that specify the
         * position. */
        resultObj = Tcl_GetObjResult( interp );
        if ( Tcl_GetIntFromObj( interp, objv[2], &axis ) == TCL_OK ) {
            if ( Tcl_ListObjGetElements( interp, objv[3], &ncoords, &listObjv )
                 == TCL_OK ) {
                for ( i = 0; i < ncoords; i++ ) {
                    if ( Tcl_GetDoubleFromObj( interp, listObjv[i],
                                               &coords[i] ) != TCL_OK ) {
                        Tcl_SetStringObj( resultObj,
                                          "is not a valid number", -1 );
                        result = TCL_ERROR;
                        break;
                    }
                }

                /* Optional fourth element is whether to append the label and
                 * units. */
                trailed = 0;
                if ( objc == 5 ) {
                    if ( Tcl_GetBooleanFromObj( interp, objv[4], &trailed )
                         != TCL_OK ) {
                        result = TCL_ERROR;
                        Tcl_SetStringObj( resultObj, "is not a boolean", -1 );
                    }
                }
                if ( result != TCL_ERROR ) {
                    result = gaiaSimpleQueryCoord( indf, axis, coords,
                                                   trailed, ncoords, &coord,
                                                   &error_mess );
                    if ( result == TCL_OK ) {
                        Tcl_SetStringObj( resultObj, coord, -1 );
                    }
                    else {
                        Tcl_SetStringObj( resultObj, error_mess, -1 );
                        free( error_mess );
                    }
                }
            }
            else {
                result = TCL_ERROR;
                Tcl_SetStringObj( resultObj,
                                  " is not a list of pixel indices", -1 );
            }
        }
        else {
            result = TCL_ERROR;
            Tcl_SetStringObj( resultObj, "axis value not an integer", -1 );
        }
    }
    return result;
}
