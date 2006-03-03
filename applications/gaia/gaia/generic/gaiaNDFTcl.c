/*+
 *   Name:
 *      gaiaNDFTcl
 *
 *   Purpose:
 *      Simple, as needed, access to NDFs from Tcl scripts.
 *
 *   Language:
 *      C
 *
 *   Authors:
 *      PWD: Peter W. Draper, Starlink - University of Durham
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
#include <gaiaNDF.h>

static int gaiaNdfOpen( ClientData clientData, Tcl_Interp *interp,
                        int objc, Tcl_Obj *CONST objv[] );
static int gaiaNdfClose( ClientData clientData, Tcl_Interp *interp,
                         int objc, Tcl_Obj *CONST objv[] );
static int gaiaNdfMap( ClientData clientData, Tcl_Interp *interp,
                       int objc, Tcl_Obj *CONST objv[] );

static int gaiaNdfGtWcs( ClientData clientData, Tcl_Interp *interp,
                         int objc, Tcl_Obj *CONST objv[] );

/**
 * Register all the NDF access commands.
 */
int Ndf_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "ndf::open", gaiaNdfOpen,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::close", gaiaNdfClose,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::map", gaiaNdfMap,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "ndf::getwcs", gaiaNdfGtWcs,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );
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
        Tcl_WrongNumArgs( interp, objc, objv, "ndf_name" );
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
    Tcl_Obj *resultObj;

    /* Check arguments, only allow one, the ndf identifier */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, objc, objv, "ndf_identifier" );
        return TCL_ERROR;
    }

    /* Get the identifier */
    if ( Tcl_GetIntFromObj( interp, objv[1], &indf ) == TCL_OK ) {
        result = gaiaSimpleCloseNDF( &indf );
    }
    else {
        resultObj = Tcl_GetObjResult( interp );
        Tcl_SetStringObj( resultObj, "not an integer" , -1 );
        result = TCL_ERROR;
    }
    return result;
}

/**
 * Map the NDF data array in double precision. The result is a memory address
 * (long int) and the number of elements mapped.
 */
static int gaiaNdfMap( ClientData clientData, Tcl_Interp *interp,
                       int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
    char *error_mess;
    int el;
    int indf;
    int result;
    void *dataPtr;

    /* Check arguments, only allow one, the ndf identifier */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, objc, objv, "ndf_identifier" );
        return TCL_ERROR;
    }

    /* Get the identifier */
    resultObj = Tcl_GetObjResult( interp );
    if ( Tcl_GetIntFromObj( interp, objv[1], &indf ) == TCL_OK ) {
        result = gaiaSimpleMapNDF( indf, "_DOUBLE", "DATA", &dataPtr, 
                                   &el, &error_mess  );
        if ( result == TCL_OK ) {
            Tcl_ListObjAppendElement( interp, resultObj, 
                                      Tcl_NewLongObj( (long) dataPtr ) );
            Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewIntObj( el ) );
        }
        else {
            Tcl_SetStringObj( resultObj, error_mess, -1 );
            free( error_mess );
        }
    }
    else {
        Tcl_SetStringObj( resultObj, "not an integer" , -1 );
        result = TCL_ERROR;
    }
    return result;
}

/**
 * Return the address of the NDF WCS component. This is an AST FrameSet.
 * 
 * Can return a WCS for a specific axis using option second argument.
 */
static int gaiaNdfGtWcs( ClientData clientData, Tcl_Interp *interp,
                         int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *iwcs;
    Tcl_Obj *resultObj;
    char *error_mess;
    int axis;
    int indf;
    int result;

    /* Check arguments, only allow one or two, the ndf identifier and an axis
     * number */
    if ( objc != 2 && objc != 3 ) {
        Tcl_WrongNumArgs( interp, objc, objv, "ndf_identifier [axis]" );
        return TCL_ERROR;
    }

    /* Get the identifier */
    resultObj = Tcl_GetObjResult( interp );
    if ( Tcl_GetIntFromObj( interp, objv[1], &indf ) != TCL_OK ) {
        Tcl_SetStringObj( resultObj, "not an integer" , -1 );
        return TCL_ERROR;
    }

    /* Get the axis */
    axis = -1;
    if ( objc == 3 ) {
        if ( Tcl_GetIntFromObj( interp, objv[2], &axis ) != TCL_OK ) {
            Tcl_SetStringObj( resultObj, "not an integer" , -1 );
            return TCL_ERROR;
        }
    }

    if ( axis == -1 ) {
        /* Full WCS */
        result = gaiaSimpleWCSNDF( indf, &iwcs, &error_mess );
    }
    else {
        /* WCS for one axis */
        result = gaiaSimpleAxisWCSNDF( indf, axis, &iwcs, &error_mess );
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
