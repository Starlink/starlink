/*+
 *   Name:
 *      GaiaUtilsTcl

 *   Purpose:
 *      Utility routines from Tcl scripts.

 *   Language:
 *      C++

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics and Astronomy Research Council

 *  Notes:
 *      C++ as it makes the Skycat class available, does not define a class.

 *   Authors:
 *      PWD: Peter W. Draper, Starlink - University of Durham

 *   History:
 *      31-MAR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#include <tcl.h>
#include <HTTP.h>

/* Local prototypes */
static int GaiaUtilsUrlGet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );

/**
 * Register all the gaiautils:: commands.
 */
int GaiaUtils_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "gaiautils::urlget", GaiaUtilsUrlGet,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    return TCL_OK;
}

/**
 * Get a file from a specified URL and return its content as the result.
 *
 * One argument the URL.
 */
static int GaiaUtilsUrlGet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    HTTP http;
    char *result = NULL;
    int nlines = 0;

    /* Check arguments, require one */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "URL" );
        return TCL_ERROR;
    }

    //  Get the file.
    result = http.get( Tcl_GetString( objv[1] ), nlines, 1 );

    //  And return its content as the result.
    if ( result != NULL ) {
        Tcl_SetResult( interp, result, TCL_VOLATILE );
        return TCL_OK;
    } 
    return TCL_ERROR;
}
