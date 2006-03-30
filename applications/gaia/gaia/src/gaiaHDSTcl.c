/*+
 *   Name:
 *      gaiaHDSTcl

 *   Purpose:
 *      Access necessary HDS routines.

 *   Language:
 *      C

 *   Authors:
 *      PWD: Peter W. Draper, JAC - University of Durham

 *   History:
 *      29-MAR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#include <stdio.h>
#include <stdlib.h>

#include <tcl.h>
#include <gaiaHDS.h>

/* Local prototypes */
static int gaiaHDSTclTune( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] );

/**
 * Register all the array commands.
 */
int HDS_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "hds::tune", gaiaHDSTclTune,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );
    return TCL_OK;
}


/**
 * Set a HDS tuning parameter.
 */
static int gaiaHDSTclTune( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] )
{
    char *what;
    char *error_mess;
    int value;

    /* Check arguments */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "what value" );
        return TCL_ERROR;
    }

    /* Get what. */
    what = Tcl_GetString( objv[1] );

    /* Get value. */
    if ( Tcl_GetIntFromObj( interp, objv[2], &value ) != TCL_OK ) {
        Tcl_AppendResult( interp, "Failed to read integer",
                          (char *) NULL );
        return TCL_ERROR;
    }

    if ( gaiaHDSTune( what, value, &error_mess ) != TCL_OK ) {
        Tcl_SetResult( interp, error_mess, TCL_VOLATILE );
        free( error_mess );
        return TCL_ERROR;
    }
    return TCL_OK;
}
