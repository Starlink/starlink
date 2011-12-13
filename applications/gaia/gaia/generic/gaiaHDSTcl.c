/*+
 *  Name:
 *     gaiaHDSTcl

 *  Purpose:
 *     Access necessary HDS routines.

 *  Language:
 *     C

 *  Authors:
 *     PWD: Peter W. Draper, JAC - University of Durham

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
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *   History:
 *      29-MAR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>

#include <tcl.h>
#include <gaiaHDS.h>

/* Local prototypes */
static int gaiaHdsTclTune( ClientData clientData, Tcl_Interp *interp,
                           int objc, Tcl_Obj *CONST objv[] );
static int gaiaHdsTclGtune( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );

/**
 * Register all the array commands.
 */
int Hds_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "hds::tune", gaiaHdsTclTune,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "hds::gtune", gaiaHdsTclGtune,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );
    return TCL_OK;
}


/**
 * Set an HDS tuning parameter.
 */
static int gaiaHdsTclTune( ClientData clientData, Tcl_Interp *interp,
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
        return TCL_ERROR;
    }

    if ( gaiaHDSTune( what, value, &error_mess ) != TCL_OK ) {
        Tcl_SetResult( interp, error_mess, TCL_VOLATILE );
        free( error_mess );
        return TCL_ERROR;
    }
    return TCL_OK;
}

/**
 * Get the current value of an HDS tuning parameter.
 */
static int gaiaHdsTclGtune( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    char *what;
    char *error_mess;
    int value;

    /* Check arguments */
    if ( objc != 2 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "what" );
        return TCL_ERROR;
    }

    /* Get what. */
    what = Tcl_GetString( objv[1] );

    /* Get value. */
    value = 0;
    if ( gaiaHDSGTune( what, &value, &error_mess ) != TCL_OK ) {
        Tcl_SetResult( interp, error_mess, TCL_VOLATILE );
        free( error_mess );
        return TCL_ERROR;
    }
    Tcl_SetObjResult( interp, Tcl_NewIntObj( value ) );
    return TCL_OK;
}
