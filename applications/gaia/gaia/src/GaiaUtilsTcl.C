/*+
 *   Name:
 *      GaiaUtilsTcl

 *   Purpose:
 *      Utility routines from Tcl scripts.

 *   Language:
 *      C++

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
extern "C" {
#include <ast.h>
}
#include "gaiaUtils.h"

/* Local prototypes */
static int GaiaUtilsGtAxisWcs( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsGt2DWcs( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsAstSet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );
static int GaiaUtilsUrlGet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] );

/**
 * Register all the gaiautils:: commands.
 */
int GaiaUtils_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "gaiautils::get2dwcs", GaiaUtilsGt2DWcs,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::getaxiswcs", GaiaUtilsGtAxisWcs,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::astset", GaiaUtilsAstSet,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "gaiautils::urlget", GaiaUtilsUrlGet,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    return TCL_OK;
}

/**
 * Extract a WCS for a two axes from a full WCS. The result is
 * the address of an AST FrameSet.
 *
 * There are five required arguments, the address of the AST FrameSet, the two
 * axes to extract and their lengths (in base units, usually the width and
 * height of the image) and one optional coordinate to use as a replacement
 * value for the removed axis (a const to the PermMap).
 */
static int GaiaUtilsGt2DWcs( ClientData clientData, Tcl_Interp *interp,
                             int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *fullwcs;
    AstFrameSet *newwcs;
    char *error_mess;
    int axis1;
    int axis2;
    int index;
    int length1;
    int length2;
    int result;
    long adr;

    /* Check arguments, only allow five or six, the frameset, the two axes and
     * some scale lengths along the axes (base units), plus the replacement
     * value. */
    if ( objc != 6 && objc != 7 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frameset axis1 axis2 length1 "
                          "length2 [axis3_coord]" );
        return TCL_ERROR;
    }

    /* Get the frameset */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    fullwcs = (AstFrameSet *) adr;

    /* Get the axes, these are AST ones.*/
    if ( Tcl_GetIntFromObj( interp, objv[2], &axis1 ) != TCL_OK ) {
        return TCL_ERROR;
    }
    if ( Tcl_GetIntFromObj( interp, objv[3], &axis2 ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* Get the lengths */
    if ( Tcl_GetIntFromObj( interp, objv[4], &length1 ) != TCL_OK ) {
        return TCL_ERROR;
    }
    if ( Tcl_GetIntFromObj( interp, objv[5], &length2 ) != TCL_OK ) {
        return TCL_ERROR;
    }

    /* And the replacement index */
    index = 0;
    if ( objc == 7 ) {
        if ( Tcl_GetIntFromObj( interp, objv[6], &index ) != TCL_OK ) {
            return TCL_ERROR;
        }
    }

    /* Extract 2D WCS */
    result = gaiaUtilsGt2DWcs( fullwcs, axis1, axis2, length1, length2,
                               index, &newwcs, &error_mess );

    /* Export the new WCS as a long containing the address */
    if ( result == TCL_OK ) {
        Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) newwcs ) );
    }
    else {
        Tcl_SetResult( interp, error_mess, TCL_VOLATILE );
        free( error_mess );
    }
    return result;
}

/**
 * Extract a WCS for a specific axis from a full WCS. The result is
 * the address of an AST FrameSet.
 *
 * There are four arguments, the address of the AST FrameSet, the axis to
 * extract, an offset and the coordinate of a value along the axis.  The
 * offset is for the GRID domain and is used when a section of a WCS is
 * required (set this to 0 when not needed, this is the NDF origin).
 */
static int GaiaUtilsGtAxisWcs( ClientData clientData, Tcl_Interp *interp,
                               int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *fullwcs;
    AstFrameSet *axiswcs;
    char *error_mess;
    int axis;
    int offset;
    int result;
    long adr;

    /* Check arguments, only allow three, the frameset and an axis number, plus
     * offset */ 
    if ( objc != 4 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frameset axis offset" );
        return TCL_ERROR;
    }

    /* Get the frameset */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    fullwcs = (AstFrameSet *) adr;

    /* Get the axis and offset */
    axis = -1;
    offset = 0;
    if ( Tcl_GetIntFromObj( interp, objv[2], &axis ) != TCL_OK ) {
        return TCL_ERROR;
    }
    if ( Tcl_GetIntFromObj( interp, objv[3], &offset ) != TCL_OK ) {
        return TCL_ERROR;
    }
    result= gaiaUtilsGtAxisWcs( fullwcs, axis, offset, &axiswcs, &error_mess );

    /* Export the new WCS as a long containing the address */
    if ( result == TCL_OK ) {
        Tcl_SetObjResult( interp, Tcl_NewLongObj( (long) axiswcs ) );
    }
    else {
        Tcl_SetResult( interp, error_mess, TCL_VOLATILE );
        free( error_mess );
    }
    return result;
}

/**
 * Apply AST options to a FrameSet.
 *
 * There are two arguments, the address of the AST FrameSet and the options
 * string ("option=value,option=value" etc.).
 */
static int GaiaUtilsAstSet( ClientData clientData, Tcl_Interp *interp,
                            int objc, Tcl_Obj *CONST objv[] )
{
    AstFrameSet *wcs;
    long adr;
    
    /* Check arguments, only allow two. */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "frameset options_string" );
        return TCL_ERROR;
    }

    /* Get the frameset */
    if ( Tcl_GetLongFromObj( interp, objv[1], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    wcs = (AstFrameSet *) adr;

    /* Set the options */
    astSet( wcs, Tcl_GetString( objv[2] ) );
    if ( ! astOK ) {
        return TCL_ERROR;
    }
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
