/*+
 *   Name:
 *      gaiaSlaTcl

 *   Purpose:
 *      SLALIB-like utility routines.

 *   Language:
 *      C

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics and Astronomy Research Council

 *   Authors:
 *      PWD: Peter W. Draper, Starlink - University of Durham

 *   History:
 *      30-MAR-2006 (PWD):
 *         Original version. Extracted from StarRtdImage.
 *      {enter_changes_here}
 *-
 */

#include <stdio.h>
#include <stdlib.h>

#include <tcl.h>
#include <gaiaArray.h>

/* Local prototypes */
static int gaiaSlaObs( ClientData clientData, Tcl_Interp *interp,
                       int objc, Tcl_Obj *CONST objv[] );

static int gaiaSlaDateObs2Je( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] );

/**
 * Register all the sla:: commands.
 */
int Sla_Init( Tcl_Interp *interp )
{
    Tcl_CreateObjCommand( interp, "sla::obs", gaiaSlaObs,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );

    Tcl_CreateObjCommand( interp, "sla::dateobs2je", gaiaSlaDateObs2Je,
                          (ClientData) NULL,
                          (Tcl_CmdDeleteProc *) NULL );
    return TCL_OK;
}

/**
 * Get details about observatories.
 *
 * There are two arguments, the observation index and the observation
 * station identifier. If the observation index is set to -1 then the
 * observation station identifier value is used, otherwise it is ignored.
 * See the SLA_OBS description in SUN/67.
 *
 * The results are a list of values, the observation station identifier,
 * name, longtitude, latitude and height.
 */
static int gaiaSlaObs( ClientData clientData, Tcl_Interp *interp,
                       int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
    char c[11];
    char name[41];
    double h;
    double p;
    double w;
    int n = 0;

    /* Check arguments, only one or two */
    if ( objc != 2 && objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "station_index [station_id]" );
        return TCL_ERROR;
    }

    if ( Tcl_GetIntFromObj( interp, objv[1], &n ) != TCL_OK ) {
        return TCL_ERROR;
    }
    if ( objc >= 3 ) {
        strncpy( c, Tcl_GetString( objv[2] ), 11 );
    }
    else {
        c[0] = '\0';
    }

    /* The call */
    slaObs( n, c, name, &w, &p, &h );

    // Construct a list return of all the parameters.
    resultObj = Tcl_GetObjResult( interp );
    Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewStringObj( c, -1 ) );
    Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewStringObj( name, -1 ));
    Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewDoubleObj( w ) );
    Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewDoubleObj( p ) );
    Tcl_ListObjAppendElement( interp, resultObj, Tcl_NewDoubleObj( h ) );

    return TCL_OK;
}



/**
 * Convert a DATE-OBS FITS date string to a Julian Epoch. Two arguments are
 * needed, the name of the keyword (so that old-style DATE is allowed) and
 * the complete FITS card.
 *
 * Note: not SLALIB really, just a wcslib function.
 *
 * The result is the julian epoch.
 */
static int gaiaSlaDateObs2Je( ClientData clientData, Tcl_Interp *interp,
                              int objc, Tcl_Obj *CONST objv[] )
{
    Tcl_Obj *resultObj;
    double value;
    int result;

    /* Check arguments, require two */
    if ( objc != 3 ) {
        Tcl_WrongNumArgs( interp, 1, objv, "FITS-keyword FITS-card" );
        return TCL_ERROR;
    }

    result = hgetdate( Tcl_GetString( objv[2] ), Tcl_GetString( objv[1] ), 
                       &value );
    if ( result == 0 ) {
        resultObj = Tcl_GetObjResult( interp );
        Tcl_SetStringObj( resultObj, Tcl_GetString( objv[2] ), -1 );
        Tcl_AppendStringsToObj( resultObj, " : not a valid date", 
                                (char *) NULL );
        return TCL_ERROR;
    }
    Tcl_SetObjResult( interp, Tcl_NewDoubleObj( value ) );
    return TCL_OK;
}
